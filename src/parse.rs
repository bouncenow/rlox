use scan::*;
use expression::Expr;
use expression::ExprVal;
use stmt::Stmt;
use util::RloxError;

struct ParserState<'a> {
    tokens: &'a Vec<Token>,
    errors: Vec<String>,
    current: usize,
}

pub fn parse_for_expression(tokens: &Vec<Token>) -> Result<Expr, RloxError> {
    let parser_state = ParserState::new(tokens);
    parser_state.parse_expression()
}

pub fn parse(tokens: &Vec<Token>) -> Result<Vec<Stmt>, RloxError> {
    let parser_state = ParserState::new(tokens);
    parser_state.parse()
}

type ResExpr<'a> = Result<Expr, RloxError>;
type ResStmt<'a> = Result<Stmt, RloxError>;
type ResListStmt<'a> = Result<Vec<Stmt>, RloxError>;

impl<'a> ParserState<'a> {
    fn new(tokens: &'a Vec<Token>) -> ParserState {
        ParserState {
            tokens,
            errors: Vec::new(),
            current: 0,
        }
    }

    fn parse(mut self) -> ResListStmt<'a> {
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => return Err(e)
            }
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> ResStmt<'a> {
        if self.match_next_one(TokenType::Var) {
            let decl_result = self.var_declaration();
            if let Err(_) = &decl_result {
                self.synchronize();
            }
            return decl_result;
        }

        let statement_result = self.statement();
        if let Err(_) = &statement_result {
            self.synchronize();
        }
        statement_result
    }

    fn var_declaration(&mut self) -> ResStmt<'a> {
        let name = self.consume(TokenType::Identifier, "Expect variable name.")?;

        let initializer = if self.match_next_one(TokenType::Equal) {
            let expr = self.expression()?;
            Some(expr)
        } else {
            None
        };

        self.consume(TokenType::Semicolon, "Expect semicolon after variable declaration")?;
        Ok(Stmt::Var { name, initializer })
    }

    fn statement(&mut self) -> ResStmt<'a> {
        if self.match_next_one(TokenType::Print) {
            return self.print_statement();
        } else if self.match_next_one(TokenType::LeftBrace) {
            let block = self.block()?;
            return Ok(Stmt::Block { statements: block });
        }

        self.expression_statement()
    }

    fn block(&mut self) -> ResListStmt<'a> {
        let mut statements = Vec::new();

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(statements)
    }

    fn print_statement(&mut self) -> ResStmt<'a> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after print statement.")?;
        Ok(Stmt::Print { expr })
    }

    fn expression_statement(&mut self) -> ResStmt<'a> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Expression { expr })
    }

    fn parse_expression(mut self) -> ResExpr<'a> {
        self.expression()
    }

    fn expression(&mut self) -> ResExpr<'a> {
        self.assignment()
    }

    fn assignment(&mut self) -> ResExpr<'a> {
        let expr = self.equality()?;

        if self.match_next_one(TokenType::Equal) {
            let value = self.assignment()?;

            return if let Expr::Variable { name } = expr {
                Ok(Expr::Assign { name, value: Box::new(value) })
            } else {
                Err(RloxError::new("Invalid assignment target".to_string()))
            };
        }

        return Ok(expr);
    }

    fn equality(&mut self) -> ResExpr<'a> {
        let mut expr = self.comparison()?;

        while self.match_next(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ResExpr<'a> {
        let mut expr = self.addition()?;

        while self.match_next(&[TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]) {
            let operator = self.previous();
            let right = self.addition()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn addition(&mut self) -> ResExpr<'a> {
        let mut expr = self.multiplication()?;

        while self.match_next(&[TokenType::Plus, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.multiplication()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> ResExpr<'a> {
        let mut expr = self.unary()?;

        while self.match_next(&[TokenType::Star, TokenType::Slash]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ResExpr<'a> {
        if self.match_next(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }

        self.primary()
    }

    fn primary(&mut self) -> ResExpr<'a> {
        if self.match_next_one(TokenType::False) {
            return Ok(Expr::Literal { value: ExprVal::Boolean(false) });
        }
        if self.match_next_one(TokenType::True) {
            return Ok(Expr::Literal { value: ExprVal::Boolean(true) });
        }
        if self.match_next_one(TokenType::Nil) {
            return Ok(Expr::Literal { value: ExprVal::Nil });
        }

        if self.match_next(&[TokenType::Number, TokenType::String]) {
            let token_literal = self.previous().literal.clone();
            return match token_literal {
                Some(TokenLiteralValue::Double(d)) => Ok(Expr::Literal { value: ExprVal::Double(d) }),
                Some(TokenLiteralValue::String(s)) => Ok(Expr::Literal { value: ExprVal::String(s) }),
                _ => panic!("Illegally scanned token")
            };
        }

        if self.match_next_one(TokenType::LeftParen) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen, "Expect right paren")?;
            return Ok(Expr::Grouping { expr: Box::new(expr) });
        }

        if self.match_next_one(TokenType::Identifier) {
            return Ok(Expr::Variable { name: self.previous().clone() });
        }

        Err(RloxError::new("Expect expression".to_string()))
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().token_type {
                TokenType::Class | TokenType::Fun | TokenType::Var | TokenType::For | TokenType::If
                | TokenType::While | TokenType::Print | TokenType::Return => {
                    return;
                }
                _ => {}
            }

            self.advance();
        }
    }

    fn consume(&mut self, token_type: TokenType, error_message: &str) -> Result<Token, RloxError> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            self.add_error(error_message.to_string());
            Err(RloxError::new(error_message.to_string()))
        }
    }

    fn add_error(&mut self, error: String) {
        self.errors.push(error);
    }

    fn match_next(&mut self, token_types: &[TokenType]) -> bool {
        let one_of_matches = token_types.iter()
            .any(|t| {
                self.check(*t)
            });

        if one_of_matches {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_next_one(&mut self, token_type: TokenType) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().token_type == token_type
        }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }
}