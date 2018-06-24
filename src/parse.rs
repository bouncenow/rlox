use scan::*;
use expression::Expr;
use expression::ExprVal;
use stmt::*;
use util::RloxError;

struct ParserState<'a> {
    tokens: &'a Vec<Token>,
    errors: Vec<String>,
    current: usize,
    loop_stack: Vec<usize>,
}

pub fn parse_for_expression(tokens: &Vec<Token>) -> Result<Expr, RloxError> {
    let parser_state = ParserState::new(tokens);
    parser_state.parse_expression()
}

pub fn parse(tokens: &Vec<Token>) -> Result<Vec<Stmt>, RloxError> {
    let parser_state = ParserState::new(tokens);
    parser_state.parse()
}

type ResExpr = Result<Expr, RloxError>;
type ResStmt = Result<Stmt, RloxError>;
type ResListStmt = Result<Vec<Stmt>, RloxError>;

impl<'a> ParserState<'a> {
    fn new(tokens: &'a Vec<Token>) -> ParserState {
        ParserState {
            tokens,
            errors: Vec::new(),
            current: 0,
            loop_stack: vec![0],
        }
    }

    fn parse(mut self) -> ResListStmt {
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => return Err(e)
            }
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> ResStmt {
        let result = if self.match_next_one(TokenType::Var) {
            self.var_declaration()
        } else if self.match_next_one(TokenType::Fun) {
            self.function("function")
        } else {
            self.statement()
        };

        if let Err(_) = &result {
            self.synchronize();
        }

        result
    }

    fn var_declaration(&mut self) -> ResStmt {
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

    fn function(&mut self, kind: &'static str) -> ResStmt {
        let name = self.consume(TokenType::Identifier, &format!("Expect {} name", kind))?;
        self.consume(TokenType::LeftParen, &format!("Expect '(' after {} name", kind))?;

        let mut parameters = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                if parameters.len() >= 8 {
                    return Err(RloxError::new("Can't have more than 8 parameters".to_string()));
                }

                parameters.push(self.consume(TokenType::Identifier, "Expect parameter name.")?);
                if !self.match_next_one(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after the arguments")?;

        self.consume(TokenType::LeftBrace, &format!("Expect '{{' before {} body", kind))?;
        self.increase_loop_stack();
        let body = self.block()?;
        self.pop_loop_stack();

        Ok(Stmt::Function { decl: FunctionDecl { name, parameters, body } })
    }

    fn return_statement(&mut self) -> ResStmt {
        let keyword = self.previous();
        let value = if !self.check(TokenType::Semicolon) {
            Some(Box::new(self.expression()?))
        } else {
            None
        };

        self.consume(TokenType::Semicolon, "Expect ';' after return statement")?;
        Ok(Stmt::Return { keyword, value })
    }

    fn statement(&mut self) -> ResStmt {
        if self.match_next_one(TokenType::Print) {
            return self.print_statement();
        } else if self.match_next_one(TokenType::LeftBrace) {
            let block = self.block()?;
            return Ok(Stmt::Block { statements: block });
        } else if self.match_next_one(TokenType::If) {
            return self.if_statement();
        } else if self.match_next_one(TokenType::While) {
            return self.while_statement();
        } else if self.match_next_one(TokenType::For) {
            return self.for_statement();
        } else if self.match_next_one(TokenType::Return) {
            return self.return_statement();
        } else if self.match_next_one(TokenType::Break) {
            return self.break_statement();
        }

        self.expression_statement()
    }

    fn block(&mut self) -> ResListStmt {
        let mut statements = Vec::new();

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(statements)
    }

    fn print_statement(&mut self) -> ResStmt {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after print statement.")?;
        Ok(Stmt::Print { expr })
    }

    fn expression_statement(&mut self) -> ResStmt {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Expression { expr })
    }

    fn if_statement(&mut self) -> ResStmt {
        self.consume(TokenType::LeftParen, "Expect '(' after if.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after if condition")?;

        let then_branch = self.statement()?;
        let else_branch = if self.match_next_one(TokenType::Else) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If { condition, then_branch: Box::new(then_branch), else_branch })
    }

    fn while_statement(&mut self) -> ResStmt {
        self.consume(TokenType::LeftParen, "Expect '(' after while.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after while condition.")?;
        self.start_loop();
        let body = self.statement()?;
        self.end_loop();

        Ok(Stmt::While { condition: Box::new(condition), body: Box::new(body) })
    }

    fn for_statement(&mut self) -> ResStmt {
        self.consume(TokenType::LeftParen, "Expect '(' after for.")?;

        let initializer = if self.match_next_one(TokenType::Semicolon) {
            None
        } else if self.match_next_one(TokenType::Var) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let for_condition = if !self.check(TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::Semicolon, "Expect ';' after loop condition")?;

        let increment = if !self.check(TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;

        self.start_loop();
        let for_body = self.statement()?;
        self.end_loop();

        let while_body = match increment {
            Some(e) => Stmt::Block { statements: vec![for_body, Stmt::Expression { expr: e }] },
            None => for_body
        };

        let while_condition = match for_condition {
            Some(e) => e,
            None => Expr::Literal { value: ExprVal::Boolean(true) }
        };
        let while_loop = Stmt::While { condition: Box::new(while_condition), body: Box::new(while_body) };

        let whole_stmt = match initializer {
            Some(s) => Stmt::Block { statements: vec![s, while_loop] },
            None => while_loop
        };
        Ok(whole_stmt)
    }

    fn break_statement(&mut self) -> ResStmt {
        if !self.is_inside_loop() {
            return Err(RloxError::new("Break statement can appear only inside the loop!".to_string()));
        }
        self.consume(TokenType::Semicolon, "Expect ';' after break.")?;
        Ok(Stmt::Break)
    }

    fn parse_expression(mut self) -> ResExpr {
        self.expression()
    }

    fn expression(&mut self) -> ResExpr {
        self.assignment()
    }

    fn assignment(&mut self) -> ResExpr {
        let expr = self.or()?;

        if self.match_next_one(TokenType::Equal) {
            let value = self.assignment()?;

            return if let Expr::Variable { name, .. } = expr {
                Ok(Expr::Assign { name, value: Box::new(value), resolve_at: None })
            } else {
                Err(RloxError::new("Invalid assignment target".to_string()))
            };
        }

        return Ok(expr);
    }

    fn or(&mut self) -> ResExpr {
        let mut expr = self.and()?;

        while self.match_next_one(TokenType::Or) {
            let operator = self.previous();
            let right = self.and()?;
            expr = Expr::Logical { left: Box::new(expr), operator, right: Box::new(right) };
        }

        Ok(expr)
    }

    fn and(&mut self) -> ResExpr {
        let mut expr = self.equality()?;

        while self.match_next_one(TokenType::And) {
            let operator = self.previous();
            let right = self.equality()?;
            expr = Expr::Logical { left: Box::new(expr), operator, right: Box::new(right) };
        }

        Ok(expr)
    }

    fn equality(&mut self) -> ResExpr {
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

    fn comparison(&mut self) -> ResExpr {
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

    fn addition(&mut self) -> ResExpr {
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

    fn multiplication(&mut self) -> ResExpr {
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

    fn unary(&mut self) -> ResExpr {
        if self.match_next(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }

        self.call()
    }

    fn call(&mut self) -> ResExpr {
        let mut expr = self.primary()?;

        loop {
            if self.match_next_one(TokenType::LeftParen) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> ResExpr {
        let mut arguments = Vec::new();

        if !self.check(TokenType::RightParen) {
            arguments.push(self.expression()?);
            while self.match_next_one(TokenType::Comma) {
                arguments.push(self.expression()?);
            }
        }

        let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments")?;

        Ok(Expr::Call { callee: Box::new(callee), paren, arguments })
    }

    fn primary(&mut self) -> ResExpr {
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
            return Ok(Expr::Variable { name: self.previous().clone(), resolve_at: None });
        }

        Err(RloxError::new("Expect expression".to_string()))
    }

    fn increase_loop_stack(&mut self) {
        self.loop_stack.push(0)
    }

    fn pop_loop_stack(&mut self) {
        self.loop_stack.pop().unwrap();
    }

    fn start_loop(&mut self) {
        match self.loop_stack.last_mut() {
            Some(d) => *d += 1,
            None => panic!("Illegally constructed loop stack"),
        }
    }

    fn end_loop(&mut self) {
        match self.loop_stack.last_mut() {
            Some(d) => *d -= 1,
            None => panic!("Illegally constructed loop stack"),
        }
    }

    fn is_inside_loop(&self) -> bool {
        match self.loop_stack.last() {
            Some(d) => *d > 0,
            None => panic!("Illegally constructed loop stack"),
        }
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