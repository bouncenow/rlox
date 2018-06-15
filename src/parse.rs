use scan::*;
use util::RloxError;

#[derive(Debug)]
pub enum Expr<'a> {
    Binary { left: Box<Expr<'a>>, operator: &'a Token, right: Box<Expr<'a>> },
    Unary { operator: &'a Token, right: Box<Expr<'a>> },
    Literal { value: ExprLiteralValue },
    Grouping { expr: Box<Expr<'a>> },
}

#[derive(Debug)]
pub enum ExprLiteralValue {
    String(String),
    Double(f64),
    Boolean(bool),
    Nil
}

struct ParserState<'a> {
    tokens: &'a Vec<Token>,
    errors: Vec<String>,
    current: usize,
}

pub fn parse(tokens: &Vec<Token>) -> Result<Expr, RloxError> {
    let parser_state = ParserState::new(tokens);
    parser_state.parse_expression()
}

type Res<'b> = Result<Expr<'b>, RloxError>;

impl<'a> ParserState<'a> {
    fn new(tokens: &'a Vec<Token>) -> ParserState {
        ParserState {
            tokens,
            errors: Vec::new(),
            current: 0,
        }
    }

    fn parse_expression(mut self) -> Res<'a> {
        self.expression()
    }

    fn expression(&mut self) -> Res<'a> {
        self.equality()
    }

    fn equality(&mut self) -> Res<'a> {
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

    fn comparison(&mut self) -> Res<'a> {
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

    fn addition(&mut self) -> Res<'a> {
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

    fn multiplication(&mut self) -> Res<'a> {
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

    fn unary(&mut self) -> Res<'a> {
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

    fn primary(&mut self) -> Res<'a> {
        if self.match_next_one(TokenType::False) {
            return Ok(Expr::Literal { value: ExprLiteralValue::Boolean(false) });
        }
        if self.match_next_one(TokenType::True) {
            return Ok(Expr::Literal { value: ExprLiteralValue::Boolean(true) });
        }
        if self.match_next_one(TokenType::Nil) {
            return Ok(Expr::Literal { value: ExprLiteralValue::Nil });
        }

        if self.match_next(&[TokenType::Number, TokenType::String]) {
            let token_literal = self.previous().literal.clone();
            return match token_literal {
                Some(TokenLiteralValue::Double(d)) => Ok(Expr::Literal {value: ExprLiteralValue::Double(d)}),
                Some(TokenLiteralValue::String(s)) => Ok(Expr::Literal {value: ExprLiteralValue::String(s)}),
                _ => panic!("Illegally scanned token")
            };
        }

        if self.match_next_one(TokenType::LeftParen) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen, "Expect right paren")?;
            return Ok(Expr::Grouping { expr: Box::new(expr) });
        }

        Err(RloxError::new("Expect expression".to_string()))
    }

    fn consume(&mut self, token_type: TokenType, error_message: &str) -> Result<&'a Token, RloxError> {
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

    fn advance(&mut self) -> &'a Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn previous(&self) -> &'a Token {
        &self.tokens[self.current - 1]
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