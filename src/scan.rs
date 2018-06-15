use std::any::Any;
use std::collections::HashMap;

use util::ErrorWithPartialResult;
use util::RloxError;

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub literal: Option<Box<Any>>
}

#[derive(Debug, Copy, Clone)]
pub enum TokenType {
    LeftParen, RightParen,
    LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus,
    Semicolon, Slash, Star,

    Bang, BangEqual, Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    Identifier,
    String, Number,

    And,
    Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,
    Break,

    Eof
}

pub fn scan_tokens(source: &str) -> Result<Vec<Token>, ErrorWithPartialResult<Vec<Token>>> {
    let scanner_state = ScannerState::new(source);
    scanner_state.scan_tokens()
}

struct ScannerState {
    source: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    keywords: HashMap<&'static str, TokenType>,
    errors: Vec<String>
}

impl ScannerState {

    fn new(source: &str) -> ScannerState {
        let keywords: HashMap<&'static str, TokenType> =
            [
                ("and", TokenType::And),
                ("class", TokenType::Class),
                ("else", TokenType::Else),
                ("false", TokenType::False),
                ("for", TokenType::For),
                ("fun", TokenType::Fun),
                ("if", TokenType::If),
                ("nil", TokenType::Nil),
                ("or", TokenType::Or),
                ("print", TokenType::Print),
                ("return", TokenType::Return),
                ("super", TokenType::Super),
                ("this", TokenType::This),
                ("true", TokenType::True),
                ("var", TokenType::Var),
                ("while", TokenType::While),
                ("break", TokenType::Break)
            ].iter().cloned().collect();

        ScannerState {
            tokens: Vec::new(),
            source: source.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
            keywords,
            errors: Vec::new()
        }
    }

    fn scan_tokens(mut self) -> Result<Vec<Token>, ErrorWithPartialResult<Vec<Token>>> {
        println!("Scanning!");

        while !self.check_if_at_end() {
            self.sync_start_current();
            self.scan_token();
        }

        if self.errors.is_empty() {
            Ok(self.tokens)
        } else {
            Err(ErrorWithPartialResult {
                error: RloxError {
                    errors: self.errors
                },
                partial_result: self.tokens
            })
        }
    }

    fn sync_start_current(&mut self) {
        self.start = self.current
    }

    fn check_if_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        if self.current < self.source.len() {
            self.current += 1;
            self.source[self.current - 1]
        } else {
            panic!("Illegal advance at the end!")
        }
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '!' => {
                if self.match_next('=') {
                    self.add_token(TokenType::BangEqual)
                } else {
                    self.add_token(TokenType::Bang)
                }
            }
            '=' => {
                if self.match_next('=') {
                    self.add_token(TokenType::EqualEqual)
                } else {
                    self.add_token(TokenType::Equal)
                }
            }
            '<' => {
                if self.match_next('=') {
                    self.add_token(TokenType::LessEqual)
                } else {
                    self.add_token(TokenType::Less)
                }
            }
            '>' => {
                if self.match_next('=') {
                    self.add_token(TokenType::GreaterEqual)
                } else {
                    self.add_token(TokenType::Greater)
                }
            }
            '/' => {
                if self.match_next('/') {
                    while let Some(_) = self.peek() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            ' ' | '\t' | '\r' => {}
            '\n' => {
                self.line += 1;
            }
            '"' => {
                self.string();
            }
            '0' ... '9' => {
                self.number();
            }
            'a' ... 'z' | 'A' ... 'Z' | '_' => {
                self.identifier();
            }
            c   => {
                let error = format!("Unexpected character: {}", c);
                self.report_error(&error);
            }
        }
    }

    fn string(&mut self) {
        while let Some(c) = self.peek() {
            if c == '"' {
                break;
            }
            if c == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.check_if_at_end() {
            self.report_error("Unterminated string.");
            return;
        }

        self.advance();

        let str_value = self.get_source_substring(self.start + 1, self.current - 1);
        self.add_token_with_literal(TokenType::String, Some(Box::new(str_value)))
    }

    fn number(&mut self) {
        while ScannerState::is_option_digit(self.peek()) {
            self.advance();
        }

        if let Some('.') = self.peek() {
            if ScannerState::is_option_digit(self.peek_next()) {
                self.advance();
                while ScannerState::is_option_digit(self.peek()) {
                    self.advance();
                }
            }
        }

        let num_val: f64 = self.get_current_lexeme().parse()
            .expect("Wrongly parsed position of double!");
        self.add_token_with_literal(TokenType::Number, Some(Box::new(num_val)));
    }

    fn is_option_digit(c: Option<char>) -> bool {
        match c {
            Some(d) if d.is_digit(10) => true,
            _ => false
        }
    }

    fn identifier(&mut self) {
        while ScannerState::is_option_alphanumeric(self.peek()) {
            self.advance();
        }

        let text = self.get_current_lexeme();
        let token_type = match self.keywords.get::<str>(&text) {
            Some(keyword_type) => *keyword_type,
            None => TokenType::Identifier
        };

        self.add_token(token_type);
    }

    fn is_option_alphanumeric(c: Option<char>) -> bool {
        match c {
            Some(d) if d.is_alphanumeric() || d == '_' => true,
            _ => false
        }
    }

    fn match_next(&mut self, expected: char) -> bool {
        if let Some(c) = self.peek() {
            if c != expected {
                return false
            }
        } else {
            return false
        }

        self.current += 1;
        return true
    }

    fn peek(&self) -> Option<char> {
        if self.current < self.source.len() {
            Some(self.source[self.current])
        } else {
            None
        }
    }

    fn peek_next(&self) -> Option<char> {
        if self.current + 1 < self.source.len() {
            Some(self.source[self.current + 1])
        } else {
            None
        }
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_with_literal(token_type, None);
    }

    fn add_token_with_literal(&mut self, token_type: TokenType, literal: Option<Box<Any>>) {
        let lexeme = self.get_current_lexeme();
        self.tokens.push(
            Token {
                token_type,
                lexeme,
                line: self.line,
                literal
            }
        )
    }

    fn get_current_lexeme(&self) -> String {
        self.get_source_substring(self.start, self.current)
    }

    fn get_source_substring(&self, start: usize, end: usize) -> String {
        self.source[start..end].iter().collect()
    }

    fn report_error(&mut self, error: &str) {
        self.errors.push(error.to_string());
    }
}
