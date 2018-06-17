use std::fmt;
use std::fmt::Write;

use scan::Token;

#[derive(Debug)]
pub struct RloxError {
    pub errors: Vec<String>
}

impl RloxError {
    pub fn new(message: String) -> RloxError {
        let mut errors = Vec::new();
        errors.push(message);
        RloxError {
            errors
        }
    }
}

impl fmt::Display for RloxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut out = String::new();
        let mut not_first = false;
        for e in &self.errors {
            if not_first {
                write!(out, "\n{}", e)?;
            } else {
                write!(out, "{}", e)?;
            }
            not_first = true;
        }
        write!(f, "{}", out)
    }
}

pub struct ErrorRepr {
    pub token: Token,
    pub message: String
}

#[derive(Debug)]
pub struct ErrorWithPartialResult<T> {
    pub error: RloxError,
    pub partial_result: T
}