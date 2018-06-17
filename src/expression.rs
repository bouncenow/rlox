use std::fmt;

use scan::Token;

#[derive(Debug)]
pub enum Expr {
    Assign { name: Token, value: Box<Expr> },
    Binary { left: Box<Expr>, operator: Token, right: Box<Expr> },
    Unary { operator: Token, right: Box<Expr> },
    Literal { value: ExprVal },
    Grouping { expr: Box<Expr> },
    Variable { name: Token },
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprVal {
    String(String),
    Double(f64),
    Boolean(bool),
    Nil
}

impl fmt::Display for ExprVal {

    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            ExprVal::String(s) => write!(f, "{}", s),
            ExprVal::Double(d) => write!(f, "{}", d),
            ExprVal::Boolean(b) => write!(f, "{}", b),
            &ExprVal::Nil => write!(f, "nil")
        }
    }

}