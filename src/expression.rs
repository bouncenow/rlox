use std::fmt;
use std::rc::Rc;

use scan::Token;
use functions::RloxCallable;

#[derive(Debug, Clone)]
pub enum Expr {
    Assign { name: Token, value: Box<Expr>, resolve_at: Option<usize> },
    Binary { left: Box<Expr>, operator: Token, right: Box<Expr> },
    Unary { operator: Token, right: Box<Expr> },
    Literal { value: ExprVal },
    Grouping { expr: Box<Expr> },
    Variable { name: Token, resolve_at: Option<usize> },
    Logical { left: Box<Expr>, operator: Token, right: Box<Expr> },
    Call { callee: Box<Expr>, paren: Token, arguments: Vec<Expr> },
}


#[derive(Clone)]
pub enum ExprVal {
    String(String),
    Double(f64),
    Boolean(bool),
    Callable(Rc<RloxCallable>),
    Nil,
}

impl fmt::Display for ExprVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            ExprVal::String(s) => write!(f, "{}", s),
            ExprVal::Double(d) => write!(f, "{}", d),
            ExprVal::Boolean(b) => write!(f, "{}", b),
            &ExprVal::Nil => write!(f, "nil"),
            ExprVal::Callable(_) => write!(f, "fn<>")
        }
    }
}

impl PartialEq for ExprVal {
    fn eq(&self, other: &ExprVal) -> bool {
        match (self, other) {
            (ExprVal::String(l), ExprVal::String(r)) => l == r,
            (ExprVal::Double(l), ExprVal::Double(r)) => l == r,
            (ExprVal::Boolean(l), ExprVal::Boolean(r)) => l == r,
            (&ExprVal::Nil, &ExprVal::Nil) => true,
            _ => false
        }
    }
}

impl fmt::Debug for ExprVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            ExprVal::String(s) => write!(f, "{}", s),
            ExprVal::Double(d) => write!(f, "{}", d),
            ExprVal::Boolean(b) => write!(f, "{}", b),
            &ExprVal::Nil => write!(f, "nil"),
            ExprVal::Callable(_) => write!(f, "fn<>")
        }
    }
}