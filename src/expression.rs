use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;

use scan::Token;
use functions::RloxCallable;
use stmt::FunctionBody;
use class::ClassInstance;
use class::RloxClass;

#[derive(Debug, Clone)]
pub enum Expr {
    Assign { name: Token, value: Box<Expr>, resolve_at: Option<usize> },
    Binary { left: Box<Expr>, operator: Token, right: Box<Expr> },
    Unary { operator: Token, right: Box<Expr> },
    Literal { value: ExprVal },
    Grouping { expr: Box<Expr> },
    Variable { v: Variable },
    Logical { left: Box<Expr>, operator: Token, right: Box<Expr> },
    Call { callee: Box<Expr>, paren: Token, arguments: Vec<Expr> },
    FunctionExpr { body: FunctionBody },
    Get { object: Box<Expr>, name: Token },
    Set { object: Box<Expr>, name: Token, value: Box<Expr> },
    This { keyword: Token, resolve_at: Option<usize> },
    Super { keyword: Token, method: Token, resolve_at: Option<usize> },
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Token,
    pub resolve_at: Option<usize>,
}

#[derive(Clone)]
pub enum ExprVal {
    String(String),
    Double(f64),
    Boolean(bool),
    Callable(Rc<RloxCallable>),
    Class(Rc<RloxClass>),
    ClassInstance(Rc<RefCell<ClassInstance>>),
    Nil,
}

impl fmt::Display for ExprVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            ExprVal::String(s) => write!(f, "{}", s),
            ExprVal::Double(d) => write!(f, "{}", d),
            ExprVal::Boolean(b) => write!(f, "{}", b),
            &ExprVal::Nil => write!(f, "nil"),
            ExprVal::Callable(_) => write!(f, "callable <>"),
            ExprVal::Class(cls) => write!(f, "class {}", cls),
            ExprVal::ClassInstance(c) => write!(f, "instance of {}", &c.borrow().class.name.lexeme)
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
            ExprVal::Callable(_) => write!(f, "callable <>"),
            ExprVal::Class(cls) => write!(f, "class {}", cls),
            ExprVal::ClassInstance(c) => write!(f, "instance of {}", &c.borrow().class.name.lexeme)
        }
    }
}