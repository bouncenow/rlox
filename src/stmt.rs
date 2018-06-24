use std::fmt;

use expression::*;
use scan::Token;

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression { expr: Expr },
    Function { decl: FunctionDecl },
    Print { expr: Expr },
    Var { name: Token, initializer: Option<Expr> },
    Block { statements: Vec<Stmt> },
    If { condition: Expr, then_branch: Box<Stmt>, else_branch: Option<Box<Stmt>> },
    While { condition: Box<Expr>, body: Box<Stmt> },
    Return { keyword: Token, value: Option<Box<Expr>> },
}

#[derive(Clone)]
pub struct FunctionDecl {
    pub name: Token,
    pub parameters: Vec<Token>,
    pub body: Vec<Stmt>,
}

impl fmt::Debug for FunctionDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "fn {}", &self.name.lexeme)
    }
}