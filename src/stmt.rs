use expression::*;
use scan::Token;

#[derive(Debug)]
pub enum Stmt {
    Expression { expr: Expr },
    Print { expr: Expr },
    Var { name: Token, initializer: Option<Expr> },
    Block { statements: Vec<Stmt> },
    If { condition: Expr, then_branch: Box<Stmt>, else_branch: Option<Box<Stmt>> }
}