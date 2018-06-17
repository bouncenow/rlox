use expression::*;
use scan::Token;

pub enum Stmt {
    Expression { expr: Expr },
    Print { expr: Expr },
    Var { name: Token, initializer: Option<Expr> },
    Block { statements: Vec<Stmt> },
}