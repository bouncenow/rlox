use scan::Token;
use parse::Expr;
use parse::ExprLiteralValue;

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

pub struct ErrorRepr {
    pub token: Token,
    pub message: String
}

#[derive(Debug)]
pub struct ErrorWithPartialResult<T> {
    pub error: RloxError,
    pub partial_result: T
}

pub fn pretty_print(expr: &Expr) -> String {
    match expr {
        Expr::Binary {left, operator, right} => {
            format!("({} {} {})",
                    operator.lexeme,
                    pretty_print(left.as_ref()),
                    pretty_print(right.as_ref())
            )
        }
        Expr::Unary {operator, right} => {
            format!("({} {})",
                    operator.lexeme,
                    pretty_print(right.as_ref())
            )
        }
        Expr::Literal {value} => {
            match value {
                ExprLiteralValue::String(s) => format!("{}", s),
                ExprLiteralValue::Double(d) => format!("{}", d),
                ExprLiteralValue::Boolean(b) => format!("{}", b),
                ExprLiteralValue::Nil => format!("nil"),
            }
        }
        Expr::Grouping {expr} => {
            format!("{}", pretty_print(expr.as_ref()))
        }
    }
}