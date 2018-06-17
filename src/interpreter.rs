use std::collections::HashMap;
use std::rc::Rc;

use expression::*;
use scan::TokenType;
use scan::Token;
use stmt::*;

pub struct Interpreter {
    env: Rc<Environment>
}

struct Environment {
    values: HashMap<String, Option<ExprVal>>,
    enclosing: Option<Rc<Environment>>,
}

impl Environment {
    fn new_global() -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    fn new_with_enclosing(enclosing: Rc<Environment>) -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    fn define(&mut self, name: String, value: Option<ExprVal>) {
        self.values.insert(name, value);
    }

    fn get(&self, name: String) -> Result<Option<ExprVal>, String> {
        match self.values.get(&name) {
            Some(v) => Ok(v.clone()),
            None => {
                match self.enclosing {
                    Some(ref e) => e.get(name),
                    None => Err("Undeclared variable".to_string())
                }
            }
        }
    }

    fn assign(&mut self, name: Token, value: ExprVal) -> Result<(), String> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme, Some(value));
            Ok(())
        } else {
            match self.enclosing {
                Some(ref mut e) => Rc::get_mut(e).unwrap().assign(name, value),
                None => Err(format!("Undefined variable: {}.", &name.lexeme))
            }
        }
    }

    fn get_enclosing(&self) -> Option<Rc<Environment>> {
        match self.enclosing {
            Some(ref e) => Some(Rc::clone(e)),
            None => None
        }
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: Rc::new(Environment::new_global())
        }
    }

    pub fn execute(&mut self, statements: &[Stmt]) {
        for s in statements.iter() {
            match self.execute_single(s) {
                Err(e) => println!("Runtime error: {}", e),
                _ => {}
            }
        }
    }

    fn execute_single(&mut self, statement: &Stmt) -> Result<(), String> {
        match statement {
            Stmt::Print { expr } => {
                let value = self.evaluate(&expr)?;
                println!("{}", value);
                Ok(())
            }

            Stmt::Expression { expr } => {
                self.evaluate(&expr)?;
                Ok(())
            }

            Stmt::Var { name, initializer } => {
                let val = match initializer {
                    Some(ref e) => Some(self.evaluate(e)?),
                    None => None
                };
                Rc::get_mut(&mut self.env).unwrap().define(name.lexeme.clone(), val);
                Ok(())
            }

            Stmt::Block { statements } => {
                let current_env = Rc::clone(&self.env);
                self.execute_block(&statements, Rc::new(Environment::new_with_enclosing(current_env)))
            }

            Stmt::If { condition, then_branch, else_branch} => {
                if is_truthy(&self.evaluate(&condition)?) {
                    self.execute_single(&then_branch)?;
                } else {
                    if let Some(e) = else_branch {
                        self.execute_single(&e)?;
                    }
                }
                Ok(())
            }

            Stmt::While { condition, body } => {
                while is_truthy(&self.evaluate(&condition)?) {
                    self.execute_single(&body)?;
                }
                Ok(())
            }
        }
    }

    fn execute_block(&mut self, statements: &Vec<Stmt>, env: Rc<Environment>) -> Result<(), String> {
        self.env = env;

        for stmt in statements {
            let exec_res = self.execute_single(stmt);
            if let Err(_) = &exec_res {
                self.env = self.env.get_enclosing().unwrap();
                return exec_res;
            }
        }
        self.env = self.env.get_enclosing().unwrap();
        Ok(())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<ExprVal, String> {
        match expr {
            Expr::Literal { value } => Ok(value.clone()),

            Expr::Grouping { expr } => self.evaluate(&expr),

            Expr::Unary { operator, right } => {
                let operand = self.evaluate(&right)?;
                match operator.token_type {
                    TokenType::Minus => {
                        if let ExprVal::Double(d) = operand {
                            Ok(ExprVal::Double(-d))
                        } else {
                            Err("Expected operand to be double".to_string())
                        }
                    }
                    TokenType::Bang => {
                        Ok(ExprVal::Boolean(is_truthy(&operand)))
                    }
                    _ => panic!("Illegal token type for unary operator")
                }
            }

            Expr::Binary { left, operator, right } => {
                let left_val = self.evaluate(&left)?;
                let right_val = self.evaluate(&right)?;
                match operator.token_type {
                    TokenType::Minus => {
                        match (left_val, right_val) {
                            (ExprVal::Double(l), ExprVal::Double(r)) => Ok(ExprVal::Double(l - r)),
                            _ => Err("Expected operands to be double".to_string())
                        }
                    }
                    TokenType::Slash => {
                        match (left_val, right_val) {
                            (ExprVal::Double(l), ExprVal::Double(r)) => Ok(ExprVal::Double(l / r)),
                            _ => Err("Expected operands to be double".to_string())
                        }
                    }
                    TokenType::Star => {
                        match (left_val, right_val) {
                            (ExprVal::Double(l), ExprVal::Double(r)) => Ok(ExprVal::Double(l * r)),
                            _ => Err("Expected operands to be double".to_string())
                        }
                    }
                    TokenType::Plus => {
                        match (left_val, right_val) {
                            (ExprVal::Double(l), ExprVal::Double(r)) => Ok(ExprVal::Double(l + r)),
                            (ExprVal::String(l), ExprVal::String(r)) => {
                                let concat = format!("{}{}", l, r);
                                Ok(ExprVal::String(concat))
                            }
                            _ => Err("Expected operands to be strings or doubles".to_string())
                        }
                    }
                    TokenType::GreaterEqual => {
                        match (left_val, right_val) {
                            (ExprVal::Double(l), ExprVal::Double(r)) => Ok(ExprVal::Boolean(l >= r)),
                            _ => Err("Expected operands to be double".to_string())
                        }
                    }
                    TokenType::Greater => {
                        match (left_val, right_val) {
                            (ExprVal::Double(l), ExprVal::Double(r)) => Ok(ExprVal::Boolean(l > r)),
                            _ => Err("Expected operands to be double".to_string())
                        }
                    }
                    TokenType::LessEqual => {
                        match (left_val, right_val) {
                            (ExprVal::Double(l), ExprVal::Double(r)) => Ok(ExprVal::Boolean(l <= r)),
                            _ => Err("Expected operands to be double".to_string())
                        }
                    }
                    TokenType::Less => {
                        match (left_val, right_val) {
                            (ExprVal::Double(l), ExprVal::Double(r)) => Ok(ExprVal::Boolean(l < r)),
                            _ => Err("Expected operands to be double".to_string())
                        }
                    }
                    TokenType::BangEqual => {
                        Ok(ExprVal::Boolean(left_val != right_val))
                    }
                    TokenType::EqualEqual => {
                        Ok(ExprVal::Boolean(left_val == right_val))
                    }
                    _ => panic!("Illegal token type for binary operator")
                }
            }

            Expr::Variable { name } => {
                let val = self.env.get(name.lexeme.clone())?;
                match val {
                    Some(v) => Ok(v),
                    None => Ok(ExprVal::Nil),
                }
            }

            Expr::Assign { name, value } => {
                let value = self.evaluate(&value)?;
                Rc::get_mut(&mut self.env).unwrap().assign(name.clone(), value.clone())?;
                Ok(value.clone())
            }

            Expr::Logical {left, operator, right} => {
                let left = self.evaluate(&left)?;

                if operator.token_type == TokenType::Or {
                    if is_truthy(&left) {
                        return Ok(left)
                    }
                } else {
                    if !is_truthy(&left) {
                        return Ok(left)
                    }
                }

                self.evaluate(&right)
            }
        }
    }
}

fn is_truthy(val: &ExprVal) -> bool {
    match val {
        &ExprVal::Nil => false,
        &ExprVal::Boolean(b) => b,
        _ => true
    }
}