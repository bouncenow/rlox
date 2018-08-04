use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;

use expression::*;
use scan::TokenType;
use scan::Token;
use stmt::*;
use functions::*;
use class::*;

pub struct Interpreter {
    globals: Rc<RefCell<Environment>>,
    current_env: Rc<RefCell<Environment>>
}

pub struct Environment {
    values: HashMap<String, Option<ExprVal>>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    fn new_global() -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn new_with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn define(&mut self, name: String, value: Option<ExprVal>) {
        self.values.insert(name, value);
    }

    fn get(&self, name: &str) -> IResult<Option<ExprVal>> {
        match self.values.get(&name as &str) {
            Some(v) => Ok(v.clone()),
            None => {
                match self.enclosing {
                    Some(ref e) => e.borrow().get(name),
                    None => Err(IError::Error(format!("Undeclared variable: {}", name)))
                }
            }
        }
    }

    fn assign(&mut self, name: Token, value: ExprVal) -> IResult<()> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme, Some(value));
            Ok(())
        } else {
            match &self.enclosing {
                Some(e) => {
                    e.borrow_mut().assign(name, value)
                }
                None => Err(IError::Error(format!("Undefined variable: {}.", &name.lexeme)))
            }
        }
    }

    pub fn get_at(&self, distance: usize, name: &str) -> IResult<Option<ExprVal>> {
        if distance > 0 {
            let ancestor_env = self.ancestor(distance);
            let ancestor_env_borrowed = ancestor_env.borrow();
            Ok(ancestor_env_borrowed.get(name)?.clone())
        } else {
            Ok(self.get(name)?.clone())
        }
    }

    fn assign_at(&mut self, distance: usize, name: Token, value: ExprVal) -> IResult<()> {
        if distance > 0 {
            let ancestor_env = self.ancestor(distance);
            let mut ancestor_env_borrowed = ancestor_env.borrow_mut();
            ancestor_env_borrowed.assign(name, value)
        } else {
            self.assign(name, value)
        }
    }

    fn ancestor(&self, distance: usize) -> Rc<RefCell<Environment>> {
        if distance < 1 {
            panic!("Can't get ancestor for 0 level (use self)")
        }
        let mut current = match &self.enclosing {
            Some(e) => Rc::clone(e),
            _ => panic!("Wrongly resolved variable")
        };
        for _ in 0..(distance - 1) {
            let enclosing = match &current.borrow().enclosing {
                Some(e) => Rc::clone(e),
                None => panic!("Wrongly resolved variable")
            };
            current = enclosing;
        }

        current
    }
}

pub type IResult<T> = Result<T, IError>;

#[derive(Debug)]
pub enum IError {
    Error(String),
    Return(ExprVal),
    Break,
}

impl fmt::Display for IError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            IError::Error(s) => write!(f, "Runtime error: {}", s),
            IError::Return(e) => write!(f, "Return statement: {}", e),
            &IError::Break => write!(f, "Break statement"),
        }
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut global = Environment::new_global();
        global.define("clock".to_string(), Some(ExprVal::Callable(Rc::new(Clock {}))));
        let globals = Rc::new(RefCell::new(global));
        let current_env = Rc::clone(&globals);
        Interpreter {
            globals,
            current_env
        }
    }

    pub fn get_global_env(&self) -> Rc<RefCell<Environment>> {
        Rc::clone(&self.globals)
    }

    pub fn execute(&mut self, statements: &[Stmt]) {
        for s in statements.iter() {
            match self.execute_single(s) {
                Err(IError::Return(_)) => panic!("Illegally placed return statement, should be a \
                    resolve error"),
                Err(IError::Break) => panic!("Illegally placed break statement, should be a parse \
                    error"),
                Err(e) => println!("Runtime error: {}", e),
                _ => {}
            }
        }
    }

    fn execute_single(&mut self, statement: &Stmt) -> IResult<()> {
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
                self.current_env.borrow_mut().define(name.lexeme.clone(), val);
                Ok(())
            }

            Stmt::Block { statements } => {
                let current_env = Rc::clone(&self.current_env);
                self.execute_block(&statements, Rc::new(RefCell::new(Environment::new_with_enclosing(current_env))))
            }

            &Stmt::Break => {
                Err(IError::Break)
            }

            Stmt::If { condition, then_branch, else_branch } => {
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
                    match self.execute_single(&body){
                        Err(IError::Break) => break,
                        Err(e) => return Err(e),
                        _ => {}
                    };
                }
                Ok(())
            }

            Stmt::Function { decl } => {
                let function = RloxFunction::new(decl.body.clone(), Rc::clone(&self.current_env), false);
                self.current_env.borrow_mut().define(decl.name.lexeme.clone(),
                                                                   Some(ExprVal::Callable(Rc::new(function))));
                Ok(())
            }

            Stmt::Class { name, methods } => {

                let mut methods_with_names = HashMap::new();
                for method_decl in methods.iter() {
                    let is_initializer = method_decl.name.lexeme == "init";
                    let method = Rc::new(RloxFunction::new(method_decl.body.clone(), Rc::clone(&self.current_env), is_initializer));
                    methods_with_names.insert(method_decl.name.lexeme.clone(), method);
                }

                let class = ExprVal::Callable(Rc::new(RloxClass::new(name.clone(), methods_with_names)));
                self.current_env.borrow_mut().define(name.lexeme.clone(), Some(class));
                Ok(())
            }

            Stmt::Return { keyword: _, value } => {
                let value = match value {
                    Some(e) => self.evaluate(&e)?,
                    None => ExprVal::Nil
                };

                Err(IError::Return(value))
            }
        }
    }

    pub fn execute_block(
        &mut self,
        statements: &Vec<Stmt>,
        block_env: Rc<RefCell<Environment>>
    ) -> IResult<()> {
        let previous_env = Rc::clone(&self.current_env);
        self.current_env = block_env;

        for stmt in statements {
            let exec_res = self.execute_single(stmt);
            if let Err(_) = &exec_res {
                self.current_env = previous_env;
                return exec_res;
            }
        }
        self.current_env = previous_env;
        Ok(())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> IResult<ExprVal> {
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
                            Err(IError::Error("Expected operand to be double".to_string()))
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
                            _ => Err(IError::Error("Expected operands to be double".to_string()))
                        }
                    }
                    TokenType::Slash => {
                        match (left_val, right_val) {
                            (ExprVal::Double(l), ExprVal::Double(r)) => Ok(ExprVal::Double(l / r)),
                            _ => Err(IError::Error("Expected operands to be double".to_string()))
                        }
                    }
                    TokenType::Star => {
                        match (left_val, right_val) {
                            (ExprVal::Double(l), ExprVal::Double(r)) => Ok(ExprVal::Double(l * r)),
                            _ => Err(IError::Error("Expected operands to be double".to_string()))
                        }
                    }
                    TokenType::Plus => {
                        match (left_val, right_val) {
                            (ExprVal::Double(l), ExprVal::Double(r)) => Ok(ExprVal::Double(l + r)),
                            (ExprVal::String(l), ExprVal::String(r)) => {
                                let concat = format!("{}{}", l, r);
                                Ok(ExprVal::String(concat))
                            },
                            (ExprVal::Double(l), ExprVal::String(r)) => {
                                let concat = format!("{}{}", l, r);
                                Ok(ExprVal::String(concat))
                            },
                            (ExprVal::String(l), ExprVal::Double(r)) => {
                                let concat = format!("{}{}", l, r);
                                Ok(ExprVal::String(concat))
                            }
                            _ => Err(IError::Error("Expected operands to be strings or doubles".to_string()))
                        }
                    }
                    TokenType::GreaterEqual => {
                        match (left_val, right_val) {
                            (ExprVal::Double(l), ExprVal::Double(r)) => Ok(ExprVal::Boolean(l >= r)),
                            _ => Err(IError::Error("Expected operands to be double".to_string()))
                        }
                    }
                    TokenType::Greater => {
                        match (left_val, right_val) {
                            (ExprVal::Double(l), ExprVal::Double(r)) => Ok(ExprVal::Boolean(l > r)),
                            _ => Err(IError::Error("Expected operands to be double".to_string()))
                        }
                    }
                    TokenType::LessEqual => {
                        match (left_val, right_val) {
                            (ExprVal::Double(l), ExprVal::Double(r)) => Ok(ExprVal::Boolean(l <= r)),
                            _ => Err(IError::Error("Expected operands to be double".to_string()))
                        }
                    }
                    TokenType::Less => {
                        match (left_val, right_val) {
                            (ExprVal::Double(l), ExprVal::Double(r)) => Ok(ExprVal::Boolean(l < r)),
                            _ => Err(IError::Error("Expected operands to be double".to_string()))
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

            Expr::Variable { name, resolve_at } => {
                let val = self.look_up_variable(name, *resolve_at)?;
                match val {
                    Some(v) => Ok(v),
                    None => Ok(ExprVal::Nil),
                }
            }

            Expr::Assign { name, value, resolve_at } => {
                let value = self.evaluate(&value)?;

                match resolve_at {
                    Some(d) => self.current_env.borrow_mut().assign_at(*d, name.clone(), value.clone())?,
                    None => self.globals.borrow_mut().assign(name.clone(), value.clone())?
                };

                Ok(value.clone())
            }

            Expr::Logical { left, operator, right } => {
                let left = self.evaluate(&left)?;

                if operator.token_type == TokenType::Or {
                    if is_truthy(&left) {
                        return Ok(left);
                    }
                } else {
                    if !is_truthy(&left) {
                        return Ok(left);
                    }
                }

                self.evaluate(&right)
            }

            Expr::FunctionExpr { body } => {
                let function = RloxFunction::new(body.clone(), Rc::clone(&self.current_env), false);
                Ok(ExprVal::Callable(Rc::new(function)))
            }

            Expr::Call { callee, paren: _, arguments } => {
                let callee = self.evaluate(&callee)?;

                let mut args_evaluated = Vec::new();
                for a in arguments.iter() {
                    let a_e = self.evaluate(a)?;
                    args_evaluated.push(a_e);
                }

                if let ExprVal::Callable(c) = callee {
                    if args_evaluated.len() == c.arity() {
                        c.call(self, args_evaluated)
                    } else {
                        Err(IError::Error(format!("Expected {} arguments but got {}", c.arity(), args_evaluated.len())))
                    }
                } else {
                    Err(IError::Error("Can only call functions and classes.".to_string()))
                }
            }

            Expr::Get { object, name } => {
                let object = self.evaluate(&object)?;
                if let ExprVal::ClassInstance(c) = object {
                    self.resolve_instance_property(Rc::clone(&c), &name)
                } else {
                    Err(IError::Error("Only instances have properties.".to_string()))
                }
            }

            Expr::Set { object, name, value } => {
                let object = self.evaluate(&object)?;
                if let ExprVal::ClassInstance(c) = object {
                    let value = self.evaluate(&value)?;
                    c.borrow_mut().set(&name.lexeme, value.clone());
                    Ok(value)
                } else {
                    Err(IError::Error("Only instances have fields.".to_string()))
                }
            }

            Expr::This { keyword, resolve_at } => {
                let val = self.look_up_variable(keyword, *resolve_at)?;
                match val {
                    Some(v) => Ok(v),
                    None => Ok(ExprVal::Nil),
                }
            }
        }
    }

    fn resolve_instance_property(&self, instance: Rc<RefCell<ClassInstance>>, name: &Token) -> IResult<ExprVal> {
        let borrowed_instance = instance.borrow();
        match borrowed_instance.get_instance_field(&name.lexeme) {
            Some(e) => Ok(e),
            None => {
                let class = Rc::clone(&borrowed_instance.class);
                match class.find_method(&name.lexeme, Rc::clone(&instance)) {
                    Some(m) => Ok(ExprVal::Callable(m)),
                    None => Err(IError::Error(format!("Unresolved method: {}", &name.lexeme)))
                }
            }
        }
    }

    fn look_up_variable(&mut self, name: &Token, resolve_at: Option<usize>) -> IResult<Option<ExprVal>> {
        match resolve_at {
            Some(d) => Ok(self.current_env.borrow().get_at(d, &name.lexeme)?),
            None => Ok(self.globals.borrow().get(&name.lexeme)?)
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