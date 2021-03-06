use std::time::{SystemTime, UNIX_EPOCH};
use std::rc::Rc;
use std::cell::RefCell;

use interpreter::*;
use expression::ExprVal;
use stmt::FunctionBody;
use class::ClassInstance;

pub trait RloxCallable {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<ExprVal>) -> IResult<ExprVal>;
    fn arity(&self) -> usize;
}

pub struct RloxFunction {
    declaration: FunctionBody,
    closure: Rc<RefCell<Environment>>,
    is_initializer: bool
}

impl RloxFunction {
    pub fn new(declaration: FunctionBody, closure: Rc<RefCell<Environment>>, is_initializer: bool) -> RloxFunction {
        RloxFunction { declaration, closure, is_initializer }
    }

    pub fn bind(&self, instance: Rc<RefCell<ClassInstance>>, is_initializer: bool) -> RloxFunction {
        let mut env_with_this = Environment::new_with_enclosing(Rc::clone(&self.closure));
        env_with_this.define("this".to_string(), Some(ExprVal::ClassInstance(instance)));
        RloxFunction::new(self.declaration.clone(), Rc::new(RefCell::new(env_with_this)), is_initializer)
    }
}

impl RloxCallable for RloxFunction {

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<ExprVal>) -> IResult<ExprVal> {
        let func_env = Rc::new(RefCell::new(Environment::new_with_enclosing(Rc::clone(&self.closure))));
        for (i, p) in self.declaration.parameters.iter().enumerate() {
            func_env.borrow_mut().define(p.lexeme.clone(), Some(arguments[i].clone()));
        }

        let result = match interpreter.execute_block(&self.declaration.body, func_env) {
            Err(IError::Return(value)) => {
                Ok(value)
            },
            Err(IError::Break) => {
                panic!("Illegally placed break statement, should be a parse error")
            }
            Err(e) => Err(e),
            _ => Ok(ExprVal::Nil)
        };

        if !self.is_initializer {
            result
        } else {
            if let Err(_) = result {
                result
            } else {
                Ok(self.closure.borrow().get_at(0, "this").unwrap().unwrap())
            }
        }
    }

    fn arity(&self) -> usize {
        self.declaration.parameters.len()
    }
}

pub struct Clock {}

impl RloxCallable for Clock {
    fn call(&self, _interpreter: &mut Interpreter, _arguments: Vec<ExprVal>) -> IResult<ExprVal> {
        let start = SystemTime::now();

        let since_the_epoch = start.duration_since(UNIX_EPOCH)
            .expect("Can't happen");

        Ok(ExprVal::Double(since_the_epoch.as_secs() as f64))
    }

    fn arity(&self) -> usize {
        0
    }
}