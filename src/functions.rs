use std::time::{SystemTime, UNIX_EPOCH};
use std::rc::Rc;
use std::cell::RefCell;

use interpreter::*;
use expression::ExprVal;
use stmt::FunctionDecl;

pub trait RloxCallable {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<ExprVal>) -> IResult<ExprVal>;
    fn arity(&self) -> usize;
}

pub struct RloxFunction {
    declaration: FunctionDecl,
    closure: Rc<RefCell<Environment>>
}

impl RloxFunction {
    pub fn new(declaration: FunctionDecl, closure: Rc<RefCell<Environment>>) -> RloxFunction {
        RloxFunction { declaration, closure }
    }
}

impl RloxCallable for RloxFunction {

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<ExprVal>) -> IResult<ExprVal> {
        let func_env = Rc::new(RefCell::new(Environment::new_with_enclosing(Rc::clone(&self.closure))));
        for (i, p) in self.declaration.parameters.iter().enumerate() {
            func_env.borrow_mut().define(p.lexeme.clone(), Some(arguments[i].clone()));
        }

        match interpreter.execute_block(&self.declaration.body, func_env) {
            Err(IError::Return(value)) => {
                Ok(value)
            },
            Err(e) => Err(e),
            _ => Ok(ExprVal::Nil)
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