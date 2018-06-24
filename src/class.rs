use std::fmt;
use std::rc::Rc;

use scan::*;
use functions::RloxCallable;
use interpreter::*;
use expression::*;

#[derive(Clone)]
pub struct RloxClass {
    pub name: Token
}

#[derive(Clone)]
pub struct ClassInstance {
    pub class: Rc<RloxClass>,
}

impl fmt::Display for RloxClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "class {}", &self.name.lexeme)
    }
}

impl fmt::Display for ClassInstance {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} instance", &self.class.name.lexeme)
    }
}

impl RloxCallable for RloxClass {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<ExprVal>) -> Result<ExprVal, IError> {
        Ok(ExprVal::ClassInstance(ClassInstance { class: Rc::new(self.clone()) }))
    }

    fn arity(&self) -> usize {
        0
    }
}

