use std::fmt;
use std::rc::Rc;
use std::collections::HashMap;
use std::cell::RefCell;

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
    pub fields: HashMap<String, ExprVal>,
}

impl ClassInstance {
    fn new(class: Rc<RloxClass>) -> ClassInstance {
        ClassInstance { class, fields: HashMap::new() }
    }

    pub fn get(&self, name: &str) -> IResult<ExprVal> {
        if let Some(e) = self.fields.get(name) {
            Ok(e.clone())
        } else {
            Err(IError::Error(format!("Undefined property: {}.", name)))
        }
    }

    pub fn set(&mut self, name: &str, value: ExprVal) {
        self.fields.insert(name.to_string(), value);
    }
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
        Ok(ExprVal::ClassInstance(Rc::new(RefCell::new(ClassInstance::new(Rc::new(self.clone()))))))
    }

    fn arity(&self) -> usize {
        0
    }
}

