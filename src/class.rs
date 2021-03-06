use std::fmt;
use std::rc::Rc;
use fnv::FnvHashMap;
use std::cell::RefCell;

use scan::*;
use functions::*;
use interpreter::*;
use expression::*;

#[derive(Clone)]
pub struct RloxClass {
    pub name: Token,
    pub methods: Rc<FnvHashMap<String, Rc<RloxFunction>>>,
    pub superclass: Option<Rc<RloxClass>>,
}

impl RloxClass {
    pub fn new(name: Token, methods: FnvHashMap<String, Rc<RloxFunction>>, superclass: Option<Rc<RloxClass>>) -> RloxClass {
        RloxClass { name, methods: Rc::new(methods), superclass }
    }

    pub fn find_method(&self, name: &str, instance: Rc<RefCell<ClassInstance>>) -> Option<Rc<RloxFunction>> {
        match self.methods.get(name) {
            Some(f) => {
                let bind_method = f.bind(instance, name == "init");
                Some(Rc::new(bind_method))
            }
            None => {
                match self.superclass {
                    Some(ref c) => c.find_method(name, instance),
                    None => None
                }
            },
        }
    }
}

#[derive(Clone)]
pub struct ClassInstance {
    pub class: Rc<RloxClass>,
    pub fields: FnvHashMap<String, ExprVal>,
}

impl ClassInstance {
    fn new(class: Rc<RloxClass>) -> ClassInstance {
        ClassInstance { class, fields: FnvHashMap::default() }
    }

    pub fn get_instance_field(&self, name: &str) -> Option<ExprVal> {
        if let Some(e) = self.fields.get(name) {
            Some(e.clone())
        } else {
            None
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
        let instance = Rc::new(
            RefCell::new(
                ClassInstance::new(
                    Rc::new(self.clone())
                )
            )
        );
        if let Some(initializer) = self.methods.get("init") {
            initializer.bind(Rc::clone(&instance), true).call(interpreter, arguments)?;
        }

        Ok(ExprVal::ClassInstance(instance))
    }

    fn arity(&self) -> usize {
        match self.methods.get("init") {
            Some(i) => i.arity(),
            None => 0
        }
    }
}

