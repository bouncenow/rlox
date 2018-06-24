use std::fmt;

use scan::*;

#[derive(Clone)]
pub struct RloxClass {
    pub name: Token
}

impl fmt::Display for RloxClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "class {}", &self.name.lexeme)
    }
}


