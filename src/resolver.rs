use std::collections::HashMap;
use std::mem;

use stmt::*;
use scan::*;
use expression::*;

pub struct Resolver<> {
    scopes: Vec<HashMap<String, bool>>,
    current_function_type: Option<FunctionType>,
    current_class_type: Option<ClassType>,
}

#[derive(Copy, Clone)]
enum FunctionType {
    Function,
    Method,
    Initializer,
}

#[derive(Copy, Clone)]
enum ClassType {
    Class,
    Subclass,
}

type RResult<T> = Result<T, String>;

impl Resolver {
    pub fn new() -> Resolver {
        Resolver { scopes: Vec::new(), current_function_type: None, current_class_type: None }
    }

    pub fn resolve_single_stmt(&mut self, stmt: &mut Stmt) -> RResult<()> {
        match stmt {
            Stmt::Block { ref mut statements } => {
                self.begin_scope();
                self.resolve_stmts(statements)?;
                self.end_scope();
                Ok(())
            }

            Stmt::Var { name, initializer } => {
                self.declare(&name)?;
                match initializer {
                    Some(ref mut e) => self.resolve_expr(e)?,
                    None => {}
                };
                self.define(&name);
                Ok(())
            }

            Stmt::Function { ref mut decl } => {
                self.declare(&decl.name)?;
                self.define(&decl.name);

                self.resolve_function(&mut decl.body, FunctionType::Function)
            }

            Stmt::Class { ref name, ref mut methods, ref mut superclass } => {
                let enclosing = mem::replace(&mut self.current_class_type, Some(ClassType::Class));
                self.declare(name)?;

                if let Some(v) = superclass {
                    self.current_class_type = Some(ClassType::Subclass);
                    self.resolve_variable(v)?;
                }

                self.define(name);

                if let Some(_v) = superclass {
                    self.begin_scope();
                    match self.scopes.last_mut() {
                        Some(s) => s.insert("super".to_string(), true),
                        None => panic!("Scope stack should not be empty!")
                    };
                };

                self.begin_scope();
                match self.scopes.last_mut() {
                    Some(s) => s.insert("this".to_string(), true),
                    None => panic!("Scope stack should not be empty!"),
                };

                for ref mut method in methods {
                    let function_type = if method.name.lexeme == "init" {
                        FunctionType::Initializer
                    } else {
                        FunctionType::Method
                    };
                    self.resolve_function(&mut method.body, function_type)?;
                }

                self.end_scope();

                if let Some(_v) = superclass {
                    self.end_scope();
                }

                self.current_class_type = enclosing;
                Ok(())
            }

            Stmt::Expression { ref mut expr } => {
                self.resolve_expr(expr)
            }

            Stmt::If { ref mut condition, ref mut then_branch, ref mut else_branch } => {
                self.resolve_expr(condition)?;
                self.resolve_single_stmt(then_branch)?;
                match else_branch {
                    Some(ref mut s) => self.resolve_single_stmt(s),
                    None => Ok(())
                }
            }

            Stmt::Print { ref mut expr } => {
                self.resolve_expr(expr)
            }

            Stmt::Return { value, .. } => {
                if let None = self.current_function_type {
                    return Err("Cannot return from top-level code".to_string());
                }
                match value {
                    Some(ref mut e) => {
                        if let Some(FunctionType::Initializer) = self.current_function_type {
                            return Err("Cannot return a value from initializer".to_string());
                        }
                        self.resolve_expr(e)
                    },
                    None => Ok(())
                }
            }

            Stmt::While { ref mut condition, ref mut body } => {
                self.resolve_expr(condition)?;
                self.resolve_single_stmt(body)
            }

            &mut Stmt::Break => Ok(())
        }
    }

    pub fn resolve_stmts(&mut self, stmts: &mut Vec<Stmt>) -> RResult<()> {
        for stmt in stmts {
            self.resolve_single_stmt(stmt)?;
        }
        Ok(())
    }

    fn resolve_function(&mut self, func_decl: &mut FunctionBody, function_type: FunctionType) -> RResult<()> {
        let enclosing = mem::replace(&mut self.current_function_type, Some(function_type));
        self.begin_scope();
        for p in func_decl.parameters.iter() {
            self.declare(p)?;
            self.define(p);
        }

        self.resolve_stmts(&mut func_decl.body)?;
        self.end_scope();
        self.current_function_type = enclosing;
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &mut Expr) -> RResult<()> {
        match expr {
            Expr::Variable { ref mut v } => {
                self.resolve_variable(v)
            }

            Expr::Assign { name, ref mut value, ref mut resolve_at } => {
                self.resolve_expr(value)?;
                if let Some(d) = self.resolve_local_distance(&name) {
                    *resolve_at = Some(d);
                }
                Ok(())
            }

            Expr::This { keyword, ref mut resolve_at } => {
                if let None = self.current_class_type {
                    return Err("Cannot use this outside of the class".to_string());
                }
                if let Some(d) = self.resolve_local_distance(&keyword) {
                    *resolve_at = Some(d);
                }
                Ok(())
            }

            Expr::Binary { ref mut left, ref mut right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            }

            Expr::FunctionExpr { ref mut body } => {
                self.resolve_function(body, FunctionType::Function)
            }

            Expr::Call { ref mut callee, ref mut arguments, .. } => {
                self.resolve_expr(callee)?;
                for a in arguments.iter_mut() {
                    self.resolve_expr(a)?;
                }

                Ok(())
            }

            Expr::Get { ref mut object, .. } => {
                self.resolve_expr(object)
            }

            Expr::Set { ref mut object, ref mut value, ..} => {
                self.resolve_expr(object)?;
                self.resolve_expr(value)
            }

            Expr::Grouping { ref mut expr } => {
                self.resolve_expr(expr)
            }

            Expr::Literal { .. } => {
                Ok(())
            }

            Expr::Logical { ref mut left, ref mut right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            }

            Expr::Unary { ref mut right, .. } => {
                self.resolve_expr(right)
            }

            Expr::Super { keyword, ref mut resolve_at, .. } => {
                if let None = self.current_class_type {
                    return Err("Cannot use outside of a class".to_string());
                } else if let Some(ClassType::Class) = self.current_class_type {
                    return Err("Cannot use outside of a subclass".to_string());
                }
                if let Some(d) = self.resolve_local_distance(&keyword) {
                    *resolve_at = Some(d);
                }
                Ok(())
            }
        }
    }

    fn resolve_variable(&mut self, v: &mut Variable) -> RResult<()> {
        if let Some(s) = self.scopes.last() {
            if let Some(&false) = s.get(&v.name.lexeme) {
                return Err("Cannot read local variable in its own initializer.".to_string());
            }
        }

        if let Some(d) = self.resolve_local_distance(&v.name) {
            v.resolve_at = Some(d);
        }
        Ok(())
    }

    fn resolve_local_distance(&mut self, name: &Token) -> Option<usize> {
        for (i, s) in self.scopes.iter().rev().enumerate() {
            if s.contains_key(&name.lexeme) {
                return Some(i)
            }
        }

        None
    }

    fn declare(&mut self, name: &Token) -> RResult<()> {
        match self.scopes.last_mut() {
            Some(scope) => {
                if scope.contains_key(&name.lexeme) {
                    Err("Variable with this name already declared in this scope".to_string())
                } else {
                    scope.insert(name.lexeme.clone(), false);
                    Ok(())
                }
            },
            None => Ok(())
        }
    }

    fn define(&mut self, name: &Token) {
        match self.scopes.last_mut() {
            Some(scope) => {
                scope.insert(name.lexeme.clone(), true);
            },
            None => {}
        };
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}