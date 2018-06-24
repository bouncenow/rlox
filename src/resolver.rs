use std::collections::HashMap;

use stmt::*;
use scan::*;
use expression::*;

pub struct Resolver<> {
    scopes: Vec<HashMap<String, bool>>,
}

type RResult<T> = Result<T, String>;

impl Resolver {
    pub fn new() -> Resolver {
        Resolver { scopes: Vec::new() }
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
                self.declare(&name);
                match initializer {
                    Some(ref mut e) => self.resolve_expr(e)?,
                    None => {}
                };
                self.define(&name);
                Ok(())
            }

            Stmt::Function { ref mut decl } => {
                self.declare(&decl.name);
                self.define(&decl.name);

                self.resolve_function(decl)
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
                match value {
                    Some(ref mut e) => self.resolve_expr(e),
                    None => Ok(())
                }
            }

            Stmt::While { ref mut condition, ref mut body } => {
                self.resolve_expr(condition)?;
                self.resolve_single_stmt(body)
            }
        }
    }

    pub fn resolve_stmts(&mut self, stmts: &mut Vec<Stmt>) -> RResult<()> {
        for stmt in stmts {
            self.resolve_single_stmt(stmt)?;
        }
        Ok(())
    }

    fn resolve_function(&mut self, func_decl: &mut FunctionDecl) -> RResult<()> {
        self.begin_scope();
        for p in func_decl.parameters.iter() {
            self.declare(p);
            self.define(p);
        }

        self.resolve_stmts(&mut func_decl.body)?;
        self.end_scope();
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &mut Expr) -> RResult<()> {
        match expr {
            Expr::Variable { ref mut name, ref mut resolve_at } => {
                if let Some(s) = self.scopes.last() {
                    if let Some(&false) = s.get(&name.lexeme) {
                        return Err("Cannot read local variable in its own initializer.".to_string());
                    }
                }

                if let Some(d) = self.resolve_local_distance(&name) {
                    *resolve_at = Some(d);
                }
                Ok(())
            }

            Expr::Assign { name, ref mut value, ref mut resolve_at } => {
                self.resolve_expr(value)?;
                if let Some(d) = self.resolve_local_distance(&name) {
                    *resolve_at = Some(d);
                }
                Ok(())
            }

            Expr::Binary { ref mut left, ref mut right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            }

            Expr::Call { ref mut callee, ref mut arguments, .. } => {
                self.resolve_expr(callee)?;
                for a in arguments.iter_mut() {
                    self.resolve_expr(a)?;
                }

                Ok(())
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
        }
    }

    fn resolve_local_distance(&mut self, name: &Token) -> Option<usize> {
        for (i, s) in self.scopes.iter().rev().enumerate() {
            if s.contains_key(&name.lexeme) {
                /*self.interpreter.resolve(expr, i);*/
                return Some(i)
            }
        }

        None
    }

    fn declare(&mut self, name: &Token) {
        match self.scopes.last_mut() {
            Some(scope) => {
                scope.insert(name.lexeme.clone(), false);
            },
            None => {}
        };
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