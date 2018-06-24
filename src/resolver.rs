use std::collections::HashMap;
use std::mem;

use stmt::*;
use scan::*;
use expression::*;

pub struct Resolver<> {
    scopes: Vec<HashMap<String, bool>>,
    current_function_type: Option<FunctionType>,
}

#[derive(Copy, Clone)]
enum FunctionType {
    Function
}

type RResult<T> = Result<T, String>;

impl Resolver {
    pub fn new() -> Resolver {
        Resolver { scopes: Vec::new(), current_function_type: None }
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

            Stmt::Function { ref mut body, name } => {
                self.declare(&name)?;
                self.define(&name);

                self.resolve_function(body, FunctionType::Function)
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
                    Some(ref mut e) => self.resolve_expr(e),
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