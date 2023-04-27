//! Visiting the AST to execute the program.
//!
//! This is where the program is effectively being executed. This module ties
//! all modules into one.

use std::collections::HashMap;

use string_builder::Builder as StringBuilder;
use Expr::*;
use Stmt::*;
use Value::*;

use super::env::Env;
use super::value::{Value, ValueRef};
use crate::tast::{Block, Expr, Fun, Stmt, Var};

/// Interpreter for our language.
pub(crate) struct Interpreter<'a> {
    /// The execution environment stack.
    pub env: Vec<Env>,
    /// Map between function names and their definition.
    pub fun_env: &'a HashMap<String, Fun>,
    /// Standard output, as a growable string.
    pub stdout: StringBuilder,
}

impl<'a> Interpreter<'a> {
    /// Shortcut to produce the result of a call to an integer binop operation
    /// `f` that takes two integers and returns a value.
    fn int_binop(
        &mut self,
        f: impl Fn(&rug::Integer, &rug::Integer) -> Value,
        args: &Vec<ValueRef>,
    ) -> Option<ValueRef> {
        let lhs = *args.get(0)?;
        let rhs = *args.get(1)?;
        match (self.get_value(lhs), self.get_value(rhs)) {
            (IntV(lhs), IntV(rhs)) if args.len() == 2 => Some(self.add_value(f(lhs, rhs))),
            _ => None,
        }
    }

    /// Checks if we can apply builtin functions to the call to
    /// `f_name(..args)`.
    fn check_builtin(&mut self, f_name: &str, args: &Vec<ValueRef>) -> Option<ValueRef> {
        match f_name {
            "+" => self.int_binop(|x, y| IntV((x + y).into()), args),
            "-" => self.int_binop(|x, y| IntV((x - y).into()), args),
            "*" => self.int_binop(|x, y| IntV((x * y).into()), args),
            "/" => self.int_binop(|x, y| IntV((x / y).into()), args),
            "%" => self.int_binop(|x, y| IntV((x % y).into()), args),
            "==" => self.int_binop(|x, y| BoolV(x == y), args),
            "!=" => self.int_binop(|x, y| BoolV(x != y), args),
            ">=" => self.int_binop(|x, y| BoolV(x >= y), args),
            ">" => self.int_binop(|x, y| BoolV(x > y), args),
            "<=" => self.int_binop(|x, y| BoolV(x <= y), args),
            "<" => self.int_binop(|x, y| BoolV(x < y), args),
            "print" => {
                let mut args = args.iter();
                // Special case for the first item, which may not have a space before.
                if let Some(&arg) = args.next() {
                    let val = self.get_value(arg);
                    self.stdout.append(format!("{val}"));
                }

                for &arg in args {
                    let val = self.get_value(arg);
                    self.stdout.append(format!(" {val}"));
                }

                self.stdout.append("\n");
                Some(self.add_value(UnitV))
            }
            _ => None,
        }
    }

    /// Executes a call to a function in scope.
    pub fn call(&mut self, f_name: impl AsRef<str>, args: Vec<ValueRef>) -> ValueRef {
        let f_name = f_name.as_ref();

        // Override in case of builtin.
        if let Some(res) = self.check_builtin(f_name, &args) {
            res
        } else {
            self.push_scope();
            let f = self
                .fun_env
                .get(f_name)
                .unwrap_or_else(|| panic!("Runtime error: {f_name} is not defined"));

            // Check number of arguments.
            assert_eq!(
                f.params.len(),
                args.len(),
                "Runtime error: Mismatch in the number of arguments for function call to {}",
                f.name
            );

            // Introduce arguments in the typing context
            for (arg, value) in f.params.iter().zip(args.iter()) {
                self.add_var(arg.name.clone(), *value);
            }

            let res = self.visit_block(&f.body).to_owned();

            // Request our value back!
            let value = self.pop_scope(res).unwrap();

            self.add_value(value)
        }
    }

    /// Creates a new scope.
    fn push_scope(&mut self) {
        self.env.push(Env::default());
    }

    /// Pops and removes the current scope.
    ///
    /// Give as argument a value you want to retrieve before popping that scope,
    /// and it will give it back to you. Now or never to retrieve values in the
    /// scope, other will be freed!
    fn pop_scope(&mut self, value: ValueRef) -> Option<Value> {
        // Refuse the pop the static environment
        // And refuse to pop if the requested value is not in that scope.
        if self.env.len() >= 2 && value.stratum == self.env.len() - 1 {
            self.env.pop().map(|env| env.close(value))
        } else {
            None
        }
    }

    /// Gets the definition of a variable.
    fn get_var(&self, v: impl AsRef<Var>) -> Option<ValueRef> {
        // Iterate over environments in reverse (last declared first processed)
        // order
        // Returns the first environment that has that variable declared
        let v = v.as_ref();
        self.env.iter().rev().find_map(|e| e.get_var(v)).copied()
    }

    /// Declares a new variable in the context.
    fn add_var(&mut self, name: impl Into<Var>, value: impl Into<ValueRef>) {
        self.env.last_mut().unwrap().add_var(name.into(), value);
    }

    /// Adds a value to the dynamic store/slab.
    fn add_value(&mut self, value: Value) -> ValueRef {
        ValueRef {
            stratum: self.env.len() - 1,
            key: self.env.last_mut().unwrap().add_value(value),
        }
    }

    /// Gets a value from the dynamic store/slab.
    fn get_value(&self, value: ValueRef) -> &Value {
        self.env[value.stratum].get_value(value.key).unwrap()
    }

    /// Returns the final standard output of the execution of the program.
    pub fn stdout(self) -> String {
        self.stdout.string().unwrap()
    }
}

impl Interpreter<'_> {
    /// Executes an expression.
    fn visit_expr(&mut self, e: &Expr) -> ValueRef {
        match e {
            UnitE => self.add_value(UnitV),
            IntegerE(i) => self.add_value(IntV(i.clone())),
            StringE(s) => self.add_value(StrV(s.clone())),
            VarE(v) => self
                .get_var(v)
                .unwrap_or_else(|| panic!("Runtime error: unknown variable {}", v.name)),
            CallE { name, args } => {
                let args = args.iter().map(|arg| self.visit_expr(arg)).collect();
                self.call(name, args)
            }
            IfE(box cond, box iftrue, box iffalse) => {
                let cond = self.visit_expr(cond);
                if self.get_value(cond).truth() {
                    self.visit_block(iftrue)
                } else {
                    self.visit_block(iffalse)
                }
            }
            BlockE(box e) => self.visit_block(e),
            CopyE(box e) => self.visit_expr(e), // no-op
            OwnE(box e) => self.visit_expr(e),
            FieldE(box strukt, field) => {
                let strukt = self.visit_expr(strukt);
                if let StructV(_, fields) = self.get_value(strukt) {
                    fields[field]
                } else {
                    panic!("Runtime error: value should be a structure");
                }
            }
            StructE { name, fields } => {
                let fields = fields
                    .iter()
                    .map(|(k, v)| (k.clone(), self.visit_expr(v)))
                    .collect();
                self.add_value(StructV(name.to_owned(), fields))
            }
        }
    }

    /// Executes a block.
    fn visit_block(&mut self, b: &Block) -> ValueRef {
        for stmt in &b.stmts {
            self.visit_stmt(stmt);
        }
        self.visit_expr(&b.ret)
    }

    /// Executes a statement.
    fn visit_stmt(&mut self, s: &Stmt) {
        match s {
            Declare(v, e) | Assign(v, e) => {
                let e = self.visit_expr(e);
                self.add_var(v.name.clone(), e);
            }
            ExprS(e) => {
                self.visit_expr(e);
            }
            While { cond, body } => {
                while {
                    let e = self.visit_expr(cond);
                    self.get_value(e).truth()
                } {
                    self.visit_block(body);
                }
            }
        }
    }
}
