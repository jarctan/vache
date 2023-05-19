//! Visiting the CFG to execute the program.
//!
//! This is where the program is effectively being executed. This module brings
//! all other submodules together.

use std::collections::HashMap;

use string_builder::Builder as StringBuilder;
use Branch::*;
use Place::*;
use Value::*;

use super::env::Env;
use super::value::{Value, ValueRef};
use crate::mir::{Branch, Cfg, CfgLabel, Fun, InstrKind, Mode, Place, RValue, Var};
use crate::tast::Stratum;

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
        stratum: Stratum,
    ) -> Option<ValueRef> {
        let lhs = *args.get(0)?;
        let rhs = *args.get(1)?;
        match (self.get_value(lhs), self.get_value(rhs)) {
            (IntV(lhs), IntV(rhs)) if args.len() == 2 => Some(self.add_value(f(lhs, rhs), stratum)),
            _ => None,
        }
    }

    /// Checks if we can apply builtin functions to the call to
    /// `f_name(..args)`.
    fn check_builtin(
        &mut self,
        f_name: &str,
        args: &Vec<ValueRef>,
        stratum: Stratum,
    ) -> Option<ValueRef> {
        match f_name {
            "+" => self.int_binop(|x, y| IntV((x + y).into()), args, stratum),
            "-" => self.int_binop(|x, y| IntV((x - y).into()), args, stratum),
            "*" => self.int_binop(|x, y| IntV((x * y).into()), args, stratum),
            "/" => self.int_binop(|x, y| IntV((x / y).into()), args, stratum),
            "%" => self.int_binop(|x, y| IntV((x % y).into()), args, stratum),
            "==" => self.int_binop(|x, y| BoolV(x == y), args, stratum),
            "!=" => self.int_binop(|x, y| BoolV(x != y), args, stratum),
            ">=" => self.int_binop(|x, y| BoolV(x >= y), args, stratum),
            ">" => self.int_binop(|x, y| BoolV(x > y), args, stratum),
            "<=" => self.int_binop(|x, y| BoolV(x <= y), args, stratum),
            "<" => self.int_binop(|x, y| BoolV(x < y), args, stratum),
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
                Some(self.add_value(UnitV, self.current_stratum()))
            }
            _ => None,
        }
    }

    /// Returns the id of the current stratum.
    pub fn current_stratum(&self) -> Stratum {
        Stratum::try_from(self.env.len() - 1).unwrap()
    }

    /// Executes a call to a function in scope.
    pub fn call(
        &mut self,
        f_name: impl AsRef<str>,
        args: Vec<ValueRef>,
        stratum: Stratum,
    ) -> Option<ValueRef> {
        let f_name = f_name.as_ref();

        // Override in case of builtin.
        if let Some(res) = self.check_builtin(f_name, &args, stratum) {
            Some(res)
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
                self.add_var(arg.name.clone());
                self.set_var(&arg.name, *value);
            }

            // Introduce return variable
            if let Some(ret_v) = &f.ret_v {
                self.add_var(ret_v.name.clone());
            }

            self.visit_cfg(&f.body, &f.entry_l);

            // Request the final value (if the function returns a value, of course)
            self.pop_scope(f.ret_v.as_ref().map(|ret_v| self.get_var(ret_v)))
        }
    }

    /// Creates a new scope.
    fn push_scope(&mut self) {
        self.env.push(Env::new(self.env.len().try_into().unwrap()));
    }

    /// Pops and removes the current scope.
    ///
    /// Give as argument a value you want to retrieve before popping that scope,
    /// and it will give it back to you. Now or never to retrieve values in the
    /// scope, other will be freed!
    fn pop_scope(&mut self, value: Option<ValueRef>) -> Option<ValueRef> {
        // Refuse the pop the static environment
        assert!(self.env.len() >= 2);
        let env = self.env.pop().unwrap();
        value.map(|value| {
            if let Some(value) = env.close(value) {
                self.add_value(value, self.current_stratum())
            } else {
                value
            }
        })
    }

    /// Gets the definition of a variable.
    fn get_var(&self, v: impl AsRef<Var>) -> ValueRef {
        let v = v.as_ref();
        // Iterate over environments in reverse (last declared first processed)
        // order
        // Returns the first environment that has that variable declared
        self.env
            .iter()
            .rev()
            .find_map(|e| e.get_var(v))
            .copied()
            .unwrap_or_else(|| panic!("Runtime error: variable {} should exist", v))
    }

    /// Gets a mutable reference into the value of a variable.
    fn get_var_mut(&mut self, v: impl AsRef<Var>) -> &mut ValueRef {
        // Iterate over environments in reverse (last declared first processed)
        // order
        // Returns the first environment that has that variable declared
        let v = v.as_ref();
        self.env
            .iter_mut()
            .rev()
            .find_map(|e| e.get_var_mut(v))
            .unwrap_or_else(|| panic!("Runtime error: variable {} should exist", v))
    }

    /// Declares a new variable in the context.
    fn add_var(&mut self, name: impl Into<Var>) {
        self.env.last_mut().unwrap().add_var(name.into());
    }

    /// Assigns a variable in the context.
    fn set_var(&mut self, name: impl AsRef<Var>, value: impl Into<ValueRef>) {
        let name = name.as_ref();
        *self.get_var_mut(name) = value.into();
    }

    /// Adds a value to the dynamic store/slab.
    fn add_value(&mut self, value: Value, stratum: Stratum) -> ValueRef {
        let stratum: usize = stratum.into();
        self.env[stratum].add_value(value)
    }

    /// Gets a value from the dynamic store/slab.
    fn get_value(&self, value: ValueRef) -> &Value {
        self.env[usize::from(value.stratum)]
            .get_value(value.key)
            .unwrap()
    }

    /// Gets a value from the dynamic store/slab.
    fn get_value_mut(&mut self, value: ValueRef) -> &mut Value {
        self.env[usize::from(value.stratum)]
            .get_value_mut(value.key)
            .unwrap()
    }

    /// Gets the value of a variable.
    fn get_var_value(&self, v: impl AsRef<Var>) -> &Value {
        self.get_value(self.get_var(v))
    }

    /// Returns the final standard output of the execution of the program.
    pub fn stdout(self) -> String {
        self.stdout.string().unwrap()
    }

    /// Gets a variable, possibly cloning it if its addressing mode and stratum
    /// require so.
    ///
    /// No-op if we decide not to clone, otherwise returns the identifier of the
    /// location of the cloned variable.
    pub fn opt_clone(&mut self, mode: &Mode, v_ref: ValueRef, stratum: Stratum) -> ValueRef {
        match mode {
            Mode::Cloned => self.add_value(self.get_value(v_ref).clone(), stratum),
            Mode::Moved | Mode::Borrowed | Mode::MutBorrowed => {
                assert!(stratum >= v_ref.stratum, "Runtime error: ownership addressing should be specified as owned if moving variable out of its stratum");
                v_ref
            }
        }
    }

    /// Visit a right-value.
    fn visit_rvalue(&mut self, rvalue: &'a RValue, stratum: Stratum) -> ValueRef {
        match rvalue {
            RValue::Unit => self.add_value(UnitV, stratum),
            RValue::Integer(i) => self.add_value(IntV(i.clone()), stratum),
            RValue::String(s) => self.add_value(StrV(s.clone()), stratum),
            RValue::Var(v) => {
                let v_ref = self.get_var(v);
                self.opt_clone(&v.mode, v_ref, stratum)
            }
            RValue::Field(lhs, field) => match self.get_var_value(lhs) {
                StructV(_, strukt) => strukt[field],
                _ => panic!("Runtime error: field access should only be on structs"),
            },
            RValue::Index(array, index, mode) => {
                match (self.get_var_value(array), self.get_var_value(index)) {
                    (ArrayV(array), IntV(index)) => {
                        let index = index
                            .to_usize()
                            .expect("Runtime error: array index is too big");
                        self.opt_clone(mode, array[index], stratum)
                    }
                    _ => panic!("Runtime error: incorrect indexing"),
                }
            }
            RValue::Struct { name, fields } => {
                let fields = fields
                    .iter()
                    .map(|(k, v)| (k.clone(), self.get_var(v)))
                    .collect();
                self.add_value(StructV(name.to_owned(), fields), stratum)
            }
            RValue::Array(array) => {
                let array = array.iter().map(|v| self.get_var(v)).collect();
                self.add_value(ArrayV(array), stratum)
            }
        }
    }

    /// Executes an expression, returning the first label that do not exist in
    /// the CFG. Often, this is the return/exit label.
    fn visit_cfg(&mut self, cfg: &'a Cfg, label: &CfgLabel) {
        let branch = match &cfg[label].kind {
            InstrKind::Noop => DefaultB,
            InstrKind::Declare(v) => {
                self.add_var(v.clone());
                DefaultB
            }
            InstrKind::Assign(VarP(v), rvalue) => {
                let value = self.visit_rvalue(rvalue, self.get_var(v).stratum);
                self.set_var(v, value);
                DefaultB
            }
            InstrKind::Assign(IndexP(array, index), rvalue) => {
                let array_ref = self.get_var(array);
                let stm = array_ref.stratum;

                let value = self.visit_rvalue(rvalue, stm);

                let index = self.get_var_value(index);
                let index = if let IntV(index) = index {
                    index.to_usize().expect("Index is too big")
                } else {
                    panic!("Runtime error: index should be an integer")
                };

                if let ArrayV(array) = self.get_value_mut(array_ref) {
                    array[index] = value;
                } else {
                    panic!("Runtime error: indexed element should be an array")
                }
                DefaultB
            }

            InstrKind::Assign(FieldP(_, _), _) => {
                todo!()
            }
            InstrKind::Call {
                name,
                args,
                destination,
            } => {
                let args = args.iter().map(|v| self.get_var(v)).collect();
                let stratum = destination
                    .as_ref()
                    .map_or(self.current_stratum(), |dest| self.get_var(dest).stratum);
                let call_result = self.call(name, args, stratum);
                if let Some(destination) = destination {
                    self.set_var(
                        destination,
                        call_result.expect(
                            "if the destination is set, then the function should return a value",
                        ),
                    );
                }
                DefaultB
            }
            InstrKind::Branch(cond) => {
                if self.get_var_value(cond).truth() {
                    TrueB
                } else {
                    FalseB
                }
            }
        };
        if let Some(next) = cfg.take_branch(label, &branch) {
            self.visit_cfg(cfg, next)
        }
    }
}