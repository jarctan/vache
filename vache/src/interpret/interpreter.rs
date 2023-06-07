//! Visiting the CFG to execute the program.
//!
//! This is where the program is effectively being executed. This module brings
//! all other submodules together.

use std::collections::HashMap;

use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use string_builder::Builder as StringBuilder;
use Branch::*;
use Place::*;
use Value::*;

use super::env::Env;
use super::value::{Value, ValueRef};
use crate::mir::{Branch, CfgI, CfgLabel, Fun, InstrKind, Mode, Place, Pointer, RValue, Var};
use crate::tast::Stratum;

/// Interpreter for our language.
pub(crate) struct Interpreter<'a, 'ctx> {
    /// The execution environment stack.
    pub env: Vec<Env<'ctx>>,
    /// Map between function names and their definition.
    pub fun_env: &'a HashMap<&'ctx str, Fun<'ctx>>,
    /// Standard output, as a growable string.
    pub stdout: StringBuilder,
}

impl<'a, 'ctx> Interpreter<'a, 'ctx> {
    /// Shortcut to produce the result of a call to an integer binop operation
    /// `f` that takes two integers and returns a value.
    fn int_binop(
        &mut self,
        f: impl Fn(&BigInt, &BigInt) -> Value<'ctx>,
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
            "+" => self.int_binop(|x, y| IntV(x + y), args, stratum),
            "-" => self.int_binop(|x, y| IntV(x - y), args, stratum),
            "*" => self.int_binop(|x, y| IntV(x * y), args, stratum),
            "/" => self.int_binop(|x, y| IntV(x / y), args, stratum),
            "%" => self.int_binop(|x, y| IntV(x % y), args, stratum),
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
                self.add_var(arg.name);
                self.set_var(arg.name, *value);
            }

            self.visit_cfg(&f.body, f.entry_l);

            // Request the final value (if the function returns a value, of course)
            self.pop_scope(f.ret_v.as_ref().map(|ret_v| self.get_ptr(ret_v)))
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
    fn get_var(&self, v: impl AsRef<Var<'ctx>>) -> ValueRef {
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
    fn get_var_mut(&mut self, v: impl AsRef<Var<'ctx>>) -> &mut ValueRef {
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
    fn add_var(&mut self, name: impl Into<Var<'ctx>>) {
        self.env.last_mut().unwrap().add_var(name.into());
    }

    /// Assigns a variable.
    fn set_var(&mut self, name: impl AsRef<Var<'ctx>>, value: impl Into<ValueRef>) {
        let name = name.as_ref();
        *self.get_var_mut(name) = value.into();
    }

    /// Assigns `*ptr`.
    fn set_at_ptr(&mut self, ptr: impl Into<Pointer<'ctx>>, value: impl Into<ValueRef>) {
        *self.get_ptr_mut(ptr) = value.into();
    }

    /// Adds a value to the dynamic store/slab.
    fn add_value(&mut self, value: Value<'ctx>, stratum: Stratum) -> ValueRef {
        let stratum: usize = stratum.into();
        self.env[stratum].add_value(value)
    }

    /// Gets a value from the dynamic store/slab.
    fn get_value(&self, value: ValueRef) -> &Value<'ctx> {
        self.env[usize::from(value.stratum)]
            .get_value(value.key)
            .unwrap()
    }

    /// Gets a value from the dynamic store/slab.
    fn get_value_mut(&mut self, value: ValueRef) -> &mut Value<'ctx> {
        self.env[usize::from(value.stratum)]
            .get_value_mut(value.key)
            .unwrap()
    }

    /// Gets the value reference at a given pointer.
    fn get_ptr(&self, ptr: impl Into<Pointer<'ctx>>) -> ValueRef {
        let ptr = ptr.into();
        match ptr.place() {
            VarP(var) => self.get_var(var),
            FieldP(strukt, field) => {
                if let StructV(_, fields) = self.get_ptr_value(strukt) && let Some(var_ref) = fields.get(field) {
                    *var_ref
                } else{
                    panic!()
                }
            }
            IndexP(array, index) => {
                match (
                    self.get_ptr_value(array),
                    self.get_ptr_value(index),
                ) {
                    (ArrayV(array), IntV(index)) => {
                        let index = index
                            .to_usize()
                            .expect("Runtime error: array index is too big");
                        array[index]
                    }
                    _ => panic!("Runtime error: incorrect indexing"),
                }
            }
            DerefP(..) => panic!(),
        }
    }

    /// Mutably gets the value reference to a given location.
    fn get_ptr_mut(&mut self, ptr: impl Into<Pointer<'ctx>>) -> &mut ValueRef {
        let ptr = ptr.into();
        match ptr.place() {
            VarP(var) => self.get_var_mut(var),
            FieldP(strukt, field) => {
                if let StructV(_, fields) = self.get_ptr_value_mut(strukt) && let Some(var_ref) = fields.get_mut(field) {
                    var_ref
                } else{
                    panic!()
                }
            }
            IndexP(array, index) => {
                if let IntV(index) = self.get_ptr_value(index) {
                    
                    let index = index
                    .to_usize()
                    .expect("Runtime error: array index is too big");
                    if let ArrayV(array) = self.get_ptr_value_mut(array) {
                        &mut array[index]
                    } else {
                        panic!("Runtime error: incorrect indexing")
                    }
                } else {
                    panic!("Runtime error: incorrect indexing")
                }
            }
            DerefP(..) => panic!(),
        }
    }

    /// Gets the value at a given pointer location.
    fn get_ptr_value(&self, ptr: impl Into<Pointer<'ctx>>) -> &Value<'ctx> {
        self.get_value(self.get_ptr(ptr))
    }

    /// Mutably gets the value at a given pointer location.
    fn get_ptr_value_mut(&mut self, ptr: impl Into<Pointer<'ctx>>) -> &mut Value<'ctx> {
        self.get_value_mut(self.get_ptr(ptr))
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
            Mode::Cloned | Mode::SBorrowed => {
                self.add_value(self.get_value(v_ref).clone(), stratum)
            }
            Mode::Moved | Mode::Borrowed | Mode::MutBorrowed => {
                assert!(stratum >= v_ref.stratum, "Runtime error: ownership addressing should be specified as owned if moving variable out of its stratum");
                v_ref
            }
            Mode::Assigning => {
                panic!("Runtime error: expected cloning mode, but got assigning mode")
            }
        }
    }

    /// Visit a right-value.
    fn visit_rvalue(&mut self, rvalue: &RValue<'ctx>, stratum: Stratum) -> ValueRef {
        match rvalue {
            RValue::Unit => self.add_value(UnitV, stratum),
            RValue::Integer(i) => self.add_value(IntV((*i).clone()), stratum),
            RValue::String(s) => self.add_value(StrV(s.to_string()), stratum),
            RValue::Place(place) => {
                let v_ref = self.get_ptr(place.as_ptr());
                self.opt_clone(&place.mode(), v_ref, stratum)
            }
            /*RValue::MovedVar(v) => {
                let v_ref = self.get_var(v);
                self.opt_clone(&Mode::Moved, v_ref, stratum)
            }*/
            RValue::Struct { name, fields } => {
                let fields = fields.iter().map(|(&k, v)| (k, self.get_ptr(v))).collect();
                self.add_value(StructV(name, fields), stratum)
            }
            RValue::Array(array) => {
                let array = array.iter().map(|v| self.get_ptr(v)).collect();
                self.add_value(ArrayV(array), stratum)
            }
        }
    }

    /// Executes an expression, returning the first label that do not exist in
    /// the CFG. Often, this is the return/exit label.
    fn visit_cfg(&mut self, cfg: &CfgI<'ctx>, label: CfgLabel) {
        let branch = match &cfg[&label].kind {
            InstrKind::Noop => DefaultB,
            InstrKind::Declare(v) => {
                self.add_var(v.name);
                DefaultB
            }
            InstrKind::Assign(ptr, rvalue) => {
                let value = self.visit_rvalue(rvalue, self.get_ptr(ptr).stratum);
                self.set_at_ptr(*ptr, value);
                DefaultB
            }
            InstrKind::Call {
                name,
                args,
                destination,
            } => {
                let args = args.iter().map(|v| self.get_ptr(v)).collect();
                let stratum = destination
                    .as_ref()
                    .map_or(self.current_stratum(), |dest| self.get_ptr(dest).stratum);
                let call_result = self.call(name, args, stratum);
                if let Some(destination) = destination {
                    self.set_at_ptr(
                        destination,
                        call_result.expect(
                            "if the destination is set, then the function should return a value",
                        ),
                    );
                }
                DefaultB
            }
            InstrKind::Branch(cond) => {
                if self.get_ptr_value(cond).truth() {
                    TrueB
                } else {
                    FalseB
                }
            }
            // Do nothing: it was a marker instruction for the liveness analysis.
            InstrKind::Return(_) => DefaultB,
        };
        if let Some(next) = cfg.take_branch(label, &branch) {
            self.visit_cfg(cfg, next)
        }
    }
}
