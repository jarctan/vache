//! Visiting the CFG to execute the program.
//!
//! This is where the program is effectively being executed. This module brings
//! all other submodules together.

use std::borrow::Borrow;
use std::collections::HashMap;

use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use string_builder::Builder as StringBuilder;
use Branch::*;
use Place::*;
use Value::*;

use super::env::Env;
use super::value::{Value, ValueRef};
use crate::mir::{
    Arg, ArgKind, Branch, CfgI, CfgLabel, Fun, InstrKind, LhsMode, Mode, Place, Pointer, RValue,
    Reference, Struct, Varname,
};
use crate::tast::Stratum;

/// Interpreter for our language.
pub(crate) struct Interpreter<'a, 'mir, 'ctx> {
    /// The execution environment stack.
    pub env: Vec<Env<'ctx>>,
    /// Map between function names and their definition.
    pub fun_env: &'a HashMap<&'ctx str, Fun<'mir, 'ctx>>,
    /// Map between structure names and their definition.
    ///
    /// Used to get the ordering of the fields in the definition, when debugging
    /// a structure.
    pub struct_env: &'a HashMap<&'ctx str, Struct<'ctx>>,
    /// Standard output, as a growable string.
    pub stdout: StringBuilder,
}

impl<'a, 'mir, 'ctx> Interpreter<'a, 'mir, 'ctx> {
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

    /// Shortcut to produce the result of a call to an integer binop operation
    /// `f` that takes two booleans and returns a value.
    fn bool_binop(
        &mut self,
        f: impl Fn(&bool, &bool) -> Value<'ctx>,
        args: &Vec<ValueRef>,
        stratum: Stratum,
    ) -> Option<ValueRef> {
        let lhs = *args.get(0)?;
        let rhs = *args.get(1)?;
        match (self.get_value(lhs), self.get_value(rhs)) {
            (BoolV(lhs), BoolV(rhs)) if args.len() == 2 => {
                Some(self.add_value(f(lhs, rhs), stratum))
            }
            _ => None,
        }
    }

    /// Shortcut to produce the result of a call to an integer unary operation
    /// `f` that takes one boolean and returns a value.
    fn bool_unop(
        &mut self,
        f: impl Fn(&bool) -> Value<'ctx>,
        args: &Vec<ValueRef>,
        stratum: Stratum,
    ) -> Option<ValueRef> {
        let arg = *args.get(0)?;
        match self.get_value(arg) {
            BoolV(arg) if args.len() == 1 => Some(self.add_value(f(arg), stratum)),
            _ => None,
        }
    }

    /// Returns the stringify-cation of a value from the store.
    fn display(&self, value: &ValueRef) -> String {
        let value = self.get_value(*value);
        match value {
            UninitV => unreachable!(),
            UnitV => "()".to_string(),
            IntV(i) => format!("{i}"),
            StrV(s) => format!("{s}"),
            BoolV(b) => format!("{b}"),
            StructV(name, fields) => {
                // Retrieve the order of the fields.
                let fields_order = &self.struct_env[name].fields_order;

                // Display the fields
                let fields = fields_order
                    .iter()
                    .map(|field| {
                        let value = &fields[field];
                        format!(" {}: {}", field, self.display(value))
                    })
                    .join(",");

                // Extra `\n` if there are some fields
                format!("{name} {{{fields} }}")
            }
            RangeV(start, end) => {
                format!("{}..{}", self.display(start), self.display(end))
            }
            ArrayV(items) => {
                let items = items.iter().map(|item| self.display(item)).join(", ");
                format!("[{items}]")
            }
            TupleV(items) => {
                let items = items.iter().map(|item| self.display(item)).join(", ");
                format!("({items})")
            }
            VariantV {
                enun: _,
                variant,
                args,
            } => {
                if !args.is_empty() {
                    // Only display the parenthesis if we are not a unit variant
                    let args = args.iter().map(|item| self.display(item)).join(", ");
                    format!("{variant}({args})")
                } else {
                    format!("{variant}")
                }
            }
        }
    }

    /// Checks if two values pointed by values references are equal.
    ///
    /// Will perform nested/recursive comparison if we encounter value
    /// references. In that regard, it's more lenient (and useful) than
    /// simple equality. But it relies on our ability to
    /// [`Self::get_value`].
    fn eq(&self, x: ValueRef, y: ValueRef) -> bool {
        let (x, y) = (self.get_value(x), self.get_value(y));
        match (x, y) {
            (UninitV, _) | (_, UninitV) => unreachable!(),
            (UnitV, UnitV) => true,
            (UnitV, _) | (_, UnitV) => false,
            (BoolV(x), BoolV(y)) => x == y,
            (BoolV(_), _) | (_, BoolV(_)) => false,
            (StrV(x), StrV(y)) => x == y,
            (StrV(_), _) | (_, StrV(_)) => false,
            (IntV(x), IntV(y)) => x == y,
            (RangeV(e1, e2), RangeV(e3, e4)) => self.eq(*e1, *e2) && self.eq(*e3, *e4),
            (RangeV(_, _), _) | (_, RangeV(_, _)) => false,
            (StructV(name1, fields1), StructV(name2, fields2)) => {
                name1 == name2
                    && fields1.iter().all(|(k, &v1)| {
                        let v2 = fields2[k]; // Same struct name so v2 must exist
                        self.eq(v1, v2)
                    })
            }
            (StructV(..), _) | (_, StructV(..)) => false,
            (
                VariantV {
                    enun: enun1,
                    variant: variant1,
                    args: args1,
                },
                VariantV {
                    enun: enun2,
                    variant: variant2,
                    args: args2,
                },
            ) => {
                enun1 == enun2
                    && variant1 == variant2
                    && args1
                        .iter()
                        .zip(args2.iter()) // NB: same `Vec` length since they come from the same enum
                        .all(|(&v1, &v2)| self.eq(v1, v2))
            }
            (VariantV { .. }, _) | (_, VariantV { .. }) => false,
            (ArrayV(items1), ArrayV(items2)) => {
                items1.len() == items2.len() && // NB: here we NEED to check for equality since they can be of different length
                items1
                    .iter()
                    .zip(items2.iter())
                    .all(|(&v1, &v2)| self.eq(v1, v2))
            }
            (ArrayV(_), _) | (_, ArrayV(_)) => false,
            (TupleV(tuple1), TupleV(tuple2)) => tuple1
                .iter()
                .zip(tuple2.iter()) // NB: same `Vec` length since they come from the same tuple type
                .all(|(&v1, &v2)| self.eq(v1, v2)),
            (TupleV(_), _) | (_, TupleV(_)) => false,
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
            "==" => {
                assert!(args.len() == 2);
                Some(self.add_value(BoolV(self.eq(args[0], args[1])), stratum))
            }
            "!=" => {
                assert!(args.len() == 2);
                Some(self.add_value(BoolV(!self.eq(args[0], args[1])), stratum))
            }
            ">=" => self.int_binop(|x, y| BoolV(x >= y), args, stratum),
            ">" => self.int_binop(|x, y| BoolV(x > y), args, stratum),
            "<=" => self.int_binop(|x, y| BoolV(x <= y), args, stratum),
            "<" => self.int_binop(|x, y| BoolV(x < y), args, stratum),
            "&&" => self.bool_binop(|x, y| BoolV(*x && *y), args, stratum),
            "||" => self.bool_binop(|x, y| BoolV(*x || *y), args, stratum),
            "!" => self.bool_unop(|x| BoolV(!x), args, stratum),
            "len" => {
                assert!(args.len() == 1);
                match self.get_value(args[0]) {
                    ArrayV(array) => Some(self.add_value(IntV(array.len().into()), stratum)),
                    _ => None,
                }
            }
            "assert" => self.bool_unop(
                |x| {
                    assert!(x, "Assertion failed");
                    UnitV
                },
                args,
                stratum,
            ),
            "debug" => {
                let args = args.iter().map(|arg| self.display(arg)).join(" ");
                self.stdout.append(args);
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
        f_name: impl Borrow<str>,
        args: Vec<ValueRef>,
        stratum: Stratum,
    ) -> Option<ValueRef> {
        let f_name = f_name.borrow();

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
            for (arg, &value) in f.params.iter().zip(args.iter()) {
                self.add_var(arg.name(), value);
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
    fn get_var(&self, v: impl Borrow<Varname<'ctx>>) -> Option<ValueRef> {
        let v = v.borrow();
        // Iterate over environments in reverse (last declared first processed)
        // order
        // Returns the first environment that has that variable declared
        self.env.iter().rev().find_map(|e| e.get_var(v)).copied()
    }

    /// Declares a new variable in the context.
    fn add_var(&mut self, name: impl Into<Varname<'ctx>>, r: impl Into<ValueRef>) {
        let name = name.into();
        let r = r.into();
        self.env.last_mut().unwrap().add_var(name, r);

        // Keep it, useful for debugging
        /* eprintln!(
            "[{:?} : {:?} = {:?} i.e. {}]",
            name,
            r,
            self.get_value(r),
            self.display(&r)
        );*/
    }

    /// Assigns `*ptr`.
    fn set_at_ptr(&mut self, ptr: impl Into<Pointer<'ctx>>, value: impl Into<Value<'ctx>>) {
        let ptr = ptr.into();
        let value = value.into();
        let r = self.get_ptr(ptr);
        self.set_value(r, value);
        // Keep it, useful for debugging
        /*eprintln!(
            "[{:?} : {:?} = {:?} i.e. {}]",
            ptr.place(),
            r,
            self.get_value(r),
            self.display(&r)
        );*/
    }

    /// Adds a value to the dynamic store/slab.
    fn add_value(&mut self, value: Value<'ctx>, stratum: Stratum) -> ValueRef {
        let stratum: usize = stratum.into();
        self.env[stratum].add_value(value)
    }

    /// Sets a value in the dynamic store/slab.
    pub fn set_value(&mut self, val_ref: ValueRef, rvalue: Value<'ctx>) {
        let stratum = usize::from(val_ref.stratum);
        self.env[stratum].set_value(val_ref, rvalue);
    }

    /// Gets a value from the dynamic store/slab.
    fn get_value(&self, value: ValueRef) -> &Value<'ctx> {
        self.env[usize::from(value.stratum)]
            .get_value(value.key)
            .unwrap()
    }

    /// Gets a value from the dynamic store/slab.
    fn retrieve_value(&mut self, value: ValueRef) -> Value<'ctx> {
        self.env[usize::from(value.stratum)].remove_value(value.key)
    }

    /// Gets the value reference at a given pointer.
    fn get_ptr_opt(&self, ptr: impl Into<Pointer<'ctx>>) -> Option<ValueRef> {
        let ptr = ptr.into();
        match ptr.place() {
            VarP(var) => self.get_var(var),
            FieldP(compound, field) => match self.get_ptr_value(compound) {
                StructV(_, fields) => Some(fields[field]),
                TupleV(elems) => {
                    let elem = field
                        .parse::<usize>()
                        .expect("Runtime error: array index is too big");
                    Some(elems[elem])
                }
                _ => panic!(),
            },
            IndexP(array, index) => match (self.get_ptr_value(array), self.get_ptr_value(index)) {
                (ArrayV(array), IntV(index)) => {
                    let index = index
                        .to_usize()
                        .expect("Runtime error: array index is too big");
                    Some(array[index])
                }
                (array, index) => panic!("Runtime error: incorrect indexing {array:?}[{index:?}]"),
            },
        }
    }

    /// Gets the value reference at a given pointer.
    fn get_ptr(&self, ptr: impl Into<Pointer<'ctx>>) -> ValueRef {
        let ptr = ptr.into();
        self.get_ptr_opt(ptr)
            .unwrap_or_else(|| panic!("Runtime error: variable {:?} should exist", ptr.place()))
    }

    /// Gets the value at a given pointer location.
    fn get_ptr_value(&self, ptr: impl Into<Pointer<'ctx>>) -> &Value<'ctx> {
        self.get_value(self.get_ptr(ptr))
    }

    /// Returns the final standard output of the execution of the program.
    pub fn stdout(self) -> String {
        self.stdout.string().unwrap()
    }

    /// Clones the value behind a value reference, retuning the value reference
    /// of the cloned value.
    fn clone(&mut self, val_ref: ValueRef, stm: Stratum) -> ValueRef {
        let value = self.get_value(val_ref);
        let new_value = match value {
            UninitV => unreachable!(),
            UnitV => UnitV,
            IntV(i) => IntV(i.clone()),
            StrV(s) => StrV(s.clone()),
            BoolV(b) => BoolV(*b),
            StructV(name, fields) => {
                let fields: HashMap<_, _> =
                    fields.iter().map(|(name, field)| (*name, *field)).collect();
                StructV(
                    name,
                    fields
                        .into_iter()
                        .map(|(name, field)| (name, self.clone(field, stm)))
                        .collect(),
                )
            }
            ArrayV(items) => {
                let items: Vec<_> = items.to_vec();
                ArrayV(
                    items
                        .into_iter()
                        .map(|item| self.clone(item, stm))
                        .collect(),
                )
            }
            TupleV(elems) => {
                let elems: Vec<_> = elems.to_vec();
                TupleV(
                    elems
                        .into_iter()
                        .map(|elem| self.clone(elem, stm))
                        .collect(),
                )
            }
            VariantV {
                enun,
                variant,
                args,
            } => {
                let args: Vec<_> = args.to_vec();
                VariantV {
                    enun,
                    variant,
                    args: args.into_iter().map(|arg| self.clone(arg, stm)).collect(),
                }
            }
            RangeV(start, end) => {
                let (start, end) = (*start, *end);
                RangeV(self.clone(start, stm), self.clone(end, stm))
            }
        };
        self.add_value(new_value, stm)
    }

    /// Moves the value out of its original location, leaving an uninitialized
    /// ([`Value::UninitV`]) value at the value reference, and move the
    /// value to a new [`ValueRef`]. It will return the new [`ValueRef`] that
    /// stores that [`Value`].
    fn move_out(&mut self, val_ref: ValueRef, stm: Stratum) -> ValueRef {
        let value = self.env[usize::from(val_ref.stratum)].move_out(val_ref.key);
        self.add_value(value, stm)
    }

    /// Swaps two value locations.
    fn swap_values(&mut self, r1: ValueRef, r2: ValueRef) {
        debug_assert!(
            r1.stratum == r2.stratum,
            "Swapped values should be of the same stratum"
        );
        self.env[usize::from(r1.stratum)].swap_values(r1, r2);
    }

    /// Visits a function argument, optionally choosing to clone the value.
    pub fn visit_arg(&mut self, arg: &Arg<'mir, 'ctx>, stratum: Stratum) -> ValueRef {
        match &arg.kind {
            ArgKind::Standard(r) => self.visit_reference(r, stratum),
            ArgKind::InPlace(r) => {
                debug_assert_eq!(r.mode(), Mode::MutBorrowed);
                self.visit_reference(r, stratum)
            }
            ArgKind::Binding(_, _) => todo!(),
        }
    }

    /// Visits a reference, optionally choosing to clone the value.
    pub fn visit_reference(&mut self, r: &Reference<'mir, 'ctx>, stratum: Stratum) -> ValueRef {
        let v_ref = self.get_ptr(r.as_ptr());
        match r.mode() {
            Mode::Cloned | Mode::Borrowed | Mode::SBorrowed => self.clone(v_ref, stratum),
            Mode::Moved => self.move_out(v_ref, stratum),
            Mode::SMutBorrowed | Mode::MutBorrowed => {
                debug_assert!(
                    stratum >= v_ref.stratum,
                    "Runtime error: borrowing value {:?} out of scope",
                    self.get_value(v_ref)
                );
                v_ref
            }
        }
    }

    /// Visit a right-value.
    fn visit_rvalue(&mut self, rvalue: &RValue<'mir, 'ctx>, stratum: Stratum) -> ValueRef {
        match rvalue {
            RValue::Unit => self.add_value(UnitV, stratum),
            RValue::Bool(b) => self.add_value(BoolV(*b), stratum),
            RValue::Integer(i) => self.add_value(IntV((*i).clone()), stratum),
            RValue::String(s) => self.add_value(StrV(s.to_string()), stratum),
            RValue::Place(place) => self.visit_reference(place, stratum),
            RValue::Struct { name, fields } => {
                let fields = fields
                    .iter()
                    .map(|(&k, r)| (k, self.visit_reference(r, stratum)))
                    .collect();
                self.add_value(StructV(name, fields), stratum)
            }
            RValue::Array(array) => {
                let array = array
                    .iter()
                    .map(|r| self.visit_reference(r, stratum))
                    .collect();
                self.add_value(ArrayV(array), stratum)
            }
            RValue::Tuple(items) => {
                let items = items
                    .iter()
                    .map(|r| self.visit_reference(r, stratum))
                    .collect();
                self.add_value(TupleV(items), stratum)
            }
            RValue::Range(start, end) => {
                let start = self.visit_reference(start, stratum);
                let end = self.visit_reference(end, stratum);
                self.add_value(RangeV(start, end), stratum)
            }
            RValue::Variant {
                enun,
                variant,
                args,
            } => {
                let args = args
                    .iter()
                    .map(|r| self.visit_reference(r, stratum))
                    .collect();
                self.add_value(
                    VariantV {
                        enun,
                        variant,
                        args,
                    },
                    stratum,
                )
            }
        }
    }

    /// Executes an expression, returning the first label that do not exist in
    /// the CFG. Often, this is the return/exit label.
    fn visit_cfg(&mut self, cfg: &CfgI<'mir, 'ctx>, label: CfgLabel) {
        //eprintln!("{:?}", cfg[&label].kind);
        let branch = match &cfg[&label].kind {
            InstrKind::Noop => DefaultB,
            InstrKind::Assign(ptr, rvalue) => {
                match ptr.mode() {
                    LhsMode::Assigning => {
                        let val_ref = self.visit_rvalue(rvalue, self.get_ptr(ptr.as_ptr()).stratum);
                        let value = self.retrieve_value(val_ref);
                        self.set_at_ptr(ptr.as_ptr(), value);
                    }
                    LhsMode::Declaring => {
                        let val_ref = self.visit_rvalue(rvalue, self.current_stratum());
                        if let VarP(var) = ptr.place() {
                            self.add_var(var, val_ref);
                        } else {
                            panic!(
                                "interpreter error: can only declare variables. Consider removing the declare mode"
                            );
                        }
                    }
                }
                DefaultB
            }
            InstrKind::SwapS(place1, place2) => {
                self.swap_values(self.get_ptr(place1), self.get_ptr(place2));
                DefaultB
            }
            InstrKind::Call {
                name,
                args,
                destination: Some(destination),
            } => {
                match destination.mode() {
                    LhsMode::Assigning => {
                        let stratum = self.get_ptr(destination.as_ptr()).stratum;
                        let args = args
                            .iter()
                            .map(|arg| self.visit_arg(arg, stratum))
                            .collect();
                        let call_result = self.call(name.name, args, stratum).expect(
                            "if the destination is set, then the function should return a value",
                        );
                        let value = self.retrieve_value(call_result);
                        self.set_at_ptr(destination.as_ptr(), value);
                    }
                    LhsMode::Declaring => {
                        let stratum = self.current_stratum();
                        let args = args
                            .iter()
                            .map(|arg| self.visit_arg(arg, stratum))
                            .collect();
                        let call_result = self.call(name.name, args, stratum).expect(
                            "if the destination is set, then the function should return a value",
                        );
                        if let VarP(var) = destination.place() {
                            self.add_var(var, call_result);
                        } else {
                            panic!(
                                "Can only declare variables. Consider removing the declare mode"
                            );
                        }
                    }
                }
                DefaultB
            }
            InstrKind::Call {
                name: _,
                args: _,
                destination: None,
            } => todo!(),
            InstrKind::Branch(cond) => {
                if self.get_ptr_value(cond).truth() {
                    BoolB(true)
                } else {
                    BoolB(false)
                }
            }
            // Do nothing: it was a marker instruction for the liveness analysis.
            InstrKind::PhantomUse(_) => DefaultB,
        };
        if let Some(next) = cfg.take_branch(label, &branch) {
            self.visit_cfg(cfg, next)
        }
    }
}
