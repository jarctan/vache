//! Defining the environment in which the program executes.

use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt;

use slab::Slab;

use super::value::{Value, ValueRef};
use crate::{mir::Varname, tast::Stratum};

/// Execution environment.
///
/// You can only add new values, not mutate previous ones.
///
/// You can however shadow variables definitions, effectively changing the value
/// they point to.
pub struct Env<'ctx> {
    /// Slab of actual values.
    slab: Slab<Value<'ctx>>,
    /// Map between vars and their keys in the slab.
    var_env: HashMap<Varname<'ctx>, ValueRef>,
    /// Stratum id for this environment.
    stratum: Stratum,
}
impl<'ctx> Env<'ctx> {
    /// Creates a new, empty environment.
    pub fn new(stratum: Stratum) -> Self {
        Self {
            slab: Slab::new(),
            var_env: HashMap::new(),
            stratum,
        }
    }

    /// Gets a value from the environment, based on the key in the slab.
    pub fn get_value(&self, key: usize) -> Option<&Value<'ctx>> {
        let value = self.slab.get(key)?;
        debug_assert!(
            !matches!(value, Value::UninitV),
            "Runtime error: getting an uninitialized value at key {key}"
        );
        Some(value)
    }

    /// Adds a value to that environment, returning the key to it in the slab.
    pub fn add_value(&mut self, value: Value<'ctx>) -> ValueRef {
        ValueRef {
            key: self.slab.insert(value),
            stratum: self.stratum,
        }
    }

    /// Sets a value in the environment.
    ///
    /// # Panics
    /// The value must already exist.
    pub fn set_value(&mut self, val_ref: ValueRef, rvalue: Value<'ctx>) {
        assert_eq!(
            val_ref.stratum, self.stratum,
            "Runtime error: not in the right stratum"
        );
        self.slab[val_ref.key] = rvalue;
    }

    /// Removes a value from the environment.
    ///
    /// # Panics
    /// Panics if the key corresponds to no value.
    pub fn remove_value(&mut self, key: usize) -> Value<'ctx> {
        let value = self.slab.remove(key);
        debug_assert!(
            !matches!(value, Value::UninitV),
            "Runtime error: getting an uninitialized value at key {key}"
        );
        value
    }

    /// Moves a [`Value`] out of the store indexed by `key`, leaving a
    /// [`Value::UninitV`] at this place. Returns the removed [`Value`].
    pub fn move_out(&mut self, key: usize) -> Value<'ctx> {
        std::mem::take(&mut self.slab[key])
    }

    /// Swaps the values at two memory locations.
    ///
    /// # Panics
    /// Panics if the two locations are not in the same stratum.
    pub fn swap_values(&mut self, r1: ValueRef, r2: ValueRef) {
        debug_assert!(
            r1.stratum == r2.stratum,
            "Swapped values should be of the same stratum"
        );
        // https://stackoverflow.com/questions/25531963/how-can-i-swap-items-in-a-vector-slice-or-array-in-rust
        // Can't take two mutable loans from one slab, so instead just cast
        // them to their raw pointers to do the swap
        let pa: *mut Value = &mut self.slab[r1.key];
        let pb: *mut Value = &mut self.slab[r2.key];
        unsafe {
            std::ptr::swap(pa, pb);
        }
    }

    /// Gets the definition of a variable.
    pub fn get_var(&self, v: impl Borrow<Varname<'ctx>>) -> Option<&ValueRef> {
        self.var_env.get(v.borrow())
    }

    /// Declares a new variable in the context.
    ///
    /// # Panics
    /// Panics if the var is not stated as declared in that stratum/environment.
    /// You should only add a var definition in the stratum in which it is
    /// tied to.
    pub fn add_var(&mut self, name: impl Into<Varname<'ctx>>, val_ref: ValueRef) {
        self.var_env.insert(name.into(), val_ref);
    }

    /// Closes the environment, returning one final value from the slab.
    ///
    /// Provide the reference of the value you want to keep, and it will return
    /// it for you, before closing the environment once and for all (and
    /// thus freeing all values in that environment). If the value was not
    /// defined in that environment, it won't return the value.
    ///
    /// # Panics
    /// Panics if the value was defined in a more _internal_ environment.
    pub fn close(mut self, value: ValueRef) -> Option<Value<'ctx>> {
        assert!(value.stratum <= self.stratum);
        if value.stratum == self.stratum {
            Some(self.remove_value(value.key))
        } else {
            None
        }
    }
}

impl fmt::Debug for Env<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Env")
            .field("stratum", &self.stratum)
            .field("values", &self.slab)
            .field("vars", &self.var_env)
            .finish()
    }
}
