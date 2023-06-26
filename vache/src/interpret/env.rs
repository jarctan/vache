//! Defining the environment in which the program executes.

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
        self.slab.get(key)
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
        self.slab.remove(key)
    }

    /// Gets the definition of a variable.
    pub fn get_var(&self, v: impl AsRef<Varname<'ctx>>) -> Option<&ValueRef> {
        self.var_env.get(v.as_ref())
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
            Some(self.slab.remove(value.key))
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
