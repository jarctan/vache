//! Defining the environment in which the program executes.

use super::value::{Value, ValueRef};
use crate::tast::Var;
use slab::Slab;
use std::collections::HashMap;

/// Execution environment.
pub struct Env {
    /// Slab of actual values.
    slab: Slab<Value>,
    /// Map between vars and their keys in the slab.
    var_env: HashMap<Var, ValueRef>,
}
impl Env {
    /// Creates a new, empty environment.
    pub fn new() -> Self {
        Self {
            slab: Slab::new(),
            var_env: HashMap::new(),
        }
    }

    /// Gets a value from the environment, based on the key in the slab.
    pub fn get_value(&self, key: usize) -> Option<&Value> {
        self.slab.get(key)
    }

    /// Adds a value to that environment, returning the key to it in the slab.
    pub fn add_value(&mut self, value: Value) -> usize {
        self.slab.insert(value)
    }

    /// Gets the definition of a variable.
    pub fn get_var(&self, v: impl AsRef<Var>) -> Option<&ValueRef> {
        self.var_env.get(v.as_ref())
    }

    /// Declares a new variable in the context.
    ///
    /// # Panics
    /// Panics if the var is not stated as declared in that stratum/environment.
    /// You should only add a var definition in the stratum in which it is
    /// tied to.
    pub fn add_var(&mut self, name: impl Into<Var>, value: impl Into<ValueRef>) {
        self.var_env.insert(name.into(), value.into());
    }

    /// Closes the environment, returning one final value from the slab.
    ///
    /// Give the reference of the value you want to keep, and it will return it for you,
    /// before closing the environment once and for all (and thus freeing all values in that
    /// environment).
    pub fn close(mut self, value: ValueRef) -> Value {
        self.slab.remove(value.key)
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}
