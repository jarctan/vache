//! Defining the environment in which the program executes.

use std::collections::HashMap;
use std::fmt;

use slab::Slab;

use super::value::{Value, ValueRef};
use crate::{mir::Var, tast::Stratum};

/// Execution environment.
///
/// You can only add new values, not mutate previous ones.
///
/// You can however shadow variables definitions, effectively changing the value
/// they point to.
pub struct Env {
    /// Slab of actual values.
    slab: Slab<Value>,
    /// Map between vars and their keys in the slab.
    var_env: HashMap<Var, ValueRef>,
    /// Stratum id for this environment.
    stratum: Stratum,
    /// Reference to the dummy value for uninitialized variables.
    uninit: ValueRef,
}
impl Env {
    /// Creates a new, empty environment.
    pub fn new(stratum: Stratum) -> Self {
        let mut slab = Slab::new();

        // Add the uninitialized value.
        let uninit = slab.insert(Value::default());

        Self {
            slab,
            var_env: HashMap::new(),
            stratum,
            uninit: ValueRef {
                stratum,
                key: uninit,
            },
        }
    }

    /// Gets a value from the environment, based on the key in the slab.
    pub fn get_value(&self, key: usize) -> Option<&Value> {
        let value = self.slab.get(key)?;
        assert!(
            !matches!(value, Value::UninitV),
            "Runtime error: getting an uninitialized value"
        );
        Some(value)
    }

    /// Adds a value to that environment, returning the key to it in the slab.
    pub fn add_value(&mut self, value: Value) -> ValueRef {
        ValueRef {
            key: self.slab.insert(value),
            stratum: self.stratum,
        }
    }

    /// Gets the definition of a variable.
    pub fn get_var(&self, v: impl AsRef<Var>) -> Option<&ValueRef> {
        self.var_env.get(v.as_ref())
    }

    /// Gets a mutable reference into the definition of a variable.
    pub fn get_var_mut(&mut self, v: impl AsRef<Var>) -> Option<&mut ValueRef> {
        self.var_env.get_mut(v.as_ref())
    }

    /// Declares a new variable in the context.
    ///
    /// # Panics
    /// Panics if the var is not stated as declared in that stratum/environment.
    /// You should only add a var definition in the stratum in which it is
    /// tied to.
    pub fn add_var(&mut self, name: impl Into<Var>) {
        self.var_env.insert(name.into(), self.uninit);
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
    pub fn close(mut self, value: ValueRef) -> Option<Value> {
        assert!(value.stratum <= self.stratum);
        if value.stratum == self.stratum {
            Some(self.slab.remove(value.key))
        } else {
            None
        }
    }

    /// Returns the reference to the uninitialized value.
    pub const fn uninit_value(&self) -> ValueRef {
        self.uninit
    }
}

impl fmt::Debug for Env {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Env")
            .field("stratum", &self.stratum)
            .field("values", &self.slab)
            .field("vars", &self.var_env)
            .finish()
    }
}
