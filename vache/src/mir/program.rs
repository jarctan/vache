//! Defining programs, the biggest units in the MIR.

use std::collections::HashMap;
use std::fmt;

use super::{Fun, Struct};
use crate::Arena;

/// A program: a collection of:
/// * structures
/// * functions
pub struct Program<'ctx> {
    /// AST arena.
    pub arena: &'ctx Arena,
    /// Collection of functions defined in the program, indexed by their names.
    pub funs: HashMap<&'ctx str, Fun<'ctx>>,
    /// Collection of structures defined in the program, indexed by their names.
    pub structs: HashMap<&'ctx str, Struct<'ctx>>,
}

impl<'ctx> fmt::Debug for Program<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            funs,
            structs,
            arena: _,
        } = self; // So that if we add a new field, we don;'t forget it here

        f.debug_struct("Program")
            .field("Functions", &funs.values())
            .field("Structures", &structs.values())
            .finish()
    }
}

impl<'ctx> AsRef<Program<'ctx>> for Program<'ctx> {
    fn as_ref(&self) -> &Program<'ctx> {
        self
    }
}
