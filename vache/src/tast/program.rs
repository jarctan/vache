//! Defining programs, the biggest unit in the Typed AST.

use std::collections::HashMap;
use std::fmt;

use super::{Enum, Fun, Struct};
use crate::Arena;

/// A program: a collection of:
/// * structures
/// * functions
#[derive(Clone)]
pub struct Program<'ctx> {
    /// AST arena.
    pub arena: &'ctx Arena,
    /// Collection of functions defined in the program, indexed by their names.
    pub funs: HashMap<&'ctx str, Fun<'ctx>>,
    /// Collection of structures defined in the program, indexed by their names.
    pub structs: &'ctx HashMap<&'ctx str, Struct<'ctx>>,
    /// Collection of enumerations defined in the program, indexed by their
    /// names.
    pub enums: &'ctx HashMap<&'ctx str, Enum<'ctx>>,
}

impl<'ctx> fmt::Debug for Program<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            funs,
            structs,
            enums,
            arena: _,
        } = self; // So that if we add a new field, we don;'t forget it here

        f.debug_struct("Program")
            .field("Functions", &funs.values())
            .field("Structures", &structs.values())
            .field("Enumerations", &enums.values())
            .finish()
    }
}
