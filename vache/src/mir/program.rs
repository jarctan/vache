//! Defining programs, the biggest units in the MIR.

use std::collections::HashMap;

use super::{Fun, Struct};
use crate::Arena;

/// A program: a collection of:
/// * structures
/// * functions
#[derive(Debug)]
pub struct Program<'ctx> {
    /// AST arena.
    pub arena: &'ctx Arena,
    /// Collection of functions defined in the program, indexed by their names.
    pub funs: HashMap<&'ctx str, Fun<'ctx>>,
    /// Collection of structures defined in the program, indexed by their names.
    pub structs: HashMap<&'ctx str, Struct<'ctx>>,
}

impl<'ctx> AsRef<Program<'ctx>> for Program<'ctx> {
    fn as_ref(&self) -> &Program<'ctx> {
        self
    }
}
