//! Defining programs, the biggest unit in the Typed AST.

use std::collections::HashMap;

use super::{Fun, Struct};
use crate::Arena;

/// A program: a collection of:
/// * structures
/// * functions
#[derive(Debug, Clone)]
pub struct Program<'ctx> {
    pub arena: &'ctx Arena,
    /// Collection of functions defined in the program, indexed by their names.
    pub funs: HashMap<&'ctx str, Fun<'ctx>>,
    /// Collection of structures defined in the program, indexed by their names.
    pub structs: HashMap<&'ctx str, Struct<'ctx>>,
}
