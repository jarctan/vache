//! Defining programs, the biggest units in the MIR.

use std::collections::HashMap;

use super::{Fun, Struct};

/// A program: a collection of:
/// * structures
/// * functions
#[derive(Debug)]
pub struct Program<'a> {
    /// Collection of functions defined in the program, indexed by their names.
    pub funs: HashMap<String, Fun<'a>>,
    /// Collection of structures defined in the program, indexed by their names.
    pub structs: HashMap<String, Struct>,
}

impl<'a> AsRef<Program<'a>> for Program<'a> {
    fn as_ref(&self) -> &Program<'a> {
        self
    }
}
