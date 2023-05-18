//! Defining programs, the biggest units in the MIR.

use std::collections::HashMap;

use super::{Fun, Struct};

/// A program: a collection of:
/// * structures
/// * functions
#[derive(Debug)]
pub struct Program {
    /// Collection of functions defined in the program, indexed by their names.
    pub funs: HashMap<String, Fun>,
    /// Collection of structures defined in the program, indexed by their names.
    pub structs: HashMap<String, Struct>,
}
