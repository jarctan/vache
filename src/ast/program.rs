//! Parsing programs, and defining their representation in the AST.

use std::collections::HashMap;

use super::{Fun, Struct};

/// A program: a collection of:
/// * structures
/// * functions
pub struct Program {
    /// Collection of functions defined in the program, indexed by their names.
    pub funs: HashMap<String, Fun>,
    /// Collection of structures defined in the program, indexed by their names.
    pub structs: HashMap<String, Struct>,
}

impl Program {
    /// Builds a program listing out of a list of structures and a list of
    /// functions.
    pub fn new(structs: Vec<Struct>, funs: Vec<Fun>) -> Self {
        Self {
            funs: funs.into_iter().map(|f| (f.name.clone(), f)).collect(),
            structs: structs.into_iter().map(|f| (f.name.clone(), f)).collect(),
        }
    }
}

impl From<Vec<Fun>> for Program {
    fn from(list: Vec<Fun>) -> Self {
        Program {
            funs: list.into_iter().map(|f| (f.name.clone(), f)).collect(),
            structs: HashMap::new(),
        }
    }
}

impl From<Fun> for Program {
    fn from(f: Fun) -> Self {
        Program::from(vec![f])
    }
}
