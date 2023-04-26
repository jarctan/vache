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

impl From<Vec<Fun>> for Program {
    fn from(list: Vec<Fun>) -> Self {
        Program {
            funs: list.into_iter().map(|f| (f.name.clone(), f)).collect(),
            structs: HashMap::new(),
        }
    }
}
