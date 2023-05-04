//! Declaring here the annotations to the CFG we compute during the analysis.

use std::collections::HashMap;

use super::borrow::{Borrow, Borrows};
use crate::mir::Var;
use crate::utils::set::Set;

/// A borrow analysis.
#[derive(Debug, PartialEq, Eq)]
pub struct Analysis {
    /// Map between variables defined in this environment and their borrows.
    ///
    /// Invariant: these are _deep_ borrows, i.e. all borrow variables are
    /// variables defined _outside_ the scope.
    pub borrows: HashMap<Var, Borrows>,
    /// Set of variables.
    pub ins: Set<Var>,
    /// Outs.
    pub outs: Set<Var>,
}

impl Analysis {
    /// Creates a new, empty environment.
    pub fn new() -> Self {
        Self {
            borrows: HashMap::new(),
            ins: Set::new(),
            outs: Set::new(),
        }
    }

    /// Gets the borrows of a variable.
    pub fn get_var(&self, v: impl AsRef<Var>) -> Option<&Borrows> {
        self.borrows.get(v.as_ref())
    }

    /// Defines a new variable in the context, stating its borrows.
    pub fn add_var(&mut self, var: impl Into<Var>, borrows: impl Into<Borrows>) {
        // Compute the set of deep borrows: borrows whose variable were defined OUTSIDE
        // this environment
        let mut deep_set = Set::new();
        for borrow in borrows.into().into_iter() {
            if let Some(borrows_lvl2) = self.borrows.get(&borrow.var) {
                for borrow_lvl2 in borrows_lvl2 {
                    deep_set += borrow_lvl2.clone();
                }
            } else {
                deep_set += Borrow::from(borrow.var.clone());
            }
        }
        self.borrows.insert(var.into(), deep_set);
    }

    pub fn close(self, vars: impl Iterator<Item = Var>) -> Set<Var> {
        // Compute the union of the deep borrows of all the variables.
        let mut res: Set<Var> = Set::new();
        for var in vars {
            if let Some(borrows) = self.borrows.get(&var) {
                for borrow in borrows {
                    res += borrow.var.clone();
                }
            } else {
                res += var;
            }
        }
        res
    }
}

impl Default for Analysis {
    fn default() -> Self {
        Self::new()
    }
}
