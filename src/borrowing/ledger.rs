//! Declaring here the annotations to the CFG we compute during the analysis.

use std::collections::HashMap;
use std::iter::Sum;
use std::ops::{BitOr, Sub};

use super::borrow::{Borrow, Borrows};
use super::flow::Flow;
use super::liveness::liveness;
use crate::mir::{Cfg, CfgLabel, Instr, Var};
use crate::utils::set::Set;

/// A loan ledger.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ledger {
    /// Map between variables defined in this environment and their borrows.
    ///
    /// Invariant: these are _deep_ borrows, i.e. all borrow variables are
    /// variables defined _outside_ the scope.
    borrows: HashMap<Var, Borrows>,
}

impl Ledger {
    /// Creates a new, empty ledger.
    pub fn new() -> Self {
        Self {
            borrows: HashMap::new(),
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

impl Default for Ledger {
    fn default() -> Self {
        Self::new()
    }
}

impl BitOr for Ledger {
    type Output = Ledger;

    fn bitor(mut self, rhs: Self) -> Self {
        self.borrows.extend(rhs.borrows);
        self
    }
}

impl<'a> Sub<&'a Ledger> for Ledger {
    type Output = Ledger;

    fn sub(mut self, rhs: &Self) -> Self {
        for key in rhs.borrows.keys() {
            self.borrows.remove(key);
        }
        self
    }
}

impl Sum for Ledger {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|acc, el| acc | el).unwrap_or_default()
    }
}

/// Loan liveness analysis.
///
/// Takes as arguments:
/// * The CFG.
/// * The exit label in that CFG.
///
/// Returns live loans for each label in the graph.
pub fn loan_liveness(cfg: &Cfg, exit_l: &CfgLabel) -> HashMap<CfgLabel, Flow<Ledger>> {
    let defs = |i: &Instr| Ledger::default();
    let uses = |i: &Instr| Ledger::default();
    liveness(cfg, exit_l, defs, uses)
}
