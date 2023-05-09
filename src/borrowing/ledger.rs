//! Declaring here the annotations to the CFG we compute during the analysis.

use std::collections::HashMap;
use std::fmt;
use std::iter::Sum;
use std::ops::{Add, BitOr, Deref, DerefMut, Sub};

use super::borrow::Borrows;
use crate::mir::Var;
use crate::utils::set::Set;

/// A loan ledger.
#[derive(Clone, PartialEq, Eq)]
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
                deep_set += borrow;
            }
        }
        self.borrows.insert(var.into(), deep_set);
    }

    pub fn is_borrowed(&self, var: impl AsRef<Var>) -> bool {
        let var = var.as_ref();
        self.borrows
            .values()
            .any(|s| s.iter().any(|borrow| &borrow.var == var))
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

impl fmt::Debug for Ledger {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(&self.borrows).finish()
    }
}

impl<B: Into<Borrows>> Add<(Var, B)> for Ledger {
    type Output = Ledger;

    fn add(mut self, (lhs, borrows): (Var, B)) -> Self {
        self.insert(lhs, borrows.into());
        self
    }
}

impl Default for Ledger {
    fn default() -> Self {
        Self::new()
    }
}

impl Add for Ledger {
    type Output = Ledger;

    fn add(self, rhs: Self) -> Self {
        self.bitor(rhs)
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

impl<'a> Sub<&'a Var> for Ledger {
    type Output = Ledger;

    fn sub(mut self, var: &Var) -> Self {
        self.borrows.remove(var);
        self
    }
}

impl Sum for Ledger {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|acc, el| acc | el).unwrap_or_default()
    }
}

impl Deref for Ledger {
    type Target = HashMap<Var, Borrows>;

    fn deref(&self) -> &Self::Target {
        &self.borrows
    }
}

impl DerefMut for Ledger {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.borrows
    }
}
