//! Declaring here the annotations to the CFG we compute during the analysis.

use std::collections::HashMap;
use std::iter::Sum;
use std::ops::{Add, BitOr, Deref, DerefMut, Sub};

use super::borrow::{Borrow, Borrows};
use crate::mir::{CfgLabel, Var};

/// A loan ledger.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ledger {
    /// Map between variables defined in this environment and their borrows.
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
        self.borrows.insert(var.into(), borrows.into());
    }

    /// Returns the complete (deep, nested) list of all borrows resulting from
    /// the borrow of `var` at CFG label `label`.
    pub fn borrow(&self, var: impl Into<Var>, label: CfgLabel) -> Borrows {
        let var = var.into();
        self.get(&var).cloned().unwrap_or_default() + Borrow { var, label }
    }
}

impl<B: Into<Borrows>> Add<(Var, B)> for Ledger {
    type Output = Ledger;

    fn add(mut self, (lhs, borrows): (Var, B)) -> Self {
        self.add_var(lhs, borrows);
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

impl<'a, I: IntoIterator<Item = &'a Var>> Sub<I> for Ledger {
    type Output = Ledger;

    fn sub(mut self, iter: I) -> Self {
        for var in iter {
            self.borrows.remove(var);
        }
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
