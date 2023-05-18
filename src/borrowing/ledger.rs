//! Declaring here the annotations to the CFG we compute during the analysis.

use std::collections::HashMap;
use std::fmt;
use std::iter::Sum;
use std::ops::{Add, BitOr, Deref, DerefMut, Sub};

use Place::*;

use super::borrow::{Borrow, Borrows};
use crate::mir::{CfgLabel, Place, Var, VarMode};

/// A loan ledger.
#[derive(Clone, PartialEq, Eq)]
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

    /// Defines a new variable in the context, stating its borrows (variables it
    /// depends on).
    pub fn add_var(&mut self, var: impl Into<Var>, borrows: impl Into<Borrows>) {
        self.borrows.insert(var.into(), borrows.into());
    }

    /// Returns the complete (deep, nested) list of all borrows resulting from
    /// the borrow of `var` at CFG label `label`.
    pub fn borrow(&self, var: &VarMode, label: CfgLabel) -> Borrows {
        if var.mode.is_borrowing() {
            let var = var.var.clone();
            self.get(&var).cloned().unwrap_or_default() + Borrow { var, label }
        } else {
            Borrows::new()
        }
    }

    /// Records new `borrows` related to the borrower that living at `place`.
    pub fn add_borrows(&mut self, place: &Place, borrows: Borrows) {
        match place {
            VarP(v) => {
                self.borrows.insert(v.clone(), borrows);
            }
            IndexP(array, _) => {
                // If you assign something to an element of an array, we need to ADD the borrows
                // to the old ones, since we have no way to know which (if any) borrows are
                // invalidated by that assignment.
                let entry = self.borrows.entry(array.clone());
                entry.or_default().extend(borrows);
            }
            FieldP(strukt, _) => {
                let entry = self.borrows.entry(strukt.clone());
                entry.or_default().extend(borrows); // TODO: change, too coarse
                                                    // grained. Separate borrows
                                                    // between fields.
            }
        }
    }

    /// Returns all borrows of a variable.
    pub fn borrows<'a>(&'a self, var: &'a Var) -> impl Iterator<Item = &'a Borrow> + 'a {
        self.borrows
            .values()
            .flat_map(move |s| s.iter().filter(move |borrow| &borrow.var == var))
    }
}

impl fmt::Debug for Ledger {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.borrows.fmt(f)
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
