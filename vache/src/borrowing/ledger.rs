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
pub struct Ledger<'ctx> {
    /// Map between variables defined in this environment and their borrows.
    borrows: HashMap<Var<'ctx>, Borrows<'ctx>>,
}

impl<'ctx> Ledger<'ctx> {
    /// Creates a new, empty ledger.
    pub fn new() -> Self {
        Self {
            borrows: HashMap::new(),
        }
    }

    /// Defines a new variable in the context, stating its borrows (variables it
    /// depends on).
    pub fn add_var(&mut self, var: impl Into<Var<'ctx>>, borrows: impl Into<Borrows<'ctx>>) {
        self.borrows.insert(var.into(), borrows.into());
    }

    /// Returns the complete (deep, nested) list of all borrows resulting from
    /// the borrow of `var` at CFG label `label`.
    pub fn borrow(&self, var: &VarMode<'ctx>, label: CfgLabel) -> Borrows<'ctx> {
        if var.mode.is_borrowing() {
            let var = var.var;
            self.get(&var).cloned().unwrap_or_default() + Borrow { var, label }
        } else {
            Borrows::new()
        }
    }

    /// Returns the list of all borrows resulting from
    /// the move of `var` at CFG label `label`.
    pub fn move_var(&self, var: Var<'ctx>) -> Borrows<'ctx> {
        self.get(&var).cloned().unwrap_or_default()
    }

    /// Records new `borrows` related to the borrower that living at `place`.
    pub fn add_borrows(&mut self, place: &Place<'ctx>, borrows: Borrows<'ctx>) {
        match place {
            VarP(v) => {
                self.borrows.insert(*v, borrows);
            }
            IndexP(array, _) => {
                // If you assign something to an element of an array, we need to ADD the borrows
                // to the old ones, since we have no way to know which (if any) borrows are
                // invalidated by that assignment.
                let entry = self.borrows.entry(*array);
                entry.or_default().extend(borrows);
            }
            FieldP(strukt, _) => {
                let entry = self.borrows.entry(*strukt);
                entry.or_default().extend(borrows); // TODO: change, too coarse
                                                    // grained. Separate borrows
                                                    // between fields.
            }
        }
    }

    /// Returns all borrows of a variable.
    pub fn borrows<'a>(&'a self, var: Var<'ctx>) -> impl Iterator<Item = &'a Borrow<'ctx>> + 'a {
        self.borrows
            .values()
            .flat_map(move |s| s.iter().filter(move |&borrow| borrow.var == var))
    }
}

impl<'ctx> fmt::Debug for Ledger<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.borrows.fmt(f)
    }
}

impl<'ctx, B: Into<Borrows<'ctx>>> Add<(Var<'ctx>, B)> for Ledger<'ctx> {
    type Output = Ledger<'ctx>;

    fn add(mut self, (lhs, borrows): (Var<'ctx>, B)) -> Self {
        self.add_var(lhs, borrows);
        self
    }
}

impl Default for Ledger<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ctx> Add for Ledger<'ctx> {
    type Output = Ledger<'ctx>;

    fn add(self, rhs: Self) -> Self {
        self.bitor(rhs)
    }
}

impl<'ctx> BitOr for Ledger<'ctx> {
    type Output = Ledger<'ctx>;

    fn bitor(mut self, rhs: Self) -> Self {
        self.borrows.extend(rhs.borrows);
        self
    }
}

impl<'ctx, 'a> Sub<&'a Ledger<'ctx>> for Ledger<'ctx> {
    type Output = Ledger<'ctx>;

    fn sub(mut self, rhs: &Self) -> Self {
        for key in rhs.borrows.keys() {
            self.borrows.remove(key);
        }
        self
    }
}

impl<'a, 'ctx> Sub<&'a Var<'ctx>> for Ledger<'ctx> {
    type Output = Ledger<'ctx>;

    fn sub(mut self, var: &Var<'ctx>) -> Self {
        self.borrows.remove(var);
        self
    }
}

impl<'ctx: 'a, 'a, I: IntoIterator<Item = &'a Var<'ctx>>> Sub<I> for Ledger<'ctx> {
    type Output = Ledger<'ctx>;

    fn sub(mut self, iter: I) -> Self {
        for var in iter {
            self.borrows.remove(var);
        }
        self
    }
}

impl<'ctx> Sum for Ledger<'ctx> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|acc, el| acc | el).unwrap_or_default()
    }
}

impl<'ctx> Deref for Ledger<'ctx> {
    type Target = HashMap<Var<'ctx>, Borrows<'ctx>>;

    fn deref(&self) -> &Self::Target {
        &self.borrows
    }
}

impl DerefMut for Ledger<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.borrows
    }
}
