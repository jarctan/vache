//! Defining the notion of borrows.

use std::fmt;
use std::hash::Hash;

use crate::mir::{CfgLabel, Var};
use crate::utils::set::Set;

/// A borrow: a variable that has been borrowed.
///
/// Knowing label + var make each borrow unique.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Borrow<'ctx> {
    /// Label in which the borrow was made.
    pub label: CfgLabel,
    /// Borrowed variable. NOT the borrower.
    pub var: Var<'ctx>,
}

impl fmt::Debug for Borrow<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}{:?}", self.var, self.label)
    }
}

/// Set of borrowed variables.
pub type Borrows<'ctx> = Set<Borrow<'ctx>>;
