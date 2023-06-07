//! Defining the notion of borrows.

use std::fmt;

use crate::mir::{CfgLabel, Place};
use crate::utils::set::Set;

/// A borrow: a variable that has been borrowed.
///
/// Knowing label + var make each borrow unique.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Borrow<'ctx> {
    /// Label in which the borrow was made.
    pub label: CfgLabel,
    /// Borrowed place. NOT the borrower.
    pub place: Place<'ctx>,
}

impl fmt::Debug for Borrow<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}{:?}", self.place, self.label)
    }
}

/// Set of borrowed variables.
pub type Borrows<'ctx> = Set<Borrow<'ctx>>;
