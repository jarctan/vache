//! Defining the notion of borrows.

use std::fmt;

use crate::mir::{CfgLabel, Loc};
use crate::utils::set::Set;

/// A borrow: a variable that has been borrowed.
///
/// Knowing label + var make each borrow unique.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Borrow<'ctx> {
    /// Label in which the borrow was made.
    pub label: CfgLabel,
    /// Stakeholder of the borrow.
    pub borrower: Loc<'ctx>,
    /// Borrowed location. NOT the borrower.
    pub loc: Loc<'ctx>,
}

impl fmt::Debug for Borrow<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}{:?}", self.loc, self.label)
    }
}

/// Set of borrowed variables.
pub type Borrows<'ctx> = Set<Borrow<'ctx>>;
