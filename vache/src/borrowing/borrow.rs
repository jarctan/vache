//! Defining the notion of borrows.

use std::fmt;

use crate::mir::{CfgLabel, Loc, Place, Pointer};
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
    pub ptr: Pointer<'ctx>,
    /// Is it a mutable borrow?
    pub mutable: bool,
}

impl<'ctx> Borrow<'ctx> {
    pub fn loc(&self) -> Loc<'ctx> {
        *self.ptr.loc()
    }

    pub fn place(&self) -> Place<'ctx> {
        *self.ptr.place()
    }
}

impl fmt::Debug for Borrow<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}{:?}", self.ptr, self.label)
    }
}

/// Set of borrowed variables.
pub type Borrows<'ctx> = Set<Borrow<'ctx>>;
