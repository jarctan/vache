//! Defining the notion of borrows.

use std::fmt;

use crate::mir::{CfgLabel, Loc, Place, Pointer, Span};
use crate::utils::set::Set;

/// A borrow: a variable that has been borrowed.
///
/// Knowing label + var make each borrow unique.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Borrow<'ctx> {
    /// Label in which the borrow was made.
    pub label: CfgLabel,
    /// Span at which the borrow was made.
    pub span: Span,
    /// Stakeholder of the borrow.
    pub borrower: Loc<'ctx>,
    /// Borrowed location. NOT the borrower.
    pub ptr: Pointer<'ctx>,
}

impl<'ctx> Borrow<'ctx> {
    /// Gets the borrowed location.
    pub fn borrowed_loc(&self) -> Loc<'ctx> {
        *self.ptr.loc()
    }

    /// Gets the borrowed place.
    pub fn borrowed_place(&self) -> Place<'ctx> {
        *self.ptr.place()
    }
}

impl fmt::Debug for Borrow<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}{:?}%{}", self.ptr, self.label, self.borrower)
    }
}

/// Borrowed variables.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Borrows<'ctx> {
    /// No borrow at all.
    None,
    /// We are an alias at another location.
    ///
    /// The borrow is then the witness of that alias: it must be a mutable
    /// borrow onto the aliased pointer.
    Aliased(Borrow<'ctx>),
    /// Some borrows.
    Distinct(Set<Borrow<'ctx>>),
}

impl<'ctx> Borrows<'ctx> {
    /// Creates a new empty [`Borrows`] with no borrow.
    pub fn new() -> Self {
        Self::None
    }

    /// Do we have any borrow?
    pub fn is_empty(&self) -> bool {
        match self {
            Borrows::None => true,
            Borrows::Aliased(_) | Borrows::Distinct(_) => false,
        }
    }
}

impl Default for Borrows<'_> {
    fn default() -> Self {
        Self::new()
    }
}

/// Set of borrowed variables.
pub type BorrowSet<'ctx> = Set<Borrow<'ctx>>;
