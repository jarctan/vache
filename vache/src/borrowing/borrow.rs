//! Defining the notion of borrows.

use std::fmt;

use crate::mir::{CfgLabel, Loc, Place, Pointer, Span};
use crate::utils::set::Set;
use crate::utils::MultiSet;

/// A loan: a variable that has been borrowed.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Loan<'ctx> {
    /// Label at which the loan was made.
    pub label: CfgLabel,
    /// Span at which the loan was made.
    pub span: Span,
    /// Borrowed location. NOT the borrower.
    pub ptr: Pointer<'ctx>,
}

impl<'ctx> From<Borrow<'ctx>> for Loan<'ctx> {
    fn from(borrow: Borrow<'ctx>) -> Self {
        Self {
            label: borrow.label,
            span: borrow.span,
            ptr: borrow.ptr,
        }
    }
}

impl fmt::Debug for Loan<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}{:?}", self.ptr, self.label)
    }
}

/// A borrow: a variable that has been borrowed.
///
/// A borrow also contains the name of the borrower.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Borrow<'ctx> {
    /// Label at which the borrow was made.
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

/// Multiset of borrowed variables.
pub type BorrowCnt<'ctx> = MultiSet<Borrow<'ctx>>;

/// Multiset of borrowed loans.
pub type LoanCnt<'ctx> = MultiSet<Loan<'ctx>>;
