//! Defining the notion of borrows.

use std::collections::hash_map;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

use crate::mir::{CfgLabel, Loc, Place, Pointer, Span};
use crate::utils::boxed;
use crate::utils::MultiSet;
use crate::utils::Set;

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
impl<'ctx> Loan<'ctx> {
    /// Transforms a loan into a borrow by adding information about the
    /// `borrower`.
    pub(crate) fn into_borrow(self, borrower: Loc<'ctx>) -> Borrow<'ctx> {
        let Self { label, span, ptr } = self;
        Borrow {
            label,
            span,
            ptr,
            borrower,
        }
    }
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

/// Set of borrowed variables.
pub type Borrows<'ctx> = Set<Borrow<'ctx>>;

/// Multiset of borrowed variables.
pub type BorrowCnt<'ctx> = MultiSet<Borrow<'ctx>>;

#[derive(Debug, Clone)]
pub enum InvalidationReason<'ctx> {
    Moved(Pointer<'ctx>),
    OutOfScope(Loc<'ctx>),
    MutationWithLiveBorrow {
        borrow: Borrow<'ctx>,
        mutation_span: Span,
    },
}

#[derive(Clone, Default)]
pub struct Invalidations<'ctx>(HashMap<Loan<'ctx>, Vec<InvalidationReason<'ctx>>>);

impl<'ctx> Invalidations<'ctx> {
    /// Creates a fresh, empty set of invalidations.
    pub fn new() -> Self {
        Self::default()
    }

    /// The corresponding set of invalidated loans, without duplicates.
    pub fn as_set(&self) -> Set<&Loan<'ctx>> {
        self.0.keys().collect()
    }

    /// Inserts a new invalidation for a given `loan` with a given reason.
    pub fn insert(&mut self, loan: impl Into<Loan<'ctx>>, reason: InvalidationReason<'ctx>) {
        self.0.entry(loan.into()).or_default().push(reason);
    }

    /// Returns one of the elements with the biggest occurrence.
    pub fn most_represented<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Loan<'ctx>> + 'a> {
        // Compute the max length of reasons (the most represented loan)
        let max = self.0.values().map(Vec::len).max();
        if let Some(max) = max {
            // Return all the elements that reach the max
            boxed(
                self.0
                    .iter()
                    .filter(move |(_, v)| v.len() == max)
                    .map(|(loan, _)| loan),
            )
        } else {
            boxed(std::iter::empty())
        }
    }

    /// Returns true iff the multiset contains a value.
    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        Loan<'ctx>: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.0.contains_key(value)
    }
}

impl<'ctx> IntoIterator for Invalidations<'ctx> {
    type IntoIter = hash_map::IntoIter<Loan<'ctx>, Vec<InvalidationReason<'ctx>>>;
    type Item = (Loan<'ctx>, Vec<InvalidationReason<'ctx>>);

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'ctx> Extend<(Loan<'ctx>, Vec<InvalidationReason<'ctx>>)> for Invalidations<'ctx> {
    fn extend<T: IntoIterator<Item = (Loan<'ctx>, Vec<InvalidationReason<'ctx>>)>>(
        &mut self,
        iter: T,
    ) {
        for (loan, invalidations) in iter {
            for invalidation in invalidations {
                self.insert(loan, invalidation);
            }
        }
    }
}

impl<'ctx> fmt::Debug for Invalidations<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}
