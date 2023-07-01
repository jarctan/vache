//! Specifying loans, and in particular the laws of Loans
//!
//! Law of loans:
//! * no two mutable borrows may exist
//! * there cannot be a mutable borrow if there is an immutable one
//!
//! Will `panic` if these laws are not enforced.

use std::iter::Sum;
use std::ops::Add;

use super::{Borrow, BorrowSet};
use crate::utils::boxed;
use crate::utils::set::Set;

/// Set of loans.
///
/// Is more precise than a simple set of borrows in that it maintains the
/// invariant that there is always:
/// * at most one mutable borrow
/// * no mutable borrow if there is at least one immutable borrow
#[derive(Clone, PartialEq, Eq, Default)]
pub enum Loans<'ctx> {
    /// Only one mutable loan.
    Mut(Borrow<'ctx>),
    /// Only immutable loans.
    Immut(BorrowSet<'ctx>),
    /// No loan.
    #[default]
    None,
}

impl<'ctx> Loans<'ctx> {
    /// Removes a borrow from the [`Loans`], returning `true` iff the borrow was
    /// found (and removed).
    pub fn remove(&mut self, borrow: &Borrow<'ctx>) -> bool {
        match self {
            Loans::Mut(borrow2) => {
                if borrow == borrow2 {
                    *self = Loans::None;
                    true
                } else {
                    false
                }
            }
            Loans::Immut(borrows) => {
                let was_there = borrows.remove(borrow);
                if borrows.is_empty() {
                    *self = Loans::None;
                }
                was_there
            }
            Loans::None => false,
        }
    }

    /// Inserts a borrow in the [`Loans`].
    ///
    /// Returns the set of [`Borrow`]s that contradicts that insertion (or empty
    /// set if there is none).
    ///
    /// # Errors
    /// Returns an error if the insertion cannot be done in any way. In that
    /// case, it returns the mutable borrow that contradicts the new borrow.
    pub fn insert(&mut self, borrow: Borrow<'ctx>) -> Result<BorrowSet<'ctx>, Borrow<'ctx>> {
        match (borrow.mutable, &mut *self) {
            (true, Loans::None) => {
                *self = Loans::Mut(borrow);
                Ok(Set::new())
            }
            (false, Loans::None) => {
                let mut set = Set::new();
                set.insert(borrow);
                *self = Loans::Immut(set);
                Ok(Set::new())
            }
            (false, Loans::Immut(borrows)) => {
                borrows.insert(borrow);
                Ok(Set::new())
            }
            (_, Loans::Mut(mut_borrow)) => Err(*mut_borrow),
            (true, Loans::Immut(borrows)) => {
                let borrows = std::mem::take(borrows);
                *self = Loans::Mut(borrow);
                Ok(borrows.iter().copied().collect())
            }
        }
    }

    /// Gets an iterator over all the borrows in the [`Loans`].
    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Borrow<'ctx>> + 'a> {
        match self {
            Loans::None => boxed(std::iter::empty()),
            Loans::Mut(borrow) => boxed(std::iter::once(borrow)),
            Loans::Immut(borrows) => boxed(borrows.iter()),
        }
    }

    /// Extends `self` with several borrows at once.
    ///
    /// # Panics
    /// Panics if adding those loans breaks the law of loans.
    #[must_use = "Add these loans to your immutable invalidations"]
    pub fn extend<I>(&mut self, iter: I) -> BorrowSet<'ctx>
    where
        I: IntoIterator<Item = Borrow<'ctx>>,
    {
        // Collect the set of invalidation
        let mut set = Set::new();
        for borrow in iter {
            // If we have immutable invalidations, we add them
            let borrows = self
                .insert(borrow)
                .expect("Reconciliating mutable borrow invalidations when merging loans is not supported yet");
            set.extend(borrows);
        }
        set
    }
}

impl<'ctx> From<Loans<'ctx>> for BorrowSet<'ctx> {
    fn from(loans: Loans<'ctx>) -> Self {
        match loans {
            Loans::Mut(borrow) => {
                let mut set = Set::new();
                set.insert(borrow);
                set
            }
            Loans::Immut(borrows) => borrows,
            Loans::None => Set::new(),
        }
    }
}

impl<'ctx> Add for Loans<'ctx> {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match (self, other) {
            (lhs, Loans::None) => lhs,
            (Loans::Immut(borrows1), Loans::Immut(borrows2)) => Loans::Immut(borrows1 + borrows2),
            (Loans::None, other) => other,
            _ => panic!(),
        }
    }
}

impl<'ctx> Sum for Loans<'ctx> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|acc, el| acc + el).unwrap_or_default()
    }
}
