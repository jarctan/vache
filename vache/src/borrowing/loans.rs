//! Specifying loans, and in particular the laws of Loans
//!
//! Law of loans:
//! * no two mutable borrows may exist
//! * there cannot be a mutable borrow if there is an immutable one
//!
//! Will `panic` if these laws are not enforced.

use std::iter::Sum;
use std::ops::Add;

use super::{Borrow, Borrows};
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
    Immut(Set<Borrow<'ctx>>),
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
    /// Returns `false` if the borrow could not be inserted without violating
    /// the law of loans.
    pub fn insert(&mut self, borrow: Borrow<'ctx>) -> bool {
        match (borrow.mutable, &mut *self) {
            (true, Loans::None) => {
                *self = Loans::Mut(borrow);
                true
            }
            (false, Loans::None) => {
                let mut set = Set::new();
                set.insert(borrow);
                *self = Loans::Immut(set);
                true
            }
            (false, Loans::Immut(borrows)) => {
                borrows.insert(borrow);
                true
            }
            (_, _) => false,
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
    pub fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = Borrow<'ctx>>,
    {
        for borrow in iter {
            assert!(
                self.insert(borrow),
                "Could not extend those loans with the newly provided ones"
            );
        }
    }
}

impl<'ctx> From<Loans<'ctx>> for Borrows<'ctx> {
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
