//! Defining and representing invalidations.
//!
//! Invalidations = borrows that contradict the law of loans.

use std::collections::hash_map;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

use codespan_reporting::diagnostic::Diagnostic;

use super::Loan;
use crate::mir::{Loc, Span};
use crate::utils::boxed;
use crate::utils::Set;

/// Reasons for invalidating a loan.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InvalidationReason<'ctx> {
    /// Borrow on a variable that goes out of scope.
    OutOfScope(Loc<'ctx>),
    /// Borrow still live when borrowed variable is mutated.
    MutationWithLiveBorrow {
        /// Span of the mutation.
        mutation_span: Span,
    },
}
impl<'ctx> InvalidationReason<'ctx> {
    /// Turns the invalidation reason into a compiler diagnostic.
    pub(crate) fn to_diagnostic(&self, loan: Loan<'ctx>) -> Diagnostic<()> {
        match self {
            InvalidationReason::OutOfScope(loc) => Diagnostic::warning()
                .with_message("invalidated borrow")
                .with_labels(vec![loan.span.as_label()])
                .with_notes(vec![format!(
                    "reason: borrow is still live when the value of the {loc:?} goes out of scope"
                )]),
            InvalidationReason::MutationWithLiveBorrow { mutation_span } => Diagnostic::warning()
                .with_message("invalidated borrow")
                .with_labels(vec![
                    loan.span.as_label().with_message("borrow made here..."),
                    mutation_span
                        .as_secondary_label()
                        .with_message("... and the borrowed variable is mutated here, while borrowed value is still valid"),
                ]),
        }
    }
}

/// Record of invalidations and their reasons.
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

    /// Inserts a new invalidation for a given `loan` with a given `reason`.
    pub fn insert(&mut self, loan: impl Into<Loan<'ctx>>, reason: InvalidationReason<'ctx>) {
        self.0.entry(loan.into()).or_default().push(reason);
    }

    /// Removes a given loan.
    pub fn remove(&mut self, loan: impl Into<Loan<'ctx>>) -> Vec<InvalidationReason<'ctx>> {
        self.0.remove(&loan.into()).unwrap_or_default()
    }

    /// Returns the loans that have been the most invalidated.
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
