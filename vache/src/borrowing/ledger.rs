//! Declaring here the annotations to the CFG we compute during the analysis.

use std::collections::HashMap;
use std::default::default;
use std::fmt;
use std::iter::Extend;
use std::iter::Sum;

use super::{Borrow, Borrows, Loans, LocTree};
use crate::mir::Mode;
use crate::mir::Reference;
use crate::mir::{CfgLabel, Loc, Place};
use crate::utils::set::Set;

/// Borrows that cannot be recovered from.
pub type UnrecoverableBorrows<'ctx> = HashMap<Borrow<'ctx>, Borrow<'ctx>>;

/// A loan ledger.
#[derive(Clone, PartialEq, Eq, Default)]
pub struct Ledger<'ctx> {
    /// Map between variables defined in this environment and their borrows.
    borrows: LocTree<'ctx, Borrows<'ctx>>,
    /// Map between locations defined in this environment and their loans (i.e.,
    /// all the borrows they have conceded).
    ///
    /// Invariant: `self.loans` and `self.borrows` are consistent (`Borrow`s are
    /// in both).
    loans: LocTree<'ctx, Loans<'ctx>>,
    /// Invalidated borrows.
    ///
    /// Currently, these are only immutable invalidations.
    invalidations: Borrows<'ctx>,
    /// Unrecoverable invalidated borrows.
    ///
    /// Currently, these are only mutable invalidations.
    unrecoverables: UnrecoverableBorrows<'ctx>,
}

impl<'ctx> Ledger<'ctx> {
    /// Returns the complete (deep, nested) list of all borrows resulting from
    /// the borrow of `reference` at CFG label `label`.
    pub fn borrow<'mir>(
        &mut self,
        borrower: Loc<'ctx>,
        reference: &Reference<'mir, 'ctx>,
        label: CfgLabel,
    ) -> Vec<Borrow<'ctx>> {
        let span = reference.as_ptr().span;
        match reference.mode() {
            mode @ (Mode::Borrowed | Mode::SBorrowed | Mode::MutBorrowed | Mode::SMutBorrowed) => {
                // You clone their loans + the loan into the borrowed pointer
                vec![Borrow {
                    ptr: reference.as_ptr(),
                    borrower,
                    label,
                    span,
                    mutable: matches!(mode, Mode::MutBorrowed | Mode::SMutBorrowed),
                }]
            }
            Mode::Moved => {
                // You take their entire place, so you take all their borrows!
                // We remove the loans from that place, and
                let borrows = self
                    .borrows(reference.place())
                    .map(move |borrow| Borrow {
                        label: borrow.label,
                        borrower, // Note that we update the borrower
                        span: borrow.span,
                        ptr: borrow.ptr,
                        mutable: borrow.mutable,
                    })
                    .collect();

                self.flush_loc(*reference.loc(), true);

                borrows
            }
            Mode::Cloned => vec![],
        }
    }

    /// Removes a place from the tracked locations in the ledger.
    ///
    /// Is a no-op if the place is not a location, but only part of it.
    pub fn flush_place(&mut self, place: impl Into<Place<'ctx>>, force: bool) {
        let place = place.into();
        if let Ok(loc) = Loc::try_from(place) {
            self.flush_loc(loc, force);
        }
    }

    /// Removes a location from the tracked locations in the ledger.
    ///
    /// Returns the borrows that have been invalidated.
    fn flush_loc(&mut self, loc: impl Into<Loc<'ctx>>, force: bool) -> Borrows<'ctx> {
        let loc = loc.into();

        let borrows: Borrows = self
            .borrows
            .remove(loc)
            .map_or(default(), |node| node.into());

        for borrow in borrows.iter() {
            // If the borrow is not invalidated, remove it from the loans.
            if !self.invalidations.contains(borrow) {
                self.loans
                    .get_mut(borrow.loc())
                    .unwrap_or_else(|| panic!("Ledger error: want to free a non-invalidated borrow {borrow:?} but loaner is not alive anymore!"))
                    .remove(borrow);
            }
        }

        if !borrows.is_empty() || force {
            let removed_loans = self
                .loans
                .remove(loc)
                .map(Loans::from)
                .map(Borrows::from)
                .unwrap_or_default();
            self.invalidations.extend(removed_loans);
        }

        borrows
    }

    /// Simultaneous removal of several locations from the ledger.
    pub fn flush_locs(&mut self, locs: impl Iterator<Item = Loc<'ctx>>, force: bool) {
        let locs: Set<_> = locs.collect();

        // First, remove all the borrows of the locations to flush
        // We store at the same time the locations we really want to remove from the
        // ledger: these are those that have at least one borrow active. Those
        // that are well-behaved and have no borrow left can stay longer without
        // problems, allowing borrowers of that location not to be invalidated
        let mut to_clean: Set<_> = default();
        for loc in locs.iter() {
            let borrows: Borrows = self
                .borrows
                .remove(loc)
                .map_or(default(), |node| node.into());

            if !borrows.is_empty() {
                to_clean.insert(*loc);
            }

            for borrow in borrows.iter() {
                // If the borrow is not invalidated, remove it from the loans.
                if !self.invalidations.contains(borrow) {
                    self.loans
                        .get_mut(borrow.loc())
                        .unwrap_or_else(|| panic!("Ledger error: want to free a non-invalidated borrow {borrow:?} but loaner is not alive anymore!"))
                        .remove(borrow);
                }
            }
        }

        // THEN, remove the locations themselves from the ledger
        // We need to do that in a second step because we don't want to add as
        // invalidated loans loans that were made by locations that are being
        // flushed at the same time as the loaner!
        for loc in if force { to_clean.iter() } else { locs.iter() } {
            let removed_loans = self
                .loans
                .remove(loc)
                .map(Loans::from)
                .map(Borrows::from)
                .unwrap_or_default();
            let loans = removed_loans
                .iter()
                .filter(|loan| !locs.contains(&loan.borrower));
            self.invalidations.extend(loans);
        }
    }

    /// Tries to flatten/merge the list of sets of borrows into a set of
    /// borrows, checking for consistency between borrows (we cannot merge
    /// if we have two mutable borrows, for instance).
    ///
    /// Assertion: the inner sets are consistent.
    ///
    /// Invariant: always returns a set that does not contradict itself.
    pub fn flatten<I1: IntoIterator<Item = Borrow<'ctx>>, I2: IntoIterator<Item = I1>>(
        &mut self,
        borrows: I2,
    ) -> Borrows<'ctx> {
        let mut map = HashMap::new();
        let mut retained = Set::new();
        for borrows in borrows {
            for borrow in borrows {
                let loans: &mut Loans = map.entry(borrow.loc()).or_default();

                // Depending on the result of the insertion, find who to blame
                // We always try to invalidate immutable borrows first because
                // that's what we can handle best afterward But
                // if need but, we emit an unrecoverable invalidation
                match loans.insert(borrow) {
                    Ok(borrows) => {
                        for borrow in borrows {
                            self.invalidations.insert(borrow);
                        }
                        retained.insert(borrow);
                    }
                    Err(contradiction) => {
                        if borrow.mutable {
                            self.unrecoverables.insert(borrow, contradiction);
                        } else {
                            self.invalidations.insert(borrow);
                        }
                    }
                }
            }
        }
        retained
    }

    /// Sets the new `borrows` of `place`.
    ///
    /// Note: will filter borrows that are made into `place` itself.
    pub fn set_borrows(&mut self, place: impl Into<Place<'ctx>>, mut borrows: Borrows<'ctx>) {
        let place = place.into();
        let loc = place.root();

        // First, flush the place.
        self.flush_place(place, true);

        // Remove borrows into ourselves.
        if let Ok(loc) = place.try_into() {
            borrows.retain(|b| b.loc() != loc);
        }

        // Register on the loaner side
        for &borrow in &borrows {
            match self.loans.get_mut_or_insert(borrow.loc()).insert(borrow) {
                Ok(borrows) => {
                    for borrow in borrows {
                        self.invalidations.insert(borrow);
                    }
                }
                Err(contradiction) => {
                    if borrow.mutable {
                        self.unrecoverables.insert(borrow, contradiction);
                    } else {
                        self.invalidations.insert(borrow);
                    }
                }
            }
        }

        // Register on the borrower side
        match Loc::try_from(place) {
            Ok(loc) => {
                if borrows.is_empty() {
                    self.borrows.remove(loc);
                } else {
                    *self.borrows.get_mut_or_insert(loc) = borrows.into_iter().collect();
                }
            }
            Err(()) => {
                if !borrows.is_empty() {
                    // If you assign something to an element of an array, we need to ADD the borrows
                    // to the old ones, since we have no way to know which (if any) borrows are
                    // invalidated by that assignment.
                    let entry = self.borrows.get_mut_or_insert(loc);
                    entry.extend(borrows);
                }
            }
        }
    }

    /// Returns all loans of a place.
    pub fn loans<'a>(&'a self, loc: Loc<'ctx>) -> Box<dyn Iterator<Item = Borrow<'ctx>> + 'a> {
        self.loans.get_all(loc)
    }

    /// Returns all loans of a place.
    pub fn borrows<'a>(
        &'a self,
        place: impl Into<Place<'ctx>>,
    ) -> Box<dyn Iterator<Item = Borrow<'ctx>> + 'a> {
        self.borrows.get_all(place.into().root())
    }

    /// Returns the list of invalidations.
    pub fn invalidations<'a>(&'a self) -> impl Iterator<Item = Borrow<'ctx>> + 'a {
        self.invalidations.iter().copied()
    }

    /// Get the unrecoverables invalidations.
    pub fn unrecoverables<'a>(&'a self) -> Option<&'a UnrecoverableBorrows<'ctx>> {
        if self.unrecoverables.is_empty() {
            None
        } else {
            Some(&self.unrecoverables)
        }
    }
}

impl fmt::Debug for Ledger<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.borrows.fmt(f)
    }
}

impl<'ctx> Sum for Ledger<'ctx> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut loans: LocTree<Loans> = default();
        let mut borrows: LocTree<Borrows> = default();
        let mut invalidations: Borrows = default();
        let mut unrecoverables: UnrecoverableBorrows<'ctx> = default();

        for ledger in iter {
            invalidations.extend(loans.append(ledger.loans));
            borrows.append(ledger.borrows);
            invalidations.extend(ledger.invalidations);
            unrecoverables.extend(ledger.unrecoverables);
        }
        Self {
            loans,
            borrows,
            invalidations,
            unrecoverables,
        }
    }
}
