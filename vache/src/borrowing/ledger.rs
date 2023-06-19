//! Declaring here the annotations to the CFG we compute during the analysis.

use std::collections::HashMap;
use std::default::default;
use std::fmt;
use std::iter::Extend;
use std::iter::Sum;
use std::ops::{Add, Sub};

use super::borrow::{Borrow, Borrows};
use super::LocTree;
use crate::mir::Mode;
use crate::mir::Reference;
use crate::mir::{CfgLabel, Loc, Place};
use crate::utils::boxed;
use crate::utils::set::Set;

/// A loan ledger.
#[derive(Clone, PartialEq, Eq, Default)]
pub struct Ledger<'ctx> {
    /// Map between variables defined in this environment and their borrows.
    borrows: LocTree<'ctx, Borrows<'ctx>>,
    /// Map between locations defined in this environment and their loans (i.e.,
    /// all the borrows they have conceded to).
    ///
    /// Invariant: `self.loans` and `self.borrows` are consistent (`Borrow`s are
    /// in both).
    loans: HashMap<Loc<'ctx>, Borrows<'ctx>>,
    /// Invalidated borrows.
    invalidations: Borrows<'ctx>,
}

impl<'ctx> Ledger<'ctx> {
    /// Returns the complete (deep, nested) list of all borrows resulting from
    /// the borrow of `reference` at CFG label `label`.
    pub fn borrow(
        &mut self,
        borrower: impl Into<Place<'ctx>>,
        reference: &Reference<'ctx>,
        label: CfgLabel,
    ) -> Borrows<'ctx> {
        let borrower = borrower.into();
        match reference.mode() {
            Mode::Borrowed | Mode::SBorrowed | Mode::MutBorrowed | Mode::SMutBorrowed => {
                // You clone their loans + the loan into the borrowed pointer
                self.borrows(reference.place())
                    .filter(|b| b.place != borrower) // Remove borrows into ourselves.
                    .map(|borrow| Borrow {
                        label: borrow.label,
                        borrower, // Note that we update the borrower. That's re-borrowing in action
                        place: borrow.place,
                    })
                    .collect::<Borrows>()
                    + Borrow {
                        place: *reference.place(),
                        borrower,
                        label,
                    }
            }
            Mode::Moved => {
                // Moving invalidates all the loans of the previous place
                let loans: Vec<_> = self.loans(*reference.place()).copied().collect();
                self.invalidations.extend(loans);

                // You take their entire place, so you take all their borrows!
                // We remove the loans from that place, and
                self.borrows(reference.place())
                    .filter(|b| b.place != borrower) // Remove borrows into ourselves.
                    .map(|borrow| Borrow {
                        label: borrow.label,
                        borrower, // Note that we update the borrower. That's re-borrowing in action
                        place: borrow.place,
                    })
                    .collect::<Borrows>()
            }
            Mode::Cloned => Borrows::new(),
            Mode::Assigning => panic!("Cannot borrow a variable in an assigning mode"),
        }
    }

    /// Removes a place from the tracked locations in the ledger.
    ///
    /// Is a no-op if the place is not a location, but only part of it.
    pub fn flush_place(&mut self, place: impl Into<Place<'ctx>>) {
        let place = place.into();
        if let Ok(loc) = Loc::try_from(place) {
            self.flush_loc(loc);
        }
    }

    /// Removes a location from the tracked locations in the ledger.
    ///
    /// Returns the borrows that have been invalidated.
    pub fn flush_loc(&mut self, loc: impl Into<Loc<'ctx>>) -> Borrows<'ctx> {
        let loc = loc.into();

        let borrows: Borrows = self
            .borrows
            .remove(loc)
            .map_or(default(), |node| node.into());

        for borrow in borrows.iter().copied() {
            // If the borrow is invalidated, remove it from the loans.
            if !self.invalidations.contains(&borrow) {
                let loc = borrow.place.root();
                self.loans
                    .get_mut(&loc)
                    .unwrap_or_else(|| panic!("Ledger error: want to free a non-invalidated borrow {borrow:?} but loaner is not alive anymore!"))
                    .remove(&borrow);
            }
        }

        let removed_loans = self.loans.remove(&loc);
        let loans: Vec<Borrow> = removed_loans
            .as_ref()
            .map(|loans| loans.iter().copied().collect())
            .unwrap_or_default();
        self.invalidations.extend(loans);

        borrows
    }

    /// Simultaneous removal of several locations from the ledger.
    pub fn flush_locs(&mut self, locs: impl Iterator<Item = Loc<'ctx>>) {
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
                to_clean.insert(loc);
            }

            for borrow in borrows.iter().copied() {
                // If the borrow is not invalidated, remove it from the loans.
                if !self.invalidations.contains(&borrow) {
                    let loc = borrow.place.root();
                    self.loans
                        .get_mut(&loc)
                        .unwrap_or_else(|| panic!("Ledger error: want to free a non-invalidated borrow {borrow:?} but loaner is not alive anymore!"))
                        .remove(&borrow);
                }
            }
        }

        // THEN, remove the locations themselves from the ledger
        // We need to do that in a second step because we don't want to add as
        // invalidated loans loans that were made by locations that are being
        // flushed at the same time as the loaner!
        for loc in to_clean {
            let removed_loans = self.loans.remove(loc);
            let loans: Vec<Borrow> = removed_loans
                .as_ref()
                .map(|loans| {
                    loans
                        .iter()
                        .copied()
                        .filter(|loan| !locs.contains(&loan.borrower.root()))
                        .collect()
                })
                .unwrap_or_default();
            self.invalidations.extend(loans);
        }
    }

    /// Sets the new `borrows` of `place`.
    ///
    /// Note: will filter borrows that are made into `place` itself.
    pub fn set_borrows(
        &mut self,
        place: impl Into<Place<'ctx>>,
        borrows: impl Into<Borrows<'ctx>>,
    ) {
        let place = place.into();

        // Remove borrows into ourselves.
        let mut borrows = borrows.into();
        borrows.retain(|b| b.place != place);

        // First, flush the place.
        self.flush_place(place);

        // Register on the loaner side
        for &borrow in &borrows {
            self.loans
                .entry(borrow.place.root())
                .or_insert_with(default)
                .insert(borrow);
        }

        // Register on the borrower side
        match Loc::try_from(place) {
            Ok(loc) => {
                if borrows.is_empty() {
                    self.borrows.remove(loc);
                } else {
                    *self.borrows.get_mut_or_insert(loc) = borrows;
                }
            }
            Err(()) => {
                let root = place.root();
                if !borrows.is_empty() {
                    // If you assign something to an element of an array, we need to ADD the borrows
                    // to the old ones, since we have no way to know which (if any) borrows are
                    // invalidated by that assignment.
                    let entry = self.borrows.get_mut_or_insert(root);
                    entry.extend(borrows);
                }
            }
        }
    }

    /// Returns all loans of a place.
    pub fn loans<'a>(
        &'a self,
        place: Place<'ctx>,
    ) -> Box<dyn Iterator<Item = &'a Borrow<'ctx>> + 'a> {
        self.loans
            .get(&place.root())
            .map_or(boxed(std::iter::empty()), |loans| boxed(loans.iter()))
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

    /// Has this place conceded any loan that is still active?
    pub fn has_loans(&self, place: Place<'ctx>) -> bool {
        if let Some(loans) = self.loans.get(&place.root()) {
            !loans.is_empty()
        } else {
            false
        }
    }
}

impl fmt::Debug for Ledger<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.borrows.fmt(f)
    }
}

impl<'ctx, B: Into<Borrows<'ctx>>> Add<(Place<'ctx>, B)> for Ledger<'ctx> {
    type Output = Ledger<'ctx>;

    fn add(mut self, (place, borrows): (Place<'ctx>, B)) -> Self {
        self.set_borrows(place, borrows);
        self
    }
}

impl<'ctx> Sub<Place<'ctx>> for Ledger<'ctx> {
    type Output = Ledger<'ctx>;

    fn sub(mut self, place: Place<'ctx>) -> Self {
        self.flush_place(place);
        self
    }
}

impl<'ctx: 'a, 'a, I: IntoIterator<Item = Loc<'ctx>>> Sub<I> for Ledger<'ctx> {
    type Output = Ledger<'ctx>;

    fn sub(mut self, locs: I) -> Self {
        self.flush_locs(locs.into_iter());
        self
    }
}

impl<'ctx> Sum for Ledger<'ctx> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut loans: HashMap<Loc, Borrows> = default();
        let mut borrows: LocTree<'ctx, Borrows<'ctx>> = default();
        let mut invalidations: Borrows = default();

        for ledger in iter {
            borrows.append(ledger.borrows);
            for (k, v) in ledger.loans {
                loans.entry(k).or_insert_with(default).extend(v);
            }
            invalidations.extend(ledger.invalidations);
        }
        Self {
            loans,
            borrows,
            invalidations,
        }
    }
}
