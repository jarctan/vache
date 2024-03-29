//! Declaring here the annotations to the CFG we compute during the analysis.

use std::cmp::Ordering;
use std::default::default;
use std::fmt;
use std::iter::Extend;
use std::iter::Sum;

use itertools::Itertools;

use super::{Borrow, BorrowCnt, Borrows, InvalidationReason, Invalidations, Loan, LocTree};
use crate::mir::{CfgLabel, Loc, Mode, Place, Reference, Span};
use crate::utils::Set;

/// A loan ledger.
#[derive(Clone, Default)]
pub struct Ledger<'ctx> {
    /// Map between variables defined in this environment and their borrows.
    borrows: LocTree<'ctx, Borrows<'ctx>>,
    /// Map between locations defined in this environment and their loans (i.e.,
    /// all the borrows they have conceded).
    ///
    /// Invariant: `self.loans` and `self.borrows` are consistent (`Borrow`s are
    /// in both).
    loans: LocTree<'ctx, Borrows<'ctx>>,
    /// Invalidated borrows.
    ///
    /// Currently, these are only immutable invalidations.
    invalidations: Invalidations<'ctx>,
}

impl<'ctx> PartialEq for Ledger<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        self.borrows == other.borrows
            && self.loans == other.loans
            && self.invalidations.as_set() == other.invalidations.as_set()
    }
}

impl<'ctx> Eq for Ledger<'ctx> {}

impl<'ctx> Ledger<'ctx> {
    /// Returns the complete (deep, nested) list of all borrows resulting from
    /// the borrow of `reference` at CFG label `label`.
    pub fn borrow<'mir>(
        &mut self,
        references: &Vec<&Reference<'mir, 'ctx>>,
        label: CfgLabel,
    ) -> (Vec<Loan<'ctx>>, Set<Loc<'ctx>>) {
        let mut flushed = Set::new();
        let mut loans = vec![];

        for reference in references {
            let span = reference.as_ptr().span;
            match reference.mode() {
                Mode::Borrowed | Mode::SBorrowed | Mode::MutBorrowed | Mode::SMutBorrowed => {
                    // You clone their loans + the loan into the borrowed pointer
                    loans.push(Loan {
                        ptr: reference.as_ptr(),
                        label,
                        span,
                    });
                }
                Mode::Moved => {
                    // You take their place, so you take all their borrows!
                    // We remove the loans from that place
                    loans.extend(self.borrows(reference.place()).map(move |borrow| Loan {
                        label: borrow.label,
                        span: borrow.span,
                        ptr: borrow.ptr,
                    }));

                    flushed.insert(*reference.loc());
                }
                Mode::Cloned => (),
            }
        }

        // All moved are invalidated
        (loans, flushed)
    }

    /// Removes a place from the tracked locations in the ledger.
    ///
    /// Is a no-op if the place is not a location, but only part of it.
    pub fn flush_place(&mut self, place: impl Into<Place<'ctx>>, span: Span, force: bool) {
        let place = place.into();
        if let Ok(loc) = Loc::try_from(place) {
            self.flush_loc(loc, span, force);
        }
    }

    /// Removes a location from the tracked locations in the ledger.
    ///
    /// Returns the borrows that have been removed as a consequence.
    ///
    /// Span is an extra annotation to say where the loc is being flushed. Only
    /// used for reporting.
    fn flush_loc(
        &mut self,
        loc: impl Into<Loc<'ctx>>,
        span: Span,
        force: bool,
    ) -> Set<Borrow<'ctx>> {
        let loc = loc.into();

        // Get back our borrows, but filter out those that are about us (they are not
        // getting invalidated, since we're flushing all the the location at once!)
        let borrows: Borrows = self
            .borrows
            .remove(loc)
            .map_or(default(), Borrows::from)
            .into_iter()
            .filter(|b| !matches!(loc.partial_cmp(&b.borrowed_loc()), Some(Ordering::Less)))
            .collect();

        /*#[cfg(not(test))]
        println!(
            "Flushing {:?}{} with borrows {borrows:?}",
            loc,
            if force { " forcefully" } else { "" }
        );*/
        for borrow in borrows.iter() {
            // If the borrow is not invalidated, remove it from the loans.
            if !self.invalidations.contains(&Loan::from(*borrow)) {
                self.loans
                            .get_mut(borrow.borrowed_loc())
                            .unwrap_or_else(|| panic!("Ledger error: want to free a non-invalidated borrow {borrow:?} but loaner is not alive anymore!"))
                            .remove(borrow);
            }
        }

        if !borrows.is_empty() || force {
            let removed_loans = self
                .loans
                .remove(loc)
                .map(BorrowCnt::from)
                .unwrap_or_default()
                .into_iter()
                .map(Loan::from)
                .map(|loan| (loan, vec![InvalidationReason::OutOfScope(loc, span)]));
            /*if !removed_loans.is_empty() {
                #[cfg(not(test))]
                println!("Invalidated ici with {:?}", removed_loans);
            }*/
            self.invalidations.extend(removed_loans);
        }

        borrows
    }

    /// Simultaneous removal of several locations from the ledger.
    ///
    /// Span is an extra annotation to say where the loc is being flushed. Only
    /// used for reporting.
    pub fn flush_locs(
        &mut self,
        locs: impl IntoIterator<Item = Loc<'ctx>>,
        span: Span,
        force: bool,
    ) {
        let locs_vec = locs.into_iter().unique().collect_vec();
        let locs: LocTree<_> = locs_vec.iter().copied().collect();

        if locs.is_empty() {
            return;
        }

        // First, remove all the borrows of the locations to flush
        // We store at the same time the locations we really want to remove from the
        // ledger: these are those that have at least one borrow active. Those
        // that are well-behaved and have no borrow left can stay longer without
        // problems, allowing borrowers of that location not to be invalidated
        let mut to_clean: Vec<_> = default();
        for loc in locs_vec.iter() {
            let borrows: Borrows = self
                .borrows
                .remove(loc)
                .map_or(default(), Borrows::from)
                .into_iter()
                .filter(|b| !matches!(loc.partial_cmp(&b.borrowed_loc()), Some(Ordering::Less)))
                .collect();

            if !borrows.is_empty() {
                to_clean.push(*loc);
            }

            for borrow in borrows.iter() {
                // If the borrow is not invalidated, remove it from the loans.
                if !self.invalidations.contains(&Loan::from(*borrow)) {
                    self.loans
                            .get_mut(borrow.borrowed_loc())
                            .unwrap_or_else(|| panic!("Ledger error: want to free a non-invalidated borrow {borrow:?} but loaner is not alive anymore!"))
                            .remove(borrow);
                }
            }
        }

        // THEN, remove the locations themselves from the ledger
        // We need to do that in a second step because we don't want to add as
        // invalidated loans loans that were made by locations that are being
        // flushed at the same time as the loaner!
        for loc in if force {
            locs_vec.iter()
        } else {
            to_clean.iter()
        } {
            let removed_loans = self
                .loans
                .remove(loc)
                .map(BorrowCnt::from)
                .unwrap_or_default();
            let removed_loans = removed_loans
                .into_iter()
                .filter(|loan| !locs.contains(loan.borrower))
                .map(Loan::from)
                .map(|loan| (loan, vec![InvalidationReason::OutOfScope(*loc, span)]));
            self.invalidations.extend(removed_loans);
        }
    }

    /// Records the updated borrows of a place in the ledger.
    ///
    /// Span is an extra annotation to say where the loc is being flushed. Only
    /// used for reporting.
    pub fn set_borrows(
        &mut self,
        place: impl Into<Place<'ctx>>,
        borrows: Borrows<'ctx>,
        span: Span,
    ) {
        let place = place.into();
        let loc = place.root();

        // First, flush the place.
        self.flush_place(place, span, true);

        // Register on the loaner side
        for &borrow in &borrows {
            self.loans
                .get_mut_or_insert(borrow.borrowed_loc())
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

    /// Returns all borrows of a place.
    pub fn borrows<'a>(
        &'a self,
        place: impl Into<Place<'ctx>>,
    ) -> Box<dyn Iterator<Item = Borrow<'ctx>> + 'a> {
        self.borrows.get_all(place.into().root())
    }

    /// **Flushes** and returns the list of invalidations.
    pub fn invalidations(&mut self) -> Invalidations<'ctx> {
        std::mem::take(&mut self.invalidations)
    }

    /// Swaps the borrows for two memory locations in the ledger.
    pub(crate) fn swap(&mut self, loc1: Loc<'ctx>, loc2: Loc<'ctx>) {
        self.borrows.swap(loc1, loc2);
    }
}

impl fmt::Debug for Ledger<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Ledger")
            .field("borrows", &self.borrows)
            .field("invalidations", &self.invalidations)
            .finish()
    }
}

impl<'ctx> Sum for Ledger<'ctx> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut loans: LocTree<Borrows> = default();
        let mut borrows: LocTree<Borrows> = default();
        let mut invalidations: Invalidations = default();

        for ledger in iter {
            loans.append(ledger.loans);
            borrows.append(ledger.borrows);
            invalidations.extend(ledger.invalidations);
        }
        Self {
            loans,
            borrows,
            invalidations,
        }
    }
}
