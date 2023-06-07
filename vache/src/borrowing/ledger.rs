//! Declaring here the annotations to the CFG we compute during the analysis.

use std::collections::HashMap;
use std::default::default;
use std::fmt;
use std::iter::Extend;
use std::iter::Sum;
use std::ops::{Add, BitOr, Sub};

use super::borrow::{Borrow, Borrows};
use super::LocTree;
use crate::mir::Reference;
use crate::mir::{CfgLabel, Loc, Place};
use crate::utils::boxed;

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
    pub fn borrow(&self, reference: &Reference<'ctx>, label: CfgLabel) -> Borrows<'ctx> {
        if reference.mode().is_borrowing() {
            self.borrows(reference.place()).collect::<Borrows>()
                + Borrow {
                    place: reference.place(),
                    label,
                }
        } else {
            Borrows::new()
        }
    }

    /// Removes a place from the tracked locations in the ledger.
    ///
    /// Is a no-op if the place is not a location, but only part of it.
    pub fn flush_place(&mut self, place: impl Into<Place<'ctx>>) {
        let place = place.into();
        if let Ok(loc) = place.try_into() {
            self.flush_loc(loc);
        }
    }

    /// Removes a location from the tracked locations in the ledger.
    pub fn flush_loc(&mut self, loc: Loc<'ctx>) {
        let removed_loans = self.loans.remove(&loc);
        let loans: Vec<Borrow> = removed_loans
            .as_ref()
            .map(|loans| loans.iter().copied().collect())
            .unwrap_or_default();
        self.invalidations.extend(loans);

        let borrows: Borrows = self
            .borrows
            .remove(loc)
            .map_or(default(), |node| node.into());

        for borrow in borrows {
            // If the borrow is invalidated, remove it from the loans.
            if !self.invalidations.contains(&borrow) {
                let loc = borrow.place.root();
                self.loans
                    .get_mut(&loc)
                    .expect(
                        "Ledger error: freeing a non-invalidated borrow whose loaner is not alive anymore!",
                    )
                    .remove(&borrow);
            }
        }
    }

    /*/// Flush as many locations as possible from the tracked locations.
    pub fn flush(&mut self) {
        let locs: Vec<Loc> = self
            .loans
            .iter()
            .filter(|(_, borrows)| borrows.is_empty())
            .map(|(&loc, _)| loc)
            .collect();
        for loc in locs {
            self.flush_loc(loc);
        }
    }

    /// Returns the list of all borrows resulting from
    /// the move of `var` at CFG label `label`.
    pub fn move_var_borrows(&self, var: impl Into<Var<'ctx>>) -> Borrows<'ctx> {
        self.borrows.get_all(Loc::from(var.into())).collect()
    }*/

    /// Sets the new `borrows` of `place`.
    pub fn set_borrows(
        &mut self,
        place: impl Into<Place<'ctx>>,
        borrows: impl Into<Borrows<'ctx>>,
    ) {
        let place = place.into();
        let borrows = borrows.into();

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
        place: Place<'ctx>,
    ) -> Box<dyn Iterator<Item = Borrow<'ctx>> + 'a> {
        self.borrows.get_all(place.root())
    }

    /// Returns the list of invalidations.
    pub fn invalidations<'a>(&'a self) -> impl Iterator<Item = Borrow<'ctx>> + 'a {
        self.invalidations.iter().copied()
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

impl<'ctx> Add for Ledger<'ctx> {
    type Output = Ledger<'ctx>;

    fn add(self, rhs: Self) -> Self {
        self.bitor(rhs)
    }
}

impl<'ctx> BitOr for Ledger<'ctx> {
    type Output = Ledger<'ctx>;

    fn bitor(mut self, rhs: Self) -> Self {
        todo!()
    }
}

impl<'ctx, 'a> Sub<&'a Ledger<'ctx>> for Ledger<'ctx> {
    type Output = Ledger<'ctx>;

    fn sub(mut self, rhs: &Self) -> Self {
        todo!()
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

    fn sub(mut self, iter: I) -> Self {
        for loc in iter {
            self.flush_loc(loc);
        }
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
