//! Declaring here the annotations to the CFG we compute during the analysis.

use std::collections::HashMap;
use std::default::default;
use std::fmt;
use std::iter::Extend;
use std::iter::Sum;

use super::{Borrow, BorrowCnt, BorrowSet, Borrows, Loan, LoanCnt, Loans, LocTree};
use crate::mir::{CfgLabel, Loc, Mode, Place, Reference};
use crate::utils::set::Set;

/// Borrows that cannot be recovered from.
pub type UnrecoverableBorrows<'ctx> = HashMap<Borrow<'ctx>, Borrow<'ctx>>;

/// A loan ledger.
#[derive(Clone, Eq, Default)]
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
    invalidations: LoanCnt<'ctx>,
    /// Unrecoverable invalidated borrows.
    ///
    /// Currently, these are only mutable invalidations.
    unrecoverables: UnrecoverableBorrows<'ctx>,
}

impl<'ctx> PartialEq for Ledger<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        self.borrows == other.borrows
            && self.loans == other.loans
            && self.invalidations.as_set() == other.invalidations.as_set()
            && self.unrecoverables == other.unrecoverables
    }
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
            Mode::Borrowed | Mode::SBorrowed | Mode::MutBorrowed | Mode::SMutBorrowed => {
                // You clone their loans + the loan into the borrowed pointer
                vec![Borrow {
                    ptr: reference.as_ptr(),
                    borrower,
                    label,
                    span,
                }]
            }
            Mode::Moved => {
                // All current loans will get invalidated
                for loan in self.loans(*reference.loc()).collect::<Vec<_>>() {
                    self.invalidations.insert(loan.into());
                }

                // You take their place, so you take all their borrows!
                // We remove the loans from that place
                let borrows = self
                    .borrows(reference.place())
                    .map(move |borrow| Borrow {
                        label: borrow.label,
                        borrower, // Note that we update the borrower
                        span: borrow.span,
                        ptr: borrow.ptr,
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
    /// Returns the borrows that have been removed as a consequence.
    fn flush_loc(&mut self, loc: impl Into<Loc<'ctx>>, force: bool) -> Set<Borrow<'ctx>> {
        let loc = loc.into();

        let borrows = self.borrows.remove(loc).map_or(default(), Borrows::from);

        /*#[cfg(not(test))]
        println!(
            "Flushing {:?}{} with borrows {borrows:?}",
            loc,
            if force { " forcefully" } else { "" }
        );*/

        match borrows {
            Borrows::None => default(),
            Borrows::Aliased(alias) => {
                self.loans
                            .get_mut(alias.borrowed_loc())
                            .unwrap_or_else(|| panic!("Ledger error: want to free a non-invalidated borrow {alias:?} but loaner is not alive anymore!"))
                            .remove(&alias);
                [alias].into_iter().collect()
            }
            Borrows::Distinct(borrows) => {
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
                        .map(Loan::from);
                    /*if !removed_loans.is_empty() {
                        #[cfg(not(test))]
                        println!("Invalidated ici with {:?}", removed_loans);
                    }*/
                    self.invalidations.extend(removed_loans);
                }

                borrows
            }
        }
    }

    /// Simultaneous removal of several locations from the ledger.
    pub fn flush_locs(&mut self, locs: impl Iterator<Item = Loc<'ctx>>, force: bool) {
        let locs: Set<_> = locs.collect();
        if locs.is_empty() {
            return;
        }
        /*#[cfg(not(test))]
        println!(
            "Flushing {:?}{}",
            locs,
            if force { " forcefully" } else { "" }
        );*/

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
                /*#[cfg(not(test))]
                println!("{loc:?} has borrows {borrows:?}, so must scheduled for cleanup");*/
                to_clean.insert(*loc);
            }

            match borrows {
                Borrows::None => (),
                Borrows::Aliased(_) => todo!(),
                Borrows::Distinct(borrows) => {
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
            }
        }

        // THEN, remove the locations themselves from the ledger
        // We need to do that in a second step because we don't want to add as
        // invalidated loans loans that were made by locations that are being
        // flushed at the same time as the loaner!
        for loc in if force { locs.iter() } else { to_clean.iter() } {
            let removed_loans = self
                .loans
                .remove(loc)
                .map(BorrowCnt::from)
                .unwrap_or_default();
            let removed_loans: Set<_> = removed_loans
                .into_iter()
                .filter(|loan| !locs.contains(&loan.borrower))
                .map(Loan::from)
                .collect();
            /*if !removed_loans.is_empty() {
                #[cfg(not(test))]
                println!(
                    "Invalidated by out of scope of {loc:?}: {:?} (only {locs:?} allowed)",
                    removed_loans
                );
            }*/
            self.invalidations.extend(removed_loans);
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
    ) -> BorrowSet<'ctx> {
        let mut map = HashMap::new();
        let mut retained = Set::new();
        for borrows in borrows {
            for borrow in borrows {
                let loans: &mut Loans = map.entry(borrow.borrowed_loc()).or_default();

                // Depending on the result of the insertion, find who to blame
                // We always try to invalidate immutable borrows first because
                // that's what we can handle best afterward But
                // if need but, we emit an unrecoverable invalidation
                match loans.insert(borrow) {
                    Ok(borrows) => {
                        for borrow in borrows {
                            /*#[cfg(not(test))]
                            println!("Invalidated flatten {:?}", borrow);*/
                            self.invalidations.insert(borrow.into());
                        }
                        retained.insert(borrow);
                    }
                    Err(_contra) => {
                        /*#[cfg(not(test))]
                        println!("Invalidated flatten 2 {:?}", borrow);*/
                        self.invalidations.insert(borrow.into());
                    }
                }
            }
        }
        retained
    }

    /// Records the updated borrows of a place in the ledger.
    pub fn set_borrows(&mut self, place: impl Into<Place<'ctx>>, borrows: Borrows<'ctx>) {
        match borrows {
            Borrows::None => self.set_borrow_set(place, Set::new()),
            Borrows::Distinct(set) => self.set_borrow_set(place, set),
            Borrows::Aliased(alias) => self.set_aliased(place, alias),
        }
    }

    /// Sets the new `place` as an alias wrt `borrows`
    fn set_aliased(&mut self, place: impl Into<Place<'ctx>>, borrow: Borrow<'ctx>) {
        let place = place.into();

        if 1 <= 3 {
            unreachable!();
        }

        // First, flush the place.
        self.flush_place(place, true);

        // Register on the loaner side
        match self
            .loans
            .get_mut_or_insert(borrow.borrowed_loc())
            .insert(borrow)
        {
            Ok(borrows) => {
                for borrow in borrows {
                    self.invalidations.insert(borrow.into());
                }
            }
            Err(_contra) => {
                self.invalidations.insert(borrow.into());
            }
        }

        // Register on the borrower side
        let loc = Loc::try_from(place).expect("Alias must be locations");
        *self.borrows.get_mut_or_insert(loc) = Borrows::Aliased(borrow);
    }

    /// Sets the new `borrows` of `place`.
    ///
    /// Note: will filter borrows that are made into `place` itself.
    pub fn set_borrow_set(&mut self, place: impl Into<Place<'ctx>>, mut borrows: BorrowSet<'ctx>) {
        let place = place.into();
        let loc = place.root();

        // First, flush the place.
        self.flush_place(place, true);

        // Register on the loaner side
        for &borrow in &borrows {
            match self
                .loans
                .get_mut_or_insert(borrow.borrowed_loc())
                .insert(borrow)
            {
                Ok(borrows) => {
                    if !borrows.is_empty() {
                        /*#[cfg(not(test))]
                        println!("Mutable borrow {borrow:?} invalidates {borrows:?}");*/
                    }
                    for borrow in borrows {
                        /*#[cfg(not(test))]
                        println!("Invalidated flatten 3 {:?}", borrow);*/
                        self.invalidations.insert(borrow.into());
                    }
                }
                Err(_contra) => {
                    /*#[cfg(not(test))]
                    println!("Invalidated there");
                    #[cfg(not(test))]
                    println!("Invalidated flatten 4 {:?}", borrow);*/
                    self.invalidations.insert(borrow.into());
                }
            }
        }

        // Register on the borrower side
        match Loc::try_from(place) {
            Ok(loc) => {
                if borrows.is_empty() {
                    self.borrows.remove(loc);
                } else {
                    /*#[cfg(not(test))]
                    println!("Registering borrows");*/
                    *self.borrows.get_mut_or_insert(loc) = Borrows::Distinct(borrows);
                    /*let borrows: Vec<Borrow> = self.borrows.get_all(loc).collect();
                    #[cfg(not(test))]
                    println!("Registered borrows {borrows:?}");*/
                }
            }
            Err(()) => {
                if !borrows.is_empty() {
                    // If you assign something to an element of an array, we need to ADD the borrows
                    // to the old ones, since we have no way to know which (if any) borrows are
                    // invalidated by that assignment.
                    let entry = self.borrows.get_mut_or_insert(loc);
                    match entry {
                        Borrows::None => *entry = Borrows::Distinct(borrows),
                        Borrows::Aliased(_) => unreachable!(),
                        Borrows::Distinct(set) => set.extend(borrows),
                    }
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

    /// Returns the list of invalidations.
    pub fn invalidations<'a>(&'a self) -> impl Iterator<Item = Loan<'ctx>> + 'a {
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
        f.debug_struct("Ledger")
            .field("borrows", &self.borrows)
            .field("invalidations", &self.invalidations)
            .field("unrecoverables", &self.unrecoverables)
            .finish()
    }
}

impl<'ctx> Sum for Ledger<'ctx> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut loans: LocTree<Loans> = default();
        let mut borrows: LocTree<Borrows> = default();
        let mut invalidations: LoanCnt = default();
        let mut unrecoverables: UnrecoverableBorrows<'ctx> = default();

        for ledger in iter {
            invalidations.extend(loans.append(ledger.loans).into_iter().map(Loan::from));
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
