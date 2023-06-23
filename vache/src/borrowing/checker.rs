//! Taking a program and computing the borrow-checking analysis on it.
//!
//! To use it:
//! * Instantiate `BorrowChecker::new()`
//! * then `borrow_checker.check(&your_program)`

use super::liveness;
use crate::mir::{Fun, Program};

/// The borrow-checker.
pub struct BorrowChecker {}

impl BorrowChecker {
    /// Creates a new borrow-checker.
    pub fn new() -> Self {
        Self {}
    }

    /// Borrow-checks a given program.
    pub fn check<'a>(&mut self, p: Program<'a>) -> Program<'a> {
        self.visit_program(p)
    }

    /// Borrow-checks a function.
    fn visit_fun<'a>(&mut self, mut f: Fun<'a>) -> Fun<'a> {
        f.body = liveness(f.body, f.entry_l, f.ret_l, &f.strata);
        f
    }

    /// Borrow-checks a program.
    fn visit_program<'a>(&mut self, mut p: Program<'a>) -> Program<'a> {
        p.funs = p
            .funs
            .into_iter()
            .map(|(name, f)| (name, self.visit_fun(f)))
            .collect();
        p
    }
}
