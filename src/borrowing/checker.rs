//! Taking a program and computing the borrow-checking analysis on it.
//!
//! To use it:
//! * Instantiate `BorrowChecker::new()`
//! * then `borrow_checker.check(&your_program)`

use std::collections::HashMap;

use super::{loan_liveness, var_liveness};
use crate::mir::{CfgLabel, Fun, Program, Var};
use crate::utils::set::Set;

/// The borrow-checker.
pub struct BorrowChecker {}

impl BorrowChecker {
    /// Creates a new borrow-checker.
    pub fn new() -> Self {
        Self {}
    }

    /// Borrow-checks a given program.
    pub fn check(&mut self, p: &Program) {
        self.visit_program(p);
    }

    /// Borrow-checks a function.
    fn visit_fun(&mut self, f: &Fun) {
        let liveliness = var_liveness(&f.body, &f.ret_l);
        println!("Var liveliness: {:?}", liveliness);
        let out_of_scope: HashMap<CfgLabel, Set<Var>> = liveliness
            .into_iter()
            .map(|(l, flow)| (l, flow.ins - &flow.outs))
            .collect();
        println!(
            "Loan liveliness: {:?}",
            loan_liveness(&f.body, &f.ret_l, &out_of_scope)
        );
    }

    /// Borrow-checks a program.
    fn visit_program(&mut self, p: &Program) {
        for f in p.funs.values() {
            self.visit_fun(f);
        }
    }
}
