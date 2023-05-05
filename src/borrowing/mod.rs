//! Borrow-checking the program.
//!
//! As a end user, you will mainly want to use the `BorrowChecker` on your
//! program.

mod borrow;
mod checker;
mod flow;
mod ledger;
mod liveness;

pub use checker::BorrowChecker;
use flow::{Flow, Flowable};
use liveness::var_liveness;
