//! Borrow-checking the program.
//!
//! As a end user, you will mainly want to use the `BorrowChecker` on your
//! program.

mod borrow;
mod checker;
mod flow;
mod fun_flow;
mod ledger;
mod liveness;
mod tree;

use borrow::{Borrow, BorrowCnt, Borrows, InvalidationReason, Invalidations, Loan};
pub use checker::BorrowChecker;
use fun_flow::FunFlow;
use liveness::liveness;
use tree::LocTree;
