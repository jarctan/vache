//! Borrow-checking the program.
//!
//! As a end user, you will mainly want to use the `BorrowChecker` on your
//! program.

mod borrow;
mod checker;
mod flow;
mod fun_flow;
mod invalidation;
mod ledger;
mod liveness;
mod tree;

use borrow::{Borrow, BorrowCnt, Borrows, Loan};
pub use checker::BorrowChecker;
use flow::Flow;
use fun_flow::FunFlow;
use invalidation::{InvalidationReason, Invalidations};
use ledger::Ledger;
use liveness::liveness;
use tree::LocTree;
