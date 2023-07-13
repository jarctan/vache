//! Borrow-checking the program.
//!
//! As a end user, you will mainly want to use the `BorrowChecker` on your
//! program.

mod borrow;
mod checker;
mod flow;
mod ledger;
mod liveness;
mod tree;

use borrow::{Borrow, BorrowCnt, Borrows, Loan, LoanCnt};
pub use checker::BorrowChecker;
use liveness::liveness;
use tree::LocTree;
