//! Borrow-checking the program.
//!
//! As a end user, you will mainly want to use the `BorrowChecker` on your
//! program.

mod borrow;
mod checker;
mod flow;
mod ledger;
mod liveness;
mod loans;
mod tree;

use borrow::{Borrow, Borrows};
pub use checker::BorrowChecker;
use liveness::liveness;
use loans::Loans;
use tree::LocTree;
