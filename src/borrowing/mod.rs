//! Borrow-checking the program.
//!
//! As a end user, you will mainly want to use the `BorrowChecker` on your
//! program.

mod analysis;
mod borrow;
mod checker;
mod liveness;

use analysis::Analysis;
pub use checker::BorrowChecker;
use liveness::liveness;
