//! Typing tests.

#![feature(default_free_fn)]
#![feature(path_file_prefix)]

#[macro_use]
extern crate vache_tests_proc;
#[macro_use]
extern crate anyhow;

pub(crate) mod exec;
pub(crate) mod typing;

pub use std::default::default;

pub use vache_lib::ast::block::*;
pub use vache_lib::ast::expr::*;
pub use vache_lib::ast::stmt::*;
pub use vache_lib::ast::ty::*;
pub use vache_lib::ast::var::*;
pub use vache_lib::ast::*;
use vache_lib::examples;
pub use ExprKind::*;
pub use StmtKind::*;
pub use Ty::*;

/// Alias for `Box::new()` to make it shorter and easier
/// to use in manually-created ASTs.
pub fn boxed<T>(t: T) -> Box<T> {
    Box::new(t)
}
