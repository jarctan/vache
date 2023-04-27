//! List of examples.

mod custom_addition;
mod is_even;
mod multiple_refs;
mod out_of_scope;

pub use custom_addition::*;
pub use is_even::*;
pub use multiple_refs::*;
pub use out_of_scope::*;
pub use Expr::*;
pub use Stmt::*;
pub use Ty::*;

pub use crate::ast::block::*;
pub use crate::ast::expr::*;
pub use crate::ast::stmt::*;
pub use crate::ast::ty::*;
pub use crate::ast::var::*;
pub use crate::ast::*;
