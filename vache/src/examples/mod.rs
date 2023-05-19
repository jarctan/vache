//! Examples.

mod arrays;
mod borrows;
mod change_field;
mod custom_addition;
mod id_fn;
mod is_even;
mod matrices;
mod multiple_refs;
mod out_of_scope;
mod structure;
mod while_loop;

pub use arrays::*;
pub use borrows::*;
pub use change_field::*;
pub use custom_addition::*;
pub use id_fn::*;
pub use is_even::*;
pub use matrices::*;
pub use multiple_refs::*;
pub use out_of_scope::*;
pub use structure::*;
pub use while_loop::*;
pub use Expr::*;
pub use Stmt::*;
pub use Ty::*;

pub use crate::ast::block::*;
pub use crate::ast::expr::*;
pub use crate::ast::stmt::*;
pub use crate::ast::ty::*;
pub use crate::ast::var::*;
pub use crate::ast::*;
pub use crate::utils::boxed;
