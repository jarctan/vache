//! Typing tests.

pub(crate) mod interpret;
pub(crate) mod typing;

pub use Expr::*;
pub use Stmt::*;
pub use Ty::*;

pub use crate::ast::block::*;
pub use crate::ast::expr::*;
pub use crate::ast::stmt::*;
pub use crate::ast::ty::*;
pub use crate::ast::var::*;
pub use crate::ast::*;
use crate::examples;
pub use crate::utils::boxed;
