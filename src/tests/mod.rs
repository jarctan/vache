//! Typing tests.

pub(crate) mod interp;
pub(crate) mod typing;

pub use crate::ast::block::*;
pub use crate::ast::expr::*;
use crate::ast::stmt::*;
pub use crate::ast::ty::*;
pub use crate::ast::var::*;
pub use crate::ast::*;
pub use Expr::*;
pub use Stmt::*;
pub use Ty::*;
