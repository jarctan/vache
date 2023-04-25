pub use crate::ast::block::*;
pub use crate::ast::expr::*;
pub use crate::ast::ty::*;
pub use crate::ast::var::*;
pub use crate::ast::*;
pub use Expr::*;
pub use Stmt::*;
pub use Ty::*;

mod basic_lifetime;
mod ceil_mod2;
mod fibo;
mod scopes;
mod simple;

use crate::{check, compile};
