use crate::ast::block::*;
use crate::ast::expr::*;
use crate::ast::ty::*;
use crate::ast::var::*;
use crate::ast::*;
use Expr::*;
use Stmt::*;
use Ty::*;

mod basic_lifetime;
mod ceil_mod2;
mod fibo;
mod scopes;
mod simple;

use crate::check;
