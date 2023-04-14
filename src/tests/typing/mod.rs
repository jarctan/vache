use crate::ast::block::*;
use crate::ast::expr::*;
use crate::ast::var::*;
use crate::ast::*;
use crate::typing::Typer;
use Expr::*;
use Stmt::*;
use Ty::*;

mod basic_lifetime;
mod ceil_mod2;
mod fibo;
mod simple;

/// Checks a given program.
///
/// In charge of allocating a new `Typer` and launching it on your program.
fn check(p: Program) {
    let mut typer = Typer::new();
    typer.check(&p);
}
