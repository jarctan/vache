use crate::ast::block::*;
use crate::ast::expr::*;
use crate::ast::ty::*;
use crate::ast::var::*;
use crate::ast::*;
use crate::typing::Typer;
use Expr::*;
use Stmt::*;
use Ty::*;

mod basic_lifetime;
mod ceil_mod2;
mod fibo;
mod scopes;
mod simple;

/// Checks a given program.
///
/// You must provide a closure/function that takes the id of the static stratum
/// and returns a program.
///
/// Under the hood, in charge of allocating a new `Typer` and launching it on your program.
fn check(mut p: impl FnMut(Stratum) -> Program) {
    let mut typer = Typer::new();
    typer.check(&p(typer.static_stratum()));
}
