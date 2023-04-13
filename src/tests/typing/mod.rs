use crate::ast::*;
use Ty::*;
use Stmt::*;
use Expr::*;
use crate::ast::block::*;
use crate::ast::expr::*;
use crate::ast::var::*;
use crate::typing::Typer;

mod simple;
mod fibo;
mod ceil_mod2;
mod basic_lifetime;

/// Checks a given program.
/// 
/// In charge of allocating a new `Typer` and launching it on your program.
fn check(p: Program) {
    let mut typer = Typer::new();
    typer.check(&p);
}