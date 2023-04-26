//! Toy Stratum language compiler library.

#![feature(box_patterns)]
#![feature(extend_one)]
#![feature(let_chains)]
#![feature(arbitrary_self_types)]
#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

use compile::Compiler;
use typing::Typer;

pub mod ast;
mod compile;
pub mod examples;
mod interp;
mod tast;
#[cfg(test)]
mod tests;
mod typing;
mod utils;

#[macro_use]
extern crate quote;

/// Checks a given program.
///
/// If it returns successfully, the program type-checked. Otherwise, it will panic (note: temporary behavior,
/// there should of course be no panicking in the future, only `Result`s).
///
/// Under the hood, this function is in charge of allocating a new `Typer` and launching it on your program.
pub fn check(p: impl Into<ast::Program>) -> tast::Program {
    let mut typer = Typer::new();
    typer.check(p.into())
}

/// Compiles a given program.
///
/// Under the hood, in charge of allocating a new `Compiler` and launching it on your program.
pub fn compile(p: impl Into<tast::Program>) -> String {
    let mut compiler = Compiler::new();
    compiler.compile(p.into())
}

/// Interprets a given program.
///
/// Under the hood, it will allocate a new `Interpreter` and launch it on your program.
/// It will call the function `main` within your program and return the standard output
/// of your program.
pub fn interp(p: impl Into<tast::Program>) -> String {
    interp::interpret(p.into())
}
