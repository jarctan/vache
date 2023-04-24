//! Toy Stratum language compiler library.

#![feature(box_patterns)]
#![feature(extend_one)]
#![feature(let_chains)]
#![feature(arbitrary_self_types)]
#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

use ast::Program;
use compile::Compiler;
use typing::Typer;

pub mod ast;
mod compile;
#[cfg(test)]
mod tests;
mod typing;

#[macro_use]
extern crate quote;

/// Checks a given program.
///
/// If it returns successfully, the program type-checked. Otherwise, it will panic (note: temporary behavior,
/// there should of course be no panicking in the future, only `Result`s).
///
/// Under the hood, this function is in charge of allocating a new `Typer` and launching it on your program.
///
/// Note: applies all transformers on the parser AST on your behalf if you check the `reduce` option.
pub fn check(p: Program, _reduce: bool) {
    let mut typer = Typer::new();
    typer.check(&p);
}

/// Compiles a given program.
///
/// Under the hood, in charge of allocating a new `Compiler` and launching it on your program.
///
/// Note: applies all transformers on the parser AST on your behalf if you check the `reduce` option.
pub fn compile(p: Program, _reduce: bool) -> String {
    let mut compiler = Compiler::new();
    compiler.compile(p)
}
