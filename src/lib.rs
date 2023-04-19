//! Toy Stratum language compiler library.

#![feature(box_patterns)]
#![feature(extend_one)]
#![feature(let_chains)]
#![feature(arbitrary_self_types)]
#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

use ast::{Program, Stratum};
use compile::Compiler;
use transformers::apply_all;
use typing::Typer;

pub mod ast;
mod compile;
#[cfg(test)]
mod tests;
mod transformers;
mod typing;

#[macro_use]
extern crate quote;

/// Checks a given program.
///
/// If it returns successfully, the program type-checked. Otherwise, it will panic (note: temporary behavior,
/// there should of course be no panicking in the future, only `Result`s).
///
/// You must provide a closure/function that takes the id of the static stratum
/// and returns a program.
///
/// Under the hood, in charge of allocating a new `Typer` and launching it on your program.
///
/// Note: applies all transformers on the parser AST on your behalf if you check the `reduce` option.
pub fn check(mut p: impl FnMut(Stratum) -> Program, reduce: bool) {
    let mut typer = Typer::new();
    let p = p(typer.static_stratum());
    let p = if reduce { apply_all(p) } else { p };
    typer.check(&p);
}

/// Compiles a given program.
///
/// Under the hood, in charge of allocating a new `Compiler` and launching it on your program.
///
/// Note: applies all transformers on the parser AST on your behalf if you check the `reduce` option.
pub fn compile(p: Program, reduce: bool) -> String {
    let mut compiler = Compiler::new();
    let p = if reduce { apply_all(p) } else { p };
    compiler.compile(p)
}
