//! Toy Stratum language compiler library.

#![feature(box_patterns)]
#![feature(extend_one)]
#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

use ast::{Program, Stratum};
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
/// You must provide a closure/function that takes the id of the static stratum
/// and returns a program.
///
/// Under the hood, in charge of allocating a new `Typer` and launching it on your program.
pub fn check(mut p: impl FnMut(Stratum) -> Program) {
    let mut typer = Typer::new();
    typer.check(&p(typer.static_stratum()));
}

/// Compiles a given program.
///
/// Under the hood, in charge of allocating a new `Compiler` and launching it on your program.
pub fn compile(p: Program) -> String {
    let mut compiler = Compiler::new();
    compiler.compile(p)
}
