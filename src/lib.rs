//! Toy Stratum language compiler library.

#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(trait_alias)]
#![feature(array_windows)]
#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

use borrowing::BorrowChecker;
use compile::Compiler;
use miring::MIRer;
use typing::Typer;

pub mod ast;
mod borrowing;
mod compile;
pub mod examples;
mod interpret;
mod mir;
mod miring;
mod tast;
#[cfg(test)]
mod tests;
mod typing;
mod utils;

#[macro_use]
extern crate quote;

/// Checks a given program.
///
/// If it returns successfully, the program type-checked. Otherwise, it will
/// panic (note: temporary behavior, there should of course be no panicking in
/// the future, only `Result`s).
///
/// Under the hood, this function is in charge of allocating a new `Typer` and
/// launching it on your program.
pub fn check(p: impl Into<ast::Program>) -> tast::Program {
    let mut typer = Typer::new();
    typer.check(p.into())
}

/// Borrow-checks a given program.
///
/// If it returns successfully, the program borrow-checked. Otherwise, it will
/// panic (note: temporary behavior, there should of course be no panicking in
/// the future, only `Result`s).
///
/// Under the hood, this function is in charge of allocating a new
/// `BorrowChecker` and launching it on your program.
pub fn borrow_check(p: impl Into<mir::Program>) -> mir::Program {
    let mut borrow_checker = BorrowChecker::new();
    borrow_checker.check(p.into())
}

/// Computes the MIR output of a given program.
///
/// Under the hood, in charge of allocating a new `MIRer` and launching it on
/// your program.
pub fn mir(p: impl Into<tast::Program>) -> mir::Program {
    let mut mirer = MIRer::new();
    mirer.gen_mir(p.into())
}

/// Compiles a given program.
///
/// Under the hood, in charge of allocating a new `Compiler` and launching it on
/// your program.
pub fn compile(p: impl Into<mir::Program>) -> String {
    let mut compiler = Compiler::new();
    compiler.compile(p.into())
}

/// Interprets a given program.
///
/// Under the hood, it will allocate a new `Interpreter` and launch it on your
/// program. It will call the function `main` within your program and return the
/// standard output of your program.
pub fn interp(p: impl Into<mir::Program>) -> String {
    interpret::interpret(p.into())
}
