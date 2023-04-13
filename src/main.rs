//! Toy Stratum language compiler.

#![feature(box_patterns)]
#![feature(extend_one)]
#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

mod ast;
mod compile;
mod examples;
mod typing;

#[macro_use]
extern crate quote;

use crate::typing::Typer;

fn main() {
    let p1 = examples::simple();
    let mut typer = Typer::new();
    typer.check(&p1);
}
