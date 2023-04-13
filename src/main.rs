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
    let p2 = examples::fibo_fibo();
    let p3 = examples::ceil_mod_2();
    let mut typer = Typer::new();
    typer.check(&p1);
    typer.check(&p2);
    typer.check(&p3);
}
