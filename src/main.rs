//! Toy Stratum language compiler.

#![feature(box_patterns)]
#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

mod ast;
mod compile;
mod examples;

use compile::{Compilable, Compiler};

fn main() {
    let p1 = examples::fibo_fibo();
    let p2 = examples::th_fn();

    let mut compiler = Compiler::new();

    println!("Hello, world!");
}
