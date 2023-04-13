//! Toy Stratum language compiler.

#![feature(box_patterns)]
#![feature(extend_one)]
#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

mod ast;
mod compile;
mod examples;

#[macro_use]
extern crate quote;

use compile::Compiler;

fn main() {
    let p1 = examples::simple();
    let p2 = examples::fibo_fibo();
    let p3 = examples::th_fn();

    let mut compiler = Compiler::new();
    println!("{}", compiler.compile(p1));
    println!("{}", compiler.compile(p2));
    println!("{}", compiler.compile(p3));
}
