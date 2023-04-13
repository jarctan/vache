//! Toy Stratum language compiler.

#![feature(box_patterns)]
#![feature(extend_one)]
#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

mod ast;
mod compile;
mod typing;
#[cfg(test)]
mod tests;

#[macro_use]
extern crate quote;

fn main() {
    println!("Hello, world!");
}
