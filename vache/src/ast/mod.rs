//! Parser Abstract Syntax Tree for the language.
//!
//! Each node in the tree = one file.

pub mod block;
pub mod expr;
pub mod fun;
pub mod place;
pub mod primitives;
pub mod program;
pub mod selfvisitor;
pub mod stmt;
pub mod structure;
pub mod ty;
pub mod var;

use std::fs;

pub use block::Block;
pub use expr::{if_e, Expr};
pub use fun::{Fun, FunSig};
use pest::Parser;
pub use place::{idx_place, Place};
pub use program::Program;
pub use selfvisitor::SelfVisitor;
pub use stmt::Stmt;
pub use structure::Struct;
pub use ty::Ty;
pub use var::{Var, VarDef};

use crate::grammar::{Grammar, Rule};

/// Can be parsed from elements of `T`.
pub trait Parsable<T> {
    /// Parses `tokens` into `Self` using `ctx`.
    fn parse(tokens: T, ctx: &mut Context) -> Self;
}

/// Parser context.
pub struct Context<'a> {
    /// Original input.
    input: &'a str,
}

impl<'a> Context<'a> {
    /// Creates a new parser context.
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }

    /// Parses `tokens`.
    #[must_use]
    pub fn parse<U, T: Parsable<U>>(&mut self, pair: U) -> T {
        T::parse(pair, self)
    }
}

/// Parses a file, and returns the parsed program, and the input.
pub fn parse_file(filepath: &str) -> Option<(Program, String)> {
    let input = fs::read_to_string(filepath).ok()?;

    let mut pairs = match Grammar::parse(Rule::program, &input) {
        Err(err) => {
            println!("Parsing errors :\n{}", err);
            return None;
        }
        Ok(pairs) => pairs,
    };

    let mut ctx = Context::new(&input);
    let program = ctx.parse(pairs.next()?);

    Some((program, input.clone()))
}
