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

pub use anyhow::Context as AnyhowContext;
pub use anyhow::Result;
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

use crate::context::Context;
use crate::grammar::{Grammar, Rule};

/// Can be parsed from elements of `T`.
pub trait Parsable<'ctx, T> {
    /// Parses `tokens` into `Self` using `ctx`.
    fn parse(tokens: T, ctx: &mut Context<'ctx>) -> Self;
}

impl<'ctx> Context<'ctx> {
    /// Parses `tokens`.
    #[must_use]
    pub fn parse<U, T: Parsable<'ctx, U>>(&mut self, pair: U) -> T {
        T::parse(pair, self)
    }
}

/// Parses a file, and returns the parsed program, and the input.
pub fn parse_file<'ctx>(ctx: &mut Context<'ctx>) -> Result<Program<'ctx>> {
    let mut pairs =
        Grammar::parse(Rule::program, ctx.config.input).context("Parsing errors found.")?;

    let program: Program = ctx.parse(pairs.next().context("Parser grammar error")?);

    Ok(program)
}
