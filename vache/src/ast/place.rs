//! Defining places: left hand side expressions.

use pest::iterators::Pair;

use super::{Context, Parsable};
use super::{Expr, Var};
use crate::grammar::*;
use crate::utils::boxed;

/// A place in the AST: allowed left hand side expressions.
#[derive(Debug, Clone)]
pub enum Place<'ctx> {
    /// A mere variable.
    VarP(Var<'ctx>),
    /// An indexed slot into an expression.
    IndexP(Box<Expr<'ctx>>, Box<Expr<'ctx>>),
    /// An field in an expression.
    FieldP(Box<Expr<'ctx>>, Box<Expr<'ctx>>),
}

use Place::*;

/// Shortcut to create an indexed variable.
pub fn idx_place<'ctx>(array: impl Into<Expr<'ctx>>, index: impl Into<Expr<'ctx>>) -> Place<'ctx> {
    Place::IndexP(boxed(array.into()), boxed(index.into()))
}

impl<'ctx> From<&'ctx str> for Place<'ctx> {
    fn from(v: &'ctx str) -> Self {
        Place::VarP(Var::from(v))
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Place<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &mut Context<'ctx>) -> Self {
        match pair.as_rule() {
            Rule::ident => VarP(ctx.parse(pair)),
            rule => panic!("parser internal error: expected place, found {rule:?}"),
        }
    }
}

impl<'ctx> PartialEq<str> for Place<'ctx> {
    fn eq(&self, other: &str) -> bool {
        match self {
            Place::VarP(v) => v == other,
            _ => false,
        }
    }
}

impl<'ctx> PartialEq<&str> for Place<'ctx> {
    fn eq(&self, other: &&str) -> bool {
        match self {
            Place::VarP(v) => v == other,
            _ => false,
        }
    }
}
