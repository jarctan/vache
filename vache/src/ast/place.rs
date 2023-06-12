//! Defining places: left hand side expressions.

use pest::iterators::Pair;

use super::{Context, Parsable};
use super::{Expr, ExprKind, VarUse};
use crate::grammar::*;
use crate::utils::boxed;

/// A place in the AST: allowed left hand side expressions.
#[derive(Debug, Clone)]
pub enum Place<'ctx> {
    /// A mere variable.
    VarP(VarUse<'ctx>),
    /// An index in an array/a map.
    IndexP(Box<Expr<'ctx>>, Box<Expr<'ctx>>),
    /// An field in a structure.
    FieldP(Box<Expr<'ctx>>, &'ctx str),
}

use ExprKind::*;
use Place::*;

/// Shortcut to create an indexed variable.
pub fn idx_place<'ctx>(array: impl Into<Expr<'ctx>>, index: impl Into<Expr<'ctx>>) -> Place<'ctx> {
    IndexP(boxed(array.into()), boxed(index.into()))
}

impl<'ctx> From<&'ctx str> for Place<'ctx> {
    fn from(v: &'ctx str) -> Self {
        VarP(VarUse::from(v))
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Place<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &mut Context<'ctx>) -> Self {
        assert_eq!(pair.as_rule(), Rule::expr);

        let expr: Expr = ctx.parse(pair);
        match expr.kind {
            PlaceE(place) => place,
            _ => panic!("Parser internal error: expected place, found {:?}", expr),
        }
    }
}

impl<'ctx> PartialEq<str> for Place<'ctx> {
    fn eq(&self, other: &str) -> bool {
        match self {
            VarP(v) => v == other,
            _ => false,
        }
    }
}

impl<'ctx> PartialEq<&str> for Place<'ctx> {
    fn eq(&self, other: &&str) -> bool {
        match self {
            VarP(v) => v == other,
            _ => false,
        }
    }
}
