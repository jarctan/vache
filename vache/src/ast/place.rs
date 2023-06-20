//! Defining places: left hand side expressions.

use std::default::default;

use pest::iterators::Pair;

use super::{Context, Parsable};
use super::{Expr, ExprKind, Mode, Span, VarUse};
use crate::grammar::*;
use crate::utils::boxed;

/// A place in the AST: allowed left hand side expressions.
#[derive(Debug, Clone)]
pub struct Place<'ctx> {
    /// Place kind.
    pub kind: PlaceKind<'ctx>,
    /// Codespan.
    pub span: Span,
    /// Place mode.
    pub mode: Mode,
}

impl<'ctx> Place<'ctx> {
    /// Creates a new place.
    pub fn new(kind: impl Into<PlaceKind<'ctx>>, span: impl Into<Span>, mode: Mode) -> Self {
        Self {
            kind: kind.into(),
            span: span.into(),
            mode,
        }
    }
}

impl<'ctx> From<PlaceKind<'ctx>> for Place<'ctx> {
    fn from(kind: PlaceKind<'ctx>) -> Self {
        Self {
            kind,
            span: default(),
            mode: default(),
        }
    }
}

/// Kind of place.
#[derive(Debug, Clone)]
pub enum PlaceKind<'ctx> {
    /// A mere variable.
    VarP(VarUse<'ctx>),
    /// An index in an array/a map.
    IndexP(Box<Expr<'ctx>>, Box<Expr<'ctx>>),
    /// An field in a structure.
    FieldP(Box<Expr<'ctx>>, &'ctx str),
    /// An element in a tuple.
    ElemP(Box<Expr<'ctx>>, usize),
}

use ExprKind::*;
use PlaceKind::*;

/// Shortcut to create an indexed variable.
pub fn idx_place<'ctx>(array: impl Into<Expr<'ctx>>, index: impl Into<Expr<'ctx>>) -> Place<'ctx> {
    IndexP(boxed(array.into()), boxed(index.into())).into()
}

impl<'ctx> From<&'ctx str> for Place<'ctx> {
    fn from(v: &'ctx str) -> Self {
        VarP(VarUse::from(v)).into()
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Place<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
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
        match self.kind {
            VarP(v) => v == other,
            _ => false,
        }
    }
}

impl<'ctx> PartialEq<&str> for Place<'ctx> {
    fn eq(&self, other: &&str) -> bool {
        match &self.kind {
            VarP(v) => v == other,
            _ => false,
        }
    }
}
