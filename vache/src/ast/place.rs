//! Defining places: left hand side expressions.

use pest::iterators::Pair;

use super::{Context, Parsable};
use super::{Expr, Var};
use crate::grammar::*;
use crate::utils::boxed;

/// A place in the AST: allowed left hand side expressions.
#[derive(Debug, Clone)]
pub enum Place {
    /// A mere variable.
    VarP(Var),
    /// An indexed slot into an expression.
    IndexP(Box<Expr>, Box<Expr>),
    /// An field in an expression.
    FieldP(Box<Expr>, Box<Expr>),
}

use Place::*;

/// Shortcut to create an indexed variable.
pub fn idx_place(array: impl Into<Expr>, index: impl Into<Expr>) -> Place {
    Place::IndexP(boxed(array.into()), boxed(index.into()))
}

impl From<&str> for Place {
    fn from(v: &str) -> Self {
        Place::VarP(Var::from(v))
    }
}

impl From<String> for Place {
    fn from(v: String) -> Self {
        Place::VarP(Var::from(v))
    }
}

impl Parsable<Pair<'_, Rule>> for Place {
    fn parse(pair: Pair<Rule>, ctx: &mut Context) -> Self {
        match pair.as_rule() {
            Rule::ident => VarP(ctx.parse(pair)),
            rule => panic!("parser internal error: expected place, found {rule:?}"),
        }
    }
}

impl PartialEq<str> for Place {
    fn eq(&self, other: &str) -> bool {
        match self {
            Place::VarP(v) => v == other,
            _ => false,
        }
    }
}

impl PartialEq<&str> for Place {
    fn eq(&self, other: &&str) -> bool {
        match self {
            Place::VarP(v) => v == other,
            _ => false,
        }
    }
}
