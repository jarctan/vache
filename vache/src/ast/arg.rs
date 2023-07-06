//! Function arguments are special kinds of expressions.

use pest::iterators::Pair;

use super::{Context, Expr, Parsable, Place, Span};
use crate::grammar::*;

/// Function call argument.
#[derive(Debug, Clone)]
pub struct Arg<'ctx> {
    /// Kind of argument.
    pub kind: ArgKind<'ctx>,
    /// Codespan.
    pub span: Span,
}

/// Kind of function call argument.
#[derive(Debug, Clone)]
pub enum ArgKind<'ctx> {
    /// Default/standard kind of argument.
    ///
    /// Notation: no extra notation.
    Standard(Expr<'ctx>),
    /// Argument mutated in place.
    ///
    /// Notation: `@place` in the code.
    InPlace(Place<'ctx>),
    /// Argument binding to a new place.
    ///
    /// Notation: `expr@place` in the code.
    Binding(Expr<'ctx>, Place<'ctx>),
}

impl<'ctx> Arg<'ctx> {
    /// Is this argument a standard argument? If so, return the expression
    /// behind it.
    pub fn as_standard(&self) -> Option<&Expr<'ctx>> {
        match &self.kind {
            ArgKind::Standard(e) => Some(e),
            _ => None,
        }
    }

    /// Is this argument a standard argument? If so, return the expression
    /// behind it.
    ///
    /// If it fails, returns the original argument.
    pub(crate) fn into_standard(self) -> Result<Expr<'ctx>, Self> {
        match self.kind {
            ArgKind::Standard(e) => Ok(e),
            _ => Err(self),
        }
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Arg<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        assert!(matches!(pair.as_rule(), Rule::arg));
        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner();
        let pair = consume!(pairs);
        let kind = match pair.as_rule() {
            Rule::std_arg => {
                let mut pairs = pair.into_inner();
                let e = ctx.parse(consume!(pairs));
                ArgKind::Standard(e)
            }
            Rule::in_place_arg => {
                let mut pairs = pair.into_inner();
                consume!(pairs, Rule::as_mut);
                let p = ctx.parse(consume!(pairs));
                ArgKind::InPlace(p)
            }
            rule => panic!("parser internal error: expected an argument, found {rule:?}"),
        };
        Arg { kind, span }
    }
}

impl<'ctx> From<Expr<'ctx>> for Arg<'ctx> {
    fn from(e: Expr<'ctx>) -> Self {
        Arg {
            span: e.span,
            kind: ArgKind::Standard(e),
        }
    }
}
