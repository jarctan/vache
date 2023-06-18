//! Parsing blocks, and defining their representation in the AST.

use std::default::default;

use double_ended_peekable::DoubleEndedPeekableExt;
use pest::iterators::Pair;
use ExprKind::*;

use super::{Context, Expr, ExprKind, Parsable, Span, Stmt};
use crate::grammar::*;

/// A block in the parser AST.
///
/// A block is a list of ordered statements, followed by a final expression.
#[derive(Debug, Clone, Default)]
pub struct Block<'ctx> {
    /// List of consecutive statements.
    pub stmts: Vec<Stmt<'ctx>>,
    /// Final return expression.
    pub ret: Expr<'ctx>,
    /// Codespan.
    pub span: Span,
}

/// Creates a block only made of an expression.
pub fn expr<'ctx>(expr: impl Into<Expr<'ctx>>) -> Block<'ctx> {
    let expr = expr.into();
    Block {
        stmts: vec![],
        span: expr.span,
        ret: expr,
    }
}

/// Creates a block only made of a list of statements, with no
/// terminating expression.
///
/// The argument to this function must be a closure that takes a fresh
/// stratum for that block as an argument, and returns a list of statements.
///
/// The final expression is then chosen to be the unit, no-op expr.
pub fn stmts<'ctx>(stmts: impl IntoIterator<Item = Stmt<'ctx>>) -> Block<'ctx> {
    let stmts = stmts.into_iter().collect();
    Block {
        ret: UnitE.into(),
        span: default(),
        stmts,
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Block<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        debug_assert!(matches!(pair.as_rule(), Rule::block));
        let span = Span::from(pair.as_span());

        let mut pairs = pair.into_inner().double_ended_peekable();
        consume!(pairs, Rule::lcb);
        consume_back!(pairs, Rule::rcb);
        let ret = if matches!(pairs.peek_back().map(Pair::as_rule), Some(Rule::expr)) {
            Some(ctx.parse(consume_back!(pairs, Rule::expr)))
        } else {
            None
        };

        let stmts: Vec<Stmt> = pairs.map(|pair| ctx.parse(pair)).collect();

        // Generating dummy ret expr with the best span for it
        let ret = ret.unwrap_or_else(|| {
            if let Some(stmt) = stmts.last() {
                Expr {
                    span: Span::at(stmt.span.end()),
                    ..default()
                }
            } else {
                Expr {
                    span: Span::at(span.end()),
                    ..default()
                }
            }
        });

        Block { stmts, ret, span }
    }
}

#[cfg(test)]
mod tests {
    use StmtKind::*;

    use super::super::StmtKind;
    use super::*;

    #[parses("{ var x: int = 5; var y: int = 7; x }" as block)]
    #[test]
    fn block(block: Block) {
        assert_eq!(block.stmts.len(), 2);
        assert!(matches!(block.stmts[0].kind, DeclareS(..)));
        assert!(matches!(block.stmts[1].kind, DeclareS(..)));
        assert_eq!(block.ret.as_var().unwrap(), "x");
    }
}
