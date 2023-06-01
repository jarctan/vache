//! Parsing blocks, and defining their representation in the AST.

use pest::iterators::Pair;

use super::{Context, Expr, Parsable, Stmt};
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
}

/// Creates a block only made of an expression.
pub fn expr(expr: Expr<'_>) -> Block<'_> {
    Block {
        stmts: vec![],
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
    Block {
        stmts: stmts.into_iter().collect(),
        ret: Expr::UnitE,
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Block<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &mut Context<'ctx>) -> Self {
        debug_assert!(matches!(pair.as_rule(), Rule::block | Rule::primitive));
        let mut stmts = vec![];
        let mut ret = None;
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::stmt => stmts.push(ctx.parse(pair)),
                Rule::expr => {
                    debug_assert!(ret.is_none());
                    ret = Some(ctx.parse(pair));
                }
                rule => panic!("parser internal error: unexpected rule {:?}", rule),
            }
        }
        Block {
            stmts,
            ret: ret.unwrap_or_default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[parses("{ let x: int = 5; let y: int = 7; x }" as block)]
    #[test]
    fn block(block: Block) {
        assert!(matches!(
            &block.stmts[..],
            [Stmt::Declare(..), Stmt::Declare(..)]
        ));
        assert_eq!(block.ret.as_var().unwrap(), "x");
    }
}
