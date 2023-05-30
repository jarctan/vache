//! Parsing blocks, and defining their representation in the AST.

use pest::iterators::Pair;

use super::{Context, Expr, Parsable, Stmt};
use crate::grammar::*;

/// A block in the parser AST.
///
/// A block is a list of ordered statements, followed by a final expression.
#[derive(Debug, Clone, Default)]
pub struct Block {
    /// List of consecutive statements.
    pub stmts: Vec<Stmt>,
    /// Final return expression.
    pub ret: Expr,
}

/// Creates a block only made of an expression.
pub fn expr(expr: Expr) -> Block {
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
pub fn stmts(stmts: impl IntoIterator<Item = Stmt>) -> Block {
    Block {
        stmts: stmts.into_iter().collect(),
        ret: Expr::UnitE,
    }
}

impl PartialEq for Block {
    fn eq(&self, _other: &Self) -> bool {
        todo!()
    }
}

impl Eq for Block {}

impl Parsable<Pair<'_, Rule>> for Block {
    fn parse(pair: Pair<Rule>, ctx: &mut Context) -> Self {
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
    use pest::Parser;

    use super::*;
    use crate::grammar::Grammar;

    #[test]
    fn block() {
        let input = "{ let x: int = 5; let y: int = 7; x }";
        let mut parsed = Grammar::parse(Rule::block, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let block: Block = ctx.parse(pair);
        eprintln!("{block:?}");
        assert!(matches!(
            &block.stmts[..],
            [Stmt::Declare(..), Stmt::Declare(..)]
        ));
        assert_eq!(block.ret.as_var().unwrap(), "x");
    }
}
