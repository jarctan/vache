//! Parsing blocks, and defining their representation in the AST.

use super::{Expr, Stmt};

/// A block in the parser AST.
///
/// A block is a list of ordered statements, followed by a final expression.
#[derive(Debug, Clone)]
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