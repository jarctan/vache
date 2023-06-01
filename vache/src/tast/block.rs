//! Defining typed blocks.

use super::{Expr, Stmt};

/// A block in the typed AST.
///
/// A block is a list of ordered statements, followed by a final expression.
#[derive(Debug, Clone)]
pub struct Block<'ctx> {
    /// List of consecutive statements.
    pub stmts: Vec<Stmt<'ctx>>,
    /// Final return expression.
    pub ret: Expr<'ctx>,
}
