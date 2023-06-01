//! Defining statements.

use super::{Block, Expr, Place, VarDef};

/// A statement.
#[derive(Debug, Clone)]
pub enum Stmt<'ctx> {
    /// A declaration. We assign the computation
    /// of the 2nd argument to the newly created variable
    /// defined in the 1st argument.
    Declare(VarDef<'ctx>, Expr<'ctx>),
    /// An assignment.
    Assign(Place<'ctx>, Expr<'ctx>),
    /// An expression, whose final value is discarded.
    ExprS(Expr<'ctx>),
    /// A while statement.
    While {
        /// Condition.
        cond: Expr<'ctx>,
        /// While body.
        body: Block<'ctx>,
    },
}
