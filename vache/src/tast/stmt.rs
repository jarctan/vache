//! Defining statements.

use super::{Block, Expr, Place, VarDef};

/// A statement.
#[derive(Debug, Clone, Default)]
pub enum Stmt<'ctx> {
    /// A declaration. We assign the computation
    /// of the 2nd argument to the newly created variable
    /// defined in the 1st argument.
    DeclareS(VarDef<'ctx>, Expr<'ctx>),
    /// An assignment.
    AssignS(Place<'ctx>, Expr<'ctx>),
    /// An expression, whose final value is discarded.
    ExprS(Expr<'ctx>),
    /// A while statement.
    WhileS {
        /// Condition.
        cond: Expr<'ctx>,
        /// While body.
        body: Block<'ctx>,
    },
    /// Hole statement.
    #[default]
    HoleS,
}
