//! Defining statements.

use super::{Block, Expr, Place, VarDef};

/// A statement.
#[derive(Debug, Clone)]
pub enum Stmt {
    /// A declaration. We assign the computation
    /// of the 2nd argument to the newly created variable
    /// defined in the 1st argument.
    Declare(VarDef, Expr),
    /// An assignment.
    Assign(Place, Expr),
    /// An expression, whose final value is discarded.
    ExprS(Expr),
    /// A while statement.
    While {
        /// Condition.
        cond: Expr,
        /// While body.
        body: Block,
    },
}

impl PartialEq for Stmt {
    fn eq(&self, _other: &Self) -> bool {
        todo!()
    }
}

impl Eq for Stmt {}
