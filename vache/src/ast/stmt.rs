//! Parsing statements, and defining their representation in the AST.

use super::{Block, Expr, Place, VarDef};
use crate::utils::boxed;

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

/// Shortcut to print several expressions in our program.
pub fn print(stmts: impl IntoIterator<Item = Expr>) -> Stmt {
    Stmt::ExprS(super::expr::call("print", stmts))
}

/// Shortcut to make a call.
pub fn call_stmt(name: impl ToString, stmts: impl IntoIterator<Item = Expr>) -> Stmt {
    Stmt::ExprS(super::expr::call(name, stmts))
}

/// Shortcut for a block statement.
pub fn block_stmt(b: impl Into<Block>) -> Stmt {
    Stmt::ExprS(Expr::BlockE(boxed(b.into())))
}

impl PartialEq for Stmt {
    fn eq(&self, _other: &Self) -> bool {
        todo!()
    }
}

impl Eq for Stmt {}