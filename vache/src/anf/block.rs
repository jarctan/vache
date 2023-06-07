//! Defining typed blocks.

use super::Stmt;

/// A block in the typed AST.
///
/// A block is a list of ordered statements, followed by a final expression.
pub type Block<'ctx> = Vec<Stmt<'ctx>>;
