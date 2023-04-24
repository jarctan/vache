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
