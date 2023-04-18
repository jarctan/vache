use super::{Expr, Stmt, Stratum};

/// A block in the parser AST.
///
/// A block is a list of ordered statements, followed by a final expression.
#[derive(Debug, Clone)]
pub struct Block {
    /// Stratum for this block.
    ///
    /// This will declare (and possibly override) a new stratum that
    /// exists for the duration of this block.
    pub stratum: Stratum,
    /// List of consecutive statements.
    pub stmts: Vec<Stmt>,
    /// Final return expression.
    pub ret: Expr,
}
/// Creates a block only made of an expression.
pub fn expr(expr: Expr) -> Block {
    Block {
        stratum: Stratum::new_concrete(),
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
pub fn stmts<R: IntoIterator<Item = Stmt>>(mut stmts: impl FnMut(Stratum) -> R) -> Block {
    let stratum = Stratum::new_concrete();
    Block {
        stratum,
        stmts: stmts(stratum).into_iter().collect(),
        ret: Expr::UnitE,
    }
}
