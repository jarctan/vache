use super::{VarDef, Expr, Block};

/// A statement.
pub enum Stmt {
    /// An assignment. We assign the computation
    /// of the 2nd argument to the newly created variable
    /// defined in the 1st argument.
    Assign(VarDef, Expr),
    /// An if expression.
    If(Expr, Block, Block)
}