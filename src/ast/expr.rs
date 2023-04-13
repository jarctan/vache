use rug::Integer;

use super::{Var, Block};

/// An expression in the parser AST.
/// 
/// Rule: all variants end with a capital `E`.
#[derive(Debug, Clone)]
pub enum Expr {
    /// Unit expression, that does nothing.
    UnitE,
    /// An unbounded integer.
    IntegerE(Integer),
    /// A variable.
    VarE(Var),
    /// A function call.
    CallE {
        /// Name/identifier of the function.
        name: String,
        /// Arguments to that function.
        args: Vec<Expr>,
    },
    /// An if expression.
    IfE(Box<Expr>, Box<Block>, Box<Block>),
    /// A block expression.
    BlockE(Box<Block>),
}

/// Shortcut to create an `Expr` which is just a variable, based on its name.
pub fn var(v: impl ToString) -> Expr {
    Expr::VarE(v.to_string().into())
}

/// Shortcut to create a constant integer `Expr` based on some integer value.
pub fn int(value: impl Into<Integer>) -> Expr {
    Expr::IntegerE(value.into())
}

/// Shortcut to create a call `Expr`.
pub fn call(name: impl ToString, stmts: impl IntoIterator<Item = Expr>) -> Expr {
    Expr::CallE {
        name: name.to_string(),
        args: stmts.into_iter().collect(),
    }
}

/// Shortcut to create a binary operation `Expr`.
pub fn binop(lhs: Expr, op: impl ToString, rhs: Expr) -> Expr {
    call(op, vec![lhs, rhs])
}

impl From<u64> for Expr {
    fn from(value: u64) -> Self {
        Expr::IntegerE(Integer::from(value))
    }
}

impl From<Var> for Expr {
    fn from(v: Var) -> Self {
        Expr::VarE(v)
    }
}
