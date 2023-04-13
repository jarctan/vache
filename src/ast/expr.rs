use rug::Integer;

use super::{Var, Block};

/// An expression in the parser AST.
#[derive(Debug, Clone)]
pub enum Expr {
    /// Unit expression, that does nothing.
    Unit,
    /// An unbounded integer.
    Integer(Integer),
    /// A variable.
    Var(Var),
    /// A function call.
    Call {
        /// Name/identifier of the function.
        name: String,
        /// Arguments to that function.
        args: Vec<Expr>,
    },
    /// An if expression.
    If(Box<Expr>, Box<Block>, Box<Block>),
}

impl Expr {
    /// Shortcut to create an `Expr` which is just a variable, based on its name.
    pub fn var(v: impl ToString) -> Self {
        Expr::Var(v.to_string().into())
    }

    /// Shortcut to create a constant integer `Expr` based on some integer value.
    pub fn int(value: impl Into<Integer>) -> Self {
        Expr::Integer(value.into())
    }

    /// Shortcut to create a call `Expr`.
    pub fn call(name: impl ToString, stmts: impl IntoIterator<Item = Expr>) -> Self {
        Expr::Call {
            name: name.to_string(),
            args: stmts.into_iter().collect(),
        }
    }
}

impl From<u64> for Expr {
    fn from(value: u64) -> Self {
        Expr::Integer(Integer::from(value))
    }
}

impl From<Var> for Expr {
    fn from(v: Var) -> Self {
        Expr::Var(v)
    }
}
