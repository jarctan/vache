use rug::Integer;

use super::{Block, Stratum, Var};

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
        /// Stratum instantiations.
        strata: Vec<Stratum>,
        /// Arguments to that function.
        args: Vec<Expr>,
        /// Return stratum.
        ret_stm: Stratum,
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
pub fn call(
    name: impl ToString,
    strata: impl IntoIterator<Item = Stratum>,
    stmts: impl IntoIterator<Item = Expr>,
    ret_stm: impl Into<Stratum>,
) -> Expr {
    Expr::CallE {
        name: name.to_string(),
        strata: strata.into_iter().collect(),
        ret_stm: ret_stm.into(),
        args: stmts.into_iter().collect(),
    }
}

/// Shortcut to create a binary operation `Expr`.
pub fn binop(lhs: Expr, op: impl ToString, rhs: Expr, stratum: Stratum, ret_stm: Stratum) -> Expr {
    call(op, vec![stratum], vec![lhs, rhs], ret_stm)
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
