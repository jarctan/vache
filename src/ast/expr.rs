use std::borrow::Cow;

use rug::Integer;

use super::{boxed, Block, Stratum, Var};

/// An expression in the parser AST.
///
/// Rule: all variants end with a capital `E`.
#[derive(Debug, Clone, PartialEq, Eq)]
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

use Expr::*;

impl Expr {
    pub(super) fn subst_stm(self, x: Stratum, with: Stratum) -> Self {
        match self {
            UnitE | IntegerE(_) | VarE(_) => self,
            IfE(box cond, box iftrue, box iffalse) => IfE(
                boxed(cond.subst_stm(x, with)),
                boxed(iftrue.subst_stm(x, with)),
                boxed(iffalse.subst_stm(x, with)),
            ),
            CallE {
                name,
                strata,
                args,
                ret_stm,
            } => CallE {
                name,
                strata: strata.into_iter().map(|s| s.subst_stm(x, with)).collect(),
                args: args.into_iter().map(|e| e.subst_stm(x, with)).collect(),
                ret_stm: ret_stm.subst_stm(x, with),
            },
            BlockE(box b) => BlockE(boxed(b.subst_stm(x, with))),
        }
    }
}

/// Shortcut to create an `Expr` which is just a variable, based on its name.
pub fn var(v: impl ToString) -> Expr {
    VarE(v.to_string().into())
}

/// Shortcut to create a constant integer `Expr` based on some integer value.
pub fn int(value: impl Into<Integer>) -> Expr {
    IntegerE(value.into())
}

/// Shortcut to create a call `Expr`.
pub fn call(
    name: impl ToString,
    strata: impl IntoIterator<Item = Stratum>,
    stmts: impl IntoIterator<Item = Expr>,
    ret_stm: impl Into<Stratum>,
) -> Expr {
    CallE {
        name: name.to_string(),
        strata: strata.into_iter().collect(),
        ret_stm: ret_stm.into(),
        args: stmts.into_iter().collect(),
    }
}

/// Shortcut to create a block `Expr`.
pub fn block(value: impl Into<Block>) -> Expr {
    BlockE(Box::new(value.into()))
}

/// Shortcut to create a binary operation `Expr`.
pub fn binop(lhs: Expr, op: impl ToString, rhs: Expr, stratum: Stratum, ret_stm: Stratum) -> Expr {
    call(op, vec![stratum], vec![lhs, rhs], ret_stm)
}

impl From<u64> for Expr {
    fn from(value: u64) -> Self {
        IntegerE(Integer::from(value))
    }
}

impl From<Var> for Expr {
    fn from(v: Var) -> Self {
        VarE(v)
    }
}
