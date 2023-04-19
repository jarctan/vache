use super::{Block, Expr, Stratum, Var, VarDef};

/// A statement.
#[derive(Debug, Clone)]
pub enum Stmt {
    /// A declaration. We assign the computation
    /// of the 2nd argument to the newly created variable
    /// defined in the 1st argument.
    Declare(VarDef, Expr),
    /// An assignment. The variable must already exist.
    Assign(Var, Expr),
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

use Stmt::*;

impl Stmt {
    pub(super) fn subst_stm(self, x: Stratum, with: Stratum) -> Self {
        match self {
            Declare(v, e) => Declare(v.subst_stm(x, with), e.subst_stm(x, with)),
            Assign(v, e) => Assign(v, e.subst_stm(x, with)),
            ExprS(e) => ExprS(e.subst_stm(x, with)),
            While { cond, body } => While {
                cond: cond.subst_stm(x, with),
                body: body.subst_stm(x, with),
            },
        }
    }
}

impl PartialEq for Stmt {
    fn eq(&self, other: &Self) -> bool {
        // Currently, a strict notion of equality: do not substitute variables, only stratum variables.
        match (self, other) {
            (Declare(v1, e1), Declare(v2, e2)) => {
                if v1 == v2 {
                    e1 == e2
                } else {
                    let other = other.clone().subst_stm(v2.stratum, v1.stratum);
                    self == &other
                }
            }
            (Assign(v1, e1), Assign(v2, e2)) => v1 == v2 && e1 == e2,
            (ExprS(e1), ExprS(e2)) => e1 == e2,
            (
                While {
                    cond: cond1,
                    body: body1,
                },
                While {
                    cond: cond2,
                    body: body2,
                },
            ) => cond1 == cond2 && body1 == body2,
            _ => false,
        }
    }
}

impl Eq for Stmt {}
