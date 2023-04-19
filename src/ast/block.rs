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

impl Block {
    pub(super) fn subst_stm(self, x: Stratum, with: Stratum) -> Self {
        Self {
            stratum: self.stratum.subst_stm(x, with),
            stmts: self
                .stmts
                .into_iter()
                .map(|stmt| stmt.subst_stm(x, with))
                .collect(),
            ret: self.ret.subst_stm(x, with),
        }
    }
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

impl PartialEq for Block {
    fn eq(&self, other: &Self) -> bool {
        // We deconstruct the block here to automatically trigger an error if we were to
        // add a new field and forget to take it into account in the partial equality test.
        let Block {
            stratum: _,
            stmts: other_stmts,
            ret: other_ret,
        } = other.clone().subst_stm(other.stratum, self.stratum);

        self.stmts.len() == other_stmts.len()
            && self.stmts.iter().zip(&other_stmts).all(|(s1, s2)| s1 == s2)
            && self.ret == other_ret
    }
}

impl Eq for Block {}
