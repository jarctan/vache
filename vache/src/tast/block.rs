//! Defining typed blocks.

use super::{Expr, Span, Stmt, TySubst};
use crate::Arena;

/// A block in the typed AST.
///
/// A block is a list of ordered statements, followed by a final expression.
#[derive(Debug, Clone)]
pub struct Block<'ctx> {
    /// List of consecutive statements.
    pub stmts: Vec<Stmt<'ctx>>,
    /// Final return expression.
    pub ret: Expr<'ctx>,
    /// Code span.
    pub span: Span,
}

impl<'ctx> Block<'ctx> {
    pub(crate) fn subst(self, arena: &'ctx Arena<'ctx>, substs: &TySubst<'ctx>) -> Self {
        Self {
            stmts: self
                .stmts
                .into_iter()
                .map(|stmt| stmt.subst(arena, substs))
                .collect(),
            ret: self.ret.subst(arena, substs),
            span: self.span,
        }
    }
}
