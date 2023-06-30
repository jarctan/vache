//! Defining typed blocks.

use super::{Expr, Span, Stmt, TySubst, TyVar};
use crate::utils::set::Set;
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

    pub(crate) fn free_vars(&self) -> Set<TyVar<'ctx>> {
        let Self {
            stmts,
            ret,
            span: _,
        } = self;
        stmts.iter().map(Stmt::free_vars).sum::<Set<_>>() + ret.free_vars()
    }
}
