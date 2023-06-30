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
    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        Self {
            stmts: self
                .stmts
                .into_iter()
                .map(|stmt| stmt.subst_ty(arena, subst))
                .collect(),
            ret: self.ret.subst_ty(arena, subst),
            span: self.span,
        }
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        let Self {
            stmts,
            ret,
            span: _,
        } = self;
        stmts.iter().map(Stmt::free_ty_vars).sum::<Set<_>>() + ret.free_ty_vars()
    }
}
