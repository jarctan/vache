//! Function arguments are special kinds of expressions.

use super::{Expr, Place, Span, Stratum, Ty, TySubst, TyVar};
use crate::utils::set::Set;
use crate::Arena;

/// Function call argument.
#[derive(Debug, Clone)]
pub struct Arg<'ctx> {
    /// Kind of argument.
    pub kind: ArgKind<'ctx>,
    /// Stratum of the expression.
    pub stm: Stratum,
    /// Codespan.
    pub span: Span,
}

impl<'ctx> Arg<'ctx> {
    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        Self {
            kind: self.kind.subst_ty(arena, subst),
            stm: self.stm,
            span: self.span,
        }
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        let Self {
            kind,
            stm: _,
            span: _,
        } = self;
        kind.free_ty_vars()
    }

    /// Is this argument a standard argument? If so, return the expression
    /// behind it.
    ///
    /// If it fails, returns the original value passed as argument.
    pub(crate) fn into_standard(self) -> Result<Expr<'ctx>, Self> {
        match self.kind {
            ArgKind::Standard(e) => Ok(e),
            _ => Err(self),
        }
    }

    /// Shortcut to create a standard function argument.
    pub(crate) fn std(expr: Expr<'ctx>) -> Arg<'ctx> {
        Self {
            stm: expr.stm,
            span: expr.span,
            kind: ArgKind::Standard(expr),
        }
    }

    /// Returns the type of that function argument.
    pub fn ty(&self) -> Ty<'ctx> {
        match &self.kind {
            ArgKind::Standard(e) => e.ty,
            ArgKind::InPlace(p) => p.ty,
            ArgKind::Binding(e, p) => {
                debug_assert_eq!(e.ty, p.ty);
                e.ty
            }
        }
    }
}

/// Kind of function call argument.
#[derive(Debug, Clone)]
pub enum ArgKind<'ctx> {
    /// Default/standard kind of argument.
    ///
    /// Notation: no extra notation.
    Standard(Expr<'ctx>),
    /// Argument mutated in place.
    ///
    /// Notation: `@place` in the code.
    InPlace(Place<'ctx>),
    /// Argument binding to a new place.
    ///
    /// Notation: `expr@place` in the code.
    Binding(Expr<'ctx>, Place<'ctx>),
}

impl<'ctx> ArgKind<'ctx> {
    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        match self {
            ArgKind::Standard(e) => e.free_ty_vars(),
            ArgKind::InPlace(p) => p.free_ty_vars(),
            ArgKind::Binding(e, p) => e.free_ty_vars() + p.free_ty_vars(),
        }
    }

    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        match self {
            ArgKind::Standard(e) => ArgKind::Standard(e.subst_ty(arena, subst)),
            ArgKind::InPlace(p) => ArgKind::InPlace(p.subst_ty(arena, subst)),
            ArgKind::Binding(e, p) => {
                ArgKind::Binding(e.subst_ty(arena, subst), p.subst_ty(arena, subst))
            }
        }
    }
}
