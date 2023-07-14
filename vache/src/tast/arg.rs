//! Function arguments are special kinds of expressions.

use super::{Expr, LhsPlace, Place, Span, Stratum, Ty, TySubst, TyVar};
use crate::utils::Set;
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

    /// Is this argument a binding argument? If so, return the from and to
    /// locations of it.
    ///
    /// Return format of the tuple: (`from`, `to`)
    pub(crate) fn as_binding(&self) -> Option<(&Expr<'ctx>, &LhsPlace<'ctx>)> {
        match &self.kind {
            ArgKind::Binding(e, p) => Some((e, p)),
            _ => None,
        }
    }

    /// Shortcut to create a standard function argument.
    pub(crate) fn std(expr: Expr<'ctx>) -> Self {
        Self {
            stm: expr.stm,
            span: expr.span,
            kind: ArgKind::Standard(expr),
        }
    }

    /// Shortcut to create an in place (pass-by-reference) function argument.
    ///
    /// We also take the `span` as argument as it will normally be bigger than
    /// the span of the place (the former is `@p`, the latter is `p`).
    pub(crate) fn in_place(place: Place<'ctx>, span: Span) -> Self {
        Self {
            stm: place.stm,
            span,
            kind: ArgKind::InPlace(place),
        }
    }

    /// Shortcut to create a binding function argument from moving `from` and
    /// biding to `to`.
    ///
    /// We also take the `span` as argument as it will normally be bigger than
    /// the span of the place (the former is `@p`, the latter is `p`).
    pub(crate) fn binding(from: Expr<'ctx>, to: LhsPlace<'ctx>, span: Span) -> Self {
        Self {
            stm: from.stm, // TODO: check this is the good stratum to use (why not `from.stm`?)
            span,
            kind: ArgKind::Binding(from, to),
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

    /// Is this argument passing by reference?
    pub fn byref(&self) -> bool {
        // We do exhaustive pattern matching to trigger an error if we add/modify
        // variants
        match &self.kind {
            ArgKind::Standard(..) => false,
            ArgKind::InPlace(..) | ArgKind::Binding(..) => true,
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
    Binding(Expr<'ctx>, LhsPlace<'ctx>),
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
