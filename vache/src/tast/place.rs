//! Defining places in the typed AST.

use super::{Expr, LhsMode, Mode, Span, Stratum, Ty, TySubst, TyVar, VarUse};
use crate::utils::boxed;
use crate::utils::set::Set;
use crate::Arena;

/// A place in the AST: allowed left hand side expressions.
#[derive(Debug, Clone)]
pub struct Place<'ctx> {
    /// The kind of place.
    pub kind: PlaceKind<'ctx>,
    /// Type of the place.
    pub ty: Ty<'ctx>,
    /// Stratum of the place.
    pub stm: Stratum,
    /// Do we transfer ownership or take by reference?
    pub mode: Mode,
    /// Codespan.
    pub span: Span,
}

impl<'ctx> Place<'ctx> {
    /// Shortcut to create a place that is a variable.
    pub fn var(
        var: impl Into<VarUse<'ctx>>,
        ty: impl Into<Ty<'ctx>>,
        stm: Stratum,
        mode: Mode,
        span: impl Into<Span>,
    ) -> Self {
        Self {
            kind: PlaceKind::VarP(var.into()),
            ty: ty.into(),
            stm,
            mode,
            span: span.into(),
        }
    }

    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, substs: &TySubst<'ctx>) -> Self {
        Self {
            kind: self.kind.subst_ty(arena, substs),
            ty: self.ty.subst(arena, substs),
            stm: self.stm,
            mode: self.mode,
            span: self.span,
        }
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        let Self {
            kind,
            ty,
            stm: _,
            mode: _,
            span: _,
        } = self;
        kind.free_ty_vars() + ty.free_vars()
    }
}

/// A lhs place in the AST: allowed left hand side expressions.
#[derive(Debug, Clone)]
pub struct LhsPlace<'ctx> {
    /// The kind of place.
    pub kind: PlaceKind<'ctx>,
    /// Type of the place.
    pub ty: Ty<'ctx>,
    /// Stratum of the place.
    pub stm: Stratum,
    /// Declaring or assigning?
    pub mode: LhsMode,
    /// Codespan.
    pub span: Span,
}

impl<'ctx> LhsPlace<'ctx> {
    /// Shortcut to create a place that is a variable.
    pub fn var(
        var: impl Into<VarUse<'ctx>>,
        ty: impl Into<Ty<'ctx>>,
        stm: Stratum,
        mode: LhsMode,
        span: impl Into<Span>,
    ) -> Self {
        Self {
            kind: PlaceKind::VarP(var.into()),
            ty: ty.into(),
            stm,
            mode,
            span: span.into(),
        }
    }

    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, substs: &TySubst<'ctx>) -> Self {
        Self {
            kind: self.kind.subst_ty(arena, substs),
            ty: self.ty.subst(arena, substs),
            stm: self.stm,
            mode: self.mode,
            span: self.span,
        }
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        let Self {
            kind,
            ty,
            stm: _,
            mode: _,
            span: _,
        } = self;
        kind.free_ty_vars() + ty.free_vars()
    }
}

/// Kinds of places.
#[derive(Debug, Clone)]
pub enum PlaceKind<'ctx> {
    /// A mere variable.
    VarP(VarUse<'ctx>),
    /// An indexed slot into an expression.
    IndexP(Box<Expr<'ctx>>, Box<Expr<'ctx>>),
    /// An field in an expression.
    FieldP(Box<Expr<'ctx>>, &'ctx str),
    /// An element in a tuple.
    ElemP(Box<Expr<'ctx>>, usize),
}

use PlaceKind::*;

impl<'ctx> PlaceKind<'ctx> {
    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, substs: &TySubst<'ctx>) -> Self {
        match self {
            VarP(var) => VarP(var.subst_ty(arena, substs)),
            IndexP(box array, box index) => IndexP(
                boxed(array.subst_ty(arena, substs)),
                boxed(index.subst_ty(arena, substs)),
            ),
            FieldP(box strukt, field) => FieldP(boxed(strukt.subst_ty(arena, substs)), field),
            ElemP(box tuple, index) => ElemP(boxed(tuple.subst_ty(arena, substs)), index),
        }
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        match self {
            VarP(var) => var.free_ty_vars(),
            IndexP(box array, box index) => array.free_ty_vars() + index.free_ty_vars(),
            FieldP(box strukt, _) => strukt.free_ty_vars(),
            ElemP(box tuple, _) => tuple.free_ty_vars(),
        }
    }
}
