//! Defining (pattern matching) patterns.

use std::default::default;

use super::{Span, Ty, TySubst, TyVar, VarDef};
use crate::utils::Set;
use crate::Arena;

/// Code pattern, that represents some data structure to match.
///
/// Used mainly for pattern matching.
#[derive(Debug, Clone, PartialEq)]
pub struct Pat<'ctx> {
    /// The kind of pattern (unit, integer, etc.).
    pub kind: PatKind<'ctx>,
    /// Type of the pattern.
    pub ty: Ty<'ctx>,
    /// Code span.
    pub span: Span,
}

impl<'ctx> Pat<'ctx> {
    /// Creates a new pattern.
    pub fn new(
        kind: impl Into<PatKind<'ctx>>,
        ty: impl Into<Ty<'ctx>>,
        span: impl Into<Span>,
    ) -> Self {
        Self {
            kind: kind.into(),
            ty: ty.into(),
            span: span.into(),
        }
    }

    /// Returns the discriminant of the pattern.
    ///
    /// The discriminant is the branch that tells on which value we should
    /// evaluate that pattern.
    pub fn discriminant(&self) -> crate::anf::Branch<'ctx> {
        use crate::anf::Branch::*;
        match self.kind {
            BoolM(b) => BoolB(b),
            IntegerM(i) => IntB(i),
            StringM(s) => StrB(s),
            IdentM(_) => DefaultB,
            VariantM {
                enun: _,
                variant,
                args: _,
            } => StrB(variant),
        }
    }

    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        Self {
            kind: self.kind.subst_ty(arena, subst),
            ty: self.ty.subst(arena, subst),
            span: self.span,
        }
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        let Self { kind, ty, span: _ } = self;
        kind.free_ty_vars() + ty.free_vars()
    }
}

/// Pattern kinds.
///
/// Rule: all kinds end with a capital `M`.
#[derive(Debug, Clone, PartialEq)]
pub enum PatKind<'ctx> {
    /// Boolean pattern.
    BoolM(bool),
    /// Integer pattern.
    IntegerM(u64),
    /// A string.
    StringM(&'ctx str),
    /// An identifier.
    IdentM(VarDef<'ctx>),
    /// An enum variant.
    VariantM {
        /// Enumerated type from which the variant originates.
        enun: &'ctx str,
        /// Variant name.
        variant: &'ctx str,
        /// Variant arguments.
        args: Vec<Pat<'ctx>>,
    },
}

use PatKind::*;

impl<'ctx> PatKind<'ctx> {
    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        match self {
            pat @ (BoolM(_) | IntegerM(_) | StringM(_)) => pat,
            IdentM(vardef) => IdentM(vardef.subst_ty(arena, subst)),
            VariantM {
                enun,
                variant,
                args,
            } => VariantM {
                enun,
                variant,
                args: args
                    .into_iter()
                    .map(|arg| arg.subst_ty(arena, subst))
                    .collect(),
            },
        }
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        match self {
            BoolM(_) | IntegerM(_) | StringM(_) => default(),
            IdentM(vardef) => vardef.free_ty_vars(),
            VariantM {
                enun: _,
                variant: _,
                args,
            } => args.iter().map(Pat::free_ty_vars).sum(),
        }
    }
}
