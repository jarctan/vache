//! Defining (pattern matching) patterns.

use super::{Span, Ty, TySubst, VarDef};
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

    pub(crate) fn subst(self, arena: &'ctx Arena<'ctx>, substs: &TySubst<'ctx>) -> Self {
        Self {
            kind: self.kind.subst(arena, substs),
            ty: self.ty.subst(arena, substs),
            span: self.span,
        }
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
    pub(crate) fn subst(self, arena: &'ctx Arena<'ctx>, substs: &TySubst<'ctx>) -> Self {
        match self {
            pat @ (BoolM(_) | IntegerM(_) | StringM(_)) => pat,
            IdentM(vardef) => IdentM(vardef.subst(arena, substs)),
            VariantM {
                enun,
                variant,
                args,
            } => VariantM {
                enun,
                variant,
                args: args
                    .into_iter()
                    .map(|arg| arg.subst(arena, substs))
                    .collect(),
            },
        }
    }
}
