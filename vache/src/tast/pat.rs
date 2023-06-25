//! Defining (pattern matching) patterns.

use super::{Span, Ty, VarDef};

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
