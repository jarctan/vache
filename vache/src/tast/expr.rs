//! Defining typed expressions.

use std::default::default;

use num_bigint::BigInt;

use super::{Block, Namespaced, Pat, Place, Span, Stratum, Ty, TySubst, TyVar};
use crate::utils::boxed;
use crate::utils::set::Set;
use crate::Arena;

/// An expression in the typed AST.
///
/// Mainly a wrapper around the parser AST expression, with additional metadata.
#[derive(Debug, Clone)]
pub struct Expr<'ctx> {
    /// The kind of expression (unit, integer, etc.).
    pub kind: ExprKind<'ctx>,
    /// Type of the expression.
    pub ty: Ty<'ctx>,
    /// Stratum of the expression.
    pub stm: Stratum,
    /// Code span,
    pub span: Span,
}

impl<'ctx> Expr<'ctx> {
    /// Creates a new expression.
    pub fn new(
        kind: impl Into<ExprKind<'ctx>>,
        ty: impl Into<Ty<'ctx>>,
        stm: impl Into<Stratum>,
        span: impl Into<Span>,
    ) -> Self {
        Self {
            kind: kind.into(),
            ty: ty.into(),
            stm: stm.into(),
            span: span.into(),
        }
    }

    /// Creates a new `hole`/placeholder expression that holds the place for
    /// some code located at `span`.
    pub fn hole(span: Span) -> Self {
        Self {
            kind: HoleE,
            ty: Ty::hole(span),
            stm: Stratum::static_stm(),
            span,
        }
    }

    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, substs: &TySubst<'ctx>) -> Self {
        Self {
            kind: self.kind.subst_ty(arena, substs),
            ty: self.ty.subst(arena, substs),
            stm: self.stm,
            span: self.span,
        }
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        let Self {
            kind,
            ty,
            stm: _,
            span: _,
        } = self;
        ty.free_vars() + kind.free_ty_vars()
    }
}

/// Variants for an expression.
///
/// Rule: all variants end with a capital `E`.
#[derive(Debug, Clone)]
pub enum ExprKind<'ctx> {
    /// Unit expression, that does nothing.
    UnitE,
    /// A boolean.
    BoolE(bool),
    /// An unbounded integer.
    IntegerE(BigInt),
    /// A string.
    StringE(&'ctx str),
    /// A place.
    PlaceE(Place<'ctx>),
    /// A range.
    ///
    /// Format: `RangeE(start, end)`.
    RangeE(Box<Expr<'ctx>>, Box<Expr<'ctx>>),
    /// An instance of a structure.
    StructE {
        /// Name (identifier).
        name: &'ctx str,
        /// Collection of field names and values.
        ///
        /// Ordered because we need to specify here the evaluation order.
        fields: Vec<(&'ctx str, Expr<'ctx>)>,
    },
    /// Array creation.
    ArrayE(Vec<Expr<'ctx>>),
    /// Tuple creation.
    TupleE(Vec<Expr<'ctx>>),
    /// A function call.
    CallE {
        /// Name/identifier of the function.
        name: Namespaced<'ctx>,
        /// Arguments to that function.
        args: Vec<Expr<'ctx>>,
    },
    /// An if expression.
    IfE(Box<Expr<'ctx>>, Box<Block<'ctx>>, Box<Block<'ctx>>),
    /// A block expression.
    BlockE(Box<Block<'ctx>>),
    /// A pattern matching.
    MatchE(Box<Expr<'ctx>>, Vec<(Pat<'ctx>, Expr<'ctx>)>),
    /// Hole expression.
    HoleE,
    /// Enum variant.
    VariantE {
        /// Enumerated type from which the variant originates.
        enun: &'ctx str,
        /// Variant name.
        variant: &'ctx str,
        /// Variant arguments.
        args: Vec<Expr<'ctx>>,
    },
}

use ExprKind::*;

impl<'ctx> ExprKind<'ctx> {
    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, substs: &TySubst<'ctx>) -> Self {
        match self {
            prim @ (UnitE | BoolE(_) | IntegerE(_) | StringE(_) | HoleE) => prim,
            PlaceE(place) => PlaceE(place.subst_ty(arena, substs)),
            RangeE(box e1, box e2) => RangeE(
                boxed(e1.subst_ty(arena, substs)),
                boxed(e2.subst_ty(arena, substs)),
            ),
            StructE { name, fields } => StructE {
                name,
                fields: fields
                    .into_iter()
                    .map(|(name, e)| (name, e.subst_ty(arena, substs)))
                    .collect(),
            },
            ArrayE(items) => ArrayE(
                items
                    .into_iter()
                    .map(|item| item.subst_ty(arena, substs))
                    .collect(),
            ),
            TupleE(elems) => TupleE(
                elems
                    .into_iter()
                    .map(|elem| elem.subst_ty(arena, substs))
                    .collect(),
            ),
            CallE { name, args } => CallE {
                name,
                args: args
                    .into_iter()
                    .map(|arg| arg.subst_ty(arena, substs))
                    .collect(),
            },
            IfE(box cond, box iftrue, box iffalse) => IfE(
                boxed(cond.subst_ty(arena, substs)),
                boxed(iftrue.subst_ty(arena, substs)),
                boxed(iffalse.subst_ty(arena, substs)),
            ),
            BlockE(box b) => BlockE(boxed(b.subst_ty(arena, substs))),
            MatchE(box matched, variants) => MatchE(
                boxed(matched.subst_ty(arena, substs)),
                variants
                    .into_iter()
                    .map(|(pat, e)| (pat.subst_ty(arena, substs), e.subst_ty(arena, substs)))
                    .collect(),
            ),
            VariantE {
                enun,
                variant,
                args,
            } => VariantE {
                enun,
                variant,
                args: args
                    .into_iter()
                    .map(|arg| arg.subst_ty(arena, substs))
                    .collect(),
            },
        }
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        match self {
            UnitE | BoolE(_) | IntegerE(_) | StringE(_) | HoleE => default(),
            PlaceE(place) => place.free_ty_vars(),
            RangeE(e1, e2) => e1.free_ty_vars() + e2.free_ty_vars(),
            StructE { name: _, fields } => fields.iter().map(|(_, e)| e.free_ty_vars()).sum(),
            ArrayE(items) => items.iter().map(Expr::free_ty_vars).sum(),
            TupleE(elems) => elems.iter().map(Expr::free_ty_vars).sum(),
            CallE { name: _, args } => args.iter().map(Expr::free_ty_vars).sum(),
            IfE(box cond, box iftrue, box iffalse) => {
                cond.free_ty_vars() + iftrue.free_ty_vars() + iffalse.free_ty_vars()
            }
            BlockE(box b) => b.free_ty_vars(),
            MatchE(box matched, branches) => {
                matched.free_ty_vars()
                    + branches
                        .iter()
                        .map(|(pat, e)| pat.free_ty_vars() + e.free_ty_vars())
                        .sum::<Set<_>>()
            }
            VariantE {
                enun: _,
                variant: _,
                args,
            } => args.iter().map(Expr::free_ty_vars).sum(),
        }
    }
}
