//! Defining statements.

use std::default::default;

use super::{Block, Expr, LhsPlace, Place, Span, TySubst, TyVar, VarDef};
use crate::utils::set::Set;
use crate::Arena;

/// A statement.
#[derive(Debug, Clone, Default)]
pub struct Stmt<'ctx> {
    /// Statement kind.
    pub kind: StmtKind<'ctx>,
    /// Codespan in the source code.
    pub span: Span,
}
impl<'ctx> Stmt<'ctx> {
    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        Self {
            kind: self.kind.subst_ty(arena, subst),
            span: self.span,
        }
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        let Self { kind, span: _ } = self;
        kind.free_ty_vars()
    }
}

/// Statement kind.
#[derive(Debug, Clone, Default)]
pub enum StmtKind<'ctx> {
    /// An assignment.
    AssignS(LhsPlace<'ctx>, Expr<'ctx>),
    /// An expression, whose final value is discarded.
    ExprS(Expr<'ctx>),
    /// A break statement.
    BreakS,
    /// A continue statement.
    ContinueS,
    /// A return statement.
    ReturnS(Expr<'ctx>),
    /// A while statement.
    WhileS {
        /// Condition.
        cond: Expr<'ctx>,
        /// While body.
        body: Block<'ctx>,
    },
    /// A for loop.
    ForS {
        /// Item used within the loop.
        item: VarDef<'ctx>,
        /// Element being iterated over.
        iter: Expr<'ctx>,
        /// For loop body.
        body: Block<'ctx>,
    },
    /// A swap statement that exchanges two places.
    SwapS(Place<'ctx>, Place<'ctx>),
    /// Hole statement.
    #[default]
    HoleS,
}

use StmtKind::*;

impl<'ctx> StmtKind<'ctx> {
    /// Enrich the [`StmtKind`] with some [`Span`] information to get a
    /// [`Stmt`].
    pub(crate) fn with_span(self, span: Span) -> Stmt<'ctx> {
        Stmt { kind: self, span }
    }

    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        match self {
            AssignS(lhs, rhs) => AssignS(lhs.subst_ty(arena, subst), rhs.subst_ty(arena, subst)),
            SwapS(place1, place2) => {
                SwapS(place1.subst_ty(arena, subst), place2.subst_ty(arena, subst))
            }
            ExprS(e) => ExprS(e.subst_ty(arena, subst)),
            BreakS => BreakS,
            ContinueS => ContinueS,
            ReturnS(e) => ReturnS(e.subst_ty(arena, subst)),
            WhileS { cond, body } => WhileS {
                cond: cond.subst_ty(arena, subst),
                body: body.subst_ty(arena, subst),
            },
            ForS { item, iter, body } => ForS {
                item: item.subst_ty(arena, subst),
                iter: iter.subst_ty(arena, subst),
                body: body.subst_ty(arena, subst),
            },
            HoleS => HoleS,
        }
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        match self {
            AssignS(lhs, rhs) => lhs.free_ty_vars() + rhs.free_ty_vars(),
            SwapS(place1, place2) => place1.free_ty_vars() + place2.free_ty_vars(),
            ExprS(e) => e.free_ty_vars(),
            BreakS | ContinueS | HoleS => default(),
            ReturnS(e) => e.free_ty_vars(),
            WhileS { cond, body } => cond.free_ty_vars() + body.free_ty_vars(),
            ForS { item, iter, body } => {
                item.free_ty_vars() + iter.free_ty_vars() + body.free_ty_vars()
            }
        }
    }
}
