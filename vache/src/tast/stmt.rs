//! Defining statements.

use super::{Block, Expr, LhsPlace, Span, TySubst, VarDef};
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
    pub(crate) fn subst(self, arena: &'ctx Arena<'ctx>, substs: &TySubst<'ctx>) -> Self {
        Self {
            kind: self.kind.subst(arena, substs),
            span: self.span,
        }
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

    pub(crate) fn subst(self, arena: &'ctx Arena<'ctx>, substs: &TySubst<'ctx>) -> Self {
        match self {
            AssignS(lhs, rhs) => AssignS(lhs.subst(arena, substs), rhs.subst(arena, substs)),
            ExprS(e) => ExprS(e.subst(arena, substs)),
            BreakS => BreakS,
            ContinueS => ContinueS,
            ReturnS(e) => ReturnS(e.subst(arena, substs)),
            WhileS { cond, body } => WhileS {
                cond: cond.subst(arena, substs),
                body: body.subst(arena, substs),
            },
            ForS { item, iter, body } => ForS {
                item: item.subst(arena, substs),
                iter: iter.subst(arena, substs),
                body: body.subst(arena, substs),
            },
            HoleS => HoleS,
        }
    }
}
