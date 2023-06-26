//! Defining statements.

use super::{Block, Expr, LhsPlace, Span, VarDef};

/// A statement.
#[derive(Debug, Clone, Default)]
pub struct Stmt<'ctx> {
    /// Statement kind.
    pub kind: StmtKind<'ctx>,
    /// Codespan in the source code.
    pub span: Span,
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

impl<'ctx> StmtKind<'ctx> {
    /// Enrich the [`StmtKind`] with some [`Span`] information to get a
    /// [`Stmt`].
    pub(crate) fn with_span(self, span: Span) -> Stmt<'ctx> {
        Stmt { kind: self, span }
    }
}
