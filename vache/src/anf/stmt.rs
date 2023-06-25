//! Defining statements.

use std::collections::HashMap;

use super::{Block, Branch, Namespaced, Pointer, Reference, Span, VarDef};
use crate::mir::RValue;

/// A statement.
#[derive(Debug)]
pub struct Stmt<'mir, 'ctx> {
    /// Statement kind.
    pub kind: StmtKind<'mir, 'ctx>,
    /// Code span in the source code.
    pub span: Span,
}

impl<'mir, 'ctx> Stmt<'mir, 'ctx> {
    /// [`Stmt`] constructor.
    pub fn new(kind: StmtKind<'mir, 'ctx>, span: Span) -> Self {
        Self { kind, span }
    }
}

/// A statement.
#[derive(Debug)]
pub enum StmtKind<'mir, 'ctx> {
    /// A declaration. We assign the computation
    /// of the 2nd argument to the newly created variable
    /// defined in the 1st argument.
    DeclareS(VarDef<'ctx>),
    /// An assignment.
    AssignS(Pointer<'ctx>, RValue<'mir, 'ctx>),
    /// A function call.
    CallS {
        /// Name of the function to call.
        name: Namespaced<'ctx>,
        /// Arguments to that function.
        args: Vec<Reference<'mir, 'ctx>>,
        /// Destination variable to hold the result.
        destination: Option<Pointer<'ctx>>,
    },
    /// An if expression.
    IfS(Reference<'mir, 'ctx>, Block<'mir, 'ctx>, Block<'mir, 'ctx>),
    /// A match statement.
    MatchS(
        Reference<'mir, 'ctx>,
        HashMap<Branch<'ctx>, Block<'mir, 'ctx>>,
    ),
    /// A block expression.
    BlockS(Block<'mir, 'ctx>),
    /// A while statement.
    WhileS {
        /// Condition block.
        cond_block: Block<'mir, 'ctx>,
        /// Condition.
        cond: Reference<'mir, 'ctx>,
        /// While body.
        body: Block<'mir, 'ctx>,
    },
    /// Return statement.
    ReturnS(Pointer<'ctx>),
    /// Break statement.
    BreakS,
    /// Continue statement.
    ContinueS,
}
