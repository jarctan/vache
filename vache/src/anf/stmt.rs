//! Defining statements.

use std::collections::HashMap;

use super::{Block, Branch, Namespaced, Pointer, Reference, VarDef};
use crate::mir::RValue;

/// A statement.
#[derive(Debug)]
pub enum Stmt<'ctx> {
    /// A declaration. We assign the computation
    /// of the 2nd argument to the newly created variable
    /// defined in the 1st argument.
    DeclareS(VarDef<'ctx>),
    /// An assignment.
    AssignS(Pointer<'ctx>, RValue<'ctx>),
    /// A function call.
    CallS {
        /// Name of the function to call.
        name: Namespaced<'ctx>,
        /// Arguments to that function.
        args: Vec<Reference<'ctx>>,
        /// Destination variable to hold the result.
        destination: Option<Pointer<'ctx>>,
    },
    /// An if expression.
    IfS(Reference<'ctx>, Block<'ctx>, Block<'ctx>),
    /// A match statement.
    MatchS(Reference<'ctx>, HashMap<Branch<'ctx>, Block<'ctx>>),
    /// A block expression.
    BlockS(Block<'ctx>),
    /// A while statement.
    WhileS {
        /// Condition block.
        cond_block: Block<'ctx>,
        /// Condition.
        cond: Reference<'ctx>,
        /// While body.
        body: Block<'ctx>,
    },
    /// Return statement.
    ReturnS(Pointer<'ctx>),
    /// Break statement.
    BreakS,
    /// Continue statement.
    ContinueS,
}
