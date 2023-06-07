//! Defining statements.

use super::{Block, Pointer, Reference, VarDef};
use crate::mir::RValue;

/// A statement.
#[derive(Debug)]
pub enum Stmt<'ctx> {
    /// A declaration. We assign the computation
    /// of the 2nd argument to the newly created variable
    /// defined in the 1st argument.
    Declare(VarDef<'ctx>),
    /// An assignment.
    Assign(Pointer<'ctx>, RValue<'ctx>),
    /// A function call.
    Call {
        /// Name of the function to call.
        name: &'ctx str,
        /// Arguments to that function.
        args: Vec<Reference<'ctx>>,
        /// Destination variable to hold the result.
        destination: Option<Pointer<'ctx>>,
    },
    /// An if expression.
    If(Reference<'ctx>, Block<'ctx>, Block<'ctx>),
    /// A block expression.
    Block(Block<'ctx>),
    /// A while statement.
    While {
        /// Condition block.
        cond_block: Block<'ctx>,
        /// Condition.
        cond: Reference<'ctx>,
        /// While body.
        body: Block<'ctx>,
    },
    /// Return statement.
    Return(Pointer<'ctx>),
}
