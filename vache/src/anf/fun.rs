//! Defining typed functions.

use super::{Block, Pointer, VarDef};

/// A function in the typed AST.
#[derive(Debug)]
pub struct Fun<'ctx> {
    /// Name of that function.
    pub name: &'ctx str,
    /// Parameters to that function, with their types
    /// and stratum.
    pub params: Vec<VarDef<'ctx>>,
    /// Return variable. The variable in which the return value is written.
    ///
    /// If `None`, the function returns nothing.
    pub ret_v: Option<Pointer<'ctx>>,
    /// Body of the function: a list of statements and
    /// a final expression.
    pub body: Block<'ctx>,
}
