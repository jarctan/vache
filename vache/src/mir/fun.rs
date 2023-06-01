//! Defining functions in the MIR.

use super::{CfgI, CfgLabel, VarDef};

/// A function in the parser AST.
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
    pub ret_v: Option<VarDef<'ctx>>,
    /// Entry label in the CFG.
    pub entry_l: CfgLabel,
    /// Return label in the CFG.
    pub ret_l: CfgLabel,
    /// Body of the function: a list of statements and
    /// a final expression.
    pub body: CfgI<'ctx>,
}
