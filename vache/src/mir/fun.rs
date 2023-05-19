//! Defining functions in the MIR.

use super::{Cfg, CfgLabel, VarDef};

/// A function in the parser AST.
#[derive(Debug)]
pub struct Fun {
    /// Name of that function.
    pub name: String,
    /// Parameters to that function, with their types
    /// and stratum.
    pub params: Vec<VarDef>,
    /// Return variable. The variable in which the return value is written.
    ///
    /// If `None`, the function returns nothing.
    pub ret_v: Option<VarDef>,
    /// Entry label in the CFG.
    pub entry_l: CfgLabel,
    /// Return label in the CFG.
    pub ret_l: CfgLabel,
    /// Body of the function: a list of statements and
    /// a final expression.
    pub body: Cfg,
}
