//! Defining functions in the MIR.

use super::Stmt;
use super::VarDef;

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
    /// Body of the function: a list of statements and
    /// a final expression.
    pub body: Vec<Stmt>,
}
