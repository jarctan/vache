use super::{Block, VarDef, Ty};

use super::stratum::Stratum;

/// A function in the parser AST.
#[derive(Debug, Clone)]
pub struct Fun {
    /// Name of that function.
    pub name: String,
    /// List of stratum variables.
    pub quantifiers: Vec<Stratum>,
    /// Arguments to that function, with their types
    /// and stratum.
    pub args: Vec<VarDef>,
    /// Return type.
    pub ret_ty: Ty,
    /// Body of the function: a list of statements and
    /// a final expression.
    pub body: Block,
}
