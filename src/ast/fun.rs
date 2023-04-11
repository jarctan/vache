use super::{VarDef, Block};

use super::stratum::Stratum;

/// A function in the parser AST.
pub struct Fun {
    /// Name of that function.
    pub name: String,
    /// List of stratum variables.
    pub quantifiers: Vec<Stratum>,
    /// Arguments to that function, with their types
    /// and stratum.
    pub args: Vec<VarDef>,
    /// Body of the function: a list of statements and
    /// a final expression.
    pub body: Block,
}