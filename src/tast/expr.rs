use std::collections::HashMap;

use rug::Integer;

use super::{Block, VarDef};

/// An expression in the parser AST.
///
/// Rule: all variants end with a capital `E`.
#[derive(Debug, Clone)]
pub enum Expr {
    /// Unit expression, that does nothing.
    UnitE,
    /// An unbounded integer.
    IntegerE(Integer),
    /// A string.
    StringE(String),
    /// A variable.
    VarE(VarDef),
    /// A field in a structure.
    FieldE(Box<Expr>, String),
    /// An instance of a structure.
    StructE {
        /// Name (identifier).
        name: String,
        /// Collection of field names and values.
        fields: HashMap<String, Expr>,
    },
    /// A function call.
    CallE {
        /// Name/identifier of the function.
        name: String,
        /// Arguments to that function.
        args: Vec<Expr>,
    },
    /// An if expression.
    IfE(Box<Expr>, Box<Block>, Box<Block>),
    /// A block expression.
    BlockE(Box<Block>),
    /// Copies an expression.
    CopyE(Box<Expr>),
    /// Owns an expression.
    OwnE(Box<Expr>),
}
