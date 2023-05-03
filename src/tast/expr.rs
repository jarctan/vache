use rug::Integer;

use super::{Block, Stratum, Ty, VarDef};

/// An expression in the typed AST.
///
/// Rule: all variants end with a capital `E`.
#[derive(Debug, Clone)]
pub struct Expr {
    pub raw: RawExpr,
    pub ty: Ty,
    pub stm: Stratum,
}

impl Expr {
    pub fn new(raw: impl Into<RawExpr>, ty: impl Into<Ty>, stm: impl Into<Stratum>) -> Self {
        Self {
            raw: raw.into(),
            ty: ty.into(),
            stm: stm.into(),
        }
    }
}

/// Variants for an expression.
///
/// Rule: all variants end with a capital `E`.
#[derive(Debug, Clone)]
pub enum RawExpr {
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
        ///
        /// Ordered because we need to specify here the evaluation order.
        fields: Vec<(String, Expr)>,
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
}
