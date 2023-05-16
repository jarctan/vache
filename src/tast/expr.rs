//! Defining typed expressions.

use rug::Integer;

use super::{Block, Stratum, Ty, VarDef};

/// An expression in the typed AST.
///
/// Mainly a wrapper around the parser AST expression, with additional metadata.
#[derive(Debug, Clone)]
pub struct Expr {
    /// The variant of the expression (unit, integer, etc.).
    pub raw: ExprKind,
    /// Type of the expression.
    pub ty: Ty,
    /// Stratum of the expression.
    pub stm: Stratum,
}

impl Expr {
    /// Creates a new expression.
    pub fn new(raw: impl Into<ExprKind>, ty: impl Into<Ty>, stm: impl Into<Stratum>) -> Self {
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
pub enum ExprKind {
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
    /// An index in an array/a map.
    IndexE(Box<Expr>, Box<Expr>),
    /// Array creation.
    ArrayE(Vec<Expr>),
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
