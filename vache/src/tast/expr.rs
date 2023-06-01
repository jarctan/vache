//! Defining typed expressions.

use num_bigint::BigInt;

use super::{Block, Place, Stratum, Ty};

/// An expression in the typed AST.
///
/// Mainly a wrapper around the parser AST expression, with additional metadata.
#[derive(Debug, Clone)]
pub struct Expr<'ctx> {
    /// The kind of expression (unit, integer, etc.).
    pub kind: ExprKind<'ctx>,
    /// Type of the expression.
    pub ty: Ty<'ctx>,
    /// Stratum of the expression.
    pub stm: Stratum,
}

impl<'ctx> Expr<'ctx> {
    /// Creates a new expression.
    pub fn new(
        kind: impl Into<ExprKind<'ctx>>,
        ty: impl Into<Ty<'ctx>>,
        stm: impl Into<Stratum>,
    ) -> Self {
        Self {
            kind: kind.into(),
            ty: ty.into(),
            stm: stm.into(),
        }
    }
}

/// Variants for an expression.
///
/// Rule: all variants end with a capital `E`.
#[derive(Debug, Clone)]
pub enum ExprKind<'ctx> {
    /// Unit expression, that does nothing.
    UnitE,
    /// An unbounded integer.
    IntegerE(BigInt),
    /// A string.
    StringE(&'ctx str),
    /// A place.
    PlaceE(Place<'ctx>),
    /// An instance of a structure.
    StructE {
        /// Name (identifier).
        name: &'ctx str,
        /// Collection of field names and values.
        ///
        /// Ordered because we need to specify here the evaluation order.
        fields: Vec<(&'ctx str, Expr<'ctx>)>,
    },
    /// Array creation.
    ArrayE(Vec<Expr<'ctx>>),
    /// A function call.
    CallE {
        /// Name/identifier of the function.
        name: &'ctx str,
        /// Arguments to that function.
        args: Vec<Expr<'ctx>>,
    },
    /// An if expression.
    IfE(Box<Expr<'ctx>>, Box<Block<'ctx>>, Box<Block<'ctx>>),
    /// A block expression.
    BlockE(Box<Block<'ctx>>),
}
