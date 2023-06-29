//! Defining typed expressions.

use num_bigint::BigInt;

use super::{Block, Namespaced, Pat, Place, Span, Stratum, Ty};

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
    /// Code span,
    pub span: Span,
}

impl<'ctx> Expr<'ctx> {
    /// Creates a new expression.
    pub fn new(
        kind: impl Into<ExprKind<'ctx>>,
        ty: impl Into<Ty<'ctx>>,
        stm: impl Into<Stratum>,
        span: impl Into<Span>,
    ) -> Self {
        Self {
            kind: kind.into(),
            ty: ty.into(),
            stm: stm.into(),
            span: span.into(),
        }
    }

    /// Creates a new `hole`/placeholder expression that holds the place for
    /// some code located at `span`.
    pub fn hole(span: impl Into<Span>) -> Self {
        Self {
            kind: HoleE,
            ty: Ty::hole(),
            stm: Stratum::static_stm(),
            span: span.into(),
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
    /// A boolean.
    BoolE(bool),
    /// An unbounded integer.
    IntegerE(BigInt),
    /// A string.
    StringE(&'ctx str),
    /// A place.
    PlaceE(Place<'ctx>),
    /// A range.
    ///
    /// Format: `RangeE(start, end)`.
    RangeE(Box<Expr<'ctx>>, Box<Expr<'ctx>>),
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
    /// Tuple creation.
    TupleE(Vec<Expr<'ctx>>),
    /// A function call.
    CallE {
        /// Name/identifier of the function.
        name: Namespaced<'ctx>,
        /// Arguments to that function.
        args: Vec<Expr<'ctx>>,
    },
    /// An if expression.
    IfE(Box<Expr<'ctx>>, Box<Block<'ctx>>, Box<Block<'ctx>>),
    /// A block expression.
    BlockE(Box<Block<'ctx>>),
    /// A pattern matching.
    MatchE(Box<Expr<'ctx>>, Vec<(Pat<'ctx>, Expr<'ctx>)>),
    /// Hole expression.
    HoleE,
    /// Enum variant.
    VariantE {
        /// Enumerated type from which the variant originates.
        enun: &'ctx str,
        /// Variant name.
        variant: &'ctx str,
        /// Variant arguments.
        args: Vec<Expr<'ctx>>,
    },
}

use ExprKind::*;
