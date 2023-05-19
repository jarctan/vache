//! Defining places in the typed AST.

use super::{Expr, Stratum, Ty, Var};

/// A place in the AST: allowed left hand side expressions.
#[derive(Debug, Clone)]
pub struct Place {
    /// The kind of place.
    pub kind: PlaceKind,
    /// Type of the expression.
    pub ty: Ty,
    /// Stratum of the expression.
    pub stm: Stratum,
}

impl Place {
    /// Shortcut to create a place that is a variable.
    pub fn var(var: Var, ty: Ty, stm: Stratum) -> Place {
        Self {
            kind: PlaceKind::VarP(var),
            ty,
            stm,
        }
    }
}

/// Kinds of places.
#[derive(Debug, Clone)]
pub enum PlaceKind {
    /// A mere variable.
    VarP(Var),
    /// An indexed slot into an expression.
    IndexP(Box<Expr>, Box<Expr>),
    /// An field in an expression.
    FieldP(Box<Expr>, Box<Expr>),
}
