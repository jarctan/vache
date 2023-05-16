//! Defining places: left hand side expressions.

use super::{Expr, Var};
use crate::utils::boxed;

/// A place in the AST: allowed left hand side expressions.
#[derive(Debug, Clone)]
pub enum Place {
    /// A mere variable.
    VarP(Var),
    /// An indexed slot into an expression.
    IndexP(Box<Expr>, Box<Expr>),
    /// An field in an expression.
    FieldP(Box<Expr>, Box<Expr>),
}

/// Shortcut to create an indexed variable.
pub fn idx_place(array: impl Into<Expr>, index: impl Into<Expr>) -> Place {
    Place::IndexP(boxed(array.into()), boxed(index.into()))
}

impl From<&str> for Place {
    fn from(v: &str) -> Self {
        Place::VarP(Var::from(v))
    }
}

impl From<String> for Place {
    fn from(v: String) -> Self {
        Place::VarP(Var::from(v))
    }
}
