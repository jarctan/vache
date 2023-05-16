//! Defining places: left hand side expressions.

use super::{Expr, Var};

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
