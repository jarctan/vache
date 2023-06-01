//! Defining places in the typed AST.

use super::{Expr, Mode, Stratum, Ty, Var};

/// A place in the AST: allowed left hand side expressions.
#[derive(Debug, Clone)]
pub struct Place<'ctx> {
    /// The kind of place.
    pub kind: PlaceKind<'ctx>,
    /// Type of the expression.
    pub ty: Ty<'ctx>,
    /// Stratum of the expression.
    pub stm: Stratum,
    /// Do we transfer ownership or take by reference?
    pub mode: Mode,
}

impl<'ctx> Place<'ctx> {
    /// Shortcut to create a place that is a variable.
    pub fn var(var: Var<'ctx>, ty: Ty<'ctx>, stm: Stratum, mode: Mode) -> Self {
        Self {
            kind: PlaceKind::VarP(var),
            ty,
            stm,
            mode,
        }
    }
}

/// Kinds of places.
#[derive(Debug, Clone)]
pub enum PlaceKind<'ctx> {
    /// A mere variable.
    VarP(Var<'ctx>),
    /// An indexed slot into an expression.
    IndexP(Box<Expr<'ctx>>, Box<Expr<'ctx>>),
    /// An field in an expression.
    FieldP(Box<Expr<'ctx>>, &'ctx str),
}
