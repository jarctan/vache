//! Defining places in the MIR.

use std::fmt;

use super::Var;

/// Kinds of places.
#[derive(Clone, Copy)]
pub enum Place<'ctx> {
    /// A mere variable.
    VarP(Var<'ctx>),
    /// An indexed slot into an expression.
    IndexP(Var<'ctx>, Var<'ctx>),
    /// An field in an expression.
    FieldP(Var<'ctx>, Var<'ctx>),
}

impl<'ctx> Place<'ctx> {
    /// Gets the defined (overwritten) variable by that place.
    pub fn defs(&self) -> &Var<'ctx> {
        use Place::*;
        match self {
            VarP(v) => v,
            IndexP(array, _) => array,
            FieldP(strukt, _) => strukt,
        }
    }

    /// Gets the variables used by that place (variables that need to exist
    /// beforehand).
    pub fn uses(&self) -> Option<Var<'ctx>> {
        use Place::*;
        match self {
            VarP(_) => None,
            IndexP(array, _) => Some(*array),
            FieldP(strukt, _) => Some(*strukt),
        }
    }
}

impl<'ctx> From<Var<'ctx>> for Place<'ctx> {
    fn from(var: Var<'ctx>) -> Self {
        Place::VarP(var)
    }
}

impl fmt::Debug for Place<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Place::VarP(v) => write!(f, "{v:?}"),
            Place::IndexP(array, index) => write!(f, "{array:?}[{index:?}]"),
            Place::FieldP(strukt, field) => write!(f, "{strukt:?}.{field:?}"),
        }
    }
}
