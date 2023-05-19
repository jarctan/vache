//! Defining places in the MIR.

use std::fmt;

use super::Var;

/// Kinds of places.
#[derive(Clone)]
pub enum Place {
    /// A mere variable.
    VarP(Var),
    /// An indexed slot into an expression.
    IndexP(Var, Var),
    /// An field in an expression.
    FieldP(Var, Var),
}

impl Place {
    /// Gets the defined (overwritten) variable by that place.
    pub fn defs(&self) -> &Var {
        use Place::*;
        match self {
            VarP(v) => v,
            IndexP(array, _) => array,
            FieldP(strukt, _) => strukt,
        }
    }

    /// Gets the variables used by that place (variables that need to exist
    /// beforehand).
    pub fn uses(&self) -> Option<Var> {
        use Place::*;
        match self {
            VarP(_) => None,
            IndexP(array, _) => Some(array.clone()),
            FieldP(strukt, _) => Some(strukt.clone()),
        }
    }
}

impl From<Var> for Place {
    fn from(var: Var) -> Self {
        Place::VarP(var)
    }
}

impl fmt::Debug for Place {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Place::VarP(v) => write!(f, "{v:?}"),
            Place::IndexP(array, index) => write!(f, "{array:?}[{index:?}]"),
            Place::FieldP(strukt, field) => write!(f, "{strukt:?}.{field:?}"),
        }
    }
}
