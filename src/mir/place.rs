//! Defining places in the MIR.

use super::Var;

/// Kinds of places.
#[derive(Debug, Clone)]
pub enum Place {
    /// A mere variable.
    VarP(Var),
    /// An indexed slot into an expression.
    IndexP(Var, Var),
    /// An field in an expression.
    FieldP(Var, Var),
}

impl Place {
    /// Gets the root variable of the place.
    pub fn root(&self) -> &Var {
        use Place::*;
        match self {
            VarP(v) => v,
            IndexP(array, _) => array,
            FieldP(strukt, _) => strukt,
        }
    }
}

impl From<Var> for Place {
    fn from(var: Var) -> Self {
        Place::VarP(var)
    }
}
