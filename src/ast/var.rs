use std::fmt;

use super::{Stratum, ty::Ty};

/// A variable in the code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var(String);

impl AsRef<Var> for Var {
    fn as_ref(&self) -> &Var {
        self
    }
}

impl From<Var> for String {
    fn from(value: Var) -> Self {
        value.0
    }
}

impl From<String> for Var {
    fn from(v: String) -> Self {
        Self(v.to_string())
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A variable definition.
///
/// The definition is accompanied by some additional
/// metadata, like the stratum it is tied to.
#[derive(Debug, Clone)]
pub struct VarDef {
    /// Variable name.
    pub(crate) name: Var,
    /// Stratum.
    pub(crate) stratum: Stratum,
    /// Type of the variable.
    pub(crate) ty: Ty,
}
impl VarDef {
    /// Creates a new variable definition.
    pub fn from(name: impl ToString, stratum: Stratum, ty: Ty) -> Self {
        let name = name.to_string().into();
        Self { name, stratum, ty }
    }
}