use std::{borrow::Cow, fmt};

use super::{ty::Ty, Stratum};

/// A variable in the code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var(String);

impl Var {
    pub(super) fn subst_var(self, x: impl AsRef<Var>, with: Cow<Var>) -> Self {
        if x.as_ref() == &self {
            with.into_owned()
        } else {
            self
        }
    }
}

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

impl From<&str> for Var {
    fn from(v: &str) -> Self {
        Self(v.to_owned())
    }
}

impl From<String> for Var {
    fn from(v: String) -> Self {
        Self(v)
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarDef {
    /// Variable name.
    pub(crate) name: Var,
    /// Stratum.
    pub(crate) stratum: Stratum,
    /// Type of the variable.
    pub(crate) ty: Ty,
}

impl VarDef {
    /// Substitutes a stratum variable with a concrete stratum in the var definition.
    pub fn subst_stm(self, x: Stratum, with: Stratum) -> Self {
        let Self { name, stratum, ty } = self;
        VarDef {
            name,
            stratum: stratum.subst_stm(x, with),
            ty,
        }
    }
}

impl AsRef<Var> for VarDef {
    fn as_ref(&self) -> &Var {
        &self.name
    }
}

/// Creates a new variable definition.
pub fn vardef(name: impl ToString, stratum: Stratum, ty: Ty) -> VarDef {
    let name = name.to_string().into();
    VarDef { name, stratum, ty }
}
