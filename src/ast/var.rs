use std::fmt;

use super::ty::Ty;

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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarDef {
    /// Variable name.
    pub(crate) name: Var,
    /// Type of the variable.
    pub(crate) ty: Ty,
}

impl AsRef<Var> for VarDef {
    fn as_ref(&self) -> &Var {
        &self.name
    }
}

/// Creates a new variable definition.
pub fn vardef(name: impl ToString, ty: Ty) -> VarDef {
    let name = name.to_string().into();
    VarDef { name, ty }
}
