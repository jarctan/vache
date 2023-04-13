use super::{Stratum, ty::Ty};

/// A variable in the code.
pub struct Var(String);

impl From<Var> for String {
    fn from(value: Var) -> Self {
        value.0
    }
}

impl<T: ToString> From<T> for Var {
    fn from(v: T) -> Self {
        Self(v.to_string())
    }
}

/// A variable definition.
///
/// The definition is accompanied by some additional
/// metadata, like the stratum it is tied to.
pub struct VarDef {
    /// Variable name.
    pub(crate) name: String,
    /// Stratum.
    pub(crate) stratum: Stratum,
    /// Type of the variable.
    pub(crate) ty: Ty,
}
impl VarDef {
    ///
    pub fn from(name: impl ToString, stratum: Stratum, ty: Ty) -> Self {
        let name = name.to_string();
        Self { name, stratum, ty }
    }
}
