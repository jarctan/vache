use super::Stratum;

/// A variable in the code.
pub struct Var(String);

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
    name: String,
    /// Stratum.
    stratum: Stratum,
}
impl VarDef {
    ///
    pub fn from(name: impl ToString, stratum: Stratum) -> Self {
        let name = name.to_string();
        Self { name, stratum }
    }
}
