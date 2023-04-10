use crate::stratum::Stratum;

pub struct Var(String);

impl<T: ToString> From<T> for Var {
    fn from(v: T) -> Self {
        Self(v.to_string())
    }
}

pub struct VarDef {
    /// Name
    name: String,
    /// Stratum
    stratum: Stratum,
}
impl VarDef {
    pub fn from(name: impl ToString, stratum: Stratum) -> Self {
        let name = name.to_string();
        Self {
            name,
            stratum,
        }
    }
}