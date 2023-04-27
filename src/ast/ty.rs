use std::fmt;

/// Types in our language.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    /// Unit type.
    UnitT,
    /// The usual boolean type.
    BoolT,
    /// An unbounded integer.
    IntT,
    /// The string type.
    StrT,
    /// Structures.
    ///
    /// Structures are identified by their names.
    StructT(String),
}

use Ty::*;

impl Ty {
    /// Is this type `Copy` in Rust (no deep clone needed).
    pub fn copyable(&self) -> bool {
        match self {
            UnitT | BoolT => true,
            IntT | StrT | StructT(_) => false,
        }
    }
}

impl Default for Ty {
    /// The default type: the unit type.
    fn default() -> Self {
        Self::UnitT
    }
}

impl Extend<Ty> for Ty {
    fn extend<T: IntoIterator<Item = Ty>>(&mut self, iter: T) {
        // The type of a collection of types is the last type.
        if let Some(ty) = iter.into_iter().last() {
            *self = ty;
        }
    }
}

impl Extend<()> for Ty {
    fn extend<T: IntoIterator<Item = ()>>(&mut self, _: T) {}
}

impl From<Ty> for () {
    fn from(_: Ty) -> Self {}
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnitT => write!(f, "()"),
            BoolT => write!(f, "bool"),
            IntT => write!(f, "int"),
            StrT => write!(f, "str"),
            StructT(s) => write!(f, "{s}{{}}"),
        }
    }
}
