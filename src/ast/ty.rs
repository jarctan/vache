use super::Stratum;
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
    /// A borrow with a given lifetime on the object.
    BorrowT(Stratum, Box<Ty>),
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

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::UnitT => write!(f, "()"),
            Ty::BoolT => write!(f, "bool"),
            Ty::IntT => write!(f, "int"),
            Ty::BorrowT(st, ty) => write!(f, "&{} {}", st, ty),
        }
    }
}

/// Type+stratum information.
///
/// Used within the TyAndStratum value.
#[derive(Debug, Clone)]
pub struct TyAndStratum {
    /// Type.
    pub ty: Ty,
    /// Stratum.
    pub stm: Stratum,
}

impl TyAndStratum {
    /// Substitutes a stratum variable with a concrete stratum in the
    /// in oneself.
    pub fn subst_var(self, v: Stratum, with: Stratum) -> Self {
        let Self { ty, stm } = self;
        Self {
            ty,
            stm: if stm == v { with } else { stm },
        }
    }
}

/// Shortcut to construct a return type with type and stratum of the returned value.
pub fn ret_ty(ty: Ty, stm: Stratum) -> TyAndStratum {
    TyAndStratum { ty, stm }
}

impl Extend<()> for TyAndStratum {
    fn extend<T: IntoIterator<Item = ()>>(&mut self, _iter: T) {}
}

// Currently required by the trait bounds on associated types on the Visitor trait
impl From<TyAndStratum> for () {
    fn from(_: TyAndStratum) -> Self {}
}

impl From<TyAndStratum> for (Ty, Stratum) {
    fn from(value: TyAndStratum) -> Self {
        (value.ty, value.stm)
    }
}

impl From<(Ty, Stratum)> for TyAndStratum {
    fn from((ty, stm): (Ty, Stratum)) -> Self {
        Self { ty, stm }
    }
}
