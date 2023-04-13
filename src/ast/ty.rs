use super::Stratum;

/// Types in our language.
pub enum Ty {
    /// An unbounded integer.
    Int,
    /// A borrow with a given lifetime on the object.
    Borrow(Stratum, Box<Ty>),
}