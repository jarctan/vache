//! Defining referencing modes.

use std::fmt;

/// Variable addressing modality.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Default)]
pub enum Mode {
    /// Have a reference onto that value.
    #[default]
    Borrowed,
    /// Have a shallow borrow onto that value.
    ///
    /// Shallow borrow = simply a `&mut`, not a clone of the `Cow` contents.
    SBorrowed,
    /// Have a (shallow) mutable reference onto that value.
    ///
    /// Shallow borrow = simply a `&mut`, not a clone of the `Cow` contents.
    MutBorrowed,
    /// Clone the value to own it.
    Cloned,
    /// Move the value out of its original variable.
    ///
    /// Is only safe when the original value is not used afterwards!
    Moved,
    /// Assigning to a variable.
    Assigning,
}

impl Mode {
    /// Is this a borrowing mode?
    pub fn is_borrowing(&self) -> bool {
        use Mode::*;
        // Note: we state all cases explicitly to force an error if we were to add new
        // variants.
        match self {
            Borrowed | MutBorrowed => true,
            SBorrowed => false, // Note: a shallow borrow is not considered borrowing.
            Cloned | Moved | Assigning => false,
        }
    }
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Mode::*;
        match self {
            Borrowed => write!(f, "&_"),
            SBorrowed => write!(f, "&"),
            MutBorrowed => write!(f, "&mut "),
            Cloned => write!(f, "^"),
            Moved => write!(f, "!"),
            Assigning => write!(f, "@"),
        }
    }
}
