use std::fmt;

/// Variable addressing modality.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum LhsMode {
    /// Assigning to a variable.
    Assigning,
    /// Declaring a variable.
    Declaring,
}

use LhsMode::*;

impl fmt::Display for LhsMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Assigning => write!(f, "*"),
            Declaring => write!(f, "$"),
        }
    }
}
