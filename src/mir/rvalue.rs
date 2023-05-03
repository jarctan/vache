//! Defining right values in the MIR.

use std::fmt;

use rug::Integer;

use super::*;

/// Possible right values in the CFG.
#[derive(Clone, PartialEq, Eq)]
pub enum RValue {
    /// Unit expression, that does nothing.
    Unit,
    /// An unbounded integer.
    Integer(Integer),
    /// A string.
    String(String),
    /// A variable.
    Var(Var),
    /// A field in a structure.
    Field(Var, String),
}

impl fmt::Debug for RValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use RValue::*;
        match self {
            Unit => write!(f, "()"),
            Integer(i) => write!(f, "{i}"),
            String(s) => write!(f, "\"{s}\""),
            Var(v) => write!(f, "{v:?}"),
            Field(v, field) => write!(f, "{v:?}.{field}"),
        }
    }
}
