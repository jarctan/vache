//! Defining right values in the MIR.

use std::fmt;

use rug::Integer;

use super::*;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Default)]
pub enum Mode {
    #[default]
    Borrowed,
    Cloned,
    Moved,
}

/// An variable with an ownership modality.
#[derive(Clone, PartialEq, Eq)]
pub struct VarMode {
    /// The variable.
    pub var: Var,
    /// Do we transfer ownership or take by reference?
    pub mode: Mode,
}

impl VarMode {
    /// A var that is taken by reference.
    pub fn refed(var: impl Into<Var>) -> Self {
        Self {
            var: var.into(),
            mode: Mode::Borrowed,
        }
    }

    /// A var that is takes ownership of the value by cloning it.
    pub fn cloned(var: impl Into<Var>) -> Self {
        Self {
            var: var.into(),
            mode: Mode::Cloned,
        }
    }
}

impl fmt::Debug for VarMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Mode::*;
        match self.mode {
            Cloned => write!(f, "^{}", self.var),
            Moved => write!(f, "!{}", self.var),
            Borrowed => write!(f, "&{}", self.var),
        }
    }
}

impl AsRef<Var> for VarMode {
    fn as_ref(&self) -> &Var {
        &self.var
    }
}

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
    Var(VarMode),
    /// A field in a structure.
    Field(VarMode, String),
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
