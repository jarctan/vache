//! Defining right values in the MIR.

use std::collections::HashMap;
use std::fmt;

use rug::Integer;

use super::*;

/// Variable addressing modality.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Default)]
pub enum Mode {
    /// Have a reference onto that value.
    #[default]
    Borrowed,
    /// Have a mutable reference onto that value.
    MutBorrowed,
    /// Clone the value to own it.
    Cloned,
    /// Move the value out of its original variable.
    ///
    /// Is only safe when the original value is not used afterwards!
    Moved,
}

impl Mode {
    /// Is this a borrowing mode?
    pub fn is_borrowing(&self) -> bool {
        use Mode::*;
        // Note: we state all cases explicitly to force an error if we were to add new
        // variants.
        match self {
            Borrowed | MutBorrowed => true,
            Cloned | Moved => false,
        }
    }
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Mode::*;
        match self {
            Borrowed => write!(f, "&"),
            MutBorrowed => write!(f, "&mut "),
            Cloned => write!(f, "^"),
            Moved => write!(f, "!"),
        }
    }
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
    /// Constructor.
    pub fn new(var: impl Into<Var>, mode: Mode) -> Self {
        Self {
            var: var.into(),
            mode,
        }
    }

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
        write!(f, "{}{}", self.mode, self.var)
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
    /// Index into an array/map.
    Index(VarMode, VarMode, Mode),
    /// Structure instantiation.
    Struct {
        /// Name of the structure to instantiate.
        name: String,
        /// Value for each field.
        fields: HashMap<String, VarMode>,
    },
    /// Array creation.
    Array(Vec<VarMode>),
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
            Index(array, index, mode) => write!(f, "{mode}{array:?}[{index:?}]"),
            Struct { name, fields } => {
                let mut res = f.debug_struct(name);
                for (name, var) in fields {
                    res.field(name, var);
                }
                res.finish()
            }
            Array(array) => f.debug_list().entries(array).finish(),
        }
    }
}

impl From<()> for RValue {
    fn from(_: ()) -> Self {
        Self::Unit
    }
}

impl From<rug::Integer> for RValue {
    fn from(value: rug::Integer) -> Self {
        Self::Integer(value)
    }
}

impl From<String> for RValue {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<VarMode> for RValue {
    fn from(var: VarMode) -> Self {
        Self::Var(var)
    }
}
