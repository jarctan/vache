//! Defining right values in the MIR.

use std::collections::HashMap;
use std::fmt;

use num_bigint::BigInt;

use super::*;

/// An variable with an ownership modality.
#[derive(PartialEq, Eq)]
pub struct VarMode<'a> {
    /// The variable.
    pub var: Var,
    /// Do we transfer ownership or take by reference?
    pub mode: &'a mut Mode,
}

impl<'a> VarMode<'a> {
    /// Constructor.
    pub fn new(var: impl Into<Var>, mode: &'a mut Mode) -> Self {
        Self {
            var: var.into(),
            mode,
        }
    }
}

impl<'a> fmt::Debug for VarMode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.mode, self.var)
    }
}

impl<'a> AsRef<Var> for VarMode<'a> {
    fn as_ref(&self) -> &Var {
        &self.var
    }
}

/// Possible right values in the CFG.
#[derive(PartialEq, Eq)]
pub enum RValue<'a> {
    /// Unit expression, that does nothing.
    Unit,
    /// An unbounded integer.
    Integer(BigInt),
    /// A string.
    String(String),
    /// A variable.
    Var(VarMode<'a>),
    /// A variable you will always want to move out.
    MovedVar(Var),
    /// A field in a structure.
    Field(Var, String),
    /// Index into an array/map.
    Index(Var, Var),
    /// Structure instantiation.
    Struct {
        /// Name of the structure to instantiate.
        name: String,
        /// Value for each field.
        fields: HashMap<String, Var>,
    },
    /// Array creation.
    Array(Vec<Var>),
}

impl<'a> fmt::Debug for RValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use RValue::*;
        match self {
            Unit => write!(f, "()"),
            Integer(i) => write!(f, "{i}"),
            String(s) => write!(f, "\"{s}\""),
            Var(v) => write!(f, "{v:?}"),
            MovedVar(v) => write!(f, "^{v}"),
            Field(v, field) => write!(f, "{v:?}.{field}"),
            Index(array, index) => write!(f, "{array:?}[{index:?}]"),
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

impl<'a> From<()> for RValue<'a> {
    fn from(_: ()) -> Self {
        Self::Unit
    }
}

impl<'a> From<BigInt> for RValue<'a> {
    fn from(value: BigInt) -> Self {
        Self::Integer(value)
    }
}

impl<'a> From<String> for RValue<'a> {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl<'a> From<VarMode<'a>> for RValue<'a> {
    fn from(var: VarMode<'a>) -> Self {
        Self::Var(var)
    }
}

impl From<Var> for RValue<'_> {
    fn from(var: Var) -> Self {
        Self::MovedVar(var)
    }
}
