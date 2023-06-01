//! Defining right values in the MIR.

use std::collections::HashMap;
use std::fmt;

use num_bigint::BigInt;

use super::*;

/// An variable with an ownership modality.
#[derive(PartialEq, Eq)]
pub struct VarMode<'ctx> {
    /// The variable.
    pub var: Var<'ctx>,
    /// Do we transfer ownership or take by reference?
    pub mode: &'ctx mut Mode,
}

impl<'ctx> VarMode<'ctx> {
    /// Constructor.
    pub fn new(var: impl Into<Var<'ctx>>, mode: &'ctx mut Mode) -> Self {
        Self {
            var: var.into(),
            mode,
        }
    }
}

impl fmt::Debug for VarMode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.mode, self.var)
    }
}

impl<'ctx> AsRef<Var<'ctx>> for VarMode<'ctx> {
    fn as_ref(&self) -> &Var<'ctx> {
        &self.var
    }
}

/// Possible right values in the CFG.
#[derive(PartialEq, Eq)]
pub enum RValue<'ctx> {
    /// Unit expression, that does nothing.
    Unit,
    /// An unbounded integer.
    Integer(&'ctx BigInt),
    /// A string.
    String(&'ctx str),
    /// A variable.
    Var(VarMode<'ctx>),
    /// A variable you will always want to move out.
    MovedVar(Var<'ctx>),
    /// A field in a structure.
    Field(Var<'ctx>, &'ctx str),
    /// Index into an array/map.
    Index(Var<'ctx>, Var<'ctx>),
    /// Structure instantiation.
    Struct {
        /// Name of the structure to instantiate.
        name: &'ctx str,
        /// Value for each field.
        fields: HashMap<&'ctx str, Var<'ctx>>,
    },
    /// Array creation.
    Array(Vec<Var<'ctx>>),
}

impl<'ctx> fmt::Debug for RValue<'ctx> {
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

impl<'ctx> From<()> for RValue<'ctx> {
    fn from(_: ()) -> Self {
        Self::Unit
    }
}

impl<'ctx> From<&'ctx BigInt> for RValue<'ctx> {
    fn from(value: &'ctx BigInt) -> Self {
        Self::Integer(value)
    }
}

impl<'ctx> From<&'ctx str> for RValue<'ctx> {
    fn from(value: &'ctx str) -> Self {
        Self::String(value)
    }
}

impl<'ctx> From<VarMode<'ctx>> for RValue<'ctx> {
    fn from(var: VarMode<'ctx>) -> Self {
        Self::Var(var)
    }
}

impl<'ctx> From<Var<'ctx>> for RValue<'ctx> {
    fn from(var: Var<'ctx>) -> Self {
        Self::MovedVar(var)
    }
}
