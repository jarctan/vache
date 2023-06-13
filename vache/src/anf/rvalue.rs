//! Defining right values in the MIR.

use std::collections::HashMap;
use std::fmt;

use num_bigint::BigInt;

use super::*;

/// Possible right values in the CFG.
#[derive(PartialEq, Eq)]
pub enum RValue<'ctx> {
    /// Unit expression, that does nothing.
    Unit,
    /// An unbounded integer.
    Integer(&'ctx BigInt),
    /// A string.
    String(&'ctx str),
    /// A place.
    Place(Reference<'ctx>),
    /// Structure instantiation.
    Struct {
        /// Name of the structure to instantiate.
        name: &'ctx str,
        /// Value for each field.
        fields: HashMap<&'ctx str, Reference<'ctx>>,
    },
    /// Array creation.
    Array(Vec<Reference<'ctx>>),
    /// Range.
    ///
    /// Format: `Range(start, end).`
    Range(Reference<'ctx>, Reference<'ctx>),
    /// Enum variant.
    Variant {
        /// Enumerated type from which the variant originates.
        enun: &'ctx str,
        /// Variant name.
        variant: &'ctx str,
        /// Variant arguments.
        args: Vec<Reference<'ctx>>,
    },
}

impl<'ctx> fmt::Debug for RValue<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use RValue::*;
        match self {
            Unit => write!(f, "()"),
            Integer(i) => write!(f, "{i}"),
            String(s) => write!(f, "\"{s}\""),
            Place(place) => write!(f, "{place:?}"),
            Struct { name, fields } => {
                let mut res = f.debug_struct(name);
                for (name, var) in fields {
                    res.field(name, var);
                }
                res.finish()
            }
            Array(array) => f.debug_list().entries(array).finish(),
            Range(start, end) => write!(f, "{start:?}..{end:?}"),
            Variant {
                enun,
                variant,
                args,
            } => {
                write!(f, "{enun}::{variant}")?;

                // If there are some, display the parameters
                if !args.is_empty() {
                    write!(f, "(")?;
                    let mut iter = args.iter();
                    write!(f, "{:?}", iter.next().unwrap())?; // `args` is not empty so unwrap is ok
                    for arg in iter {
                        write!(f, ", {:?}", arg)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
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
