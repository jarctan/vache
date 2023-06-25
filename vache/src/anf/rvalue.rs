//! Defining right values in the MIR.

use std::collections::HashMap;
use std::fmt;

use num_bigint::BigInt;

use super::*;

/// Possible right values in the CFG.
#[derive(PartialEq, Eq)]
pub enum RValue<'mir, 'ctx> {
    /// Unit expression, that does nothing.
    Unit,
    /// A boolean.
    Bool(bool),
    /// An unbounded integer.
    Integer(&'mir BigInt),
    /// A string.
    String(&'ctx str),
    /// A place.
    Place(Reference<'mir, 'ctx>),
    /// Structure instantiation.
    Struct {
        /// Name of the structure to instantiate.
        name: &'ctx str,
        /// Value for each field.
        fields: HashMap<&'ctx str, Reference<'mir, 'ctx>>,
    },
    /// Arrays.
    Array(Vec<Reference<'mir, 'ctx>>),
    /// Tuples.
    Tuple(Vec<Reference<'mir, 'ctx>>),
    /// Range.
    ///
    /// Format: `Range(start, end).`
    Range(Reference<'mir, 'ctx>, Reference<'mir, 'ctx>),
    /// Enum variant.
    Variant {
        /// Enumerated type from which the variant originates.
        enun: &'ctx str,
        /// Variant name.
        variant: &'ctx str,
        /// Variant arguments.
        args: Vec<Reference<'mir, 'ctx>>,
    },
}

impl<'mir, 'ctx> fmt::Debug for RValue<'mir, 'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use RValue::*;
        match self {
            Unit => write!(f, "()"),
            Bool(b) => write!(f, "{b}"),
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
            Tuple(items) => {
                let mut display = f.debug_tuple("");
                for item in items {
                    display.field(item);
                }
                display.finish()
            }
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

impl<'mir, 'ctx> From<()> for RValue<'mir, 'ctx> {
    fn from(_: ()) -> Self {
        Self::Unit
    }
}

impl<'mir, 'ctx> From<&'ctx BigInt> for RValue<'mir, 'ctx> {
    fn from(value: &'ctx BigInt) -> Self {
        Self::Integer(value)
    }
}

impl<'mir, 'ctx> From<&'ctx str> for RValue<'mir, 'ctx> {
    fn from(value: &'ctx str) -> Self {
        Self::String(value)
    }
}
