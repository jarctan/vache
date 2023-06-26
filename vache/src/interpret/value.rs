//! Representing values in the interpreter.

use std::collections::HashMap;
use std::fmt;

use crate::tast::Stratum;

/// Values in our language.
pub enum Value<'ctx> {
    /// Unit value.
    UnitV,
    /// Integer value.
    IntV(num_bigint::BigInt),
    /// String value.
    StrV(String),
    /// Boolean value.
    BoolV(bool),
    /// Structure value.
    ///
    /// `StructV(name, fields)`
    ///
    /// We keep the name to display structures nicely in the end.
    StructV(&'ctx str, HashMap<&'ctx str, ValueRef>),
    /// Array.
    ArrayV(Vec<ValueRef>),
    /// Tuple.
    TupleV(Vec<ValueRef>),
    /// Variant.
    VariantV {
        /// Enumerated type from which the variant originates.
        enun: &'ctx str,
        /// Variant name.
        variant: &'ctx str,
        /// Variant arguments.
        args: Vec<ValueRef>,
    },
    /// A range between `start` and `end`.
    RangeV(ValueRef, ValueRef),
}

use Value::*;

impl fmt::Debug for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnitV => write!(f, "()"),
            IntV(i) => fmt::Display::fmt(i, f),
            StrV(s) => write!(f, "\"{s}\""),
            BoolV(b) => fmt::Display::fmt(b, f),
            RangeV(start, end) => write!(f, "{start:?}..{end:?}"),
            StructV(name, fields) => {
                let mut display = f.debug_struct(name);
                for (s, v) in fields {
                    display.field(s, v);
                }
                display.finish()
            }
            ArrayV(array) => f.debug_list().entries(array).finish(),
            TupleV(items) => {
                let mut display = f.debug_tuple("");
                for item in items {
                    display.field(item);
                }
                display.finish()
            }
            VariantV {
                enun,
                variant,
                args,
            } => {
                write!(f, "{enun}::")?;
                let mut display = f.debug_tuple(variant);
                for arg in args {
                    display.field(arg);
                }
                display.finish()
            }
        }
    }
}

impl Value<'_> {
    /// Truthiness of the value.
    pub fn truth(&self) -> bool {
        if let BoolV(b) = self {
            *b
        } else {
            panic!("Runtime error: Requesting the truth value of something which is not a boolean")
        }
    }
}

/// A reference to a value.
#[derive(Clone, Copy, Debug)]
pub struct ValueRef {
    /// Stratum/ environment number in which the value resides.
    pub stratum: Stratum,
    /// Key in the slab of that environment.
    pub key: usize,
}
