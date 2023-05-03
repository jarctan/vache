//! Representing values in the interpreter.

use std::collections::HashMap;
use std::fmt;

use crate::tast::Stratum;

/// Values in our language.
#[derive(Default, Clone)]
pub enum Value {
    /// Uninit value.
    #[default]
    UninitV,
    /// Unit value.
    UnitV,
    /// Integer value.
    IntV(rug::Integer),
    /// String value.
    StrV(String),
    /// Boolean value.
    BoolV(bool),
    /// Structure value.
    ///
    /// `StructV(name, fields)`
    ///
    /// We keep the name to display structures nicely in the end.
    StructV(String, HashMap<String, ValueRef>),
}

use Value::*;

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UninitV => write!(f, "!"),
            UnitV => write!(f, "()"),
            IntV(i) => fmt::Display::fmt(i, f),
            StrV(s) => write!(f, "\"{s}\""),
            BoolV(b) => fmt::Display::fmt(b, f),
            StructV(name, fields) => {
                let mut display = f.debug_struct(name);
                for (s, v) in fields {
                    display.field(s, v);
                }
                display.finish()
            }
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UninitV => panic!("Runtime error: Requested to display an uninitialized value"),
            UnitV => write!(f, "()"),
            IntV(i) => fmt::Display::fmt(i, f),
            StrV(s) => fmt::Display::fmt(s, f),
            BoolV(b) => fmt::Display::fmt(b, f),
            StructV(name, _) => fmt::Display::fmt(name, f),
        }
    }
}

impl Value {
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
