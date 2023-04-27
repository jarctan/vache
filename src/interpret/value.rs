//! Representing values in the interpreter.

use std::collections::HashMap;
use std::fmt;

/// Values in our language.
#[derive(Debug)]
pub enum Value {
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnitV => write!(f, "()"),
            IntV(i) => write!(f, "{i}"),
            StrV(s) => write!(f, "{s}"),
            BoolV(b) => write!(f, "{b}"),
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
    pub stratum: usize,
    /// Key in the slab of that environment.
    pub key: usize,
}
