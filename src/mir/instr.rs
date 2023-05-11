//! Defining instructions, which are the basic units in the MIR.

use std::collections::HashMap;

use super::*;

#[derive(Debug, PartialEq, Eq, Default, Hash, Clone)]
pub enum Branch {
    TrueB,
    FalseB,
    #[default]
    DefaultB,
}

/// Instructions in the MIR (nodes in the CFG).
#[derive(Default)]
pub enum Instr {
    /// No-op instruction.
    #[default]
    Noop,
    /// Declare a new, uninitialized variable.
    Declare(VarDef),
    /// Assigns a variable.
    Assign(Var, RValue),
    /// Performs a call to `name(args)`, putting the result in variable
    /// `destination`.
    Call {
        /// Name of the function to call.
        name: String,
        /// Arguments to that function.
        args: Vec<Var>,
        /// Destination variable to hold the result.
        destination: VarDef,
    },
    /// Structure instantiation.
    Struct {
        /// Name of the structure to instantiate.
        name: String,
        /// Name of the structure to instantiate.
        fields: HashMap<String, Var>,
        /// Destination variable to hold the instantiated structure.
        destination: VarDef,
    },
    /// Asks for the truthiness of the first argument.
    Branch(Var),
    PushScope,
    PopScope,
}

impl fmt::Debug for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Noop => write!(f, "()"),
            Instr::Declare(x) => write!(f, "new {x:?}"),
            Instr::Assign(lhs, rhs) => write!(f, "{lhs:?} = {rhs:?}"),
            Instr::Call {
                name,
                args,
                destination,
            } => write!(f, "{destination:?} = {name}({args:?})"),
            Instr::Struct {
                name,
                fields,
                destination,
            } => {
                write!(f, "{destination:?} = ")?;
                let mut res = f.debug_struct(name);
                for (name, var) in fields {
                    res.field(name, var);
                }
                res.finish()
            }
            Instr::Branch(cond) => {
                write!(f, "{cond:?}?")
            }
            Instr::PushScope => write!(f, "PushScope"),
            Instr::PopScope => write!(f, "PopScope"),
        }
    }
}
