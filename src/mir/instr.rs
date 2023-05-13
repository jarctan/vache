//! Defining instructions and branches, which are the basic units in the
//! MIR/CFG.

use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

use super::Stratum;
use super::*;

/// Branch = label on CFG edges.
///
/// A branch is label that indicates the conditions to follow that path/jump to
/// the target node of that edge.
///
/// Example: an If statement has an outgoing edge to two other nodes. One of
/// them is labeled wit `TrueB`, the other `FalseB`.
///
/// The unconditional jump is `DefaultB`.
#[derive(Debug, PartialEq, Eq, Default, Hash, Clone)]
pub enum Branch {
    /// Branch if true.
    TrueB,
    /// Branch if false.
    FalseB,
    /// Default, always branch.
    #[default]
    DefaultB,
}

/// Instructions in the MIR (nodes in the CFG).
///
/// Instruction = scope + kind of instruction.
#[derive(Debug)]
pub struct Instr {
    /// Scope id of the instruction.
    ///
    /// This is the stratum/scope in which it is.
    pub scope: Stratum,
    /// Instruction kind.
    pub kind: InstrKind,
}

impl Deref for Instr {
    type Target = InstrKind;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}
impl DerefMut for Instr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.kind
    }
}

/// Instructions in the MIR (nodes in the CFG).
#[derive(Default)]
pub enum InstrKind {
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
        args: Vec<VarMode>,
        /// Destination variable to hold the result.
        destination: Option<Var>,
    },
    /// Structure instantiation.
    Struct {
        /// Name of the structure to instantiate.
        name: String,
        /// Name of the structure to instantiate.
        fields: HashMap<String, VarMode>,
        /// Destination variable to hold the instantiated structure.
        destination: Option<Var>,
    },
    /// Asks for the truthiness of the first argument.
    Branch(Var),
}

impl InstrKind {
    /// If this instruction mutates a variable, returns it.
    /// Otherwise, returns `None`.
    pub fn mutated_var(&self) -> Option<&Var> {
        match self {
            InstrKind::Noop | InstrKind::Branch(_) => None,
            InstrKind::Declare(v) => Some(&v.name),
            InstrKind::Call { destination: v, .. } | InstrKind::Struct { destination: v, .. } => {
                v.as_ref()
            }
            InstrKind::Assign(v, _) => Some(v),
        }
    }

    /// If this instruction mutates variable `v`, it will modify its ownership
    /// modality as "owned".
    pub fn state_as_owned(&mut self, v: &Var) {
        match self {
            InstrKind::Noop | InstrKind::Branch(_) | InstrKind::Declare(_) => {
                panic!("{v:?} not found in this instruction, cannot make it owned")
            }
            InstrKind::Call { args, .. } => {
                let var = args.iter_mut().find(|arg| &arg.var == v).unwrap();
                var.owned = true;
            }
            InstrKind::Struct { fields, .. } => {
                let var = fields.values_mut().find(|arg| &arg.var == v).unwrap();
                var.owned = true;
            }
            InstrKind::Assign(_, RValue::Var(rhs))
            | InstrKind::Assign(_, RValue::Field(rhs, _))
                if &rhs.var == v =>
            {
                rhs.owned = true;
            }
            InstrKind::Assign(_, _) => {
                panic!("{v:?} not found in this instruction, cannot make it owned")
            }
        }
    }
}

impl fmt::Debug for InstrKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InstrKind::Noop => write!(f, "()"),
            InstrKind::Declare(x) => write!(f, "new {x:?}"),
            InstrKind::Assign(lhs, rhs) => write!(f, "{lhs:?} = {rhs:?}"),
            InstrKind::Call {
                name,
                args,
                destination,
            } => {
                if let Some(destination) = destination {
                    write!(f, "{destination:?} = ")?;
                }
                write!(f, "{name}({args:?})")
            }
            InstrKind::Struct {
                name,
                fields,
                destination,
            } => {
                if let Some(destination) = destination {
                    write!(f, "{destination:?} = ")?;
                }
                let mut res = f.debug_struct(name);
                for (name, var) in fields {
                    res.field(name, var);
                }
                res.finish()
            }
            InstrKind::Branch(cond) => {
                write!(f, "{cond:?}?")
            }
        }
    }
}
