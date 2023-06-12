//! Defining instructions and branches, which are the basic units in the
//! MIR/CFG.

use std::ops::{Deref, DerefMut};

use super::*;
use crate::utils::boxed;

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
pub struct Instr<'a> {
    /// Instruction kind.
    pub kind: InstrKind<'a>,
    /// Scope id of the instruction.
    ///
    /// This is the stratum/scope in which it is.
    pub scope: Stratum,
}

impl<'a> Deref for Instr<'a> {
    type Target = InstrKind<'a>;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}
impl<'a> DerefMut for Instr<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.kind
    }
}

impl<'a> fmt::Debug for Instr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} // scope {:?}", self.kind, self.scope)
    }
}

/// Instructions in the MIR (nodes in the CFG).
#[derive(Default)]
pub enum InstrKind<'ctx> {
    /// No-op instruction.
    #[default]
    Noop,
    /// Declare a new, uninitialized variable.
    Declare(VarDef<'ctx>),
    /// Assigns a variable.
    Assign(Pointer<'ctx>, RValue<'ctx>),
    /// Performs a call to `name(args)`, putting the result in variable
    /// `destination`.
    Call {
        /// Name of the function to call.
        name: &'ctx str,
        /// Arguments to that function.
        args: Vec<Reference<'ctx>>,
        /// Destination variable to hold the result.
        destination: Option<Pointer<'ctx>>,
    },
    /// Asks for the truthiness of the first argument.
    Branch(Reference<'ctx>),
    /// Return the value.
    ///
    /// Dummy instruction to pinpoint the liveness of the return value at the
    /// end of the function.
    Return(Pointer<'ctx>),
}

impl<'cfg> InstrKind<'cfg> {
    /// If this instruction mutates a variable, returns it.
    /// Otherwise, returns `None`.
    pub fn mutated_place<'a>(&'a self) -> Box<dyn Iterator<Item = Place<'cfg>> + 'a> {
        match self {
            InstrKind::Noop | InstrKind::Branch(_) => boxed([].into_iter()),
            InstrKind::Declare(..) => boxed([].into_iter()),
            InstrKind::Call {
                destination: Some(v),
                ..
            } => boxed([v.place()].into_iter()),
            InstrKind::Call {
                destination: None, ..
            } => boxed([].into_iter()),
            InstrKind::Assign(lhs, RValue::Place(rhs)) if rhs.mode() == Mode::MutBorrowed => {
                boxed([lhs.place(), rhs.place()].into_iter())
            }
            InstrKind::Assign(lhs, _) => boxed([lhs.place()].into_iter()),
            InstrKind::Return(_) => boxed([].into_iter()),
        }
    }

    /// Changes the instruction to force cloning `place` inside that
    /// instruction.
    ///
    /// # Panics
    /// Panics if the instruction does not contain `place`.
    pub fn force_clone(&mut self, to_find: &Place) {
        match self {
            InstrKind::Assign(_, RValue::Place(reference)) => {
                assert!(to_find == &reference.place());
                *reference.mode_mut() = Mode::Cloned;
            }
            InstrKind::Call {
                name: _,
                args,
                destination: _,
            } => {
                for arg in args.iter_mut() {
                    if &arg.place() == to_find {
                        *arg.mode_mut() = Mode::Cloned;
                    }
                }
            }
            InstrKind::Assign(_, RValue::Range(start, end)) => {
                for arg in [start, end] {
                    if &arg.place() == to_find {
                        *arg.mode_mut() = Mode::Cloned;
                    }
                }
            }
            InstrKind::Assign(_, RValue::Struct { fields, .. }) => {
                for field in fields.values_mut() {
                    if &field.place() == to_find {
                        *field.mode_mut() = Mode::Cloned;
                    }
                }
            }
            InstrKind::Assign(_, RValue::Array(items)) => {
                for field in items.iter_mut() {
                    if &field.place() == to_find {
                        *field.mode_mut() = Mode::Cloned;
                    }
                }
            }
            InstrKind::Assign(_, RValue::Unit | RValue::Integer(_) | RValue::String(_)) => {
                panic!("{to_find:?} not found in this {self:?}, cannot make it owned")
            }
            InstrKind::Noop
            | InstrKind::Branch(_)
            | InstrKind::Declare(_)
            | InstrKind::Return(_) => {
                panic!("force_clone() cannot be applied here")
            }
        }
    }
}

impl<'a> fmt::Debug for InstrKind<'a> {
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
            InstrKind::Branch(cond) => {
                write!(f, "{cond:?}?")
            }
            InstrKind::Return(v) => {
                write!(f, "ret {v:?}")
            }
        }
    }
}

/// Shortcut to create a instruction of a given kind and scope.
#[cfg(test)]
pub fn instr<'a>(kind: impl Into<InstrKind<'a>>, scope: impl Into<Stratum>) -> Instr<'a> {
    Instr {
        kind: kind.into(),
        scope: scope.into(),
    }
}
