//! Defining instructions and branches, which are the basic units in the
//! MIR/CFG.

use std::ops::{Deref, DerefMut};

use Place::*;

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
pub struct Instr {
    /// Instruction kind.
    pub kind: InstrKind,
    /// Scope id of the instruction.
    ///
    /// This is the stratum/scope in which it is.
    pub scope: Stratum,
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

impl fmt::Debug for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} // scope {:?}", self.kind, self.scope)
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
    Assign(Place, RValue),
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
    /// Asks for the truthiness of the first argument.
    Branch(VarMode),
}

impl InstrKind {
    /// If this instruction mutates a variable, returns it.
    /// Otherwise, returns `None`.
    pub fn mutated_var(&self) -> Box<dyn Iterator<Item = &Var> + '_> {
        match self {
            InstrKind::Noop | InstrKind::Branch(_) => boxed([].into_iter()),
            InstrKind::Declare(v) => boxed([&v.name].into_iter()),
            InstrKind::Call {
                destination: Some(v),
                ..
            } => boxed([v].into_iter()),
            InstrKind::Call {
                destination: None, ..
            } => boxed([].into_iter()),
            InstrKind::Assign(VarP(v), RValue::Array(array)) => {
                let mut vec = vec![v];
                array
                    .iter()
                    .filter(|item| item.mode == Mode::MutBorrowed)
                    .map(|v| &v.var)
                    .collect_into(&mut vec);
                boxed(vec.into_iter())
            }
            InstrKind::Assign(VarP(v), RValue::Struct { name: _, fields }) => {
                let mut vec = vec![v];
                fields
                    .values()
                    .filter(|item| item.mode == Mode::MutBorrowed)
                    .map(|v| &v.var)
                    .collect_into(&mut vec);
                boxed(vec.into_iter())
            }
            InstrKind::Assign(
                VarP(lhs),
                RValue::Field(
                    VarMode {
                        var: rhs,
                        mode: Mode::MutBorrowed,
                    },
                    _,
                ),
            ) => boxed([lhs, rhs].into_iter()),
            InstrKind::Assign(
                VarP(lhs),
                RValue::Index(
                    VarMode {
                        var: rhs1,
                        mode: Mode::MutBorrowed,
                    },
                    VarMode {
                        var: rhs2,
                        mode: Mode::MutBorrowed,
                    },
                    _,
                ),
            ) => boxed([lhs, rhs1, rhs2].into_iter()),
            InstrKind::Assign(
                VarP(lhs),
                RValue::Index(
                    VarMode { var: rhs1, mode: _ },
                    VarMode {
                        var: rhs2,
                        mode: Mode::MutBorrowed,
                    },
                    Mode::MutBorrowed,
                ),
            ) => boxed([lhs, rhs1, rhs2].into_iter()),
            InstrKind::Assign(
                VarP(lhs),
                RValue::Index(
                    VarMode {
                        var: rhs,
                        mode: Mode::MutBorrowed,
                    },
                    _,
                    _,
                ),
            ) => boxed([lhs, rhs].into_iter()),
            InstrKind::Assign(
                VarP(lhs),
                RValue::Index(VarMode { var: rhs, mode: _ }, _, Mode::MutBorrowed),
            ) => boxed([lhs, rhs].into_iter()),
            InstrKind::Assign(
                VarP(lhs),
                RValue::Index(
                    _,
                    VarMode {
                        var: rhs,
                        mode: Mode::MutBorrowed,
                    },
                    _,
                ),
            ) => boxed([lhs, rhs].into_iter()),
            InstrKind::Assign(
                VarP(lhs),
                RValue::Var(VarMode {
                    var: rhs,
                    mode: Mode::MutBorrowed,
                }),
            ) => boxed([lhs, rhs].into_iter()),
            InstrKind::Assign(VarP(v), _) => boxed([v].into_iter()),
            InstrKind::Assign(IndexP(array, _), _) => boxed([array].into_iter()),
            InstrKind::Assign(_, _) => todo!(),
        }
    }

    /// Changes the instruction to force cloning `v` inside that instruction.
    ///
    /// # Panics
    /// Panics if the instruction does not contain `v`.
    pub fn force_clone(&mut self, v: &Var) {
        match self {
            InstrKind::Noop | InstrKind::Branch(_) | InstrKind::Declare(_) => {
                panic!("{v:?} not found in this instruction, cannot make it owned")
            }
            InstrKind::Call { args, .. } => {
                let var = args.iter_mut().find(|arg| &arg.var == v).unwrap();
                var.mode = Mode::Cloned;
            }
            InstrKind::Assign(_, RValue::Struct { fields, .. }) => {
                let var = fields.values_mut().find(|arg| &arg.var == v).unwrap();
                var.mode = Mode::Cloned;
            }
            InstrKind::Assign(_, RValue::Var(rhs))
            | InstrKind::Assign(_, RValue::Field(rhs, _))
                if &rhs.var == v =>
            {
                rhs.mode = Mode::Cloned;
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
            InstrKind::Branch(cond) => {
                write!(f, "{cond:?}?")
            }
        }
    }
}

/// Shortcut to create a instruction of a given kind and scope.
#[cfg(test)]
pub fn instr(kind: impl Into<InstrKind>, scope: impl Into<Stratum>) -> Instr {
    Instr {
        kind: kind.into(),
        scope: scope.into(),
    }
}
