//! Defining instructions and branches, which are the basic units in the
//! MIR/CFG.

use std::ops::{Deref, DerefMut};

use super::*;
use crate::utils::boxed;

/// Instructions in the MIR (nodes in the CFG).
///
/// Instruction = scope + kind of instruction.
pub struct Instr<'mir, 'ctx> {
    /// Instruction kind.
    pub kind: InstrKind<'mir, 'ctx>,
    /// Codespan of the instruction.
    pub span: Span,
    /// Scope id of the instruction.
    ///
    /// This is the stratum/scope in which it is.
    pub scope: Stratum,
}

impl<'mir, 'ctx> Deref for Instr<'mir, 'ctx> {
    type Target = InstrKind<'mir, 'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}
impl<'mir, 'ctx> DerefMut for Instr<'mir, 'ctx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.kind
    }
}

impl<'mir, 'ctx> fmt::Debug for Instr<'mir, 'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} // scope {:?}", self.kind, self.scope)
    }
}

/// Instructions in the MIR (nodes in the CFG).
#[derive(Default)]
pub enum InstrKind<'mir, 'ctx> {
    /// No-op instruction.
    #[default]
    Noop,
    /// Assigns a variable.
    Assign(LhsRef<'mir, 'ctx>, RValue<'mir, 'ctx>),
    /// Performs a call to `name(args)`, putting the result in variable
    /// `destination`.
    Call {
        /// Name of the function to call.
        name: Namespaced<'ctx>,
        /// Arguments to that function.
        args: Vec<Reference<'mir, 'ctx>>,
        /// Destination variable to hold the result.
        destination: Option<LhsRef<'mir, 'ctx>>,
    },
    /// Asks for the truthiness of the first argument.
    Branch(Reference<'mir, 'ctx>),
    /// Return the value.
    ///
    /// Dummy instruction to pinpoint the liveness of the return value at the
    /// end of the function.
    Return(Pointer<'ctx>),
}

impl<'mir, 'ctx> InstrKind<'mir, 'ctx> {
    /// If this instruction mutates some variables, returns them.
    /// Otherwise, returns [`None`].
    pub fn mutated_place<'a>(&'a self) -> Box<dyn Iterator<Item = Place<'ctx>> + 'a> {
        match self {
            InstrKind::Noop | InstrKind::Branch(_) => boxed([].into_iter()),
            InstrKind::Call {
                destination: Some(v),
                ..
            } => boxed([*v.place()].into_iter()),
            InstrKind::Call {
                destination: None, ..
            } => boxed([].into_iter()),
            InstrKind::Assign(lhs, RValue::Place(rhs))
                if matches!(rhs.mode(), Mode::MutBorrowed | Mode::SMutBorrowed) =>
            {
                boxed([*lhs.place(), *rhs.place()].into_iter())
            }
            InstrKind::Assign(lhs, _) => boxed([*lhs.place()].into_iter()),
            InstrKind::Return(_) => boxed([].into_iter()),
        }
    }

    /// Returns mutable borrows into the references of this [`InstrKind`].
    pub fn references_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut Reference<'mir, 'ctx>> + 'a> {
        match self {
            InstrKind::Noop | InstrKind::Branch(_) | InstrKind::Return(_) => {
                boxed(std::iter::empty())
            }
            InstrKind::Assign(_, RValue::Place(place)) => boxed(std::iter::once(place)),
            InstrKind::Assign(_, RValue::Array(items)) => boxed(items.iter_mut()),
            InstrKind::Assign(_, RValue::Tuple(items)) => boxed(items.iter_mut()),
            InstrKind::Assign(_, RValue::Range(start, end)) => boxed([start, end].into_iter()),
            InstrKind::Assign(_, RValue::Struct { name: _, fields }) => boxed(fields.values_mut()),
            InstrKind::Assign(
                _,
                RValue::Variant {
                    enun: _,
                    variant: _,
                    args,
                },
            ) => boxed(args.iter_mut()),
            InstrKind::Call {
                name: _,
                args,
                destination: _,
            } => boxed(args.iter_mut()),
            InstrKind::Assign(
                _,
                RValue::Unit | RValue::Bool(..) | RValue::Integer(..) | RValue::String(..),
            ) => boxed(std::iter::empty()),
        }
    }

    /// Returns the references of this [`InstrKind`].
    pub fn references<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Reference<'mir, 'ctx>> + 'a> {
        match self {
            InstrKind::Noop | InstrKind::Branch(_) | InstrKind::Return(_) => {
                boxed(std::iter::empty())
            }
            InstrKind::Assign(_, RValue::Place(place)) => boxed(std::iter::once(place)),
            InstrKind::Assign(_, RValue::Array(items)) => boxed(items.iter()),
            InstrKind::Assign(_, RValue::Tuple(items)) => boxed(items.iter()),
            InstrKind::Assign(_, RValue::Range(start, end)) => boxed([start, end].into_iter()),
            InstrKind::Assign(_, RValue::Struct { name: _, fields }) => boxed(fields.values()),
            InstrKind::Assign(
                _,
                RValue::Variant {
                    enun: _,
                    variant: _,
                    args,
                },
            ) => boxed(args.iter()),
            InstrKind::Call {
                name: _,
                args,
                destination: _,
            } => boxed(args.iter()),
            InstrKind::Assign(
                _,
                RValue::Unit | RValue::Bool(..) | RValue::Integer(..) | RValue::String(..),
            ) => boxed(std::iter::empty()),
        }
    }

    /// Changes the instruction to change `to_find`'s referencing mode to
    /// [`Mode::Cloned`].
    ///
    /// # Panics
    /// Panics if the instruction does not contain `to_find`.
    pub fn force_clone(&mut self, to_find: &Pointer<'ctx>) {
        let mut els = self
            .references_mut()
            .filter(|reference| reference.as_ptr() == *to_find)
            .collect::<Vec<_>>();

        if els.len() != 1 {
            panic!(
                "Could not find {to_find:?} to clone in {self:?} ({:?} possible entries)",
                els.len()
            )
        } else {
            let el = &mut els[0];
            el.set_mode(Mode::Cloned);
        }
    }
}

impl<'mir, 'ctx> fmt::Debug for InstrKind<'mir, 'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InstrKind::Noop => write!(f, "()"),
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
pub fn instr<'mir, 'ctx>(
    kind: impl Into<InstrKind<'mir, 'ctx>>,
    scope: impl Into<Stratum>,
) -> Instr<'mir, 'ctx> {
    Instr {
        kind: kind.into(),
        span: std::default::default(),
        scope: scope.into(),
    }
}
