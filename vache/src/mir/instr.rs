//! Defining instructions and branches, which are the basic units in the
//! MIR/CFG.

use std::ops::{Deref, DerefMut};

use super::*;
use crate::utils::boxed;

/// Instructions in the MIR (nodes in the CFG).
///
/// Instruction = scope + kind of instruction.
pub struct Instr<'a> {
    /// Instruction kind.
    pub kind: InstrKind<'a>,
    /// Codespan of the instruction.
    pub span: Span,
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
        name: Namespaced<'ctx>,
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

impl<'ctx> InstrKind<'ctx> {
    /// If this instruction mutates some variables, returns them.
    /// Otherwise, returns [`None`].
    pub fn mutated_place<'a>(&'a self) -> Box<dyn Iterator<Item = Place<'ctx>> + 'a> {
        match self {
            InstrKind::Noop | InstrKind::Branch(_) => boxed([].into_iter()),
            InstrKind::Declare(..) => boxed([].into_iter()),
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
    ) -> Box<dyn Iterator<Item = &'a mut Reference<'ctx>> + 'a> {
        match self {
            InstrKind::Noop
            | InstrKind::Declare(_)
            | InstrKind::Branch(_)
            | InstrKind::Return(_) => boxed(std::iter::empty()),
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
    pub fn references<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Reference<'ctx>> + 'a> {
        match self {
            InstrKind::Noop
            | InstrKind::Declare(_)
            | InstrKind::Branch(_)
            | InstrKind::Return(_) => boxed(std::iter::empty()),
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
    pub fn force_clone(&mut self, to_find: &Loc<'ctx>) {
        let mut els = self
            .references_mut()
            .filter(|reference| reference.loc() == to_find)
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
        span: std::default::default(),
        scope: scope.into(),
    }
}
