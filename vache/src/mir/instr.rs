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
        args: Vec<Arg<'mir, 'ctx>>,
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
    /// Returns variables mutated by this instruction.
    pub fn mutated_places<'a>(&'a self) -> Box<dyn Iterator<Item = Place<'ctx>> + 'a> {
        // Mutated places are either lhs references...
        let obvious_lhs: Box<dyn Iterator<Item = _>> = match self {
            InstrKind::Noop | InstrKind::Branch(..) | InstrKind::Return(_) => {
                boxed(std::iter::empty())
            }
            InstrKind::Call {
                name: _,
                destination: lhs,
                args,
            } => {
                let lhs: Box<dyn Iterator<Item = _>> = if let Some(lhs) = lhs {
                    boxed(std::iter::once(*lhs.place()))
                } else {
                    boxed(std::iter::empty())
                };
                boxed(lhs.chain(args.iter().flat_map(Arg::mutated_place)))
            }
            InstrKind::Assign(lhs, rval) => {
                boxed([*lhs.place()].into_iter().chain(rval.mut_vars()))
            }
        };

        // ...or mutably borrowed rhs references
        let rhs_refs = self
            .references()
            .filter(|r| r.mode().is_mutable())
            .map(|r| *r.place());

        boxed(obvious_lhs.chain(rhs_refs))
    }

    /// Returns mutable borrows into the references of this [`InstrKind`].
    ///
    /// References are the variable uses + the addressing modes on them.
    pub fn references_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut Reference<'mir, 'ctx>> + 'a> {
        match self {
            InstrKind::Noop | InstrKind::Return(_) => boxed(std::iter::empty()),
            InstrKind::Branch(r) => boxed(std::iter::once(r)),
            InstrKind::Assign(_, rval) => rval.references_mut(),
            InstrKind::Call {
                name: _,
                args,
                destination: _,
            } => boxed(args.iter_mut().flat_map(|arg| arg.references_mut())),
        }
    }

    /// Returns the references of this [`InstrKind`].
    ///
    /// References are the variable uses + the addressing modes on them.
    pub fn references<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Reference<'mir, 'ctx>> + 'a> {
        match self {
            InstrKind::Noop | InstrKind::Return(_) => boxed(std::iter::empty()),
            InstrKind::Branch(r) => boxed(std::iter::once(r)),
            InstrKind::Assign(_, rval) => rval.references(),
            InstrKind::Call {
                name: _,
                args,
                destination: _,
            } => boxed(args.iter().flat_map(|arg| arg.references())),
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

        assert_eq!(
            els.len(),
            1,
            "Could not find {to_find:?} to clone in {self:?} ({:?} possible entries)",
            els.len()
        );
        els[0].set_mode(Mode::Cloned);
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
