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
    /// Swaps two values.
    SwapS(Reference<'mir, 'ctx>, Reference<'mir, 'ctx>),
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
    /// Dummy instruction to explicitly state some value might be used at some
    /// point.
    PhantomUse(Reference<'mir, 'ctx>),
}

impl<'mir, 'ctx> InstrKind<'mir, 'ctx> {
    /// Returns pointers mutated by this instruction.
    pub fn mutated_ptrs<'a>(&'a self) -> Box<dyn Iterator<Item = Pointer<'ctx>> + 'a> {
        match self {
            InstrKind::Noop | InstrKind::Branch(..) => boxed(std::iter::empty()),
            InstrKind::SwapS(place1, place2) => {
                boxed([place1.as_ptr(), place2.as_ptr()].into_iter())
            }
            InstrKind::Call {
                name: _,
                destination: lhs,
                args,
            } => {
                let lhs: Box<dyn Iterator<Item = _>> = if let Some(lhs) = lhs {
                    boxed(std::iter::once(lhs.as_ptr()))
                } else {
                    boxed(std::iter::empty())
                };
                boxed(lhs.chain(args.iter().flat_map(Arg::mutated_ptr)))
            }
            InstrKind::Assign(lhs, rval) => {
                boxed([lhs.as_ptr()].into_iter().chain(rval.mut_vars_ptrs()))
            }
            InstrKind::PhantomUse(r) => {
                if r.mode().is_mutable() {
                    boxed(std::iter::once(r.as_ptr()))
                } else {
                    boxed(std::iter::empty())
                }
            }
        }
    }

    /// Returns variables mutated by this instruction.
    pub fn mutated_places<'a>(&'a self) -> Box<dyn Iterator<Item = Place<'ctx>> + 'a> {
        boxed(self.mutated_ptrs().map(|r| *r.place()))
    }

    /// Returns mutable borrows into the references of this [`InstrKind`].
    ///
    /// References are the variable uses + the addressing modes on them.
    pub fn references_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut Reference<'mir, 'ctx>> + 'a> {
        match self {
            InstrKind::Noop => boxed(std::iter::empty()),
            InstrKind::Branch(r) | InstrKind::PhantomUse(r) => boxed(std::iter::once(r)),
            InstrKind::Assign(_, rval) => rval.references_mut(),
            InstrKind::SwapS(place1, place2) => boxed([place1, place2].into_iter()),
            InstrKind::Call {
                name: _,
                args,
                destination: _,
            } => boxed(args.iter_mut().map(Arg::reference_mut)),
        }
    }

    /// Returns the references of this [`InstrKind`].
    ///
    /// References are the variable uses + the addressing modes on them.
    pub fn references<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Reference<'mir, 'ctx>> + 'a> {
        match self {
            InstrKind::Noop => boxed(std::iter::empty()),
            InstrKind::Branch(r) | InstrKind::PhantomUse(r) => boxed(std::iter::once(r)),
            InstrKind::Assign(_, rval) => rval.references(),
            InstrKind::SwapS(place1, place2) => boxed([place1, place2].into_iter()),
            InstrKind::Call {
                name: _,
                args,
                destination: _,
            } => boxed(args.iter().map(Arg::reference)),
        }
    }

    /// Returns a mutable handle into the [`Reference`] that matches `to_find`.
    ///
    /// # Panics
    /// Panics if the instruction does not contain `to_find`.
    pub(crate) fn find(&mut self, to_find: &Pointer<'ctx>) -> &mut Reference<'mir, 'ctx> {
        let els = self
            .references_mut()
            .filter(|reference| reference.as_ptr() == *to_find)
            .collect::<Vec<_>>();

        assert_eq!(
            els.len(),
            1,
            "Could not find {to_find:?} to clone {:?} possible entries)",
            els.len()
        );

        els.into_iter().next().unwrap()
    }

    /// Changes the instruction to change `to_find`'s referencing mode to
    /// [`Mode::Cloned`].
    ///
    /// # Panics
    /// Panics if the instruction does not contain `to_find`.
    pub(crate) fn force_clone(&mut self, to_find: &Pointer<'ctx>) {
        self.find(to_find).set_mode(Mode::Cloned);
    }
}

impl<'mir, 'ctx> fmt::Debug for InstrKind<'mir, 'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InstrKind::Noop => write!(f, "()"),
            InstrKind::Assign(lhs, rhs) => write!(f, "{lhs:?} = {rhs:?}"),
            InstrKind::SwapS(place1, place2) => write!(f, "{place1:?} <-> {place2:?}"),
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
            InstrKind::PhantomUse(r) => {
                write!(f, "use {r:?}")
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
