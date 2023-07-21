//! Function arguments

use std::fmt;

use super::{LhsRef, Place, Pointer, Reference, Span};

/// A function argument.
pub struct Arg<'mir, 'ctx> {
    /// Argument kind.
    pub kind: ArgKind<'mir, 'ctx>,
    /// Code span in the source code.
    pub span: Span,
}

/// A function argument kind:
/// * [`ArgKind::Standard`] either a pass by value argument
/// * [`ArgKind::InPlace`] or a pass by reference
/// * [`ArgKind::Binding`] or pass by value but bind the result to some value.
pub enum ArgKind<'mir, 'ctx> {
    /// Standard argument: pass by value.
    Standard(Reference<'mir, 'ctx>),
    /// Pass by reference/mutate in place.
    InPlace(Reference<'mir, 'ctx>),
    /// Pass by value and bind the result to some lhs reference.
    Binding(Reference<'mir, 'ctx>, LhsRef<'mir, 'ctx>),
}
impl<'mir, 'ctx> Arg<'mir, 'ctx> {
    /// Returns the reference of this [`Arg`].
    ///
    /// References are the variables used (not defined) in the [`Arg`] (with the
    /// addressing modes on them).
    pub fn reference<'a>(&'a self) -> &'a Reference<'mir, 'ctx> {
        match &self.kind {
            ArgKind::Standard(r) => r,
            ArgKind::InPlace(r) => r,
            ArgKind::Binding(from, _) => from,
        }
    }

    /// Returns mutable borrows into the reference of this [`Arg`].
    ///
    /// References are the variables used (not defined) in the [`Arg`] (with the
    /// addressing modes on them).
    pub fn reference_mut<'a>(&'a mut self) -> &'a mut Reference<'mir, 'ctx> {
        match &mut self.kind {
            ArgKind::Standard(r) => r,
            ArgKind::InPlace(r) => r,
            ArgKind::Binding(from, _) => from,
        }
    }

    /// Optionally returns the pointer mutated by this argument.
    pub fn mutated_ptr(&self) -> Option<Pointer<'ctx>> {
        match &self.kind {
            ArgKind::Standard(_) => None,
            ArgKind::InPlace(r) => Some(r.as_ptr()),
            ArgKind::Binding(_, rhs) => Some(rhs.as_ptr()),
        }
    }

    /// Optionally returns the place mutated by this argument.
    pub fn mutated_place(&self) -> Option<Place<'ctx>> {
        self.mutated_ptr().map(|x| *x.place())
    }
}

impl<'mir, 'ctx: 'mir> fmt::Debug for Arg<'mir, 'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ArgKind::Standard(r) => write!(f, "{r:?}"),
            ArgKind::InPlace(r) => write!(f, "@{r:?}"),
            ArgKind::Binding(from, to) => write!(f, "{from:?}@{to:?}"),
        }
    }
}
