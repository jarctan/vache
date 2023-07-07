//! Function arguments

use std::fmt;

use super::{LhsRef, Place, Pointer, Reference};
use crate::utils::boxed;

/// A function argument:
/// * [`Arg::Standard`] either a pass by value argument
/// * [`Arg::InPlace`] or a pass by reference
/// * [`Arg::Binding`] or pass by value but bind the result to some value.
pub enum Arg<'mir, 'ctx> {
    /// Standard argument: pass by value.
    Standard(Reference<'mir, 'ctx>),
    /// Pass by reference/mutate in place.
    InPlace(Reference<'mir, 'ctx>),
    /// Pass by value and bind the result to some lhs reference.
    Binding(Reference<'mir, 'ctx>, LhsRef<'mir, 'ctx>),
}
impl<'mir, 'ctx> Arg<'mir, 'ctx> {
    /// Returns the references of this [`Arg`].
    ///
    /// References are the variables used (not defined) in the [`Arg`] (with the
    /// addressing modes on them).
    pub fn references<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Reference<'mir, 'ctx>> + 'a> {
        match self {
            Arg::Standard(r) => boxed(std::iter::once(r)),
            Arg::InPlace(r) => boxed(std::iter::once(r)),
            Arg::Binding(from, _) => boxed(std::iter::once(from)),
        }
    }

    /// Returns mutable borrows into the references of this [`Arg`].
    ///
    /// References are the variables used (not defined) in the [`Arg`] (with the
    /// addressing modes on them).
    pub fn references_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut Reference<'mir, 'ctx>> + 'a> {
        match self {
            Arg::Standard(r) => boxed(std::iter::once(r)),
            Arg::InPlace(r) => boxed(std::iter::once(r)),
            Arg::Binding(from, _) => boxed(std::iter::once(from)),
        }
    }

    /// Optionally returns the pointer mutated by this argument.
    pub fn mutated_ptr(&self) -> Option<Pointer<'ctx>> {
        match self {
            Arg::Standard(_) => None,
            Arg::InPlace(r) => Some(r.as_ptr()),
            Arg::Binding(_, rhs) => Some(rhs.as_ptr()),
        }
    }

    /// Optionally returns the place mutated by this argument.
    pub fn mutated_place(&self) -> Option<Place<'ctx>> {
        self.mutated_ptr().map(|x| *x.place())
    }
}

impl<'mir, 'ctx: 'mir> fmt::Debug for Arg<'mir, 'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Arg::Standard(r) => write!(f, "{r:?}"),
            Arg::InPlace(r) => write!(f, "@{r:?}"),
            Arg::Binding(from, to) => write!(f, "{from:?}@{to:?}"),
        }
    }
}
