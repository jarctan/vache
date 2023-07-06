//! Function arguments

use std::fmt;

use super::{LhsRef, Loc, Place, Reference};
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
    /// Optional location defined (overwritten) by this argument.
    ///
    /// Remember that arguments can be of the form `a@b`, in which case we are
    /// binding (defining) the final result to b.
    pub(crate) fn def<'a>(&'a self) -> Option<Loc<'ctx>> {
        match self {
            Arg::Standard(_) => None,
            Arg::InPlace(_) => None,
            Arg::Binding(_, to) => Some(*to.loc()),
        }
    }

    /// Locations used by this argument.
    pub(crate) fn uses<'a>(&'a self) -> Box<dyn Iterator<Item = Loc<'ctx>> + 'a> {
        match self {
            Arg::Standard(r) => boxed(std::iter::once(*r.loc())),
            Arg::InPlace(r) => boxed(std::iter::once(*r.loc())),
            Arg::Binding(from, to) => boxed([*from.loc(), *to.loc()].into_iter()),
        }
    }

    /// Returns the references of this [`Arg`].
    ///
    /// References are the variable uses + the addressing modes on them.
    pub fn references<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Reference<'mir, 'ctx>> + 'a> {
        match self {
            Arg::Standard(r) => boxed(std::iter::once(r)),
            Arg::InPlace(r) => boxed(std::iter::once(r)),
            Arg::Binding(from, _) => boxed(std::iter::once(from)),
        }
    }

    /// Returns mutable borrows into the references of this [`Arg`].
    ///
    /// References are the variable uses + the addressing modes on them.
    pub fn references_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut Reference<'mir, 'ctx>> + 'a> {
        match self {
            Arg::Standard(r) => boxed(std::iter::once(r)),
            Arg::InPlace(r) => boxed(std::iter::once(r)),
            Arg::Binding(from, _) => boxed(std::iter::once(from)),
        }
    }

    /// Optionally returns the place mutated by this argument.
    pub fn mutated_place(&self) -> Option<Place<'ctx>> {
        match self {
            Arg::Standard(_) => None,
            Arg::InPlace(r) => Some(*r.place()),
            Arg::Binding(_, rhs) => Some(*rhs.place()),
        }
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
