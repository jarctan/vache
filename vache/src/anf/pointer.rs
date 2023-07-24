//! Defining pointers, which are (unsurprisingly) variables that can reference
//! into places.

use std::fmt;

use super::{Loc, Place, Reference, Span, Varname};
use crate::Arena;

/// Pointer.
///
/// Pointer are higher-level variables that can point into a value.
#[derive(PartialEq, Eq, Copy, Clone, Hash)]
pub struct Pointer<'ctx> {
    /// Location where resides the pointer.
    pub(crate) place: &'ctx Place<'ctx>,
    /// Location pointed at by the pointer.
    pub(crate) loc: &'ctx Loc<'ctx>,
    /// Related span in the source code.
    pub(crate) span: Span,
}

impl<'ctx> Pointer<'ctx> {
    /// Creates a new pointer.
    pub fn new(arena: &'ctx Arena<'ctx>, place: &'ctx Place<'ctx>, span: Span) -> Self {
        let loc = arena.alloc(place.root());
        Self { place, loc, span }
    }

    /// Gets the place of the pointer.
    pub fn place(&self) -> &'ctx Place<'ctx> {
        self.place
    }

    /// Creates a fresh pointer to a given variable.
    pub(crate) fn from_var(
        arena: &'ctx Arena<'ctx>,
        var: Varname<'ctx>,
        span: Span,
    ) -> Pointer<'ctx> {
        Self::new(arena, arena.alloc(var.into()), span)
    }
}

impl fmt::Debug for Pointer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.place)
    }
}

impl<'ctx> Pointer<'ctx> {
    /// Location pointed at by the pointer.
    pub fn loc<'a>(&'a self) -> &'ctx Loc<'ctx> {
        self.loc
    }
}

impl<'a, 'mir, 'ctx> From<&'a Reference<'mir, 'ctx>> for Pointer<'ctx> {
    fn from(value: &'a Reference<'mir, 'ctx>) -> Self {
        value.as_ptr()
    }
}

impl<'a, 'ctx> From<&'a Pointer<'ctx>> for Pointer<'ctx> {
    fn from(value: &'a Pointer<'ctx>) -> Self {
        *value
    }
}
