//! Defining pointers, which are (unsurprisingly) variables that can reference
//! into places.

use std::fmt;
use std::sync::atomic::AtomicU64;

use super::{Loc, Place, Reference, Span};
use crate::Arena;

/// Fresh label counter.
///
/// Global to avoid any confusion between label names.
pub static LABEL_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Pointer.
///
/// Pointer are higher-level variables that can point into a value.
#[derive(PartialEq, Eq, Copy, Clone, Hash)]
pub struct Pointer<'ctx> {
    /// Pointer id. Kind of a variable name.
    pub(crate) id: u64,
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
        let id = LABEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let loc = arena.alloc(place.root());
        Self {
            id,
            place,
            loc,
            span,
        }
    }

    /// Gets the place of the pointer.
    pub fn place(&self) -> &'ctx Place<'ctx> {
        self.place
    }
}

impl fmt::Debug for Pointer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "点{}@{:?}", self.id, self.place)
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
