//! Defining pointers, which are (unsurprisingly) variables that can reference
//! into places.

use std::fmt;
use std::ops::Deref;
use std::sync::atomic::AtomicU64;

use super::{Loc, Mode, Place};
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
}

impl<'ctx> Pointer<'ctx> {
    /// Creates a new pointer.
    pub fn new(arena: &'ctx Arena, place: &'ctx Place<'ctx>) -> Self {
        let id = LABEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let loc = arena.alloc(place.root());
        Self { id, place, loc }
    }

    /// Gets the place of the pointer.
    pub fn place(&self) -> Place<'ctx> {
        *self.place
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

impl<'a, 'ctx> From<&'a Reference<'ctx>> for Pointer<'ctx> {
    fn from(value: &'a Reference<'ctx>) -> Self {
        value.as_ptr()
    }
}

impl<'a, 'ctx> From<&'a Pointer<'ctx>> for Pointer<'ctx> {
    fn from(value: &'a Pointer<'ctx>) -> Self {
        *value
    }
}

/// A reference: a pointer and its referencing mode, as in Rust.
///
/// Note: We have more than the mode: we actually have a mutable into the
/// reference mode in the typed AST, so we can change this mode. Although if
/// `mode` is `None`, then the reference is moved and is a MIR-introduced one.
#[derive(PartialEq, Eq, Hash)]
pub struct Reference<'ctx> {
    /// Pointer into the location.
    pointer: Pointer<'ctx>,
    /// Do we transfer ownership or take by reference?
    ///
    /// If `None`, the mode is `Moved`.
    mode: Option<&'ctx mut Mode>,
}

impl<'ctx> Reference<'ctx> {
    /// Creates a new reference, with its pointer and mode.
    pub fn new(ptr: Pointer<'ctx>, mode: &'ctx mut Mode) -> Self {
        Self {
            pointer: ptr,
            mode: Some(mode),
        }
    }

    /// Create a new move ref into that pointer.
    pub fn new_moved(ptr: Pointer<'ctx>) -> Self {
        Self {
            pointer: ptr,
            mode: None,
        }
    }

    /// Reference mode.
    pub fn mode(&self) -> Mode {
        self.mode.as_ref().map(|mode| **mode).unwrap_or(Mode::Moved)
    }

    /// Gets a mutable reference into the mode.
    ///
    /// # Panics
    /// Panics if the reference has no associated mode in the AST.
    pub fn mode_mut(&mut self) -> &mut Mode {
        self.mode.as_mut().unwrap()
    }

    /// Makes
    pub fn make_moved(&mut self) {
        if let Some(mode) = &mut self.mode {
            **mode = Mode::Moved;
        } else {
            // Already moved by default, nothing to do.
        }
    }

    /// Sees self as a pointer, forgetting the mode.
    pub fn as_ptr(&self) -> Pointer<'ctx> {
        self.pointer
    }
}

impl<'ctx> Deref for Reference<'ctx> {
    type Target = Pointer<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.pointer
    }
}

impl fmt::Debug for Reference<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref mode) = self.mode {
            write!(f, "{}{:?}", mode, self.pointer)
        } else {
            write!(f, "{:?}", self.pointer)
        }
    }
}
