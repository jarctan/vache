//! Defining pointers, which are (unsurprisingly) variables that can reference
//! into places.

use std::fmt;
use std::ops::Deref;
use std::sync::atomic::AtomicU64;

use super::{Loc, Mode, Place, Span};
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

/// A reference: a pointer and its referencing mode, as in Rust.
///
/// Note: We have more than the mode: we actually have a mutable into the
/// reference mode in the typed AST, so we can change this mode. Although if
/// `mode` is `None`, then the reference is moved and is a MIR-introduced one.
#[derive(PartialEq, Eq, Hash)]
pub struct Reference<'mir, 'ctx: 'mir> {
    /// Pointer into the location.
    pointer: Pointer<'ctx>,
    /// Addressing/referencing mode.
    ///
    /// If there are multiple modes, that means that this reference is tied to
    /// multiple [`Mode`]s in the typed AST. The invariant is that they will
    /// always share the same mode, and modifying the mode of the [`Reference`]
    /// will modify the mode of all the `&'ctx mut Mode` in the vector.
    modes: Vec<&'mir mut Mode>,
}

impl<'mir, 'ctx: 'mir> Reference<'mir, 'ctx> {
    /// Creates a new [`Reference`], with its pointer and mode.
    pub fn new(ptr: Pointer<'ctx>, mode: &'mir mut Mode) -> Self {
        Self {
            pointer: ptr,
            modes: vec![mode],
        }
    }

    /// Creates a new [`Reference`], with multiple referencing modes at the same
    /// time.
    pub fn new_multi_modes(ptr: Pointer<'ctx>, modes: Vec<&'mir mut Mode>) -> Self {
        Self {
            pointer: ptr,
            modes,
        }
    }

    /// Create a new move ref into that pointer.
    pub fn new_moved(ptr: Pointer<'ctx>) -> Self {
        Self {
            pointer: ptr,
            modes: vec![],
        }
    }

    /// Addressing mode.
    pub fn mode(&self) -> Mode {
        self.modes.get(0).map(|mode| **mode).unwrap_or(Mode::Moved)
    }

    /// Returns the addressing modes of that [`Reference`], consuming `self`.
    ///
    /// Used so that modes from one [`Reference`] may go
    pub fn into_modes_mut(self) -> Vec<&'mir mut Mode> {
        self.modes
    }

    /// Gets a mutable reference into the mode.
    ///
    /// # Panics
    /// Panics if the reference has no associated mode in the AST.
    pub fn set_mode(&mut self, new_mode: Mode) {
        for mode in &mut self.modes {
            **mode = new_mode;
        }
    }

    /// Sees self as a pointer, forgetting the mode.
    pub fn as_ptr(&self) -> Pointer<'ctx> {
        self.pointer
    }
}

impl<'mir, 'ctx: 'mir> Deref for Reference<'mir, 'ctx> {
    type Target = Pointer<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.pointer
    }
}

impl<'mir, 'ctx: 'mir> fmt::Debug for Reference<'mir, 'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{:?}", self.mode(), self.pointer)
    }
}
