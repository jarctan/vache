//! References: pointers + modes of action on it.

use std::fmt;
use std::ops::Deref;

use super::{LhsMode, Mode, Pointer};

/// A (right-hand side) reference: a pointer and its referencing mode, as in
/// Rust.
///
/// Note: We have more than the mode: we actually have a mutable into the
/// reference mode in the typed AST, so we can change this mode. Although if
/// `mode` is `None`, then the reference is moved and is a MIR-introduced one.
#[derive(PartialEq, Eq, Hash)]
pub struct Reference<'mir, 'ctx: 'mir> {
    /// Pointer into the location designated by this reference.
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
    pub fn new(pointer: Pointer<'ctx>, mode: &'mir mut Mode) -> Self {
        Self {
            pointer,
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

/// Left-hand side references.
pub struct LhsRef<'mir, 'ctx: 'mir> {
    /// Pointer into the location designated by this reference.
    pointer: Pointer<'ctx>,
    /// Referencing mode of the [`LhsRef`].
    ///
    /// It is a mutable reference into the typed AST, so that we can change it
    /// directly.
    ///
    /// If [`None`], is [`LhsMode::Assigning`] by default.
    mode: Option<&'mir mut LhsMode>,
}

impl<'mir, 'ctx> LhsRef<'mir, 'ctx> {
    /// Creates a new [`LhsRef`] on a given `pointer` with some explicit `mode`.
    pub fn new(pointer: Pointer<'ctx>, mode: &'mir mut LhsMode) -> Self {
        Self {
            pointer,
            mode: Some(mode),
        }
    }

    /// Sees self as a pointer, forgetting the mode.
    pub fn as_ptr(&self) -> Pointer<'ctx> {
        self.pointer
    }

    /// Creates a new [`LhsRef`] on a given `pointer` with a default `mode`
    /// mode.
    pub fn declare(pointer: Pointer<'ctx>) -> LhsRef<'mir, 'ctx> {
        Self {
            pointer,
            mode: None,
        }
    }

    /// Addressing mode.
    pub fn mode(&self) -> LhsMode {
        self.mode
            .as_ref()
            .map(|mode| **mode)
            .unwrap_or(LhsMode::Declaring)
    }
}

impl<'mir, 'ctx: 'mir> Deref for LhsRef<'mir, 'ctx> {
    type Target = Pointer<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.pointer
    }
}

impl<'mir, 'ctx: 'mir> fmt::Debug for LhsRef<'mir, 'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{:?}", self.mode(), self.pointer)
    }
}
