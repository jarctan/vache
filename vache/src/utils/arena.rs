//! Defining the arena interface used by the compiler.

use bumpalo::Bump;

use crate::utils::boxed;

/// Compiler arena.
#[derive(Debug)]
pub struct Arena(Bump);

impl Arena {
    /// Creates a new arena.
    pub fn new() -> Arena {
        Arena(Bump::new())
    }

    /// Allocates a new element in the arena.
    pub fn alloc<T>(&self, t: T) -> &T {
        self.0.alloc(boxed(t))
    }
}

impl Default for Arena {
    fn default() -> Self {
        Self::new()
    }
}
