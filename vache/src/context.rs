use crate::config::Config;
use crate::utils::arena::Arena;

/// Compiler context.
pub struct Context<'ctx> {
    /// Compiler configuration.
    pub config: Config<'ctx>,
    /// Compiler arena.
    pub arena: &'ctx Arena,
}

impl<'ctx> Context<'ctx> {
    /// Creates a new compiler context.
    pub fn new(config: Config<'ctx>, arena: &'ctx Arena) -> Self {
        Self { config, arena }
    }

    /// Allocates an element in the compiler arena.
    pub fn alloc<T>(&self, t: T) -> &'ctx T {
        self.arena.alloc(t)
    }
}
