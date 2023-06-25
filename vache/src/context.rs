//! Defining the compiler context.

use codespan_reporting::files::SimpleFile;

use crate::config::Config;
use crate::reporter::{Diagnostic, Reporter};
use crate::utils::arena::{Arena, Arenable};

/// Compiler context.
pub struct Context<'ctx> {
    /// Compiler configuration.
    pub config: Config<'ctx>,
    /// Compiler arena.
    pub arena: &'ctx Arena<'ctx>,
    /// Error reporter.
    pub reporter: Reporter<'ctx>,
    /// File representation, in part for diagnostic reporting.
    pub files: &'ctx SimpleFile<&'ctx str, &'ctx str>,
}

impl<'ctx> Context<'ctx> {
    /// Creates a new compiler context.
    pub fn new(config: Config<'ctx>, arena: &'ctx Arena<'ctx>) -> Self {
        let files = arena.alloc(SimpleFile::new(
            config.filename.unwrap_or("unknown file"),
            config.input,
        ));
        Self {
            arena,
            reporter: Reporter::new(arena, files),
            config,
            files,
        }
    }

    /// Allocates an element in the compiler arena.
    pub fn alloc<T: Arenable<'ctx>>(&self, t: T) -> &'ctx T {
        self.arena.alloc(t)
    }

    /// Allocates an element in the compiler arena.
    pub fn alloc_mut<T: Arenable<'ctx>>(&self, t: T) -> &'ctx mut T {
        self.arena.alloc_mut(t)
    }

    /// Create a new error diagnostic.
    pub fn emit(&self, diagnostic: Diagnostic) {
        self.reporter.emit(diagnostic);
    }

    /// Was there any errors so far?
    pub fn has_errors(&self) -> bool {
        self.reporter.has_errors()
    }
}
