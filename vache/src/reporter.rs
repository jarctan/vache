//! User-facing error reporting facility.

use std::default::default;

use codespan_reporting::diagnostic::Severity;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use crate::Arena;

/// A compiler diagnostic.
pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<()>;

/// Collection of compiler diagnostics, ready to be displayed.
pub struct Diagnostics<'ctx> {
    /// Stream to which we will display the diagnostics.
    writer: &'ctx StandardStream,
    /// Terminal configuration.
    ///
    /// Needed to display the diagnostics to the stdout.
    config: &'ctx term::Config,
    /// Reference into the original file/source code.
    ///
    /// Needed to display the diagnostics to the stdout.
    files: &'ctx SimpleFile<&'ctx str, &'ctx str>,
    /// The actual list of diagnostics.
    diagnostics: Vec<Diagnostic>,
    /// True iff `self.diagnostics` contains at least one error diagnostic.
    is_error: bool,
}

impl<'ctx> Diagnostics<'ctx> {
    /// Displays all the diagnostics with nice colors and formatting to the
    /// standard output.
    pub fn display(&self) -> anyhow::Result<()> {
        for diagnostic in &self.diagnostics {
            term::emit(&mut self.writer.lock(), self.config, self.files, diagnostic)?;
        }
        Ok(())
    }

    /// Pushes a new diagnostic to the list.
    pub fn push(&mut self, diagnostic: Diagnostic) {
        self.is_error |= matches!(diagnostic.severity, Severity::Error | Severity::Bug);
        self.diagnostics.push(diagnostic);
    }

    /// Flushes all diagnostics and returns them.
    fn flush(&mut self) -> Diagnostics<'ctx> {
        let flushed = Diagnostics {
            writer: self.writer,
            config: self.config,
            files: self.files,
            diagnostics: std::mem::take(self.diagnostics.as_mut()),
            is_error: self.is_error,
        };

        // Revert all flags.
        self.is_error = false;

        flushed
    }

    /// Returns an iterator over the diagnostics.
    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics.iter()
    }
}

impl<'ctx> IntoIterator for Diagnostics<'ctx> {
    type IntoIter = ::std::vec::IntoIter<Diagnostic>;
    type Item = Diagnostic;

    fn into_iter(self) -> Self::IntoIter {
        self.diagnostics.into_iter()
    }
}

/// Compiler reporter.
///
/// Collects and reports any diagnostics emitted during compilation.
pub struct Reporter<'ctx> {
    /// The actual diagnostics.
    diagnostics: Diagnostics<'ctx>,
}

impl<'ctx> Reporter<'ctx> {
    /// Create a new `Reporter`.
    pub fn new(arena: &'ctx Arena, files: &'ctx SimpleFile<&'ctx str, &'ctx str>) -> Self {
        let writer = arena.alloc(StandardStream::stderr(ColorChoice::Always));
        let config = arena.alloc(default());
        Self {
            diagnostics: Diagnostics {
                writer,
                config,
                files,
                diagnostics: default(),
                is_error: false,
            },
        }
    }

    /// Create a new error diagnostic.
    pub fn emit(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// Flushes all diagnostics and returns them.
    pub fn flush(&mut self) -> Diagnostics<'ctx> {
        self.diagnostics.flush()
    }

    /// Was there any errors so far?
    pub fn has_errors(&self) -> bool {
        self.diagnostics.is_error
    }
}
