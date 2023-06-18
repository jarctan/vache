//! User-facing error reporting facility.

use std::default::default;
use std::io::Write;
use std::sync::atomic::{AtomicBool, Ordering};

use codespan_reporting::diagnostic::Severity;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use crossbeam_queue::SegQueue;

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
    diagnostics: SegQueue<Diagnostic>,
    /// True iff `self.diagnostics` contains at least one error diagnostic.
    is_error: AtomicBool,
}

impl<'ctx> Diagnostics<'ctx> {
    /// Displays all the diagnostics with nice colors and formatting to the
    /// standard output.
    ///
    /// # Warning
    /// WILL FLUSH/TRASH the diagnostics that are displayed.
    pub fn display(&self) -> anyhow::Result<()> {
        let mut writer = self.writer.lock();
        write!(&mut writer, "\r")?; // Flush anything on our line, in particular loading indicators
        while let Some(diagnostic) = self.diagnostics.pop() {
            term::emit(&mut writer, self.config, self.files, &diagnostic)?;
        }
        Ok(())
    }

    /// Pushes a new diagnostic to the list.
    pub fn push(&self, diagnostic: Diagnostic) {
        self.is_error.fetch_or(
            matches!(diagnostic.severity, Severity::Error | Severity::Bug),
            Ordering::Relaxed,
        );
        self.diagnostics.push(diagnostic);
    }

    /// Flushes all diagnostics and returns them, leaving self's own diagnostic
    /// list empty and ready to receive messages.
    fn flush(&mut self) -> Diagnostics<'ctx> {
        Diagnostics {
            writer: self.writer,
            config: self.config,
            files: self.files,
            diagnostics: std::mem::take(&mut self.diagnostics),
            // Revert error flag and get it back
            is_error: AtomicBool::new(self.is_error.swap(false, Ordering::SeqCst)),
        }
    }
}

impl<'ctx> IntoIterator for Diagnostics<'ctx> {
    type IntoIter = ::std::vec::IntoIter<Diagnostic>;
    type Item = Diagnostic;

    fn into_iter(self) -> Self::IntoIter {
        let mut res = vec![];
        while let Some(diagnostic) = self.diagnostics.pop() {
            res.push(diagnostic);
        }
        res.into_iter()
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
                is_error: AtomicBool::new(false),
            },
        }
    }

    /// Create a new error diagnostic.
    pub fn emit(&self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// Flushes all diagnostics and returns them.
    pub fn flush(&mut self) -> Diagnostics<'ctx> {
        self.diagnostics.flush()
    }

    /// Was there any errors so far?
    pub fn has_errors(&self) -> bool {
        self.diagnostics.is_error.load(Ordering::SeqCst)
    }
}
