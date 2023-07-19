//! User-facing error reporting facility.

use std::default::default;
use std::io::Write;
use std::sync::atomic::{AtomicBool, Ordering};

use codespan_reporting::diagnostic::Severity;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use crossbeam_queue::SegQueue;

/// A compiler diagnostic.
pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<()>;

lazy_static! {
    /// Terminal configuration.
    static ref TERM_CONFIG: term::Config = term::Config::default();
    /// Standard stream handle.
    static ref STD_STREAM: StandardStream = StandardStream::stderr(ColorChoice::Always);
    /// Dummy file descriptor for internal compiler errors.
    static ref DUMMY_FILE_FOR_INTERNAL_ERRORS: SimpleFile<&'static str, &'static str> =
        SimpleFile::new("internal error", "");
}

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

impl<'ctx> From<anyhow::Error> for Diagnostics<'ctx> {
    fn from(err: anyhow::Error) -> Self {
        let diagnostics = SegQueue::default();
        let mut chain = err.chain().rev();
        if let Some(final_error) = chain.next() {
            let caused_by: Vec<_> = chain.map(|x| format!("Caused by: {x}")).collect();
            diagnostics.push(
                Diagnostic::error()
                    .with_message(final_error.to_string())
                    .with_notes(caused_by),
            );
        } else {
            diagnostics.push(Diagnostic::error().with_message("internal compiler error"));
        }
        Self {
            config: &TERM_CONFIG,
            writer: &STD_STREAM,
            files: &DUMMY_FILE_FOR_INTERNAL_ERRORS,
            diagnostics,
            is_error: AtomicBool::new(true),
        }
    }
}

impl<'ctx> From<std::io::Error> for Diagnostics<'ctx> {
    fn from(err: std::io::Error) -> Self {
        let diagnostics = SegQueue::default();
        diagnostics.push(
            Diagnostic::error()
                .with_message("I/O error")
                .with_notes(vec![err.to_string()]),
        );
        Self {
            config: &TERM_CONFIG,
            writer: &STD_STREAM,
            files: &DUMMY_FILE_FOR_INTERNAL_ERRORS,
            diagnostics,
            is_error: AtomicBool::new(true),
        }
    }
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
    pub fn new(files: &'ctx SimpleFile<&'ctx str, &'ctx str>) -> Self {
        Self {
            diagnostics: Diagnostics {
                config: &TERM_CONFIG,
                writer: &STD_STREAM,
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

    /// Displays all the diagnostics to the `stderr` output.
    pub fn display(&self) -> anyhow::Result<()> {
        self.diagnostics.display()
    }

    /// Was there any errors so far?
    pub fn has_errors(&self) -> bool {
        self.diagnostics.is_error.load(Ordering::SeqCst)
    }
}
