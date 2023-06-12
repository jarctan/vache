//! Defining spans, with are markers for sections of code.
//!
//! Really handy to relate some error/warning/AST node to the actual, original
//! source code.

use std::fmt;

use codespan_reporting::diagnostic::Label;
use codespan_reporting::files::Files;

/// A span of source code.
///
/// Note: Influenced by `codespan`.
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Span(codespan::Span);

impl Span {
    /// Gives an empty span at the start of a source.
    pub const fn initial() -> Self {
        Self(codespan::Span::initial())
    }

    /// Returns the `codespan_reporting` label for this span.
    pub fn as_label(&self) -> Label<()> {
        Label::primary((), self.0.start().to_usize()..self.0.end().to_usize())
    }

    /// Returns the `codespan_reporting` (secondary) label for this span.
    pub fn as_secondary_label(&self) -> Label<()> {
        Label::secondary((), self.0.start().to_usize()..self.0.end().to_usize())
    }

    /// Combine two spans by taking the start of the earlier span and the end of
    /// the later span.
    ///
    /// Note: this will work even if the two spans are disjoint.
    ///
    /// Note: taken from `codespan`.
    pub fn merge(self, other: Self) -> Self {
        Self(self.0.merge(other.0))
    }

    /// Returns the line, col of this span start.
    pub fn line_col<'ctx, 'a, 'b: 'a>(
        &self,
        files: &'b impl Files<'a, Name = &'ctx str, Source = &'ctx str, FileId = ()>,
    ) -> (usize, usize) {
        let location = files.location((), self.0.start().to_usize()).unwrap();
        (location.line_number, location.column_number)
    }
}

impl From<Span> for Label<()> {
    fn from(span: Span) -> Self {
        span.as_label()
    }
}

impl From<pest::Span<'_>> for Span {
    fn from(span: pest::Span<'_>) -> Self {
        let start = u32::try_from(span.start()).expect("Code position is out of bounds");
        let end = u32::try_from(span.end()).expect("Code position is out of bounds");
        Self(codespan::Span::new(start, end))
    }
}

impl Default for Span {
    fn default() -> Self {
        Span::initial()
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}..{}", self.0.start(), self.0.end())
    }
}
