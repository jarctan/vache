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

    /// Creates a span at a specific position.
    pub fn at(pos: usize) -> Self {
        let pos: u32 = pos.try_into().expect("Code position is out of bounds");
        Self(codespan::Span::new(pos, pos))
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
    pub fn start_line_col<'ctx, 'a, 'b: 'a>(
        &self,
        files: &'b impl Files<'a, Name = &'ctx str, Source = &'ctx str, FileId = ()>,
    ) -> LineCol {
        let location = files.location((), self.0.start().to_usize()).unwrap();
        LineCol {
            line: location.line_number,
            col: location.column_number,
        }
    }

    /// Returns the line, col of this span end.
    pub fn end_line_col<'ctx, 'a, 'b: 'a>(
        &self,
        files: &'b impl Files<'a, Name = &'ctx str, Source = &'ctx str, FileId = ()>,
    ) -> LineCol {
        let location = files.location((), self.0.end().to_usize()).unwrap();
        LineCol {
            line: location.line_number,
            col: location.column_number,
        }
    }

    /// Returns the line, col of this span start.
    pub fn line_col<'ctx, 'a, 'b: 'a>(
        &self,
        files: &'b impl Files<'a, Name = &'ctx str, Source = &'ctx str, FileId = ()>,
    ) -> LineColSpan {
        LineColSpan {
            start: self.start_line_col(files),
            end: self.end_line_col(files),
        }
    }

    /// Start position of the span.
    pub fn start(&self) -> usize {
        self.0.start().to_usize()
    }

    /// End position of the span.
    pub fn end(&self) -> usize {
        self.0.end().to_usize()
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

/// A position in the file, represented as a line and a column.
///
/// User-friendly representation of a position.
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct LineCol {
    /// Line of the position in the file.
    pub line: usize,
    /// Column of the position in the file.
    pub col: usize,
}

impl fmt::Debug for LineCol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// User-friendly representation of a span in code, using lines and columns.
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct LineColSpan {
    /// Start position.
    pub start: LineCol,
    /// End position.
    pub end: LineCol,
}

impl fmt::Debug for LineColSpan {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
}
