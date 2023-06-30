//! Handling of type variables.

use std::fmt;
use std::sync::atomic::AtomicU64;

use crate::ast::Span;

/// Fresh type variable counter.
///
/// Global to avoid any confusion between type variable names.
pub static TY_VAR_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Type variables (abstract types).
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyVar<'ctx> {
    /// User-defined type variable.
    Named(&'ctx str),
    /// Compiler-generated type variable.
    ///
    /// It has a unique `u64` id, and a span to which it is tied.
    Gen(u64, Span),
}

impl<'ctx> TyVar<'ctx> {
    /// Creates a new, fresh type variable related to a given codespan.
    pub(crate) fn fresh(span: impl Into<Span>) -> Self {
        let id = TY_VAR_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        Self::Gen(id, span.into())
    }
}

impl<'ctx> fmt::Display for TyVar<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TyVar::Named(name) => write!(f, "{name}"),
            TyVar::Gen(id, _) => write!(f, "τ{id}"),
        }
    }
}

impl<'ctx> fmt::Debug for TyVar<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
