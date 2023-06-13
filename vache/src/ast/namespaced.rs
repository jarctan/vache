//! Defining namespaces, which allow for fully qualified, unambiguous
//! designation of symbols.

use std::default::default;
use std::fmt;

use pest::iterators::Pair;
use string_builder::Builder as StringBuilder;

use super::{Context, Parsable, Span};
use crate::grammar::*;
use crate::utils::boxed;

/// Namespace.
///
/// Series of `a::b::c` represented as a recursive, arena-relying, `Copy` data
/// structure.
#[derive(Copy, Clone, PartialEq, Eq, Default)]
pub enum Namespace<'ctx> {
    /// Root of all namespaces.
    RootN,
    /// Relative root namespace.
    #[default]
    RelativeN,
    /// Child of another namespace.
    ChildN(&'ctx Namespace<'ctx>, &'ctx str),
}

impl<'ctx> Namespace<'ctx> {
    /// TODO: remove when we implement namespace unification (so we do not need
    /// strings to compare anymore).
    pub fn as_str(&self) -> String {
        match *self {
            RootN => "::".to_string(),
            RelativeN => default(),
            ChildN(parent, child) => {
                let mut string = StringBuilder::default();
                string.append(parent.as_str());
                string.append("::");
                string.append(child);
                string.string().unwrap()
            }
        }
    }

    /// Gets the path of this namespace.
    pub fn path<'a>(&'a self) -> Box<dyn Iterator<Item = &'ctx str> + 'a> {
        match *self {
            RootN | RelativeN => boxed(std::iter::empty()),
            ChildN(parent, name) => boxed(parent.path().chain(std::iter::once(name))),
        }
    }
}

use Namespace::*;

impl fmt::Display for Namespace<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            RootN => write!(f, "::"),
            RelativeN => write!(f, "self"),
            ChildN(parent, name) => write!(f, "{}::{}", parent, name),
        }
    }
}

impl fmt::Debug for Namespace<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

/// Namespace name.
///
/// A name within a namespace. The name is unique within the namespace, but not
/// necessarily in general.
#[derive(Copy, Clone, PartialEq, Eq, Default)]
pub struct Namespaced<'ctx> {
    /// Namespace of the name.
    pub namespace: Namespace<'ctx>,
    /// The name within the namespace.
    pub name: &'ctx str,
    /// Codespan.
    pub span: Span,
}
impl<'ctx> Namespaced<'ctx> {
    /// TODO: remove when we implement namespace unification (so we do not need
    /// strings to compare anymore).
    pub fn as_str(&self) -> String {
        let mut string = StringBuilder::default();
        string.append(self.namespace.as_str());
        string.append("::");
        string.append(self.name);
        string.string().unwrap()
    }

    /// Gets the path of this namespace.
    pub fn path<'a>(&'a self) -> impl Iterator<Item = &'ctx str> + 'a {
        self.namespace.path().chain(std::iter::once(self.name))
    }

    /// Creates a new namespaced with a single identifier `span` in the relative
    /// namespace. Plus the `span` information for that identifier.
    pub fn name_with_span(name: &'ctx str, span: Span) -> Self {
        Self {
            namespace: Namespace::RelativeN,
            name,
            span,
        }
    }
}

impl<'ctx> From<&'ctx str> for Namespaced<'ctx> {
    fn from(name: &'ctx str) -> Self {
        Self {
            name,
            namespace: default(),
            span: default(),
        }
    }
}

impl fmt::Display for Namespaced<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}::{}", self.namespace, self.name)
    }
}

impl fmt::Debug for Namespaced<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Namespaced<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &mut Context<'ctx>) -> Self {
        debug_assert!(pair.as_rule() == Rule::namespaced);
        let span = Span::from(pair.as_span());

        // Note: we go through pairs in REVERSE order, so that we visit the rightmost
        // element (which is the name) first, and then build up the recursive
        // data structure.
        let mut pairs = pair.into_inner().rev();

        let name = pairs.next().unwrap().as_str();

        let namespace = pairs.fold(Namespace::RelativeN, |acc, pair| {
            Namespace::ChildN(ctx.alloc(acc), pair.as_str())
        });

        Namespaced {
            namespace,
            name,
            span,
        }
    }
}
