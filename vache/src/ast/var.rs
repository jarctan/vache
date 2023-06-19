//! Parsing types, and defining their representation in the AST.

use std::default::default;
use std::fmt;
use std::sync::atomic::AtomicU64;

use pest::iterators::Pair;

use super::{Context, Parsable, Span, Ty, TyUse};
use crate::grammar::*;
use crate::utils::boxed;
use crate::Arena;

/// Fresh variable counter.
///
/// Global to avoid any confusion between variable names.
pub static VAR_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
/// A variable.
pub struct Varname<'ctx>(&'ctx str);

impl<'ctx> Varname<'ctx> {
    /// See the variable as a string.
    pub fn as_str(&self) -> &'ctx str {
        self.0
    }
}

/// A variable in the code.
#[derive(Clone, Copy, Hash)]
pub struct VarUse<'ctx> {
    /// Name of the variable.
    name: Varname<'ctx>,
    /// Span where the variable is used.
    span: Span,
}

impl<'ctx> From<VarUse<'ctx>> for Varname<'ctx> {
    fn from(val: VarUse<'ctx>) -> Self {
        val.name
    }
}

impl PartialEq for VarUse<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl<'ctx> PartialEq<str> for Varname<'ctx> {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl<'ctx> PartialEq<&str> for Varname<'ctx> {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other
    }
}

impl<'ctx> PartialEq<str> for VarUse<'ctx> {
    fn eq(&self, other: &str) -> bool {
        let Self { name, span: _ } = self; // So that if we have more fields, we'll have a compile error to update this
        name == other // NB: var equality is computed according to the name
                      // only.
    }
}

impl<'ctx> PartialEq<&str> for VarUse<'ctx> {
    fn eq(&self, other: &&str) -> bool {
        let Self { name, span: _ } = self; // So that if we have more fields, we'll have a compile error to update this
        name == *other // NB: var equality is computed according to the
                       // name only.
    }
}

impl<'ctx> VarUse<'ctx> {
    /// A fresh variable, related to some code`span`.
    ///
    /// These are variables used internally by the CFG, that starts with `__cfg`
    /// followed by a unique numeral ID.
    pub(crate) fn fresh(arena: &'ctx Arena, span: Span) -> VarUse<'ctx> {
        let number = VAR_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let name: &str = arena.alloc(boxed(format!("時{number:?}")));
        VarUse {
            name: name.into(),
            span,
        }
    }

    /// See the variable as a string.
    pub fn as_str(&self) -> &'ctx str {
        self.name.as_str()
    }

    /// Returns the span of this variable.
    pub fn as_span(&self) -> Span {
        self.span
    }

    /// Returns the variable name (w/o span information).
    pub fn name(&self) -> Varname<'ctx> {
        self.name
    }

    /// Adds type information to the variable use to transform it into a
    /// variable declaration.
    ///
    /// Useful during the typing phase, when we want to type untyped versions.
    /// Note/TODO: will disappear when we support type inference (we will use
    /// `VarDef` directly).
    pub fn with_type(self, ty: Ty<'ctx>) -> VarDef<'ctx> {
        VarDef {
            var: self,
            ty: ty.with_span(self.span), /* We'll say that the span of the `type` in the code is
                                          * the same as the variable declaration */
            span: self.span,
        }
    }
}

impl<'ctx> AsRef<VarUse<'ctx>> for VarUse<'ctx> {
    fn as_ref(&self) -> &VarUse<'ctx> {
        self
    }
}

impl<'ctx> AsRef<Varname<'ctx>> for VarUse<'ctx> {
    fn as_ref(&self) -> &Varname<'ctx> {
        &self.name
    }
}

impl<'ctx> AsRef<Varname<'ctx>> for Varname<'ctx> {
    fn as_ref(&self) -> &Varname<'ctx> {
        self
    }
}

impl<'ctx> From<Varname<'ctx>> for &'ctx str {
    fn from(value: Varname<'ctx>) -> Self {
        value.0
    }
}

impl<'ctx> From<VarUse<'ctx>> for &'ctx str {
    fn from(value: VarUse<'ctx>) -> Self {
        value.name.into()
    }
}

impl AsRef<str> for VarUse<'_> {
    fn as_ref(&self) -> &str {
        self.name.0
    }
}

impl<'ctx> From<&'ctx str> for VarUse<'ctx> {
    fn from(name: &'ctx str) -> Self {
        Self {
            name: name.into(),
            span: Span::default(),
        }
    }
}

impl<'ctx> From<&'ctx str> for Varname<'ctx> {
    fn from(name: &'ctx str) -> Self {
        Self(name)
    }
}

impl<'a, 'ctx> From<&'a VarUse<'ctx>> for VarUse<'ctx> {
    fn from(var: &'a VarUse<'ctx>) -> Self {
        *var
    }
}

impl<'a, 'ctx> From<&'a Varname<'ctx>> for Varname<'ctx> {
    fn from(var: &'a Varname<'ctx>) -> Self {
        *var
    }
}

impl fmt::Debug for VarUse<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}

impl fmt::Display for VarUse<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Debug for Varname<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}

impl fmt::Display for Varname<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Varname<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, _ctx: &Context<'ctx>) -> Self {
        assert!(pair.as_rule() == Rule::ident);
        Self(pair.as_str())
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for VarUse<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        let span = Span::from(pair.as_span());
        VarUse {
            name: ctx.parse(pair),
            span,
        }
    }
}

/// A variable definition.
#[derive(Clone, Copy)]
pub struct VarDef<'ctx> {
    /// Variable name.
    pub(crate) var: VarUse<'ctx>,
    /// Type of the variable.
    ///
    /// Note: is a `TyUse`, not `Ty`, so we can track its location in the code.
    pub(crate) ty: TyUse<'ctx>,
    /// Span where the variable is used.
    pub(crate) span: Span,
}

impl<'ctx> VarDef<'ctx> {
    /// Returns the name of the variable (w/o codespan information).
    pub fn name(&self) -> Varname<'ctx> {
        self.var.name()
    }
}

impl<'ctx> PartialEq for VarDef<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        let Self { var, ty, span: _ } = self;
        var == &other.var && ty == &other.ty
    }
}

impl fmt::Debug for VarDef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{}", self.var, self.ty)
    }
}

impl<'ctx> AsRef<Varname<'ctx>> for VarDef<'ctx> {
    fn as_ref(&self) -> &Varname<'ctx> {
        &self.var.name
    }
}

impl<'ctx> From<VarDef<'ctx>> for VarUse<'ctx> {
    fn from(vardef: VarDef<'ctx>) -> Self {
        vardef.var
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for VarDef<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        assert!(pair.as_rule() == Rule::vardef);
        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner();

        let var = ctx.parse(consume!(pairs));
        consume!(pairs, Rule::cln);
        let ty = ctx.parse(consume!(pairs));

        VarDef { var, ty, span }
    }
}

/// Shortcut to create a new variable definition.
pub fn vardef<'ctx>(name: &'ctx str, ty: impl Into<TyUse<'ctx>>) -> VarDef<'ctx> {
    let var = VarUse::from(name);
    VarDef {
        var,
        span: default(),
        ty: ty.into(),
    }
}

#[cfg(test)]
mod tests {
    use Ty::*;

    use super::super::Ty;
    use super::*;

    #[parses("test123" as ident)]
    #[test]
    fn simple_var(var: VarUse) {
        assert_eq!(var, input);
    }

    #[parses("test: str" as vardef)]
    #[test]
    fn simple_vardef(vardef: VarDef) {
        assert_eq!(vardef.var, "test");
        assert_eq!(vardef.ty, StrT);
    }
}
