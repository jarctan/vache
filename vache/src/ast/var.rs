//! Parsing types, and defining their representation in the AST.

use std::borrow::Borrow;
use std::default::default;
use std::fmt;

use pest::iterators::Pair;

use super::{Context, Parsable, Span, Ty, TySubst, TyUse};
use crate::grammar::*;
use crate::Arena;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
/// A variable.
pub struct Varname<'ctx>(&'ctx str);

impl<'ctx> Varname<'ctx> {
    /// See the variable as a string.
    pub fn as_str(&self) -> &'ctx str {
        self.0
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

impl<'ctx> From<VarUse<'ctx>> for Varname<'ctx> {
    fn from(val: VarUse<'ctx>) -> Self {
        val.name
    }
}

/// A variable in the code.
#[derive(Clone, Copy, Default)]
pub struct VarUse<'ctx> {
    /// Name of the variable.
    name: Varname<'ctx>,
    /// Optional type annotation.
    ty: Option<TyUse<'ctx>>,
    /// Span where the variable is used.
    span: Span,
}

impl PartialEq for VarUse<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl<'ctx> PartialEq<str> for VarUse<'ctx> {
    fn eq(&self, other: &str) -> bool {
        let Self {
            name,
            span: _,
            ty: _,
        } = self; // So that if we have more fields, we'll have a compile error to update this
        name == other // NB: var equality is computed according to the name
                      // only.
    }
}

impl<'ctx> PartialEq<&str> for VarUse<'ctx> {
    fn eq(&self, other: &&str) -> bool {
        let Self {
            name,
            span: _,
            ty: _,
        } = self; // So that if we have more fields, we'll have a compile error to update this
        name == *other // NB: var equality is computed according to the
                       // name only.
    }
}

impl<'ctx> VarUse<'ctx> {
    /// Returns the annotation type of this variable use.
    ///
    /// Returns `None` if there is no type annotation for this variable use.
    pub fn ty(&self) -> Option<Ty<'ctx>> {
        Some(*self.ty?)
    }

    /// Returns the annotation type of this variable use.
    ///
    /// Returns `None` if there is no type annotation for this variable use.
    pub fn ty_use(&self) -> Option<TyUse<'ctx>> {
        Some(TyUse {
            kind: *self.ty?,
            span: self.span,
        })
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

    /// Transforms that `VarUse` into a `VarDef`, by adding a type if there is
    /// none (otherwise, it will **keep** the original type).
    pub fn as_vardef(self, ty: Ty<'ctx>) -> VarDef<'ctx> {
        VarDef {
            name: self.name,
            ty: self.ty.unwrap_or(ty.with_span(self.span)), /* We'll say that the span of the
                                                             * `type` in the code is
                                                             * the same as the variable
                                                             * declaration */
            span: self.span,
            var_span: self.span,
        }
    }
}

impl<'ctx> Borrow<Varname<'ctx>> for VarUse<'ctx> {
    fn borrow(&self) -> &Varname<'ctx> {
        &self.name
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

impl Borrow<str> for VarUse<'_> {
    fn borrow(&self) -> &str {
        self.name.0
    }
}

impl<'ctx> From<&'ctx str> for VarUse<'ctx> {
    fn from(name: &'ctx str) -> Self {
        Self {
            name: name.into(),
            ..default()
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
        if let Some(ty) = self.ty {
            write!(f, "{}: {ty}", self.name)
        } else {
            write!(f, "{}", self.name)
        }
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
            ..default()
        }
    }
}

/// A variable definition.
#[derive(Clone, Copy)]
pub struct VarDef<'ctx> {
    /// Variable name.
    pub(crate) name: Varname<'ctx>,
    /// Span of the variable name.
    pub(crate) var_span: Span,
    /// Type of the variable.
    ///
    /// Note: is a `TyUse`, not `Ty`, so we can track its location in the code.
    pub(crate) ty: TyUse<'ctx>,
    /// Span of the entire variable definition.
    ///
    /// Different from `var_span` if there is some type (which adds some extra
    /// codespan to `var_span`).
    pub(crate) span: Span,
}

impl<'ctx> VarDef<'ctx> {
    /// Returns the name of the variable (w/o codespan information).
    pub fn name(&self) -> Varname<'ctx> {
        self.name
    }

    /// Returns the variable use corresponding to that variable definition.
    pub fn var(&self) -> VarUse<'ctx> {
        VarUse {
            name: self.name,
            ty: Some(self.ty),
            span: self.var_span,
        }
    }

    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(&self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        Self {
            name: self.name,
            var_span: self.var_span,
            ty: self.ty.subst(arena, subst),
            span: self.span,
        }
    }
}

impl<'ctx> PartialEq for VarDef<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        let Self {
            name,
            ty,
            span: _,
            var_span: _,
        } = self;
        name == &other.name && ty == &other.ty
    }
}

impl fmt::Debug for VarDef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{}", self.name, self.ty)
    }
}

impl<'ctx> Borrow<Varname<'ctx>> for VarDef<'ctx> {
    fn borrow(&self) -> &Varname<'ctx> {
        &self.name
    }
}

impl<'ctx> From<VarDef<'ctx>> for VarUse<'ctx> {
    fn from(vardef: VarDef<'ctx>) -> Self {
        vardef.var()
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for VarDef<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        assert!(pair.as_rule() == Rule::vardef);
        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner();

        let var_pair = consume!(pairs);
        let var_span = Span::from(var_pair.as_span());
        let name = ctx.parse(var_pair);
        consume!(pairs, Rule::cln);
        let ty = ctx.parse(consume!(pairs));

        VarDef {
            name,
            ty,
            span,
            var_span,
        }
    }
}

/// Shortcut to create a new variable definition.
pub fn vardef<'ctx>(name: &'ctx str, ty: impl Into<TyUse<'ctx>>) -> VarDef<'ctx> {
    let name = Varname::from(name);
    VarDef {
        name,
        span: default(),
        var_span: default(),
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
        assert_eq!(vardef.name, "test");
        assert_eq!(vardef.ty, StrT);
    }
}
