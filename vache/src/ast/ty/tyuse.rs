//! Defining types with span information.

use std::default::default;
use std::fmt;
use std::ops::Deref;

use pest::iterators::Pair;

use super::*;
use crate::ast::{Context, Parsable, Span};
use crate::utils::set::Set;
use crate::{grammar::*, Arena};

/// A type in the source code.
///
/// This is a type + its use position in the source code.
#[derive(Clone, Copy, Default)]
pub struct TyUse<'ctx> {
    /// Type.
    pub kind: Ty<'ctx>,
    /// Span in the code for that use.
    pub span: Span,
}

impl<'ctx> TyUse<'ctx> {
    /// Applies a type substitution from `subst` in `self`, returning a
    /// new type.
    pub(crate) fn subst(&self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        Self {
            kind: self.kind.subst(arena, subst),
            span: self.span,
        }
    }

    /// Returns the free type variables in `self`.
    pub fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        let Self { kind, span: _ } = self;
        kind.free_vars()
    }
}

impl<'ctx> Deref for TyUse<'ctx> {
    type Target = Ty<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl<'ctx> PartialEq for TyUse<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        // We add this line to trigger an error if we add a new field without
        // changing these lines consequently.
        let Self { kind, span: _ } = *self;

        kind == other.kind
    }
}

impl<'ctx> PartialEq<Ty<'ctx>> for TyUse<'ctx> {
    fn eq(&self, other: &Ty<'ctx>) -> bool {
        self.kind == *other
    }
}

impl<'ctx> PartialEq<TyUse<'ctx>> for Ty<'ctx> {
    fn eq(&self, other: &TyUse<'ctx>) -> bool {
        self == &other.kind
    }
}

impl<'ctx> From<TyUse<'ctx>> for Ty<'ctx> {
    fn from(value: TyUse<'ctx>) -> Self {
        value.kind
    }
}

impl<'ctx> From<Ty<'ctx>> for TyUse<'ctx> {
    fn from(kind: Ty<'ctx>) -> Self {
        // TyUse with no span.
        TyUse { kind, ..default() }
    }
}

impl fmt::Display for TyUse<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl fmt::Debug for TyUse<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for TyUse<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        assert!(matches!(pair.as_rule(), Rule::ty | Rule::non_iter_ty));
        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner();
        let pair = consume!(pairs);
        let kind = match pair.as_rule() {
            Rule::unit => UnitT,
            Rule::bool_ty => BoolT,
            Rule::int_ty => IntT,
            Rule::str_ty => StrT,
            Rule::array_ty => {
                let mut pairs = pair.into_inner();
                consume!(pairs, Rule::lb);
                let inner: TyUse = ctx.parse(consume!(pairs));
                consume!(pairs, Rule::rb);
                ArrayT(ctx.alloc(inner.kind))
            }
            Rule::tuple_ty => {
                let mut pairs = pair.into_inner();
                consume!(pairs, Rule::lp);
                consume_back!(pairs, Rule::rp);
                let items: Vec<_> = pairs
                    .filter(|pair| !matches!(pair.as_rule(), Rule::cma))
                    .map(|item| TyUse::parse(item, ctx).kind)
                    .collect();
                let items: &'ctx [Ty] = ctx.alloc(items);
                TupleT(items)
            }
            Rule::iter_ty => {
                let mut pairs = pair.into_inner();
                let inner: TyUse = ctx.parse(consume!(pairs));
                let mut res = inner.kind;
                // For each `..`, add a layer of indirection
                for pair in pairs {
                    debug_assert!(matches!(pair.as_rule(), Rule::rg));
                    res = IterT(ctx.alloc(res));
                }
                res
            }
            Rule::ident => VarT(TyVar::Named(pair.as_str())),
            rule => panic!("parser internal error: expected type, found {rule:?}"),
        };
        Self { kind, span }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[parses("()" as ty)]
    #[test]
    fn unit_ty(ty: TyUse) {
        assert_eq!(ty, UnitT);
    }

    #[parses("str" as ty)]
    #[test]
    fn str_ty(ty: TyUse) {
        assert_eq!(ty, StrT);
    }

    #[parses("int" as ty)]
    #[test]
    fn int_ty(ty: TyUse) {
        assert_eq!(ty, IntT);
    }

    #[parses("bool" as ty)]
    #[test]
    fn bool_ty(ty: TyUse) {
        ensure!(ty == BoolT);
    }

    #[parses("bool.." as ty)]
    #[test]
    fn bool_iter(ty: TyUse) {
        let expected = IterT(arena.alloc(Ty::hole(Span::default())));
        ty.unify(&expected, &arena)
            .context("expected an iterator")?;
    }

    #[parses("(bool, int)" as ty)]
    #[test]
    fn tuple_ty(ty: TyUse) {
        let ty1 = Ty::hole(Span::default());
        let ty2 = Ty::hole(Span::default());
        let expected = TupleT(arena.alloc(vec![ty1, ty2]));
        let subst = ty.unify(&expected, &arena).context("expected a tuple")?;
        let ty1 = ty1.subst(&arena, &subst);
        let ty2 = ty2.subst(&arena, &subst);
        ensure!(ty1 == BoolT);
        ensure!(ty2 == IntT);
    }

    #[parses("bool......" as ty)]
    #[test]
    fn bool_iter_iter(ty: TyUse) -> Result<()> {
        let expected = IterT(arena.alloc(IterT(arena.alloc(Ty::hole(Span::default())))));
        ty.unify(&expected, &arena)
            .context("expected an iterator of iterators")?;
    }

    #[parses("blah" as ty)]
    #[test]
    fn type_var(ty: TyUse) -> Result<()> {
        ty.as_named_var().context("expected a type variable")?;
    }
}
