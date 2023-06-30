//! Parsing structs, and defining their representation in the AST.

use std::collections::HashMap;
use std::fmt;

use pest::iterators::Pair;

use super::{Context, Parsable};
use super::{Span, TySubst, TyUse};
use crate::{grammar::*, Arena};

/// An enumerated type (tag union of types).
#[derive(Clone, Default)]
pub struct Enum<'ctx> {
    /// Name of the structure.
    pub name: &'ctx str,
    /// Map of variant names and their arguments.
    pub variants: HashMap<&'ctx str, Vec<TyUse<'ctx>>>,
    /// Code span.
    pub span: Span,
}

impl fmt::Debug for Enum<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            name,
            variants,
            span: _,
        } = self; // So that if we add a new field, we don;'t forget it here

        let mut res = f.debug_struct(name);
        variants
            .iter()
            .fold(&mut res, |res, (name, ty)| res.field(name, ty));
        res.finish()
    }
}

impl<'ctx> Enum<'ctx> {
    /// Gets the arguments of of a variant in the `enum`.
    pub fn get_variant<'a>(&'a self, variant: impl AsRef<str>) -> Option<&'a [TyUse<'ctx>]> {
        self.variants.get(variant.as_ref()).map(|args| &**args)
    }

    pub(crate) fn subst(self, arena: &'ctx Arena<'ctx>, substs: &TySubst<'ctx>) -> Enum<'ctx> {
        Self {
            name: self.name,
            variants: self
                .variants
                .into_iter()
                .map(|(name, args)| {
                    (
                        name,
                        args.into_iter()
                            .map(|arg| arg.subst(arena, substs))
                            .collect(),
                    )
                })
                .collect(),
            span: self.span,
        }
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Enum<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        debug_assert!(matches!(pair.as_rule(), Rule::enum_def));

        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner().peekable();
        consume!(pairs, Rule::enum_kw);
        let name = consume!(pairs).as_str();
        consume!(pairs, Rule::lcb);
        let variants = pairs
            .filter(|arg| !matches!(arg.as_rule(), Rule::cma | Rule::rcb))
            .map(|variant| {
                debug_assert!(matches!(variant.as_rule(), Rule::variant_def));
                let mut pairs = variant.into_inner();
                let name = consume!(pairs).as_str();
                let args: Vec<_> = pairs
                    .filter(|arg| !matches!(arg.as_rule(), Rule::lp | Rule::rp | Rule::cma))
                    .map(|arg| ctx.parse(arg))
                    .collect();
                (name, args)
            })
            .collect();
        Enum {
            name,
            variants,
            span,
        }
    }
}
