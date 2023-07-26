//! Parsing structs, and defining their representation in the AST.

use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt;

use itertools::Itertools;
use pest::iterators::Pair;

use super::{Context, Parsable};
use super::{Span, TySubst, TyUse, TyVar};
use crate::utils::Set;
use crate::{grammar::*, Arena};

/// An enumerated type (tag union of types).
#[derive(Clone, Default)]
pub struct Enum<'ctx> {
    /// Name of the structure.
    pub name: &'ctx str,
    /// Type parameters.
    pub ty_params: Vec<TyVar<'ctx>>,
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
            ty_params,
        } = self; // So that if we add a new field, we don;'t forget it here

        // If we have type parameters, blend them nicely with the name in the debug
        // information
        let name = if !ty_params.is_empty() {
            let params = ty_params.iter().map(|param| format!("{param}")).join(", ");
            format!("{name}<{params}>")
        } else {
            name.to_string()
        };

        let mut res = f.debug_struct(&name);
        variants
            .iter()
            .fold(&mut res, |res, (name, ty)| res.field(name, ty));
        res.finish()
    }
}

impl<'ctx> Enum<'ctx> {
    /// Gets the arguments of of a variant in the `enum`.
    pub fn get_variant<'a>(&'a self, variant: impl Borrow<str>) -> Option<&'a [TyUse<'ctx>]> {
        self.variants.get(variant.borrow()).map(|args| &**args)
    }

    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Enum<'ctx> {
        // Type parameters of the function should be removed from the substitution we
        // will apply to that `enum`
        let subst = subst.clone() - &self.ty_params;

        Self {
            name: self.name,
            ty_params: self.ty_params,
            variants: self
                .variants
                .into_iter()
                .map(|(name, args)| {
                    (
                        name,
                        args.into_iter()
                            .map(|arg| arg.subst(arena, &subst))
                            .collect(),
                    )
                })
                .collect(),
            span: self.span,
        }
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        let Self {
            variants,
            ty_params,
            name: _,
            span: _,
        } = self;
        variants
            .values()
            .flat_map(|variant| variant.iter().map(TyUse::free_ty_vars))
            .sum::<Set<_>>()
            - ty_params.iter()
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Enum<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        debug_assert!(matches!(pair.as_rule(), Rule::enum_def));

        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner().peekable();
        consume!(pairs, Rule::enum_kw);
        let name = consume!(pairs, Rule::ident).as_str();

        // Parse optional type parameters
        let mut ty_params = vec![];
        if consume_opt!(pairs, Rule::lt).is_some() {
            loop {
                let pair = consume!(pairs);
                match pair.as_rule() {
                    Rule::gt => break,
                    Rule::cma => continue,
                    Rule::ident => ty_params.push(ctx.parse(pair)),
                    _ => unreachable!(),
                }
            }
        }

        // Parse variants
        consume!(pairs, Rule::lcb);
        consume_back!(pairs, Rule::rcb);
        let variants = pairs
            .filter(|arg| !matches!(arg.as_rule(), Rule::cma))
            .map(|variant| {
                debug_assert!(matches!(variant.as_rule(), Rule::variant_def));
                let mut pairs = variant.into_inner();
                let name = consume!(pairs).as_str();

                // Parse optional parenthesis and arguments
                let args = if consume_opt!(pairs, Rule::lp).is_some() {
                    consume_back!(pairs, Rule::rp);
                    pairs
                        .filter(|arg| !matches!(arg.as_rule(), Rule::lp | Rule::cma))
                        .map(|arg| ctx.parse(arg))
                        .collect_vec()
                } else {
                    vec![]
                };
                (name, args)
            })
            .collect();
        Enum {
            name,
            ty_params,
            variants,
            span,
        }
    }
}
