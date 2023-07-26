//! Parsing structs, and defining their representation in the AST.

use std::borrow::Borrow;
use std::collections::HashMap;
use std::default::default;
use std::fmt;

use itertools::Itertools;
use pest::iterators::Pair;

use super::{Context, Parsable};
use super::{FunSig, Span, TySubst, TyVar};
use crate::grammar::*;
use crate::utils::Set;
use crate::Arena;

/// Rust-like traits.
pub struct Trait<'ctx> {
    /// Trait name.
    pub name: &'ctx str,
    /// Type parameters for the trait.
    pub ty_params: Vec<TyVar<'ctx>>,
    /// (Required) methods.
    pub methods: HashMap<&'ctx str, FunSig<'ctx>>,
    /// Span in the source code.
    pub span: Span,
}

/// Shortcut to create a `struct` directly.
pub fn trait_def<'ctx>(
    name: &'ctx str,
    methods: impl IntoIterator<Item = (&'ctx str, FunSig<'ctx>)>,
) -> Trait<'ctx> {
    let methods = methods
        .into_iter()
        .map(|(name, method)| (name, method))
        .collect::<HashMap<_, _>>();

    Trait {
        name,
        methods,
        span: default(),
        ty_params: default(),
    }
}

impl fmt::Debug for Trait<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            name,
            ty_params,
            methods,
            span: _,
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
        methods
            .iter()
            .fold(&mut res, |res, (name, ty)| res.field(name, ty));
        res.finish()
    }
}

impl<'ctx> Trait<'ctx> {
    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        // Type parameters of the function should be removed from the substitution we
        // will apply to that `subst`
        let subst = subst.clone() - &self.ty_params;

        Self {
            name: self.name,
            ty_params: self.ty_params,
            methods: self
                .methods
                .into_iter()
                .map(|(name, method)| (name, method.subst_ty(arena, &subst)))
                .collect(),
            span: self.span,
        }
    }

    /// Gets the type of a method in the structure.
    pub fn get_method<'a>(&'a self, field: impl Borrow<str>) -> Option<&'a FunSig<'ctx>> {
        self.methods.get(field.borrow())
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        let Self {
            name: _,
            ty_params,
            methods,
            span: _,
        } = self;
        methods.values().map(|x| x.free_ty_vars()).sum::<Set<_>>() - ty_params.iter()
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Trait<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        debug_assert!(matches!(pair.as_rule(), Rule::trait_def));

        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner().peekable();
        consume!(pairs, Rule::trait_kw);
        let name = consume!(pairs).as_str();

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

        // Parse methods
        consume!(pairs, Rule::lcb);
        consume_back!(pairs, Rule::rcb);

        let methods = pairs
            .map(|pair| {
                let method: FunSig = ctx.parse(pair);
                (method.name, method)
            })
            .collect();

        Trait {
            name,
            ty_params,
            methods,
            span,
        }
    }
}
