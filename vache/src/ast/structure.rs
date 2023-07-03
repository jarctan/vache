//! Parsing structs, and defining their representation in the AST.

use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt;

use itertools::Itertools;
use pest::iterators::Pair;

use super::{Context, Parsable};
use super::{Span, Ty, TySubst, TyUse, TyVar, VarDef};
use crate::grammar::*;
use crate::utils::set::Set;
use crate::Arena;

/// A C-like `struct`.
#[derive(Clone, Default)]
pub struct Struct<'ctx> {
    /// Name of the structure.
    pub name: &'ctx str,
    /// Type parameters.
    pub ty_params: Vec<TyVar<'ctx>>,
    /// Map of field names and their types.
    pub fields: HashMap<&'ctx str, TyUse<'ctx>>,
    /// Code span.
    pub span: Span,
}

impl fmt::Debug for Struct<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            name,
            ty_params,
            fields,
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
        fields
            .iter()
            .fold(&mut res, |res, (name, ty)| res.field(name, ty));
        res.finish()
    }
}

impl<'ctx> Struct<'ctx> {
    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        // Type parameters of the function should be removed from the substitution we
        // will apply to that `subst`
        let subst = subst.clone() - &self.ty_params;

        Self {
            name: self.name,
            ty_params: self.ty_params,
            fields: self
                .fields
                .into_iter()
                .map(|(name, ty)| (name, ty.subst(arena, &subst)))
                .collect(),
            span: self.span,
        }
    }

    /// Gets the type of a field in the structure.
    pub fn get_field(&self, field: impl Borrow<str>) -> Option<Ty<'ctx>> {
        self.fields.get(field.borrow()).map(|ty| ty.kind)
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        let Self {
            name: _,
            fields,
            ty_params,
            span: _,
        } = self;
        fields.values().map(TyUse::free_ty_vars).sum::<Set<_>>() - ty_params.iter()
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Struct<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        debug_assert!(matches!(pair.as_rule(), Rule::struct_def));

        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner().peekable();
        consume!(pairs, Rule::struct_kw);
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

        // Parse fields
        consume!(pairs, Rule::lcb);
        let fields = pairs
            .filter(|field| !matches!(field.as_rule(), Rule::cma | Rule::rcb))
            .map(|field| {
                let vardef: VarDef = ctx.parse(field);
                (vardef.name.as_str(), vardef.ty)
            })
            .collect();

        Struct {
            name,
            ty_params,
            fields,
            span,
        }
    }
}
