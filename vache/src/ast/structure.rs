//! Parsing structs, and defining their representation in the AST.

use std::collections::HashMap;
use std::fmt;

use pest::iterators::Pair;

use super::{Context, Parsable};
use super::{Span, Ty, TySubst, TyUse, VarDef};
use crate::grammar::*;
use crate::Arena;

/// A C-like `struct`.
#[derive(Clone, Default)]
pub struct Struct<'ctx> {
    /// Name of the structure.
    pub name: &'ctx str,
    /// Map of field names and their types.
    pub fields: HashMap<&'ctx str, TyUse<'ctx>>,
    /// Code span.
    pub span: Span,
}

impl fmt::Debug for Struct<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            name,
            fields,
            span: _,
        } = self; // So that if we add a new field, we don;'t forget it here

        let mut res = f.debug_struct(name);
        fields
            .iter()
            .fold(&mut res, |res, (name, ty)| res.field(name, ty));
        res.finish()
    }
}

impl<'ctx> Struct<'ctx> {
    pub(crate) fn subst(self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        Self {
            name: self.name,
            fields: self
                .fields
                .into_iter()
                .map(|(name, ty)| (name, ty.subst(arena, subst)))
                .collect(),
            span: self.span,
        }
    }

    /// Gets the type of a field in the structure.
    pub fn get_field(&self, field: impl AsRef<str>) -> Option<Ty<'ctx>> {
        self.fields.get(field.as_ref()).map(|ty| ty.kind)
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Struct<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        debug_assert!(matches!(pair.as_rule(), Rule::struct_def));

        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner().peekable();
        consume!(pairs, Rule::struct_kw);
        let name = consume!(pairs).as_str();
        consume!(pairs, Rule::lcb);
        let fields = pairs
            .filter(|field| !matches!(field.as_rule(), Rule::cma | Rule::rcb))
            .map(|field| {
                let vardef: VarDef = ctx.parse(field);
                (vardef.name.as_str(), vardef.ty)
            })
            .collect();
        Struct { name, fields, span }
    }
}
