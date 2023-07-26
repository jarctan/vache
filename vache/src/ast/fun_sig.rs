//! Parsing function signatures and defining them in the AST.

use std::default::default;
use std::vec;

use pest::iterators::Pair;
use Ty::*;

use super::{param, Context, FunParam, Parsable, Span, Ty, TySubst, TyVar};
use crate::examples::TyUse;
use crate::grammar::*;
use crate::utils::Set;
use crate::Arena;

/// A function signature in the parser AST.
#[derive(Debug, Clone)]
pub struct FunSig<'ctx> {
    /// Name of that function.
    pub name: &'ctx str,
    /// Type parameters.
    pub ty_params: Vec<TyVar<'ctx>>,
    /// Parameters to that function, with their types
    /// and stratum.
    pub params: Vec<FunParam<'ctx>>,
    /// Return type.
    pub ret_ty: TyUse<'ctx>,
    /// Codespan.
    pub span: Span,
}

impl<'ctx> FunSig<'ctx> {
    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        // Type parameters of the function should be removed from the substitution we
        // will apply to that `subst`
        let subst = subst.clone() - &self.ty_params;

        Self {
            name: self.name,
            ty_params: self.ty_params,
            params: self
                .params
                .into_iter()
                .map(|p| p.subst_ty(arena, &subst))
                .collect(),
            ret_ty: self.ret_ty.subst(arena, &subst),
            span: self.span,
        }
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        let Self {
            name: _,
            ty_params,
            params,
            ret_ty,
            span: _,
        } = self;
        params.iter().map(FunParam::free_ty_vars).sum::<Set<_>>() + ret_ty.free_vars()
            - ty_params.iter()
    }

    /// Instantiate the type variables in the function signature.
    ///
    /// `span` should be the span of the call site.
    pub fn instantiate_tys(mut self, arena: &'ctx Arena<'ctx>, span: Span) -> Self {
        let subst = TySubst::from(
            arena,
            self.ty_params
                .iter()
                .map(|&p| (p, VarT(TyVar::fresh(span)))),
        );
        // Important: change the ty parameters before, otherwise our
        // substitution will get shadowed
        self.ty_params = vec![];
        self.subst_ty(arena, &subst)
    }
}

/// Shortcut to create function signatures for binary operators.
///
/// Typically used to sign builtin functions.
pub fn binop_sig<'ctx>(op: &'ctx str, arg_ty: Ty<'ctx>, ret_ty: Ty<'ctx>) -> FunSig<'ctx> {
    FunSig {
        name: op,
        ty_params: vec![],
        params: vec![param("n1", arg_ty), param("n2", arg_ty)],
        ret_ty: ret_ty.into(),
        span: default(),
    }
}

/// Shortcut to create function signatures for binary operators with a generic
/// parameter.
///
/// Typically used to sign builtin functions.
pub fn binop_gen_sig<'ctx>(op: &'ctx str, arg_ty: TyVar<'ctx>, ret_ty: Ty<'ctx>) -> FunSig<'ctx> {
    FunSig {
        name: op,
        ty_params: vec![arg_ty],
        params: vec![param("n1", VarT(arg_ty)), param("n2", VarT(arg_ty))],
        ret_ty: ret_ty.into(),
        span: default(),
    }
}

/// Shortcut to create function signatures for bool unary operators.
///
/// Typically used to sign builtin functions.
pub fn unop_bool_sig<'ctx>(op: &'ctx str, ret_ty: Ty<'ctx>) -> FunSig<'ctx> {
    FunSig {
        name: op,
        ty_params: vec![],
        params: vec![param("b", BoolT)],
        ret_ty: ret_ty.into(),
        span: default(),
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for FunSig<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        debug_assert!(matches!(pair.as_rule(), Rule::method_sig));
        let span = Span::from(pair.as_span());

        let mut pairs = pair.into_inner().peekable();

        consume!(pairs, Rule::fn_kw);

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

        // Parse parameters
        let params_pair = consume!(pairs, Rule::params);
        let params_span = params_pair.as_span();
        let mut params_pairs = params_pair.into_inner();
        consume!(params_pairs, Rule::lp);
        consume_back!(params_pairs, Rule::rp);
        let params = params_pairs
            .filter(|pair| !matches!(pair.as_rule(), Rule::cma))
            .map(|param| ctx.parse(param))
            .collect();

        let ret_ty: TyUse = if consume_opt!(pairs, Rule::arw).is_some() {
            ctx.parse(consume!(pairs, Rule::ty))
        } else {
            // Otherwise, arbitrarily place the span of the nonexistent type return at the
            // end of the span of the function parameters
            UnitT.with_span(Span::at(params_span.end()))
        };

        consume!(pairs, Rule::sc);

        FunSig {
            name,
            ty_params,
            params,
            ret_ty,
            span,
        }
    }
}
