//! Parsing functions, and defining their representation in the AST.

use std::default::default;
use std::vec;

use pest::iterators::Pair;
use Ty::*;

use super::{Block, Context, FunSig, Parsable, Span, Ty, TySubst, TyUse, TyVar, VarDef, Varname};
use crate::grammar::*;
use crate::utils::Set;
use crate::Arena;

/// A function parameter. Can optionally take by reference.
#[derive(Debug, Clone, Copy)]
pub struct FunParam<'ctx> {
    /// Variable being defined.
    pub var: VarDef<'ctx>,
    /// Is it taking by reference.
    pub byref: bool,
    /// Codespan of the entire function parameter.
    pub span: Span,
}

impl<'ctx> FunParam<'ctx> {
    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(&self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        Self {
            var: self.var.subst_ty(arena, subst),
            byref: self.byref,
            span: self.span,
        }
    }

    /// Returns the type of the function parameter.
    pub fn ty(&self) -> Ty<'ctx> {
        self.var.ty.kind
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        self.var.ty.free_vars()
    }
}

impl<'ctx> From<FunParam<'ctx>> for VarDef<'ctx> {
    fn from(param: FunParam<'ctx>) -> Self {
        param.var
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for FunParam<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner();

        // If we see the @ operator, we are "by reference".
        let byref = consume_opt!(pairs, Rule::as_mut).is_some();

        let var = ctx.parse(consume!(pairs));

        Self { var, byref, span }
    }
}

/// Shortcut to create a new function parameter.
pub fn param<'ctx>(name: &'ctx str, ty: impl Into<TyUse<'ctx>>) -> FunParam<'ctx> {
    let name = Varname::from(name);
    FunParam {
        var: VarDef {
            name,
            span: default(),
            var_span: default(),
            ty: ty.into(),
        },
        span: default(),
        byref: false,
    }
}

/// Shortcut to create a new function parameter that takes by reference.
pub fn ref_param<'ctx>(name: &'ctx str, ty: impl Into<TyUse<'ctx>>) -> FunParam<'ctx> {
    let name = Varname::from(name);
    FunParam {
        var: VarDef {
            name,
            span: default(),
            var_span: default(),
            ty: ty.into(),
        },
        span: default(),
        byref: true,
    }
}

/// A function in the parser AST.
#[derive(Debug, Clone)]
pub struct Fun<'ctx> {
    /// Name of that function.
    pub name: &'ctx str,
    /// Type parameters.
    pub ty_params: Vec<TyVar<'ctx>>,
    /// Parameters to that function, with their types
    /// and stratum.
    pub params: Vec<FunParam<'ctx>>,
    /// Return type.
    pub ret_ty: TyUse<'ctx>,
    /// Body of the function: a list of statements and
    /// a final expression.
    pub body: Block<'ctx>,
    /// Codespan.
    pub span: Span,
}

impl Default for Fun<'_> {
    fn default() -> Self {
        // Custom default constructor to override default return type in this
        // case
        Self {
            name: "",
            params: vec![],
            ret_ty: UnitT.into(),
            ty_params: default(),
            body: default(),
            span: default(),
        }
    }
}

impl<'ctx> Fun<'ctx> {
    /// Gets the (generic) function signature.
    ///
    /// Note: if you want to consume the `Fun`, prefer to
    /// use the `From`/`Into` trait.
    pub fn signature(&self) -> FunSig<'ctx> {
        FunSig {
            name: self.name,
            params: self.params.clone(),
            ty_params: self.ty_params.clone(),
            ret_ty: self.ret_ty,
            span: self.span,
        }
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Fun<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        debug_assert!(matches!(pair.as_rule(), Rule::fun));
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

        let body = ctx.parse(consume!(pairs));

        Fun {
            name,
            ty_params,
            params,
            ret_ty,
            body,
            span,
        }
    }
}

#[cfg(test)]
mod tests {
    use StmtKind::*;

    use super::super::StmtKind;
    use super::*;

    #[parses(
        "fn main(x: int, y: str) { var x: int = 5; y = 7; x }"
        as fun
    )]
    #[test]
    fn main_fn(fun: Fun) {
        assert_eq!(fun.name, "main");

        assert!(matches!(fun.params.len(), 2));
        assert!(matches!(fun.params[0].ty(), IntT));
        assert!(matches!(fun.params[1].ty(), StrT));

        assert!(matches!(fun.ret_ty.kind, UnitT));

        assert_eq!(fun.body.stmts.len(), 2);
        assert!(matches!(fun.body.stmts[0].kind, DeclareS(..)));
        assert!(matches!(fun.body.stmts[1].kind, AssignS(..)));
    }

    #[parses(
        "fn my_fn(x: int, y: str) -> int { var x: int = 5; y = 7; x }"
        as fun
    )]
    #[test]
    fn my_fn(fun: Fun) {
        assert_eq!(fun.name, "my_fn");

        assert!(matches!(fun.params.len(), 2));
        assert!(matches!(fun.params[0].ty(), IntT));
        assert!(matches!(fun.params[1].ty(), StrT));

        assert!(matches!(fun.ret_ty.kind, IntT));

        assert_eq!(fun.body.stmts.len(), 2);
        assert!(matches!(fun.body.stmts[0].kind, DeclareS(..)));
        assert!(matches!(fun.body.stmts[1].kind, AssignS(..)));
    }
}
