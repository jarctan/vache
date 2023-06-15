//! Parsing functions, and defining their representation in the AST.

use std::default::default;
use std::vec;

use pest::iterators::Pair;
use Ty::*;

use super::var::vardef;
use super::{Block, Context, Parsable, Span, Ty, VarDef};
use crate::examples::TyUse;
use crate::grammar::*;

/// A function in the parser AST.
#[derive(Debug, Clone)]
pub struct Fun<'ctx> {
    /// Name of that function.
    pub name: &'ctx str,
    /// Parameters to that function, with their types
    /// and stratum.
    pub params: Vec<VarDef<'ctx>>,
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
            ret_ty: self.ret_ty.kind,
        }
    }
}

/// A function signature in the parser AST.
#[derive(Debug, Clone)]
pub struct FunSig<'ctx> {
    /// Name of that function.
    pub name: &'ctx str,
    /// Parameters to that function, with their types
    /// and stratum.
    pub params: Vec<VarDef<'ctx>>,
    /// Return type.
    pub ret_ty: Ty<'ctx>,
}

impl<'ctx> From<Fun<'ctx>> for FunSig<'ctx> {
    fn from(f: Fun<'ctx>) -> Self {
        f.signature()
    }
}

/// Shortcut to create function signatures for binary operators.
///
/// Typically those you can find for builtin functions.
pub fn binop_int_sig<'ctx>(op: &'ctx str, ret_ty: Ty<'ctx>) -> FunSig<'ctx> {
    FunSig {
        name: op,
        params: vec![vardef("n1", IntT), vardef("n2", IntT)],
        ret_ty,
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Fun<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &mut Context<'ctx>) -> Self {
        debug_assert!(matches!(pair.as_rule(), Rule::fun));
        let span = Span::from(pair.as_span());

        let mut pairs = pair.into_inner().peekable();

        consume!(pairs, Rule::fn_kw);

        let name = consume!(pairs).as_str();

        let params_pair = consume!(pairs, Rule::params);
        let params_span = params_pair.as_span();
        let mut params_pairs = params_pair.into_inner();
        consume!(params_pairs, Rule::lp);
        consume_back!(params_pairs, Rule::rp);

        let params = params_pairs
            .filter(|pair| !matches!(pair.as_rule(), Rule::cma))
            .map(|param| ctx.parse(param))
            .collect();

        let ret_ty: TyUse = if consume_opt!(pairs, Rule::arw) {
            ctx.parse(consume!(pairs, Rule::ty))
        } else {
            // Otherwise, arbitrarily place the span of the nonexistent type return at the
            // end of the span of the function parameters
            UnitT.with_span(Span::at(params_span.end()))
        };

        let body = ctx.parse(consume!(pairs));

        Fun {
            name,
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
        assert!(matches!(fun.params[0].ty.kind, IntT));
        assert!(matches!(fun.params[1].ty.kind, StrT));

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
        assert!(matches!(fun.params[0].ty.kind, IntT));
        assert!(matches!(fun.params[1].ty.kind, StrT));

        assert!(matches!(fun.ret_ty.kind, IntT));

        assert_eq!(fun.body.stmts.len(), 2);
        assert!(matches!(fun.body.stmts[0].kind, DeclareS(..)));
        assert!(matches!(fun.body.stmts[1].kind, AssignS(..)));
    }
}
