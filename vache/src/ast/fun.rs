//! Parsing functions, and defining their representation in the AST.

use std::vec;

use pest::iterators::Pair;
use Ty::*;

use super::var::vardef;
use super::{Block, Context, Parsable, Ty, VarDef};
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
    pub ret_ty: Ty<'ctx>,
    /// Body of the function: a list of statements and
    /// a final expression.
    pub body: Block<'ctx>,
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
            ret_ty: self.ret_ty.clone(),
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
        FunSig {
            name: f.name,
            params: f.params,
            ret_ty: f.ret_ty,
        }
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
        let mut pairs = pair.into_inner().peekable();

        let name = pairs.next().unwrap().as_str();

        let params_pair = pairs.next().unwrap();
        debug_assert!(matches!(params_pair.as_rule(), Rule::params));
        let params = params_pair
            .into_inner()
            .map(|param| ctx.parse(param))
            .collect();

        let ret_ty = if pairs.peek().unwrap().as_rule() == Rule::ty {
            ctx.parse(pairs.next().unwrap())
        } else {
            UnitT
        };

        let body = ctx.parse(pairs.next().unwrap());

        Fun {
            name,
            params,
            ret_ty,
            body,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::{Stmt, VarDef};
    use super::*;

    #[parses(
        "fn main(x: int, y: str) { var x: int = 5; y = 7; x }"
        as fun
    )]
    #[test]
    fn main_fn(fun: Fun) {
        assert_eq!(fun.name, "main");
        assert!(matches!(
            &fun.params[..],
            [VarDef { ty: IntT, .. }, VarDef { ty: StrT, .. }]
        ));
        assert!(matches!(&fun.ret_ty, UnitT));
        assert!(matches!(
            &fun.body.stmts[..],
            [Stmt::Declare(..), Stmt::Assign(..)]
        ));
    }

    #[parses(
        "fn my_fn(x: int, y: str) -> int { var x: int = 5; y = 7; x }"
        as fun
    )]
    #[test]
    fn my_fn(fun: Fun) {
        assert_eq!(fun.name, "my_fn");
        assert!(matches!(
            &fun.params[..],
            [VarDef { ty: IntT, .. }, VarDef { ty: StrT, .. }]
        ));
        assert!(matches!(&fun.ret_ty, IntT));
        assert!(matches!(
            &fun.body.stmts[..],
            [Stmt::Declare(..), Stmt::Assign(..)]
        ));
    }
}
