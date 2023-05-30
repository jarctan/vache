//! Parsing functions, and defining their representation in the AST.

use std::vec;

use pest::iterators::Pair;
use Ty::*;

use super::var::vardef;
use super::{Block, Context, Parsable, Ty, VarDef};
use crate::grammar::*;

/// A function in the parser AST.
#[derive(Debug, Clone)]
pub struct Fun {
    /// Name of that function.
    pub name: String,
    /// Parameters to that function, with their types
    /// and stratum.
    pub params: Vec<VarDef>,
    /// Return type.
    pub ret_ty: Ty,
    /// Body of the function: a list of statements and
    /// a final expression.
    pub body: Block,
}

impl Fun {
    /// Gets the (generic) function signature.
    ///
    /// Note: if you want to consume the `Fun`, prefer to
    /// use the `From`/`Into` trait.
    pub fn signature(&self) -> FunSig {
        FunSig {
            name: self.name.clone(),
            params: self.params.clone(),
            ret_ty: self.ret_ty.clone(),
        }
    }
}

/// A function signature in the parser AST.
#[derive(Debug, Clone)]
pub struct FunSig {
    /// Name of that function.
    pub name: String,
    /// Parameters to that function, with their types
    /// and stratum.
    pub params: Vec<VarDef>,
    /// Return type.
    pub ret_ty: Ty,
}

impl From<Fun> for FunSig {
    fn from(f: Fun) -> Self {
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
pub fn binop_int_sig(op: impl ToString, ret_ty: Ty) -> FunSig {
    FunSig {
        name: op.to_string(),
        params: vec![vardef("n1", IntT), vardef("n2", IntT)],
        ret_ty,
    }
}

impl Parsable<Pair<'_, Rule>> for Fun {
    fn parse(pair: Pair<Rule>, ctx: &mut Context) -> Self {
        debug_assert!(matches!(pair.as_rule(), Rule::fun));
        let mut pairs = pair.into_inner();

        let name = pairs.next().unwrap().as_str().to_string();

        let params_pair = pairs.next().unwrap();
        debug_assert!(matches!(params_pair.as_rule(), Rule::params));
        let params = params_pair
            .into_inner()
            .map(|param| ctx.parse(param))
            .collect();

        let ret_ty = ctx.parse(pairs.next().unwrap());
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
    use pest::Parser;

    use super::super::{Stmt, VarDef};
    use super::*;
    use crate::grammar::Grammar;

    #[test]
    fn main_fn() {
        let input = "fn main(x: int, y: str) -> () { let x: int = 5; y = 7; x }";
        let mut parsed = Grammar::parse(Rule::fun, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let fun: Fun = ctx.parse(pair);
        eprintln!("{fun:?}");
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
}
