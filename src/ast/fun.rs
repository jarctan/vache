use std::vec;

use super::var::vardef;
use super::{Block, VarDef, Ty};
use Ty::*;

use super::stratum::Stratum;

/// A function in the parser AST.
#[derive(Debug, Clone)]
pub struct Fun {
    /// Name of that function.
    pub name: String,
    /// List of stratum variables.
    pub quantifiers: Vec<Stratum>,
    /// Arguments to that function, with their types
    /// and stratum.
    pub args: Vec<VarDef>,
    /// Return type.
    pub ret_ty: Ty,
    /// Body of the function: a list of statements and
    /// a final expression.
    pub body: Block,
}

impl Fun {
    /// Gets the signature of the function.
    /// 
    /// Note: if you want to consume the `Fun`, prefer to
    /// use the `From`/`Into` trait.
    pub fn signature(&self) -> FunSig {
        FunSig {
            name: self.name.clone(),
            quantifiers: self.quantifiers.clone(),
            args: self.args.clone(),
            ret_ty: self.ret_ty.clone()
        }
    }
}

/// A function signature in the parser AST.
#[derive(Debug, Clone)]
pub struct FunSig {
    /// Name of that function.
    pub name: String,
    /// List of stratum variables.
    pub quantifiers: Vec<Stratum>,
    /// Arguments to that function, with their types
    /// and stratum.
    pub args: Vec<VarDef>,
    /// Return type.
    pub ret_ty: Ty,
}

impl From<Fun> for FunSig {
    fn from(f: Fun) -> Self {
        FunSig {
            name: f.name,
            quantifiers: f.quantifiers,
            args: f.args,
            ret_ty: f.ret_ty
        }
    }
}

/// Shortcut to create function signatures for binary operators.
/// 
/// Typically those you can find for builtin functions.
pub fn binop_int_sig(op: impl ToString, ret_ty: Ty) -> FunSig {
    let s = Stratum::new();
    FunSig {
        name: op.to_string(),
        quantifiers: vec![s],
        args: vec![
            vardef("n1", s, IntT),
            vardef("n2", s, IntT),
        ],
        ret_ty,
    }
}
