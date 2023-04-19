use std::collections::HashMap;
use std::vec;

use super::var::vardef;
use super::{Block, Ty, TyAndStratum, VarDef};
use Ty::*;

use super::stratum::{Stratum, StratumVar};

/// A function in the parser AST.
#[derive(Debug, Clone)]
pub struct Fun {
    /// Name of that function.
    pub name: String,
    /// List of stratum variables.
    pub quantifiers: Vec<StratumVar>,
    /// Parameters to that function, with their types
    /// and stratum.
    pub params: Vec<VarDef>,
    /// Return type.
    pub ret_ty: TyAndStratum,
    /// Body of the function: a list of statements and
    /// a final expression.
    pub body: Block,
}

impl Fun {
    /// Gets the (generic) function signature.
    ///
    /// Note: if you want to consume the `Fun`, prefer to
    /// use the `From`/`Into` trait.
    pub fn signature(&self) -> GenericFunSig {
        GenericFunSig {
            quantifiers: self.quantifiers.clone(),
            sig: FunSig {
                name: self.name.clone(),
                params: self.params.clone(),
                ret_ty: self.ret_ty.clone(),
            },
        }
    }
}

/// A list of substitutions of abstract stratums with concrete ones.
pub type Subst = HashMap<StratumVar, Stratum>;

/// A function signature in the parser AST.
#[derive(Debug, Clone)]
pub struct FunSig {
    /// Name of that function.
    pub name: String,
    /// Parameters to that function, with their types
    /// and stratum.
    pub params: Vec<VarDef>,
    /// Return type.
    pub ret_ty: TyAndStratum,
}

impl FunSig {
    /// Applies a single stratum variable substitution in the function signature.
    pub fn subst_stm(self, v: Stratum, with: Stratum) -> Self {
        let Self {
            name,
            params,
            ret_ty,
        } = self;
        Self {
            name,
            params: params
                .into_iter()
                .map(|param| param.subst_stm(v, with))
                .collect(),
            ret_ty: ret_ty.subst_var(v, with),
        }
    }

    /// Applies a substitution to this function signature.
    pub fn subst(self, subst: Subst) -> Self {
        subst
            .into_iter()
            .fold(self, |s, (v, with)| s.subst_stm(Stratum::from(v), with))
    }
}

/// A stratum-generic function signature in the parser AST.
#[derive(Debug, Clone)]
pub struct GenericFunSig {
    /// List of stratum variables.
    pub quantifiers: Vec<StratumVar>,
    /// Function signature.
    pub sig: FunSig,
}

impl GenericFunSig {
    /// Instantiates the generic parameters with fresh, concrete stratums.
    pub fn instantiate(&self) -> FunSig {
        let sig = self.sig.clone();
        let quantifiers_map = self
            .quantifiers
            .iter()
            .copied()
            .map(|q| (q, Stratum::new_abstract()))
            .collect();
        sig.subst(quantifiers_map)
    }
}

impl From<Fun> for GenericFunSig {
    fn from(f: Fun) -> Self {
        GenericFunSig {
            quantifiers: f.quantifiers,
            sig: FunSig {
                name: f.name,
                params: f.params,
                ret_ty: f.ret_ty,
            },
        }
    }
}

/// Shortcut to create function signatures for binary operators.
///
/// Typically those you can find for builtin functions.
pub fn binop_int_sig(op: impl ToString, ret_ty: Ty) -> GenericFunSig {
    let stm_v = StratumVar::new();
    let stm = Stratum::from(stm_v);
    GenericFunSig {
        quantifiers: vec![stm_v],
        sig: FunSig {
            name: op.to_string(),
            params: vec![vardef("n1", stm, IntT), vardef("n2", stm, IntT)],
            ret_ty: TyAndStratum { ty: ret_ty, stm },
        },
    }
}
