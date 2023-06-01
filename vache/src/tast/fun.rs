//! Defining typed functions.

use super::{Block, Ty, VarDef};

/// A function in the typed AST.
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
