use super::{Block, Ty, VarDef};

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
