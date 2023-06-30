//! Defining typed functions.

use super::{Block, TySubst, TyUse, VarDef};
use crate::Arena;

/// A function in the typed AST.
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
}

impl<'ctx> Fun<'ctx> {
    pub(crate) fn subst(self, arena: &'ctx Arena<'ctx>, substs: &TySubst<'ctx>) -> Self {
        Self {
            name: self.name,
            params: self
                .params
                .into_iter()
                .map(|param| param.subst(arena, substs))
                .collect(),
            ret_ty: self.ret_ty.subst(arena, substs),
            body: self.body.subst(arena, substs),
        }
    }
}
