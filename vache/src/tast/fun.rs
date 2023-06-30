//! Defining typed functions.

use super::{Block, TySubst, TyUse, TyVar, VarDef};
use crate::utils::set::Set;
use crate::Arena;

/// A function in the typed AST.
#[derive(Debug, Clone)]
pub struct Fun<'ctx> {
    /// Name of that function.
    pub name: &'ctx str,
    /// Type parameters.
    pub ty_params: Vec<TyVar<'ctx>>,
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
    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        // Type parameters of the function should be removed from the substitution we
        // will apply to that function
        let subst = subst.clone() - &self.ty_params;

        Self {
            name: self.name,
            ty_params: self.ty_params,
            params: self
                .params
                .into_iter()
                .map(|param| param.subst_ty(arena, &subst))
                .collect(),
            ret_ty: self.ret_ty.subst(arena, &subst),
            body: self.body.subst_ty(arena, &subst),
        }
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        let Self {
            name: _,
            params,
            ret_ty,
            ty_params,
            body,
        } = self;
        params.iter().map(VarDef::free_ty_vars).sum::<Set<_>>()
            + ret_ty.free_ty_vars()
            + body.free_ty_vars()
            - ty_params.iter()
    }
}
