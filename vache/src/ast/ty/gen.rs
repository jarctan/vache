//! Defining parametrized types.

use super::*;
use crate::utils::set::Set;
use crate::Arena;

/// Parametrized type.
#[derive(Debug, Clone, Copy)]
pub struct GenTy<'ctx> {
    /// Type parameters.
    params: &'ctx [TyVar<'ctx>],
    /// Actual type.
    ty: Ty<'ctx>,
}

impl<'ctx> GenTy<'ctx> {
    /// Substitutes a type variable `from` for `to` in `self`, returning a new
    /// type.
    pub(crate) fn subst_var(
        &self,
        arena: &'ctx Arena<'ctx>,
        from: TyVar<'ctx>,
        to: Ty<'ctx>,
    ) -> Self {
        if self.params.iter().all(|&el| el != from) {
            Self {
                params: self.params,
                ty: self.ty.subst_var(arena, from, to),
            }
        } else {
            *self
        }
    }

    /// Applies a type substitution from `subst` in `self`, returning a
    /// new type.
    pub(crate) fn subst(&self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        subst
            .substs
            .iter()
            .fold(*self, |acc, (var, ty)| acc.subst_var(arena, *var, *ty))
    }

    /// Returns the free type variables in `self`.
    pub fn free_vars(&self) -> Set<TyVar<'ctx>> {
        self.ty.free_vars() - self.params.iter()
    }
}
