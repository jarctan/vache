//! Defining typed functions.

use std::fmt;

use super::{Block, Span, Stratum, Ty, TySubst, TyUse, TyVar, VarDef, Varname};
use crate::ast;
use crate::utils::Set;
use crate::Arena;

/// A function parameter. Can optionally take by reference.
#[derive(Clone, Copy)]
pub struct FunParam<'ctx> {
    /// Variable being defined.
    pub var: VarDef<'ctx>,
    /// Is it taking by reference.
    pub byref: bool,
    /// Codespan of the entire function parameter.
    pub span: Span,
}

impl<'ctx> fmt::Debug for FunParam<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.byref {
            write!(f, "@{:?}", self.var)
        } else {
            write!(f, "{:?}", self.var)
        }
    }
}

impl<'ctx> FunParam<'ctx> {
    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(&self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        Self {
            var: self.var.subst_ty(arena, subst),
            byref: self.byref,
            span: self.span,
        }
    }

    /// Constructs a typed `VarDef` based on the parser `Vardef` by adding the
    /// stratum information.
    pub fn with_stratum(param: ast::FunParam<'ctx>, stm: Stratum) -> Self {
        Self {
            var: VarDef::with_stratum(param.var, stm),
            byref: param.byref,
            span: param.span,
        }
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        let Self {
            var,
            byref: _,
            span: _,
        } = self;
        var.free_ty_vars()
    }

    /// Returns the name of the function parameter.
    pub fn name(&self) -> Varname<'ctx> {
        self.var.name()
    }

    /// Returns the type of the function parameter.
    pub fn ty(&self) -> Ty<'ctx> {
        self.var.ty
    }
}

impl<'ctx> From<FunParam<'ctx>> for Varname<'ctx> {
    fn from(param: FunParam<'ctx>) -> Self {
        param.name()
    }
}

/// A function in the typed AST.
#[derive(Debug, Clone)]
pub struct Fun<'ctx> {
    /// Name of that function.
    pub name: &'ctx str,
    /// Type parameters.
    pub ty_params: Vec<TyVar<'ctx>>,
    /// Parameters to that function, with their types
    /// and stratum.
    pub params: Vec<FunParam<'ctx>>,
    /// Return type.
    pub ret_ty: TyUse<'ctx>,
    /// Body of the function: a list of statements and
    /// a final expression.
    pub body: Block<'ctx>,
    /// Codespan.
    pub span: Span,
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
            span: self.span,
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
            span: _,
        } = self;
        params.iter().map(FunParam::free_ty_vars).sum::<Set<_>>()
            + ret_ty.free_ty_vars()
            + body.free_ty_vars()
            - ty_params.iter()
    }
}
