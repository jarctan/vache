//! Enhancing variable definitions of the parser AST with stratum information.

use std::default::default;
use std::fmt;
use std::sync::atomic::AtomicU64;

use super::{Span, Stratum, Ty, TySubst, TyVar, Varname};
use crate::ast;
use crate::utils::set::Set;
use crate::Arena;

/// Fresh variable counter.
///
/// Global to avoid any confusion between variable names.
pub static VAR_COUNTER: AtomicU64 = AtomicU64::new(0);

/// A variable in the code.
///
/// Different from the `VarUse` in AST because here we don't have any optional
/// type annotation.
#[derive(Clone, Copy, Default)]
pub struct VarUse<'ctx> {
    /// Name of the variable.
    name: Varname<'ctx>,
    /// Span where the variable is used.
    span: Span,
}

impl<'ctx> VarUse<'ctx> {
    /// Returns the variable name (w/o span information).
    pub fn name(&self) -> Varname<'ctx> {
        self.name
    }

    /// A fresh variable, related to some code`span`.
    ///
    /// These are variables used internally by the CFG, that starts with `__cfg`
    /// followed by a unique numeral ID.
    pub(crate) fn fresh(arena: &'ctx Arena, span: Span) -> VarUse<'ctx> {
        let number = VAR_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let name: &str = arena.alloc(format!("時{number:?}"));
        VarUse {
            name: name.into(),
            span,
        }
    }

    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(self, _arena: &'ctx Arena<'ctx>, _substs: &TySubst<'ctx>) -> Self {
        // Subst is a no-op
        self
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        default()
    }
}

impl<'ctx> From<&'ctx str> for VarUse<'ctx> {
    fn from(name: &'ctx str) -> Self {
        Self {
            name: name.into(),
            ..default()
        }
    }
}

impl PartialEq for VarUse<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl<'ctx> PartialEq<str> for VarUse<'ctx> {
    fn eq(&self, other: &str) -> bool {
        let Self { name, span: _ } = self; // So that if we have more fields, we'll have a compile error to update this
        name == other // NB: var equality is computed according to the name
                      // only.
    }
}

impl<'ctx> PartialEq<&str> for VarUse<'ctx> {
    fn eq(&self, other: &&str) -> bool {
        let Self { name, span: _ } = self; // So that if we have more fields, we'll have a compile error to update this
        name == *other // NB: var equality is computed according to the
                       // name only.
    }
}

impl fmt::Debug for VarUse<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}

impl fmt::Display for VarUse<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

// Direct translation from the `VarUse` in AST and the `VarUse` in the TAST:
// just forget the type
impl<'ctx> From<crate::ast::VarUse<'ctx>> for VarUse<'ctx> {
    fn from(var: crate::ast::VarUse<'ctx>) -> Self {
        Self {
            name: var.name(),
            span: var.as_span(),
        }
    }
}

/// A variable definition, with stratum and type information.
#[derive(Clone, Copy, PartialEq)]
pub struct VarDef<'ctx> {
    /// Variable name.
    pub(crate) var: VarUse<'ctx>,
    /// Type of the variable.
    pub(crate) ty: Ty<'ctx>,
    /// Type of the variable.
    pub(crate) stm: Stratum,
    /// Code span.
    pub(crate) span: Span,
}

impl<'ctx> VarDef<'ctx> {
    /// Constructs a typed `VarDef` based on the parser `Vardef` by adding the
    /// stratum information.
    pub fn with_stratum(vardef: ast::VarDef<'ctx>, stm: Stratum) -> Self {
        Self {
            var: vardef.var().into(),
            ty: vardef.ty.into(),
            stm,
            span: vardef.span,
        }
    }

    /// Returns the name of the variable that is defined (w/o span, type or
    /// stratum information).
    pub(crate) fn name(&self) -> Varname<'ctx> {
        self.var.name()
    }

    /// Applies a [`TySubst`] to `self`.
    pub(crate) fn subst_ty(
        &self,
        arena: &'ctx Arena<'ctx>,
        substs: &TySubst<'ctx>,
    ) -> VarDef<'ctx> {
        Self {
            var: self.var.subst_ty(arena, substs),
            ty: self.ty.subst(arena, substs),
            stm: self.stm,
            span: self.span,
        }
    }

    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        let Self {
            var: _,
            ty,
            stm: _,
            span: _,
        } = self;
        ty.free_vars()
    }
}

impl fmt::Debug for VarDef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{}:{}", self.var, self.ty, self.stm)
    }
}

impl<'ctx> AsRef<VarUse<'ctx>> for VarDef<'ctx> {
    fn as_ref(&self) -> &VarUse<'ctx> {
        &self.var
    }
}

impl<'ctx> From<VarDef<'ctx>> for VarUse<'ctx> {
    fn from(vardef: VarDef<'ctx>) -> Self {
        vardef.var
    }
}

impl<'ctx> From<VarUse<'ctx>> for Varname<'ctx> {
    fn from(var: VarUse<'ctx>) -> Self {
        var.name
    }
}

impl<'ctx> From<VarDef<'ctx>> for Varname<'ctx> {
    fn from(vardef: VarDef<'ctx>) -> Self {
        vardef.var.name()
    }
}

impl<'ctx> From<VarDef<'ctx>> for crate::ast::VarDef<'ctx> {
    fn from(vardef: VarDef<'ctx>) -> Self {
        Self {
            name: vardef.var.name,
            ty: crate::ast::TyUse {
                kind: vardef.ty,
                span: vardef.span,
            },
            span: vardef.span,
            var_span: vardef.var.span,
        }
    }
}

/// Shortcut to create a new variable definition.
#[cfg(test)]
pub fn vardef<'ctx>(name: &'ctx str, ty: Ty<'ctx>, stm: Stratum) -> VarDef<'ctx> {
    let var = name.into();
    VarDef {
        var,
        ty,
        stm,
        span: default(),
    }
}
