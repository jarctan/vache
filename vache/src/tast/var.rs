//! Enhancing variable definitions of the parser AST with stratum information.

use std::fmt;

use super::{Span, Stratum, Ty, VarUse, Varname};
use crate::ast;

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
            var: vardef.var,
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

/// Shortcut to create a new variable definition.
#[cfg(test)]
pub fn vardef<'ctx>(name: &'ctx str, ty: Ty<'ctx>, stm: Stratum) -> VarDef<'ctx> {
    use std::default::default;

    let var = name.into();
    VarDef {
        var,
        ty,
        stm,
        span: default(),
    }
}
