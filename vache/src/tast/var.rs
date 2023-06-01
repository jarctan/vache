//! Enhancing variable definitions of the parser AST with stratum information.

use std::fmt;

use super::{Stratum, Ty, Var};
use crate::ast;

/// A variable definition, with stratum and type information.
#[derive(Clone, PartialEq, Eq)]
pub struct VarDef<'ctx> {
    /// Variable name.
    pub(crate) name: Var<'ctx>,
    /// Type of the variable.
    pub(crate) ty: Ty<'ctx>,
    /// Type of the variable.
    pub(crate) stm: Stratum,
}

impl<'ctx> VarDef<'ctx> {
    /// Constructs a typed `VarDef` based on the parser `Vardef` by adding the
    /// stratum information.
    pub fn with_stratum(vardef: ast::VarDef<'ctx>, stm: Stratum) -> Self {
        Self {
            name: vardef.name,
            ty: vardef.ty,
            stm,
        }
    }
}

impl fmt::Debug for VarDef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{}:{}", self.name, self.ty, self.stm)
    }
}

impl<'ctx> AsRef<Var<'ctx>> for VarDef<'ctx> {
    fn as_ref(&self) -> &Var<'ctx> {
        &self.name
    }
}

impl<'ctx> From<VarDef<'ctx>> for Var<'ctx> {
    fn from(vardef: VarDef<'ctx>) -> Self {
        vardef.name
    }
}

/// Shortcut to create a new variable definition.
#[cfg(test)]
pub fn vardef<'ctx>(name: &'ctx str, ty: Ty<'ctx>, stm: Stratum) -> VarDef<'ctx> {
    let name = name.into();
    VarDef { name, ty, stm }
}
