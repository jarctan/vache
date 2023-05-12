//! Enhancing variable definitions of the parser AST with stratum information.

use std::fmt;

use super::{Stratum, Ty, Var};
use crate::ast;

/// A variable definition, with stratum and type information.
#[derive(Clone, PartialEq, Eq)]
pub struct VarDef {
    /// Variable name.
    pub(crate) name: Var,
    /// Type of the variable.
    pub(crate) ty: Ty,
    /// Type of the variable.
    pub(crate) stm: Stratum,
}

impl VarDef {
    /// Constructs a typed `VarDef` based on the parser `Vardef` by adding the
    /// stratum information.
    pub fn with_stratum(vardef: ast::VarDef, stm: Stratum) -> Self {
        Self {
            name: vardef.name,
            ty: vardef.ty,
            stm,
        }
    }
}

impl fmt::Debug for VarDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{}:{}", self.name, self.ty, self.stm)
    }
}

impl AsRef<Var> for VarDef {
    fn as_ref(&self) -> &Var {
        &self.name
    }
}

impl From<VarDef> for Var {
    fn from(vardef: VarDef) -> Self {
        vardef.name
    }
}
