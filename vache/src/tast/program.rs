//! Defining programs, the biggest unit in the Typed AST.

use std::collections::HashMap;
use std::fmt;

use super::{Enum, Fun, Struct, TyVar};
use crate::utils::Set;
use crate::Arena;

/// A program: a collection of:
/// * structures
/// * functions
#[derive(Clone)]
pub struct Program<'ctx> {
    /// AST arena.
    pub arena: &'ctx Arena<'ctx>,
    /// Collection of functions defined in the program, indexed by their names.
    pub funs: HashMap<&'ctx str, Fun<'ctx>>,
    /// Collection of structures defined in the program, indexed by their names.
    pub structs: &'ctx HashMap<&'ctx str, Struct<'ctx>>,
    /// Collection of enumerations defined in the program, indexed by their
    /// names.
    pub enums: &'ctx HashMap<&'ctx str, Enum<'ctx>>,
}

impl<'ctx> Program<'ctx> {
    /// Returns the free type variables in `self`.
    pub(crate) fn free_ty_vars(&self) -> Set<TyVar<'ctx>> {
        let Self {
            arena: _,
            funs,
            structs,
            enums,
        } = self;
        funs.values().map(Fun::free_ty_vars).sum::<Set<_>>()
            + structs.values().map(Struct::free_ty_vars).sum::<Set<_>>()
            + enums.values().map(Enum::free_ty_vars).sum::<Set<_>>()
    }
}

impl<'ctx> fmt::Debug for Program<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            funs,
            structs,
            enums,
            arena: _,
        } = self; // So that if we add a new field, we don;'t forget it here

        f.debug_struct("Program")
            .field("Functions", &funs.values())
            .field("Structures", &structs.values())
            .field("Enumerations", &enums.values())
            .finish()
    }
}
