//! Defining substitutions for types.

use std::default::default;
use std::fmt;
use std::ops::{Add, AddAssign};

use super::*;
use crate::Arena;

/// A type substitution: collection of type variables and their type mapping.
#[derive(Clone)]
pub struct TySubst<'ctx> {
    /// Actual substitutions.
    pub(super) substs: Vec<(TyVar<'ctx>, Ty<'ctx>)>,
    /// Reference to the [`Arena`].
    ///
    /// Used when we merge two [`TySubst`], since we need to apply one
    /// [`TySubst`] to the other.
    pub(super) arena: &'ctx Arena<'ctx>,
}

impl<'ctx> fmt::Debug for TySubst<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.substs)
    }
}

impl<'ctx> TySubst<'ctx> {
    /// Creates a new, empty type substitution with a given arena.
    pub fn new(arena: &'ctx Arena<'ctx>) -> Self {
        Self {
            substs: default(),
            arena,
        }
    }
}

impl<'ctx> Add<&TySubst<'ctx>> for TySubst<'ctx> {
    type Output = Self;

    fn add(mut self, rhs: &TySubst<'ctx>) -> Self::Output {
        self += rhs;
        self
    }
}

impl<'ctx> AddAssign<&TySubst<'ctx>> for TySubst<'ctx> {
    fn add_assign(&mut self, rhs: &TySubst<'ctx>) {
        self.substs.extend(
            rhs.substs
                .iter()
                .map(|&(var, ty)| (var, ty.subst(self.arena, self)))
                .collect::<Vec<_>>(),
        );
    }
}
