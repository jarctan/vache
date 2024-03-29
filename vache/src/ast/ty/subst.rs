//! Defining substitutions for types.

use std::collections::HashMap;
use std::default::default;
use std::fmt;
use std::ops::{Add, AddAssign, Sub, SubAssign};

use itertools::Itertools;

use super::*;
use crate::Arena;

/// A type substitution: collection of type variables and their type mapping.
#[derive(Clone)]
pub struct TySubst<'ctx> {
    /// Actual substitutions.
    pub(super) subst: HashMap<TyVar<'ctx>, Ty<'ctx>>,
    /// Reference to the [`Arena`].
    ///
    /// Used when we merge two [`TySubst`], since we need to apply one
    /// [`TySubst`] to the other.
    pub(super) arena: &'ctx Arena<'ctx>,
}

impl<'ctx> TySubst<'ctx> {
    /// Creates a new, empty type substitution with a given arena.
    pub fn new(arena: &'ctx Arena<'ctx>) -> Self {
        Self {
            subst: default(),
            arena,
        }
    }

    /// Creates a new type substitution out of an iterator.
    pub fn from(
        arena: &'ctx Arena<'ctx>,
        subst: impl IntoIterator<Item = (TyVar<'ctx>, Ty<'ctx>)>,
    ) -> Self {
        Self {
            subst: subst.into_iter().collect(),
            arena,
        }
    }

    /// Gets the substitution for a given type variable, if any.
    pub fn get(&self, var: TyVar<'ctx>) -> Option<Ty<'ctx>> {
        Some(*self.subst.get(&var)?)
    }

    /// Inserts a new type substitution for `from` to `to` (in the latest
    /// substitution scope).
    ///
    /// Returns the previous substitution for that type and in the same scope,
    /// if any.
    pub(crate) fn insert(&mut self, from: TyVar<'ctx>, to: Ty<'ctx>) -> Option<Ty<'ctx>> {
        self.subst.insert(from, to)
    }
}

impl<'ctx> fmt::Debug for TySubst<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.subst)
    }
}

impl<'ctx> Add<&TySubst<'ctx>> for TySubst<'ctx> {
    type Output = Self;

    fn add(mut self, rhs: &TySubst<'ctx>) -> Self::Output {
        self += rhs;
        self
    }
}

impl<'a, 'ctx: 'a, I: IntoIterator<Item = &'a TyVar<'ctx>>> SubAssign<I> for TySubst<'ctx> {
    fn sub_assign(&mut self, rhs: I) {
        for i in rhs {
            self.subst.remove(i);
        }
    }
}

impl<'a, 'ctx: 'a, I: IntoIterator<Item = &'a TyVar<'ctx>>> Sub<I> for TySubst<'ctx> {
    type Output = Self;

    fn sub(mut self, rhs: I) -> Self::Output {
        self -= rhs;
        self
    }
}

impl<'ctx> AddAssign<&TySubst<'ctx>> for TySubst<'ctx> {
    fn add_assign(&mut self, rhs: &TySubst<'ctx>) {
        self.subst.extend(
            rhs.subst
                .iter()
                .map(|(var, ty)| (*var, ty.subst(self.arena, self)))
                .collect_vec(),
        );
    }
}
