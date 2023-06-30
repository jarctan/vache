//! Defining types in general.

use std::default::default;
use std::fmt;

use itertools::Itertools;

use super::*;
use crate::ast::Span;
use crate::utils::set::Set;
use crate::Arena;

/// Types in our language.
///
/// NB: use unification instead of equality on types if you deal with type
/// variables.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Ty<'ctx> {
    /// Unit type.
    UnitT,
    /// The usual boolean type.
    BoolT,
    /// An unbounded integer.
    IntT,
    /// The string type.
    StrT,
    /// Structures.
    ///
    /// Structures are identified by their names.
    StructT(&'ctx str),
    /// Enumerated types.
    ///
    /// Structures are identified by their names.
    EnumT(&'ctx str),
    /// Type variable.
    VarT(TyVar<'ctx>),
    /// Arrays.
    ArrayT(&'ctx Ty<'ctx>),
    /// Tuples.
    TupleT(&'ctx [Ty<'ctx>]),
    /// Iterator.
    IterT(&'ctx Ty<'ctx>),
}

use Ty::*;

impl<'ctx> Ty<'ctx> {
    /// The hole type. Takes a `span` as argument to relate that hole type to
    /// some actual codespan (useful for error reporting).
    pub fn hole(span: Span) -> Self {
        VarT(TyVar::fresh(span))
    }

    /// Inner function to substitute a type variable `from` for `to` in `self`,
    /// returning a new type. DO NOT use this function directly.
    ///
    /// Result:
    /// * if `Ok(x)`, then some changes/substitutions have been made
    /// * if `Err(y)`, then y is guaranteed to be the original type without
    ///   change, NO substitution has been made/could be applied.
    fn _subst_var(
        &self,
        arena: &'ctx Arena<'ctx>,
        from: TyVar<'ctx>,
        to: Ty<'ctx>,
    ) -> Result<Self, Self> {
        match *self {
            self_ty @ (UnitT | BoolT | IntT | StrT | StructT(_) | EnumT(_)) => Err(self_ty),
            self_ty @ VarT(v) => {
                if v == from {
                    Ok(to)
                } else {
                    Err(self_ty)
                }
            }
            self_ty @ TupleT(items) => {
                let items: Vec<_> = items
                    .iter()
                    .map(|item| item._subst_var(arena, from, to))
                    .collect();
                if items.iter().any(|item| item.is_ok()) {
                    let items: &[Ty] = arena.alloc(
                        items
                            .into_iter()
                            .map(|item| item.unwrap_or_else(|x| x))
                            .collect::<Vec<_>>(),
                    );
                    Ok(TupleT(items))
                } else {
                    Err(self_ty)
                }
            }
            self_ty @ ArrayT(inner) => match inner._subst_var(arena, from, to) {
                Ok(new) => Ok(ArrayT(arena.alloc(new))),
                Err(_) => Err(self_ty),
            },
            self_ty @ IterT(inner) => match inner._subst_var(arena, from, to) {
                Ok(new) => Ok(IterT(arena.alloc(new))),
                Err(_) => Err(self_ty),
            },
        }
    }

    /// Substitutes a type variable `from` for `to` in `self`, returning a new
    /// type.
    pub(crate) fn subst_var(
        &self,
        arena: &'ctx Arena<'ctx>,
        from: TyVar<'ctx>,
        to: Ty<'ctx>,
    ) -> Self {
        self._subst_var(arena, from, to).unwrap_or_else(|x| x)
    }

    /// Applies a type substitution from `subst` in `self`, returning a
    /// new type.
    pub(crate) fn subst(&self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        subst
            .substs
            .iter()
            .fold(*self, |acc, &(var, ty)| acc.subst_var(arena, var, ty))
    }

    /// Returns the free type variables in `self`.
    pub fn free_vars(&self) -> Set<TyVar<'ctx>> {
        match *self {
            UnitT | BoolT | IntT | StrT => default(),
            StructT(_) | EnumT(_) => default(),
            VarT(v) => [v].into_iter().collect(),
            ArrayT(array) => array.free_vars(),
            IterT(iter) => iter.free_vars(),
            TupleT(items) => items.iter().map(|item| item.free_vars()).sum(),
        }
    }

    /// Does this type variable occur in this type?
    pub fn occurs(&self, var: TyVar<'ctx>) -> bool {
        match *self {
            UnitT | BoolT | IntT | StrT => false,
            StructT(_) | EnumT(_) => false,
            VarT(v) => v == var,
            ArrayT(array) => array.occurs(var),
            IterT(iter) => iter.occurs(var),
            TupleT(items) => items.iter().any(|item| item.occurs(var)),
        }
    }

    /// Does this type reduce to a named type variable.
    ///
    /// If so, returns the variable name.
    pub fn as_named_var(&self) -> Option<&'ctx str> {
        match self {
            VarT(TyVar::Named(name)) => Some(name),
            _ => None,
        }
    }

    /// Adds codespan information to this type to transform it into a `TyUse`.
    pub(crate) fn with_span(self, span: impl Into<Span>) -> TyUse<'ctx> {
        TyUse {
            kind: self,
            span: span.into(),
        }
    }

    /// Tries to unify two types.
    ///
    /// If it succeeds, it returns the substitution that need to be applied to
    /// see both types as "equal". Otherwise, returns `None`.
    pub fn unify(&self, other: &Self, arena: &'ctx Arena<'ctx>) -> Option<TySubst<'ctx>> {
        // Convoluted way of pattern matching, but this way we will get a compile
        // error if we add a new variant but forget to handle it here.
        match (*self, *other) {
            (VarT(name), ty) | (ty, VarT(name)) => {
                if !ty.occurs(name) {
                    Some(TySubst {
                        arena,
                        substs: vec![(name, ty)],
                    })
                } else {
                    None
                }
            }
            (UnitT, UnitT) => Some(TySubst::new(arena)),
            (UnitT, _) => None,
            (BoolT, BoolT) => Some(TySubst::new(arena)),
            (BoolT, _) => None,
            (IntT, IntT) => Some(TySubst::new(arena)),
            (IntT, _) => None,
            (StrT, StrT) => Some(TySubst::new(arena)),
            (StrT, _) => None,
            (ArrayT(inner1), ArrayT(inner2)) => inner1.unify(inner2, arena),
            (ArrayT(..), _) => None,
            (TupleT(items1), TupleT(items2)) => {
                if items1.len() == items2.len() {
                    items1
                        .iter()
                        .zip(items2.iter())
                        .map(|(i1, i2)| i1.unify(i2, arena))
                        .fold(Some(TySubst::new(arena)), |acc, el| match (acc, el) {
                            (Some(acc), Some(el)) => Some(acc + &el),
                            _ => None,
                        })
                } else {
                    None
                }
            }
            (TupleT(..), _) => None,
            (IterT(inner1), IterT(inner2)) => inner1.unify(inner2, arena),
            (IterT(..), _) => None,
            (StructT(name1), StructT(name2)) => (name1 == name2).then_some(TySubst::new(arena)),
            (StructT(..), _) => None,
            (EnumT(name1), EnumT(name2)) => (name1 == name2).then_some(TySubst::new(arena)),
            (EnumT(..), _) => None,
        }
    }
}

impl Default for Ty<'_> {
    fn default() -> Self {
        Self::hole(Span::default())
    }
}

impl Ty<'_> {
    /// Is this type [`Copy`] in Rust (if bitwise copy is sufficient).
    pub fn copyable(&self) -> bool {
        match self {
            UnitT | BoolT => true,
            IntT | StrT | StructT(_) | EnumT(_) | ArrayT(_) | IterT(_) => false,
            // Tuple is copy only if all items are copy
            TupleT(items) => items.iter().all(|item| item.copyable()),
            VarT(_) => panic!("We don't know!"),
        }
    }
}

impl<'ctx> From<Ty<'ctx>> for () {
    fn from(_: Ty<'ctx>) -> Self {}
}

impl fmt::Display for Ty<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnitT => write!(f, "()"),
            BoolT => write!(f, "bool"),
            IntT => write!(f, "int"),
            StrT => write!(f, "str"),
            StructT(s) => write!(f, "{s}"),
            EnumT(e) => write!(f, "{e}"),
            VarT(v) => write!(f, "{v}"),
            ArrayT(ty) => write!(f, "[{ty}]"),
            TupleT(items) => write!(f, "({})", items.iter().join(", ")),
            IterT(ty) => write!(f, "{ty}.."),
        }
    }
}

impl fmt::Debug for Ty<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}
