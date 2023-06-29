//! Parsing types, and defining their representation in the AST.

use std::default::default;
use std::fmt;
use std::ops::{Add, Deref};
use std::sync::atomic::AtomicU64;

use itertools::Itertools;
use pest::iterators::Pair;

use super::{Context, Parsable, Span};
use crate::utils::set::Set;
use crate::{grammar::*, Arena};

/// Fresh type variable counter.
///
/// Global to avoid any confusion between type variable names.
pub static TY_VAR_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Debug)]
pub struct TySubst<'ctx> {
    substs: Vec<(TyVar<'ctx>, Ty<'ctx>)>,
    arena: &'ctx Arena<'ctx>,
}

impl<'ctx> TySubst<'ctx> {
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
        self.substs.extend(
            rhs.substs
                .iter()
                .map(|&(var, ty)| (var, ty.subst(self.arena, &self)))
                .collect::<Vec<_>>(),
        );
        self
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyVar<'ctx> {
    Named(&'ctx str),
    Gen(u64),
}

impl<'ctx> TyVar<'ctx> {
    fn fresh() -> Self {
        let id = TY_VAR_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        Self::Gen(id)
    }
}

impl<'ctx> fmt::Display for TyVar<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TyVar::Named(name) => write!(f, "{name}"),
            TyVar::Gen(id) => write!(f, "τ{id}"),
        }
    }
}

impl<'ctx> fmt::Debug for TyVar<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

/// A type in the source code.
///
/// This is a type + its use position in the source code.
#[derive(Clone, Copy, Default)]
pub struct TyUse<'ctx> {
    /// Type.
    pub kind: Ty<'ctx>,
    /// Span in the code for that use.
    pub span: Span,
}

impl<'ctx> Deref for TyUse<'ctx> {
    type Target = Ty<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

/// Shortcut for the `UnitT` without any span context.
#[allow(non_snake_case)]
pub fn unitT<'ctx>() -> TyUse<'ctx> {
    UnitT.into()
}

/// Shortcut for the `BoolT` without any span context.
#[allow(non_snake_case)]
pub fn boolT<'ctx>() -> TyUse<'ctx> {
    BoolT.into()
}

/// Shortcut for the `StrT` without any span context.
#[allow(non_snake_case)]
pub fn strT<'ctx>() -> TyUse<'ctx> {
    StrT.into()
}

/// Shortcut for the `IntT` without any span context.
#[allow(non_snake_case)]
pub fn intT<'ctx>() -> TyUse<'ctx> {
    IntT.into()
}

/// Shortcut for the `VarT` without any span context.
#[allow(non_snake_case)]
pub fn varT(name: &str) -> TyUse<'_> {
    VarT(TyVar::Named(name)).into()
}

/// Shortcut for the `ArrayT` without any span context.
#[allow(non_snake_case)]
pub fn arrayT<'ctx>(item: &'ctx Ty<'ctx>) -> TyUse<'ctx> {
    ArrayT(item).into()
}

impl<'ctx> PartialEq for TyUse<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        // We add this line to trigger an error if we add a new field without
        // changing these lines consequently.
        let Self { kind, span: _ } = *self;

        kind == other.kind
    }
}

impl<'ctx> PartialEq<Ty<'ctx>> for TyUse<'ctx> {
    fn eq(&self, other: &Ty<'ctx>) -> bool {
        self.kind == *other
    }
}

impl<'ctx> PartialEq<TyUse<'ctx>> for Ty<'ctx> {
    fn eq(&self, other: &TyUse<'ctx>) -> bool {
        self == &other.kind
    }
}

impl<'ctx> From<TyUse<'ctx>> for Ty<'ctx> {
    fn from(value: TyUse<'ctx>) -> Self {
        value.kind
    }
}

impl<'ctx> From<Ty<'ctx>> for TyUse<'ctx> {
    fn from(kind: Ty<'ctx>) -> Self {
        // TyUse with no span.
        TyUse { kind, ..default() }
    }
}

impl fmt::Display for TyUse<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl fmt::Debug for TyUse<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

/// Parametrized type.
#[derive(Debug, Clone, Copy)]
pub struct GenTy<'ctx> {
    quantifiers: &'ctx [TyVar<'ctx>],
    ty: Ty<'ctx>,
}

impl<'ctx> GenTy<'ctx> {
    pub fn subst_var(&self, arena: &'ctx Arena<'ctx>, from: TyVar<'ctx>, to: Ty<'ctx>) -> Self {
        if self.quantifiers.iter().all(|&el| el != from) {
            Self {
                quantifiers: self.quantifiers,
                ty: self.ty.subst_var(arena, from, to),
            }
        } else {
            *self
        }
    }

    pub fn subst(&self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        subst
            .substs
            .iter()
            .fold(*self, |acc, &(var, ty)| acc.subst_var(arena, var, ty))
    }

    pub fn free_vars(&self) -> Set<TyVar<'ctx>> {
        self.ty.free_vars() - self.quantifiers.iter()
    }
}

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
    pub fn hole() -> Self {
        VarT(TyVar::fresh())
    }

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

    pub fn subst_var(&self, arena: &'ctx Arena<'ctx>, from: TyVar<'ctx>, to: Ty<'ctx>) -> Self {
        self._subst_var(arena, from, to).unwrap_or_else(|x| x)
    }

    pub fn subst(&self, arena: &'ctx Arena<'ctx>, subst: &TySubst<'ctx>) -> Self {
        subst
            .substs
            .iter()
            .fold(*self, |acc, &(var, ty)| acc.subst_var(arena, var, ty))
    }

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
        Self::hole()
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for TyUse<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        assert!(matches!(pair.as_rule(), Rule::ty | Rule::non_iter_ty));
        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner();
        let pair = consume!(pairs);
        let kind = match pair.as_rule() {
            Rule::unit => UnitT,
            Rule::bool_ty => BoolT,
            Rule::int_ty => IntT,
            Rule::str_ty => StrT,
            Rule::array_ty => {
                let mut pairs = pair.into_inner();
                consume!(pairs, Rule::lb);
                let inner: TyUse = ctx.parse(consume!(pairs));
                consume!(pairs, Rule::rb);
                ArrayT(ctx.alloc(inner.kind))
            }
            Rule::tuple_ty => {
                let mut pairs = pair.into_inner();
                consume!(pairs, Rule::lp);
                consume_back!(pairs, Rule::rp);
                let items: Vec<_> = pairs
                    .filter(|pair| !matches!(pair.as_rule(), Rule::cma))
                    .map(|item| TyUse::parse(item, ctx).kind)
                    .collect();
                let items: &'ctx [Ty] = ctx.alloc(items);
                TupleT(items)
            }
            Rule::iter_ty => {
                let mut pairs = pair.into_inner();
                let inner: TyUse = ctx.parse(consume!(pairs));
                let mut res = inner.kind;
                // For each `..`, add a layer of indirection
                for pair in pairs {
                    debug_assert!(matches!(pair.as_rule(), Rule::rg));
                    res = IterT(ctx.alloc(res));
                }
                res
            }
            Rule::ident => VarT(TyVar::Named(pair.as_str())),
            rule => panic!("parser internal error: expected type, found {rule:?}"),
        };
        Self { kind, span }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[parses("()" as ty)]
    #[test]
    fn unit_ty(ty: TyUse) {
        assert_eq!(ty, UnitT);
    }

    #[parses("str" as ty)]
    #[test]
    fn str_ty(ty: TyUse) {
        assert_eq!(ty, StrT);
    }

    #[parses("int" as ty)]
    #[test]
    fn int_ty(ty: TyUse) {
        assert_eq!(ty, IntT);
    }

    #[parses("bool" as ty)]
    #[test]
    fn bool_ty(ty: TyUse) {
        ensure!(ty == BoolT);
    }

    #[parses("bool.." as ty)]
    #[test]
    fn bool_iter(ty: TyUse) {
        let expected = IterT(arena.alloc(Ty::hole()));
        ty.unify(&expected, &arena)
            .context("expected an iterator")?;
    }

    #[parses("(bool, int)" as ty)]
    #[test]
    fn tuple_ty(ty: TyUse) {
        let ty1 = Ty::hole();
        let ty2 = Ty::hole();
        let expected = TupleT(arena.alloc(vec![ty1, ty2]));
        let substs = ty.unify(&expected, &arena).context("expected a tuple")?;
        let ty1 = ty1.subst(&arena, &substs);
        let ty2 = ty2.subst(&arena, &substs);
        ensure!(ty1 == BoolT);
        ensure!(ty2 == IntT);
    }

    #[parses("bool......" as ty)]
    #[test]
    fn bool_iter_iter(ty: TyUse) -> Result<()> {
        let expected = IterT(arena.alloc(IterT(arena.alloc(Ty::hole()))));
        ty.unify(&expected, &arena)
            .context("expected an iterator of iterators")?;
    }

    #[parses("blah" as ty)]
    #[test]
    fn type_var(ty: TyUse) -> Result<()> {
        ty.as_named_var().context("expected a type variable")?;
    }
}
