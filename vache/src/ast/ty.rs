//! Parsing types, and defining their representation in the AST.

use std::default::default;
use std::fmt;
use std::ops::Deref;

use pest::iterators::Pair;

use super::{Context, Parsable, Span};
use crate::grammar::*;

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

/// Shortcut for the `StructT` without any span context.
#[allow(non_snake_case)]
pub fn structT(name: &str) -> TyUse<'_> {
    StructT(name).into()
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

/// Types in our language.
///
/// NB: is `PartialEq` but NOT `Eq` because of `HoleT` that is equal to every
/// other element.
#[derive(Clone, Copy, Default)]
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
    /// Arrays.
    ArrayT(&'ctx Ty<'ctx>),
    /// Iterator.
    IterT(&'ctx Ty<'ctx>),
    /// Hole type.
    #[default]
    HoleT,
}

use Ty::*;

impl<'ctx> Ty<'ctx> {
    /// Does this type reduce to an array type.
    ///
    /// If so, returns the type of the items.
    pub fn as_array(&self) -> Option<Ty<'ctx>> {
        match self {
            ArrayT(inner) => Some(**inner),
            HoleT => Some(HoleT),
            _ => None,
        }
    }

    /// Does this type reduce to an iter type.
    ///
    /// If so, returns the type of the items.
    pub fn as_iter(&self) -> Option<Ty<'ctx>> {
        match self {
            IterT(inner) => Some(**inner),
            HoleT => Some(HoleT),
            _ => None,
        }
    }

    /// Does this type reduce to a int type.
    pub fn is_int(&self) -> bool {
        matches!(self, IntT | HoleT)
    }

    /// Adds codespan information to this type to transform it into a `TyUse`.
    pub(crate) fn with_span(self, span: impl Into<Span>) -> TyUse<'ctx> {
        TyUse {
            kind: self,
            span: span.into(),
        }
    }
}

impl<'ctx> PartialEq for Ty<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        // Convoluted way of pattern matching, but this way we will get a compile
        // error if we add a new variant but forget to handle it here.
        match (self, other) {
            (HoleT, _) | (_, HoleT) => true,
            (UnitT, UnitT) => true,
            (UnitT, _) => false,
            (BoolT, BoolT) => true,
            (BoolT, _) => false,
            (IntT, IntT) => true,
            (IntT, _) => false,
            (StrT, StrT) => true,
            (StrT, _) => false,
            (ArrayT(inner1), ArrayT(inner2)) => inner1 == inner2,
            (ArrayT(..), _) => false,
            (IterT(inner1), IterT(inner2)) => inner1 == inner2,
            (IterT(..), _) => false,
            (StructT(name1), StructT(name2)) => name1 == name2,
            (StructT(..), _) => false,
        }
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for TyUse<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &mut Context<'ctx>) -> Self {
        assert!(matches!(pair.as_rule(), Rule::ty | Rule::non_iter_ty));
        let span = Span::from(pair.as_span());
        let mut pairs = pair.into_inner();
        let pair = pairs.next().unwrap();
        let kind = match pair.as_rule() {
            Rule::unit => UnitT,
            Rule::bool_ty => BoolT,
            Rule::int_ty => IntT,
            Rule::str_ty => StrT,
            Rule::array_ty => {
                let inner: TyUse = ctx.parse(pair.into_inner().next().unwrap());
                ArrayT(ctx.alloc(inner.kind))
            }
            Rule::iter_ty => {
                let mut pairs = pair.into_inner();
                let inner: TyUse = ctx.parse(pairs.next().unwrap());
                let mut res = inner.kind;
                // For each `..`, add a layer of indirection
                for _ in pairs {
                    res = IterT(ctx.alloc(res));
                }
                res
            }
            Rule::ident => StructT(pair.as_str()),
            rule => panic!("parser internal error: expected type, found {rule:?}"),
        };
        Self { kind, span }
    }
}

impl Ty<'_> {
    /// Is this type `Copy` in Rust (no deep clone needed).
    pub fn copyable(&self) -> bool {
        match self {
            UnitT | BoolT => true,
            IntT | StrT | StructT(_) | ArrayT(_) | IterT(_) | HoleT => false,
        }
    }
}

impl<'ctx> Extend<Ty<'ctx>> for Ty<'ctx> {
    fn extend<T: IntoIterator<Item = Ty<'ctx>>>(&mut self, iter: T) {
        // The type of a collection of types is the last type.
        if let Some(ty) = iter.into_iter().last() {
            *self = ty;
        }
    }
}

impl Extend<()> for Ty<'_> {
    fn extend<T: IntoIterator<Item = ()>>(&mut self, _: T) {}
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
            ArrayT(ty) => write!(f, "[{ty}]"),
            IterT(ty) => write!(f, "{ty}.."),
            HoleT => write!(f, "?"),
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
        assert_eq!(ty, BoolT);
    }

    #[parses("bool.." as ty)]
    #[test]
    fn bool_iter(ty: TyUse) {
        ty.as_iter().context("expected an iterator")?;
    }

    #[parses("bool......" as ty)]
    #[test]
    fn bool_iter_iter(ty: TyUse) -> Result<()> {
        ty.as_iter()
            .context("expected an iterator")?
            .as_iter()
            .context("expected an iterator")?;
    }
}
