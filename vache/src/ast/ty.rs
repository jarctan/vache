//! Parsing types, and defining their representation in the AST.

use std::fmt;

use pest::iterators::Pair;

use super::{Context, Parsable};
use crate::grammar::*;
use crate::utils::boxed;

/// Types in our language.
#[derive(Debug, Clone, PartialEq, Eq)]
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
    ArrayT(Box<Ty<'ctx>>),
}

use Ty::*;

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Ty<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &mut Context<'ctx>) -> Self {
        assert_eq!(pair.as_rule(), Rule::ty);
        let mut pairs = pair.into_inner();
        let pair = pairs.next().unwrap();
        match pair.as_rule() {
            Rule::unit => UnitT,
            Rule::bool_ty => BoolT,
            Rule::int_ty => IntT,
            Rule::str_ty => StrT,
            Rule::array_ty => ArrayT(boxed(ctx.parse(pair.into_inner().next().unwrap()))),
            Rule::ident => StructT(pair.as_str()),
            rule => panic!("parser internal error: expected type, found {rule:?}"),
        }
    }
}

impl Ty<'_> {
    /// Is this type `Copy` in Rust (no deep clone needed).
    pub fn copyable(&self) -> bool {
        match self {
            UnitT | BoolT => true,
            IntT | StrT | StructT(_) | ArrayT(_) => false,
        }
    }
}

impl Default for Ty<'_> {
    /// The default type: the unit type.
    fn default() -> Self {
        Self::UnitT
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
            StructT(s) => write!(f, "{s}{{}}"),
            ArrayT(box ty) => write!(f, "{ty}[]"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[parses("()" as ty)]
    #[test]
    fn unit_ty(ty: Ty) {
        assert_eq!(ty, UnitT);
    }

    #[parses("str" as ty)]
    #[test]
    fn str_ty(ty: Ty) {
        assert_eq!(ty, StrT);
    }

    #[parses("int" as ty)]
    #[test]
    fn int_ty(ty: Ty) {
        assert_eq!(ty, IntT);
    }

    #[parses("bool" as ty)]
    #[test]
    fn bool_ty(ty: Ty) {
        assert_eq!(ty, BoolT);
    }
}
