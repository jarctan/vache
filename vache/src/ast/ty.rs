//! Parsing types, and defining their representation in the AST.

use std::fmt;

use pest::iterators::Pair;

use super::{Context, Parsable};
use crate::grammar::*;

/// Types in our language.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
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
    StructT(String),
    /// Arrays.
    ArrayT(Box<Ty>),
}

use Ty::*;

impl Parsable<Pair<'_, Rule>> for Ty {
    fn parse(pair: Pair<Rule>, _ctx: &mut Context) -> Self {
        assert_eq!(pair.as_rule(), Rule::ty);
        let mut pairs = pair.into_inner();
        let pair = pairs.next().unwrap();
        match pair.as_rule() {
            Rule::unit => UnitT,
            Rule::bool_ty => BoolT,
            Rule::int_ty => IntT,
            Rule::str_ty => StrT,
            rule => panic!("parser internal error: expected type, found {rule:?}"),
        }
    }
}

impl Ty {
    /// Is this type `Copy` in Rust (no deep clone needed).
    pub fn copyable(&self) -> bool {
        match self {
            UnitT | BoolT => true,
            IntT | StrT | StructT(_) | ArrayT(_) => false,
        }
    }
}

impl Default for Ty {
    /// The default type: the unit type.
    fn default() -> Self {
        Self::UnitT
    }
}

impl Extend<Ty> for Ty {
    fn extend<T: IntoIterator<Item = Ty>>(&mut self, iter: T) {
        // The type of a collection of types is the last type.
        if let Some(ty) = iter.into_iter().last() {
            *self = ty;
        }
    }
}

impl Extend<()> for Ty {
    fn extend<T: IntoIterator<Item = ()>>(&mut self, _: T) {}
}

impl From<Ty> for () {
    fn from(_: Ty) -> Self {}
}

impl fmt::Display for Ty {
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
    use pest::Parser;

    use super::*;
    use crate::grammar::Grammar;

    #[test]
    fn unit_ty() {
        let input = "()";
        let mut parsed = Grammar::parse(Rule::ty, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let ty: Ty = ctx.parse(pair);
        assert_eq!(ty, UnitT);
    }

    #[test]
    fn str_ty() {
        let input = "str";
        let mut parsed = Grammar::parse(Rule::ty, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let ty: Ty = ctx.parse(pair);
        assert_eq!(ty, StrT);
    }

    #[test]
    fn int_ty() {
        let input = "int";
        let mut parsed = Grammar::parse(Rule::ty, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let ty: Ty = ctx.parse(pair);
        assert_eq!(ty, IntT);
    }

    #[test]
    fn bool_ty() {
        let input = "bool";
        let mut parsed = Grammar::parse(Rule::ty, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let ty: Ty = ctx.parse(pair);
        assert_eq!(ty, BoolT);
    }
}
