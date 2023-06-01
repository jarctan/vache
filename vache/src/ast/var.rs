//! Parsing types, and defining their representation in the AST.

use std::fmt;

use pest::iterators::Pair;

use super::{Context, Parsable, Ty};
use crate::utils::boxed;
use crate::{grammar::*, Arena};

/// A variable in the code.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Var<'ctx>(&'ctx str);

impl<'ctx> Var<'ctx> {
    /// A control-flow graph variable.
    ///
    /// These are variables used internally by the CFG, that starts with `__cfg`
    /// followed by a unique numeral ID.
    pub(crate) fn cfg(arena: &'ctx Arena, number: u64) -> Var<'ctx> {
        let value = arena.alloc(boxed(format!("__cfg{number:?}")));
        Var(value)
    }

    /// See the variable as a string.
    pub fn as_str(&self) -> &str {
        self.as_ref()
    }
}

impl<'ctx> AsRef<Var<'ctx>> for Var<'ctx> {
    fn as_ref(&self) -> &Var<'ctx> {
        self
    }
}

impl<'ctx> From<Var<'ctx>> for &'ctx str {
    fn from(value: Var<'ctx>) -> Self {
        value.0
    }
}

impl AsRef<str> for Var<'_> {
    fn as_ref(&self) -> &str {
        self.0
    }
}

impl<'ctx> From<&'ctx str> for Var<'ctx> {
    fn from(v: &'ctx str) -> Self {
        Self(v)
    }
}

impl fmt::Debug for Var<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}

impl fmt::Display for Var<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Var<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, _ctx: &mut Context) -> Self {
        assert!(pair.as_rule() == Rule::ident);
        Var(pair.as_str())
    }
}

impl<'ctx> PartialEq<str> for Var<'ctx> {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl<'ctx> PartialEq<&str> for Var<'ctx> {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other
    }
}

/// A variable definition.
#[derive(Clone, PartialEq, Eq)]
pub struct VarDef<'ctx> {
    /// Variable name.
    pub(crate) name: Var<'ctx>,
    /// Type of the variable.
    pub(crate) ty: Ty<'ctx>,
}

impl fmt::Debug for VarDef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{}", self.name, self.ty)
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

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for VarDef<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &mut Context<'ctx>) -> Self {
        assert!(pair.as_rule() == Rule::vardef);
        let mut pairs = pair.into_inner();

        let name = ctx.parse(pairs.next().unwrap());
        let ty = ctx.parse(pairs.next().unwrap());

        VarDef { name, ty }
    }
}

/// Shortcut to create a new variable definition.
pub fn vardef<'ctx>(name: &'ctx str, ty: Ty<'ctx>) -> VarDef<'ctx> {
    let name = name.into();
    VarDef { name, ty }
}

#[cfg(test)]
mod tests {
    use pest::Parser;
    use Ty::*;

    use super::*;
    use crate::grammar::Grammar;

    #[parses("test123" as ident)]
    #[test]
    fn simple_var(var: Var) {
        assert_eq!(var, input);
    }

    #[parses("test: str" as vardef)]
    #[test]
    fn simple_vardef(vardef: VarDef) {
        assert_eq!(vardef.name, "test");
        assert_eq!(vardef.ty, StrT);
    }
}
