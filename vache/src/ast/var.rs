//! Parsing types, and defining their representation in the AST.

use std::fmt;

use pest::iterators::Pair;

use super::{Context, Parsable, Ty};
use crate::grammar::*;

/// A variable in the code.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Var(String);

impl Var {
    /// A control-flow graph variable.
    ///
    /// These are variables used internally by the CFG, that starts with `__cfg`
    /// followed by a unique numeral ID.
    pub(crate) fn cfg(number: u64) -> Var {
        Var(format!("__cfg{number:?}"))
    }

    /// See the variable as a string.
    pub fn as_str(&self) -> &str {
        self.as_ref()
    }
}

impl AsRef<Var> for Var {
    fn as_ref(&self) -> &Var {
        self
    }
}

impl From<Var> for String {
    fn from(value: Var) -> Self {
        value.0
    }
}

impl AsRef<str> for Var {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl From<&str> for Var {
    fn from(v: &str) -> Self {
        Self(v.to_owned())
    }
}

impl From<String> for Var {
    fn from(v: String) -> Self {
        Self(v)
    }
}

impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> Parsable<Pair<'a, Rule>> for Var {
    fn parse(pair: Pair<'a, Rule>, _ctx: &mut Context) -> Self {
        assert!(pair.as_rule() == Rule::ident);
        Var(pair.as_str().to_owned())
    }
}

impl PartialEq<str> for Var {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl PartialEq<&str> for Var {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other
    }
}

/// A variable definition.
#[derive(Clone, PartialEq, Eq)]
pub struct VarDef {
    /// Variable name.
    pub(crate) name: Var,
    /// Type of the variable.
    pub(crate) ty: Ty,
}

impl fmt::Debug for VarDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{}", self.name, self.ty)
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

/// Shortcut to create a new variable definition.
pub fn vardef(name: impl ToString, ty: Ty) -> VarDef {
    let name = name.to_string().into();
    VarDef { name, ty }
}

#[cfg(test)]
mod test {
    use pest::Parser;

    use super::*;
    use crate::grammar::Grammar;

    #[test]
    fn simple_var() {
        let input = "test123";
        let mut parsed = Grammar::parse(Rule::ident, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let var: Var = ctx.parse(pair);
        assert_eq!(var, input);
    }
}
