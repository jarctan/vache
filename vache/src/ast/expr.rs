//! Parsing expressions, and defining their representation in the AST.

use num_bigint::BigInt;
use pest::iterators::Pair;

use super::{Block, Var};
use super::{Context, Parsable};
use crate::grammar::*;
use crate::utils::boxed;

/// An expression in the parser AST.
///
/// Rule: all variants end with a capital `E`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    /// Unit expression, that does nothing.
    UnitE,
    /// An unbounded integer.
    IntegerE(BigInt),
    /// A string.
    StringE(String),
    /// A variable.
    VarE(Var),
    /// A field in a structure.
    FieldE(Box<Expr>, String),
    /// An instance of a structure.
    StructE {
        /// Name (identifier).
        name: String,
        /// Collection of field names and values.
        ///
        /// Ordered because we need to specify here the evaluation order.
        fields: Vec<(String, Expr)>,
    },
    /// An index in an array/a map.
    IndexE(Box<Expr>, Box<Expr>),
    /// Array creation.
    ArrayE(Vec<Expr>),
    /// A function call.
    CallE {
        /// Name/identifier of the function.
        name: String,
        /// Arguments to that function.
        args: Vec<Expr>,
    },
    /// An if expression.
    IfE(Box<Expr>, Box<Block>, Box<Block>),
    /// A block expression.
    BlockE(Box<Block>),
}

use Expr::*;

impl Expr {
    /// Sees this expression as a field.
    ///
    /// # Errors
    /// Returns `None` if the expression is not a field.
    pub fn as_field(&self) -> Option<(&Expr, &str)> {
        if let FieldE(box strukt, field) = self {
            Some((strukt, field))
        } else {
            None
        }
    }

    /// Sees this expression as a variable.
    ///
    /// # Errors
    /// Returns `None` if the expression is not a variable.
    pub fn as_var(&self) -> Option<&Var> {
        if let VarE(var) = self {
            Some(var)
        } else {
            None
        }
    }
}

/// Shortcut to create an `Expr` which is just a variable, based on its name.
pub fn var(v: impl ToString) -> Expr {
    VarE(v.to_string().into())
}

/// Shortcut to create a constant integer `Expr` based on some integer value.
pub fn int(value: impl Into<BigInt>) -> Expr {
    IntegerE(value.into())
}

/// Shortcut to create a constant string `String` based on some string value.
pub fn string(value: impl Into<String>) -> Expr {
    StringE(value.into())
}

/// Shortcut to create a call `Expr`.
pub fn call(name: impl ToString, stmts: impl IntoIterator<Item = Expr>) -> Expr {
    CallE {
        name: name.to_string(),
        args: stmts.into_iter().collect(),
    }
}

/// Shortcut to create a `s.field` expression.
pub fn field(e: Expr, member: impl ToString) -> Expr {
    FieldE(boxed(e), member.to_string())
}

/// Shortcut to create an if expression.
pub fn if_e(e: Expr, iftrue: impl Into<Block>, iffalse: impl Into<Block>) -> Expr {
    IfE(boxed(e), boxed(iftrue.into()), boxed(iffalse.into()))
}

/// Shortcut to create a `x[y]` expression.
pub fn index(e1: Expr, ix: Expr) -> Expr {
    IndexE(boxed(e1), boxed(ix))
}

/// Shortcut to create a `MyStruct { (field: value)* }` expression.
pub fn structure(name: impl ToString, fields: impl IntoIterator<Item = (String, Expr)>) -> Expr {
    StructE {
        name: name.to_string(),
        fields: fields.into_iter().collect(),
    }
}

/// Shortcut to create a `[el1, el2, ..]` expression.
pub fn array(items: impl IntoIterator<Item = Expr>) -> Expr {
    ArrayE(items.into_iter().collect())
}

/// Shortcut to create a block `Expr`.
pub fn block(value: impl Into<Block>) -> Expr {
    BlockE(Box::new(value.into()))
}

/// Shortcut to create a binary operation `Expr`.
pub fn binop(lhs: Expr, op: impl ToString, rhs: Expr) -> Expr {
    call(op, vec![lhs, rhs])
}

impl From<u64> for Expr {
    fn from(value: u64) -> Self {
        IntegerE(BigInt::from(value))
    }
}

impl From<Var> for Expr {
    fn from(v: Var) -> Self {
        VarE(v)
    }
}

impl<'a> Parsable<Pair<'a, Rule>> for Expr {
    fn parse(pair: Pair<'a, Rule>, ctx: &mut Context) -> Self {
        match pair.as_rule() {
            Rule::unit => UnitE,
            Rule::integer => IntegerE(ctx.parse(pair)),
            Rule::string => StringE(ctx.parse(pair)),
            Rule::ident => VarE(ctx.parse(pair)),
            Rule::with_postfix => {
                let mut pairs = pair.into_inner();
                let expr = ctx.parse(pairs.next().unwrap());
                pairs.fold(expr, |acc, pair| match pair.as_rule() {
                    Rule::field_postfix => {
                        let inner = pair.into_inner().next().unwrap();
                        if let VarE(field) = ctx.parse(inner) {
                            FieldE(boxed(acc), field.into())
                        } else {
                            panic!("Expected a variable on left-hand side of field access.")
                        }
                    }
                    Rule::index_postfix => IndexE(
                        boxed(acc),
                        boxed(ctx.parse(pair.into_inner().next().unwrap())),
                    ),
                    Rule::call_postfix => {
                        if let VarE(name) = acc {
                            CallE {
                                name: name.into(),
                                args: pair.into_inner().map(|pair| ctx.parse(pair)).collect(),
                            }
                        } else {
                            panic!("Expected a function name")
                        }
                    }
                    rule => panic!("expected postfix, found {rule:?}"),
                })
            }
            rule => panic!("expected expression, found {rule:?}"),
        }
    }
}

#[cfg(test)]
mod test {
    use pest::Parser;

    use super::*;
    use crate::grammar::Grammar;

    #[test]
    fn simple_var() {
        let input = "test123";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        assert_eq!(expr.as_var().unwrap(), input);
    }

    #[test]
    fn simple_string() {
        let input = "\"this is a test\nAnd another\"";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        assert!(matches!(expr, StringE(_)));
    }

    #[test]
    fn simple_decimal() {
        let input = "1234567890";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        assert!(matches!(expr, IntegerE(_)));
    }

    #[test]
    fn unit() {
        let input = "()";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        assert!(matches!(expr, UnitE));
    }

    #[test]
    fn field() {
        let input = "test.a";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        let field = expr.as_field().unwrap();
        let lhs = field.0.as_var().unwrap();
        let rhs = field.1;
        assert!(lhs == "test");
        assert!(rhs == "a");
    }

    #[test]
    fn nested_fields() {
        let input = "test.a.b";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        let field = expr.as_field().unwrap();
        let lhs = field.0.as_field().unwrap();
        let rhs1 = lhs.1;
        let lhs = lhs.0.as_var().unwrap();
        let rhs2 = field.1;
        assert!(lhs == "test");
        assert!(rhs1 == "a");
        assert!(rhs2 == "b");
    }

    #[test]
    fn if_then() {
        let input = "if x { y } else { z } ";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        assert!(matches!(expr, IntegerE(_)));
    }
}
