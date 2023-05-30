//! Parsing expressions, and defining their representation in the AST.

use num_bigint::BigInt;
use pest::iterators::Pair;
use pest::pratt_parser::*;

use super::{Block, Var};
use super::{Context, Parsable};
use crate::grammar::*;
use crate::utils::boxed;

lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = PrattParser::new()
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left) | Op::infix(Rule::div, Assoc::Left))
        .op(Op::infix(Rule::pow, Assoc::Right));
}

/// An expression in the parser AST.
///
/// Rule: all variants end with a capital `E`.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum Expr {
    /// Unit expression, that does nothing.
    #[default]
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
    /// Returns: `(strukt, field)`.
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

    /// Sees this expression as an index.
    ///
    /// Returns: `(array, index)`.
    ///
    /// # Errors
    /// Returns `None` if the expression is not a index.
    pub fn as_index(&self) -> Option<(&Expr, &Expr)> {
        if let IndexE(box array, box index) = self {
            Some((array, index))
        } else {
            None
        }
    }

    /// Sees this expression as an function call.
    ///
    /// Returns: `(name, args)`.
    ///
    /// # Errors
    /// Returns `None` if the expression is not a function call.
    pub fn as_call(&self) -> Option<(&str, &Vec<Expr>)> {
        if let CallE { name, args } = self {
            Some((name, args))
        } else {
            None
        }
    }

    /// Sees this expression as an integer.
    ///
    /// # Errors
    /// Returns `None` if the expression is not an integer.
    pub fn as_integer(&self) -> Option<&BigInt> {
        if let IntegerE(ref i) = self {
            Some(i)
        } else {
            None
        }
    }

    /// Sees this expression as a string.
    ///
    /// # Errors
    /// Returns `None` if the expression is not a string.
    pub fn as_string(&self) -> Option<&str> {
        if let StringE(ref s) = self {
            Some(s)
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

    /// Sees this expression as an array declaration.
    ///
    /// # Errors
    /// Returns `None` if the expression is not an array declaration.
    pub fn as_array(&self) -> Option<&Vec<Expr>> {
        if let ArrayE(ref array) = self {
            Some(array)
        } else {
            None
        }
    }

    /// Sees this expression as a if-then expression.
    ///
    /// Format of the output: `(condition, iftrue_block, iffalse_block)`.
    ///
    /// # Errors
    /// Returns `None` if the expression is not an if-then expression.
    pub fn as_if_then(&self) -> Option<(&Expr, &Block, &Block)> {
        if let IfE(box cond, box iftrue, box iffalse) = self {
            Some((cond, iftrue, iffalse))
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

impl Parsable<Pair<'_, Rule>> for Expr {
    fn parse(pair: Pair<Rule>, ctx: &mut Context) -> Self {
        match pair.as_rule() {
            Rule::expr | Rule::primitive => {
                let pair = pair.into_inner().next().unwrap();
                match pair.as_rule() {
                    Rule::unit => UnitE,
                    Rule::integer => IntegerE(ctx.parse(pair)),
                    Rule::string => StringE(ctx.parse(pair)),
                    Rule::ident => VarE(ctx.parse(pair)),
                    Rule::array => ArrayE(pair.into_inner().map(|pair| ctx.parse(pair)).collect()),
                    Rule::with_postfix => ctx.parse(pair),
                    Rule::if_then => {
                        let mut pairs = pair.into_inner();
                        let cond = ctx.parse(pairs.next().unwrap());
                        let if_block = ctx.parse(pairs.next().unwrap());
                        let else_block =
                            pairs.next().map(|pair| ctx.parse(pair)).unwrap_or_default();
                        IfE(boxed(cond), boxed(if_block), boxed(else_block))
                    }
                    Rule::binop => PRATT_PARSER
                        .map_primary(|primary| Expr::parse(primary, ctx))
                        .map_infix(|lhs, op, rhs| CallE {
                            name: op.as_str().to_owned(),
                            args: vec![lhs, rhs],
                        })
                        .parse(pair.into_inner()),
                    rule => panic!("parser internal error: expected expression, found {rule:?}"),
                }
            }
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
            _ => panic!(
                "expected primitive or an expr, found {:?} for {}",
                pair.as_rule(),
                pair.as_str()
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use pest::Parser;

    use super::super::Stmt;
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
        assert_eq!(expr.as_string().unwrap(), &input[1..input.len() - 1]);
    }

    #[test]
    fn simple_decimal() {
        let input = "1234567890";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        assert_eq!(expr.as_integer().unwrap(), &BigInt::from(1234567890));
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
    fn array_indexing() {
        let input = "test[\"a\"]";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        let (array, index) = expr.as_index().unwrap();
        assert!(array.as_var().unwrap() == "test");
        assert!(index.as_string().unwrap() == "a");
    }

    #[test]
    fn matrix_indexing() {
        let input = "blah[\"a\"][\"b\"]";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        let (nested_index, ix2) = expr.as_index().unwrap();
        let (array, ix1) = nested_index.as_index().unwrap();
        assert!(array.as_var().unwrap() == "blah");
        assert!(ix1.as_string().unwrap() == "a");
        assert!(ix2.as_string().unwrap() == "b");
    }

    #[test]
    fn mix_array_index() {
        let input = "blah.a[\"b\"]";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        let (nested, ix) = expr.as_index().unwrap();
        let (array, field) = nested.as_field().unwrap();
        assert!(array.as_var().unwrap() == "blah");
        assert!(field == "a");
        assert!(ix.as_string().unwrap() == "b");
    }

    #[test]
    fn call_expression() {
        let input = "my_call(a, b, c)";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        let (call, args) = expr.as_call().unwrap();
        assert!(call == "my_call");
        assert_eq!(
            args.iter()
                .map(|arg| arg.as_var().unwrap().as_str().to_owned())
                .collect::<Vec<String>>(),
            ["a", "b", "c"]
        );
    }

    #[test]
    fn empty_array_decl() {
        let input = "[]";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        let items = expr.as_array().unwrap();
        assert!(items
            .iter()
            .map(|arg| arg.as_integer().unwrap())
            .collect::<Vec<&BigInt>>()
            .is_empty());
    }

    #[test]
    fn array_decl() {
        let input = "[1, 2, 3, 4, 5]";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        let items = expr.as_array().unwrap();
        assert_eq!(
            items
                .iter()
                .map(|arg| arg.as_integer().unwrap())
                .collect::<Vec<&BigInt>>(),
            [&BigInt::from(1), &2.into(), &3.into(), &4.into(), &5.into()]
        );
    }

    #[test]
    fn if_then() {
        let input = "if x { y } else { z }";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        let (cond, if_block, else_block) = expr.as_if_then().unwrap();
        assert!(cond.as_var().unwrap() == "x");
        assert!(if_block.stmts.is_empty());
        assert!(if_block.ret.as_var().unwrap() == "y");
        assert!(else_block.stmts.is_empty());
        assert!(else_block.ret.as_var().unwrap() == "z");
    }

    #[test]
    fn more_complex_if_then_else() {
        let input = "if x { let k: int = 12; y } else { let w: int = 16; z }";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        let (cond, if_block, else_block) = expr.as_if_then().unwrap();
        assert!(cond.as_var().unwrap() == "x");
        assert!(matches!(&if_block.stmts[..], [Stmt::Declare(..)]));
        assert!(if_block.ret.as_var().unwrap() == "y");
        assert!(matches!(&else_block.stmts[..], [Stmt::Declare(..)]));
        assert!(else_block.ret.as_var().unwrap() == "z");
    }

    #[test]
    fn binary_operations() {
        let input = "x + 2 * 7";
        let mut parsed = Grammar::parse(Rule::expr, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let expr: Expr = ctx.parse(pair);
        eprintln!("{expr:?}");
        let (name1, args) = expr.as_call().unwrap();
        let (name2, _) = args[1].as_call().unwrap();
        assert_eq!(name1, "+");
        assert_eq!(name2, "*");
    }
}
