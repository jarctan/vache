//! Parsing expressions, and defining their representation in the AST.

use num_bigint::BigInt;
use pest::iterators::Pair;
use pest::pratt_parser::*;
use Place::*;

use super::{Block, Context, Parsable, Place, Var};
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
#[derive(Debug, Clone, Default)]
pub enum Expr<'ctx> {
    /// Unit expression, that does nothing.
    #[default]
    UnitE,
    /// An unbounded integer.
    IntegerE(BigInt),
    /// A string.
    StringE(&'ctx str),
    /// A variable.
    PlaceE(Place<'ctx>),
    /// An instance of a structure.
    StructE {
        /// Name (identifier).
        name: &'ctx str,
        /// Collection of field names and values.
        ///
        /// Ordered because we need to specify here the evaluation order.
        fields: Vec<(&'ctx str, Expr<'ctx>)>,
    },
    /// Array creation.
    ArrayE(Vec<Expr<'ctx>>),
    /// A function call.
    CallE {
        /// Name/identifier of the function.
        name: &'ctx str,
        /// Arguments to that function.
        args: Vec<Expr<'ctx>>,
    },
    /// An if expression.
    IfE(Box<Expr<'ctx>>, Box<Block<'ctx>>, Box<Block<'ctx>>),
    /// A block expression.
    BlockE(Box<Block<'ctx>>),
}

use Expr::*;

impl<'ctx> Expr<'ctx> {
    /// Sees this expression as a field.
    ///
    /// Returns: `(strukt, field)`.
    ///
    /// # Errors
    /// Returns `None` if the expression is not a field.
    pub fn as_field(&self) -> Option<(&Expr<'ctx>, &'ctx str)> {
        if let PlaceE(FieldP(box strukt, field)) = self {
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
    pub fn as_index(&self) -> Option<(&Expr<'ctx>, &Expr<'ctx>)> {
        if let PlaceE(IndexP(box array, box index)) = self {
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
    pub fn as_call(&self) -> Option<(&'ctx str, &[Expr<'ctx>])> {
        if let CallE { name, args } = self {
            Some((name, args.as_slice()))
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
    pub fn as_string(&self) -> Option<&'ctx str> {
        if let StringE(s) = self {
            Some(s)
        } else {
            None
        }
    }

    /// Sees this expression as a variable.
    ///
    /// # Errors
    /// Returns `None` if the expression is not a variable.
    pub fn as_var(&self) -> Option<Var<'ctx>> {
        if let PlaceE(VarP(var)) = self {
            Some(*var)
        } else {
            None
        }
    }

    /// Sees this expression as an array declaration.
    ///
    /// # Errors
    /// Returns `None` if the expression is not an array declaration.
    pub fn as_array(&self) -> Option<&[Expr<'ctx>]> {
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
    pub fn as_if_then(&self) -> Option<(&Expr<'ctx>, &Block<'ctx>, &Block<'ctx>)> {
        if let IfE(box cond, box iftrue, box iffalse) = self {
            Some((cond, iftrue, iffalse))
        } else {
            None
        }
    }
}

/// Shortcut to create an `Expr` which is just a variable, based on its name.
pub fn var(v: &str) -> Expr<'_> {
    PlaceE(VarP(v.into()))
}

/// Shortcut to create a constant integer `Expr` based on some integer value.
pub fn int<'ctx>(value: impl Into<BigInt>) -> Expr<'ctx> {
    IntegerE(value.into())
}

/// Shortcut to create a constant string `String` based on some string value.
pub fn string(value: &str) -> Expr<'_> {
    StringE(value)
}

/// Shortcut to create a call `Expr`.
pub fn call<'ctx>(name: &'ctx str, stmts: impl IntoIterator<Item = Expr<'ctx>>) -> Expr<'ctx> {
    CallE {
        name,
        args: stmts.into_iter().collect(),
    }
}

/// Shortcut to create a `s.field` expression.
pub fn field<'ctx>(e: Expr<'ctx>, member: &'ctx str) -> Expr<'ctx> {
    PlaceE(FieldP(boxed(e), member))
}

/// Shortcut to create an if expression.
pub fn if_e<'ctx>(
    e: Expr<'ctx>,
    iftrue: impl Into<Block<'ctx>>,
    iffalse: impl Into<Block<'ctx>>,
) -> Expr<'ctx> {
    IfE(boxed(e), boxed(iftrue.into()), boxed(iffalse.into()))
}

/// Shortcut to create a `x[y]` expression.
pub fn index<'ctx>(e1: Expr<'ctx>, ix: Expr<'ctx>) -> Expr<'ctx> {
    PlaceE(IndexP(boxed(e1), boxed(ix)))
}

/// Shortcut to create a `MyStruct { (field: value)* }` expression.
pub fn structure<'ctx>(
    name: &'ctx str,
    fields: impl IntoIterator<Item = (&'ctx str, Expr<'ctx>)>,
) -> Expr<'ctx> {
    StructE {
        name,
        fields: fields.into_iter().collect(),
    }
}

/// Shortcut to create a `[el1, el2, ..]` expression.
pub fn array<'ctx>(items: impl IntoIterator<Item = Expr<'ctx>>) -> Expr<'ctx> {
    ArrayE(items.into_iter().collect())
}

/// Shortcut to create a block `Expr`.
pub fn block<'ctx>(value: impl Into<Block<'ctx>>) -> Expr<'ctx> {
    BlockE(Box::new(value.into()))
}

/// Shortcut to create a binary operation `Expr`.
pub fn binop<'ctx>(lhs: Expr<'ctx>, op: &'ctx str, rhs: Expr<'ctx>) -> Expr<'ctx> {
    call(op, vec![lhs, rhs])
}

impl<'ctx> From<Var<'ctx>> for Expr<'ctx> {
    fn from(v: Var<'ctx>) -> Self {
        PlaceE(VarP(v))
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Expr<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &mut Context<'ctx>) -> Self {
        match pair.as_rule() {
            Rule::expr | Rule::primitive => {
                let pair = pair.into_inner().next().unwrap();
                match pair.as_rule() {
                    Rule::unit => UnitE,
                    Rule::integer => IntegerE(ctx.parse(pair)),
                    Rule::string => StringE(ctx.parse(pair)),
                    Rule::ident => PlaceE(VarP(ctx.parse(pair))),
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
                            name: op.as_str(),
                            args: vec![lhs, rhs],
                        })
                        .parse(pair.into_inner()),
                    Rule::struct_instance => {
                        let mut pairs = pair.into_inner();
                        let name = pairs.next().unwrap().as_str();
                        let fields = pairs
                            .array_chunks::<2>()
                            .map(|[field, value]| {
                                let field: Var = ctx.parse(field);
                                let value: Expr = ctx.parse(value);
                                (field.as_str(), value)
                            })
                            .collect();
                        StructE { name, fields }
                    }
                    rule => panic!("parser internal error: expected expression, found {rule:?}"),
                }
            }
            Rule::with_postfix => {
                let mut pairs = pair.into_inner();
                let expr = ctx.parse(pairs.next().unwrap());
                pairs.fold(expr, |acc, pair| match pair.as_rule() {
                    Rule::field_postfix => {
                        let inner = pair.into_inner().next().unwrap();
                        let field: Var = ctx.parse(inner);
                        PlaceE(FieldP(boxed(acc), field.into()))
                    }
                    Rule::index_postfix => PlaceE(IndexP(
                        boxed(acc),
                        boxed(ctx.parse(pair.into_inner().next().unwrap())),
                    )),
                    Rule::call_postfix => {
                        if let PlaceE(VarP(name)) = acc {
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
                "Parser internal error: expected primitive or an expr, found {:?} for {}",
                pair.as_rule(),
                pair.as_str()
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::Stmt;
    use super::*;

    #[parses("test123" as expr)]
    #[test]
    fn simple_var(expr: Expr) {
        assert_eq!(expr.as_var().unwrap(), "test123");
    }

    #[parses("\"this is a test\nAnd another\"" as expr)]
    #[test]
    fn simple_string(expr: Expr) {
        assert_eq!(expr.as_string().unwrap(), "this is a test\nAnd another");
    }

    #[parses("1234567890" as expr)]
    #[test]
    fn simple_decimal(expr: Expr) {
        assert_eq!(expr.as_integer().unwrap(), &BigInt::from(1234567890));
    }

    #[parses("()" as expr)]
    #[test]
    fn unit(expr: Expr) {
        assert!(matches!(expr, UnitE));
    }

    #[parses("test.a" as expr)]
    #[test]
    fn field(expr: Expr) {
        let field = expr.as_field().unwrap();
        let lhs = field.0.as_var().unwrap();
        let rhs = field.1;
        assert!(lhs == "test");
        assert!(rhs == "a");
    }

    #[parses("test.a.b" as expr)]
    #[test]
    fn nested_fields(expr: Expr) {
        let field = expr.as_field().unwrap();
        let lhs = field.0.as_field().unwrap();
        let rhs1 = lhs.1;
        let lhs = lhs.0.as_var().unwrap();
        let rhs2 = field.1;
        assert!(lhs == "test");
        assert!(rhs1 == "a");
        assert!(rhs2 == "b");
    }

    #[parses("test[\"a\"]" as expr)]
    #[test]
    fn array_indexing(expr: Expr) {
        let (array, index) = expr.as_index().unwrap();
        assert!(array.as_var().unwrap() == "test");
        assert!(index.as_string().unwrap() == "a");
    }

    #[parses("blah[\"a\"][\"b\"]" as expr)]
    #[test]
    fn matrix_indexing(expr: Expr) {
        let (nested_index, ix2) = expr.as_index().unwrap();
        let (array, ix1) = nested_index.as_index().unwrap();
        assert!(array.as_var().unwrap() == "blah");
        assert!(ix1.as_string().unwrap() == "a");
        assert!(ix2.as_string().unwrap() == "b");
    }

    #[parses("blah.a[\"b\"]" as expr)]
    #[test]
    fn mix_array_index(expr: Expr) {
        let (nested, ix) = expr.as_index().unwrap();
        let (array, field) = nested.as_field().unwrap();
        assert!(array.as_var().unwrap() == "blah");
        assert!(field == "a");
        assert!(ix.as_string().unwrap() == "b");
    }

    #[parses("my_call(a,b,c)" as expr)]
    #[test]
    fn call_expression(expr: Expr) {
        let (call, args) = expr.as_call().unwrap();
        assert!(call == "my_call");
        assert_eq!(
            args.iter()
                .map(|arg| arg.as_var().unwrap().as_str().to_owned())
                .collect::<Vec<String>>(),
            ["a", "b", "c"]
        );
    }

    #[parses("[]" as expr)]
    #[test]
    fn empty_array_decl(expr: Expr) {
        let items = expr.as_array().unwrap();
        assert!(items
            .iter()
            .map(|arg| arg.as_integer().unwrap())
            .collect::<Vec<&BigInt>>()
            .is_empty());
    }

    #[parses("[1, 2, 3, 4, 5]" as expr)]
    #[test]
    fn array_decl(expr: Expr) {
        let items = expr.as_array().unwrap();
        assert_eq!(
            items
                .iter()
                .map(|arg| arg.as_integer().unwrap())
                .collect::<Vec<&BigInt>>(),
            [&BigInt::from(1), &2.into(), &3.into(), &4.into(), &5.into()]
        );
    }

    #[parses("if x { y } else { z }" as expr)]
    #[test]
    fn if_then(expr: Expr) {
        let (cond, if_block, else_block) = expr.as_if_then().unwrap();
        assert!(cond.as_var().unwrap() == "x");
        assert!(if_block.stmts.is_empty());
        assert!(if_block.ret.as_var().unwrap() == "y");
        assert!(else_block.stmts.is_empty());
        assert!(else_block.ret.as_var().unwrap() == "z");
    }

    #[parses("if x { var k: int = 12; y } else { var w: int = 16; z }" as expr)]
    #[test]
    fn more_complex_if_then_else(expr: Expr) {
        let (cond, if_block, else_block) = expr
            .as_if_then()
            .context("expr should be an if then else")?;
        assert!(cond.as_var().context("not a var")? == "x");
        assert!(matches!(&if_block.stmts[..], [Stmt::Declare(..)]));
        assert!(if_block.ret.as_var().context("not a var")? == "y");
        assert!(matches!(&else_block.stmts[..], [Stmt::Declare(..)]));
        assert!(else_block.ret.as_var().context("not a var")? == "z");
    }

    #[parses("a + b * c" as expr)]
    #[test]
    fn binary_operations(expr: Expr) {
        let (name1, args) = expr.as_call().context("a + b * c should be a call")?;
        let (name2, _) = args[1].as_call().context("b * c should be a call")?;
        assert_eq!(name1, "+");
        assert_eq!(name2, "*");
    }
}
