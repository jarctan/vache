//! Parsing expressions, and defining their representation in the AST.

use std::default::default;
use std::ops::{Deref, DerefMut};

use num_bigint::BigInt;
use pest::iterators::Pair;
use pest::pratt_parser::*;
use Place::*;

use super::{Block, Context, Namespaced, Parsable, Place, Span, VarUse};
use crate::grammar::*;
use crate::utils::boxed;

lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = PrattParser::new()
        .op(Op::infix(Rule::eq, Assoc::Left) | Op::infix(Rule::neq, Assoc::Left))
        .op(Op::infix(Rule::le, Assoc::Left)
            | Op::infix(Rule::lt, Assoc::Left)
            | Op::infix(Rule::ge, Assoc::Left)
            | Op::infix(Rule::gt, Assoc::Left))
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left)
            | Op::infix(Rule::div, Assoc::Left)
            | Op::infix(Rule::rem, Assoc::Left))
        .op(Op::infix(Rule::pow, Assoc::Right));
}

/// An located expression.
#[derive(Debug, Clone, Default)]
pub struct Expr<'ctx> {
    /// Expression kind.
    pub kind: ExprKind<'ctx>,
    /// Codespan.
    pub span: Span,
}

impl<'ctx> Deref for Expr<'ctx> {
    type Target = ExprKind<'ctx>;

    fn deref(&self) -> &ExprKind<'ctx> {
        &self.kind
    }
}

impl<'ctx> DerefMut for Expr<'ctx> {
    fn deref_mut(&mut self) -> &mut ExprKind<'ctx> {
        &mut self.kind
    }
}

impl<'ctx> From<ExprKind<'ctx>> for Expr<'ctx> {
    fn from(kind: ExprKind<'ctx>) -> Self {
        Self {
            kind,
            span: default(),
        }
    }
}

/// Expression kinds in the AST.
///
/// Rule: all variants end with a capital `E`.
#[derive(Debug, Clone, Default)]
pub enum ExprKind<'ctx> {
    /// Unit expression, that does nothing.
    #[default]
    UnitE,
    /// An unbounded integer.
    IntegerE(BigInt),
    /// A string.
    StringE(&'ctx str),
    /// A variable.
    PlaceE(Place<'ctx>),
    /// A range.
    ///
    /// Format: `RangeE(start, end)`.
    RangeE(Box<Expr<'ctx>>, Box<Expr<'ctx>>),
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
        name: Namespaced<'ctx>,
        /// Arguments to that function.
        args: Vec<Expr<'ctx>>,
    },
    /// An if expression.
    IfE(Box<Expr<'ctx>>, Box<Block<'ctx>>, Box<Block<'ctx>>),
    /// A block expression.
    BlockE(Box<Block<'ctx>>),
    /// A namespaced symbol.
    NamespacedE(Namespaced<'ctx>),
    /// A pattern matching.
    MatchE(Box<Expr<'ctx>>, Vec<(Box<Expr<'ctx>>, Box<Expr<'ctx>>)>),
}

use ExprKind::*;

impl<'ctx> Expr<'ctx> {
    /// Sees this expression as a field.
    ///
    /// Returns: `(strukt, field)`.
    ///
    /// # Errors
    /// Returns `None` if the expression is not a field.
    pub fn as_field(&self) -> Option<(&Expr<'ctx>, &'ctx str)> {
        if let PlaceE(FieldP(box strukt, field)) = &self.kind {
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
        if let PlaceE(IndexP(box array, box index)) = &self.kind {
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
    pub fn as_call(&self) -> Option<(Namespaced<'ctx>, &[Expr<'ctx>])> {
        if let CallE { name, args } = &self.kind {
            Some((*name, args.as_slice()))
        } else {
            None
        }
    }

    /// Sees this expression as an integer.
    ///
    /// # Errors
    /// Returns `None` if the expression is not an integer.
    pub fn as_integer(&self) -> Option<&BigInt> {
        if let IntegerE(ref i) = &self.kind {
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
        if let StringE(s) = &self.kind {
            Some(s)
        } else {
            None
        }
    }

    /// Sees this expression as a variable.
    ///
    /// # Errors
    /// Returns `None` if the expression is not a variable.
    pub fn as_var(&self) -> Option<VarUse<'ctx>> {
        if let PlaceE(VarP(var)) = &self.kind {
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
        if let ArrayE(ref array) = &self.kind {
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
        if let IfE(box cond, box iftrue, box iffalse) = &self.kind {
            Some((cond, iftrue, iffalse))
        } else {
            None
        }
    }
}

/// Shortcut to create an `Expr` which is just a variable, based on its name.
pub fn var(v: &str) -> Expr<'_> {
    PlaceE(VarP(v.into())).into()
}

/// Shortcut to create a constant integer `Expr` based on some integer value.
pub fn int<'ctx>(value: impl Into<BigInt>) -> Expr<'ctx> {
    IntegerE(value.into()).into()
}

/// Shortcut to create a constant string `String` based on some string value.
pub fn string(value: &str) -> Expr<'_> {
    StringE(value).into()
}

/// Shortcut to create a call `Expr`.
pub fn call<'ctx>(
    name: impl Into<Namespaced<'ctx>>,
    stmts: impl IntoIterator<Item = Expr<'ctx>>,
) -> Expr<'ctx> {
    CallE {
        name: name.into(),
        args: stmts.into_iter().collect(),
    }
    .into()
}

/// Shortcut to create a `s.field` expression.
pub fn field<'ctx>(e: Expr<'ctx>, member: &'ctx str) -> Expr<'ctx> {
    PlaceE(FieldP(boxed(e), member)).into()
}

/// Shortcut to create an if expression.
pub fn if_e<'ctx>(
    e: Expr<'ctx>,
    iftrue: impl Into<Block<'ctx>>,
    iffalse: impl Into<Block<'ctx>>,
) -> Expr<'ctx> {
    IfE(boxed(e), boxed(iftrue.into()), boxed(iffalse.into())).into()
}

/// Shortcut to create a `x[y]` expression.
pub fn index<'ctx>(e1: Expr<'ctx>, ix: Expr<'ctx>) -> Expr<'ctx> {
    PlaceE(IndexP(boxed(e1), boxed(ix))).into()
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
    .into()
}

/// Shortcut to create a `[el1, el2, ..]` expression.
pub fn array<'ctx>(items: impl IntoIterator<Item = Expr<'ctx>>) -> Expr<'ctx> {
    ArrayE(items.into_iter().collect()).into()
}

/// Shortcut to create a block `Expr`.
pub fn block<'ctx>(value: impl Into<Block<'ctx>>) -> Expr<'ctx> {
    BlockE(Box::new(value.into())).into()
}

/// Shortcut to create a binary operation `Expr`.
pub fn binop<'ctx>(lhs: Expr<'ctx>, op: &'ctx str, rhs: Expr<'ctx>) -> Expr<'ctx> {
    call(op, vec![lhs, rhs])
}

impl<'ctx> From<VarUse<'ctx>> for Expr<'ctx> {
    fn from(v: VarUse<'ctx>) -> Self {
        Expr {
            span: v.as_span(),
            kind: PlaceE(VarP(v)),
        }
    }
}

/// Parse a if-then. Common to both statement and expression parsing, so we
/// factor it out here.
///
/// # Panics
/// Panics if the pair is _not_ an if-then-else rule.
pub(super) fn parse_if_then_else<'ctx>(
    ctx: &mut Context<'ctx>,
    pair: Pair<'ctx, Rule>,
) -> Expr<'ctx> {
    debug_assert!(matches!(pair.as_rule(), Rule::if_then));
    let span = Span::from(pair.as_span());
    let mut pairs = pair.into_inner();
    consume!(pairs, Rule::if_kw);
    let cond = ctx.parse(consume!(pairs));
    let if_block: Block = ctx.parse(consume!(pairs));
    let else_block = if consume_opt!(pairs, Rule::else_kw) {
        ctx.parse(consume!(pairs))
    } else {
        Block {
            span: Span::at(if_block.span.end()), /* Put the position of the dummy else block at
                                                  * the end of the if block */
            ..default()
        }
    };
    Expr {
        kind: IfE(boxed(cond), boxed(if_block), boxed(else_block)),
        span,
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Expr<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &mut Context<'ctx>) -> Self {
        let span = Span::from(pair.as_span());
        let kind = match pair.as_rule() {
            Rule::expr | Rule::primitive | Rule::matched_primitive | Rule::matched_expr => {
                let pair = pair.into_inner().next().unwrap();
                match pair.as_rule() {
                    Rule::unit => UnitE,
                    Rule::integer => IntegerE(ctx.parse(pair)),
                    Rule::string => StringE(ctx.parse(pair)),
                    Rule::ident => PlaceE(VarP(ctx.parse(pair))),
                    Rule::array => {
                        let mut pairs = pair.into_inner();
                        consume!(pairs, Rule::lb);
                        consume_back!(pairs, Rule::rb);
                        let expr = ArrayE(
                            pairs
                                .filter(|rule| !matches!(rule.as_rule(), Rule::cma))
                                .map(|pair| ctx.parse(pair))
                                .collect(),
                        );
                        expr
                    }
                    Rule::with_postfix | Rule::primitive | Rule::matched_primitive => {
                        Expr::parse(pair, ctx).kind
                    }
                    Rule::if_then => parse_if_then_else(ctx, pair).kind,
                    Rule::binop => {
                        // Map `infix` pairs to their underlying operators, leaving the other
                        // elements as is.
                        // Necessary because the `infix` rule is no longer silent in the pest
                        // grammar, so it adds an extra layer of indirection in the pairs that we
                        // must unwrap here. Pratt parser requires elements
                        // in the form `prefix* ~ primary ~ postfix* ~
                        // (infix ~ prefix* ~ primary ~ postfix*)*`
                        // where infix is one of the infix rules (add, sub, mul, etc.), and **NOT**
                        // the `infix` rule.
                        let pairs = pair.into_inner().map(|pair| {
                            if let Rule::infix = pair.as_rule() {
                                pair.into_inner().next().unwrap()
                            } else {
                                pair
                            }
                        });
                        PRATT_PARSER
                            .map_primary(|primary| Expr::parse(primary, ctx))
                            .map_infix(|lhs, op, rhs| {
                                let op_span = Span::from(op.as_span());
                                let span = rhs.span.merge(rhs.span);
                                let kind = CallE {
                                    name: Namespaced::name_with_span(op.as_str(), op_span),
                                    args: vec![lhs, rhs],
                                };
                                Expr { span, kind }
                            })
                            .parse(pairs)
                            .kind
                    }
                    Rule::struct_instance => {
                        let mut pairs = pair.into_inner();
                        let name = consume!(pairs).as_str();
                        consume!(pairs, Rule::lcb);
                        let fields = pairs
                            .filter(|pair| {
                                !matches!(pair.as_rule(), Rule::lcb | Rule::cln | Rule::cma)
                            })
                            .array_chunks::<2>()
                            .map(|[field, value]| {
                                let field: VarUse = ctx.parse(field);
                                let value: Expr = ctx.parse(value);
                                (field.as_str(), value)
                            })
                            .collect();
                        StructE { name, fields }
                    }
                    Rule::block => BlockE(boxed(Block::parse(pair, ctx))),
                    Rule::subexpr => {
                        let mut pairs = pair.into_inner();
                        consume!(pairs, Rule::lp);
                        let expr = Expr::parse(consume!(pairs), ctx).kind;
                        consume!(pairs, Rule::rp);
                        expr
                    }
                    Rule::namespaced => NamespacedE(ctx.parse(pair)),
                    Rule::match_expr => {
                        let mut pairs = pair.into_inner();
                        consume!(pairs, Rule::match_kw);
                        let matched = Expr::parse(consume!(pairs), ctx);
                        let mut pairs = consume!(pairs, Rule::match_block).into_inner();
                        consume!(pairs, Rule::lcb);
                        consume_back!(pairs, Rule::rcb);
                        let branches = pairs
                            .filter(|pair| !matches!(pair.as_rule(), Rule::pipe | Rule::farw))
                            .array_chunks()
                            .map(|[pat, expr]| (boxed(ctx.parse(pat)), boxed(ctx.parse(expr))))
                            .collect::<Vec<_>>();
                        MatchE(boxed(matched), branches)
                    }
                    rule => panic!("parser internal error: expected expression, found {rule:?}"),
                }
            }
            Rule::with_postfix => {
                let mut pairs = pair.into_inner();
                let expr: Expr = ctx.parse(consume!(pairs));
                pairs
                    .fold(expr, |acc, pair| {
                        let span = acc.span.merge(Span::from(pair.as_span()));
                        let kind = match pair.as_rule() {
                            Rule::field_postfix => {
                                let mut pairs = pair.into_inner();
                                consume!(pairs, Rule::dt);
                                let field: VarUse = ctx.parse(consume!(pairs));
                                PlaceE(FieldP(boxed(acc), field.into()))
                            }
                            Rule::index_postfix => {
                                let mut pairs = pair.into_inner();
                                consume!(pairs, Rule::lb);
                                let expr =
                                    PlaceE(IndexP(boxed(acc), boxed(ctx.parse(consume!(pairs)))));
                                consume!(pairs, Rule::rb);
                                expr
                            }
                            Rule::call_postfix => {
                                let mut pairs = pair.into_inner();
                                consume!(pairs, Rule::lp);
                                consume_back!(pairs, Rule::rp);
                                let pairs =
                                    pairs.filter(|pair| !matches!(pair.as_rule(), Rule::cma));
                                match acc.kind {
                                    PlaceE(VarP(name)) => CallE {
                                        name: Namespaced::name_with_span(
                                            name.as_str(),
                                            name.as_span(),
                                        ),
                                        args: pairs.map(|pair| ctx.parse(pair)).collect(),
                                    },
                                    NamespacedE(name) => CallE {
                                        name,
                                        args: pairs.map(|pair| ctx.parse(pair)).collect(),
                                    },
                                    _ => panic!("Expected a callable expression"),
                                }
                            }
                            Rule::range_postfix => {
                                let mut pairs = pair.into_inner();
                                consume!(pairs, Rule::rg);
                                RangeE(boxed(acc), boxed(ctx.parse(consume!(pairs))))
                            }
                            rule => panic!("expected postfix, found {rule:?}"),
                        };
                        Expr { kind, span }
                    })
                    .kind
            }
            _ => panic!(
                "Parser internal error: expected primitive or an expr, found {:?}",
                pair.as_rule()
            ),
        };
        Self { kind, span }
    }
}

#[cfg(test)]
mod tests {
    use StmtKind::*;

    use super::super::StmtKind;
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
        assert!(matches!(expr.kind, UnitE));
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
        assert!(call.name == "my_call");
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
        assert_eq!(if_block.stmts.len(), 1);
        assert!(if_block.ret.as_var().context("not a var")? == "y");
        assert_eq!(else_block.stmts.len(), 1);
        assert!(matches!(else_block.stmts[0].kind, DeclareS(..)));
        assert!(else_block.ret.as_var().context("not a var")? == "z");
    }

    #[parses("a + b * c" as expr)]
    #[test]
    fn binary_operations(expr: Expr) {
        let (name1, args) = expr.as_call().context("a + b * c should be a call")?;
        let (name2, _) = args[1].as_call().context("b * c should be a call")?;
        assert_eq!(name1.name, "+");
        assert_eq!(name2.name, "*");
    }
}
