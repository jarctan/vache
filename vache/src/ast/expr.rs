//! Parsing expressions, and defining their representation in the AST.

use std::default::default;
use std::ops::{Deref, DerefMut};

use codespan_reporting::diagnostic::Diagnostic;
use itertools::Itertools;
use num_bigint::BigInt;
use pest::iterators::Pair;
use pest::pratt_parser::*;
use PlaceKind::*;

use super::{Arg, Block, Context, Mode, Namespaced, Parsable, Place, PlaceKind, Span, VarUse};
use crate::codes::*;
use crate::grammar::*;
use crate::utils::boxed;

lazy_static! {
    /// Precedences, from less binding to most binding.
    static ref PRATT_PARSER: PrattParser<Rule> = PrattParser::new()
        .op(Op::infix(Rule::and, Assoc::Left) | Op::infix(Rule::or, Assoc::Left))
        .op(Op::infix(Rule::eqq, Assoc::Left) | Op::infix(Rule::neq, Assoc::Left)) // 7
        .op(Op::infix(Rule::le, Assoc::Left) // 6
            | Op::infix(Rule::lt, Assoc::Left)
            | Op::infix(Rule::ge, Assoc::Left)
            | Op::infix(Rule::gt, Assoc::Left))
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left)) // 4
        .op(Op::infix(Rule::mul, Assoc::Left) // 3
            | Op::infix(Rule::div, Assoc::Left)
            | Op::infix(Rule::rem, Assoc::Left))
        .op(Op::infix(Rule::pow, Assoc::Right)) // 2
        .op(Op::prefix(Rule::bang));
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
    /// A boolean.
    BoolE(bool),
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
    /// Tuple creation.
    TupleE(Vec<Expr<'ctx>>),
    /// A function call.
    CallE {
        /// Name/identifier of the function.
        name: Namespaced<'ctx>,
        /// Arguments to that function.
        args: Vec<Arg<'ctx>>,
    },
    /// An if expression.
    IfE(Box<Expr<'ctx>>, Box<Block<'ctx>>, Box<Block<'ctx>>),
    /// A block expression.
    BlockE(Box<Block<'ctx>>),
    /// A namespaced symbol.
    NamespacedE(Namespaced<'ctx>),
    /// A pattern matching.
    MatchE(Box<Expr<'ctx>>, Vec<(Expr<'ctx>, Expr<'ctx>)>),
    /// Hole expression.
    ///
    /// In case the parsing we do fails.
    HoleE,
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
        if let PlaceE(place) = &self.kind && let FieldP(box strukt, field) = &place.kind {
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
        if let PlaceE(place) = &self.kind && let IndexP(box array, box index) = &place.kind {
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
    pub fn as_call(&self) -> Option<(Namespaced<'ctx>, &[Arg<'ctx>])> {
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
        if let PlaceE(place) = &self.kind && let VarP(var) = &place.kind {
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

    /// Hole expression, in case we cannot parse the expression.
    fn hole(span: Span) -> Expr<'ctx> {
        Self { kind: HoleE, span }
    }
}

/// Shortcut to create an `Expr` which is just a variable, based on its name.
pub fn var(v: &str) -> Expr<'_> {
    PlaceE(Place::from(VarP(v.into()))).into()
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
pub fn call<'ctx, A: Into<Arg<'ctx>>>(
    name: impl Into<Namespaced<'ctx>>,
    args: impl IntoIterator<Item = A>,
) -> Expr<'ctx> {
    CallE {
        name: name.into(),
        args: args.into_iter().map(|arg| arg.into()).collect(),
    }
    .into()
}

/// Shortcut to create a `s.field` expression.
pub fn field<'ctx>(e: Expr<'ctx>, member: &'ctx str) -> Expr<'ctx> {
    PlaceE(Place::from(FieldP(boxed(e), member))).into()
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
    PlaceE(Place::from(IndexP(boxed(e1), boxed(ix)))).into()
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
            kind: PlaceE(Place::from(VarP(v))),
        }
    }
}

/// Parse a if-then. Common to both statement and expression parsing, so we
/// factor it out here.
///
/// # Panics
/// Panics if the pair is _not_ an if-then-else rule.
pub(super) fn parse_if_then_else<'ctx>(ctx: &Context<'ctx>, pair: Pair<'ctx, Rule>) -> Expr<'ctx> {
    debug_assert!(matches!(pair.as_rule(), Rule::if_then));
    let span = Span::from(pair.as_span());
    let mut pairs = pair.into_inner();
    consume!(pairs, Rule::if_kw);
    let cond = ctx.parse(consume!(pairs));
    let if_block: Block = ctx.parse(consume!(pairs));
    let mut pairs = pairs.rev().peekable();

    // Check if we have at least one other branch
    if pairs.peek().is_some() {
        // Parse the final `else if? { }`.
        let final_block: Block = ctx.parse(consume!(pairs));
        // If the next thing we got (backwards, remember) is an expression, then we are
        // facing an else if block
        let mut else_block = if let Some(cond) = consume_opt!(pairs, Rule::expr) {
            // If the final block is an `else if`, generate a dummy else block out of it

            consume!(pairs, Rule::if_kw);
            consume!(pairs, Rule::else_kw);

            let cond = ctx.parse(cond);

            // Generate a dummy else block
            let else_block = Block {
                span: Span::at(final_block.span.end()), /* Put the position of the dummy else
                                                         * block
                                                         * at
                                                         * the end of the final if block */
                ..default()
            };
            return Expr {
                kind: IfE(boxed(cond), boxed(final_block), boxed(else_block)),
                span,
            };
        } else {
            consume!(pairs, Rule::else_kw);
            final_block
        };

        // Split the iterator with the `else` keyword. Awkward way to do it but could
        // not find better.
        let pairs = pairs.group_by(|pair| matches!(pair.as_rule(), Rule::else_kw));
        let pairs = pairs.into_iter();

        // Then we now have a group of [`else`] followed by a group of [block,if], i.e.
        // our iterator is `([block,expr,if][else])*`. (remember that we reversed the
        // iterator). We array chunk by 2, discard the second chunk (which is the group
        // [else]) and concentrate on the remaining iterator, which should have
        // only 3 elements. First one is the block, second one is the expr, third is the
        // if token.
        //
        // By proceeding in reverse order, and updating the `else_block` in a nested
        // way, we effectively reconstruct the if-then-else were meant to be
        // understood.
        for mut pairs in pairs
            .array_chunks::<2>()
            .map(|[(_, blk), (_, _else_kw)]| blk)
        {
            // Parse the block
            let pair = consume!(pairs);
            let span1 = Span::from(pair.as_span());
            let block = ctx.parse(pair);

            // Parse the expr
            let pair = consume!(pairs, Rule::expr);
            let span2 = Span::from(pair.as_span());
            let cond = ctx.parse(pair);

            // Parse the final if keyword
            consume!(pairs, Rule::if_kw);

            // Update the else block in a nested way
            else_block = Block {
                ret: Expr {
                    span: span2.merge(span1).merge(else_block.span),
                    kind: IfE(boxed(cond), boxed(block), boxed(else_block)),
                },
                span: span1.merge(span2),
                ..default()
            };
        }
        Expr {
            kind: IfE(boxed(cond), boxed(if_block), boxed(else_block)),
            span,
        }
    } else {
        // In case we have no else branch
        // Generate a dummy else block
        let else_block = Block {
            span: Span::at(if_block.span.end()), /* Put the position of the dummy else block
                                                  * at
                                                  * the end of the if block */
            ..default()
        };
        Expr {
            kind: IfE(boxed(cond), boxed(if_block), boxed(else_block)),
            span,
        }
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Expr<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        let span = Span::from(pair.as_span());
        let kind = match pair.as_rule() {
            Rule::expr | Rule::primitive | Rule::matched_primitive | Rule::matched_expr => {
                let pair = pair.into_inner().next().unwrap();
                match pair.as_rule() {
                    Rule::unit => UnitE,
                    Rule::integer => IntegerE(ctx.parse(pair)),
                    Rule::boolean => BoolE(ctx.parse(pair)),
                    Rule::string => StringE(ctx.parse(pair)),
                    Rule::ident => PlaceE(Place::new(VarP(ctx.parse(pair)), span, default())),
                    Rule::array => {
                        let mut pairs = pair.into_inner();
                        consume!(pairs, Rule::lb);
                        consume_back!(pairs, Rule::rb);
                        ArrayE(
                            pairs
                                .filter(|rule| !matches!(rule.as_rule(), Rule::cma))
                                .map(|pair| ctx.parse(pair))
                                .collect(),
                        )
                    }
                    Rule::tuple => {
                        let mut pairs = pair.into_inner();
                        consume!(pairs, Rule::lp);
                        consume_back!(pairs, Rule::rp);
                        let items = pairs
                            .filter(|pair| !matches!(pair.as_rule(), Rule::cma))
                            .map(|item| ctx.parse(item))
                            .collect();
                        TupleE(items)
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
                        let pairs = pair.into_inner().map(|pair| match pair.as_rule() {
                            Rule::infix | Rule::prefix => pair.into_inner().next().unwrap(),
                            _ => pair,
                        });
                        PRATT_PARSER
                            .map_primary(|primary| Expr::parse(primary, ctx))
                            .map_infix(|lhs, op, rhs| {
                                let op_span = Span::from(op.as_span());
                                let span = lhs.span.merge(op_span).merge(rhs.span);
                                let kind = CallE {
                                    name: Namespaced::name_with_span(op.as_str(), op_span),
                                    args: vec![Arg::from(lhs), Arg::from(rhs)],
                                };
                                Expr { span, kind }
                            })
                            .map_prefix(|op, mut rhs| match op.as_rule() {
                                Rule::as_mut => {
                                    let op_span = Span::from(op.as_span());
                                    let span = rhs.span.merge(op_span);
                                    match &mut rhs.kind {
                                        PlaceE(place) => {
                                            place.mode = Mode::MutBorrowed;
                                            place.span = span;
                                        }
                                        _ => ctx.emit(
                                            Diagnostic::error()
                                                .with_code(AS_MUT_NOT_PLACE_ERROR)
                                                .with_message(format!(
                                                    "Only places can be mutably borrowed"
                                                ))
                                                .with_labels(vec![rhs
                                                    .span
                                                    .as_label()
                                                    .with_message("not a place")]),
                                        ),
                                    }
                                    Expr {
                                        kind: rhs.kind,
                                        span,
                                    }
                                }
                                _ => {
                                    let op_span = Span::from(op.as_span());
                                    let span = rhs.span.merge(op_span);
                                    let kind = CallE {
                                        name: Namespaced::name_with_span(op.as_str(), op_span),
                                        args: vec![Arg::from(rhs)],
                                    };
                                    Expr { span, kind }
                                }
                            })
                            .parse(pairs)
                            .kind
                    }
                    Rule::struct_instance => {
                        let mut pairs = pair.into_inner();
                        let name = consume!(pairs).as_str();
                        consume!(pairs, Rule::lcb);
                        consume_back!(pairs, Rule::rcb);
                        let fields = pairs
                            .filter(|pair| !matches!(pair.as_rule(), Rule::cln | Rule::cma))
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
                            .map(|[pat, expr]| (ctx.parse(pat), ctx.parse(expr)))
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
                                // After a `.`, we have either an `ident` or a tuple index.
                                if let Some(pair) = consume_opt!(pairs, Rule::ident) {
                                    let field: VarUse = ctx.parse(pair);
                                    PlaceE(Place::new(
                                        FieldP(boxed(acc), field.into()),
                                        span,
                                        default(),
                                    ))
                                } else {
                                    let pair = consume!(pairs, Rule::tuple_idx);
                                    let el = pair.as_str().parse().expect("is not valid usize");
                                    PlaceE(Place::new(ElemP(boxed(acc), el), span, default()))
                                }
                            }
                            Rule::index_postfix => {
                                let mut pairs = pair.into_inner();
                                consume!(pairs, Rule::lb);
                                let expr = PlaceE(Place::new(
                                    IndexP(boxed(acc), boxed(ctx.parse(consume!(pairs)))),
                                    span,
                                    default(),
                                ));
                                consume!(pairs, Rule::rb);
                                expr
                            }
                            Rule::call_postfix => {
                                let mut pairs = pair.into_inner();
                                consume!(pairs, Rule::lp);
                                consume_back!(pairs, Rule::rp);
                                let pairs =
                                    pairs.filter(|pair| !matches!(pair.as_rule(), Rule::cma));
                                let mut args = vec![];
                                for pair in pairs {
                                    match ctx.parse(pair) {
                                        Some(arg) => args.push(arg),
                                        None => return Expr::hole(span),
                                    }
                                }
                                match acc.kind {
                                    PlaceE(place) if let VarP(name) = place.kind => CallE {
                                        name: Namespaced::name_with_span(
                                            name.as_str(),
                                            name.as_span(),
                                        ),
                                        args,
                                    },
                                    NamespacedE(name) => CallE {
                                        name,
                                        args,
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
                .map(|arg| arg
                    .as_standard()
                    .unwrap()
                    .as_var()
                    .unwrap()
                    .as_str()
                    .to_owned())
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
        let (name2, _) = args[1]
            .as_standard()
            .context("Should be a standard argument")?
            .as_call()
            .context("b * c should be a call")?;
        assert_eq!(name1.name, "+");
        assert_eq!(name2.name, "*");
    }
}
