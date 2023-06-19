//! Parser Abstract Syntax Tree for the language.
//!
//! Each node in the tree = one file.

/// Consumes and returns one pair from the `pairs`.
///
/// Usage:
/// * `consume!(pairs)` consumes and returns the next pair.
/// * `consume!(pairs, Rule::my_rule)` consumes and returns the next pair. On
///   debug targets, it will check that the consumed pair is of rule `my_rule`.
///
/// In both cases, `pairs` must a mutable, peekable
/// iterator over pairs, and `Rule::my_rule` a valid pest rule variant.
macro_rules! consume {
    ($pairs: expr, $rule: pat) => {{
        let pair = $pairs.next().unwrap();
        debug_assert!(
            matches!(pair.as_rule(), $rule),
            "expected {}, found rule {:?} for `{}`",
            stringify!($rule),
            pair.as_rule(),
            pair.as_str(),
        );
        pair
    }};
    ($pairs: expr) => {
        $pairs.next().unwrap()
    };
}

/// Consumes and returns one pair starting from the **end** of `pairs`.
///
/// Usage:
/// * `consume!(pairs)` consumes and returns the next pair from the `end`.
/// * `consume!(pairs, Rule::my_rule)` consumes and returns the next pair
///   starting from the`end. On debug targets, it will check that the consumed
///   pair is of rule `my_rule`.
///
/// In both cases, `pairs` must a mutable, peekable
/// iterator over pairs, and `Rule::my_rule` a valid pest rule variant.
macro_rules! consume_back {
    ($pairs: expr, $rule: pat) => {{
        let pair = $pairs.next_back().unwrap();
        debug_assert!(
            matches!(pair.as_rule(), $rule),
            "expected {}, found rule {:?} for `{}`",
            stringify!($rule),
            pair.as_rule(),
            pair.as_str(),
        );
        pair
    }};
    ($pairs: expr) => {
        $pairs.next_back().unwrap()
    };
}

/// Checks if the next pair of `pairs` is of rule `rule`, if so, consumes and
/// returns that pair.
///
/// `consume_opt(pairs, Rule::my_rule)` where `pairs` is a mutable, peekable
/// iterator over pairs, and `Rule::my_rule` a valid pest rule variant.
macro_rules! consume_opt {
    ($pairs: expr, $rule: pat) => {{
        if let Some(pair) = $pairs.peek() && matches!(pair.as_rule(), $rule) {
                                                                            $pairs.next().unwrap();
                                                                            true
                                                                        } else {
                                                                            false
                                                                        }
    }};
}

pub mod block;
pub mod enumeration;
pub mod expr;
pub mod fun;
pub mod mode;
pub mod namespaced;
pub mod place;
pub mod primitives;
pub mod program;
pub mod reporting;
pub mod span;
pub mod stmt;
pub mod structure;
pub mod ty;
pub mod var;

use std::io::Write;
use std::time::Instant;

pub use anyhow::Context as AnyhowContext;
pub use anyhow::Result;
pub use block::Block;
pub use enumeration::Enum;
pub use expr::{if_e, Expr, ExprKind};
pub use fun::{Fun, FunSig};
use itertools::Itertools;
pub use mode::Mode;
pub use namespaced::Namespaced;
use pest::error::ErrorVariant;
use pest::error::InputLocation;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
pub use place::{idx_place, Place, PlaceKind};
pub use program::Program;
use reporting::pretty_print_rule;
pub use span::Span;
pub use stmt::{Stmt, StmtKind};
pub use structure::Struct;
pub use ty::{arrayT, boolT, intT, strT, unitT, varT, Ty, TyUse};
pub use var::{VarDef, VarUse, Varname};

use crate::codes::*;
use crate::context::Context;
use crate::grammar::{Grammar, Rule};
use crate::reporter::Diagnostic;

/// Can be parsed from elements of `T`.
pub trait Parsable<'ctx, T> {
    /// Parses `tokens` into `Self` using `ctx`.
    fn parse(tokens: T, ctx: &Context<'ctx>) -> Self;
}

impl<'ctx> Context<'ctx> {
    /// Parses `tokens`.
    #[must_use]
    pub fn parse<U, T: Parsable<'ctx, U>>(&self, pair: U) -> T {
        T::parse(pair, self)
    }
}

/// Parses some source code into some `pairs`.
///
/// The source code is in the `config` of the context `ctx`.
fn parse_pairs<'ctx>(ctx: &mut Context<'ctx>, rule: Rule) -> Result<Pairs<'ctx, Rule>> {
    match Grammar::parse(rule, ctx.config.input) {
        Ok(pairs) => Ok(pairs),
        Err(err) => {
            let msg = match &err.variant {
                ErrorVariant::ParsingError { .. } => "Unexpected token",
                ErrorVariant::CustomError { message } => message,
            };
            let notes = match &err.variant {
                ErrorVariant::ParsingError {
                    positives,
                    negatives,
                } => {
                    let mut notes = vec![];
                    if !positives.is_empty() {
                        let positives = positives
                            .iter()
                            .map(pretty_print_rule)
                            .unique()
                            .collect::<Vec<_>>();
                        let len = positives.len();
                        if len > 1 {
                            notes.push(format!(
                                "Expected either {}, or {}",
                                positives[..len - 1].join(", "),
                                positives[len - 1]
                            ));
                        } else {
                            notes.push(format!("Expected {}", positives[0]));
                        }
                    }
                    if !negatives.is_empty() {
                        let negatives = negatives
                            .iter()
                            .map(pretty_print_rule)
                            .unique()
                            .collect::<Vec<_>>();
                        let len = negatives.len();
                        if len > 1 {
                            notes.push(format!(
                                "Expected either {}, or {}",
                                negatives[..len - 1].join(", "),
                                negatives[len - 1]
                            ));
                        } else {
                            notes.push(format!("Expected {}", negatives[0]));
                        }
                    }
                    notes
                }
                ErrorVariant::CustomError { message: _ } => vec![],
            };

            let span = match err.location {
                InputLocation::Pos(pos) => pest::Span::new(ctx.config.input, pos, pos),
                InputLocation::Span((start, end)) => pest::Span::new(ctx.config.input, start, end),
            }
            .context("Could not create span for error")?;
            let span: Span = span.into();

            ctx.emit(
                Diagnostic::error()
                    .with_code(PARSER_ERROR)
                    .with_message(msg)
                    .with_labels(vec![span.as_label()])
                    .with_notes(notes),
            );
            let reports = ctx.reporter.flush();
            reports.display()?;
            bail!("Parsing errors found");
        }
    }
}

/// Parses some source code into a parsable element `T`, using rule `rule`.
///
/// This is the version for `Parsable` from `Pairs` (with an s).
///
/// The source code is in the `config` of the context `ctx`.
pub fn parse_rules<'ctx, T: Parsable<'ctx, Pairs<'ctx, Rule>>>(
    ctx: &mut Context<'ctx>,
    rule: Rule,
) -> Result<T> {
    let pairs = parse_pairs(ctx, rule)?;

    let res: T = ctx.parse(pairs);

    // Check for second-stage parser errors
    if !ctx.reporter.has_errors() {
        Ok(res)
    } else {
        ctx.reporter.flush().display()?;
        bail!("Parsing errors found");
    }
}

/// Parses some source code into a parsable element `T`, using rule `rule`.
///
/// This is the version for `Parsable` from `Pair` (without s).
///
/// The source code is in the `config` of the context `ctx`.
pub fn parse_rule<'ctx, T: Parsable<'ctx, Pair<'ctx, Rule>>>(
    ctx: &mut Context<'ctx>,
    rule: Rule,
) -> Result<T> {
    let mut pairs = parse_pairs(ctx, rule)?;

    let res: T = ctx.parse(pairs.next().context("Parser grammar error")?);

    // Check for second-stage parser errors
    if !ctx.reporter.has_errors() {
        Ok(res)
    } else {
        ctx.reporter.flush().display()?;
        bail!("Parsing errors found");
    }
}

/// Parses the source code for a file, and returns the parsed program.
///
/// The source code is in the `config` of the context `ctx`.
pub fn parse_file<'ctx>(ctx: &mut Context<'ctx>) -> Result<Program<'ctx>> {
    print!("Parsing...");
    std::io::stdout().flush()?;
    let start = Instant::now();
    let res = parse_rules(ctx, Rule::program)?;
    println!("\rParsed file [{:?}]", start.elapsed());
    Ok(res)
}
