//! Parser Abstract Syntax Tree for the language.
//!
//! Each node in the tree = one file.

pub mod block;
pub mod expr;
pub mod fun;
pub mod place;
pub mod primitives;
pub mod program;
pub mod reporting;
pub mod span;
pub mod stmt;
pub mod structure;
pub mod ty;
pub mod var;

pub use anyhow::Context as AnyhowContext;
pub use anyhow::Result;
pub use block::Block;
pub use expr::{if_e, Expr, ExprKind};
pub use fun::{Fun, FunSig};
use itertools::Itertools;
use pest::error::ErrorVariant;
use pest::error::InputLocation;
use pest::iterators::Pair;
use pest::Parser;
pub use place::{idx_place, Place};
pub use program::Program;
use reporting::pretty_print_rule;
pub use span::Span;
pub use stmt::{Stmt, StmtKind};
pub use structure::Struct;
pub use ty::{arrayT, boolT, intT, strT, structT, unitT, Ty, TyUse};
pub use var::{VarDef, VarUse, Varname};

use crate::codes::*;
use crate::context::Context;
use crate::grammar::{Grammar, Rule};
use crate::reporter::Diagnostic;

/// Can be parsed from elements of `T`.
pub trait Parsable<'ctx, T> {
    /// Parses `tokens` into `Self` using `ctx`.
    fn parse(tokens: T, ctx: &mut Context<'ctx>) -> Self;
}

impl<'ctx> Context<'ctx> {
    /// Parses `tokens`.
    #[must_use]
    pub fn parse<U, T: Parsable<'ctx, U>>(&mut self, pair: U) -> T {
        T::parse(pair, self)
    }
}

/// Parses some source code into a parsable element `T`, using rule `rule`.
///
/// The source code is in the `config` of the context `ctx`.
pub fn parse_rule<'ctx, T: Parsable<'ctx, Pair<'ctx, Rule>>>(
    ctx: &mut Context<'ctx>,
    rule: Rule,
) -> Result<T> {
    let mut pairs = match Grammar::parse(rule, ctx.config.input) {
        Ok(pairs) => pairs,
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
                        notes.push(format!(
                            "Expected either {}",
                            positives
                                .iter()
                                .map(pretty_print_rule)
                                .unique()
                                .collect::<Vec<_>>()
                                .join(", ")
                        ));
                    }
                    if !negatives.is_empty() {
                        notes.push(format!(
                            "Are not allowed: {}",
                            negatives
                                .iter()
                                .map(pretty_print_rule)
                                .unique()
                                .collect::<Vec<_>>()
                                .join(", ")
                        ));
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
            bail!("Parsing errors found.");
        }
    };

    let res: T = ctx.parse(pairs.next().context("Parser grammar error")?);

    Ok(res)
}

/// Parses the source code for a file, and returns the parsed program.
///
/// The source code is in the `config` of the context `ctx`.
pub fn parse_file<'ctx>(ctx: &mut Context<'ctx>) -> Result<Program<'ctx>> {
    parse_rule(ctx, Rule::program)
}
