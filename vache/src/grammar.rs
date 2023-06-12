//! Defining the grammar of the language.

#![allow(clippy::missing_docs_in_private_items)]

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct Grammar;
