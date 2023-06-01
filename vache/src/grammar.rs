//! Defining the grammar of the language.

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct Grammar;
