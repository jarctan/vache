//! Facilities for pest error reporting.

use crate::grammar::Rule;

/// Stringify a rule in a human-friendly format.
pub fn pretty_print_rule(rule: &Rule) -> &'static str {
    match rule {
        Rule::EOI => "end of input",
        Rule::program => "a program",
        Rule::fun => "a function",
        Rule::params => "function parameters",
        Rule::struct_def => "a structure declaration",
        Rule::block => "a block",
        Rule::stmt => "a statement",
        Rule::while_loop => "a while loop",
        Rule::assign => "an assignment",
        Rule::declare => "`var x = /**/`",
        Rule::vardef => "`x : type`",
        Rule::expr => "an expression",
        Rule::binop => " a binary operator",
        Rule::with_postfix => "an expression",
        Rule::primitive => "an expression (without postfix `()`, `[]` `.`, or `..`)",
        Rule::field_postfix => "`.field`",
        Rule::index_postfix => "`[index]`",
        Rule::call_postfix => "`(arg1, arg2, ...)`",
        Rule::range_postfix => "`..expr`",
        Rule::struct_instance => "`MyStruct { ... }`",
        Rule::unit => "`()`",
        Rule::integer => "an integer",
        Rule::string => "a string",
        Rule::ty => "a type",
        Rule::array_ty => "an array type",
        Rule::bool_ty => "the `bool` type",
        Rule::str_ty => "the `str` type",
        Rule::int_ty => "the `int` type",
        Rule::keyword => "some reserved keyword",
        Rule::infix => "an infix operator",
        Rule::add => "`+`",
        Rule::sub => "`-`",
        Rule::mul => "`*`",
        Rule::div => "`/`",
        Rule::rem => "`%`",
        Rule::pow => "`^`",
        Rule::le => "`<=`",
        Rule::lt => "`<`",
        Rule::ge => "`>=`",
        Rule::gt => "`>`",
        Rule::eq => "`==`",
        Rule::neq => "`!=`",
        Rule::ident => "an identifier",
        Rule::if_then => "`if cond {}`",
        Rule::array => "`[.,.,...]`",
        Rule::iter_ty => "`type..`",
        Rule::iter_tail => "`..`",
        Rule::non_iter_ty => "a type (except an iterator type)",
        Rule::WHITESPACE => "a whitespace",
        Rule::COMMENT => "a comment",
    }
}
