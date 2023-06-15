//! Facilities for pest error reporting.

use crate::grammar::Rule;

/// Stringify a rule in a human-friendly format.
pub fn pretty_print_rule(rule: &Rule) -> &'static str {
    match rule {
        Rule::EOI => "end of input",
        Rule::program => "a program",
        Rule::fun => "a function",
        Rule::params => "function parameters",
        Rule::struct_def => "a `struct` definition",
        Rule::enum_def => "an `enum` definition",
        Rule::variant_def => "an `enum` variant",
        Rule::block => "a block",
        Rule::stmt => "a statement",
        Rule::while_loop => "a while loop",
        Rule::for_loop => "a for loop",
        Rule::assign => "an assignment",
        Rule::declare => "`var x = /**/`",
        Rule::vardef => "`x : type`",
        Rule::expr => "an expression",
        Rule::subexpr => "`( expr )`",
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
        Rule::ident => "an identifier (`a-Z[a-Z | 0-9]*` except reserved keywords)",
        Rule::namespaced => "`a::b::c`",
        Rule::if_then => "`if cond {}`",
        Rule::match_expr => "a `match`",
        Rule::array => "`[a,b,c,...]`",
        Rule::iter_ty => "`type..`",
        Rule::non_iter_ty => "a type (except an iterator type)",
        Rule::WHITESPACE => "a whitespace",
        Rule::COMMENT => "a comment",
        // Tokens
        Rule::sc => "`;`",
        Rule::lcb => "`{`",
        Rule::rcb => "`}`",
        Rule::eq => "`=`",
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
        Rule::eqq => "`==`",
        Rule::neq => "`!=`",
        Rule::cln => "`:`",
        Rule::cma => "`,`",
        Rule::rg => "`..`",
        Rule::arw => "`->`",
        Rule::farw => "`=>`",
        Rule::pipe => "`|`",
        Rule::lp => "`(`",
        Rule::rp => "`)`",
        Rule::lb => "`[`",
        Rule::rb => "`]`",
        Rule::dt => "`.`",
        Rule::clncln => "`::`",
        Rule::while_kw => "`while`",
        Rule::for_kw => "`for`",
        Rule::in_kw => "`in`",
        Rule::if_kw => "`if`",
        Rule::else_kw => "`else`",
        Rule::var_kw => "`var`",
        Rule::fn_kw => "`fn`",
        Rule::struct_kw => "`struct`",
        Rule::enum_kw => "`enum`",
        Rule::match_kw => "`match`",
        Rule::match_block => "`{ i1 => e1 | ... | in => en }",
        Rule::matched_expr => "an expression (except a struct instance)",
        Rule::matched_primitive | Rule::primitive_wo_struct => {
            "an expression (without postfix `()`, `[]` `.`, or `..`) except a structure instance"
        }
    }
}
