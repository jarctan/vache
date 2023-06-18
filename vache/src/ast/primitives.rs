//! Parsing primitives (integers, strings, etc.).

use num_bigint::BigInt;
use pest::iterators::Pair;

use super::{Context, Parsable};
use crate::grammar::*;

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for &'ctx str {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        debug_assert!(pair.as_rule() == Rule::string);

        // Takes care of escaped newlines and tabs
        let value = pair.as_str().replace("\\n", "\n").replace("\\t", "\t");
        let value: &str = ctx.alloc(value);
        &value[1..value.len() - 1]
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for bool {
    fn parse(pair: Pair<'ctx, Rule>, _ctx: &Context<'ctx>) -> Self {
        debug_assert!(pair.as_rule() == Rule::boolean);

        match pair.as_str() {
            "true" => true,
            "false" => false,
            _ => unreachable!(),
        }
    }
}

impl<'ctx> Parsable<'ctx, Pair<'_, Rule>> for BigInt {
    fn parse(pair: Pair<Rule>, _ctx: &Context) -> Self {
        debug_assert!(pair.as_rule() == Rule::integer);

        let value = pair.as_str().replace('_', "");
        BigInt::parse_bytes(value.as_bytes(), 10).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[parses("\"this is a test\nAnd another\"" as string)]
    #[test]
    fn simple_string(string: &str) {
        assert_eq!(string, "this is a test\nAnd another");
    }

    #[parses("1234567890" as integer)]
    #[test]
    fn simple_decimal(integer: BigInt) {
        assert_eq!(integer, BigInt::from(1234567890));
    }
}
