//! Parsing primitives (integers, strings, etc.).

use num_bigint::BigInt;
use pest::iterators::Pair;

use super::{Context, Parsable};
use crate::grammar::*;

impl Parsable<Pair<'_, Rule>> for String {
    fn parse(pair: Pair<Rule>, _ctx: &mut Context) -> Self {
        debug_assert!(pair.as_rule() == Rule::string);

        // Takes care of escaped newlines and tabs
        let value = pair.as_str().replace("\\n", "\n").replace("\\t", "\t");
        value[1..value.len() - 1].to_string()
    }
}

impl Parsable<Pair<'_, Rule>> for BigInt {
    fn parse(pair: Pair<Rule>, _ctx: &mut Context) -> Self {
        debug_assert!(pair.as_rule() == Rule::integer);

        let value = pair.as_str().replace('_', "");
        BigInt::parse_bytes(value.as_bytes(), 10).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use pest::Parser;

    use super::*;
    use crate::grammar::Grammar;

    #[test]
    fn simple_string() {
        let input = "\"this is a test\nAnd another\"";
        let mut parsed = Grammar::parse(Rule::string, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let string: String = ctx.parse(pair);
        assert_eq!(string, "this is a test\nAnd another");
    }

    #[test]
    fn simple_decimal() {
        let input = "1234567890";
        let mut parsed = Grammar::parse(Rule::integer, input).expect("failed to parse");
        let pair = parsed.next().expect("Nothing parsed");
        let mut ctx = Context::new(input);
        let integer: BigInt = ctx.parse(pair);
        assert_eq!(integer, BigInt::from(1234567890));
    }
}
