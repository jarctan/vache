//! Parsing programs, and defining their representation in the AST.

use std::default::default;

use pest::iterators::Pairs;

use super::{Context, Enum, Fun, Parsable, Struct, Trait};
use crate::grammar::*;

/// A program: a collection of:
/// * structures
/// * functions
#[derive(Default)]
pub struct Program<'ctx> {
    /// List of functions defined in the program.
    pub funs: Vec<Fun<'ctx>>,
    /// List of structures defined in the program.
    pub structs: Vec<Struct<'ctx>>,
    /// List of enumerations defined in the program.
    pub enums: Vec<Enum<'ctx>>,
    /// List of traits defined in the program.
    pub traits: Vec<Trait<'ctx>>,
}

impl<'ctx> Program<'ctx> {
    /// Builds a program listing out of a list of structures and a list of
    /// functions.
    pub fn new(
        structs: Vec<Struct<'ctx>>,
        enums: Vec<Enum<'ctx>>,
        traits: Vec<Trait<'ctx>>,
        funs: Vec<Fun<'ctx>>,
    ) -> Self {
        Self {
            funs,
            structs,
            traits,
            enums,
        }
    }
}

impl<'ctx> From<Vec<Fun<'ctx>>> for Program<'ctx> {
    fn from(funs: Vec<Fun<'ctx>>) -> Self {
        Program { funs, ..default() }
    }
}

impl<'ctx> From<Fun<'ctx>> for Program<'ctx> {
    fn from(f: Fun<'ctx>) -> Self {
        Program::from(vec![f])
    }
}

impl<'ctx> Parsable<'ctx, Pairs<'ctx, Rule>> for Program<'ctx> {
    fn parse(pairs: Pairs<'ctx, Rule>, ctx: &Context<'ctx>) -> Self {
        let mut funs: Vec<_> = default();
        let mut structs: Vec<_> = default();
        let mut enums: Vec<_> = default();
        let mut traits: Vec<_> = default();

        for pair in pairs {
            match pair.as_rule() {
                Rule::fun => {
                    let fun: Fun = ctx.parse(pair);
                    funs.push(fun);
                }
                Rule::struct_def => {
                    let s: Struct = ctx.parse(pair);
                    structs.push(s);
                }
                Rule::enum_def => {
                    let e: Enum = ctx.parse(pair);
                    enums.push(e);
                }
                Rule::trait_def => {
                    let t: Trait = ctx.parse(pair);
                    traits.push(t);
                }
                _ => unreachable!(),
            }
        }

        Program {
            funs,
            structs,
            traits,
            enums,
        }
    }
}
