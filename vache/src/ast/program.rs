//! Parsing programs, and defining their representation in the AST.

use std::{collections::HashMap, default::default};

use pest::iterators::Pair;

use super::{Context, Fun, Parsable, Struct};
use crate::grammar::*;

/// A program: a collection of:
/// * structures
/// * functions
pub struct Program<'ctx> {
    /// Collection of functions defined in the program, indexed by their names.
    pub funs: HashMap<&'ctx str, Fun<'ctx>>,
    /// Collection of structures defined in the program, indexed by their names.
    pub structs: HashMap<&'ctx str, Struct<'ctx>>,
}

impl<'ctx> Program<'ctx> {
    /// Builds a program listing out of a list of structures and a list of
    /// functions.
    pub fn new(structs: Vec<Struct<'ctx>>, funs: Vec<Fun<'ctx>>) -> Self {
        Self {
            funs: funs.into_iter().map(|f| (f.name, f)).collect(),
            structs: structs.into_iter().map(|f| (f.name, f)).collect(),
        }
    }
}

impl<'ctx> From<Vec<Fun<'ctx>>> for Program<'ctx> {
    fn from(list: Vec<Fun<'ctx>>) -> Self {
        Program {
            funs: list.into_iter().map(|f| (f.name, f)).collect(),
            structs: HashMap::new(),
        }
    }
}

impl<'ctx> From<Fun<'ctx>> for Program<'ctx> {
    fn from(f: Fun<'ctx>) -> Self {
        Program::from(vec![f])
    }
}

impl<'ctx> Parsable<'ctx, Pair<'ctx, Rule>> for Program<'ctx> {
    fn parse(pair: Pair<'ctx, Rule>, ctx: &mut Context<'ctx>) -> Self {
        debug_assert!(matches!(pair.as_rule(), Rule::program));
        let pairs = pair.into_inner();

        let mut funs: HashMap<&str, _> = default();
        let structs = default();

        for pair in pairs {
            match pair.as_rule() {
                Rule::fun => {
                    let fun: Fun = ctx.parse(pair);
                    funs.insert(fun.name, fun);
                }
                Rule::struct_def => todo!(),
                _ => unreachable!(),
            }
        }

        Program { funs, structs }
    }
}
