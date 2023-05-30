//! Parsing programs, and defining their representation in the AST.

use std::{collections::HashMap, default::default};

use pest::iterators::Pair;

use super::{Context, Fun, Parsable, Struct};
use crate::grammar::*;

/// A program: a collection of:
/// * structures
/// * functions
pub struct Program {
    /// Collection of functions defined in the program, indexed by their names.
    pub funs: HashMap<String, Fun>,
    /// Collection of structures defined in the program, indexed by their names.
    pub structs: HashMap<String, Struct>,
}

impl Program {
    /// Builds a program listing out of a list of structures and a list of
    /// functions.
    pub fn new(structs: Vec<Struct>, funs: Vec<Fun>) -> Self {
        Self {
            funs: funs.into_iter().map(|f| (f.name.clone(), f)).collect(),
            structs: structs.into_iter().map(|f| (f.name.clone(), f)).collect(),
        }
    }
}

impl From<Vec<Fun>> for Program {
    fn from(list: Vec<Fun>) -> Self {
        Program {
            funs: list.into_iter().map(|f| (f.name.clone(), f)).collect(),
            structs: HashMap::new(),
        }
    }
}

impl From<Fun> for Program {
    fn from(f: Fun) -> Self {
        Program::from(vec![f])
    }
}

impl Parsable<Pair<'_, Rule>> for Program {
    fn parse(pair: Pair<Rule>, ctx: &mut Context) -> Self {
        debug_assert!(matches!(pair.as_rule(), Rule::program));
        let pairs = pair.into_inner();

        let mut funs: HashMap<String, _> = default();
        let structs = default();

        for pair in pairs {
            match pair.as_rule() {
                Rule::fun => {
                    let fun: Fun = ctx.parse(pair);
                    funs.insert(fun.name.clone(), fun);
                }
                Rule::struct_def => todo!(),
                _ => unreachable!(),
            }
        }

        Program { funs, structs }
    }
}
