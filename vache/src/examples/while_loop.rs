//! Basic while loop.

#![allow(missing_docs)]

use std::default::default;

use super::*;

/// Basic while-loop program.
pub fn while_loop<'ctx>() -> Program<'ctx> {
    Fun {
        name: "main",
        body: stmts(vec![
            declare(vardef("n", intT()), int(10)),
            WhileS {
                cond: binop(var("n"), ">=", int(5)),
                body: stmts(vec![
                    debug(vec![var("n")]),
                    assign(Place::from("n"), binop(var("n"), "-", int(1))),
                ]),
            }
            .into(),
            debug(vec![var("n")]),
        ]),
        ..default()
    }
    .into()
}

/// Two while loops one after the other.
pub fn while_loop2<'ctx>() -> Program<'ctx> {
    Fun {
        name: "main",
        body: stmts(vec![
            declare(vardef("n", intT()), int(10)),
            WhileS {
                cond: binop(var("n"), ">=", int(5)),
                body: stmts(vec![
                    debug(vec![var("n")]),
                    assign(Place::from("n"), binop(var("n"), "-", int(1))),
                ]),
            }
            .into(),
            debug(vec![var("n")]),
            WhileS {
                cond: binop(var("n"), ">=", int(1)),
                body: stmts(vec![
                    debug(vec![var("n")]),
                    assign(Place::from("n"), binop(var("n"), "-", int(1))),
                ]),
            }
            .into(),
            debug(vec![var("n")]),
        ]),
        ..default()
    }
    .into()
}
