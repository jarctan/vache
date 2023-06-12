//! Assigning to a variable to a stratum that lives longer.
#![allow(missing_docs)]

use std::default::default;

use super::*;

pub fn out_of_scope() -> impl Into<Program<'static>> {
    vec![Fun {
        name: "main",
        body: stmts(vec![
            declare(vardef("n", intT()), int(12)),
            block_stmt(stmts(vec![
                declare(vardef("p", intT()), int(24)),
                assign(Place::from("n"), var("p")),
                assign(Place::from("p"), int(27)),
            ])),
            print(vec![var("n")]),
        ]),
        ..default()
    }]
}
