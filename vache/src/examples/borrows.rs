//! Borrowing variables.
#![allow(missing_docs)]

use std::default::default;

use super::*;

pub fn one_borrow() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![Fun {
            name: "main",
            body: stmts(vec![
                declare(vardef("n", intT()), int(10)),
                declare(vardef("y", intT()), var("n")),
                assign(Place::from("n"), binop(var("n"), "+", int(2))),
                print(vec![var("n"), var("y")]),
            ]),
            ..default()
        }],
    )
}

pub fn two_borrows() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![Fun {
            name: "main",
            body: stmts(vec![
                declare(vardef("n", intT()), int(10)),
                declare(vardef("y", intT()), var("n")),
                declare(vardef("z", intT()), var("n")),
                assign(Place::from("n"), binop(var("n"), "+", int(2))),
                print(vec![var("n"), var("y"), var("z")]),
            ]),
            ..default()
        }],
    )
}
