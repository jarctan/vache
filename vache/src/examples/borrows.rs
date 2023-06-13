//! Borrowing variables.

#![allow(missing_docs)]

use super::*;

pub fn one_borrow<'ctx>() -> Program<'ctx> {
    Fun {
        name: "main",
        body: stmts(vec![
            declare(vardef("n", intT()), int(10)),
            declare(vardef("y", intT()), var("n")),
            assign(Place::from("n"), binop(var("n"), "+", int(2))),
            print(vec![var("n"), var("y")]),
        ]),
        ..default()
    }
    .into()
}

pub fn two_borrows<'ctx>() -> Program<'ctx> {
    Fun {
        name: "main",
        body: stmts(vec![
            declare(vardef("n", intT()), int(10)),
            declare(vardef("y", intT()), var("n")),
            declare(vardef("z", intT()), var("n")),
            assign(Place::from("n"), binop(var("n"), "+", int(2))),
            print(vec![var("n"), var("y"), var("z")]),
        ]),
        ..default()
    }
    .into()
}
