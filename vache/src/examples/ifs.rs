//! Example with if expressions.
#![allow(missing_docs)]

use std::default::default;

use super::*;

pub fn simple_if<'ctx>() -> impl Into<Program<'ctx>> {
    vec![Fun {
        name: "main",
        body: expr(if_e(
            binop(int(1), "==", int(0)),
            stmts([declare(vardef("x", intT()), int(2))]),
            stmts([declare(vardef("y", intT()), int(2))]),
        )),
        ..default()
    }]
}
