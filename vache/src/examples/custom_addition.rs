//! Wrap builtin addition in a new function, and implement it.
//!
//! Essentially an example to verify that giving twice the same argument leads
//! to no problem, ie we preserve the value of our argument in the end and we
//! compute the right thing.

use std::default::default;

use super::*;

#[allow(missing_docs)]
pub fn custom_addition() -> impl Into<Program<'static>> {
    vec![
        Fun {
            name: "add",
            params: vec![param("n", intT()), param("m", intT())],
            ret_ty: intT(),
            body: expr(binop(var("m"), "+", var("n"))),
            ..default()
        },
        Fun {
            name: "main",
            body: Block {
                stmts: vec![
                    declare(vardef("n", intT()), int(12)),
                    declare(vardef("res", intT()), call("add", vec![var("n"), var("n")])),
                    debug(vec![var("res"), var("n")]),
                ],
                ret: UnitE.into(),
                span: default(),
            },
            ..default()
        },
    ]
}
