//! Use a variable multiple times.
#![allow(missing_docs)]

use std::default::default;

use super::*;

pub fn multiple_refs() -> impl Into<Program<'static>> {
    vec![
        Fun {
            name: "is_even",
            params: vec![vardef("n", intT())],
            ret_ty: boolT(),
            body: Block {
                stmts: vec![
                    declare(vardef("x", intT()), int(2)),
                    declare(vardef("res", intT()), binop(var("n"), "%", var("x"))),
                ],
                ret: binop(var("res"), "==", int(0)),
                span: default(),
            },
            ..default()
        },
        Fun {
            name: "main",
            body: stmts(vec![
                declare(vardef("n", intT()), int(12)),
                declare(vardef("even1", BoolT), call("is_even", vec![var("n")])),
                declare(vardef("even2", BoolT), call("is_even", vec![var("n")])),
                assign(Place::from("n"), int(11)),
                declare(vardef("even3", BoolT), call("is_even", vec![var("n")])),
                print(vec![var("even1"), var("even2"), var("even3")]),
            ]),
            ..default()
        },
    ]
}
