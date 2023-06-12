//! Standard id function.
//!
//! Complicated code to do simple things.
#![allow(missing_docs)]

use std::default::default;

use super::*;

pub fn id_fn() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![
            Fun {
                name: "id",
                params: vec![vardef("n", intT())],
                ret_ty: intT(),
                body: expr(var("n")),
                ..default()
            },
            Fun {
                name: "main",
                body: stmts(vec![
                    declare(vardef("x", intT()), int(12)),
                    declare(
                        vardef("res", intT()),
                        block(Block {
                            stmts: vec![declare(vardef("n", intT()), call("id", vec![var("x")]))],
                            ret: var("n"),
                            span: default(),
                        }),
                    ),
                    print(vec![var("res")]),
                ]),
                ..default()
            },
        ],
    )
}
