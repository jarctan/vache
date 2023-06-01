//! Standard id function.
//!
//! Complicated code to do simple things.
#![allow(missing_docs)]

use super::*;

pub fn id_fn() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![
            Fun {
                name: "id",
                params: vec![vardef("n", IntT)],
                ret_ty: IntT,
                body: expr(var("n")),
            },
            Fun {
                name: "main",
                params: vec![],
                ret_ty: UnitT,
                body: stmts(vec![
                    Declare(vardef("x", IntT), int(12)),
                    Declare(
                        vardef("res", IntT),
                        block(Block {
                            stmts: vec![Declare(vardef("n", IntT), call("id", vec![var("x")]))],
                            ret: var("n"),
                        }),
                    ),
                    print(vec![var("res")]),
                ]),
            },
        ],
    )
}
