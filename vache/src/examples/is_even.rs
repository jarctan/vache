//! Basic "is_even" function.

use std::default::default;

use super::*;

/// Is n even.
pub fn is_even() -> Fun<'static> {
    Fun {
        name: "is_even",
        params: vec![vardef("n", intT())],
        ret_ty: boolT(),
        span: default(),
        body: Block {
            stmts: vec![
                declare(vardef("x", intT()), int(2)),
                declare(vardef("res", intT()), binop(var("n"), "%", var("x"))),
            ],
            ret: binop(var("res"), "==", int(0)),
            span: default(),
        },
    }
}

/// Is 10 even.
pub fn is_10_even() -> impl Into<Program<'static>> {
    let main = Fun {
        name: "main",
        body: stmts(vec![call_stmt(
            "print",
            vec![call("is_even", vec![int(10)])],
        )]),
        ..default()
    };

    vec![is_even(), main]
}

/// Is 11 even.
pub fn is_11_even() -> impl Into<Program<'static>> {
    let main = Fun {
        name: "main",
        body: stmts(vec![call_stmt(
            "print",
            vec![call("is_even", vec![int(11)])],
        )]),
        ..default()
    };

    vec![is_even(), main]
}
