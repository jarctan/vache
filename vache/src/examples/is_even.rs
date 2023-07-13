//! Basic "is_even" function.

use super::*;

/// Is n even.
pub fn is_even() -> Fun<'static> {
    Fun {
        name: "is_even",
        params: vec![param("n", intT())],
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
    }
}

/// Is 10 even.
pub fn is_10_even() -> impl Into<Program<'static>> {
    let main = Fun {
        name: "main",
        body: stmts(vec![call_stmt(
            "debug",
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
            "debug",
            vec![call("is_even", vec![int(11)])],
        )]),
        ..default()
    };

    vec![is_even(), main]
}
