//! Basic "is_even" function.

use super::*;

/// Is n even.
pub fn is_even() -> Fun {
    Fun {
        name: "is_even".to_string(),
        params: vec![vardef("n", IntT)],
        ret_ty: BoolT,
        body: Block {
            stmts: vec![
                Declare(vardef("x", IntT), int(2)),
                Declare(vardef("res", IntT), binop(var("n"), "%", var("x"))),
            ],
            ret: binop(var("res"), "==", int(0)),
        },
    }
}

/// Is 10 even.
pub fn is_10_even() -> impl Into<Program> {
    let main = Fun {
        name: "main".to_string(),
        params: vec![],
        ret_ty: UnitT,
        body: stmts(vec![call_stmt(
            "print",
            vec![call("is_even", vec![int(10)])],
        )]),
    };

    vec![is_even(), main]
}

/// Is 11 even.
pub fn is_11_even() -> impl Into<Program> {
    let main = Fun {
        name: "main".to_string(),
        params: vec![],
        ret_ty: UnitT,
        body: stmts(vec![call_stmt(
            "print",
            vec![call("is_even", vec![int(11)])],
        )]),
    };

    vec![is_even(), main]
}
