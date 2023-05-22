//! Example with if expressions.
#![allow(missing_docs)]

use super::*;

pub fn simple_if() -> impl Into<Program> {
    vec![Fun {
        name: "main".to_string(),
        params: vec![],
        ret_ty: UnitT,
        body: expr(if_e(
            binop(int(1), "==", int(0)),
            stmts([Declare(vardef("x", IntT), int(2))]),
            stmts([Declare(vardef("y", IntT), int(2))]),
        )),
    }]
}
