//! Basic while loop.
#![allow(missing_docs)]

use super::*;

pub fn borrows() -> impl Into<Program> {
    Program::new(
        vec![],
        vec![Fun {
            name: "main".to_string(),
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![
                Declare(vardef("n", IntT), int(10)),
                Declare(vardef("y", IntT), var("n")),
                Assign(Var::from("n"), binop(var("n"), "+", int(2))),
                print(vec![var("n"), var("y")]),
            ]),
        }],
    )
}
