//! Borrowing variables.
#![allow(missing_docs)]

use super::*;

pub fn one_borrow() -> impl Into<Program> {
    Program::new(
        vec![],
        vec![Fun {
            name: "main".to_string(),
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![
                Declare(vardef("n", IntT), int(10)),
                Declare(vardef("y", IntT), var("n")),
                Assign(Place::from("n"), binop(var("n"), "+", int(2))),
                print(vec![var("n"), var("y")]),
            ]),
        }],
    )
}

pub fn two_borrows() -> impl Into<Program> {
    Program::new(
        vec![],
        vec![Fun {
            name: "main".to_string(),
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![
                Declare(vardef("n", IntT), int(10)),
                Declare(vardef("y", IntT), var("n")),
                Declare(vardef("z", IntT), var("n")),
                Assign(Place::from("n"), binop(var("n"), "+", int(2))),
                print(vec![var("n"), var("y"), var("z")]),
            ]),
        }],
    )
}
