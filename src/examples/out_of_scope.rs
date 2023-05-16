//! Assigning to a variable to a stratum that lives longer.
#![allow(missing_docs)]

use super::*;

pub fn out_of_scope() -> impl Into<Program> {
    vec![Fun {
        name: "main".to_string(),
        params: vec![],
        ret_ty: UnitT,
        body: Block {
            stmts: vec![
                Declare(vardef("n", IntT), int(12)),
                block_stmt(stmts(vec![
                    Declare(vardef("p", IntT), int(24)),
                    Assign(Place::from("n"), var("p")),
                    Assign(Place::from("p"), int(27)),
                ])),
                print(vec![var("n")]),
            ],
            ret: UnitE,
        },
    }]
}
