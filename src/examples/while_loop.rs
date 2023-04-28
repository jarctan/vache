//! Basic while loop.
#![allow(missing_docs)]

use super::*;

pub fn while_loop() -> impl Into<Program> {
    Program::new(
        vec![],
        vec![Fun {
            name: "main".to_string(),
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![
                Declare(vardef("n", IntT), int(10)),
                WhileS {
                    cond: binop(var("n"), ">=", int(5)),
                    body: stmts(vec![
                        print(vec![var("n")]),
                        Assign(Var::from("n"), binop(var("n"), "-", int(1))),
                    ]),
                },
                print(vec![var("n")]),
            ]),
        }],
    )
}
