//! What happens if we get the value of a field, and change that value? Should
//! it propagate back into the original structure declaration? As it stands in
//! the semantics of the language, the answer is no.
#![allow(missing_docs)]

use super::*;

pub fn get_field_and_mutate<'ctx>() -> impl Into<Program<'ctx>> {
    Program::new(
        vec![Struct {
            name: "Person",
            fields: vec![("name", StrT), ("age", IntT), ("country", StrT)]
                .into_iter()
                .collect(),
        }],
        vec![Fun {
            name: "main",
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![
                Declare(
                    vardef("john", StructT("Person")),
                    structure(
                        "Person",
                        vec![
                            ("name", string("doe")),
                            ("age", int(21)),
                            ("country", string("US")),
                        ],
                    ),
                ),
                Declare(vardef("n", StrT), field(var("john"), "name")),
                Assign(Place::from("n"), string("12")),
                print(vec![var("n"), field(var("john"), "name")]),
            ]),
        }],
    )
}
