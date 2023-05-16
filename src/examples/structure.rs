//! What happens if we get the value of a field, and change that value? Should
//! it propagate back into the original structure declaration? As it stands in
//! the semantics of the language, the answer is no.
#![allow(missing_docs)]

use super::*;

pub fn get_field_and_mutate() -> impl Into<Program> {
    Program::new(
        vec![Struct {
            name: "Person".to_string(),
            fields: vec![
                ("name".to_string(), StrT),
                ("age".to_string(), IntT),
                ("country".to_string(), StrT),
            ]
            .into_iter()
            .collect(),
        }],
        vec![Fun {
            name: "main".to_string(),
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![
                Declare(
                    vardef("john", StructT("Person".to_string())),
                    structure(
                        "Person",
                        vec![
                            ("name".to_string(), string("doe")),
                            ("age".to_string(), int(21)),
                            ("country".to_string(), string("US")),
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
