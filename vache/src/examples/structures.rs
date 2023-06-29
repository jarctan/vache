//! What happens if we get the value of a field, and change that value? Should
//! it propagate back into the original structure declaration? As it stands in
//! the semantics of the language, the answer is no.

#![allow(missing_docs)]

use super::*;

pub fn get_field_and_mutate<'ctx>() -> Program<'ctx> {
    Program::new(
        vec![Struct {
            name: "Person",
            fields: vec![("name", strT()), ("age", intT()), ("country", strT())]
                .into_iter()
                .collect(),
            ..default()
        }],
        default(),
        vec![Fun {
            name: "main",
            body: stmts(vec![
                declare(
                    vardef("john", varT("Person")),
                    structure(
                        "Person",
                        vec![
                            ("name", string("doe")),
                            ("age", int(21)),
                            ("country", string("US")),
                        ],
                    ),
                ),
                declare(vardef("n", strT()), field(var("john"), "name")),
                assign(Place::from("n"), string("12")),
                print(vec![var("n"), field(var("john"), "name")]),
            ]),
            ..default()
        }],
    )
}
