//! Change the value of a field through another variable.

use std::default::default;

use super::*;

#[allow(missing_docs)]
pub fn change_field() -> impl Into<Program<'static>> {
    Program::new(
        vec![Struct {
            name: "Person",
            fields: vec![("name", strT()), ("age", intT()), ("country", strT())]
                .into_iter()
                .collect(),
            ..default()
        }],
        vec![Fun {
            name: "main",
            body: stmts(vec![
                declare(
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
                declare(vardef("n", strT()), field(var("john"), "name")),
                assign(Place::from("n"), string("dupont")),
                print(vec![var("n"), field(var("john"), "name")]),
            ]),
            ..default()
        }],
    )
}
