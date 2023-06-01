//! Change the value of a field through another variable.

use super::*;

#[allow(missing_docs)]
pub fn change_field() -> impl Into<Program<'static>> {
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
                Assign(Place::from("n"), string("dupont")),
                print(vec![var("n"), field(var("john"), "name")]),
            ]),
        }],
    )
}
