use super::*;

fn person_struct<'ctx>() -> Struct<'ctx> {
    Struct {
        name: "Person",
        fields: vec![("name", strT()), ("age", intT()), ("country", strT())]
            .into_iter()
            .collect(),
        ..default()
    }
}

#[vache_test("doe\n")]
fn simple_field_access() -> Program {
    Program::new(
        vec![person_struct()],
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
                declare(vardef("n", StrT), field(var("john"), "name")),
                debug(vec![var("n")]),
            ]),
            ..default()
        }],
    )
}

#[vache_test("12 doe\n")]
fn get_field_and_mutate() -> Program {
    examples::get_field_and_mutate()
}
