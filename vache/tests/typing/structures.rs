//! Checking structure definition, and obvious errors like accessing an
//! unknown field, wrong instantiation of a structure, etc.

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

#[test]
fn simple_structure() {
    test(Program::new(
        vec![person_struct()],
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
                declare(vardef("n", StrT), field(var("john"), "name")),
            ]),
            ..default()
        }],
    ));
}

#[test]
#[should_fail(FIELD_ACCESS_ERROR)]
fn access_unknown_field() -> Program {
    Program::new(
        vec![person_struct()],
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
                declare(vardef("n", StrT), field(var("john"), "test")), // should fail!
            ]),
            ..default()
        }],
    )
}

#[test]
#[should_fail(STRUCT_INSTANCE_ERROR)]
fn missing_field() -> Program {
    Program::new(
        vec![person_struct()],
        vec![Fun {
            name: "main",
            body: stmts(vec![declare(
                vardef("john", StructT("Person")), // should fail!
                structure("Person", vec![("name", string("doe")), ("age", int(21))]),
            )]),
            ..default()
        }],
    )
}

#[test]
#[should_fail(STRUCT_INSTANCE_ERROR)]
fn extra_field() -> Program {
    Program::new(
        vec![person_struct()],
        vec![Fun {
            name: "main",
            body: stmts(vec![declare(
                vardef("john", StructT("Person")),
                structure(
                    "Person",
                    vec![
                        ("name", string("doe")),
                        ("age", int(21)),
                        ("country", string("US")),
                        ("city", string("San Francisco")), // should fail!
                    ],
                ),
            )]),
            ..default()
        }],
    )
}

#[test]
#[should_fail(TYPE_MISMATCH_ERROR)]
fn type_mismatch() -> Program {
    Program::new(
        vec![person_struct()],
        vec![Fun {
            name: "main",
            body: stmts(vec![declare(
                vardef("john", StructT("Person")),
                structure(
                    "Person",
                    vec![
                        ("name", string("doe")),
                        ("age", string("21")), // should fail!
                        ("country", string("US")),
                    ],
                ),
            )]),
            ..default()
        }],
    )
}
