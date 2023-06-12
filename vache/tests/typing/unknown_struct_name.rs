//! Checking that any reference to an undeclared structure name triggers an
//! error, be it in the function parameters, variable declaration or in the
//! fields of a structure declaration.

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
#[should_fail(UNKNOWN_STRUCT_ERROR)]
fn unknown_struct_in_field() -> Program {
    Program::new(
        vec![Struct {
            name: "Person",
            fields: vec![
                ("name", strT()),
                ("age", intT()),
                ("house", structT("UnknownStruct")), // should fail
            ]
            .into_iter()
            .collect(),
            ..default()
        }],
        vec![],
    )
}

#[test]
#[should_fail(UNKNOWN_STRUCT_ERROR)]
fn unknown_struct_in_params() -> Program {
    Program::new(
        vec![person_struct()],
        vec![Fun {
            name: "test",
            params: vec![vardef("a", StructT("UnknownStruct"))], // should fail
            body: expr(UnitE),
            ..default()
        }],
    )
}

#[test]
#[should_fail(UNKNOWN_STRUCT_ERROR, TYPE_MISMATCH_ERROR)]
fn unknown_struct_in_declare() -> Program {
    Program::new(
        vec![person_struct()],
        vec![Fun {
            name: "test",
            body: stmts(vec![declare(
                vardef("john", StructT("UnknownStruct")), // should fail
                structure(
                    "Person",
                    vec![
                        ("name", string("doe")),
                        ("age", int(21)),
                        ("country", string("US")),
                    ],
                ),
            )]),
            ..default()
        }],
    )
}
