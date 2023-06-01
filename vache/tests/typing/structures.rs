//! Checking structure definition, and obvious errors like accessing an
//! unknown field, wrong instantiation of a structure, etc.

use super::*;

fn person_struct<'ctx>() -> Struct<'ctx> {
    Struct {
        name: "Person",
        fields: vec![("name", StrT), ("age", IntT), ("country", StrT)]
            .into_iter()
            .collect(),
    }
}

#[test]
fn simple_structure() {
    test(Program::new(
        vec![person_struct()],
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
            ]),
        }],
    ));
}

#[test]
#[should_panic]
fn access_unknown_field() {
    test(Program::new(
        vec![person_struct()],
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
                Declare(vardef("n", StrT), field(var("john"), "test")), // should fail!
            ]),
        }],
    ));
}

#[test]
#[should_panic]
fn missing_field() {
    test(Program::new(
        vec![person_struct()],
        vec![Fun {
            name: "main",
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![Declare(
                vardef("john", StructT("Person")), // should fail!
                structure("Person", vec![("name", string("doe")), ("age", int(21))]),
            )]),
        }],
    ));
}

#[test]
#[should_panic]
fn extra_field() {
    test(Program::new(
        vec![person_struct()],
        vec![Fun {
            name: "main",
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![Declare(
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
        }],
    ));
}

#[test]
#[should_panic]
fn type_mismatch() {
    test(Program::new(
        vec![person_struct()],
        vec![Fun {
            name: "main",
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![Declare(
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
        }],
    ));
}
