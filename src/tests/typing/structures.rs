use super::*;

fn person_struct() -> Struct {
    Struct {
        name: "Person".to_string(),
        fields: vec![
            ("name".to_string(), StrT),
            ("age".to_string(), IntT),
            ("country".to_string(), StrT),
        ]
        .into_iter()
        .collect(),
    }
}

#[test]
fn simple_structure() {
    test(Program::new(
        vec![person_struct()],
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
            name: "main".to_string(),
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![Declare(
                vardef("john", StructT("Person".to_string())), // should fail!
                structure(
                    "Person",
                    vec![
                        ("name".to_string(), string("doe")),
                        ("age".to_string(), int(21)),
                    ],
                ),
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
            name: "main".to_string(),
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![Declare(
                vardef("john", StructT("Person".to_string())),
                structure(
                    "Person",
                    vec![
                        ("name".to_string(), string("doe")),
                        ("age".to_string(), int(21)),
                        ("country".to_string(), string("US")),
                        ("city".to_string(), string("San Francisco")), // should fail!
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
            name: "main".to_string(),
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![Declare(
                vardef("john", StructT("Person".to_string())),
                structure(
                    "Person",
                    vec![
                        ("name".to_string(), string("doe")),
                        ("age".to_string(), string("21")), // should fail!
                        ("country".to_string(), string("US")),
                    ],
                ),
            )]),
        }],
    ));
}
