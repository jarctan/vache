//! Checking that any reference to an undeclared structure name triggers an
//! error, be it in the function parameters, variable declaration or in the
//! fields of a structure declaration.

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
#[should_panic]
fn unknown_struct_in_field() {
    test(Program::new(
        vec![Struct {
            name: "Person".to_string(),
            fields: vec![
                ("name".to_string(), StrT),
                ("age".to_string(), IntT),
                ("house".to_string(), StructT("UnknownStruct".to_string())), // should fail
            ]
            .into_iter()
            .collect(),
        }],
        vec![],
    ));
}

#[test]
#[should_panic]
fn unknown_struct_in_params() {
    test(Program::new(
        vec![person_struct()],
        vec![Fun {
            name: "test".to_string(),
            params: vec![vardef("a", StructT("UnknownStruct".to_string()))], // should fail
            ret_ty: UnitT,
            body: expr(UnitE),
        }],
    ));
}

#[test]
#[should_panic]
fn unknown_struct_in_declare() {
    test(Program::new(
        vec![person_struct()],
        vec![Fun {
            name: "test".to_string(),
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![Declare(
                vardef("john", StructT("UnknownStruct".to_string())), // should fail
                structure(
                    "Person",
                    vec![
                        ("name".to_string(), string("doe")),
                        ("age".to_string(), int(21)),
                        ("country".to_string(), string("US")),
                    ],
                ),
            )]),
        }],
    ));
}
