//! Checking that any reference to an undeclared structure name triggers an
//! error, be it in the function parameters, variable declaration or in the
//! fields of a structure declaration.

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
#[should_panic]
fn unknown_struct_in_field() {
    test(Program::new(
        vec![Struct {
            name: "Person",
            fields: vec![
                ("name", StrT),
                ("age", IntT),
                ("house", StructT("UnknownStruct")), // should fail
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
            name: "test",
            params: vec![vardef("a", StructT("UnknownStruct"))], // should fail
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
            name: "test",
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![Declare(
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
        }],
    ));
}
