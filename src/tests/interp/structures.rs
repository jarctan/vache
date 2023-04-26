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
fn simple_field_access() {
    test(
        Program::new(
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
                    print(vec![var("n")]),
                ]),
            }],
        ),
        "doe\n",
    );
}
