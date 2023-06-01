use super::*;

fn person_struct<'ctx>() -> Struct<'ctx> {
    Struct {
        name: "Person",
        fields: vec![("name", StrT), ("age", IntT), ("country", StrT)]
            .into_iter()
            .collect(),
    }
}

#[vache_test("doe\n")]
fn simple_field_access() -> Program {
    Program::new(
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
                print(vec![var("n")]),
            ]),
        }],
    )
}

#[vache_test("12 doe\n")]
fn get_field_and_mutate() -> Program {
    examples::get_field_and_mutate().into()
}
