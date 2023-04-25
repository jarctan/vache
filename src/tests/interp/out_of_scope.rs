use super::*;

#[test]
fn out_of_scope() {
    test(
        vec![Fun {
            name: "main".to_string(),
            params: vec![],
            ret_ty: UnitT,
            body: Block {
                stmts: vec![
                    Declare(vardef("n", IntT), int(12)),
                    block_stmt(stmts(vec![
                        Declare(vardef("p", IntT), int(24)),
                        Assign(Var::from("n"), var("p")),
                    ])),
                    print(vec![var("n")]),
                ],
                ret: UnitE,
            },
        }],
        "24\n",
    );
}
