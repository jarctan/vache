use super::*;

/// Check twice if 10 is even, just to be sure.
/// One never knows, maybe it changed its mind?
///
/// This test is to prove that you can reuse a variable multiple times.
#[test]
fn multiple_refs() {
    test(
        vec![
            Fun {
                name: "is_even".to_string(),
                params: vec![vardef("n", IntT)],
                ret_ty: BoolT,
                body: Block {
                    stmts: vec![
                        Declare(vardef("x", IntT), int(2)),
                        Declare(vardef("res", IntT), binop(var("n"), "%", var("x"))),
                    ],
                    ret: binop(var("res"), "==", int(0)),
                },
            },
            Fun {
                name: "main".to_string(),
                params: vec![],
                ret_ty: UnitT,
                body: Block {
                    stmts: vec![
                        Declare(vardef("n", IntT), int(12)),
                        Declare(vardef("even1", BoolT), call("is_even", vec![var("n")])),
                        Declare(vardef("even2", BoolT), call("is_even", vec![var("n")])),
                        Declare(vardef("n", IntT), int(11)),
                        Declare(vardef("even3", BoolT), call("is_even", vec![var("n")])),
                        print(vec![var("even1"), var("even2"), var("even3")]),
                    ],
                    ret: UnitE,
                },
            },
        ],
        "true true false\n",
    );
}
