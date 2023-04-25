use super::*;

/// Wrap builtin addition in a new function, and implement it.
///
/// Essentially a test to verify that giving twice the same argument leads to no problem, ie
/// we preserve the value of our argument in the end and we compute the right thing.
#[test]
fn custom_addition() {
    test(
        vec![
            Fun {
                name: "add".to_string(),
                params: vec![vardef("n", IntT), vardef("m", IntT)],
                ret_ty: IntT,
                body: expr(binop(var("m"), "+", var("n"))),
            },
            Fun {
                name: "main".to_string(),
                params: vec![],
                ret_ty: UnitT,
                body: Block {
                    stmts: vec![
                        Declare(vardef("n", IntT), int(12)),
                        Declare(vardef("res", IntT), call("add", vec![var("n"), var("n")])),
                        print(vec![var("res"), var("n")]),
                    ],
                    ret: UnitE,
                },
            },
        ],
        "24 12\n",
    );
}
