use super::*;

/// Ceil modulo 2.
#[test]
fn ceil_mod_2() {
    test({
        let f1 = Fun {
            name: "is_even",
            params: vec![vardef("n", IntT)],
            ret_ty: boolT(),
            body: Block {
                stmts: vec![
                    declare(vardef("x", IntT), int(2)),
                    declare(vardef("res", IntT), binop(var("n"), "%", var("x"))),
                ],
                ret: binop(var("res"), "==", int(0)),
                span: default(),
            },
            ..default()
        };

        let f2 = Fun {
            name: "ceil_mod_2",
            params: vec![vardef("n", IntT)],
            ret_ty: intT(),
            body: expr(IfE(
                boxed(call("is_even", vec![var("n")])),
                boxed(expr(var("n"))),
                boxed(expr(binop(var("n"), "+", int(1)))),
            )),
            ..default()
        };

        vec![f1, f2]
    });
}
