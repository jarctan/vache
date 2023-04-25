use super::*;

/// Ceil modulo 2.
#[test]
fn ceil_mod_2() {
    compile(check({
        let f1 = Fun {
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
        };

        let f2 = Fun {
            name: "ceil_mod_2".to_string(),
            params: vec![vardef("n", IntT)],
            ret_ty: IntT,
            body: expr(IfE(
                boxed(call("is_even", vec![var("n")])),
                boxed(expr(var("n"))),
                boxed(expr(binop(var("n"), "+", int(1)))),
            )),
        };

        vec![f1, f2]
    }));
}
