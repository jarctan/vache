use super::*;

/// Fibonacci example.
#[test]
fn fibo() {
    check(|_| {
        let stm_v = StratumVar::new();
        let stm = Stratum::from(stm_v);
        let f = Fun {
            name: "fibo".to_string(),
            quantifiers: vec![stm_v],
            params: vec![vardef("n", stm, IntT)],
            ret_ty: ret_ty(IntT, stm),
            body: Block {
                stratum: Stratum::new_concrete(),
                stmts: vec![
                    Declare(vardef("d", stm, IntT), int(2)),
                    Declare(
                        vardef("c", stm, BoolT),
                        binop(var("n"), "<", var("d"), stm, stm),
                    ),
                ],
                ret: IfE(
                    boxed(var("c")),
                    boxed(expr(var("n"))),
                    boxed(Block {
                        stratum: Stratum::new_concrete(),
                        stmts: vec![
                            Declare(
                                vardef("a", stm, IntT),
                                binop(var("n"), "-", int(1), stm, stm),
                            ),
                            Declare(
                                vardef("b", stm, IntT),
                                binop(var("n"), "-", int(2), stm, stm),
                            ),
                        ],
                        ret: binop(
                            call("fibo", vec![stm], vec![var("a")], stm),
                            "+",
                            call("fibo", vec![stm], vec![var("b")], stm),
                            stm,
                            stm,
                        ),
                    }),
                ),
            },
        };
        vec![f]
    });
}
