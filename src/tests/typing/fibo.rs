use super::*;

/// Fibonacci example.
#[test]
fn fibo() {
    check({
        let s = Stratum::new();
        let f = Fun {
            name: "fibo".to_string(),
            quantifiers: vec![s],
            params: vec![vardef("n", s, IntT)],
            ret_ty: IntT,
            body: Block {
                stratum: Stratum::new(),
                stmts: vec![
                    Declare(vardef("d", s, IntT), int(2)),
                    Declare(
                        vardef("c", s, BoolT),
                        binop(var("n"), "<", var("d")),
                    ),
                ],
                ret: IfE(
                    boxed(var("c")),
                    boxed(expr(var("n"))),
                    boxed(Block {
                        stratum: Stratum::new(),
                        stmts: vec![
                            Declare(
                                vardef("a", s, IntT),
                                binop(var("n"), "-", int(1)),
                            ),
                            Declare(
                                vardef("b", s, IntT),
                                binop(var("n"), "-", int(2)),
                            ),
                        ],
                        ret: binop(call("fibo", vec![var("a")]), "+", call("fibo", vec![var("b")])),
                    }),
                ),
            },
        };
        vec![f]
    });
}