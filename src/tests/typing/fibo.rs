use super::*;

/// Fibonacci example.
#[test]
fn fibo() {
    compile(check(vec![Fun {
        name: "fibo".to_string(),
        params: vec![vardef("n", IntT)],
        ret_ty: IntT,
        body: Block {
            stmts: vec![
                Declare(vardef("d", IntT), int(2)),
                Declare(vardef("c", BoolT), binop(var("n"), "<", var("d"))),
            ],
            ret: IfE(
                boxed(var("c")),
                boxed(expr(var("n"))),
                boxed(Block {
                    stmts: vec![
                        Declare(vardef("a", IntT), binop(var("n"), "-", int(1))),
                        Declare(vardef("b", IntT), binop(var("n"), "-", int(2))),
                    ],
                    ret: binop(
                        call("fibo", vec![var("a")]),
                        "+",
                        call("fibo", vec![var("b")]),
                    ),
                }),
            ),
        },
    }]));
}
