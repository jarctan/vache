use super::*;

/// Fibonacci example.
#[test]
fn fibo() {
    test(vec![Fun {
        name: "fibo",
        params: vec![vardef("n", IntT)],
        ret_ty: intT(),
        body: Block {
            stmts: vec![
                declare(vardef("d", IntT), int(2)),
                declare(vardef("c", BoolT), binop(var("n"), "<", var("d"))),
            ],
            ret: IfE(
                boxed(var("c")),
                boxed(expr(var("n"))),
                boxed(Block {
                    stmts: vec![
                        declare(vardef("a", IntT), binop(var("n"), "-", int(1))),
                        declare(vardef("b", IntT), binop(var("n"), "-", int(2))),
                    ],
                    ret: binop(
                        call("fibo", vec![var("a")]),
                        "+",
                        call("fibo", vec![var("b")]),
                    ),
                    span: default(),
                }),
            )
            .into(),
            span: default(),
        },
        ..default()
    }]);
}
