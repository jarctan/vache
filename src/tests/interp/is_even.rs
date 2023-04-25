use super::*;

#[test]
fn is_even() {
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

    // is 10 even?
    test(
        {
            let f2 = Fun {
                name: "main".to_string(),
                params: vec![],
                ret_ty: UnitT,
                body: stmts(vec![ExprS(call(
                    "print",
                    vec![call("is_even", vec![int(10)])],
                ))]),
            };

            vec![f1.clone(), f2]
        },
        "true \n", // space here because of the way `print` prints
    );

    // is 11 even?
    test(
        {
            let f2 = Fun {
                name: "main".to_string(),
                params: vec![],
                ret_ty: UnitT,
                body: stmts(vec![ExprS(call(
                    "print",
                    vec![call("is_even", vec![int(11)])],
                ))]),
            };

            vec![f1, f2]
        },
        "false \n", // space here because of the way `print` prints
    );
}
