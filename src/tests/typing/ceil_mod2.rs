use super::*;

/// Ceil modulo 2.
#[test]
fn ceil_mod_2() {
    check({
        let s1 = Stratum::new();
        let f1 = Fun {
            name: "is_even".to_string(),
            quantifiers: vec![s1],
            params: vec![vardef("ivar_1", s1, IntT)],
            ret_ty: BoolT,
            body: Block {
                stratum: Stratum::new(),
                stmts: vec![
                    Declare(vardef("ivar_5", s1, IntT), int(2)),
                    Declare(
                        vardef("ivar_2", s1, IntT),
                        binop(var("ivar_1"), "%", var("ivar_5")),
                    ),
                    Declare(vardef("ivar_3", s1, IntT), int(0)),
                ],
                ret: binop(binop(var("ivar_2"), "%", var("ivar_3")), "==", int(1)),
            },
        };

        let s2 = Stratum::new();
        let f2 = Fun {
            name: "ceil_mod_2".to_string(),
            quantifiers: vec![s2],
            params: vec![vardef("n", s2, IntT)],
            ret_ty: IntT,
            body: expr(IfE(
                boxed(call("is_even", vec![var("n")])),
                boxed(expr(var("n"))),
                boxed(expr(binop(var("n"), "+", int(1)))),
            )),
        };

        vec![f1, f2]
    });
}
