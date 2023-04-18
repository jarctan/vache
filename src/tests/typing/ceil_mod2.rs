use super::*;

/// Ceil modulo 2.
#[test]
fn ceil_mod_2() {
    check({
        let s1_v = StratumVar::new();
        let s1 = Stratum::from(s1_v);
        let f1 = Fun {
            name: "is_even".to_string(),
            quantifiers: vec![s1_v],
            params: vec![vardef("ivar_1", s1, IntT)],
            ret_ty: ret_ty(BoolT, s1),
            body: Block {
                stratum: Stratum::new_concrete(),
                stmts: vec![
                    Declare(vardef("ivar_5", s1, IntT), int(2)),
                    Declare(
                        vardef("ivar_2", s1, IntT),
                        binop(var("ivar_1"), "%", var("ivar_5"), s1, s1),
                    ),
                    Declare(vardef("ivar_3", s1, IntT), int(0)),
                ],
                ret: binop(
                    binop(var("ivar_2"), "%", var("ivar_3"), s1, s1),
                    "==",
                    int(1),
                    s1,
                    s1,
                ),
            },
        };

        let s2_v = StratumVar::new();
        let s2 = Stratum::from(s2_v);
        let f2 = Fun {
            name: "ceil_mod_2".to_string(),
            quantifiers: vec![s2_v],
            params: vec![vardef("n", s2, IntT)],
            ret_ty: ret_ty(IntT, s2),
            body: expr(IfE(
                boxed(call("is_even", vec![s2], vec![var("n")], s2)),
                boxed(expr(var("n"))),
                boxed(expr(binop(var("n"), "+", int(1), s2, s2))),
            )),
        };

        vec![f1, f2]
    });
}
