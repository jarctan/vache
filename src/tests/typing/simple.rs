use super::*;

#[test]
fn simple_fun() {
    check(|_| {
        let stm_v = StratumVar::new();
        let s = Stratum::from(stm_v);
        let f = Fun {
            name: "simple".to_string(),
            quantifiers: vec![stm_v],
            params: vec![vardef("n", s, IntT)],
            ret_ty: ret_ty(UnitT, s),
            body: stmts(|_| {
                vec![
                    Declare(vardef("d", s, IntT), int(2)),
                    Declare(vardef("e", s, IntT), var("n")),
                ]
            }),
        };
        vec![f]
    });
}
