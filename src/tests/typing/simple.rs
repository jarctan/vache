use super::*;

#[test]
fn simple_fun() {
    check({
        let s = Stratum::new();
        let f = Fun {
            name: "simple".to_string(),
            quantifiers: vec![s],
            params: vec![vardef("n", s, IntT)],
            ret_ty: UnitT,
            body: stmts({
                vec![
                    Declare(vardef("d", s, IntT), int(2)),
                    Declare(vardef("e", s, IntT), var("n")),
                ]
            }),
        };
        vec![f]
    });
}