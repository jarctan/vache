use super::*;

#[test]
fn simple_fun() {
    test(vec![Fun {
        name: "simple".to_string(),
        params: vec![vardef("n", IntT)],
        ret_ty: UnitT,
        body: stmts(vec![
            Declare(vardef("d", IntT), int(2)),
            Declare(vardef("e", IntT), var("n")),
        ]),
    }]);
}
