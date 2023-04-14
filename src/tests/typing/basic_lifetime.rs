use super::*;

/// Fibonacci example.
#[test]
fn basic_lifetime() {
    check({
        let a = Stratum::new();
        let b = Stratum::new();
        let f = Fun {
            name: "main".to_string(),
            quantifiers: vec![],
            params: vec![],
            ret_ty: UnitT,
            body: Block {
                stratum: a,
                stmts: vec![Declare(vardef("x", a, IntT), int(5))],
                ret: BlockE(boxed(Block {
                    stratum: b,
                    stmts: vec![
                        Declare(vardef("y", b, IntT), int(6)),
                        Assign(Var::from("y"), var("x")),
                    ],
                    ret: UnitE,
                })),
            },
        };
        vec![f]
    });
}
