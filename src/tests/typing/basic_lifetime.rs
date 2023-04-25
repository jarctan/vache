use super::*;

/// Fibonacci example.
#[test]
fn basic_lifetime() {
    test(vec![Fun {
        name: "main".to_string(),
        params: vec![],
        ret_ty: UnitT,
        body: Block {
            stmts: vec![Declare(vardef("x", IntT), int(5))],
            ret: BlockE(boxed(Block {
                stmts: vec![
                    Declare(vardef("y", IntT), int(6)),
                    Assign(Var::from("y"), var("x")),
                ],
                ret: UnitE,
            })),
        },
    }]);
}
