//! Checking what happens when assigning to a variable that lives longer that
//! the assigned expression.

use super::*;

#[test]
fn basic_lifetime() {
    test(vec![Fun {
        name: "main",
        params: vec![],
        ret_ty: UnitT,
        body: Block {
            stmts: vec![Declare(vardef("x", IntT), int(5))],
            ret: BlockE(boxed(Block {
                stmts: vec![
                    Declare(vardef("y", IntT), int(6)),
                    Assign(Place::from("y"), var("x")),
                ],
                ret: UnitE,
            })),
        },
    }]);
}
