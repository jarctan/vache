//! Checking what happens when assigning to a variable that lives longer that
//! the assigned expression.

use super::*;

#[test]
fn basic_lifetime() {
    test(vec![Fun {
        name: "main",
        body: Block {
            stmts: vec![declare(vardef("x", IntT), int(5))],
            ret: block(Block {
                stmts: vec![
                    declare(vardef("y", IntT), int(6)),
                    assign(Place::from("y"), var("x")),
                ],
                ret: UnitE.into(),
                span: default(),
            }),
            span: default(),
        },
        ..default()
    }]);
}
