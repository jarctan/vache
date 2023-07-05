//! Doing the most basic example ever.

use super::*;

#[test]
fn simple_fun() -> Result<()> {
    test(vec![
        Fun {
            name: "test",
            params: vec![param("n", IntT)],
            body: stmts(vec![
                declare(vardef("d", IntT), int(2)),
                declare(vardef("e", IntT), var("n")),
            ]),
            ..default()
        },
        Fun {
            name: "main",
            ..default()
        },
    ])
}
