use super::*;

#[vache_test("Hello, world!\n")]
fn hello_world() -> Program {
    Program::from(vec![Fun {
        name: "main",
        body: Block {
            stmts: vec![
                declare(vardef("s", StrT), string("Hello, world!")),
                debug(vec![var("s")]),
            ],
            ret: UnitE.into(),
            span: default(),
        },
        ..default()
    }])
}
