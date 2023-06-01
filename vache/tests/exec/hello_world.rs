use super::*;

#[vache_test("Hello, world!\n")]
fn hello_world() -> Program {
    Program::from(vec![Fun {
        name: "main",
        params: vec![],
        ret_ty: UnitT,
        body: Block {
            stmts: vec![
                Declare(vardef("s", StrT), string("Hello, world!")),
                print(vec![var("s")]),
            ],
            ret: UnitE,
        },
    }])
}
