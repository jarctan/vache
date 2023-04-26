use super::*;

#[test]
fn is_even() {
    test(
        vec![Fun {
            name: "main".to_string(),
            params: vec![],
            ret_ty: UnitT,
            body: Block {
                stmts: vec![
                    Declare(vardef("s", StrT), string("Hello, world!")),
                    print(vec![var("s")]),
                ],
                ret: UnitE,
            },
        }],
        "Hello, world!\n",
    );
}
