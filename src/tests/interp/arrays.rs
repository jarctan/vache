//! Checking that arrays type correctly.

use super::*;

#[test]
fn simple_array_indexing() {
    test(
        Program::new(
            vec![],
            vec![Fun {
                name: "main".to_string(),
                params: vec![],
                ret_ty: UnitT,
                body: stmts(vec![
                    Declare(
                        vardef("list", ArrayT(boxed(StrT))),
                        array([
                            string("tomato"),
                            string("lettuce"),
                            string("zucchini"),
                            string("avocado"),
                        ]),
                    ),
                    Declare(vardef("item", StrT), index(var("list"), int(1))),
                    print([var("item")]),
                ]),
            }],
        ),
        "lettuce\n",
    );
}
