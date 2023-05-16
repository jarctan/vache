//! Array-related examples.
#![allow(missing_docs)]

use super::*;

/// Create an array, and get an item from it.
pub fn simple_array_indexing() -> impl Into<Program> {
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
    )
}
