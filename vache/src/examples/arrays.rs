//! Array-related examples.
#![allow(missing_docs)]

use super::*;

/// Create an array, and get an item from it.
pub fn simple_array_indexing() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![Fun {
            name: "main",
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

pub fn separate_index_for_array() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![Fun {
            name: "main",
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
                Declare(vardef("ix", IntT), int(2)),
                Declare(vardef("item", StrT), index(var("list"), var("ix"))),
                print([var("item"), var("ix")]),
            ]),
        }],
    )
}

pub fn simple_array_assignment() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![Fun {
            name: "main",
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
                Declare(vardef("ix", IntT), int(2)),
                Assign(idx_place(var("list"), var("ix")), string("cucumber")),
                Declare(vardef("item", StrT), index(var("list"), var("ix"))),
                print([var("item"), var("ix")]),
            ]),
        }],
    )
}

pub fn assignment_while_borrowed() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![Fun {
            name: "main",
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
                Declare(vardef("ix", IntT), int(2)),
                Declare(vardef("item_bef", StrT), index(var("list"), var("ix"))),
                Assign(idx_place(var("list"), var("ix")), string("cucumber")),
                Declare(vardef("item", StrT), index(var("list"), var("ix"))),
                print([var("item"), var("item_bef"), var("ix")]),
            ]),
        }],
    )
}

pub fn cloning_array() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![Fun {
            name: "main",
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
                Declare(vardef("list2", ArrayT(boxed(StrT))), var("list")),
                Assign(idx_place(var("list2"), int(1)), string("cabbage")),
                Assign(idx_place(var("list"), int(2)), string("cucumber")),
                Declare(vardef("item1", StrT), index(var("list"), int(2))),
                Declare(vardef("item2", StrT), index(var("list2"), int(1))),
                print([var("item1"), var("item2")]),
            ]),
        }],
    )
}
