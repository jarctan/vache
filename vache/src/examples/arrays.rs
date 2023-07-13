//! Array-related examples.

#![allow(missing_docs)]

use super::*;

/// Create an array, and get an item from it.
pub fn simple_array_indexing<'ctx>() -> Program<'ctx> {
    Fun {
        name: "main",
        body: stmts(vec![
            declare(
                vardef("list", arrayT(&StrT)),
                array([
                    string("tomato"),
                    string("lettuce"),
                    string("zucchini"),
                    string("avocado"),
                ]),
            ),
            declare(vardef("item", StrT), index(var("list"), int(1))),
            debug([var("item")]),
        ]),
        ..default()
    }
    .into()
}

pub fn separate_index_for_array<'ctx>() -> Program<'ctx> {
    Fun {
        name: "main",
        body: stmts(vec![
            declare(
                vardef("list", ArrayT(&StrT)),
                array([
                    string("tomato"),
                    string("lettuce"),
                    string("zucchini"),
                    string("avocado"),
                ]),
            ),
            declare(vardef("ix", IntT), int(2)),
            declare(vardef("item", StrT), index(var("list"), var("ix"))),
            debug([var("item"), var("ix")]),
        ]),
        ..default()
    }
    .into()
}

pub fn simple_array_assignment<'ctx>() -> Program<'ctx> {
    Fun {
        name: "main",
        body: stmts(vec![
            declare(
                vardef("list", ArrayT(&StrT)),
                array([
                    string("tomato"),
                    string("lettuce"),
                    string("zucchini"),
                    string("avocado"),
                ]),
            ),
            declare(vardef("ix", IntT), int(2)),
            assign(idx_place(var("list"), var("ix")), string("cucumber")),
            declare(vardef("item", StrT), index(var("list"), var("ix"))),
            debug([var("item"), var("ix")]),
        ]),
        ..default()
    }
    .into()
}

pub fn assignment_while_borrowed<'ctx>() -> Program<'ctx> {
    Fun {
        name: "main",
        body: stmts(vec![
            declare(
                vardef("list", ArrayT(&StrT)),
                array([
                    string("tomato"),
                    string("lettuce"),
                    string("zucchini"),
                    string("avocado"),
                ]),
            ),
            declare(vardef("ix", IntT), int(2)),
            declare(vardef("item_bef", StrT), index(var("list"), var("ix"))),
            assign(idx_place(var("list"), var("ix")), string("cucumber")),
            declare(vardef("item", StrT), index(var("list"), var("ix"))),
            debug([var("item"), var("item_bef"), var("ix")]),
        ]),
        ..default()
    }
    .into()
}

pub fn cloning_array<'ctx>() -> Program<'ctx> {
    Fun {
        name: "main",
        body: stmts(vec![
            declare(
                vardef("list", ArrayT(&StrT)),
                array([
                    string("tomato"),
                    string("lettuce"),
                    string("zucchini"),
                    string("avocado"),
                ]),
            ),
            declare(vardef("list2", ArrayT(&StrT)), var("list")),
            assign(idx_place(var("list2"), int(1)), string("cabbage")),
            assign(idx_place(var("list"), int(2)), string("cucumber")),
            declare(vardef("item1", StrT), index(var("list"), int(2))),
            declare(vardef("item2", StrT), index(var("list2"), int(1))),
            debug([var("item1"), var("item2")]),
        ]),
        ..default()
    }
    .into()
}
