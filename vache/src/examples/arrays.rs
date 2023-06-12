//! Array-related examples.
#![allow(missing_docs)]

use std::default::default;

use super::*;

/// Create an array, and get an item from it.
pub fn simple_array_indexing() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![Fun {
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
                print([var("item")]),
            ]),
            ..default()
        }],
    )
}

pub fn separate_index_for_array() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![Fun {
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
                print([var("item"), var("ix")]),
            ]),
            ..default()
        }],
    )
}

pub fn simple_array_assignment() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![Fun {
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
                print([var("item"), var("ix")]),
            ]),
            ..default()
        }],
    )
}

pub fn assignment_while_borrowed() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![Fun {
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
                print([var("item"), var("item_bef"), var("ix")]),
            ]),
            ..default()
        }],
    )
}

pub fn cloning_array() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![Fun {
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
                print([var("item1"), var("item2")]),
            ]),
            ..default()
        }],
    )
}
