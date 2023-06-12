//! Checking that arrays type correctly.

use super::*;

#[test]
fn simple_array() {
    test(Program::new(
        vec![],
        vec![Fun {
            name: "main",
            body: stmts(vec![declare(
                vardef("list", ArrayT(&IntT)),
                array([int(0), int(1), int(2), int(3)]),
            )]),
            ..default()
        }],
    ));
}

#[should_fail(HETEROGENEOUS_LISTS_ERROR)]
#[test]
fn heterogeneous_list() -> Program {
    Program::new(
        vec![],
        vec![Fun {
            name: "main",
            body: stmts(vec![declare(
                vardef("l", ArrayT(&IntT)),
                array([int(0), int(1), int(2), string("not the right type!")]),
            )]),
            ..default()
        }],
    )
}

#[should_fail(HETEROGENEOUS_LISTS_ERROR)]
#[test]
fn heterogeneous_list2() -> Program {
    Program::new(
        vec![],
        vec![Fun {
            name: "main",
            body: stmts(vec![declare(
                vardef("l", ArrayT(&IntT)),
                array([
                    int(0),
                    string("not the right type!"),
                    string("again"),
                    string("and again"),
                ]),
            )]),
            ..default()
        }],
    )
}

/// Note: the empty list currently panics, but should not in the future.
#[should_fail(EMPTY_LIST_ERROR)]
#[test]
fn empty_list() -> Program {
    Program::new(
        vec![],
        vec![Fun {
            name: "main",
            body: stmts(vec![declare(vardef("l", ArrayT(&IntT)), array([]))]),
            ..default()
        }],
    )
}

#[test]
fn simple_array_indexing() {
    test(Program::new(
        vec![],
        vec![Fun {
            name: "main",
            body: stmts(vec![
                declare(
                    vardef("list", ArrayT(&IntT)),
                    array([int(0), int(1), int(2), int(3)]),
                ),
                declare(vardef("item", IntT), index(var("list"), int(1))),
                print([var("item")]),
            ]),
            ..default()
        }],
    ));
}
