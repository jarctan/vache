//! Checking that arrays type correctly.

use super::*;

#[test]
fn simple_array() -> Result<()> {
    test(Program::from(Fun {
        name: "main",
        body: stmts(vec![declare(
            vardef("list", ArrayT(&IntT)),
            array([int(0), int(1), int(2), int(3)]),
        )]),
        ..default()
    }))
}

#[should_fail(HETEROGENEOUS_LISTS_ERROR)]
#[test]
fn heterogeneous_list() -> Program {
    Fun {
        name: "main",
        body: stmts(vec![declare(
            vardef("l", ArrayT(&IntT)),
            array([int(0), int(1), int(2), string("not the right type!")]),
        )]),
        ..default()
    }
    .into()
}

#[should_fail(HETEROGENEOUS_LISTS_ERROR)]
#[test]
fn heterogeneous_list2() -> Program {
    Fun {
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
    }
    .into()
}

#[test]
fn empty_list() -> Result<()> {
    test(Program::from(Fun {
        name: "main",
        body: stmts(vec![declare(vardef("l", ArrayT(&IntT)), array([]))]),
        ..default()
    }))
}

#[test]
fn simple_array_indexing() -> Result<()> {
    test(Program::from(Fun {
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
    }))
}
