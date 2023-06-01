//! Checking that arrays type correctly.

use super::*;

#[test]
fn simple_array() {
    test(Program::new(
        vec![],
        vec![Fun {
            name: "main",
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![Declare(
                vardef("list", ArrayT(boxed(IntT))),
                array([int(0), int(1), int(2), int(3)]),
            )]),
        }],
    ));
}

#[test]
#[should_panic]
fn heterogeneous_list() {
    test(Program::new(
        vec![],
        vec![Fun {
            name: "main",
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![Declare(
                vardef("l", ArrayT(boxed(IntT))),
                array([int(0), int(1), int(2), string("not the right type!")]),
            )]),
        }],
    ));
}

#[test]
#[should_panic]
fn heterogeneous_list2() {
    test(Program::new(
        vec![],
        vec![Fun {
            name: "main",
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![Declare(
                vardef("l", ArrayT(boxed(IntT))),
                array([
                    int(0),
                    string("not the right type!"),
                    string("again"),
                    string("and again"),
                ]),
            )]),
        }],
    ));
}

/// Note: the empty list currently panics, but should not in the future.
#[test]
#[should_panic]
fn empty_list() {
    test(Program::new(
        vec![],
        vec![Fun {
            name: "main",
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![Declare(vardef("l", ArrayT(boxed(IntT))), array([]))]),
        }],
    ));
}

#[test]
fn simple_array_indexing() {
    test(Program::new(
        vec![],
        vec![Fun {
            name: "main",
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![
                Declare(
                    vardef("list", ArrayT(boxed(IntT))),
                    array([int(0), int(1), int(2), int(3)]),
                ),
                Declare(vardef("item", IntT), index(var("list"), int(1))),
                print([var("item")]),
            ]),
        }],
    ));
}
