//! Giving examples for lists of lists (matrices).
#![allow(missing_docs)]

use super::*;

/// Modify a cell, but no read afterwards on the matrix: therefore we should
/// consume when modifying.
pub fn simple_matrix_modify_consume() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![Fun {
            name: "main",
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![
                Declare(
                    vardef("list", ArrayT(&ArrayT(&IntT))),
                    array((0..2).map(|i| array((0..2).map(|j| int(i * 2 + j))))),
                ),
                Assign(idx_place(index(var("list"), int(0)), int(1)), int(42)),
            ]),
        }],
    )
}

/// Modify a cell, but read afterwards a cell in the matrix: therefore we should
/// only `&mut`.
pub fn simple_matrix_modify() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![Fun {
            name: "main",
            params: vec![],
            ret_ty: UnitT,
            body: stmts(vec![
                Declare(
                    vardef("list", ArrayT(&ArrayT(&IntT))),
                    array((0..2).map(|i| array((0..2).map(|j| int(i * 2 + j))))),
                ),
                Assign(idx_place(index(var("list"), int(0)), int(1)), int(42)),
                print([index(index(var("list"), int(0)), int(1))]),
            ]),
        }],
    )
}
