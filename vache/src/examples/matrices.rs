//! Giving examples for lists of lists (matrices).
#![allow(missing_docs)]

use std::default::default;

use super::*;

/// Modify a cell, but no read afterwards on the matrix: therefore we should
/// consume when modifying.
pub fn simple_matrix_modify_consume() -> impl Into<Program<'static>> {
    Program::new(
        vec![],
        vec![Fun {
            name: "main",
            body: stmts(vec![
                declare(
                    vardef("list", ArrayT(&ArrayT(&IntT))),
                    array((0..2).map(|i| array((0..2).map(|j| int(i * 2 + j))))),
                ),
                assign(idx_place(index(var("list"), int(0)), int(1)), int(42)),
            ]),
            ..default()
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
            body: stmts(vec![
                declare(
                    vardef("list", ArrayT(&ArrayT(&IntT))),
                    array((0..2).map(|i| array((0..2).map(|j| int(i * 2 + j))))),
                ),
                assign(idx_place(index(var("list"), int(0)), int(1)), int(42)),
                print([index(index(var("list"), int(0)), int(1))]),
            ]),
            ..default()
        }],
    )
}
