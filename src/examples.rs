//! Several handy examples.

use crate::ast::*;
use Ty::*;
use Stmt::*;
use Expr::*;
use crate::ast::block::*;
use crate::ast::expr::*;
use crate::ast::var::*;

/// Simple example.
pub(crate) fn simple() -> Program {
    let s = Stratum::new();
    let f = Fun {
        name: "simple".to_string(),
        quantifiers: vec![s],
        args: vec![vardef("n", s, IntT)],
        ret_ty: UnitT,
        body: stmts({
            vec![
                Declare(vardef("d", s, IntT), int(2)),
                Declare(vardef("e", s, IntT), var("n")),
            ]
        }),
    };
    vec![f]
}

/// Fibo fibo example.
pub(crate) fn fibo_fibo() -> Program {
    let s = Stratum::new();
    let f = Fun {
        name: "fibo_fibo".to_string(),
        quantifiers: vec![s],
        args: vec![vardef("n", s, IntT)],
        ret_ty: IntT,
        body: Block {
            stmts: vec![
                Declare(vardef("d", s, IntT), int(2)),
                Declare(
                    vardef("c", s, IntT),
                    binop(var("n"), "<", var("d")),
                ),
            ],
            ret: If(
                boxed(var("c")),
                boxed(expr(var("n"))),
                boxed(Block {
                    stmts: vec![
                        Declare(
                            vardef("a", s, IntT),
                            binop(var("n"), "-", int(1)),
                        ),
                        Declare(
                            vardef("a", s, IntT),
                            binop(var("n"), "-", int(2)),
                        ),
                    ],
                    ret: call("fibo_fibo", vec![var("a"), var("b")]),
                }),
            ),
        },
    };
    vec![f]
}

/// Ceil modulo 2.
pub(crate) fn ceil_mod_2() -> Program {
    let s1 = Stratum::new();
    let f1 = Fun {
        name: "is_even".to_string(),
        quantifiers: vec![s1],
        args: vec![vardef("ivar_1", s1, IntT)],
        ret_ty: BoolT,
        body: Block {
            stmts: vec![
                Declare(vardef("ivar_5", s1, IntT), int(2)),
                Declare(
                    vardef("ivar_2", s1, IntT),
                    binop(var("ivar_1"), "%", var("ivar_5")),
                ),
                Declare(vardef("ivar_3", s1, IntT), int(0)),
            ],
            ret: binop(binop(var("ivar_2"), "%", var("ivar_3")), "==", int(1)),
        },
    };

    let s2 = Stratum::new();
    let f2 = Fun {
        name: "ceil_mod_2".to_string(),
        quantifiers: vec![s2],
        args: vec![vardef("n", s2, IntT)],
        ret_ty: IntT,
        body: expr(
            If(
                boxed(call("is_even", vec![var("n")])),
                boxed(expr(var("n"))),
                boxed(expr(binop(var("n"), "+", int(1))))
            )
        ),
    };

    vec![f1, f2]
}