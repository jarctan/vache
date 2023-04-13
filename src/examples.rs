//! Several handy examples.

use crate::ast::*;
use Ty::*;

/// Simple example.
pub(crate) fn simple() -> Program {
    let s = Stratum::new();
    let f = Fun {
        name: "simple".to_string(),
        quantifiers: vec![s],
        args: vec![VarDef::from("n", s, IntT)],
        body: Block::stmts({
            vec![
                Stmt::Declare(VarDef::from("d", s, IntT), Expr::int(2)),
                Stmt::Declare(VarDef::from("e", s, BoolT), Expr::var("n")),
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
        args: vec![VarDef::from("n", s, IntT)],
        body: Block {
            stmts: vec![
                Stmt::Declare(VarDef::from("d", s, IntT), Expr::int(2)),
                Stmt::Declare(
                    VarDef::from("c", s, IntT),
                    Expr::call("<", vec![Expr::var("n"), Expr::var("d")]),
                ),
            ],
            ret: Expr::If(
                Box::new(Expr::var("c")),
                Box::new(Block::expr(Expr::var("n"))),
                Box::new(Block {
                    stmts: vec![
                        Stmt::Declare(
                            VarDef::from("a", s, IntT),
                            Expr::call("-", vec![Expr::var("n"), Expr::int(1)]),
                        ),
                        Stmt::Declare(
                            VarDef::from("a", s, IntT),
                            Expr::call("-", vec![Expr::var("n"), Expr::int(2)]),
                        ),
                    ],
                    ret: Expr::call("fibo_fibo", vec![Expr::var("a"), Expr::var("b")]),
                }),
            ),
        },
    };
    let f2 = Fun {
        name: "fibo_fibo".to_string(),
        quantifiers: vec![s],
        args: vec![VarDef::from("n", s, IntT)],
        body: Block {
            stmts: vec![
                Stmt::Declare(VarDef::from("d", s, IntT), Expr::int(2)),
                Stmt::Declare(
                    VarDef::from("c", s, IntT),
                    Expr::call("<", vec![Expr::var("n"), Expr::var("d")]),
                ),
            ],
            ret: Expr::If(
                Box::new(Expr::var("c")),
                Box::new(Block::expr(Expr::var("n"))),
                Box::new(Block {
                    stmts: vec![
                        Stmt::Declare(
                            VarDef::from("a", s, IntT),
                            Expr::call("-", vec![Expr::var("n"), Expr::int(1)]),
                        ),
                        Stmt::Declare(
                            VarDef::from("a", s, IntT),
                            Expr::call("-", vec![Expr::var("n"), Expr::int(2)]),
                        ),
                    ],
                    ret: Expr::call("fibo_fibo", vec![Expr::var("a"), Expr::var("b")]),
                }),
            ),
        },
    };
    vec![f, f2]
}

/// Another two-function example.
pub(crate) fn th_fn() -> Program {
    let s1 = Stratum::new();
    let f1 = Fun {
        name: "is_this_even_qm".to_string(),
        quantifiers: vec![s1],
        args: vec![VarDef::from("ivar_1", s1, IntT)],
        body: Block {
            stmts: vec![
                Stmt::Declare(VarDef::from("ivar_5", s1, IntT), Expr::int(2)),
                Stmt::Declare(
                    VarDef::from("ivar_2", s1, IntT),
                    Expr::call("%", vec![Expr::var("ivar_1"), Expr::var("ivar_5")]),
                ),
                Stmt::Declare(VarDef::from("ivar_3", s1, IntT), Expr::int(0)),
            ],
            ret: Expr::call("%", vec![Expr::var("ivar_2"), Expr::var("ivar_3")]),
        },
    };

    let s2 = Stratum::new();
    let f2 = Fun {
        name: "th_fn".to_string(),
        quantifiers: vec![s2],
        args: vec![VarDef::from("n", s2, IntT)],
        body: Block {
            stmts: vec![
                Stmt::Declare(VarDef::from("d", s2, IntT), Expr::int(2)),
                Stmt::Declare(
                    VarDef::from("c", s2, IntT),
                    Expr::call("<", vec![Expr::var("n"), Expr::var("d")]),
                ),
            ],
            ret: Expr::If(
                Box::new(Expr::var("c")),
                Box::new(Block::expr(Expr::var("n"))),
                Box::new(Block {
                    stmts: vec![
                        Stmt::Declare(
                            VarDef::from("a", s2, IntT),
                            Expr::call("-", vec![Expr::var("n"), Expr::int(1)]),
                        ),
                        Stmt::Declare(
                            VarDef::from("a", s2, IntT),
                            Expr::call("-", vec![Expr::var("n"), Expr::int(2)]),
                        ),
                    ],
                    ret: Expr::call("fibo_fibo", vec![Expr::var("a"), Expr::var("b")]),
                }),
            ),
        },
    };

    vec![f1, f2]
}