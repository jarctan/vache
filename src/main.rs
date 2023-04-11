#![feature(box_patterns)]

mod ast;
mod compile;

use ast::*;
use compile::{Compilable, Compiler};

fn fibo_fibo() -> Program {
    let s = Stratum::new();
    let f = Fun {
        name: "fibo_fibo".to_string(),
        quantifiers: vec![s],
        args: vec![VarDef::from("n", s)],
        body: Block::stmts({
            vec![
                Stmt::Assign(VarDef::from("d", s), Expr::int(2)),
                Stmt::Assign(VarDef::from("c", s), Expr::call("<", vec![Expr::var("n"), Expr::var("d")])),
                Stmt::If(Expr::var("c"), Block::expr(Expr::var("n")), Block {
                    stmts: vec![
                        Stmt::Assign(VarDef::from("a", s), Expr::call("-", vec![Expr::var("n"), Expr::int(1)])),
                        Stmt::Assign(VarDef::from("a", s), Expr::call("-", vec![Expr::var("n"), Expr::int(2)])),
                    ],
                    ret: Expr::call("fibo_fibo", vec![Expr::var("a"), Expr::var("b")]),
                }),
            ]
        }),
    };
    vec![f]
}

fn th_fn() -> Program {
    let s1 = Stratum::new();
    let f1 = Fun {
        name: "is_this_even_qm".to_string(),
        quantifiers: vec![s1],
        args: vec![VarDef::from("ivar_1", s1)],
        body: Block {
            stmts: vec![
                Stmt::Assign(VarDef::from("ivar_5", s1), Expr::int(2)),
                Stmt::Assign(VarDef::from("ivar_2", s1), Expr::call("%", vec![Expr::var("ivar_1"), Expr::var("ivar_5")])),
                Stmt::Assign(VarDef::from("ivar_3", s1), Expr::int(0)),
            ],
            ret: Expr::call("%", vec![Expr::var("ivar_2"), Expr::var("ivar_3")])
        },
    };

    let s2 = Stratum::new();
    let f2 = Fun {
        name: "th_fn".to_string(),
        quantifiers: vec![s2],
        args: vec![VarDef::from("n", s2)],
        body: Block::stmts({
            vec![
                Stmt::Assign(VarDef::from("d", s2), Expr::int(2)),
                Stmt::Assign(VarDef::from("c", s2), Expr::call("<", vec![Expr::var("n"), Expr::var("d")])),
                Stmt::If(Expr::var("c"), Block::expr(Expr::var("n")), Block {
                    stmts: vec![
                        Stmt::Assign(VarDef::from("a", s2), Expr::call("-", vec![Expr::var("n"), Expr::int(1)])),
                        Stmt::Assign(VarDef::from("a", s2), Expr::call("-", vec![Expr::var("n"), Expr::int(2)])),
                    ],
                    ret: Expr::call("fibo_fibo", vec![Expr::var("a"), Expr::var("b")]),
                }),
            ]
        }),
    };

    vec![f1, f2]
}

fn main() {
    let p1 = fibo_fibo();
    let p2 = th_fn();

    let mut compiler = Compiler::new();

    println!("Hello, world!");
}
