#![feature(box_patterns)]

mod block;
mod expr;
mod fun;
mod stmt;
mod stratum;
mod var;
mod compile;
mod program;

use block::Block;
use fun::Fun;
use stratum::Stratum;
use var::VarDef;
use stmt::Stmt;
use program::Program;

use crate::expr::{Expr, Op};

fn fibo_fibo() -> Program {
    let s = Stratum::new();
    let f = Fun {
        name: "fibo_fibo".to_string(),
        quantifiers: vec![s],
        args: vec![VarDef::from("n", s)],
        body: Block::stmts({
            vec![
                Stmt::Assign(VarDef::from("d", s), Expr::int(2)),
                Stmt::Assign(VarDef::from("c", s), Expr::BinOp(Op::Less, Box::new(Expr::var("n")), Box::new(Expr::var("d")))),
                Stmt::If(Expr::var("c"), Block::expr(Expr::var("n")), Block {
                    stmts: vec![
                        Stmt::Assign(VarDef::from("a", s), Expr::BinOp(Op::Sub, Box::new(Expr::var("n")), Box::new(Expr::int(1)))),
                        Stmt::Assign(VarDef::from("a", s), Expr::BinOp(Op::Sub, Box::new(Expr::var("n")), Box::new(Expr::int(2)))),
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
                Stmt::Assign(VarDef::from("ivar_2", s1), Expr::BinOp(Op::Mod, Box::new(Expr::var("ivar_1")), Box::new(Expr::var("ivar_5")))),
                Stmt::Assign(VarDef::from("ivar_3", s1), Expr::int(0)),
            ],
            ret: Expr::BinOp(Op::Mod, Box::new(Expr::var("ivar_2")), Box::new(Expr::var("ivar_3")))
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
                Stmt::Assign(VarDef::from("c", s2), Expr::BinOp(Op::Less, Box::new(Expr::var("n")), Box::new(Expr::var("d")))),
                Stmt::If(Expr::var("c"), Block::expr(Expr::var("n")), Block {
                    stmts: vec![
                        Stmt::Assign(VarDef::from("a", s2), Expr::BinOp(Op::Sub, Box::new(Expr::var("n")), Box::new(Expr::int(1)))),
                        Stmt::Assign(VarDef::from("a", s2), Expr::BinOp(Op::Sub, Box::new(Expr::var("n")), Box::new(Expr::int(2)))),
                    ],
                    ret: Expr::call("fibo_fibo", vec![Expr::var("a"), Expr::var("b")]),
                }),
            ]
        }),
    };

    vec![f1, f2]
}

pub trait Compilable {
    fn compile(self, c: &mut Compiler) -> String;
}

pub struct Compiler {

}
impl Compiler {
    fn new() -> Self {
        Self {

        }
    }
    fn compile(&mut self, c: impl Compilable) -> String {
        c.compile(self)
    }
}

fn main() {
    let p1 = fibo_fibo();
    let p2 = th_fn();

    let compiler = Compiler::new();
    println!(compiler.compile(p1));

    println!("Hello, world!");
}
