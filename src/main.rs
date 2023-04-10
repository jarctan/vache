#![feature(box_patterns)]

use std::sync::atomic::AtomicU64;

use rug::Integer;

pub static COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Copy)]
pub struct Stratum(u64);
impl Stratum {
    pub fn new() -> Self {
        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

pub struct Var(String);

impl<T: ToString> From<T> for Var {
    fn from(v: T) -> Self {
        Self(v.to_string())
    }
}

pub struct VarDef {
    /// Name
    name: String,
    /// Stratum
    stratum: Stratum,
}
impl VarDef {
    fn from(name: impl ToString, stratum: Stratum) -> Self {
        let name = name.to_string();
        Self {
            name,
            stratum,
        }
    }
}

pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Less,
    Mod,
}

pub struct Block {
    stmts: Vec<Stmt>,
    ret: Expr,
}
impl Block {
    pub fn expr(expr: Expr) -> Self {
        Block {
            stmts: vec![],
            ret: expr,
        }
    }

    pub fn stmts(stmts: impl IntoIterator<Item=Stmt>) -> Self {
        Block {
            stmts: stmts.into_iter().collect(),
            ret: Expr::Unit,
        }
    }
}

pub enum Expr {
    Unit,
    Integer(Integer),
    Var(Var),
    BinOp(Op, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
}

impl Expr {
    pub fn var(v: impl ToString) -> Self {
        Expr::Var(v.into())
    }
    pub fn int(value: impl Into<Integer>) -> Self {
        Expr::Integer(value.into())
    }
    pub fn call(name: impl ToString, stmts: impl IntoIterator<Item=Expr>) -> Self {
        Expr::Call(name.to_string(), stmts.into_iter().collect())
    }
}

impl From<u64> for Expr {
    fn from(value: u64) -> Self {
        Expr::Integer(Integer::from(value))
    }
}

impl From<Var> for Expr {
    fn from(v: Var) -> Self {
        Expr::Var(v)
    }
}

pub enum Stmt {
    Assign(VarDef, Expr),
    If(Expr, Block, Block)
}

pub struct Fun {
    name: String,
    quantifiers: Vec<Stratum>,
    args: Vec<VarDef>,
    body: Block,
}

fn fibo_fibo() {
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
}

fn th_fn() {
    let s1 = Stratum::new();
    let f = Fun {
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
    let f = Fun {
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
}

fn main() {
    println!("Hello, world!");
}
