use rug::Integer;

use crate::var::Var;

pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Less,
    Mod,
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