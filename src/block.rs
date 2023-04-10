use crate::{stmt::Stmt, expr::Expr};

pub struct Block {
    pub stmts: Vec<Stmt>,
    pub ret: Expr,
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