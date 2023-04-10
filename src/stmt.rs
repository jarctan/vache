use crate::{var::VarDef, expr::Expr, block::Block};

pub enum Stmt {
    Assign(VarDef, Expr),
    If(Expr, Block, Block)
}