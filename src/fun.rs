use crate::{var::VarDef, block::Block};

use super::stratum::Stratum;

pub struct Fun {
    pub name: String,
    pub quantifiers: Vec<Stratum>,
    pub args: Vec<VarDef>,
    pub body: Block,
}