//! Typed AST for the language.
//!
//! Each node in the tree = one file. Some parts are re-exported from the parser
//! AST.

pub mod fun;
pub mod instr;
pub mod program;
pub mod rvalue;

use std::collections::HashMap;
use std::fmt;

pub use fun::Fun;
pub use instr::Instr;
pub use program::Program;
pub use rvalue::RValue;

pub use crate::ast::structure::Struct;
pub use crate::ast::ty::Ty;
pub use crate::ast::var::{Var, VarDef};

/// A label in the control flow graph.
#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub struct CfgLabel(u64);

impl CfgLabel {
    /// Creates a new CFG label with id `id`.
    pub fn new(id: u64) -> CfgLabel {
        CfgLabel(id)
    }
}

impl fmt::Debug for CfgLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "§{}", self.0)
    }
}

/// A control flow graph.
///
/// Nodes of this graph are labeled by `CfgLabel` and contain instructions.
pub type Cfg = HashMap<CfgLabel, Instr>;
