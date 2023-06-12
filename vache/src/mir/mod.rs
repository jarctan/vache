//! Medium Intermediate Representation for the language.
//!
//! This representation is the first destructured, control flow-graph
//! representation.
//!
//! It goes after the typed AST, and before the final backend version in the
//! compilation process.
//!
//! Each node in the tree = one file. Some parts are re-exported from the typed
//! AST.

pub mod fun;
pub mod graph;
pub mod instr;
pub mod program;

use std::fmt;

pub use fun::Fun;
pub use graph::{Cfg, CfgI, CfgLabel};
#[cfg(test)]
pub use instr::instr;
pub use instr::{Branch, Instr, InstrKind};
pub use program::Program;

pub use crate::anf::{
    Loc, Mode, Place, Pointer, RValue, Reference, Stratum, Struct, Ty, VarDef, VarUse, Varname,
};
#[cfg(test)]
pub use crate::tast::vardef;
