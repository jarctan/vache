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
pub mod rvalue;

use std::fmt;

pub use fun::Fun;
pub use graph::{Cfg, CfgLabel};
pub use instr::{Branch, Instr, InstrKind};
pub use program::Program;
pub use rvalue::{RValue, VarMode};

pub use crate::tast::{Stratum, Struct, Ty, Var, VarDef};
