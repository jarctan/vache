//! Typed AST for the language.
//!
//! Each node in the tree = one file. Some parts are re-exported from the parser
//! AST.

pub mod fun;
pub mod graph;
pub mod instr;
pub mod program;
pub mod rvalue;

use std::fmt;

pub use fun::Fun;
pub use graph::{Cfg, CfgLabel};
pub use instr::{Branch, Instr};
pub use program::Program;
pub use rvalue::{RValue, VarMode};

pub use crate::ast::structure::Struct;
pub use crate::ast::ty::Ty;
pub use crate::ast::var::{Var, VarDef};
