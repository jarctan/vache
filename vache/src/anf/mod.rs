//! Typed AST for the language.
//!
//! Each node in the tree = one file. Some parts are re-exported from the parser
//! AST.

pub mod block;
pub mod fun;
pub mod location;
pub mod place;
pub mod pointer;
pub mod program;
pub mod rvalue;
pub mod stmt;

pub use block::Block;
pub use fun::Fun;
pub use location::Loc;
pub use place::Place;
pub use pointer::{Pointer, Reference};
pub use program::Program;
pub use rvalue::RValue;
pub use stmt::Stmt;

pub use crate::tast::{Mode, Stratum, Struct, Ty, VarDef, VarUse, Varname};
