//! Typed AST for the language.
//!
//! Each node in the tree = one file. Some parts are re-exported from the parser
//! AST.

pub mod block;
pub mod branch;
pub mod fun;
pub mod location;
pub mod place;
pub mod pointer;
pub mod program;
pub mod reference;
pub mod rvalue;
pub mod stmt;

pub use block::Block;
pub use branch::Branch;
pub use fun::Fun;
pub use location::Loc;
pub use place::Place;
pub use pointer::Pointer;
pub use program::Program;
pub use reference::{LhsRef, Reference};
pub use rvalue::RValue;
pub use stmt::{Stmt, StmtKind};

pub use crate::tast::{
    Enum, FunParam, LhsMode, Mode, Namespaced, Span, Stratum, Struct, Ty, VarDef, VarUse, Varname,
};
