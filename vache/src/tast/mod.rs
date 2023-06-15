//! Typed AST for the language.
//!
//! Each node in the tree = one file. Some parts are re-exported from the parser
//! AST.

pub mod block;
pub mod expr;
pub mod fun;
pub mod mode;
pub mod pat;
pub mod place;
pub mod program;
pub mod stmt;
pub mod stratum;
pub mod var;

pub use block::Block;
pub use expr::{Expr, ExprKind};
pub use fun::Fun;
pub use mode::Mode;
pub use pat::{Pat, PatKind};
pub use place::{Place, PlaceKind};
pub use program::Program;
pub use stmt::Stmt;
pub use stratum::Stratum;
#[cfg(test)]
pub use var::vardef;
pub use var::VarDef;

pub use crate::ast::{Enum, Namespaced, Span, Struct, Ty, TyUse, VarUse, Varname};
