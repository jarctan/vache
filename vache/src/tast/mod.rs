//! Typed AST for the language.
//!
//! Each node in the tree = one file. Some parts are re-exported from the parser
//! AST.

pub mod arg;
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

pub use arg::{Arg, ArgKind};
pub use block::Block;
pub use expr::{Expr, ExprKind};
pub use fun::{Fun, FunParam};
pub use mode::LhsMode;
pub use pat::{Pat, PatKind};
pub use place::{LhsPlace, Place, PlaceKind};
pub use program::Program;
pub use stmt::{Stmt, StmtKind};
pub use stratum::Stratum;
#[cfg(test)]
pub use var::vardef;
pub use var::{VarDef, VarUse};

pub use crate::ast::{Enum, Mode, Namespaced, Span, Struct, Ty, TySubst, TyUse, TyVar, Varname};
