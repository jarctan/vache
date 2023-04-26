//! Abstract Syntax Tree for the language.
//!
//! Each node in the tree = one file.

pub mod block;
pub mod expr;
pub mod fun;
pub mod program;
pub mod selfvisitor;
pub mod stmt;

pub use crate::ast::structure::Struct;
pub use crate::ast::ty::Ty;
pub use crate::ast::var::{Var, VarDef};
pub use block::Block;
pub use expr::Expr;
pub use fun::{Fun, FunSig};
pub use program::Program;
pub use selfvisitor::SelfVisitor;
pub use stmt::Stmt;
