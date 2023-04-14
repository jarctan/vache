//! Abstract Syntax Tree module for the language.
//!
//! Each node in the tree = one file.

pub mod block;
pub mod expr;
pub mod fun;
pub mod program;
pub mod selfvisitor;
pub mod stmt;
pub mod stratum;
pub mod ty;
pub mod var;
pub mod visitor;

/// Alias for `Box::new()` to make it shorter and easier
/// to use in manually-created ASTs.
pub fn boxed<T>(t: T) -> Box<T> {
    Box::new(t)
}

pub use block::Block;
pub use expr::Expr;
pub use fun::{Fun, FunSig};
pub use program::Program;
pub use selfvisitor::SelfVisitor;
pub use stmt::Stmt;
pub use stratum::Stratum;
pub use ty::Ty;
pub use var::{Var, VarDef};
pub use visitor::Visitor;
