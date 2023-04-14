//! Visiting the AST.

use super::{Block, Expr, Fun, Program, Stmt};

/// Visitor of the AST.
///
/// By default, all functions for non-terminal nodes will just explore their children recursively.
/// Override any method definition to change that behavior for any given node in the AST.
pub trait SelfVisitor {
    /// Output of the visitor.
    ///
    /// The output must have a monoidal structure, ie we can add two together
    /// to produce a new one, and have a default one. This is witnessed by the
    /// `Default` and `Extend` trait requirements.
    type Output: Default + Extend<Self::Output>;

    /// Visits a block.
    fn visit_block(&mut self, b: Block) -> Self::Output {
        let mut o = Self::Output::default();
        o.extend(b.stmts.into_iter().map(|s| self.visit_stmt(s)));
        o.extend_one(self.visit_expr(b.ret));
        o
    }

    /// Visits an expression.
    fn visit_expr(&mut self, e: Expr) -> Self::Output;

    /// Visits a function.
    fn visit_fun(&mut self, f: Fun) -> Self::Output {
        self.visit_block(f.body)
    }

    /// Visits a program.
    fn visit_program(&mut self, p: Program) -> Self::Output {
        let mut o = Self::Output::default();
        o.extend(p.into_iter().map(|f| self.visit_fun(f)));
        o
    }

    /// Visits a statement.
    fn visit_stmt(&mut self, s: Stmt) -> Self::Output;
}
