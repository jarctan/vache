//! Visiting the AST.

use super::{Block, Expr, Fun, Program, Stmt};

/// Visitor of the AST.
///
/// By default, all functions for non-terminal nodes will just explore their children recursively.
/// Override any method definition to change that behavior for any given node in the AST.
pub trait Visitor {
    /// Visitor output for expressions.
    ///
    /// The output must have a monoidal structure, ie we can add two together
    /// to produce a new one, and have a default one. It must also be interact accordingly
    /// with `SOutput`. This is witnessed by the `Default` and `Extend` trait requirements.
    type EOutput: Extend<Self::SOutput> + Into<Self::SOutput>;
    /// Visitor output for statements.
    ///
    /// The output must have a monoidal structure, ie we can add two together
    /// to produce a new one, and have a default one. This is witnessed by the
    /// `Default` and `Extend` trait requirements.
    type SOutput: Default + Extend<Self::SOutput>;

    /// Visits a block.
    fn visit_block(&mut self, b: &Block) -> Self::EOutput {
        let mut o = self.visit_expr(&b.ret);
        o.extend(b.stmts.iter().map(|s| self.visit_stmt(s)));
        o
    }

    /// Visits an expression.
    fn visit_expr(&mut self, e: &Expr) -> Self::EOutput;

    /// Visits a function.
    fn visit_fun(&mut self, f: &Fun) -> Self::SOutput {
        self.visit_block(&f.body).into()
    }

    /// Visits a program.
    fn visit_program(&mut self, p: &Program) -> Self::SOutput {
        let mut o = Self::SOutput::default();
        o.extend(p.iter().map(|f| self.visit_fun(f)));
        o
    }

    /// Visits a statement.
    fn visit_stmt(&mut self, s: &Stmt) -> Self::SOutput;
}
