//! Visiting the AST, consuming it at the same time.

use super::{Block, Expr, Fun, Program, Stmt, Struct};

/// Visitor of the AST.
pub trait SelfVisitor<'ctx> {
    /// Expression output.
    type EOutput;
    /// Statement output.
    type SOutput;
    /// Block output.
    type BOutput;
    /// Function output.
    type FOutput;
    /// Structure output.
    type TOutput;
    /// Program output.
    type POutput;

    /// Visits a block.
    fn visit_block(&mut self, b: Block<'ctx>) -> Self::BOutput;
    /// Visits an expression.
    fn visit_expr(&mut self, e: Expr<'ctx>) -> Self::EOutput;

    /// Visits a function.
    fn visit_fun(&mut self, f: Fun<'ctx>) -> Self::FOutput;

    /// Visits a program.
    fn visit_program(&mut self, p: Program<'ctx>) -> Self::POutput;

    /// Visits a structure.
    fn visit_struct(&mut self, s: Struct<'ctx>) -> Self::TOutput;

    /// Visits a statement.
    fn visit_stmt(&mut self, s: Stmt<'ctx>) -> Self::SOutput;
}
