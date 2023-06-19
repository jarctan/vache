//! Collecting addressing modes per span in the final version of the Typed AST.
//!
//! Used for testing purposes to check that some variables are cloned/moved
//! correctly in test programs. And for the language server protocol to provide
//! semantic highlighting accordingly.

use std::collections::HashMap;
use std::default::default;

use ExprKind::*;
use PlaceKind::*;
use Stmt::*;

use crate::tast::*;
use crate::Context;

/// Collect all addressing modes in the TAST, and returns a map between spans
/// and their addressing modes.
///
/// Note: is meant to be called _AFTER_ the loan analysis, otherwise you will
/// have uninteresting results (everything's borrowed). Calling after the
/// analysis will tell you the actual findings of which places should be cloned,
/// moved or simply borrowed.
///
/// Returns: Hashmap of (line, col) -> mode, where (line, col) is the beginning
/// of the span of the place.
pub fn farm_modes<'ctx>(
    context: &mut Context<'ctx>,
    p: &Program<'ctx>,
) -> HashMap<(usize, usize), Mode> {
    let farmer = ModeFarmer::new(context);
    farmer.visit(p)
}

/// Mode collector.
///
/// Will traverse the TAST to find places, and collect their addressing modes.
struct ModeFarmer<'a, 'ctx> {
    /// The ongoing collection.
    ///
    /// Hashmap of (line, col) -> mode, where (line, col) is the beginning
    /// of the span of the place.
    farm: HashMap<(usize, usize), Mode>,
    /// The context. Used to translate spans to actual line:col.
    context: &'a Context<'ctx>,
}

impl<'a, 'ctx> ModeFarmer<'a, 'ctx> {
    /// Creates a new mode farmer.
    pub fn new(context: &'a Context<'ctx>) -> Self {
        Self {
            farm: default(),
            context,
        }
    }

    /// Main entry to visit a program, and return the addressing modes of all
    /// the places in that program.
    pub fn visit(mut self, p: &Program<'ctx>) -> HashMap<(usize, usize), Mode> {
        self.visit_program(p);
        self.farm
    }

    /// Collects referencing modes in a program.
    fn visit_program(&mut self, p: &Program<'ctx>) {
        for f in p.funs.values() {
            self.visit_function(f);
        }
    }

    /// Collects referencing modes in a function.
    fn visit_function(&mut self, f: &Fun<'ctx>) {
        self.visit_block(&f.body);
    }

    /// Collects referencing modes in a block.
    fn visit_block(&mut self, b: &Block<'ctx>) {
        for stmt in &b.stmts {
            self.visit_stmt(stmt);
        }
        self.visit_expr(&b.ret);
    }

    /// Collects referencing modes in a statement.
    fn visit_stmt(&mut self, s: &Stmt<'ctx>) {
        match s {
            DeclareS(_, e) | AssignS(_, e) | ExprS(e) => self.visit_expr(e),
            WhileS { cond, body } => {
                self.visit_expr(cond);
                self.visit_block(body);
            }
            ForS {
                item: _,
                iter,
                body,
            } => {
                self.visit_expr(iter);
                self.visit_block(body);
            }
            HoleS => (),
            BreakS => (),
            ContinueS => (),
            ReturnS(ret) => self.visit_expr(ret),
        }
    }

    /// Collects referencing modes in an expression.
    fn visit_expr(&mut self, e: &Expr<'ctx>) {
        match &e.kind {
            UnitE | BoolE(_) | IntegerE(_) | StringE(_) => (),
            PlaceE(place) => self.visit_place(place),
            RangeE(box e1, box e2) => {
                self.visit_expr(e1);
                self.visit_expr(e2);
            }
            StructE { name: _, fields } => {
                for (_, field) in fields {
                    self.visit_expr(field);
                }
            }
            ArrayE(array) => {
                for item in array {
                    self.visit_expr(item);
                }
            }
            CallE { name: _, args } => {
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            IfE(cond, iftrue, iffalse) => {
                self.visit_expr(cond);
                self.visit_block(iftrue);
                self.visit_block(iffalse);
            }
            BlockE(b) => self.visit_block(b),
            VariantE {
                enun: _,
                variant: _,
                args,
            } => {
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            HoleE => (),
        }
    }

    /// Collects modes in a place.
    fn visit_place(&mut self, place: &Place<'ctx>) {
        // Add the place to the farm
        // If there is already an entry for the same position, keep it and discard ours,
        // so that we keep the one that matches the biggest (topmost) element.
        self.farm
            .entry(place.span.line_col(self.context.files))
            .or_insert(place.mode);
        match &place.kind {
            VarP(_) => (),
            IndexP(box a, box i) => {
                self.visit_expr(a);
                self.visit_expr(i);
            }
            FieldP(box strukt, _) => self.visit_expr(strukt),
        }
    }
}
