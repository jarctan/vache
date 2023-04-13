//! Typing.

use std::collections::HashMap;

use crate::ast::{Visitor, Program, Var, VarDef, Expr, Block, Fun, Stmt, Ty};
use Ty::*;

/// A typer that will type-check some program by
/// visiting it.
pub struct Typer {
    /// Map between vars and their definitions.
    vardefs: HashMap<Var, VarDef>,
}

impl Typer {
    /// Creates a new typer.
    pub fn new() -> Self {
        Self {
            vardefs: HashMap::new(),
        }
    }

    /// Type-checks a piece of code.
    pub fn check(&mut self, p: &Program) {
        self.visit_program(p);
    }

    /// Gets the definition of a variable.
    fn get_var(&self, v: impl AsRef<Var>) -> Option<&VarDef> {
        self.vardefs.get(v.as_ref())
    }

    /// Declares a new variable in the context.
    fn add_var(&mut self, vardef: impl Into<VarDef>) {
        let vardef = vardef.into();
        self.vardefs.insert(vardef.name.to_owned(), vardef);
    }
}

impl Visitor for Typer {
    type Output = Ty;

    fn visit_expr(&mut self, e: &Expr) -> Ty {
        match e {
            Expr::Unit => UnitT,
            Expr::Integer(_) => IntT,
            Expr::Var(v) => self.get_var(v).unwrap_or_else(|| panic!("{v} does not exist in this context")).ty.clone(),
            Expr::Call { name, args } => todo!(),
            Expr::If(box cond, box iftrue, box iffalse) => {
                let cond_ty = self.visit_expr(cond);
                let iftrue_ty = self.visit_block(iftrue);
                let iffalse_ty = self.visit_block(iffalse);
                assert!(cond_ty == BoolT, "if and else branches should have the same type");
                assert!(iftrue_ty == iffalse_ty, "if and else branches should have the same type");
                iftrue_ty
            }
        }
    }

    fn visit_block(&mut self, b: &Block) -> Ty {
        for s in &b.stmts {
            self.visit_stmt(s);
        }
        self.visit_expr(&b.ret)
    }

    fn visit_fun(&mut self, f: &Fun) -> Ty {
        for arg in &f.args {
            self.add_var(arg.clone());
        }
        let body_ty = self.visit_block(&f.body);
        UnitT
    }

    fn visit_stmt(&mut self, s: &Stmt) -> Ty {
        match s {
            Stmt::Declare(vardef, expr) => {
                self.add_var(vardef.clone());
                let expr_ty = self.visit_expr(expr);
                assert!(vardef.ty == expr_ty, "expression type ({}) does not match type annotation ({})", expr_ty, vardef.ty);
            },
            Stmt::Assign(_, _) => todo!(),
            Stmt::While { cond, block } => todo!(),
        }
        UnitT
    }
}