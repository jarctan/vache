//! Typing.

use std::collections::HashMap;

use crate::ast::{Visitor, Program, Var, VarDef, Expr, Block, Fun, Stmt, Ty, fun::{FunSig, binop_int_sig}};
use Ty::*;

/// A typer that will type-check some program by
/// visiting it.
pub struct Typer {
    /// Map between vars and their definitions.
    var_env: HashMap<Var, VarDef>,
    /// Map between function names and their definitions.
    fun_env: HashMap<String, FunSig>,
}

impl Typer {
    /// Creates a new typer.
    pub fn new() -> Self {
        let mut typer = Self {
            var_env: HashMap::new(),
            fun_env: HashMap::new(),
        };

        // Add builtin function signatures.
        typer.add_fun(binop_int_sig("+", IntT));
        typer.add_fun(binop_int_sig("-", IntT));
        typer.add_fun(binop_int_sig("*", IntT));
        typer.add_fun(binop_int_sig("/", IntT));
        typer.add_fun(binop_int_sig("%", IntT));
        typer.add_fun(binop_int_sig("==", BoolT));
        typer.add_fun(binop_int_sig("<=", BoolT));
        typer.add_fun(binop_int_sig("<", BoolT));
        typer.add_fun(binop_int_sig(">=", BoolT));
        typer.add_fun(binop_int_sig(">", BoolT));
        typer.add_fun(binop_int_sig("!=", BoolT));

        typer
    }

    /// Type-checks a piece of code.
    pub fn check(&mut self, p: &Program) {
        self.visit_program(p);
    }

    /// Gets the definition of a variable.
    fn get_var(&self, v: impl AsRef<Var>) -> Option<&VarDef> {
        self.var_env.get(v.as_ref())
    }

    /// Declares a new variable in the context.
    fn add_var(&mut self, vardef: impl Into<VarDef>) {
        let vardef = vardef.into();
        self.var_env.insert(vardef.name.to_owned(), vardef);
    }

    /// Gets the definition of a function.
    fn get_fun(&self, f: impl AsRef<str>) -> Option<&FunSig> {
        self.fun_env.get(f.as_ref())
    }

    /// Declares a new function in the context.
    fn add_fun(&mut self, fundef: impl Into<FunSig>) {
        let fundef = fundef.into();
        self.fun_env.insert(fundef.name.to_owned(), fundef);
    }
}

impl Visitor for Typer {
    type Output = Ty;

    fn visit_expr(&mut self, e: &Expr) -> Ty {
        match e {
            Expr::Unit => UnitT,
            Expr::Integer(_) => IntT,
            Expr::Var(v) => self.get_var(v).unwrap_or_else(|| panic!("{v} does not exist in this context")).ty.clone(),
            Expr::Call { name, args } => {
                let fun = self.get_fun(name).unwrap_or_else(|| panic!("Function {name} does not exist in this scope"));
                fun.ret_ty.clone()
            }
            Expr::If(box cond, box iftrue, box iffalse) => {
                let cond_ty = self.visit_expr(cond);
                let iftrue_ty = self.visit_block(iftrue);
                let iffalse_ty = self.visit_block(iffalse);
                assert_eq!(cond_ty, BoolT, "condition {cond:?} should compute to a boolean value");
                assert_eq!(iftrue_ty, iffalse_ty, "if and else branches should have the same type");
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

        // Add the function signature to the context before visiting the body
        // to permit recursion
        self.add_fun(f.signature());

        let body_ty = self.visit_block(&f.body);
        assert_eq!(body_ty, f.ret_ty, "the body should return a value of type {}, got {body_ty} instead", f.ret_ty);
        UnitT
    }

    fn visit_stmt(&mut self, s: &Stmt) -> Ty {
        match s {
            Stmt::Declare(vardef, expr) => {
                self.add_var(vardef.clone());
                let expr_ty = self.visit_expr(expr);
                assert_eq!(vardef.ty, expr_ty, "expression type ({expr_ty}) of {expr:?} should match type annotation ({})", vardef.ty);
            },
            Stmt::Assign(_, _) => todo!(),
            Stmt::While { cond, block } => todo!(),
        }
        UnitT
    }
}