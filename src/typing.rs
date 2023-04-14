//! Typing.

use std::collections::HashMap;

use crate::ast::fun::binop_int_sig;
use crate::ast::{Block, Expr, Fun, FunSig, Program, Stmt, Stratum, Ty, Var, VarDef, Visitor};
use Ty::*;

/// A typing environment.
///
/// A typing environment is characterized by its stratum identifier.
/// It contains variable and function definitions.
struct Env {
    /// Stratum id for this environment.
    stratum: Stratum,
    /// Map between vars and their definitions.
    var_env: HashMap<Var, VarDef>,
    /// Map between function names and their definitions.
    fun_env: HashMap<String, FunSig>,
}

impl Env {
    /// Creates a new, empty environment.
    fn new(stratum: Stratum) -> Self {
        Self {
            stratum,
            var_env: HashMap::new(),
            fun_env: HashMap::new(),
        }
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

impl Default for Env {
    fn default() -> Self {
        Self::new(Stratum::new())
    }
}

/// A typer that will type-check some program by
/// visiting it.
pub struct Typer {
    /// The typing environment.
    env: Vec<Env>,
}

impl Typer {
    /// Creates a new typer.
    pub fn new() -> Self {
        let mut typer = Self {
            env: vec![Env::default()],
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

    /// Defines a new stratum in the context, on top of our current strata.
    fn push_stratum(&mut self, stratum: Stratum) {
        self.env.push(Env::new(stratum));
    }

    /// Removes the stratum on top of our current context strata.
    fn pop_stratum(&mut self) -> Option<Stratum> {
        self.env.pop().map(|env| env.stratum)
    }

    /// Gets the definition of a variable.
    fn get_var(&self, v: impl AsRef<Var>) -> Option<&VarDef> {
        // Iterate over environments in reverse (last declared first processed)
        // order
        // Returns the first environment that has that variable declared
        let v = v.as_ref();
        self.env.iter().rev().find_map(|env| env.get_var(v))
    }

    /// Declares a new variable in the context.
    fn add_var(&mut self, vardef: impl Into<VarDef>) {
        // Variable is declared in the current environment, ie the last in our list
        self.env
            .last_mut()
            .expect("No environment to insert variable in")
            .add_var(vardef);
    }

    /// Gets the definition of a function.
    fn get_fun(&self, f: impl AsRef<str>) -> Option<&FunSig> {
        let f = f.as_ref();
        self.env.iter().rev().find_map(|env| env.get_fun(f))
    }

    /// Declares a new function in the context.
    fn add_fun(&mut self, fundef: impl Into<FunSig>) {
        self.env
            .last_mut()
            .expect("No environment to insert variable in")
            .add_fun(fundef);
    }
}

impl Visitor for Typer {
    type Output = Ty;

    fn visit_expr(&mut self, e: &Expr) -> Ty {
        use Expr::*;
        match e {
            UnitE => UnitT,
            IntegerE(_) => IntT,
            VarE(v) => self
                .get_var(v)
                .unwrap_or_else(|| panic!("{v} does not exist in this context"))
                .ty
                .clone(),
            CallE { name, args } => {
                let args: Vec<Ty> = args.iter().map(|arg| self.visit_expr(arg)).collect();
                let fun = self
                    .get_fun(name)
                    .unwrap_or_else(|| panic!("Function {name} does not exist in this scope"));
                assert_eq!(
                    args.len(),
                    fun.params.len(),
                    "Expected {} arguments to function {name}, found {}",
                    fun.params.len(),
                    args.len()
                );
                for (i, (arg_ty, VarDef { ty: param_ty, .. })) in
                    args.iter().zip(fun.params.iter()).enumerate()
                {
                    assert_eq!(
                        arg_ty, param_ty,
                        "type of {i}th argument should be {param_ty}, not {arg_ty}"
                    );
                }
                fun.ret_ty.clone()
            }
            IfE(box cond, box iftrue, box iffalse) => {
                let cond_ty = self.visit_expr(cond);
                let iftrue_ty = self.visit_block(iftrue);
                let iffalse_ty = self.visit_block(iffalse);
                assert_eq!(
                    cond_ty, BoolT,
                    "condition {cond:?} should compute to a boolean value"
                );
                assert_eq!(
                    iftrue_ty, iffalse_ty,
                    "if and else branches should have the same type"
                );
                iftrue_ty
            }
            BlockE(e) => self.visit_block(e),
        }
    }

    fn visit_block(&mut self, b: &Block) -> Ty {
        self.push_stratum(b.stratum);
        for s in &b.stmts {
            self.visit_stmt(s);
        }
        let final_ty = self.visit_expr(&b.ret);
        assert_eq!(self.pop_stratum(), Some(b.stratum));
        final_ty
    }

    fn visit_fun(&mut self, f: &Fun) -> Ty {
        for arg in &f.params {
            self.add_var(arg.clone());
        }

        // Add the function signature to the context before visiting the body
        // to permit recursion
        self.add_fun(f.signature());

        let body_ty = self.visit_block(&f.body);
        assert_eq!(
            body_ty, f.ret_ty,
            "the body should return a value of type {}, got {body_ty} instead",
            f.ret_ty
        );
        UnitT
    }

    fn visit_stmt(&mut self, s: &Stmt) -> Ty {
        match s {
            Stmt::Declare(vardef, expr) => {
                self.add_var(vardef.clone());
                let expr_ty = self.visit_expr(expr);
                assert_eq!(
                    vardef.ty, expr_ty,
                    "expression type ({expr_ty}) of {expr:?} should match type annotation ({})",
                    vardef.ty
                );
            }
            Stmt::Assign(var, expr) => {
                let expr_ty = self.visit_expr(expr);
                let vardef = self
                    .get_var(var)
                    .unwrap_or_else(|| panic!("Assigning to an undeclared variable {var}"));
                assert_eq!(vardef.ty, expr_ty, "expression type ({expr_ty}) of {expr:?} should match the type of variable {var} ({})", vardef.ty);
            }
            Stmt::While { cond, body } => {
                let cond_ty = self.visit_expr(cond);
                assert_eq!(
                    cond_ty, BoolT,
                    "condition {cond:?} should compute to a boolean value"
                );
                self.visit_block(body);
            }
        }
        UnitT
    }
}
