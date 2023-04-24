//! Typing.

use std::collections::HashMap;

use crate::ast::fun::binop_int_sig;
use crate::ast::{Block, Expr, Fun, FunSig, Program, Stmt, Ty, Var, VarDef, Visitor};
use Ty::*;

/// A typing environment.
///
/// Contains definitions for variables and functions.
struct Env {
    /// Map between vars and their definitions.
    var_env: HashMap<Var, VarDef>,
    /// Map between function names and their definitions.
    fun_env: HashMap<String, FunSig>,
}

impl Env {
    /// Creates a new, empty environment.
    fn new() -> Self {
        Self {
            var_env: HashMap::new(),
            fun_env: HashMap::new(),
        }
    }

    /// Gets the definition of a variable.
    fn get_var(&self, v: impl AsRef<Var>) -> Option<&VarDef> {
        self.var_env.get(v.as_ref())
    }

    /// Declares a new variable in the context.
    ///
    /// # Panics
    /// Panics if the var is not stated as declared in that stratum/environment.
    /// You should only add a var definition in the stratum in which it is
    /// tied to.
    fn add_var(&mut self, vardef: impl Into<VarDef>) {
        let vardef = vardef.into();
        self.var_env.insert(vardef.name.to_owned(), vardef);
    }

    /// Gets the definition of a function.
    fn get_fun(&self, f: impl AsRef<str>) -> Option<&FunSig> {
        self.fun_env.get(f.as_ref())
    }

    /// Declares a new function in the context.
    fn add_fun(&mut self, fun_def: impl Into<FunSig>) {
        let fun_def = fun_def.into();
        self.fun_env.insert(fun_def.name.to_owned(), fun_def);
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

/// A typer that will type-check some program by
/// visiting it.
pub(crate) struct Typer {
    /// The typing environment stack.
    env: Vec<Env>,
}

impl Typer {
    /// Creates a new typer.
    pub fn new() -> Self {
        let env = Env::default();
        let mut typer = Self { env: vec![env] };

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

    /// Creates a new scope.
    fn push_scope(&mut self) {
        self.env.push(Env::default());
    }

    /// Pops and removes the current scope.
    fn pop_scope(&mut self) -> Option<()> {
        // Refuse the pop the static environment
        if self.env.len() >= 2 {
            self.env.pop().map(|_| ())
        } else {
            None
        }
    }

    /// Gets the definition of a variable.
    fn get_var(&self, v: impl AsRef<Var>) -> Option<&VarDef> {
        // Iterate over environments in reverse (last declared first processed)
        // order
        // Returns the first environment that has that variable declared
        let v = v.as_ref();
        self.env.iter().rev().find_map(|e| e.get_var(v))
    }

    /// Declares a new variable in the context.
    fn add_var(&mut self, vardef: impl Into<VarDef>) {
        let vardef = vardef.into();
        self.env.last_mut().unwrap().add_var(vardef);
    }

    /// Gets the definition of a function.
    fn get_fun(&self, f: impl AsRef<str>) -> Option<&FunSig> {
        let f = f.as_ref();
        self.env.iter().rev().find_map(|e| e.get_fun(f))
    }

    /// Declares a new function in the context.
    fn add_fun(&mut self, fun_def: impl Into<FunSig>) {
        // Functions are always inserted in the topmost scope
        self.env.last_mut().unwrap().add_fun(fun_def);
    }
}

impl Visitor for Typer {
    type EOutput = Ty;
    type SOutput = ();

    fn visit_expr(&mut self, e: &Expr) -> Ty {
        use Expr::*;
        match e {
            UnitE => UnitT,
            IntegerE(_) => IntT,
            VarE(v) => {
                let vardef = self
                    .get_var(v)
                    .unwrap_or_else(|| panic!("{v} does not exist in this context"));
                vardef.ty.clone()
            }
            CallE {
                name,
                args: args_exprs,
            } => {
                let args: Vec<Ty> = args_exprs.iter().map(|arg| self.visit_expr(arg)).collect();
                let fun = self
                    .get_fun(name)
                    .unwrap_or_else(|| panic!("Function {name} does not exist in this scope"));

                // Check the number of arguments.
                assert_eq!(
                    args.len(),
                    fun.params.len(),
                    "Expected {} arguments to function {name}, found {}",
                    fun.params.len(),
                    args.len()
                );

                // Check type of arguments.
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
        self.push_scope();
        for s in &b.stmts {
            self.visit_stmt(s);
        }
        let final_ty = self.visit_expr(&b.ret);
        self.pop_scope().unwrap();
        final_ty
    }

    fn visit_fun(&mut self, f: &Fun) {
        // Add the function signature to the context before visiting the body
        // to allow for recursion
        self.add_fun(f.signature());

        // Introduce arguments in the typing context
        for arg in &f.params {
            self.add_var(arg.clone());
        }

        let body_ty = self.visit_block(&f.body);
        assert_eq!(
            body_ty, f.ret_ty,
            "the body should return a value of type {}, got {body_ty} instead",
            f.ret_ty,
        );
    }

    fn visit_stmt(&mut self, s: &Stmt) {
        match s {
            Stmt::Declare(vardef, expr) => {
                self.add_var(vardef.clone());
                let expr_ty = self.visit_expr(expr);

                // Check the type
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

                // Check the type
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
            Stmt::ExprS(e) => self.visit_expr(e).into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_pop_option() {
        let mut typer = Typer::new();
        assert_eq!(typer.pop_scope(), None); // Cannot pop the static stratum

        typer.push_scope();
        assert!(typer.pop_scope().is_some());

        assert_eq!(typer.pop_scope(), None); // Still cannot pop it
    }
}
