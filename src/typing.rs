//! Typing.

use std::collections::HashMap;

use crate::ast::fun::binop_int_sig;
use crate::ast::SelfVisitor;
use crate::ast::{self, boxed};
use crate::tast::*;
use unzip3::Unzip3;
use Expr::*;
use Ty::*;

/// A typing environment.
///
/// Contains definitions for variables and functions.
struct Env {
    /// Map between vars and their definitions.
    var_env: HashMap<ast::Var, ast::VarDef>,
    /// Map between function names and their definitions.
    fun_env: HashMap<String, ast::FunSig>,
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
    fn get_var(&self, v: impl AsRef<ast::Var>) -> Option<&ast::VarDef> {
        self.var_env.get(v.as_ref())
    }

    /// Declares a new variable in the context.
    ///
    /// # Panics
    /// Panics if the var is not stated as declared in that stratum/environment.
    /// You should only add a var definition in the stratum in which it is
    /// tied to.
    fn add_var(&mut self, vardef: impl Into<ast::VarDef>) {
        let vardef = vardef.into();
        self.var_env.insert(vardef.name.to_owned(), vardef);
    }

    /// Gets the definition of a function.
    fn get_fun(&self, f: impl AsRef<str>) -> Option<&ast::FunSig> {
        self.fun_env.get(f.as_ref())
    }

    /// Declares a new function in the context.
    fn add_fun(&mut self, fun_def: impl Into<ast::FunSig>) {
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
    pub fn check(&mut self, p: ast::Program) -> Program {
        self.visit_program(p)
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
    ///
    /// It will return a reference into that definition, and the id of the stratum
    /// in which the variables resides.
    fn get_var(&self, v: impl AsRef<ast::Var>) -> Option<(&ast::VarDef, Stratum)> {
        // Iterate over environments in reverse (last declared first processed)
        // order
        // Returns the first environment that has that variable declared
        let v = v.as_ref();
        self.env
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, e)| e.get_var(v).map(|x| (x, i)))
    }

    /// Declares a new variable in the context.
    fn add_var(&mut self, vardef: impl Into<ast::VarDef>) {
        let vardef = vardef.into();
        self.env.last_mut().unwrap().add_var(vardef);
    }

    /// Gets the definition of a function.
    fn get_fun(&self, f: impl AsRef<str>) -> Option<&ast::FunSig> {
        let f = f.as_ref();
        self.env.iter().rev().find_map(|e| e.get_fun(f))
    }

    /// Declares a new function in the context.
    fn add_fun(&mut self, fun_def: impl Into<ast::FunSig>) {
        // Functions are always inserted in the topmost scope
        self.env.last_mut().unwrap().add_fun(fun_def);
    }

    /// Returns the current stratum/scope id.
    fn current_stratum(&self) -> usize {
        self.env.len() - 1
    }
}

/// Stratum/scope identifier.
type Stratum = usize;

impl SelfVisitor for Typer {
    type EOutput = (Expr, Ty, Stratum);
    type SOutput = Stmt;
    type BOutput = (Block, Ty, Stratum);
    type FOutput = Fun;
    type POutput = Program;

    fn visit_expr(&mut self, e: ast::Expr) -> (Expr, Ty, Stratum) {
        use Expr::*;
        match e {
            ast::Expr::UnitE => (UnitE, UnitT, self.current_stratum()),
            ast::Expr::IntegerE(i) => (IntegerE(i), IntT, self.current_stratum()),
            ast::Expr::StringE(s) => (StringE(s), StrT, self.current_stratum()),
            ast::Expr::VarE(v) => {
                let (vardef, stm) = self
                    .get_var(&v)
                    .unwrap_or_else(|| panic!("{v} does not exist in this context"));
                (VarE(vardef.clone()), vardef.ty.clone(), stm)
            }
            // Make a special case for `print` until we get generic functions so that we
            // can express `print` more elegantly with the other builtin functions.
            ast::Expr::CallE { name, args } if name == "print" => {
                let (args, _, _): (Vec<Expr>, Vec<Ty>, Vec<Stratum>) =
                    args.into_iter().map(|arg| self.visit_expr(arg)).unzip3();
                (CallE { name, args }, UnitT, self.current_stratum())
            }
            ast::Expr::CallE { name, args } => {
                let (args, args_ty, _): (Vec<Expr>, Vec<Ty>, Vec<Stratum>) =
                    args.into_iter().map(|arg| self.visit_expr(arg)).unzip3();
                let fun = self
                    .get_fun(&name)
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
                    args_ty.iter().zip(fun.params.iter()).enumerate()
                {
                    assert_eq!(
                        arg_ty, param_ty,
                        "type of {i}th argument should be {param_ty}, not {arg_ty}"
                    );
                }

                (
                    CallE { name, args },
                    fun.ret_ty.clone(),
                    self.current_stratum(),
                )
            }
            ast::Expr::IfE(box cond, box iftrue, box iffalse) => {
                let (cond, cond_ty, _) = self.visit_expr(cond);
                let (iftrue, iftrue_ty, true_stm) = self.visit_block(iftrue);
                let (iffalse, iffalse_ty, false_stm) = self.visit_block(iffalse);
                assert_eq!(
                    cond_ty, BoolT,
                    "condition {cond:?} should compute to a boolean value"
                );
                assert_eq!(
                    iftrue_ty, iffalse_ty,
                    "if and else branches should have the same type"
                );
                (
                    IfE(boxed(cond), boxed(iftrue), boxed(iffalse)),
                    iftrue_ty,
                    std::cmp::max(true_stm, false_stm),
                )
            }
            ast::Expr::BlockE(box e) => {
                let (b, ty, stm) = self.visit_block(e);
                (BlockE(boxed(b)), ty, stm)
            }
        }
    }

    fn visit_block(&mut self, b: ast::Block) -> (Block, Ty, Stratum) {
        self.push_scope();
        let stmts = b.stmts.into_iter().map(|s| self.visit_stmt(s)).collect();
        let (ret, final_ty, stm) = self.visit_expr(b.ret);
        self.pop_scope().unwrap();
        (Block { stmts, ret }, final_ty, stm)
    }

    fn visit_fun(&mut self, f: ast::Fun) -> Fun {
        // Add the function signature to the context before visiting the body
        // to allow for recursion
        self.add_fun(f.signature());

        // Introduce arguments in the typing context
        for arg in &f.params {
            self.add_var(arg.clone());
        }

        let (body, body_ty, _) = self.visit_block(f.body);
        assert_eq!(
            body_ty, f.ret_ty,
            "the body should return a value of type {}, got {body_ty} instead",
            f.ret_ty,
        );

        Fun {
            name: f.name,
            params: f.params,
            ret_ty: f.ret_ty,
            body,
        }
    }

    fn visit_stmt(&mut self, s: ast::Stmt) -> Stmt {
        use Stmt::*;
        match s {
            ast::Stmt::Declare(vardef, expr) => {
                self.add_var(vardef.clone());
                let (expr, expr_ty, _) = self.visit_expr(expr);

                // Check the type
                assert_eq!(
                    vardef.ty, expr_ty,
                    "expression type ({expr_ty}) of {expr:?} should match type annotation ({})",
                    vardef.ty
                );

                Declare(vardef, expr)
            }
            ast::Stmt::Assign(var, expr) => {
                let (expr, expr_ty, from_stm) = self.visit_expr(expr);
                let (vardef, to_stm) = self
                    .get_var(&var)
                    .unwrap_or_else(|| panic!("Assigning to an undeclared variable {var}"));

                // Check the type
                assert_eq!(vardef.ty, expr_ty, "expression type ({expr_ty}) of {expr:?} should match the type of variable {var} ({})", vardef.ty);
                Assign(
                    VarDef {
                        name: var,
                        ty: vardef.ty.clone(),
                    },
                    if from_stm <= to_stm {
                        expr
                    } else {
                        OwnE(boxed(expr))
                    },
                )
            }
            ast::Stmt::While { cond, body } => {
                let (cond, cond_ty, _) = self.visit_expr(cond);
                assert_eq!(
                    cond_ty, BoolT,
                    "condition {cond:?} should compute to a boolean value"
                );
                let (body, body_ty, _) = self.visit_block(body);
                assert_eq!(
                    body_ty, UnitT,
                    "body of expression should not return anything"
                );
                While { cond, body }
            }
            ast::Stmt::ExprS(e) => ExprS(self.visit_expr(e).0),
        }
    }

    fn visit_program(&mut self, p: ast::Program) -> Program {
        p.into_iter().map(|f| self.visit_fun(f)).collect()
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
