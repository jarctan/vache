//! Typing.

use std::collections::HashMap;

use crate::ast::fun::{binop_int_sig, Subst};
use crate::ast::{
    Block, Expr, Fun, GenericFunSig, Program, Stmt, Stratum, Ty, TyAndStratum, Var, VarDef, Visitor,
};
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
    fun_env: HashMap<String, GenericFunSig>,
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
    ///
    /// # Panics
    /// Panics if the var is not stated as declared in that stratum/environment.
    /// You should only add a var definition in the stratum in which it is
    /// tied to.
    fn add_var(&mut self, vardef: impl Into<VarDef>) {
        let vardef = vardef.into();
        assert!(vardef.stratum == self.stratum);
        self.var_env.insert(vardef.name.to_owned(), vardef);
    }

    /// Gets the definition of a function.
    fn get_fun(&self, f: impl AsRef<str>) -> Option<&GenericFunSig> {
        self.fun_env.get(f.as_ref())
    }

    /// Declares a new function in the context.
    fn add_fun(&mut self, fun_def: impl Into<GenericFunSig>) {
        let fun_def = fun_def.into();
        self.fun_env.insert(fun_def.sig.name.to_owned(), fun_def);
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new(Stratum::new_concrete())
    }
}

/// A typer that will type-check some program by
/// visiting it.
pub(crate) struct Typer {
    /// The typing stack.
    ///
    /// The typing stack is the ordered list of stratums.
    ///
    /// Invariant: `set{self.stack}==self.env.keys()`
    stack: Vec<Stratum>,
    /// The typing environment.
    env: HashMap<Stratum, Env>,
}

impl Typer {
    /// Creates a new typer.
    pub fn new() -> Self {
        let env = Env::default();
        let mut typer = Self {
            stack: vec![env.stratum],
            env: std::iter::once((env.stratum, env)).collect(),
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

    /// Defines a new stratum in the context, on top of our current strata context.
    fn push_stratum(&mut self, stratum: Stratum) {
        self.stack.push(stratum);
        self.env.insert(stratum, Env::new(stratum));
    }

    /// Removes the stratum on top of our current strata context.
    ///
    /// Note: you can't remove the default, static stratum.
    fn pop_stratum(&mut self) -> Option<Stratum> {
        // Refuse the pop if only one stratum left (which must be the static stratum then)
        if self.stack.len() >= 2 {
            let s = self.stack.pop()?;
            self.env.remove(&s);
            Some(s)
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
        self.stack.iter().rev().find_map(|s| self.env[s].get_var(v))
    }

    /// Declares a new variable in the context.
    fn add_var(&mut self, vardef: impl Into<VarDef>) {
        let vardef = vardef.into();
        // Variable is declared in the scope it belongs to
        self.env
            .get_mut(&vardef.stratum)
            .unwrap_or_else(|| {
                panic!(
                    "Adding variable `{}` to undefined stratum {}",
                    vardef.name, vardef.stratum
                )
            })
            .add_var(vardef);
    }

    /// Gets the definition of a function.
    fn get_fun(&self, f: impl AsRef<str>) -> Option<&GenericFunSig> {
        let f = f.as_ref();
        self.stack.iter().rev().find_map(|s| self.env[s].get_fun(f))
    }

    /// Declares a new function in the context.
    fn add_fun(&mut self, fun_def: impl Into<GenericFunSig>) {
        // Functions are always inserted in the topmost scope
        self.env
            .get_mut(
                self.stack
                    .last()
                    .expect("No environment to insert variable in"),
            )
            .unwrap()
            .add_fun(fun_def);
    }

    /// Returns a static stratum.
    pub fn static_stratum(&self) -> Stratum {
        self.stack[0]
    }

    /// Is the stratum `s1` alive for at least the stratum `s2`
    fn is_stratum_included(&self, s1: Stratum, s2: Stratum) -> bool {
        s2 == self.static_stratum() || s1 == s2 || {
            let mut found_s1 = false;
            for s in self.stack.iter().rev() {
                let env = &self.env[s];
                found_s1 |= env.stratum == s1;
                if env.stratum == s2 {
                    return found_s1;
                }
            }
            false
        }
    }

    /// For debugging purposes only: get back the stratum list.
    ///
    /// TODO: remove or revamp this function.
    fn stratum_list(&self) -> Vec<Stratum> {
        self.stack.to_vec()
    }
}

impl Visitor for Typer {
    type EOutput = TyAndStratum;
    type SOutput = ();

    fn visit_expr(&mut self, e: &Expr) -> TyAndStratum {
        use Expr::*;
        match e {
            UnitE => TyAndStratum {
                ty: UnitT,
                stm: self.static_stratum(),
            },
            IntegerE(_) => TyAndStratum {
                ty: IntT,
                stm: self.static_stratum(),
            },
            VarE(v) => {
                let vardef = self
                    .get_var(v)
                    .unwrap_or_else(|| panic!("{v} does not exist in this context"));
                TyAndStratum {
                    ty: vardef.ty.clone(),
                    stm: vardef.stratum,
                }
            }
            CallE {
                name,
                strata,
                ret_stm,
                args: args_exprs,
            } => {
                let args: Vec<TyAndStratum> =
                    args_exprs.iter().map(|arg| self.visit_expr(arg)).collect();
                let fun = self
                    .get_fun(name)
                    .unwrap_or_else(|| panic!("Function {name} does not exist in this scope"));

                // Check number of arguments and stratum instances.
                assert_eq!(
                    args.len(),
                    fun.sig.params.len(),
                    "Expected {} arguments to function {name}, found {}",
                    fun.sig.params.len(),
                    args.len()
                );
                assert_eq!(
                    strata.len(),
                    fun.quantifiers.len(),
                    "Expected {} stratum instances to function {name}, found {}",
                    fun.quantifiers.len(),
                    strata.len()
                );

                // Substitute quantifiers in the generic function signature with our strata,
                // to get an instanced version of the function signature.
                let mut subst: Subst = strata
                    .iter()
                    .copied()
                    .zip(fun.quantifiers.iter().copied())
                    .map(|(b, a)| (a, b))
                    .collect();

                // Substitute final stratum parameter if the return stratum is quantified
                if let Stratum::Abstract(var) = fun.sig.ret_ty.stm {
                    subst.insert(var, *ret_stm);
                }

                // Retrieve the instanced version of the function signature.
                let fun = fun.sig.clone().subst(subst);

                // Check arguments.
                for (
                    i,
                    (
                        TyAndStratum {
                            ty: arg_ty,
                            stm: arg_stm,
                        },
                        VarDef {
                            ty: param_ty,
                            stratum: param_stm,
                            ..
                        },
                    ),
                ) in args.into_iter().zip(fun.params.into_iter()).enumerate()
                {
                    assert_eq!(
                        arg_ty, param_ty,
                        "type of {i}th argument should be {param_ty}, not {arg_ty}"
                    );
                    assert!(self.is_stratum_included(param_stm, arg_stm), "{i}th argument {:?} does not live long enough (found {arg_stm}, expected {param_stm})", args_exprs[i]);
                }

                // Check that the return stratum is enough for what the caller needs
                if let (Stratum::Concrete(_), &Stratum::Concrete(_)) = (fun.ret_ty.stm, ret_stm) {
                    assert!(
                        self.is_stratum_included(*ret_stm, fun.ret_ty.stm),
                        "return value does not live long enough (found {}, expected {ret_stm})",
                        fun.ret_ty.stm
                    );
                }

                // If so, return the stratum the caller requested
                TyAndStratum {
                    ty: fun.ret_ty.ty,
                    stm: *ret_stm,
                }
            }
            IfE(box cond, box iftrue, box iffalse) => {
                let cond_ty = self.visit_expr(cond);
                let iftrue_ty = self.visit_block(iftrue);
                let iffalse_ty = self.visit_block(iffalse);
                assert_eq!(
                    cond_ty.ty, BoolT,
                    "condition {cond:?} should compute to a boolean value"
                );
                assert_eq!(
                    iftrue_ty.ty, iffalse_ty.ty,
                    "if and else branches should have the same type"
                );
                iftrue_ty
            }
            BlockE(e) => self.visit_block(e),
        }
    }

    fn visit_block(&mut self, b: &Block) -> TyAndStratum {
        self.push_stratum(b.stratum);
        for s in &b.stmts {
            self.visit_stmt(s);
        }
        let final_ty = self.visit_expr(&b.ret);
        assert_eq!(self.pop_stratum(), Some(b.stratum));
        final_ty
    }

    fn visit_fun(&mut self, f: &Fun) {
        // Add the function signature to the context before visiting the body
        // to allow for recursion
        self.add_fun(f.signature());

        // Introduce the generic, abstract stratums
        for &q in &f.quantifiers {
            self.push_stratum(q.into());
        }

        // Introduce arguments in the typing context
        for arg in &f.params {
            self.add_var(arg.clone());
        }

        let body_ty = self.visit_block(&f.body).ty;
        assert_eq!(
            body_ty, f.ret_ty.ty,
            "the body should return a value of type {}, got {body_ty} instead",
            f.ret_ty.ty,
        );

        // Remove the abstract stratums at the end
        for _ in &f.quantifiers {
            self.pop_stratum();
        }
    }

    fn visit_stmt(&mut self, s: &Stmt) {
        match s {
            Stmt::Declare(vardef, expr) => {
                self.add_var(vardef.clone());
                let (expr_ty, expr_stm) = self.visit_expr(expr).into();

                // Check the type
                assert_eq!(
                    vardef.ty, expr_ty,
                    "expression type ({expr_ty}) of {expr:?} should match type annotation ({})",
                    vardef.ty
                );

                // check the stratum
                assert!(self.is_stratum_included(vardef.stratum, expr_stm), "your expression {expr:?} does not live long enough (found {expr_stm}, expected {} because of {}) {:?}", vardef.stratum, vardef.name, self.stratum_list());
            }
            Stmt::Assign(var, expr) => {
                let (expr_ty, expr_stm) = self.visit_expr(expr).into();
                let vardef = self
                    .get_var(var)
                    .unwrap_or_else(|| panic!("Assigning to an undeclared variable {var}"));

                // Check the type
                assert_eq!(vardef.ty, expr_ty, "expression type ({expr_ty}) of {expr:?} should match the type of variable {var} ({})", vardef.ty);

                // Check the stratum
                assert!(self.is_stratum_included(vardef.stratum, expr_stm), "your expression {expr:?} does not live long enough (found {expr_stm}, expected {} because of {}) {:?}", vardef.stratum, vardef.name, self.stratum_list());
            }
            Stmt::While { cond, body } => {
                let cond_ty = self.visit_expr(cond).ty;
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
        assert_eq!(typer.pop_stratum(), None); // Cannot pop the static stratum

        let s = Stratum::new_concrete();
        typer.push_stratum(s);
        assert_eq!(typer.pop_stratum(), Some(s));

        assert_eq!(typer.pop_stratum(), None); // Still cannot pop it
    }

    #[test]
    fn push_pop_order() {
        let mut typer = Typer::new();

        // Create two stratums, pop them
        // and check they are returned in correct order
        let s = Stratum::new_concrete();
        let s2 = Stratum::new_concrete();
        typer.push_stratum(s);
        typer.push_stratum(s2);
        assert_eq!(typer.pop_stratum(), Some(s2));
        assert_eq!(typer.pop_stratum(), Some(s));
    }

    #[test]
    fn always_in_static() {
        let mut typer = Typer::new();

        // Create a new stratum, push and check
        let s = Stratum::new_concrete();
        typer.push_stratum(s);
        assert!(
            typer.is_stratum_included(s, typer.static_stratum()),
            "static should always contain all lifetimes"
        );

        // Create a new stratum but don't push it
        let s2 = Stratum::new_concrete();
        assert!(
            typer.is_stratum_included(s2, typer.static_stratum()),
            "static should always contain all lifetimes"
        );
    }

    #[test]
    fn stratum_refl() {
        let typer = Typer::new();
        let s = Stratum::new_concrete();
        // Note: we did not even push s to the typer
        assert!(typer.is_stratum_included(s, s));
    }
}
