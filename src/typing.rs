//! Typing.

use std::collections::{HashMap, HashSet};

use unzip3::Unzip3;
use Expr::*;
use Ty::*;

use crate::ast;
use crate::ast::fun::binop_int_sig;
use crate::ast::SelfVisitor;
use crate::tast::*;
use crate::utils::{boxed, keys_match};

/// A typing environment.
///
/// Contains definitions for variables and functions.
struct Env {
    /// Map between vars and their definitions.
    var_env: HashMap<ast::Var, ast::VarDef>,
}

impl Env {
    /// Creates a new, empty environment.
    fn new() -> Self {
        Self {
            var_env: HashMap::new(),
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
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

/// A typer that will type-check some program by
/// visiting it.
pub(crate) struct Typer {
    /// Map between function names and their definitions.
    fun_env: HashMap<String, ast::FunSig>,
    /// Map between function names and their definitions.
    struct_env: HashMap<String, Struct>,
    /// Set of valid type names. Initialized at the very beginning,
    /// allows to have mutually-referencing structures.
    valid_type_names: HashSet<String>,
    /// The typing environment stack.
    env: Vec<Env>,
}

impl Typer {
    /// Creates a new typer.
    pub fn new() -> Self {
        let mut typer = Self {
            fun_env: HashMap::new(),
            struct_env: HashMap::new(),
            valid_type_names: HashSet::new(),
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
    /// It will return a reference into that definition, and the id of the
    /// stratum in which the variables resides.
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
        self.fun_env.get(f.as_ref())
    }

    /// Declares a new function in the context.
    fn add_fun(&mut self, fun_def: impl Into<ast::FunSig>) {
        let fun_def = fun_def.into();
        self.fun_env.insert(fun_def.name.to_owned(), fun_def);
    }

    /// Gets the definition of a structure.
    fn get_struct(&self, s: impl AsRef<str>) -> Option<&Struct> {
        self.struct_env.get(s.as_ref())
    }

    /// Declares a new structure in the context.
    fn add_struct(&mut self, struct_def: impl Into<Struct>) {
        let struct_def = struct_def.into();
        self.struct_env
            .insert(struct_def.name.to_owned(), struct_def);
    }

    /// Returns the current stratum/scope id.
    fn current_stratum(&self) -> usize {
        self.env.len() - 1
    }

    /// Checks that `ty` is well defined in the environment.
    ///
    /// In particular, unknown structure names will raise an error.
    fn check_ty(&mut self, ty: &Ty) {
        match ty {
            UnitT | BoolT | IntT | StrT => (),
            StructT(name) => assert!(
                self.valid_type_names.contains(name),
                "Unknown struct {name}"
            ),
        }
    }
}

/// Stratum/scope identifier.
type Stratum = usize;

impl SelfVisitor for Typer {
    type BOutput = (Block, Ty, Stratum);
    type EOutput = (Expr, Ty, Stratum);
    type FOutput = Fun;
    type POutput = Program;
    type SOutput = Stmt;
    type TOutput = Struct;

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
            ast::Expr::FieldE(box s, field) => {
                let (s, ty, stm) = self.visit_expr(s);
                if let StructT(name) = ty {
                    let strukt = self.get_struct(&name).unwrap();
                    let ty = strukt.get_field(&field).clone();
                    (FieldE(boxed(s), field), ty, stm)
                } else {
                    panic!("Cannot get a field of something which is not a struct");
                }
            }
            ast::Expr::StructE {
                name: s_name,
                fields,
            } => {
                // Compute the type of the fields
                // Because of borrowing rules, we need to do that before we immutably borrow
                // `self` through `.get_struct()` since we need a mutable borrow into `self`
                // here.
                let fields = fields
                    .into_iter()
                    .map(|(name, expr)| (name, self.visit_expr(expr)))
                    .collect();

                let strukt = self.get_struct(&s_name).unwrap();

                // Check that the instance has the same field names as the declaration
                assert!(keys_match(&strukt.fields, &fields));

                // Check that the type of each field matches the expected one
                for (fname, (_, ty, _)) in &fields {
                    let expected = strukt.get_field(fname);
                    assert_eq!(
                        expected, ty,
                        "field `{fname}` of `{s_name}` should be of type {expected}, found {ty}"
                    );
                }

                let static_stratum = 0;
                let common_stm = fields
                    .values()
                    .fold(static_stratum, |s1, &(_, _, s2)| core::cmp::max(s1, s2));
                (
                    Expr::StructE {
                        name: s_name.clone(),
                        fields: fields
                            .into_iter()
                            .map(|(name, (e, _, _))| (name, e))
                            .collect(),
                    },
                    StructT(s_name),
                    common_stm,
                )
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
        // Introduce arguments in the typing context
        for arg in &f.params {
            self.check_ty(&arg.ty);
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

                // Check type declaration.
                self.check_ty(&vardef.ty);

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
        let ast::Program { funs, structs } = p;

        // Add all function signatures to the context to allow for (mutual) recursion.
        for (name, f) in &funs {
            assert_eq!(*name, *f.name);
            self.add_fun(f.signature());
        }

        // Add all valid type names in the context, so they may be referenced
        // everywhere.
        for name in structs.keys() {
            assert!(
                self.valid_type_names.insert(name.clone()),
                "{name} is defined twice"
            );
        }

        // Note: order is important.
        // We must visit structures first.
        Program {
            structs: structs
                .into_iter()
                .map(|(name, s)| (name, self.visit_struct(s)))
                .collect(),
            funs: funs
                .into_iter()
                .map(|(name, f)| (name, self.visit_fun(f)))
                .collect(),
        }
    }

    fn visit_struct(&mut self, strukt: ast::Struct) -> Struct {
        // Check that all types in the structure exist.
        for ty in strukt.fields.values() {
            self.check_ty(ty);
        }

        // TODO: do not return Struct in this function. Nor should we return
        // Fun in `visit_fun`. We should just append them to the context and retrieve
        // them all only at the end, in one go. This would avoid this
        // disgraceful clone.
        self.add_struct(strukt.clone());

        strukt
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
