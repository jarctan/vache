//! Typing.

use std::collections::{HashMap, HashSet};
use std::default::default;

use ExprKind::*;
use PlaceKind::*;
use Ty::*;

use crate::ast::fun::binop_int_sig;
use crate::ast::SelfVisitor;
use crate::tast::*;
use crate::utils::{boxed, keys_match};
use crate::{ast, Context};

/// A typing environment.
///
/// Contains definitions for variables and functions.
struct Env<'ctx> {
    /// Map between vars and their definitions.
    var_env: HashMap<Var<'ctx>, ast::VarDef<'ctx>>,
}

impl<'ctx> Env<'ctx> {
    /// Creates a new, empty environment.
    fn new() -> Self {
        Self {
            var_env: HashMap::new(),
        }
    }

    /// Gets the definition of a variable.
    fn get_var(&self, v: impl AsRef<Var<'ctx>>) -> Option<&ast::VarDef<'ctx>> {
        self.var_env.get(v.as_ref())
    }

    /// Declares a new variable in the context.
    ///
    /// # Panics
    /// Panics if the var is not stated as declared in that stratum/environment.
    /// You should only add a var definition in the stratum in which it is
    /// tied to.
    fn add_var(&mut self, vardef: impl Into<ast::VarDef<'ctx>>) {
        let vardef = vardef.into();
        self.var_env.insert(vardef.name.to_owned(), vardef);
    }
}

impl Default for Env<'_> {
    fn default() -> Self {
        Self::new()
    }
}

/// A typer that will type-check some program by
/// visiting it.
pub(crate) struct Typer<'t, 'ctx> {
    /// Compilation context.
    ctx: &'t mut Context<'ctx>,
    /// Map between function names and their definitions.
    fun_env: HashMap<&'ctx str, ast::FunSig<'ctx>>,
    /// Map between function names and their definitions.
    struct_env: HashMap<&'ctx str, Struct<'ctx>>,
    /// Set of valid type names. Initialized at the very beginning,
    /// allows to have mutually-referencing structures.
    valid_type_names: HashSet<&'ctx str>,
    /// The typing environment stack.
    env: Vec<Env<'ctx>>,
}

impl<'t, 'ctx> Typer<'t, 'ctx> {
    /// Creates a new typer.
    pub fn new(ctx: &'t mut Context<'ctx>) -> Self {
        let mut typer = Self {
            ctx,
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
    pub fn check(&mut self, p: ast::Program<'ctx>) -> Program<'ctx> {
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
    fn get_var(&self, v: impl AsRef<Var<'ctx>>) -> Option<(&ast::VarDef<'ctx>, Stratum)> {
        // Iterate over environments in reverse (last declared first processed)
        // order
        // Returns the first environment that has that variable declared
        let v = v.as_ref();
        self.env
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, e)| e.get_var(v).map(|x| (x, i.try_into().unwrap())))
    }

    /// Declares a new variable in the context.
    fn add_var(&mut self, vardef: impl Into<ast::VarDef<'ctx>>) {
        let vardef = vardef.into();
        self.env.last_mut().unwrap().add_var(vardef);
    }

    /// Gets the definition of a function.
    fn get_fun(&self, f: impl AsRef<str>) -> Option<&ast::FunSig<'ctx>> {
        self.fun_env.get(f.as_ref())
    }

    /// Declares a new function in the context.
    fn add_fun(&mut self, fun_def: impl Into<ast::FunSig<'ctx>>) {
        let fun_def = fun_def.into();
        self.fun_env.insert(fun_def.name, fun_def);
    }

    /// Gets the definition of a structure.
    fn get_struct(&self, s: impl AsRef<str>) -> Option<&Struct<'ctx>> {
        self.struct_env.get(s.as_ref())
    }

    /// Declares a new structure in the context.
    fn add_struct(&mut self, struct_def: impl Into<Struct<'ctx>>) {
        let struct_def = struct_def.into();
        self.struct_env.insert(struct_def.name, struct_def);
    }

    /// Returns the current stratum/scope id.
    fn current_stratum(&self) -> Stratum {
        (self.env.len() - 1).try_into().unwrap()
    }

    /// Checks that `ty` is well defined in the environment.
    ///
    /// In particular, unknown structure names will raise an error.
    fn check_ty(&self, ty: &Ty) {
        match ty {
            UnitT | BoolT | IntT | StrT => (),
            ArrayT(box ty) => self.check_ty(ty),
            StructT(name) => assert!(
                self.valid_type_names.contains(name),
                "Unknown struct {name}"
            ),
        }
    }

    /// Types a place (lhs expression).
    fn visit_place(&mut self, place: ast::Place<'ctx>, mode: Mode) -> Place<'ctx> {
        match place {
            ast::Place::VarP(var) => {
                let (vardef, stm) = self
                    .get_var(var)
                    .unwrap_or_else(|| panic!("Assigning to an undeclared variable {var}"));
                Place::var(var, vardef.ty.clone(), stm, mode)
            }
            ast::Place::IndexP(box e, box ix) => {
                let e = self.visit_expr(e);
                let ix = self.visit_expr(ix);
                if let ArrayT(box item_ty) = &e.ty && let IntT = ix.ty {
                    let ty = item_ty.clone(); // Needed now because we move e after
                    let e_stm = e.stm; // Needed now because we move e after
                    Place {
                        kind: IndexP(boxed(e), boxed(ix)),
                        ty,
                        stm: e_stm,
                        mode,
                    }
                } else {
                    panic!("Only integer indexing is supported, and for arrays only");
                }
            }
            ast::Place::FieldP(_, _) => todo!(),
        }
    }
}

impl<'t, 'ctx> SelfVisitor<'ctx> for Typer<'t, 'ctx> {
    type BOutput = Block<'ctx>;
    type EOutput = Expr<'ctx>;
    type FOutput = Fun<'ctx>;
    type POutput = Program<'ctx>;
    type SOutput = Stmt<'ctx>;
    type TOutput = Struct<'ctx>;

    fn visit_expr(&mut self, e: ast::Expr<'ctx>) -> Expr<'ctx> {
        match e {
            ast::Expr::UnitE => Expr::new(UnitE, UnitT, self.current_stratum()),
            ast::Expr::IntegerE(i) => Expr::new(IntegerE(i), IntT, self.current_stratum()),
            ast::Expr::StringE(s) => Expr::new(StringE(s), StrT, self.current_stratum()),
            ast::Expr::PlaceE(place) => match place {
                ast::Place::VarP(v) => {
                    let (vardef, stm) = self
                        .get_var(v)
                        .unwrap_or_else(|| panic!("{v} does not exist in this context"));
                    Expr::new(
                        PlaceE(Place::var(vardef.name, vardef.ty.clone(), stm, default())),
                        vardef.ty.clone(),
                        stm,
                    )
                }
                ast::Place::FieldP(box s, field) => {
                    let s = self.visit_expr(s);
                    if let StructT(name) = &s.ty {
                        let strukt = self.get_struct(name).unwrap();
                        let ty = strukt.get_field(field).clone();
                        let s_stm = s.stm; // Needed now because we move s after
                        Expr::new(
                            PlaceE(Place {
                                kind: FieldP(boxed(s), field),
                                mode: default(),
                                ty: ty.clone(),
                                stm: s_stm,
                            }),
                            ty,
                            s_stm,
                        )
                    } else {
                        panic!("Cannot get a field of something which is not a struct");
                    }
                }

                ast::Place::IndexP(box e, box ix) => {
                    let e = self.visit_expr(e);
                    let ix = self.visit_expr(ix);
                    if let ArrayT(box item_ty) = &e.ty && let IntT = ix.ty {
                    let ty = item_ty.clone(); // Needed now because we move e after
                    let e_stm = e.stm; // Needed now because we move e after
                    Expr::new(PlaceE(Place {
                        kind: IndexP(boxed(e), boxed(ix)),
                        mode: default(),
                        ty: ty.clone(),
                        stm: e_stm
                    }), ty, e_stm)
                } else {
                    panic!("Only integer indexing is supported, and for arrays only");
                }
                }
            },
            // Make a special case for `print` until we get generic functions so that we
            // can express `print` more elegantly with the other builtin functions.
            ast::Expr::CallE { name, args } if name == "print" => {
                let args: Vec<Expr> = args.into_iter().map(|arg| self.visit_expr(arg)).collect();

                Expr::new(CallE { name, args }, UnitT, self.current_stratum())
            }
            ast::Expr::CallE { name, args } => {
                let args: Vec<Expr> = args.into_iter().map(|arg| self.visit_expr(arg)).collect();
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
                for (i, (Expr { ty: arg_ty, .. }, ast::VarDef { ty: param_ty, .. })) in
                    args.iter().zip(fun.params.iter()).enumerate()
                {
                    assert_eq!(
                        arg_ty, param_ty,
                        "type of {i}th argument should be {param_ty}, not {arg_ty}"
                    );
                }

                Expr::new(
                    CallE { name, args },
                    fun.ret_ty.clone(),
                    self.current_stratum(),
                )
            }
            ast::Expr::IfE(box cond, box iftrue, box iffalse) => {
                let cond = self.visit_expr(cond);
                let iftrue = self.visit_block(iftrue);
                let iffalse = self.visit_block(iffalse);
                assert_eq!(
                    cond.ty, BoolT,
                    "condition {cond:?} should compute to a boolean value"
                );
                assert_eq!(
                    iftrue.ret.ty, iffalse.ret.ty,
                    "if and else branches should have the same type"
                );

                let iftrue_stm = iftrue.ret.stm;
                let iffalse_stm = iffalse.ret.stm;
                let if_ty = iftrue.ret.ty.clone();
                Expr::new(
                    IfE(boxed(cond), boxed(iftrue), boxed(iffalse)),
                    if_ty,
                    std::cmp::max(iftrue_stm, iffalse_stm),
                )
            }
            ast::Expr::BlockE(box e) => {
                let b = self.visit_block(e);
                let ret_stm = b.ret.stm;
                let ret_ty = b.ret.ty.clone();
                Expr::new(BlockE(boxed(b)), ret_ty, ret_stm)
            }
            ast::Expr::StructE {
                name: s_name,
                fields,
            } => {
                // Compute the type of the fields
                // Because of borrowing rules, we need to do that before we immutably borrow
                // `self` through `.get_struct()` since we need a mutable borrow into `self`
                // here.
                let fields: Vec<(&str, Expr)> = fields
                    .into_iter()
                    .map(|(name, expr)| (name, self.visit_expr(expr)))
                    .collect();

                let strukt = self.get_struct(s_name).unwrap();

                // Check that the instance has the same field names as the declaration
                assert!(
                    keys_match(&strukt.fields, fields.iter().map(|(field, _)| field)),
                    "{:?} do not match {:?}",
                    fields.iter().map(|(field, _)| field).collect::<Vec<_>>(),
                    strukt.fields,
                );

                // Check that the type of each field matches the expected one
                for (fname, Expr { ty, .. }) in &fields {
                    let expected = strukt.get_field(fname);
                    assert_eq!(
                        expected, ty,
                        "field `{fname}` of `{s_name}` should be of type {expected}, found {ty}"
                    );
                }

                let common_stm = fields
                    .iter()
                    .map(|(_, field)| field)
                    .fold(Stratum::static_stm(), |s1, Expr { stm: s2, .. }| {
                        core::cmp::max(s1, *s2)
                    });
                Expr::new(
                    StructE {
                        name: s_name,
                        fields,
                    },
                    StructT(s_name),
                    common_stm,
                )
            }
            ast::Expr::ArrayE(array) => {
                assert!(!array.is_empty(), "empty arrays are not supported yet");
                // Compute the type of the items
                // Because of borrowing rules, we need to do that before we immutably borrow
                // `self` through `.get_struct()` since we need a mutable borrow into `self`
                // here.
                let array: Vec<Expr> = array
                    .into_iter()
                    .map(|expr| self.visit_expr(expr))
                    .collect();

                let common_stm = array
                    .iter()
                    .fold(Stratum::static_stm(), |s1, Expr { stm: s2, .. }| {
                        core::cmp::max(s1, *s2)
                    });
                let ty = ArrayT(boxed(array[0].ty.clone()));
                assert!(
                    array.iter().all(|item| item.ty == array[0].ty),
                    "all items in the list should have the same type"
                );
                Expr::new(ArrayE(array), ty, common_stm)
            }
        }
    }

    fn visit_block(&mut self, b: ast::Block<'ctx>) -> Block<'ctx> {
        self.push_scope();
        let stmts = b.stmts.into_iter().map(|s| self.visit_stmt(s)).collect();
        let ret = self.visit_expr(b.ret);
        self.pop_scope().unwrap();
        Block { stmts, ret }
    }

    fn visit_fun(&mut self, f: ast::Fun<'ctx>) -> Fun<'ctx> {
        let stm = self.current_stratum();
        // Introduce arguments in the typing context
        for arg in &f.params {
            self.check_ty(&arg.ty);
            self.add_var(arg.clone());
        }

        let body = self.visit_block(f.body);
        let body_ty = &body.ret.ty;
        assert_eq!(
            body_ty, &f.ret_ty,
            "the body should return a value of type {}, got {body_ty} instead",
            f.ret_ty,
        );

        Fun {
            name: f.name,
            params: f
                .params
                .into_iter()
                .map(|param| VarDef::with_stratum(param, stm))
                .collect(),
            ret_ty: f.ret_ty,
            body,
        }
    }

    fn visit_stmt(&mut self, s: ast::Stmt<'ctx>) -> Stmt<'ctx> {
        use Stmt::*;
        match s {
            ast::Stmt::Declare(vardef, expr) => {
                let stm = self.current_stratum();
                self.add_var(vardef.clone());
                let expr = self.visit_expr(expr);
                let expr_ty = &expr.ty;

                // Check type declaration.
                self.check_ty(&vardef.ty);

                // Check the type
                assert_eq!(
                    &vardef.ty, expr_ty,
                    "expression type ({expr_ty}) of {expr:?} should match type annotation ({})",
                    vardef.ty
                );

                Declare(VarDef::with_stratum(vardef, stm), expr)
            }
            ast::Stmt::Assign(place, expr) => {
                let expr = self.visit_expr(expr);
                let expr_ty = &expr.ty;
                let place = self.visit_place(place, Mode::Assigning);

                // Check the type
                assert_eq!(&place.ty, expr_ty, "expression type ({expr_ty}) of {expr:?} should match the type of variable {place:?} ({})", place.ty);
                Assign(place, expr)
            }
            ast::Stmt::While { cond, body } => {
                let cond = self.visit_expr(cond);
                assert_eq!(
                    cond.ty, BoolT,
                    "condition {cond:?} should compute to a boolean value"
                );
                let body = self.visit_block(body);
                assert_eq!(
                    body.ret.ty, UnitT,
                    "body of expression should not return anything"
                );
                While { cond, body }
            }
            ast::Stmt::ExprS(e) => ExprS(self.visit_expr(e)),
        }
    }

    fn visit_program(&mut self, p: ast::Program<'ctx>) -> Program<'ctx> {
        let ast::Program { funs, structs } = p;

        // Add all function signatures to the context to allow for (mutual) recursion.
        for (&name, f) in &funs {
            assert_eq!(name, f.name);
            self.add_fun(f.signature());
        }

        // Add all valid type names in the context, so they may be referenced
        // everywhere.
        for &name in structs.keys() {
            assert!(
                self.valid_type_names.insert(name),
                "{name} is defined twice"
            );
        }

        // Note: order is important.
        // We must visit structures first.
        Program {
            arena: self.ctx.arena,
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

    fn visit_struct(&mut self, strukt: ast::Struct<'ctx>) -> Struct<'ctx> {
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
    use crate::{config::Config, Arena};

    #[test]
    fn check_pop_option() {
        let arena = Arena::new();
        let config = Config { input: "" };
        let mut ctx = Context::new(config, &arena);
        let mut typer = Typer::new(&mut ctx);
        assert_eq!(typer.pop_scope(), None); // Cannot pop the static stratum

        typer.push_scope();
        assert!(typer.pop_scope().is_some());

        assert_eq!(typer.pop_scope(), None); // Still cannot pop it
    }
}
