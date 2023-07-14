//! Typing.

use std::borrow::Borrow;
use std::collections::HashMap;
use std::default::default;

use itertools::Itertools;
use num_traits::ToPrimitive;
use ExprKind::*;
use PatKind::*;
use PlaceKind::*;
use StmtKind::*;
use Ty::*;

use crate::ast::fun::{binop_bool_sig, binop_gen_sig, binop_int_sig, unop_bool_sig};
use crate::codes::*;
use crate::reporter::Diagnostic;
use crate::tast::place::LhsPlace;
use crate::tast::*;
use crate::utils::{boxed, keys_match};
use crate::Scoped;
use crate::{ast, Context};

/// A typer that will type-check some program by
/// visiting it.
pub(crate) struct Typer<'t, 'ctx> {
    /// Compilation context.
    ctx: &'t mut Context<'ctx>,
    /// Map between function names and their definitions.
    fun_env: HashMap<&'ctx str, &'ctx ast::FunSig<'ctx>>,
    /// Map between `struct` names and their definitions.
    struct_env: HashMap<&'ctx str, &'ctx Struct<'ctx>>,
    /// Map between `enum` names and their definitions.
    enum_env: HashMap<&'ctx str, &'ctx Enum<'ctx>>,
    /// The typing environment stack.
    env: Scoped<Varname<'ctx>, ast::VarDef<'ctx>>,
    /// The typing environment stack.
    generic_types: Scoped<TyVar<'ctx>, ()>,
    /// Type variable substitutions.
    subst: TySubst<'ctx>,
}

impl<'t, 'ctx> Typer<'t, 'ctx> {
    /// Creates a new typer.
    pub fn new(ctx: &'t mut Context<'ctx>) -> Self {
        let mut typer = Self {
            subst: TySubst::new(ctx.arena),
            ctx,
            fun_env: default(),
            struct_env: default(),
            enum_env: default(),
            env: default(),
            generic_types: default(),
        };

        // Add builtin function signatures.
        typer.add_fun(binop_int_sig("+", IntT));
        typer.add_fun(binop_int_sig("-", IntT));
        typer.add_fun(binop_int_sig("*", IntT));
        typer.add_fun(binop_int_sig("/", IntT));
        typer.add_fun(binop_int_sig("%", IntT));
        typer.add_fun(binop_gen_sig("==", TyVar::Named("T"), BoolT));
        let array_t = ArrayT(typer.ctx.alloc(VarT(TyVar::Named("T"))));
        typer.add_fun(ast::FunSig {
            name: "push",
            ty_params: vec![TyVar::Named("T")],
            params: vec![
                ast::ref_param("array", array_t),
                ast::param("el", VarT(TyVar::Named("T"))),
            ],
            ret_ty: UnitT,
            span: default(),
        });
        typer.add_fun(ast::FunSig {
            name: "swap",
            ty_params: vec![TyVar::Named("T")],
            params: vec![
                ast::ref_param("x", VarT(TyVar::Named("T"))),
                ast::ref_param("y", VarT(TyVar::Named("T"))),
            ],
            ret_ty: UnitT,
            span: default(),
        });
        typer.add_fun(ast::FunSig {
            name: "len",
            ty_params: vec![TyVar::Named("T")],
            params: vec![ast::param("x", VarT(TyVar::Named("T")))],
            ret_ty: IntT,
            span: default(),
        });
        typer.add_fun(binop_int_sig("rand", IntT));
        typer.add_fun(binop_int_sig("<=", BoolT));
        typer.add_fun(binop_int_sig("<", BoolT));
        typer.add_fun(binop_int_sig(">=", BoolT));
        typer.add_fun(binop_int_sig(">", BoolT));
        typer.add_fun(binop_int_sig("!=", BoolT));
        typer.add_fun(binop_bool_sig("&&", BoolT));
        typer.add_fun(binop_bool_sig("||", BoolT));
        typer.add_fun(unop_bool_sig("!", BoolT));
        typer.add_fun(unop_bool_sig("assert", UnitT));

        typer
    }

    /// Type-checks a piece of code.
    pub fn check(&mut self, p: ast::Program<'ctx>) -> Program<'ctx> {
        self.visit_program(p)
    }

    /// Creates a new scope.
    fn push_scope(&mut self) {
        self.env.push_scope();
        self.generic_types.push_scope();
    }

    /// Pops and removes the current scope.
    fn pop_scope(&mut self) -> anyhow::Result<()> {
        self.env.pop_scope()?;
        self.generic_types.pop_scope()?;
        Ok(())
    }

    /// Gets the definition of a variable.
    ///
    /// It will return a reference into that definition, and the id of the
    /// stratum in which the variables resides.
    fn get_var(&self, v: impl Borrow<Varname<'ctx>>) -> Option<(&ast::VarDef<'ctx>, Stratum)> {
        // Iterate over environments in reverse (last declared first processed)
        // order
        // Returns the first environment that has that variable declared
        let v = v.borrow();
        self.env
            .get_and_scope(v)
            .map(|(i, x)| (x, i.try_into().unwrap()))
    }

    /// Declares a new variable in the context.
    fn add_var(&mut self, vardef: impl Into<ast::VarDef<'ctx>>) {
        let vardef = vardef.into();
        self.env.insert(vardef.name(), vardef);
    }

    /// Checks if some global identifier is already declared in scope.
    ///
    /// If it is already declared, it will emit an error.
    ///
    /// * `name`: The name of the identifier.
    /// * `span`: The span of the identifier.
    fn check_already_declared(&self, name: &str, span: Span) {
        // Compute the `span` of the already defined item, if any.
        let item = if let Some(f) = self.fun_env.get(name) {
            Some(f.span)
        } else if let Some(s) = self.struct_env.get(name) {
            Some(s.span)
        } else {
            self.enum_env.get(name).map(|e| e.span)
        };

        if let Some(span2) = item {
            self.ctx.emit(
                Diagnostic::error()
                    .with_code(ITEM_REDEFINED_ERROR)
                    .with_message(format!("Duplicate definition with the same name `{name}`"))
                    .with_labels(vec![
                        span.as_label().with_message("new definition"),
                        span2
                            .as_secondary_label()
                            .with_message("previous definition"),
                    ]),
            );
        }
    }

    /// Declares a new function in the context.
    fn add_fun(&mut self, fun_def: impl Into<ast::FunSig<'ctx>>) {
        let fun_def = fun_def.into();
        self.check_already_declared(fun_def.name, fun_def.span);
        self.fun_env.insert(fun_def.name, self.ctx.alloc(fun_def));
    }

    /// Declares a new structure in the context.
    fn add_struct(&mut self, strukt: impl Into<Struct<'ctx>>) {
        let strukt = strukt.into();
        self.check_already_declared(strukt.name, strukt.span);
        self.struct_env.insert(strukt.name, self.ctx.alloc(strukt));
    }

    /// Declares a new enum in the context.
    fn add_enum(&mut self, enun: impl Into<Enum<'ctx>>) {
        let enun = enun.into();
        self.check_already_declared(enun.name, enun.span);
        self.enum_env.insert(enun.name, self.ctx.alloc(enun));
    }

    /// Returns the current stratum/scope id.
    fn current_stratum(&self) -> Stratum {
        (self.env.len() - 1).try_into().unwrap()
    }

    /// Type-checks a **lhs** place expression.
    ///
    /// This will essentially visit and type the place, and **change the mode**
    /// to `Assigning`.
    ///
    /// * `ret_ty`: Return type of the function.
    /// * `in_loop`: is this place in a loop?
    fn visit_lhs_place(
        &mut self,
        place: ast::Place<'ctx>,
        ret_ty: TyUse<'ctx>,
        in_loop: bool,
    ) -> Option<LhsPlace<'ctx>> {
        match place.kind {
            ast::PlaceKind::VarP(var) => {
                if let Some((vardef, stm)) = self.get_var(var) {
                    Some(LhsPlace::var(
                        var,
                        vardef.ty,
                        stm,
                        LhsMode::Assigning,
                        place.span,
                    ))
                } else {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(UNKNOWN_VAR_ERROR)
                            .with_message(format!("Unknown variable `{var}`"))
                            .with_labels(vec![var.as_span().into()]),
                    );
                    None
                }
            }
            ast::PlaceKind::IndexP(box e, box ix) => {
                let e = self.visit_expr(e, ret_ty, in_loop);
                let ix = self.visit_expr(ix, ret_ty, in_loop);
                let array_ty = self.ctx.alloc(Ty::hole(e.span));
                match (
                    e.ty.unify(&ArrayT(array_ty), &mut self.subst),
                    ix.ty.unify(&IntT, &mut self.subst),
                ) {
                    (true, true) => {
                        let ty = array_ty.subst(self.ctx.arena, &self.subst);
                        let e_stm = e.stm; // Needed now because we move e after
                        Some(LhsPlace {
                            kind: IndexP(boxed(e), boxed(ix)),
                            ty,
                            stm: e_stm,
                            mode: LhsMode::Assigning,
                            span: place.span,
                        })
                    }
                    (false, true) => {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(TYPE_MISMATCH_ERROR)
                                .with_message(format!(
                                    "Expected array type, found type {}",
                                    e.ty.subst(self.ctx.arena, &self.subst)
                                ))
                                .with_labels(vec![e.span.as_label()])
                                .with_notes(vec!["Only array can be indexed".to_string()]),
                        );
                        None
                    }
                    (_, false) => {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(TYPE_MISMATCH_ERROR)
                                .with_message(format!(
                                    "Expected type int, found type {}",
                                    ix.ty.subst(self.ctx.arena, &self.subst)
                                ))
                                .with_labels(vec![ix.span.as_label()])
                                .with_notes(vec![
                                    "Only integer indexing is supported for arrays".to_string()
                                ]),
                        );
                        None
                    }
                }
            }
            ast::PlaceKind::FieldP(box s, field) => {
                let s = self.visit_expr(s, ret_ty, in_loop);
                match s.ty {
                    StructT(strukt) => {
                        let strukt = self.struct_env[strukt];
                        // Check that the field we want to access exists
                        let ty = match strukt.get_field(field) {
                            Some(ty) => ty,
                            None => {
                                self.ctx.emit(
                                    Diagnostic::error()
                                        .with_code(FIELD_ACCESS_ERROR)
                                        .with_message(format!("No such field `{field}`"))
                                        .with_labels(vec![s
                                            .span
                                            .as_label()
                                            .with_message(format!("has no field named `{field}`"))])
                                        .with_notes(vec![format!(
                                            "possible fields are {}",
                                            strukt
                                                .fields
                                                .keys()
                                                .map(|x| format!("`{x}`"))
                                                .join(", ")
                                        )]),
                                );
                                return None;
                            }
                        };

                        Some(LhsPlace {
                            stm: s.stm,
                            kind: FieldP(boxed(s), field),
                            mode: LhsMode::Assigning,
                            ty,
                            span: place.span,
                        })
                    }
                    VarT(_) => Some(LhsPlace {
                        stm: s.stm,
                        kind: FieldP(boxed(s), field),
                        mode: LhsMode::Assigning,
                        ty: Ty::hole(place.span),
                        span: place.span,
                    }),
                    _ => {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(FIELD_NOT_STRUCT_ERROR)
                                .with_message("Field access only works for structures")
                                .with_labels(vec![s.span.as_label().with_message("has no fields")]),
                        );
                        None
                    }
                }
            }
            ast::PlaceKind::ElemP(box tuple, elem) => {
                let tuple = self.visit_expr(tuple, ret_ty, in_loop);
                match tuple.ty {
                    TupleT(elems) => {
                        // Check that the field we want to access exists
                        let ty = match elems.get(elem) {
                            Some(ty) => *ty,
                            None => {
                                self.ctx.emit(
                                    Diagnostic::error()
                                        .with_code(TUPLE_ACCESS_ERROR)
                                        .with_message("Tuple index is out of bounds")
                                        .with_labels(vec![tuple.span.as_label().with_message(
                                            format!("has only {} elements", elems.len()),
                                        )]),
                                );
                                return None;
                            }
                        };

                        Some(LhsPlace {
                            stm: tuple.stm,
                            kind: ElemP(boxed(tuple), elem),
                            mode: LhsMode::Assigning,
                            ty,
                            span: place.span,
                        })
                    }
                    VarT(_) => Some(LhsPlace {
                        stm: tuple.stm,
                        kind: ElemP(boxed(tuple), elem),
                        mode: LhsMode::Assigning,
                        ty: Ty::hole(place.span),
                        span: place.span,
                    }),
                    _ => {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_message("This kind of indexing only works for tuples")
                                .with_labels(vec![tuple
                                    .span
                                    .as_label()
                                    .with_message("has no elements")]),
                        );
                        None
                    }
                }
            }
        }
    }

    /// Type-checks a **rhs** place expression.
    ///
    /// * `ret_ty`: Return type of the function.
    /// * `mode`: Desired overriding mode. Put [`None`] if you do not wish to
    ///   override the value set by the parser AST.
    /// * `in_loop`: is this place in a loop?
    fn visit_rhs_place(
        &mut self,
        place: ast::Place<'ctx>,
        mode: Option<Mode>,
        ret_ty: TyUse<'ctx>,
        in_loop: bool,
    ) -> Option<Place<'ctx>> {
        let mut place = match place.kind {
            ast::PlaceKind::VarP(v) => match self.get_var(v) {
                Some((vardef, stm)) => Some(Place::var(v, vardef.ty, stm, place.mode, place.span)),
                None => {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(UNKNOWN_VAR_ERROR)
                            .with_message(format!("Unknown variable `{v}`"))
                            .with_labels(vec![v.as_span().into()]),
                    );
                    None
                }
            },
            ast::PlaceKind::FieldP(box s, field) => {
                let s = self.visit_expr(s, ret_ty, in_loop);
                if let StructT(strukt) = &s.ty {
                    let strukt = self.struct_env[strukt];
                    // Check that the field we want to access exists
                    let ty = match strukt.get_field(field) {
                        Some(ty) => ty,
                        None => {
                            self.ctx.emit(
                                Diagnostic::error()
                                    .with_code(FIELD_ACCESS_ERROR)
                                    .with_message(format!("No such field `{field}`"))
                                    .with_labels(vec![s
                                        .span
                                        .as_label()
                                        .with_message(format!("has no field named `{field}`"))])
                                    .with_notes(vec![format!(
                                        "possible fields are {}",
                                        strukt.fields.keys().map(|x| format!("`{x}`")).join(", ")
                                    )]),
                            );
                            Ty::hole(place.span)
                        }
                    };
                    let stm = s.stm; // Needed now because we move s after
                    Some(Place {
                        kind: FieldP(boxed(s), field),
                        mode: place.mode,
                        ty,
                        stm,
                        span: place.span,
                    })
                } else {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(FIELD_NOT_STRUCT_ERROR)
                            .with_message("Field access only works for structures")
                            .with_labels(vec![s.span.as_label().with_message("has no fields")]),
                    );
                    None
                }
            }
            ast::PlaceKind::ElemP(box tuple, elem) => {
                let tuple = self.visit_expr(tuple, ret_ty, in_loop);
                if let TupleT(elems) = &tuple.ty {
                    // Check that the field we want to access exists
                    let ty = match elems.get(elem) {
                        Some(ty) => *ty,
                        None => {
                            self.ctx.emit(
                                Diagnostic::error()
                                    .with_code(TUPLE_ACCESS_ERROR)
                                    .with_message("Tuple index is out of bounds")
                                    .with_labels(vec![tuple.span.as_label().with_message(
                                        format!("has only {} elements", elems.len()),
                                    )]),
                            );
                            Ty::hole(place.span)
                        }
                    };
                    let stm = tuple.stm; // Needed now because we move `tuple` after
                    Some(Place {
                        kind: ElemP(boxed(tuple), elem),
                        mode: place.mode,
                        ty,
                        stm,
                        span: place.span,
                    })
                } else {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_message("This kind of indexing only works for tuples")
                            .with_labels(vec![tuple
                                .span
                                .as_label()
                                .with_message("has no elements")]),
                    );
                    None
                }
            }
            ast::PlaceKind::IndexP(box e, box ix) => {
                let e = self.visit_expr(e, ret_ty, in_loop);
                let ix = self.visit_expr(ix, ret_ty, in_loop);
                let array_ty = self.ctx.alloc(Ty::hole(e.span));
                match (
                    e.ty.unify(&ArrayT(array_ty), &mut self.subst),
                    ix.ty.unify(&IntT, &mut self.subst),
                ) {
                    (true, true) => {
                        let ty = array_ty.subst(self.ctx.arena, &self.subst);
                        let e_stm = e.stm; // Needed now because we move e after
                        Some(Place {
                            kind: IndexP(boxed(e), boxed(ix)),
                            mode: place.mode,
                            ty,
                            stm: e_stm,
                            span: place.span,
                        })
                    }
                    (false, true) => {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(TYPE_MISMATCH_ERROR)
                                .with_message(format!(
                                    "Expected array type, found type {}",
                                    e.ty.subst(self.ctx.arena, &self.subst)
                                ))
                                .with_labels(vec![e.span.as_label()])
                                .with_notes(vec!["Only array can be indexed".to_string()]),
                        );
                        None
                    }
                    (_, false) => {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(TYPE_MISMATCH_ERROR)
                                .with_message(format!(
                                    "Expected type int, found type {}",
                                    ix.ty.subst(self.ctx.arena, &self.subst)
                                ))
                                .with_labels(vec![ix.span.as_label()])
                                .with_notes(vec![
                                    "Only integer indexing is supported for arrays".to_string()
                                ]),
                        );
                        None
                    }
                }
            }
        }?;

        // If set, override the mode
        if let Some(mode) = mode {
            place.mode = mode;
        }
        Some(place)
    }

    /// Type-checks a call on a callable object.
    ///
    /// * `namespaced`: element being called
    /// * `args`: arguments to the call
    /// * `span`: span of the entire call location
    fn visit_call(
        &mut self,
        namespaced: Namespaced<'ctx>,
        args: Vec<Arg<'ctx>>,
        span: Span,
    ) -> Expr<'ctx> {
        let mut path = namespaced.path();
        let root = path.next().unwrap(); // Namespaced necessarily starts with something

        if let Some(enun) = self.enum_env.get(&root) && let Some(variant) = path.next()
        && let Some(params) = enun.variants.get(variant) && path.next().is_none() {
            if args.len() != params.len() {
                self.ctx.emit(
                    Diagnostic::error()
                        .with_code(ARG_NB_MISMATCH)
                        .with_message(
                        format!("Expected {} argument(s), found {}", params.len(), args.len()))
                        .with_labels(vec![namespaced.span.as_label()])
                        .with_notes(vec![format!("constructor {root}::{variant} expects {} argument(s)", params.len())])
                        .with_labels(vec![enun.span.as_secondary_label().with_message("variant is defined here")]),
                );
                return Expr::hole(namespaced.span);
            }

            // Check type of arguments.
            let mut new_args = vec![];
            for (
                i,
                (
                    arg,
                    ast::TyUse { kind: param_ty, .. },
                ),
            ) in args.into_iter().zip(params.iter()).enumerate()
            {
                let arg_ty = arg.ty();
                match arg.into_standard() {
                    Ok(arg) => {
                        if !arg_ty.unify(param_ty, &mut self.subst) {
                            self.ctx.emit(
                                    Diagnostic::error()
                                        .with_code(TYPE_MISMATCH_ERROR)
                                        .with_message(format!(
                                            "expected type {}, found type {}",
                                            param_ty.subst(self.ctx.arena, &self.subst),
                                            arg_ty.subst(self.ctx.arena, &self.subst)
                                        ))
                                        .with_labels(vec![arg.span.as_label()])
                                        .with_notes(vec![format!(
                                            "{} is the expected type for argument #{} of `{}`",
                                            param_ty.subst(self.ctx.arena, &self.subst),
                                            i + 1,
                                            root
                                        )]),
                                );
                        }
                        new_args.push(arg);
                    }
                    Err(arg) => {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(WRONG_REF_MODE_ERROR)
                                .with_message("the `@` referencing mode is not allowed in patterns")
                                .with_labels(vec![arg.span.as_label()])
                                .with_notes(vec!["help: remove the @ symbol".to_string()])
                        );
                        return Expr::hole(span);
                    }
                }
            }

            Expr::new(VariantE { enun: root, variant, args: new_args }, EnumT(enun.name), self.current_stratum(), span)
        } else if let Some(&fun) = self.fun_env.get(root) {
            let fun = fun.clone().instantiate_tys(self.ctx.arena, span);
            let name = root;
            // Check the number of arguments.
            if args.len() != fun.params.len() {
                self.ctx.emit(
                    Diagnostic::error()
                        .with_code(ARG_NB_MISMATCH)
                        .with_message(
                        format!("Expected {} arguments, found {}", fun.params.len(), args.len()))
                        .with_labels(vec![namespaced.span.as_label()])
                        .with_notes(vec![format!("{root} expects {} argument(s)", fun.params.len())]),
                );
                return Expr::hole(namespaced.span);
            }

            // Check type of arguments.
            for (
                i,
                (
                    arg,
                    param
                ),
            ) in args.iter().zip(fun.params.iter()).enumerate()
            {
                let arg_ty = arg.ty();
                let param_ty = param.ty();
                if param.byref != arg.byref() {
                    let displayed = |byref| if byref { "some referencing (@) mode" } else {"no referencing (@) mode"};
                        self.ctx.emit(
                        Diagnostic::error()
                            .with_code(WRONG_REF_MODE_ERROR)
                            .with_message(format!(
                                "expected {}, found {}",
                                displayed(param.byref),
                                displayed(arg.byref()),
                            ))
                            .with_labels(vec![arg.span.as_label(), param.span.as_secondary_label().with_message("function parameter defined here")])
                            .with_notes(vec![format!("help: this function parameter {} be modified in place or bound to another value", if param.byref {
                                "must"
                            } else {
                                "cannot"
                            })])
                       );
                }
                if !arg_ty.unify(&param_ty, &mut self.subst)  {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(TYPE_MISMATCH_ERROR)
                            .with_message(format!(
                                "expected type {}, found type {}",
                                param_ty.subst(self.ctx.arena, &self.subst),
                                arg_ty.subst(self.ctx.arena, &self.subst)
                            ))
                            .with_labels(vec![arg.span.as_label()])
                            .with_notes(vec![format!(
                                "{} is the expected type for argument #{} of `{}`",
                                param_ty.subst(self.ctx.arena, &self.subst),
                                i + 1,
                                name
                            )]),
                        );
                }
            }

            Expr::new(
                CallE { name: namespaced, args },
                fun.ret_ty,
                self.current_stratum(),
                span,
            )
        } else {
            self.ctx.emit(
                Diagnostic::error()
                    .with_code(NOT_CALLABLE_ERROR)
                    .with_message(
                        format!("{} is not a callable identifier", root))
                    .with_labels(vec![namespaced.span.as_label()])
            );
            Expr::hole(namespaced.span)
        }
    }

    /// Type-checks an expression.
    ///
    /// * `ret_ty`: Return type of the function.
    /// * `in_loop`: is this expression in a loop?
    fn visit_expr(&mut self, e: ast::Expr<'ctx>, ret_ty: TyUse<'ctx>, in_loop: bool) -> Expr<'ctx> {
        let span = e.span;
        match e.kind {
            ast::ExprKind::UnitE => Expr::new(UnitE, UnitT, self.current_stratum(), span),
            ast::ExprKind::BoolE(b) => Expr::new(BoolE(b), BoolT, self.current_stratum(), span),
            ast::ExprKind::IntegerE(i) => {
                Expr::new(IntegerE(i), IntT, self.current_stratum(), span)
            }
            ast::ExprKind::StringE(s) => Expr::new(StringE(s), StrT, self.current_stratum(), span),
            ast::ExprKind::NamespacedE(namespaced) => self.visit_call(namespaced, vec![], span),
            ast::ExprKind::PlaceE(place) => {
                let place = self.visit_rhs_place(place, None, ret_ty, in_loop);
                match place {
                    Some(place) => {
                        let (stm, ty) = (place.stm, place.ty);
                        Expr::new(PlaceE(place), ty, stm, span)
                    }
                    None => Expr::hole(span),
                }
            }
            // Make a special case for `print` until we get generic functions so that we
            // can express `print` more elegantly with the other builtin functions.
            ast::ExprKind::CallE {
                name,
                args: old_args,
            } if name.name == "debug" => {
                let mut args = vec![];
                for arg in old_args.into_iter() {
                    match arg.into_standard() {
                        Ok(arg) => args.push(Arg::std(self.visit_expr(arg, ret_ty, in_loop))),
                        Err(arg) => {
                            self.ctx.emit(
                                Diagnostic::error()
                                    .with_code(WRONG_REF_MODE_ERROR)
                                    .with_message("`@` referencing mode is not allowed for any argument to `print`")
                                    .with_labels(vec![arg.span.as_label()])
                                    .with_notes(vec![
                                        "help: remove the `@` operator".to_string()
                                    ]),
                            );
                            return Expr::hole(span);
                        }
                    }
                }

                Expr::new(CallE { name, args }, UnitT, self.current_stratum(), span)
            }
            ast::ExprKind::CallE {
                name,
                args: old_args,
            } => {
                let mut args = vec![];
                for arg in old_args.into_iter() {
                    let arg = match arg.kind {
                        ast::ArgKind::Standard(e) => Arg::std(self.visit_expr(e, ret_ty, in_loop)),
                        ast::ArgKind::InPlace(place) => {
                            match self.visit_rhs_place(
                                place,
                                Some(Mode::MutBorrowed),
                                ret_ty,
                                in_loop,
                            ) {
                                Some(place) => Arg::in_place(place, arg.span),
                                None => return Expr::hole(span),
                            }
                        }
                        ast::ArgKind::Binding(from, to) => {
                            let from = self.visit_expr(from, ret_ty, in_loop);
                            let to = match self.visit_lhs_place(to, ret_ty, in_loop) {
                                Some(place) => place,
                                None => return Expr::hole(span),
                            };
                            if from.ty != to.ty {
                                self.ctx.emit(
                                    Diagnostic::error()
                                        .with_code(TYPE_MISMATCH_ERROR)
                                        .with_message("both operands of the `@` binding operator should have the same type")
                                        .with_labels(vec![from.span.as_label().with_message(format!(
                                            "has type {}",
                                            from.ty.subst(self.ctx.arena, &self.subst)
                                        ))])
                                        .with_labels(vec![to.span.as_label().with_message(format!(
                                            "has type {}",
                                            to.ty.subst(self.ctx.arena, &self.subst)
                                        ))]),
                                );
                                return Expr::hole(span);
                            }

                            Arg::binding(from, to, span)
                        }
                    };
                    args.push(arg);
                }
                self.visit_call(name, args, span)
            }
            ast::ExprKind::IfE(box cond, box iftrue, box iffalse) => {
                let cond = self.visit_expr(cond, ret_ty, in_loop);
                let iftrue = self.visit_block(iftrue, ret_ty, in_loop);
                let iffalse = self.visit_block(iffalse, ret_ty, in_loop);

                // Condition must be a bool
                if !cond.ty.unify(&BoolT, &mut self.subst) {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(TYPE_MISMATCH_ERROR)
                            .with_message(format!(
                                "expected bool, found {}",
                                cond.ty.subst(self.ctx.arena, &self.subst)
                            ))
                            .with_labels(vec![cond.span.as_label()])
                            .with_notes(vec!["if condition must be of type bool".to_string()]),
                    );
                }

                // If and else branches must be of the same type
                if iftrue.ret.ty != iffalse.ret.ty {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(TYPE_MISMATCH_ERROR)
                            .with_message("if and else branches must have the same type")
                            .with_labels(vec![iftrue.span.as_label().with_message(format!(
                                "has type {}",
                                iftrue.ret.ty.subst(self.ctx.arena, &self.subst)
                            ))])
                            .with_labels(vec![iffalse.span.as_label().with_message(format!(
                                "has type {}",
                                iffalse.ret.ty.subst(self.ctx.arena, &self.subst)
                            ))]),
                    );
                }

                let iftrue_stm = iftrue.ret.stm;
                let iffalse_stm = iffalse.ret.stm;
                let if_ty = iftrue.ret.ty;
                Expr::new(
                    IfE(boxed(cond), boxed(iftrue), boxed(iffalse)),
                    if_ty,
                    std::cmp::max(iftrue_stm, iffalse_stm),
                    span,
                )
            }
            ast::ExprKind::BlockE(box e) => {
                let b = self.visit_block(e, ret_ty, in_loop);
                let ret_stm = b.ret.stm;
                let ret_ty = b.ret.ty;
                Expr::new(BlockE(boxed(b)), ret_ty, ret_stm, span)
            }
            ast::ExprKind::StructE {
                name: s_name,
                fields,
            } => {
                // Compute the type of the fields
                // Because of borrowing rules, we need to do that before we immutably borrow
                // `self` through `.get_struct()` since we need a mutable borrow into `self`
                // here.
                let fields: Vec<(&str, Expr)> = fields
                    .into_iter()
                    .map(|(name, expr)| (name, self.visit_expr(expr, ret_ty, in_loop)))
                    .collect();

                let strukt = match self.struct_env.get(s_name) {
                    Some(strukt) => strukt,
                    None => {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(UNKNOWN_TYPE_VAR)
                                .with_message(format!("no structure named `{s_name}` in context"))
                                .with_labels(vec![span.into()]),
                        );
                        return Expr::hole(span);
                    }
                };

                // Check that the instance has the same field names as the declaration
                if !keys_match(&strukt.fields, fields.iter().map(|(field, _)| field)) {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(STRUCT_INSTANCE_ERROR)
                            .with_message(
                                "Erroneous struct instantiation (missing or extraneous fields)",
                            )
                            .with_labels(vec![
                                span.as_label().with_message("your instantiation"),
                                strukt
                                    .span
                                    .as_secondary_label()
                                    .with_message("struct definition"),
                            ]),
                    );
                } else {
                    // Ok, so the field names match
                    // Check that the type of each field matches the expected one
                    for (fname, Expr { ty, span, .. }) in &fields {
                        let expected = strukt.get_field(*fname).unwrap(); // Ok because we checked just before our declaration matches the structure
                                                                          // fields.
                        if expected != *ty {
                            self.ctx.emit(
                                Diagnostic::error()
                                    .with_code(TYPE_MISMATCH_ERROR)
                                    .with_message(format!(
                                        "expected type {}, found type {}",
                                        expected.subst(self.ctx.arena, &self.subst),
                                        ty.subst(self.ctx.arena, &self.subst)
                                    ))
                                    .with_labels(vec![span.as_label()])
                                    .with_notes(vec![format!(
                                        "field `{fname}` of `{s_name}` is of type {}",
                                        ty.subst(self.ctx.arena, &self.subst)
                                    )]),
                            );
                        }
                    }
                }

                let common_stm = fields
                    .iter()
                    .map(|(_, field)| field)
                    .fold(Stratum::static_stm(), |s1, Expr { stm: s2, .. }| {
                        std::cmp::max(s1, *s2)
                    });
                Expr::new(
                    StructE {
                        name: s_name,
                        fields,
                    },
                    StructT(strukt.name),
                    common_stm,
                    span,
                )
            }
            ast::ExprKind::ArrayE(array) => {
                // Compute the type of the items
                let array: Vec<Expr> = array
                    .into_iter()
                    .map(|expr| self.visit_expr(expr, ret_ty, in_loop))
                    .collect();

                // Compute the common stratum, which is the max of the stratum of all the items
                let common_stm = array
                    .iter()
                    .fold(Stratum::static_stm(), |s1, Expr { stm: s2, .. }| {
                        core::cmp::max(s1, *s2)
                    });

                // Check that all items unify to the same type
                let all_same_type = array
                    .array_windows::<2>()
                    .all(|[item1, item2]| item1.ty.unify(&item2.ty, &mut self.subst));

                // Computing the type of the array
                let ty = if let Some(item1) = array.first() {
                    // there is at least one array element! easy-peasy
                    ArrayT(self.ctx.alloc(item1.ty))
                } else {
                    // no item in array, so we must use a type variable
                    ArrayT(self.ctx.alloc(Ty::hole(span)))
                };

                if !all_same_type {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(HETEROGENEOUS_LISTS_ERROR)
                            .with_message(format!(
                                "all items in the list should have the same type {}",
                                array[0].ty,
                            ))
                            .with_labels(vec![span.into()]),
                    );
                }

                Expr::new(ArrayE(array), ty, common_stm, span)
            }
            ast::ExprKind::TupleE(items) => {
                // Compute the type each the items
                let items: Vec<Expr> = items
                    .into_iter()
                    .map(|expr| self.visit_expr(expr, ret_ty, in_loop))
                    .collect();

                let items_ty: Vec<Ty> = items.iter().map(|item| item.ty).collect();
                let items_ty: &'ctx [Ty] = self.ctx.alloc(items_ty);

                // Compute the common stratum, which is the max of the stratum of all the items
                let common_stm = items
                    .iter()
                    .fold(Stratum::static_stm(), |s1, Expr { stm: s2, .. }| {
                        core::cmp::max(s1, *s2)
                    });

                Expr::new(TupleE(items), TupleT(items_ty), common_stm, span)
            }
            ast::ExprKind::RangeE(box start, box end) => {
                let start = self.visit_expr(start, ret_ty, in_loop);
                let end = self.visit_expr(end, ret_ty, in_loop);

                // Check that the type of both ends is `int`
                if !start.ty.unify(&IntT, &mut self.subst) {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(TYPE_MISMATCH_ERROR)
                            .with_message(format!(
                                "expected type int, found type {}",
                                start.ty.subst(self.ctx.arena, &self.subst)
                            ))
                            .with_labels(vec![start.span.as_label()])
                            .with_notes(vec!["ranges only work for integers".to_string()]),
                    );
                }
                if !end.ty.unify(&IntT, &mut self.subst) {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(TYPE_MISMATCH_ERROR)
                            .with_message(format!(
                                "expected type int, found type {}",
                                end.ty.subst(self.ctx.arena, &self.subst)
                            ))
                            .with_labels(vec![end.span.as_label()])
                            .with_notes(vec!["ranges only work for integers".to_string()]),
                    );
                }

                let (start_stm, end_stm) = (start.stm, end.stm); // Necessary bc we move start before getting `start.stm`
                Expr::new(
                    RangeE(boxed(start), boxed(end)),
                    IterT(self.ctx.alloc(IntT)),
                    std::cmp::max(start_stm, end_stm),
                    span,
                )
            }
            ast::expr::ExprKind::MatchE(box matched, branches) => {
                let matched = self.visit_expr(matched, ret_ty, in_loop);

                // Compute the branches.
                let mut new_branches: Vec<(Pat, Expr)> = vec![];
                for (pat, expr) in branches {
                    let pat = match self.visit_pattern(pat, matched.ty, matched.stm) {
                        Some(pat) => pat,
                        None => return Expr::hole(span),
                    };
                    if pat.ty != matched.ty {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(TYPE_MISMATCH_ERROR)
                                .with_message(format!(
                                    "Expected type {:?}, found type {:?}",
                                    pat.ty.subst(self.ctx.arena, &self.subst),
                                    matched.ty.subst(self.ctx.arena, &self.subst)
                                ))
                                .with_labels(vec![
                                    pat.span
                                        .as_label()
                                        .with_message(format!("pattern is of type {:?}", pat.ty)),
                                    matched.span.as_secondary_label().with_message(format!(
                                        "matched element is of type {:?}",
                                        matched.ty
                                    )),
                                ]),
                        );
                    };
                    self.push_scope();
                    self.introduce_pat_vars(&pat);
                    let expr = self.visit_expr(expr, ret_ty, in_loop);
                    self.pop_scope().expect("should be able to pop scope here");
                    new_branches.push((pat, expr));
                }

                // Compute the return type of the `match`
                let ty = if !new_branches.is_empty() {
                    // Not empty, so there is at least one expression with a type
                    let ty = new_branches[0].1.ty;

                    // Check that all branches have the same type
                    for (_, expr) in &new_branches {
                        if expr.ty != ty {
                            self.ctx.emit(
                                Diagnostic::error()
                                    .with_code(TYPE_MISMATCH_ERROR)
                                    .with_message("Branches do not have the same type")
                                    .with_labels(vec![expr.span.as_label().with_message(format!(
                                        "expected type {}, found type {}",
                                        ty.subst(self.ctx.arena, &self.subst),
                                        expr.ty.subst(self.ctx.arena, &self.subst)
                                    ))]),
                            );
                        }
                    }

                    ty
                } else {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(EMPTY_MATCH_ERROR)
                            .with_message("Empty `match`es are not supported yet")
                            .with_labels(vec![e.span.as_label()]),
                    );
                    Ty::hole(span)
                };

                // Compute the common stm of all the branches, which is the max of the stratum
                // of all the items
                let common_stm = new_branches
                    .iter()
                    .fold(Stratum::static_stm(), |s1, (_, Expr { stm: s2, .. })| {
                        core::cmp::max(s1, *s2)
                    });

                Expr::new(MatchE(boxed(matched), new_branches), ty, common_stm, span)
            }
            ast::ExprKind::HoleE => unreachable!(),
        }
    }

    /// Type-checks a statement.
    ///
    /// * `ret_ty`: Return type of the function.
    /// * `in_loop`: is this statement in a loop?
    fn visit_stmt(&mut self, s: ast::Stmt<'ctx>, ret_ty: TyUse<'ctx>, in_loop: bool) -> Stmt<'ctx> {
        let res: Option<Stmt> = try {
            match s.kind {
                ast::StmtKind::DeclareS(var, expr) => {
                    let rhs_span = expr.span;
                    let stm = self.current_stratum();
                    let expr = self.visit_expr(expr, ret_ty, in_loop);
                    let expr_ty = &expr.ty;

                    // Either get the annotation or create a fresh type variable
                    let var_ty = match var.ty_use() {
                        Some(ty) => ty.kind,
                        None => Ty::hole(expr.span),
                    };

                    // Check the type
                    let var_ty = if var_ty.unify(expr_ty, &mut self.subst) {
                        var_ty.subst(self.ctx.arena, &self.subst)
                    } else {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(TYPE_MISMATCH_ERROR)
                                .with_message("left and right hand side have incompatible types")
                                .with_labels(vec![rhs_span.as_label().with_message(format!(
                                    "expected type {}, found type {expr_ty}",
                                    var_ty.subst(self.ctx.arena, &self.subst)
                                ))]),
                        );
                        var_ty
                    };

                    self.add_var(VarDef {
                        var: var.into(),
                        ty: var_ty,
                        stm,
                        span: var.as_span(),
                    });

                    AssignS(
                        LhsPlace::var(var, var_ty, stm, LhsMode::Declaring, var.as_span()),
                        expr,
                    )
                    .with_span(s.span)
                }
                ast::StmtKind::AssignS(place, expr) => {
                    let rhs_span = expr.span;
                    let expr = self.visit_expr(expr, ret_ty, in_loop);
                    let expr_ty = &expr.ty;
                    let place = self.visit_lhs_place(place, ret_ty, in_loop)?;

                    // Check the type
                    if !place.ty.unify(expr_ty, &mut self.subst) {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(TYPE_MISMATCH_ERROR)
                                .with_message("left and right hand side have incompatible types")
                                .with_labels(vec![rhs_span.as_label().with_message(format!(
                                    "expected type {}, found type {expr_ty}",
                                    place.ty.subst(self.ctx.arena, &self.subst)
                                ))]),
                        );
                    }

                    AssignS(place, expr).with_span(s.span)
                }
                ast::StmtKind::WhileS { cond, body } => {
                    let cond = self.visit_expr(cond, ret_ty, in_loop);
                    if cond.ty != BoolT {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(TYPE_MISMATCH_ERROR)
                                .with_message(format!(
                                    "expected type bool, found type {}",
                                    cond.ty.subst(self.ctx.arena, &self.subst)
                                ))
                                .with_labels(vec![body.span.as_label().with_message("here")])
                                .with_notes(vec![
                                    "condition of while loop must be of type bool".to_string()
                                ]),
                        );
                    }

                    let body = self.visit_block(body, ret_ty, true);
                    if body.ret.ty != UnitT {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_message("body of expression should not return anything")
                                .with_labels(vec![body.span.as_label().with_message("here")]),
                        );
                    }

                    WhileS { cond, body }.with_span(s.span)
                }
                ast::StmtKind::ForS { item, iter, body } => {
                    let stm = self.current_stratum();

                    let iter = self.visit_expr(iter, ret_ty, in_loop);

                    let item_ty = self.ctx.alloc(Ty::hole(item.as_span()));
                    let item_ty = if iter.ty.unify(&IterT(item_ty), &mut self.subst) {
                        item_ty.subst(self.ctx.arena, &self.subst)
                    } else {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(TYPE_MISMATCH_ERROR)
                                .with_message(format!(
                                    "Expected iterator, found type {}",
                                    iter.ty.subst(self.ctx.arena, &self.subst)
                                ))
                                .with_labels(vec![iter.span.as_label()])
                                .with_notes(vec!["For loop requires an iterator here".to_string()]),
                        );
                        *item_ty
                    };
                    let item = item.as_vardef(item_ty);
                    //
                    // Introduce a new intermediate scope, in which `item` is defined`
                    self.push_scope();

                    self.add_var(item);
                    let item = VarDef::with_stratum(item, stm);

                    let body = self.visit_block(body, ret_ty, true);

                    // Pop the intermediate scope
                    self.pop_scope().expect("should be able to pop scope here");

                    if !body.ret.ty.unify(&UnitT, &mut self.subst) {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(TYPE_MISMATCH_ERROR)
                                .with_message("body of expression should not return anything")
                                .with_labels(vec![body.span.as_label().with_message("here")]),
                        );
                    }
                    ForS { item, iter, body }.with_span(s.span)
                }
                ast::StmtKind::ExprS(e) => {
                    let e = self.visit_expr(e, ret_ty, in_loop);
                    // Try to unify the expression with the unit type, which prevents some
                    // unnecessary `could not infer type` at this stage if the
                    // expression fails to compile
                    // Moreover, we may be interested in emitting a warning if the result is
                    // not an expression, since it should be used
                    if !e.ty.unify(&UnitT, &mut self.subst) {
                        self.ctx.emit(
                            Diagnostic::warning()
                                .with_code(UNUSED_RESULT)
                                .with_message(
                                    "this expression computes to a value that is not used",
                                )
                                .with_labels(vec![e.span.as_label().with_message("here")]),
                        );
                    }
                    ExprS(e).with_span(s.span)
                }
                ast::StmtKind::BreakS => {
                    if !in_loop {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(NOT_IN_LOOP_ERROR)
                                .with_message("this instruction can only be used within loops")
                                .with_labels(vec![s.span.as_label()]),
                        );
                    }
                    BreakS.with_span(s.span)
                }
                ast::StmtKind::ContinueS => {
                    if !in_loop {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(NOT_IN_LOOP_ERROR)
                                .with_message("this instruction can only be used within loops")
                                .with_labels(vec![s.span.as_label()]),
                        );
                    }
                    ContinueS.with_span(s.span)
                }
                ast::StmtKind::ReturnS(ret) => {
                    let ret = self.visit_expr(ret, ret_ty, in_loop);
                    if ret_ty.kind != ret.ty {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(TYPE_MISMATCH_ERROR)
                                .with_message(format!(
                                    "expected type {}, got type {}",
                                    ret_ty.kind.subst(self.ctx.arena, &self.subst),
                                    ret.ty.subst(self.ctx.arena, &self.subst)
                                ))
                                .with_labels(vec![
                                    ret_ty
                                        .span
                                        .as_label()
                                        .with_message("what the function returns"),
                                    ret.span.as_label().with_message("what you want to return"),
                                ]),
                        );
                    }
                    ReturnS(ret).with_span(s.span)
                }
                ast::StmtKind::SwapS(place1, place2) => {
                    let place1 =
                        self.visit_rhs_place(place1, Some(Mode::SMutBorrowed), ret_ty, in_loop)?;
                    let place2 =
                        self.visit_rhs_place(place2, Some(Mode::SMutBorrowed), ret_ty, in_loop)?;
                    if place1.stm != place2.stm {
                        let scope_str = |s, other| {
                            if s < other {
                                "lives longer"
                            } else {
                                "lives shorter"
                            }
                        };
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(SAME_SCOPE_SWAP_ERROR)
                                .with_message("swapped places must belong to the same scope")
                                .with_labels(vec![
                                    place1
                                        .span
                                        .as_label()
                                        .with_message(scope_str(place1.stm, place2.stm)),
                                    place2
                                        .span
                                        .as_label()
                                        .with_message(scope_str(place2.stm, place1.stm)),
                                ])
                                .with_notes(vec!["help: swapped places must have been declared in the same scope".to_string(),
                                "help: consider swapping the places manually (using a temporary variable) if you really want to exchange the values, but this will necessarily trigger a clone".to_string()]),
                        );
                    }
                    SwapS(place1, place2).with_span(s.span)
                }
                ast::StmtKind::HoleS => unreachable!(),
            }
        };
        res.unwrap_or_default()
    }

    /// Type-checks a block.
    ///
    /// * `ret_ty`: Return type of the function.
    /// * `in_loop`: is this block in a loop?
    fn visit_block(
        &mut self,
        b: ast::Block<'ctx>,
        ret_ty: TyUse<'ctx>,
        in_loop: bool,
    ) -> Block<'ctx> {
        let span = b.span;
        self.push_scope();
        let stmts = b
            .stmts
            .into_iter()
            .map(|s| self.visit_stmt(s, ret_ty, in_loop))
            .collect();
        let ret = self.visit_expr(b.ret, ret_ty, in_loop);
        self.pop_scope().expect("should be able to pop scope here");
        Block { stmts, ret, span }
    }

    /// Type-checks a function.
    fn visit_fun(&mut self, mut f: ast::Fun<'ctx>) -> Fun<'ctx> {
        let stm = self.current_stratum();

        self.push_scope();
        for &ty_param in &f.ty_params {
            // Trivial substitution.
            self.generic_types.insert(ty_param, ());
        }

        // Introduce arguments in the typing context
        for arg in &mut f.params {
            self.add_var(*arg);
        }

        let body = self.visit_block(f.body, f.ret_ty, false);
        let body_ty = &body.ret.ty;
        if body_ty != &f.ret_ty {
            self.ctx.emit(
                Diagnostic::error()
                    .with_code(TYPE_MISMATCH_ERROR)
                    .with_message(format!(
                        "expected type {:?}, found type {:?}",
                        f.ret_ty.subst(self.ctx.arena, &self.subst),
                        body_ty.subst(self.ctx.arena, &self.subst)
                    ))
                    .with_labels(vec![
                        body.ret.span.as_label().with_message(format!(
                            "body returns a value of type {}",
                            body_ty.subst(self.ctx.arena, &self.subst)
                        )),
                        f.ret_ty.span.as_secondary_label().with_message(format!(
                            "function declared to return type {:?}",
                            f.ret_ty.subst(self.ctx.arena, &self.subst)
                        )),
                    ]),
            );
        }

        self.pop_scope().expect("should be able to pop scope here");

        Fun {
            name: f.name,
            ty_params: f.ty_params,
            params: f
                .params
                .into_iter()
                .map(|param| FunParam::with_stratum(param, stm))
                .collect(),
            ret_ty: f.ret_ty,
            body,
            span: f.span,
        }
    }

    /// Type-checks a program.
    fn visit_program(&mut self, p: ast::Program<'ctx>) -> Program<'ctx> {
        let ast::Program {
            funs,
            structs,
            enums,
        } = p;

        // Add all function signatures to the context to allow for (mutual) recursion.
        for f in &funs {
            self.add_fun(f.signature());
        }

        // Add all valid type names in the context, so they may be referenced
        // everywhere.
        for strukt in &structs {
            let name = strukt.name;
            // Insert and check for redefinition at the same time
            if self
                .subst
                .insert(TyVar::Named(name), StructT(name))
                .is_some()
            {
                self.ctx.emit(
                    Diagnostic::error()
                        .with_code(ITEM_REDEFINED_ERROR)
                        .with_message(format!("{name} is defined several times"))
                        .with_labels(vec![strukt.span.as_label().with_message("redefined here")]),
                );
            }
        }

        for enun in &enums {
            let name = enun.name;
            // Insert and check for redefinition at the same time
            if self.subst.insert(TyVar::Named(name), EnumT(name)).is_some() {
                self.ctx.emit(
                    Diagnostic::error()
                        .with_code(ITEM_REDEFINED_ERROR)
                        .with_message(format!("{name} is defined several times"))
                        .with_labels(vec![enun.span.as_label().with_message("redefined here")]),
                );
            }
        }

        // Note: order is important.
        // We must visit structures first.
        let structs: HashMap<_, _> = structs
            .into_iter()
            .map(|s| (s.name, self.visit_struct(s)))
            .collect();

        let enums: HashMap<_, _> = enums
            .into_iter()
            .map(|e| (e.name, self.visit_enum(e)))
            .collect();

        let funs: HashMap<_, _> = funs
            .into_iter()
            .map(|f| (f.name, self.visit_fun(f)))
            .collect();

        // Check the signature of the main function.
        match funs.get("main") {
            Some(main) => {
                if !main.ret_ty.kind.unify(&UnitT, &mut self.subst)
                    || !main.ty_params.is_empty()
                    || !main.params.is_empty()
                {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(BAD_SIGNATURE)
                            .with_message("wrong signature for the `main` function")
                            .with_labels(vec![main.span.as_label()])
                            .with_notes(vec![
                                "remember: `main` function has no (type) parameters and returns unit"
                                    .to_string(),
                            ]),
                    );
                }
            }
            None => self.ctx.emit(
                Diagnostic::error()
                    .with_code(NO_MAIN_FN_ERROR)
                    .with_message("please add a `main` function to your program"),
            ),
        }

        // Finally, apply type inference substitutions recursively.
        let structs = structs
            .into_iter()
            .map(|(name, s)| (name, s.subst_ty(self.ctx.arena, &self.subst)))
            .collect();
        let enums = enums
            .into_iter()
            .map(|(name, e)| (name, e.subst_ty(self.ctx.arena, &self.subst)))
            .collect();
        let funs: HashMap<_, _> = funs
            .into_iter()
            .map(|(name, f)| (name, f.subst_ty(self.ctx.arena, &self.subst)))
            .collect();

        let p = Program {
            arena: self.ctx.arena,
            structs: self.ctx.alloc(structs),
            enums: self.ctx.alloc(enums),
            funs,
        };

        // Any free variable left is a type inference error
        for var in p.free_ty_vars() {
            match var {
                TyVar::Named(name) => self.ctx.emit(
                    Diagnostic::error()
                        .with_code(UNKNOWN_TYPE_VAR)
                        .with_message(format!("no type named `{name}` in context")),
                ),
                TyVar::Gen(_, span) => self.ctx.emit(
                    Diagnostic::error()
                        .with_code(TYPE_INFER_ERROR)
                        .with_message("could not infer type")
                        .with_labels(vec![span.as_label()])
                        .with_notes(vec![
                            "Please consider adding an explicit type to it".to_string()
                        ]),
                ),
            }
        }

        p
    }

    /// Type-checks a structure.
    fn visit_struct(&mut self, strukt: ast::Struct<'ctx>) -> Struct<'ctx> {
        // TODO: do not return Struct in this function. Nor should we return
        // Fun in `visit_fun`. We should just append them to the context and retrieve
        // them all only at the end, in one go. This would avoid this
        // disgraceful clone.
        self.add_struct(strukt.clone());

        strukt
    }

    /// Type-checks a enumeration.
    fn visit_enum(&mut self, enun: ast::Enum<'ctx>) -> Enum<'ctx> {
        // TODO: do not return Struct in this function. Nor should we return
        // Fun in `visit_fun`. We should just append them to the context and retrieve
        // them all only at the end, in one go. This would avoid this
        // disgraceful clone.
        self.add_enum(enun.clone());

        enun
    }

    /// Visits a pattern.
    ///
    /// * `pat`: pattern to match
    /// * `ty`: expected type of the pattern
    /// * `stm`: expected stratum of the pattern
    fn visit_pattern(
        &mut self,
        pat: ast::Expr<'ctx>,
        ty: Ty<'ctx>,
        stm: Stratum,
    ) -> Option<Pat<'ctx>> {
        let span = pat.span;
        match pat.kind {
            ast::ExprKind::BoolE(i) => Some(Pat::new(BoolM(i), IntT, span)),
            ast::ExprKind::IntegerE(i) => match i.to_u64() {
                Some(i) => Some(Pat::new(IntegerM(i), IntT, span)),
                _ => {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_message("Integer pattern is too big")
                            .with_labels(vec![pat.span.as_label()]),
                    );
                    None
                }
            },
            ast::ExprKind::StringE(s) => Some(Pat::new(StringM(s), IntT, span)),
            ast::ExprKind::PlaceE(place) => {
                if let ast::PlaceKind::VarP(v) = place.kind {
                    Some(Pat::new(
                        IdentM(VarDef::with_stratum(v.as_vardef(ty), stm)),
                        IntT,
                        span,
                    ))
                } else {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(INVALID_PATTERN)
                            .with_message("Not a valid pattern")
                            .with_labels(vec![pat.span.as_label()]),
                    );
                    None
                }
            }
            ast::ExprKind::TupleE(_) => todo!(),
            ast::ExprKind::RangeE(..)
            | ast::ExprKind::StructE { .. }
            | ast::ExprKind::ArrayE(_)
            | ast::ExprKind::MatchE(..)
            | ast::ExprKind::IfE(..)
            | ast::ExprKind::BlockE(..)
            | ast::ExprKind::UnitE => {
                self.ctx.emit(
                    Diagnostic::error()
                        .with_code(INVALID_PATTERN)
                        .with_message("Not a valid pattern")
                        .with_labels(vec![pat.span.as_label()]),
                );
                None
            }
            ast::ExprKind::CallE { name, args } => {
                let mut path = name.path();
                let root = path.next().unwrap(); // Namespaced necessarily starts with something

                if let Some(enun) = self.enum_env.get(&root) && let Some(variant) = path.next()
                && let Some(params) = enun.variants.get(variant) && path.next().is_none() {
                    if args.len() != params.len() {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(ARG_NB_MISMATCH)
                                .with_message(
                                format!("Expected {} argument(s), found {}", params.len(), args.len()))
                                .with_labels(vec![name.span.as_label()])
                                .with_notes(vec![format!("constructor {root}::{variant} expects {} argument(s)", params.len())])
                                .with_labels(vec![enun.span.as_secondary_label().with_message("variant is defined here")]),
                        );
                        return None;
                    }


                    // Compute the pattern for the args
                    let (mut args, old_args): (Vec<_>, _) = (default(), args);
                    for (arg, ast::TyUse { kind: ty, .. }) in old_args.into_iter().zip(params.iter()) {
                        match arg.into_standard() {
                            Ok(arg) => args.push(self.visit_pattern(arg, *ty, stm)?),
                            Err(arg) => {
                                self.ctx.emit(
                                    Diagnostic::error()
                                        .with_code(WRONG_REF_MODE_ERROR)
                                        .with_message("the `@` referencing mode is not allowed in patterns")
                                        .with_labels(vec![arg.span.as_label()])
                                        .with_notes(vec!["help: remove the @ symbol".to_string()])
                                );
                                return None;
                            }
                        }
                    }

                    Some(Pat::new(VariantM { enun: root, variant, args }, EnumT(root), span))
                } else {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(NOT_CALLABLE_ERROR)
                            .with_message(
                                format!("{} is not a callable identifier", root))
                            .with_labels(vec![name.span.as_label()])
                    );
                    None
                }
            }
            ast::ExprKind::NamespacedE(_) => todo!(),
            ast::ExprKind::HoleE => unreachable!(),
        }
    }

    /// Introduces the identifiers in the given pattern into the typing context.
    fn introduce_pat_vars(&mut self, pat: &Pat<'ctx>) {
        match &pat.kind {
            BoolM(_) | IntegerM(_) | StringM(_) => todo!(),
            IdentM(vardef) => self.add_var(*vardef),
            VariantM {
                enun: _,
                variant: _,
                args,
            } => {
                for arg in args {
                    self.introduce_pat_vars(arg);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{config::Config, Arena};

    #[test]
    fn check_pop_option() {
        let arena = Arena::new();
        let config = Config {
            input: "",
            ..default()
        };
        let mut ctx = Context::new(config, &arena);
        let mut typer = Typer::new(&mut ctx);
        assert!(typer.pop_scope().is_err()); // Cannot pop the static stratum

        typer.push_scope();
        assert!(typer.pop_scope().is_ok());

        assert!(typer.pop_scope().is_err()); // Still cannot pop it
    }
}
