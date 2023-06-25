//! Typing.

use std::collections::HashMap;
use std::default::default;

use itertools::Itertools;
use num_traits::ToPrimitive;
use ExprKind::*;
use PatKind::*;
use PlaceKind::*;
use StmtKind::*;
use Ty::*;

use crate::ast::fun::{binop_bool_sig, binop_int_sig, unop_bool_sig};
use crate::codes::*;
use crate::reporter::Diagnostic;
use crate::tast::*;
use crate::utils::{boxed, keys_match};
use crate::{ast, Context};

/// A typing environment.
///
/// Contains definitions for variables and functions.
struct Env<'ctx> {
    /// Map between vars and their definitions.
    var_env: HashMap<Varname<'ctx>, ast::VarDef<'ctx>>,
}

impl<'ctx> Env<'ctx> {
    /// Creates a new, empty environment.
    fn new() -> Self {
        Self {
            var_env: HashMap::new(),
        }
    }

    /// Gets the definition of a variable.
    fn get_var(&self, v: impl AsRef<Varname<'ctx>>) -> Option<&ast::VarDef<'ctx>> {
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
        self.var_env.insert(vardef.name(), vardef);
    }
}

impl Default for Env<'_> {
    fn default() -> Self {
        Self::new()
    }
}

/// Kind of type name.
///
/// In the source code, we will find plenty of type names, that can stand for
/// different things. Here we list all the different kinds of names we can have.
pub enum TypeNameKind {
    /// Name of a struct.
    Struct,
    /// Name of an enum.
    Enum,
}

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
    /// Map between valid type names and their kind (structure, variants, etc.).
    /// Initialized at the very beginning, allows to have
    /// mutually-referencing structures.
    valid_type_names: HashMap<&'ctx str, TypeNameKind>,
    /// The typing environment stack.
    env: Vec<Env<'ctx>>,
}

impl<'t, 'ctx> Typer<'t, 'ctx> {
    /// Creates a new typer.
    pub fn new(ctx: &'t mut Context<'ctx>) -> Self {
        let mut typer = Self {
            ctx,
            fun_env: default(),
            struct_env: default(),
            enum_env: default(),
            valid_type_names: default(),
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
        typer.add_fun(binop_bool_sig("&&", BoolT));
        typer.add_fun(binop_bool_sig("||", BoolT));
        typer.add_fun(unop_bool_sig("!", BoolT));

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
    fn get_var(&self, v: impl AsRef<Varname<'ctx>>) -> Option<(&ast::VarDef<'ctx>, Stratum)> {
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

    /// Declares a new function in the context.
    fn add_fun(&mut self, fun_def: impl Into<ast::FunSig<'ctx>>) {
        let fun_def = fun_def.into();
        self.fun_env.insert(fun_def.name, self.ctx.alloc(fun_def));
    }

    /// Declares a new structure in the context.
    fn add_struct(&mut self, strukt: impl Into<Struct<'ctx>>) {
        let strukt = strukt.into();
        self.struct_env.insert(strukt.name, self.ctx.alloc(strukt));
    }

    /// Declares a new enum in the context.
    fn add_enum(&mut self, enun: impl Into<Enum<'ctx>>) {
        let enun = enun.into();
        self.enum_env.insert(enun.name, self.ctx.alloc(enun));
    }

    /// Returns the current stratum/scope id.
    fn current_stratum(&self) -> Stratum {
        (self.env.len() - 1).try_into().unwrap()
    }

    /// Checks that `ty` is well defined in the environment.
    ///
    /// In particular, unknown structure names will raise an error.
    ///
    /// Can eventually refine the type given in argument (note: will change with
    /// type inference).
    fn check_ty(&mut self, ty: &mut ast::TyUse<'ctx>) {
        match &mut ty.kind {
            UnitT | BoolT | IntT | StrT | HoleT => (),
            ArrayT(item) => self.check_ty(&mut item.with_span(ty.span)),
            TupleT(items) => {
                for item in items.iter() {
                    self.check_ty(&mut item.with_span(ty.span));
                }
            }
            IterT(item) => self.check_ty(&mut item.with_span(ty.span)),
            VarT(name) => match self.valid_type_names.get(name) {
                Some(TypeNameKind::Struct) => {
                    ty.kind = StructT(name);
                }
                Some(TypeNameKind::Enum) => {
                    ty.kind = EnumT(name);
                }
                None => self.ctx.emit(
                    Diagnostic::error()
                        .with_code(UNKNOWN_TYPE_VAR)
                        .with_message(format!("no type named `{name}` in context"))
                        .with_labels(vec![ty.span.into()]),
                ),
            },
            StructT(_) => unreachable!(),
            EnumT(_) => unreachable!(),
        }
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
    ) -> Option<Place<'ctx>> {
        match place.kind {
            ast::PlaceKind::VarP(var) => {
                if let Some((vardef, stm)) = self.get_var(var) {
                    Some(Place::var(var, vardef.ty, stm, Mode::Assigning, place.span))
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
                match (e.ty.as_array(), ix.ty.is_int()) {
                    (Some(ty), true) => {
                        let e_stm = e.stm; // Needed now because we move e after
                        Some(Place {
                            kind: IndexP(boxed(e), boxed(ix)),
                            ty,
                            stm: e_stm,
                            mode: Mode::Assigning,
                            span: place.span,
                        })
                    }
                    (None, true) => {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(TYPE_MISMATCH_ERROR)
                                .with_message(format!("Expected array type, found type {}", e.ty))
                                .with_labels(vec![e.span.as_label()])
                                .with_notes(vec!["Only array can be indexed".to_string()]),
                        );
                        None
                    }
                    (_, false) => {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(TYPE_MISMATCH_ERROR)
                                .with_message(format!("Expected type int, found type {}", ix.ty))
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

                        Some(Place {
                            stm: s.stm,
                            kind: FieldP(boxed(s), field),
                            mode: Mode::Assigning,
                            ty,
                            span: place.span,
                        })
                    }
                    HoleT => Some(Place {
                        stm: s.stm,
                        kind: FieldP(boxed(s), field),
                        mode: Mode::Assigning,
                        ty: HoleT,
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

                        Some(Place {
                            stm: tuple.stm,
                            kind: ElemP(boxed(tuple), elem),
                            mode: Mode::Assigning,
                            ty,
                            span: place.span,
                        })
                    }
                    HoleT => Some(Place {
                        stm: tuple.stm,
                        kind: ElemP(boxed(tuple), elem),
                        mode: Mode::Assigning,
                        ty: HoleT,
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

    /// Type-checks a call on a callable object.
    ///
    /// * `namespaced`: element being called
    /// * `args`: arguments to the call
    /// * `span`: span of the entire call location
    pub fn visit_call(
        &mut self,
        namespaced: Namespaced<'ctx>,
        args: Vec<Expr<'ctx>>,
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
            for (
                i,
                (
                    Expr {
                        ty: arg_ty, span, ..
                    },
                    ast::TyUse { kind: param_ty, .. },
                ),
            ) in args.iter().zip(params.iter()).enumerate()
            {
                if arg_ty != param_ty {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(TYPE_MISMATCH_ERROR)
                            .with_message(format!(
                                "expected type {param_ty}, found type {arg_ty}"
                            ))
                            .with_labels(vec![span.as_label()])
                            .with_notes(vec![format!(
                                "{param_ty} is the expected type for argument #{} of `{}`",
                                i + 1,
                                root
                            )]),
                    );
                }
            }

            Expr::new(VariantE { enun: root, variant, args }, EnumT(enun.name), self.current_stratum(), span)
        } else if let Some(fun) = self.fun_env.get(root) {
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
                    Expr {
                        ty: arg_ty, span, ..
                    },
                    ast::VarDef { ty: param_ty, .. },
                ),
            ) in args.iter().zip(fun.params.iter()).enumerate()
            {
                if arg_ty != param_ty {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(TYPE_MISMATCH_ERROR)
                            .with_message(format!(
                                "expected type {param_ty}, found type {arg_ty}"
                            ))
                            .with_labels(vec![span.as_label()])
                            .with_notes(vec![format!(
                                "{param_ty} is the expected type for argument #{} of `{}`",
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
            ast::ExprKind::PlaceE(place) => match place.kind {
                ast::PlaceKind::VarP(v) => match self.get_var(v) {
                    Some((vardef, stm)) => Expr::new(
                        PlaceE(Place::var(v, vardef.ty, stm, place.mode, place.span)),
                        vardef.ty,
                        stm,
                        span,
                    ),
                    None => {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(UNKNOWN_VAR_ERROR)
                                .with_message(format!("Unknown variable `{v}`"))
                                .with_labels(vec![v.as_span().into()]),
                        );
                        Expr::hole(span)
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
                                            strukt
                                                .fields
                                                .keys()
                                                .map(|x| format!("`{x}`"))
                                                .join(", ")
                                        )]),
                                );
                                HoleT
                            }
                        };
                        let stm = s.stm; // Needed now because we move s after
                        Expr::new(
                            PlaceE(Place {
                                kind: FieldP(boxed(s), field),
                                mode: place.mode,
                                ty,
                                stm,
                                span: place.span,
                            }),
                            ty,
                            stm,
                            span,
                        )
                    } else {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(FIELD_NOT_STRUCT_ERROR)
                                .with_message("Field access only works for structures")
                                .with_labels(vec![s.span.as_label().with_message("has no fields")]),
                        );
                        Expr::hole(span)
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
                                HoleT
                            }
                        };
                        let stm = tuple.stm; // Needed now because we move `tuple` after
                        Expr::new(
                            PlaceE(Place {
                                kind: ElemP(boxed(tuple), elem),
                                mode: place.mode,
                                ty,
                                stm,
                                span: place.span,
                            }),
                            ty,
                            stm,
                            span,
                        )
                    } else {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_message("This kind of indexing only works for tuples")
                                .with_labels(vec![tuple
                                    .span
                                    .as_label()
                                    .with_message("has no elements")]),
                        );
                        Expr::hole(span)
                    }
                }
                ast::PlaceKind::IndexP(box e, box ix) => {
                    let e = self.visit_expr(e, ret_ty, in_loop);
                    let ix = self.visit_expr(ix, ret_ty, in_loop);
                    match (e.ty.as_array(), ix.ty.is_int()) {
                        (Some(ty), true) => {
                            let e_stm = e.stm; // Needed now because we move e after
                            Expr::new(
                                PlaceE(Place {
                                    kind: IndexP(boxed(e), boxed(ix)),
                                    mode: place.mode,
                                    ty,
                                    stm: e_stm,
                                    span: place.span,
                                }),
                                ty,
                                e_stm,
                                span,
                            )
                        }
                        (None, true) => {
                            self.ctx.emit(
                                Diagnostic::error()
                                    .with_code(TYPE_MISMATCH_ERROR)
                                    .with_message(format!(
                                        "Expected array type, found type {}",
                                        e.ty
                                    ))
                                    .with_labels(vec![e.span.as_label()])
                                    .with_notes(vec!["Only array can be indexed".to_string()]),
                            );
                            Expr::hole(span)
                        }
                        (_, false) => {
                            self.ctx.emit(
                                Diagnostic::error()
                                    .with_code(TYPE_MISMATCH_ERROR)
                                    .with_message(format!(
                                        "Expected type int, found type {}",
                                        ix.ty
                                    ))
                                    .with_labels(vec![ix.span.as_label()])
                                    .with_notes(vec![
                                        "Only integer indexing is supported for arrays".to_string(),
                                    ]),
                            );
                            Expr::hole(span)
                        }
                    }
                }
            },
            // Make a special case for `print` until we get generic functions so that we
            // can express `print` more elegantly with the other builtin functions.
            ast::ExprKind::CallE { name, args } if name.name == "print" => {
                let args: Vec<Expr> = args
                    .into_iter()
                    .map(|arg| self.visit_expr(arg, ret_ty, in_loop))
                    .collect();

                Expr::new(CallE { name, args }, UnitT, self.current_stratum(), span)
            }
            ast::ExprKind::CallE { name, args } => {
                let args: Vec<Expr> = args
                    .into_iter()
                    .map(|arg| self.visit_expr(arg, ret_ty, in_loop))
                    .collect();
                self.visit_call(name, args, span)
            }
            ast::ExprKind::IfE(box cond, box iftrue, box iffalse) => {
                let cond = self.visit_expr(cond, ret_ty, in_loop);
                let iftrue = self.visit_block(iftrue, ret_ty, in_loop);
                let iffalse = self.visit_block(iffalse, ret_ty, in_loop);

                // Condition must be a bool
                if !cond.ty.is_bool() {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(TYPE_MISMATCH_ERROR)
                            .with_message(format!("expected bool, found {}", cond.ty))
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
                            .with_labels(vec![iftrue
                                .span
                                .as_label()
                                .with_message(format!("has type {}", iftrue.ret.ty))])
                            .with_labels(vec![iffalse
                                .span
                                .as_label()
                                .with_message(format!("has type {}", iffalse.ret.ty))]),
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

                let strukt = &self.struct_env[s_name];

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
                        let expected = strukt.get_field(fname).unwrap(); // Ok because we checked just before our declaration matches the structure
                                                                         // fields.
                        if expected != *ty {
                            self.ctx.emit(
                                Diagnostic::error()
                                    .with_code(TYPE_MISMATCH_ERROR)
                                    .with_message(format!(
                                        "expected type {expected}, found type {ty}"
                                    ))
                                    .with_labels(vec![span.as_label()])
                                    .with_notes(vec![format!(
                                        "field `{fname}` of `{s_name}` is of type {ty}"
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
                if array.is_empty() {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(EMPTY_LIST_ERROR)
                            .with_message("empty arrays are not supported yet")
                            .with_labels(vec![span.into()]),
                    );
                    return Expr::hole(span);
                }

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

                // Check that all items have the same type and compute the final type
                let ty = ArrayT(self.ctx.alloc(array[0].ty));
                if !array.iter().all(|item| item.ty == array[0].ty) {
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
                if !start.ty.is_int() {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(TYPE_MISMATCH_ERROR)
                            .with_message(format!("expected type int, found type {}", start.ty))
                            .with_labels(vec![start.span.as_label()])
                            .with_notes(vec!["ranges only work for integers".to_string()]),
                    );
                }
                if !end.ty.is_int() {
                    self.ctx.emit(
                        Diagnostic::error()
                            .with_code(TYPE_MISMATCH_ERROR)
                            .with_message(format!("expected type int, found type {}", end.ty))
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
                                    pat.ty, matched.ty
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
                    self.pop_scope().unwrap();
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
                                        ty, expr.ty
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
                    HoleT
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
        }
    }

    /// Type-checks a statement.
    ///
    /// * `ret_ty`: Return type of the function.
    /// * `in_loop`: is this statement in a loop?
    fn visit_stmt(&mut self, s: ast::Stmt<'ctx>, ret_ty: TyUse<'ctx>, in_loop: bool) -> Stmt<'ctx> {
        let res: Option<Stmt> = try {
            match s.kind {
                ast::StmtKind::DeclareS(mut vardef, expr) => {
                    let rhs_span = expr.span;
                    let stm = self.current_stratum();
                    let expr = self.visit_expr(expr, ret_ty, in_loop);
                    let expr_ty = &expr.ty;

                    // Check type declaration.
                    self.check_ty(&mut vardef.ty);

                    // Check the type
                    if &vardef.ty != expr_ty {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(TYPE_MISMATCH_ERROR)
                                .with_message("Left and right hand side have incompatible types")
                                .with_labels(vec![rhs_span.as_label().with_message(format!(
                                    "expected type {}, found type {expr_ty}",
                                    vardef.ty
                                ))]),
                        );
                    }

                    self.add_var(vardef);

                    DeclareS(VarDef::with_stratum(vardef, stm), expr).with_span(s.span)
                }
                ast::StmtKind::AssignS(place, expr) => {
                    let rhs_span = expr.span;
                    let expr = self.visit_expr(expr, ret_ty, in_loop);
                    let expr_ty = &expr.ty;
                    let place = self.visit_lhs_place(place, ret_ty, in_loop)?;

                    // Check the type
                    if &place.ty != expr_ty {
                        self.ctx.emit(
                            Diagnostic::error()
                                .with_code(TYPE_MISMATCH_ERROR)
                                .with_message("Left and right hand side have incompatible types")
                                .with_labels(vec![rhs_span.as_label().with_message(format!(
                                    "expected type {}, found type {expr_ty}",
                                    place.ty
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
                                .with_message("expected type bool, found type int")
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

                    let item_ty = match iter.ty.as_iter() {
                        Some(item_ty) => item_ty,
                        None => {
                            self.ctx.emit(
                                Diagnostic::error()
                                    .with_code(TYPE_MISMATCH_ERROR)
                                    .with_message(format!(
                                        "Expected iterator, found type {}",
                                        iter.ty
                                    ))
                                    .with_labels(vec![iter.span.as_label()])
                                    .with_notes(vec![
                                        "For loop requires an iterator here".to_string()
                                    ]),
                            );
                            HoleT
                        }
                    };
                    let item = item.with_type(item_ty);
                    //
                    // Introduce a new intermediate scope, in which `item` is defined`
                    self.push_scope();

                    self.add_var(item);
                    let item = VarDef::with_stratum(item, stm);

                    let body = self.visit_block(body, ret_ty, true);

                    // Pop the intermediate scope
                    self.pop_scope();

                    if body.ret.ty != UnitT {
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
                    ExprS(self.visit_expr(e, ret_ty, in_loop)).with_span(s.span)
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
                                    ret_ty.kind, ret.ty
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
        self.pop_scope().unwrap();
        Block { stmts, ret, span }
    }

    /// Type-checks a function.
    fn visit_fun(&mut self, mut f: ast::Fun<'ctx>) -> Fun<'ctx> {
        let stm = self.current_stratum();

        self.push_scope();
        // Introduce arguments in the typing context
        for arg in &mut f.params {
            self.check_ty(&mut arg.ty);
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
                        f.ret_ty, body_ty
                    ))
                    .with_labels(vec![
                        body.ret
                            .span
                            .as_label()
                            .with_message(format!("body returns a value of type {}", body_ty)),
                        f.ret_ty
                            .span
                            .as_secondary_label()
                            .with_message(format!("function declared to return {:?}", f.ret_ty)),
                    ]),
            );
        }

        self.pop_scope();

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
                .valid_type_names
                .insert(name, TypeNameKind::Struct)
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
            if self
                .valid_type_names
                .insert(name, TypeNameKind::Enum)
                .is_some()
            {
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
        let structs = structs
            .into_iter()
            .map(|s| (s.name, self.visit_struct(s)))
            .collect();

        let enums = enums
            .into_iter()
            .map(|e| (e.name, self.visit_enum(e)))
            .collect();

        Program {
            arena: self.ctx.arena,
            structs: self.ctx.alloc(structs),
            enums: self.ctx.alloc(enums),
            funs: funs
                .into_iter()
                .map(|f| (f.name, self.visit_fun(f)))
                .collect(),
        }
    }

    /// Type-checks a structure.
    fn visit_struct(&mut self, mut strukt: ast::Struct<'ctx>) -> Struct<'ctx> {
        // Check that all types in the structure exist.
        for ty in strukt.fields.values_mut() {
            self.check_ty(ty);
        }

        // TODO: do not return Struct in this function. Nor should we return
        // Fun in `visit_fun`. We should just append them to the context and retrieve
        // them all only at the end, in one go. This would avoid this
        // disgraceful clone.
        self.add_struct(strukt.clone());

        strukt
    }

    /// Type-checks a enumeration.
    fn visit_enum(&mut self, mut enun: ast::Enum<'ctx>) -> Enum<'ctx> {
        // Check that all types in the enum exist.
        for args in enun.variants.values_mut() {
            for arg in args {
                self.check_ty(arg);
            }
        }

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
                        IdentM(VarDef::with_stratum(v.with_type(ty), stm)),
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
                        args.push(self.visit_pattern(arg, *ty, stm)?);
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
        }
    }

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
        assert_eq!(typer.pop_scope(), None); // Cannot pop the static stratum

        typer.push_scope();
        assert!(typer.pop_scope().is_some());

        assert_eq!(typer.pop_scope(), None); // Still cannot pop it
    }
}
