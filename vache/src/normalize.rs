//! Typing.

use std::collections::HashMap;
use std::default::default;
use std::marker::PhantomData;

use StmtKind::*;

use crate::anf::*;
use crate::tast;
use crate::Arena;

/// Typed AST to ANF transformer.
pub(crate) struct Normalizer<'mir, 'ctx> {
    /// Phantom reference to the `'mir` lifetime.
    mir_lifetime: PhantomData<&'mir ()>,
    /// Compiler arena.
    arena: &'ctx Arena<'ctx>,
    /// Current stratum.
    stm: Stratum,
    /// Variable translation between pattern identifiers and the places they
    /// actually represent in the MIR.
    ///
    /// Represented as a vec for each different scope.
    var_t9n: Vec<HashMap<Varname<'ctx>, &'ctx Place<'ctx>>>,
}

impl<'mir, 'ctx> Normalizer<'mir, 'ctx> {
    /// Normalize a given program.
    pub fn normalize(p: &'mir mut tast::Program<'ctx>) -> Program<'mir, 'ctx> {
        let tast::Program {
            funs,
            structs,
            enums,
            arena,
        } = p;

        let mut normalizer = Self {
            mir_lifetime: PhantomData,
            arena,
            stm: Stratum::static_stm(),
            var_t9n: vec![default()],
        };

        Program {
            funs: funs
                .iter_mut()
                .map(|(name, f)| (*name, normalizer.visit_fun(f)))
                .collect(),
            structs,
            enums,
        }
    }

    /// Pushes a new scope.
    fn push_scope(&mut self) {
        // Update the stratum
        self.stm = u64::from(self.stm).checked_add(1).unwrap().into();

        // Pushes a new variable translation environment
        self.var_t9n.push(default());
    }

    /// Pops a scope.
    fn pop_scope(&mut self) {
        // Update the stratum
        self.stm = u64::from(self.stm).checked_sub(1).unwrap().into();

        // Pops the variable translation environment
        self.var_t9n
            .pop()
            .expect("there should be always at least one environment");
    }

    /// Creates a fresh variable definition, that is related to some code`span`.
    fn fresh_vardef(&mut self, ty: Ty<'ctx>, span: Span) -> VarDef<'ctx> {
        let var = VarUse::fresh(self.arena, span);

        // Add the trivial translation for that vardef
        self.add_translation(var, self.arena.alloc(Place::from(var)));
        VarDef {
            var,
            ty,
            stm: self.stm,
            span,
        }
    }

    /// Add the translation between a pattern-introduced variable name and the
    /// pointer it actually represents.
    ///
    /// Since there are some identifiers that are patterns representing some
    /// field in the matched expression.
    fn add_translation(&mut self, var: impl Into<Varname<'ctx>>, place: &'ctx Place<'ctx>) {
        let var = var.into();
        self.var_t9n
            .last_mut()
            .expect("there is always at least one translation env")
            .insert(var, place);
    }

    /// Gets the pointer translation of a variable name.
    ///
    /// If that variable is a pattern, we will get the pointer into what the
    /// pattern represents. Otherwise, the variable is a legit variable and
    /// we will get a new pointer into it.
    ///
    /// * Span: span of the use of that pointer/variable. Might be bigger than
    ///   the span of `var`
    /// if we have some mode operator on it, for instance `@`. We want to
    /// capture the span of `@a`, not just `a`.
    fn get_translation_ptr(&self, var: VarUse<'ctx>, span: Span) -> Option<Pointer<'ctx>> {
        let place = self
            .var_t9n
            .iter()
            .rev() // Search through latest env first
            .find_map(|var_t9n| var_t9n.get(&var.name()))
            .copied()?;
        Some(Pointer::new(self.arena, place, span))
    }

    /// Gets the place translation of a variable name.
    ///
    /// If that variable is a pattern, we will get the place to which the
    /// pattern points. Otherwise, the variable is a legit variable and we
    /// will get the actual place of that variable.
    fn get_translation_place(&self, var: impl Into<Varname<'ctx>>) -> Option<Place<'ctx>> {
        let var = var.into();
        self.var_t9n
            .iter()
            .rev() // Search through latest env first
            .find_map(|var_t9n| var_t9n.get(&var))
            .map(|&&place| place)
    }

    /// Visits a pattern `pat` that stands for `ptr` and introduces the
    /// necessary variables for it in the context.
    fn introduce_pat_vars(&mut self, pat: &tast::Pat<'ctx>, ptr: Pointer<'ctx>) {
        match &pat.kind {
            tast::PatKind::BoolM(_) | tast::PatKind::IntegerM(_) | tast::PatKind::StringM(_) => {
                todo!()
            }
            tast::PatKind::IdentM(i) => self.add_translation(i.name(), ptr.place()),
            tast::PatKind::VariantM {
                enun: _,
                variant: _,
                args,
            } => {
                for (i, arg) in args.iter().enumerate() {
                    let field = self.arena.alloc(format!("{}", i + 1));
                    let place = self.arena.alloc(Place::FieldP(ptr, field));
                    let ptr = Pointer::new(self.arena, place, pat.span);
                    self.introduce_pat_vars(arg, ptr);
                }
            }
        }
    }

    /// Visits a (rhs) place. It will append the ANF statements for that
    /// place to `stmts`.
    ///
    /// The arguments to this function are:
    /// * The current list of ANF statements.
    /// * The expression itself (as a node in the typed AST).
    /// * `mode`: overriding mode, if any.
    /// * The pointer to the value that will be returned by the function.
    fn visit_rhs_place<'a>(
        &'a mut self,
        stmts: &'a mut Vec<Stmt<'mir, 'ctx>>,
        place: &'mir mut tast::Place<'ctx>,
        ret_ptr: Pointer<'ctx>,
    ) -> Reference<'mir, 'ctx> {
        match &mut place.kind {
            tast::PlaceKind::VarP(v) => {
                let ptr = self
                    .get_translation_ptr(*v, place.span)
                    .unwrap_or_else(|| panic!("Could not find translation for {v:?}"));
                Reference::new(ptr, &mut place.mode)
            }
            tast::PlaceKind::FieldP(box strukt, field) => {
                let strukt_ptr = self.visit_expr(stmts, strukt, Some(Mode::SMutBorrowed), ret_ptr);
                let final_ptr = Pointer::new(
                    self.arena,
                    self.arena.alloc(Place::FieldP(strukt_ptr.as_ptr(), field)),
                    place.span,
                );

                Reference::new(final_ptr, &mut place.mode)
            }
            tast::PlaceKind::ElemP(box tuple, elem) => {
                let tuple_ptr = self.visit_expr(stmts, tuple, Some(Mode::SMutBorrowed), ret_ptr);

                // Write the element index as a string.
                let field = self.arena.alloc(format!("{elem}"));

                let final_ptr = Pointer::new(
                    self.arena,
                    self.arena.alloc(Place::FieldP(tuple_ptr.as_ptr(), field)),
                    place.span,
                );

                Reference::new(final_ptr, &mut place.mode)
            }
            tast::PlaceKind::IndexP(box e, box ix) => {
                let e_ptr = self.visit_expr(stmts, e, Some(Mode::SMutBorrowed), ret_ptr);
                let ix_ptr = self.visit_expr(stmts, ix, Some(Mode::SBorrowed), ret_ptr);
                let final_ptr = Pointer::new(
                    self.arena,
                    self.arena
                        .alloc(Place::IndexP(e_ptr.as_ptr(), ix_ptr.as_ptr())),
                    place.span,
                );

                Reference::new(final_ptr, &mut place.mode)
            }
        }
    }

    /// Visits a _lhs_ place. It will append the ANF statements for that
    /// place to `stmts`.
    ///
    /// The arguments to this function are:
    /// * The current list of ANF statements.
    /// * The expression itself (as a node in the typed AST).
    /// * `mode`: overriding mode, if any.
    /// * The pointer to the value that will be returned by the function.
    fn visit_lhs_place<'a>(
        &'a mut self,
        stmts: &'a mut Vec<Stmt<'mir, 'ctx>>,
        place: &'mir mut tast::LhsPlace<'ctx>,
        ret_ptr: Pointer<'ctx>,
    ) -> LhsRef<'mir, 'ctx> {
        let lhs = match &mut place.kind {
            tast::PlaceKind::VarP(ref var) => {
                match place.mode {
                    LhsMode::Assigning => self.get_translation_place(*var).expect(""),
                    LhsMode::Declaring => {
                        // Check if the variable already exists, if so create a new binding.
                        let place = if self.get_translation_place(*var).is_some() {
                            let vardef = self.fresh_vardef(place.ty, place.span);
                            Place::VarP(vardef.name())
                        } else {
                            Place::VarP(var.name())
                        };

                        // Add the translation AFTER we computed the rhs, so that the rhs
                        // may still refer to the old value
                        self.add_translation(*var, self.arena.alloc(place));
                        place
                    }
                }
            }
            tast::PlaceKind::IndexP(box array, box ix) => {
                let array = self.visit_expr(stmts, array, Some(Mode::SMutBorrowed), ret_ptr);

                let ix = self.visit_expr(stmts, ix, Some(Mode::SBorrowed), ret_ptr);

                Place::IndexP(array.as_ptr(), ix.as_ptr())
            }
            tast::PlaceKind::FieldP(box strukt, field) => {
                let strukt = self.visit_expr(stmts, strukt, Some(Mode::SMutBorrowed), ret_ptr);

                Place::FieldP(strukt.as_ptr(), field)
            }
            tast::PlaceKind::ElemP(box tuple, elem) => {
                let tuple = self.visit_expr(stmts, tuple, Some(Mode::SMutBorrowed), ret_ptr);

                // Write the element index as a string.
                let field = self.arena.alloc(format!("{elem}"));

                Place::FieldP(tuple.as_ptr(), field)
            }
        };
        let lhs = Pointer::new(self.arena, self.arena.alloc(lhs), place.span);
        LhsRef::new(lhs, &mut place.mode)
    }

    /// Visits an expression. It will append the ANF statements for that
    /// expression to `stmts`.
    ///
    /// The arguments to this function are:
    /// * The current list of ANF statements.
    /// * The expression itself (as a node in the typed AST).
    /// * `mode`: overriding mode, if any.
    /// * The pointer to the value that will be returned by the function.
    fn visit_expr<'a>(
        &'a mut self,
        stmts: &'a mut Vec<Stmt<'mir, 'ctx>>,
        e: &'mir mut tast::Expr<'ctx>,
        mode: Option<Mode>,
        ret_ptr: Pointer<'ctx>,
    ) -> Reference<'mir, 'ctx> {
        match &mut e.kind {
            tast::ExprKind::UnitE => {
                let vardef = self.fresh_vardef(e.ty, e.span);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()), e.span);
                stmts.push(Stmt::new(AssignS(LhsRef::declare(ptr), RValue::Unit), e.span));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::BoolE(b) => {
                let vardef = self.fresh_vardef(e.ty, e.span);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()), e.span);
                stmts.push(Stmt::new(AssignS(LhsRef::declare(ptr), RValue::Bool(*b)), e.span));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::IntegerE(i) => {
                let vardef = self.fresh_vardef(e.ty, e.span);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()), e.span);
                stmts.push(Stmt::new(AssignS(LhsRef::declare(ptr), RValue::Integer(i)), e.span));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::StringE(s) => {
                let vardef = self.fresh_vardef(e.ty, e.span);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()),  e.span);
                stmts.push(Stmt::new(AssignS(LhsRef::declare(ptr), RValue::String(s)), e.span));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::RangeE(start, end) => {
                let vardef = self.fresh_vardef(e.ty, e.span);
                let start_ptr = self.visit_expr(stmts, start, Some(Mode::Borrowed), ret_ptr);
                let end_ptr = self.visit_expr(stmts, end, Some(Mode::Borrowed), ret_ptr);
                let final_ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()), e.span);
                stmts.push(Stmt::new(AssignS(LhsRef::declare(final_ptr), RValue::Range(start_ptr, end_ptr)), e.span));
                Reference::new_moved(final_ptr)
            }
            tast::ExprKind::PlaceE(place) => {
                // If we requested a specific mode, set it
                if let Some(mode) = mode {
                    place.mode = mode;
                }

                self.visit_rhs_place(stmts, place, ret_ptr)
            }
            tast::ExprKind::CallE { name, args } => {
                let arg_vars = args
                    .iter_mut()
                    .map(|arg| self.visit_arg(stmts, arg, None, ret_ptr))
                    .collect();

                let vardef = self.fresh_vardef(e.ty, e.span);
                let destination = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()), e.span);

                stmts.push(Stmt::new(CallS {
                    name: *name,
                    args: arg_vars,
                    destination: Some(LhsRef::declare(destination)),
                }, e.span));

                Reference::new_moved(destination)
            }
            tast::ExprKind::VariantE { enun, variant, args } => {
                let args = args
                    .iter_mut()
                    .map(|arg| self.visit_expr(stmts, arg, None, ret_ptr))
                    .collect();

                let vardef = self.fresh_vardef(e.ty, e.span);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()), e.span);
                stmts.push(Stmt::new(AssignS(LhsRef::declare(ptr), RValue::Variant { enun, variant, args }), e.span));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::IfE(box cond, box iftrue, box iffalse) => {
                // The switch variable
                let cond = self.visit_expr(stmts, cond, Some(Mode::SBorrowed), ret_ptr);

                // Destination
                let dest_def = self.fresh_vardef(iftrue.ret.ty, e.span);
                let destination = Pointer::new(self.arena, self.arena.alloc(dest_def.name().into()), e.span);

                // Branches
                let span = iftrue.span;
                let (iftrue_ptr, mut iftrue) = self.visit_block(iftrue, ret_ptr);
                iftrue.push(Stmt::new(AssignS(LhsRef::declare(destination), RValue::Place(iftrue_ptr)), span));

                let span = iffalse.span;
                let (iffalse_ptr, mut iffalse) = self.visit_block(iffalse, ret_ptr);
                iffalse.push(Stmt::new(AssignS(LhsRef::declare(destination), RValue::Place(iffalse_ptr)), span));

                // If condition
                stmts.push(Stmt::new(IfS(cond, iftrue, iffalse), e.span));

                Reference::new_moved(destination)
            }
            tast::ExprKind::MatchE(box matched, _branches) => {
                match matched.ty {
                    Ty::UnitT => todo!(),
                    Ty::BoolT => todo!(),
                    Ty::IntT => todo!(),
                    Ty::StrT => todo!(),
                    Ty::EnumT(_) => {
                        /*// The switch variable
                        let matched = self.visit_expr(stmts, matched, Some(Mode::SBorrowed), ret_ptr);
                        let dest_def = self.fresh_vardef(e.ty, e.span);
                        let destination = Pointer::new(self.arena, self.arena.alloc(dest_def.name().into()), e.span);

                        // Visit the branches, compute the discriminant
                        let branches: HashMap<Branch, _> = branches.iter_mut().map(|(pattern, expr)| {
                            let mut branch_stmts = vec![];
                            self.push_scope();
                            self.introduce_pat_vars(pattern, matched.as_ptr());
                            self.visit_expr(&mut branch_stmts,expr,  Some(Mode::Moved),ret_ptr);
                            self.pop_scope();
                            (pattern.discriminant(), branch_stmts)
                        }).collect();

                        // Destination
                        stmts.push(Stmt::new(DeclareS(dest_def), e.span));

                        // If condition
                        stmts.push(Stmt::new(MatchS(matched, branches), e.span));

                        Reference::new_moved(destination)*/
                        todo!()
                    },
                    Ty::TupleT(_) => todo!(),
                    _ => unreachable!()
                }
            }
            tast::ExprKind::BlockE(box b) => {
                let span = b.span;
                let (ret, block) = self.visit_block(b, ret_ptr);
                stmts.push(Stmt::new(BlockS(block), span));
                ret
            }
            tast::ExprKind::StructE {
                name: s_name,
                fields,
            } => {
                // Destination
                let dest_def = self.fresh_vardef(e.ty, e.span);
                let destination = Pointer::new(self.arena, self.arena.alloc(dest_def.name().into()), e.span);

                let field_vars = fields
                    .iter_mut()
                    .map(|(name, field)| (*name, self.visit_expr(stmts, field, default(), ret_ptr)))
                    .collect();

                stmts.push(Stmt::new(AssignS(
                    LhsRef::declare(destination),
                    RValue::Struct {
                        name: s_name,
                        fields: field_vars,
                    },
                ), e.span));

                Reference::new_moved(destination)
            }
            tast::ExprKind::ArrayE(array) => {
                // Destination
                let dest_def = self.fresh_vardef(e.ty, e.span);
                let destination = Pointer::new(self.arena, self.arena.alloc(dest_def.name().into()), e.span);

                // Visit each item in the array
                let array_vars = array
                    .iter_mut()
                    .map(|item| self.visit_expr(stmts, item, default(), ret_ptr))
                    .collect();

                // Finally, assign the array to the destination
                stmts.push(Stmt::new(AssignS(LhsRef::declare(destination), RValue::Array(array_vars)), e.span));

                Reference::new_moved(destination)
            }
            tast::ExprKind::TupleE(items) => {
                // Destination
                let dest_def = self.fresh_vardef(e.ty, e.span);
                let destination = Pointer::new(self.arena, self.arena.alloc(dest_def.name().into()), e.span);

                // Visit each item in the tuple
                let items_vars = items
                    .iter_mut()
                    .map(|item| self.visit_expr(stmts, item, default(), ret_ptr))
                    .collect();

                // Finally, assign the tuple to the destination
                stmts.push(Stmt::new(AssignS(LhsRef::declare(destination), RValue::Tuple(items_vars)), e.span));

                Reference::new_moved(destination)
            }
            tast::ExprKind::HoleE => panic!("Cannot compile code with holes; your code probably went through normalization even if it did not typecheck"),
        }
    }

    /// Visits a function argument. It It will add the nodes for that
    /// argument in the CFG, and return the CFG label for it.
    ///
    /// The arguments to this function are:
    /// * The current list of ANF statements.
    /// * The function argument to visit.
    /// * The destination variable that will store the result of this
    ///   expression.
    /// * `mode`: Overriding mode, if any.
    /// * The pointer to the value that will be returned by the function.
    fn visit_arg<'a>(
        &'a mut self,
        stmts: &'a mut Vec<Stmt<'mir, 'ctx>>,
        arg: &'mir mut tast::Arg<'ctx>,
        mode: Option<Mode>,
        ret_ptr: Pointer<'ctx>,
    ) -> Arg<'mir, 'ctx> {
        match &mut arg.kind {
            tast::ArgKind::Standard(e) => Arg::Standard(self.visit_expr(stmts, e, mode, ret_ptr)),
            tast::ArgKind::InPlace(p) => Arg::InPlace(self.visit_rhs_place(stmts, p, ret_ptr)),
            tast::ArgKind::Binding(e, p) => {
                let e = self.visit_expr(stmts, e, mode, ret_ptr);
                let p = self.visit_lhs_place(stmts, p, ret_ptr);
                Arg::Binding(e, p)
            }
        }
    }

    /// Visits a block. It will return the ANF'ed block and the reference to the
    /// final value of the block.
    ///
    /// Takes as arguments:
    /// * The block itself (as a node of the typed AST).
    /// * The pointer to the value that will be returned by the function.
    fn visit_block(
        &mut self,
        b: &'mir mut tast::Block<'ctx>,
        ret_ptr: Pointer<'ctx>,
    ) -> (Reference<'mir, 'ctx>, Block<'mir, 'ctx>) {
        let mut stmts: Block = default();
        self.push_scope();
        for stmt in &mut b.stmts {
            self.visit_stmt(stmt, &mut stmts, ret_ptr);
        }
        let reference = self.visit_expr(&mut stmts, &mut b.ret, Some(Mode::Moved), ret_ptr);
        self.pop_scope();
        (reference, stmts)
    }

    /// Visits a statement, appending the ANF'ed statements to `stmts`.
    ///
    /// Takes as arguments:
    /// * The statement to visit.
    /// * The list of ANF statements to append to.
    /// * The pointer to the value that will be returned by the function.
    fn visit_stmt<'a>(
        &'a mut self,
        s: &'mir mut tast::Stmt<'ctx>,
        stmts: &'a mut Vec<Stmt<'mir, 'ctx>>,
        ret_ptr: Pointer<'ctx>,
    ) {
        match &mut s.kind {
            tast::StmtKind::AssignS(place, expr) => {
                let rhs = self.visit_expr(stmts, expr, None, ret_ptr);
                let lhs_ref = self.visit_lhs_place(stmts, place, ret_ptr);
                stmts.push(Stmt::new(AssignS(lhs_ref, RValue::Place(rhs)), s.span));
            }
            tast::StmtKind::SwapS(place1, place2) => {
                let place1 = self.visit_rhs_place(stmts, place1, ret_ptr);
                let place2 = self.visit_rhs_place(stmts, place2, ret_ptr);
                stmts.push(Stmt::new(SwapS(place1, place2), s.span));
            }
            tast::StmtKind::WhileS { cond, body } => {
                let mut cond_block = vec![];
                self.push_scope();
                let cond = self.visit_expr(&mut cond_block, cond, Some(Mode::Borrowed), ret_ptr);
                self.pop_scope();

                let (_, body) = self.visit_block(body, ret_ptr);

                stmts.push(Stmt::new(
                    WhileS {
                        cond_block,
                        cond,
                        body,
                    },
                    s.span,
                ));
            }
            tast::StmtKind::ForS { .. } => {
                /*
                HUGE TODO TODO TODO TODO TODO TODO
                we are skipping loan analysis here!!!

                let cond_block: Vec<Stmt> = default();

                self.push_scope();
                block.push(DeclareS(*item));
                let iter = self.visit_expr(&mut block, iter, default(), structs);
                let (_, body) = self.visit_block(body, structs);
                block.push(Stmt::While {
                    cond_block,
                    cond,
                    body,
                });
                self.pop_scope();*/
            }
            tast::StmtKind::ExprS(e) => {
                self.visit_expr(stmts, e, Some(Mode::Moved), ret_ptr);
            }
            tast::StmtKind::HoleS => panic!("Normalization should not be run on a code with holes"),
            tast::StmtKind::BreakS => stmts.push(Stmt::new(BreakS, s.span)),
            tast::StmtKind::ContinueS => stmts.push(Stmt::new(ContinueS, s.span)),
            tast::StmtKind::ReturnS(ret) => {
                let stmts: &'a mut std::vec::Vec<Stmt<'mir, 'ctx>> = stmts;
                let ret: Reference<'mir, 'ctx> =
                    self.visit_expr(stmts, ret, Some(Mode::Moved), ret_ptr);
                stmts.push(Stmt::new(
                    AssignS(LhsRef::declare(ret_ptr), RValue::Place(ret)),
                    s.span,
                ));
                stmts.push(Stmt::new(ReturnS, s.span));
            }
        }
    }

    /// Visits a function.
    fn visit_fun(&mut self, f: &'mir mut tast::Fun<'ctx>) -> Fun<'mir, 'ctx> {
        let vardef = self.fresh_vardef(f.ret_ty.kind, f.body.ret.span);
        let ret_ptr = Pointer::new(
            self.arena,
            self.arena.alloc(vardef.name().into()),
            f.body.ret.span,
        );

        // Body scope
        self.push_scope();
        for &param in &f.params {
            self.add_translation(param, self.arena.alloc(Place::from(param)));
        }

        let body_ret_span = f.body.ret.span;

        // Compute the body
        let (ret, mut body) = self.visit_block(&mut f.body, ret_ptr);

        // Move the result of the body into the final return value
        body.push(Stmt::new(
            AssignS(LhsRef::declare(ret_ptr), RValue::Place(ret)),
            body_ret_span,
        ));

        self.pop_scope();

        Fun {
            name: f.name,
            params: f.params.clone(),
            ret_v: Some(ret_ptr),
            body,
        }
    }
}
