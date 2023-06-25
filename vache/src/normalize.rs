//! Typing.

use std::collections::HashMap;
use std::default::default;

use StmtKind::*;

use crate::anf::*;
use crate::tast;
use crate::Arena;

/// Typed AST to ANF transformer.
pub(crate) struct Normalizer<'ctx> {
    /// Compiler arena.
    arena: &'ctx Arena,
    /// Current stratum.
    stm: Stratum,
    /// Variable translation between pattern identifiers and the places they
    /// actually represent in the MIR.
    ///
    /// Represented as a vec for each different scope.
    var_t9n: Vec<HashMap<Varname<'ctx>, &'ctx Place<'ctx>>>,
}

impl<'ctx> Normalizer<'ctx> {
    /// Normalize a given program.
    pub fn normalize<'tast>(p: &'ctx mut tast::Program<'tast>) -> Program<'ctx> {
        let tast::Program {
            funs,
            structs,
            enums,
            arena,
        } = p;

        let mut normalizer = Self {
            arena: *arena,
            stm: Stratum::static_stm(),
            var_t9n: vec![default()],
        };

        Program {
            funs: funs
                .iter_mut()
                .map(|(name, f)| (*name, normalizer.visit_fun(f)))
                .collect(),
            arena,
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
    fn get_translation_ptr(&self, var: VarUse<'ctx>) -> Option<Pointer<'ctx>> {
        let place = self
            .var_t9n
            .iter()
            .rev() // Search through latest env first
            .find_map(|var_t9n| var_t9n.get(&var.name()))
            .copied()?;
        Some(Pointer::new(self.arena, place, var.as_span()))
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

    /// Visits an expression. It It will add the nodes for that
    /// block in the CFG, and return the CFG label for it.
    ///
    /// It takes as arguments:
    /// * The expression itself (as a parser AST node).
    /// * The destination variable that will store the result of this
    ///   expression.
    /// * `mode`: Overriding mode, if any.
    /// * The map of structures declarations.
    fn visit_expr<'tast>(
        &mut self,
        stmts: &mut Vec<Stmt<'ctx>>,
        e: &'ctx mut tast::Expr<'tast>,
        mode: Option<Mode>,
        ret_ptr: Pointer<'ctx>,
    ) -> Reference<'ctx> {
        match &mut e.kind {
            tast::ExprKind::UnitE => {
                let vardef = self.fresh_vardef(e.ty, e.span);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()), e.span);
                stmts.push(Stmt::new(DeclareS(vardef), e.span));
                stmts.push(Stmt::new(AssignS(ptr, RValue::Unit), e.span));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::BoolE(b) => {
                let vardef = self.fresh_vardef(e.ty, e.span);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()), e.span);
                stmts.push(Stmt::new(DeclareS(vardef), e.span));
                stmts.push(Stmt::new(AssignS(ptr, RValue::Bool(*b)), e.span));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::IntegerE(i) => {
                let vardef = self.fresh_vardef(e.ty, e.span);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()), e.span);
                stmts.push(Stmt::new(DeclareS(vardef), e.span));
                stmts.push(Stmt::new(AssignS(ptr, RValue::Integer(i)), e.span));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::StringE(s) => {
                let vardef = self.fresh_vardef(e.ty, e.span);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()),  e.span);
                stmts.push(Stmt::new(DeclareS(vardef), e.span));
                stmts.push(Stmt::new(AssignS(ptr, RValue::String(s)), e.span));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::RangeE(start, end) => {
                let vardef = self.fresh_vardef(e.ty, e.span);
                let start_ptr = self.visit_expr(stmts, start, Some(Mode::Borrowed), ret_ptr);
                let end_ptr = self.visit_expr(stmts, end, Some(Mode::Borrowed), ret_ptr);
                let final_ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()), e.span);
                stmts.push(Stmt::new(DeclareS(vardef), e.span));
                stmts.push(Stmt::new(AssignS(final_ptr, RValue::Range(start_ptr, end_ptr)), e.span));
                Reference::new_moved(final_ptr)
            }
            tast::ExprKind::PlaceE(place) => {
                // If we requested a specific mode, set it
                if let Some(mode) = mode {
                    place.mode = mode;
                }

                match &mut place.kind {
                    tast::PlaceKind::VarP(v) => {
                        let ptr = self.get_translation_ptr(*v).unwrap_or_else(|| panic!("Could not find translation for {v:?}"));
                        Reference::new(ptr, &mut place.mode)
                    }
                    tast::PlaceKind::FieldP(box strukt, field) => {
                        let strukt_ptr = self.visit_expr(stmts, strukt, Some(Mode::Moved), ret_ptr);
                        let final_ptr = Pointer::new(
                            self.arena,
                            self.arena.alloc(Place::FieldP(strukt_ptr.as_ptr(), field)),
                            place.span
                        );

                        // The reference into `FieldP` will depend on multiple modes: the global one,
                        // and the modes of the strukt expression.
                        let mut modes = vec![&mut place.mode];
                        modes.extend(strukt_ptr.into_modes_mut());

                        Reference::new_multi_modes(final_ptr, modes)
                    }
                    tast::PlaceKind::ElemP(box tuple, elem) => {
                        let tuple_ptr = self.visit_expr(stmts, tuple, Some(Mode::Moved), ret_ptr);

                        // Write the element index as a string.
                        let field = self.arena.alloc(format!("{elem}"));

                        let final_ptr = Pointer::new(
                            self.arena,
                            self.arena.alloc(Place::FieldP(tuple_ptr.as_ptr(), field)),
                            place.span
                        );

                        // The reference into `ElemP` will depend on multiple modes: the global one,
                        // and the modes of the tuple expression.
                        let mut modes = vec![&mut place.mode];
                        modes.extend(tuple_ptr.into_modes_mut());

                        Reference::new_multi_modes(final_ptr, modes)
                    }
                    tast::PlaceKind::IndexP(box e, box ix) => {
                        let e_ptr = self.visit_expr(stmts, e, Some(Mode::SMutBorrowed), ret_ptr);
                        let ix_ptr = self.visit_expr(stmts, ix, Some(Mode::SBorrowed), ret_ptr);
                        let final_ptr = Pointer::new(
                            self.arena,
                            self.arena
                                .alloc(Place::IndexP(e_ptr.as_ptr(), ix_ptr.as_ptr())),
                            place.span
                        );

                        // The reference into `IndexP` will depend on multiple modes: the global one,
                        // the modes of the array expression, and the modes of the index expression.
                        // All are tied.
                        let mut modes = vec![&mut place.mode];
                        modes.extend(e_ptr.into_modes_mut());
                        modes.extend(ix_ptr.into_modes_mut());

                        Reference::new_multi_modes(final_ptr, modes)
                    }
                }
            }
            tast::ExprKind::CallE { name, args } => {
                let arg_vars = args
                    .iter_mut()
                    .map(|arg| self.visit_expr(stmts, arg, None, ret_ptr))
                    .collect();

                let vardef = self.fresh_vardef(e.ty, e.span);
                let destination = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()), e.span);
                stmts.push(Stmt::new(DeclareS(vardef), e.span));

                stmts.push(Stmt::new(CallS {
                    name: *name,
                    args: arg_vars,
                    destination: Some(destination),
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
                stmts.push(Stmt::new(DeclareS(vardef), e.span));
                stmts.push(Stmt::new(AssignS(ptr, RValue::Variant { enun, variant, args }), e.span));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::IfE(box cond, box iftrue, box iffalse) => {
                // The switch variable
                let cond = self.visit_expr(stmts, cond, Some(Mode::SBorrowed), ret_ptr);

                // Destination
                let dest_def = self.fresh_vardef(iftrue.ret.ty, e.span);
                let destination = Pointer::new(self.arena, self.arena.alloc(dest_def.name().into()), e.span);
                stmts.push(Stmt::new(DeclareS(dest_def), e.span));

                // Branches
                let span = iftrue.span;
                let (iftrue_ptr, mut iftrue) = self.visit_block(iftrue, ret_ptr);
                iftrue.push(Stmt::new(AssignS(destination, RValue::Place(iftrue_ptr)), span));

                let span = iffalse.span;
                let (iffalse_ptr, mut iffalse) = self.visit_block(iffalse, ret_ptr);
                iffalse.push(Stmt::new(AssignS(destination, RValue::Place(iffalse_ptr)), span));

                // If condition
                stmts.push(Stmt::new(IfS(cond, iftrue, iffalse), e.span));

                Reference::new_moved(destination)
            }
            tast::ExprKind::MatchE(box matched, branches) => {
                match matched.ty {
                    Ty::UnitT => todo!(),
                    Ty::BoolT => todo!(),
                    Ty::IntT => todo!(),
                    Ty::StrT => todo!(),
                    Ty::EnumT(_) => {
                        // The switch variable
                        let matched = self.visit_expr(stmts, matched, Some(Mode::SBorrowed), ret_ptr);
                        let dest_def = self.fresh_vardef(e.ty, e.span);
                        let destination = Pointer::new(self.arena, self.arena.alloc(dest_def.name().into()), e.span);

                        // Visit the branches, compute the discriminant
                        let branches: HashMap<Branch, _> = branches.iter_mut().map(|(pattern, expr)| {
                            let mut branch_stmts = vec![];
                            self.push_scope();
                            self.introduce_pat_vars(pattern, matched.as_ptr());
                            self.visit_expr(&mut branch_stmts,expr,  Some(Mode::Moved),destination);
                            self.pop_scope();
                            (pattern.discriminant(), branch_stmts)
                        }).collect();

                        // Destination
                        stmts.push(Stmt::new(DeclareS(dest_def), e.span));

                        // If condition
                        stmts.push(Stmt::new(MatchS(matched, branches), e.span));

                        Reference::new_moved(destination)
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
                stmts.push(Stmt::new(DeclareS(dest_def), e.span));

                let field_vars = fields
                    .iter_mut()
                    .map(|(name, field)| (*name, self.visit_expr(stmts, field, default(), ret_ptr)))
                    .collect();

                stmts.push(Stmt::new(AssignS(
                    destination,
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
                stmts.push(Stmt::new(DeclareS(dest_def), e.span));

                // Visit each item in the array
                let array_vars = array
                    .iter_mut()
                    .map(|item| self.visit_expr(stmts, item, default(), ret_ptr))
                    .collect();

                // Finally, assign the array to the destination
                stmts.push(Stmt::new(AssignS(destination, RValue::Array(array_vars)), e.span));

                Reference::new_moved(destination)
            }
            tast::ExprKind::TupleE(items) => {
                // Destination
                let dest_def = self.fresh_vardef(e.ty, e.span);
                let destination = Pointer::new(self.arena, self.arena.alloc(dest_def.name().into()), e.span);
                stmts.push(Stmt::new(DeclareS(dest_def), e.span));

                // Visit each item in the tuple
                let items_vars = items
                    .iter_mut()
                    .map(|item| self.visit_expr(stmts, item, default(), ret_ptr))
                    .collect();

                // Finally, assign the tuple to the destination
                stmts.push(Stmt::new(AssignS(destination, RValue::Tuple(items_vars)), e.span));

                Reference::new_moved(destination)
            }
            tast::ExprKind::HoleE => panic!("Cannot compile code with holes; your code probably went through normalization even if it did not typecheck"),
        }
    }

    /// Visits a block. It It will add the nodes for that
    /// block in the CFG, and return the (entry) CFG label for it.
    ///
    /// Takes as arguments:
    /// * The block itself (as a parser AST node)
    /// * The map of structures declarations.
    /// * The _function_ return pointer.
    fn visit_block<'tast>(
        &mut self,
        b: &'ctx mut tast::Block<'tast>,
        ret_ptr: Pointer<'ctx>,
    ) -> (Reference<'ctx>, Block<'ctx>) {
        let mut stmts: Block = default();
        self.push_scope();
        for stmt in &mut b.stmts {
            self.visit_stmt(stmt, &mut stmts, ret_ptr);
        }
        let reference = self.visit_expr(&mut stmts, &mut b.ret, Some(Mode::Moved), ret_ptr);
        self.pop_scope();
        (reference, stmts)
    }

    /// Visits a statements, producing the CFG nodes and returning the label for
    /// the entry CFG node for that statement.
    ///
    /// Takes as arguments:
    /// * The statement to visit.
    /// * The label to jump at in the CFG after the execution of the statement.
    /// * A map of structures declarations.
    fn visit_stmt<'tast>(
        &mut self,
        s: &'ctx mut tast::Stmt<'tast>,
        stmts: &mut Vec<Stmt<'ctx>>,
        ret_ptr: Pointer<'ctx>,
    ) {
        match &mut s.kind {
            tast::StmtKind::DeclareS(old_vardef, expr) => {
                // Check if the variable already exists, if so create a new binding.
                let vardef = if self.get_translation_place(*old_vardef).is_some() {
                    self.fresh_vardef(old_vardef.ty, old_vardef.span)
                } else {
                    *old_vardef
                };
                stmts.push(Stmt::new(DeclareS(vardef), s.span));
                let tmp = self.visit_expr(stmts, expr, None, ret_ptr);

                // Add the translation AFTER we computed the rhs, so that the rhs may still
                // refer to the old value
                self.add_translation(*old_vardef, self.arena.alloc(Place::from(vardef)));

                stmts.push(Stmt::new(
                    AssignS(
                        Pointer::new(
                            self.arena,
                            self.arena.alloc(Place::VarP(vardef.name())),
                            old_vardef.span,
                        ),
                        RValue::Place(tmp),
                    ),
                    s.span,
                ));
            }
            tast::StmtKind::AssignS(place, expr) => {
                let rhs = self.visit_expr(stmts, expr, None, ret_ptr);
                let lhs = match &mut place.kind {
                    tast::PlaceKind::VarP(ref var) => self.get_translation_place(*var).expect(""),
                    tast::PlaceKind::IndexP(box array, box ix) => {
                        let array =
                            self.visit_expr(stmts, array, Some(Mode::SMutBorrowed), ret_ptr);

                        let ix = self.visit_expr(stmts, ix, Some(Mode::SBorrowed), ret_ptr);

                        Place::IndexP(array.as_ptr(), ix.as_ptr())
                    }
                    tast::PlaceKind::FieldP(box strukt, field) => {
                        let strukt =
                            self.visit_expr(stmts, strukt, Some(Mode::SMutBorrowed), ret_ptr);

                        Place::FieldP(strukt.as_ptr(), field)
                    }
                    tast::PlaceKind::ElemP(box tuple, elem) => {
                        let tuple =
                            self.visit_expr(stmts, tuple, Some(Mode::SMutBorrowed), ret_ptr);

                        // Write the element index as a string.
                        let field = self.arena.alloc(format!("{elem}"));

                        Place::FieldP(tuple.as_ptr(), field)
                    }
                };
                let lhs = Pointer::new(self.arena, self.arena.alloc(lhs), place.span);
                stmts.push(Stmt::new(AssignS(lhs, RValue::Place(rhs)), s.span));
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
                let ret = self.visit_expr(stmts, ret, Some(Mode::Moved), ret_ptr);
                stmts.push(Stmt::new(AssignS(ret_ptr, RValue::Place(ret)), s.span));
                stmts.push(Stmt::new(ReturnS(ret_ptr), s.span));
            }
        }
    }

    /// Visits a function.
    fn visit_fun<'tast>(&mut self, f: &'ctx mut tast::Fun<'tast>) -> Fun<'ctx> {
        let vardef = self.fresh_vardef(f.ret_ty.kind, f.body.ret.span);
        let ret_ptr = Pointer::new(
            self.arena,
            self.arena.alloc(vardef.name().into()),
            f.body.ret.span,
        );

        // Declare the return variable
        let mut body = vec![Stmt::new(DeclareS(vardef), vardef.span)];

        // Body scope
        self.push_scope();
        for &param in &f.params {
            self.add_translation(param, self.arena.alloc(Place::from(param)));
        }

        let body_ret_span = f.body.ret.span;

        // Compute the body
        let (ret, inner_body) = self.visit_block(&mut f.body, ret_ptr);
        body.extend(inner_body);

        // Move the result of the body into the final return value
        body.push(Stmt::new(
            AssignS(ret_ptr, RValue::Place(ret)),
            body_ret_span,
        ));
        body.push(Stmt::new(ReturnS(ret_ptr), body_ret_span));

        self.pop_scope();

        Fun {
            name: f.name,
            params: f.params.clone(),
            ret_v: Some(ret_ptr),
            body,
        }
    }
}
