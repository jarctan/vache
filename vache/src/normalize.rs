//! Typing.

use std::collections::HashMap;
use std::default::default;

use Stmt::*;

use crate::anf::*;
use crate::tast;
use crate::Arena;

/// Typed AST to ANF transformer.
pub(crate) struct Normalizer<'ctx> {
    /// Compiler arena.
    arena: &'ctx Arena,
    /// Current stratum.
    stm: Stratum,
}

impl<'ctx> Normalizer<'ctx> {
    /// Creates a new MIR processor.
    pub fn new(arena: &'ctx Arena) -> Self {
        Self {
            arena,
            stm: Stratum::static_stm(),
        }
    }

    /// Normalize a given program.
    pub fn normalize<'tast>(&mut self, p: &'ctx mut tast::Program<'tast>) -> Program<'ctx> {
        self.visit_program(p)
    }

    /// Pushes a scope, updating the current stratum id consequently.
    fn push_scope(&mut self) {
        self.stm = u64::from(self.stm).checked_add(1).unwrap().into();
    }

    /// Pops a scope, updating the current stratum id consequently.
    fn pop_scope(&mut self) {
        self.stm = u64::from(self.stm).checked_sub(1).unwrap().into();
    }

    /// Creates a fresh variable definition, that is related to some code`span`.
    fn fresh_vardef(&self, ty: Ty<'ctx>, span: Span) -> VarDef<'ctx> {
        let var = VarUse::fresh(self.arena, span);
        VarDef {
            var,
            ty,
            stm: self.stm,
            span,
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
        structs: &HashMap<&'ctx str, Struct<'ctx>>,
        ret_ptr: Pointer<'ctx>,
    ) -> Reference<'ctx> {
        match &mut e.kind {
            tast::ExprKind::UnitE => {
                let vardef = self.fresh_vardef(e.ty, e.span);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()));
                stmts.push(DeclareS(vardef));
                stmts.push(AssignS(ptr, RValue::Unit));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::BoolE(b) => {
                let vardef = self.fresh_vardef(e.ty, e.span);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()));
                stmts.push(DeclareS(vardef));
                stmts.push(AssignS(ptr, RValue::Bool(*b)));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::IntegerE(i) => {
                let vardef = self.fresh_vardef(e.ty, e.span);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()));
                stmts.push(DeclareS(vardef));
                stmts.push(AssignS(ptr, RValue::Integer(i)));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::StringE(s) => {
                let vardef = self.fresh_vardef(e.ty, e.span);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()));
                stmts.push(DeclareS(vardef));
                stmts.push(AssignS(ptr, RValue::String(s)));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::RangeE(start, end) => {
                let vardef = self.fresh_vardef(e.ty, e.span);
                let start_ptr = self.visit_expr(stmts, start, Some(Mode::Borrowed), structs, ret_ptr);
                let end_ptr = self.visit_expr(stmts, end, Some(Mode::Borrowed), structs, ret_ptr);
                let final_ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()));
                stmts.push(DeclareS(vardef));
                stmts.push(AssignS(final_ptr, RValue::Range(start_ptr, end_ptr)));
                Reference::new_moved(final_ptr)
            }
            tast::ExprKind::PlaceE(place) => {
                // If we requested a specific mode, set it
                if let Some(mode) = mode {
                    place.mode = mode;
                }

                match &mut place.kind {
                    tast::PlaceKind::VarP(v) => {
                        let ptr = Pointer::new(self.arena, self.arena.alloc(Place::from(*v)));
                        Reference::new(ptr, &mut place.mode)
                    }
                    tast::PlaceKind::FieldP(box strukt, field) => {
                        let strukt_ptr = self.visit_expr(stmts, strukt, Some(Mode::Moved), structs, ret_ptr);
                        let final_ptr = Pointer::new(
                            self.arena,
                            self.arena.alloc(Place::FieldP(strukt_ptr.as_ptr(), field)),
                        );

                        // The reference into `FieldP` will depend on multiple modes: the global one,
                        // and the modes of the strukt expression.
                        let mut modes = vec![&mut place.mode];
                        modes.extend(strukt_ptr.into_modes_mut());

                        Reference::new_multi_modes(final_ptr, modes)
                    }
                    tast::PlaceKind::ElemP(box tuple, elem) => {
                        let tuple_ptr = self.visit_expr(stmts, tuple, Some(Mode::Moved), structs, ret_ptr);

                        // Write the element index as a string.
                        let field = self.arena.alloc(format!("{elem}"));

                        let final_ptr = Pointer::new(
                            self.arena,
                            self.arena.alloc(Place::FieldP(tuple_ptr.as_ptr(), field)),
                        );

                        // The reference into `ElemP` will depend on multiple modes: the global one,
                        // and the modes of the tuple expression.
                        let mut modes = vec![&mut place.mode];
                        modes.extend(tuple_ptr.into_modes_mut());

                        Reference::new_multi_modes(final_ptr, modes)
                    }
                    tast::PlaceKind::IndexP(box e, box ix) => {
                        let e_ptr = self.visit_expr(stmts, e, Some(Mode::SMutBorrowed), structs, ret_ptr);
                        let ix_ptr = self.visit_expr(stmts, ix, Some(Mode::SBorrowed), structs, ret_ptr);
                        let final_ptr = Pointer::new(
                            self.arena,
                            self.arena
                                .alloc(Place::IndexP(e_ptr.as_ptr(), ix_ptr.as_ptr())),
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
                    .map(|arg| self.visit_expr(stmts, arg, None, structs, ret_ptr))
                    .collect();

                let vardef = self.fresh_vardef(e.ty, e.span);
                let destination = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()));
                stmts.push(DeclareS(vardef));

                stmts.push(CallS {
                    name: *name,
                    args: arg_vars,
                    destination: Some(destination),
                });

                Reference::new_moved(destination)
            }
            tast::ExprKind::VariantE { enun, variant, args } => {
                let args = args
                    .iter_mut()
                    .map(|arg| self.visit_expr(stmts, arg, None, structs, ret_ptr))
                    .collect();

                let vardef = self.fresh_vardef(e.ty, e.span);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()));
                stmts.push(DeclareS(vardef));
                stmts.push(AssignS(ptr, RValue::Variant { enun, variant, args }));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::IfE(box cond, box iftrue, box iffalse) => {
                // The switch variable
                let cond = self.visit_expr(stmts, cond, Some(Mode::SBorrowed), structs, ret_ptr);

                // Destination
                let dest_def = self.fresh_vardef(iftrue.ret.ty, e.span);
                let destination = Pointer::new(self.arena, self.arena.alloc(dest_def.name().into()));
                stmts.push(DeclareS(dest_def));

                // Branches
                let (iftrue_ptr, mut iftrue) = self.visit_block(iftrue, structs, ret_ptr);
                iftrue.push(AssignS(destination, RValue::Place(iftrue_ptr)));

                let (iffalse_ptr, mut iffalse) = self.visit_block(iffalse, structs, ret_ptr);
                iffalse.push(AssignS(destination, RValue::Place(iffalse_ptr)));

                // If condition
                stmts.push(IfS(cond, iftrue, iffalse));

                Reference::new_moved(destination)
            }
            tast::ExprKind::BlockE(box b) => {
                let (ret, block) = self.visit_block(b, structs, ret_ptr);
                stmts.push(BlockS(block));
                ret
            }
            tast::ExprKind::StructE {
                name: s_name,
                fields,
            } => {
                // Destination
                let dest_def = self.fresh_vardef(e.ty, e.span);
                let destination = Pointer::new(self.arena, self.arena.alloc(dest_def.name().into()));
                stmts.push(DeclareS(dest_def));

                let field_vars = fields
                    .iter_mut()
                    .map(|(name, field)| (*name, self.visit_expr(stmts, field, default(), structs, ret_ptr)))
                    .collect();

                stmts.push(AssignS(
                    destination,
                    RValue::Struct {
                        name: s_name,
                        fields: field_vars,
                    },
                ));

                Reference::new_moved(destination)
            }
            tast::ExprKind::ArrayE(array) => {
                // Destination
                let dest_def = self.fresh_vardef(e.ty, e.span);
                let destination = Pointer::new(self.arena, self.arena.alloc(dest_def.name().into()));
                stmts.push(DeclareS(dest_def));

                // Visit each item in the array
                let array_vars = array
                    .iter_mut()
                    .map(|item| self.visit_expr(stmts, item, default(), structs, ret_ptr))
                    .collect();

                // Finally, assign the array to the destination
                stmts.push(AssignS(destination, RValue::Array(array_vars)));

                Reference::new_moved(destination)
            }
            tast::ExprKind::TupleE(items) => {
                // Destination
                let dest_def = self.fresh_vardef(e.ty, e.span);
                let destination = Pointer::new(self.arena, self.arena.alloc(dest_def.name().into()));
                stmts.push(DeclareS(dest_def));

                // Visit each item in the tuple
                let items_vars = items
                    .iter_mut()
                    .map(|item| self.visit_expr(stmts, item, default(), structs, ret_ptr))
                    .collect();

                // Finally, assign the tuple to the destination
                stmts.push(AssignS(destination, RValue::Tuple(items_vars)));

                Reference::new_moved(destination)
            }
            tast::ExprKind::HoleE => panic!("Cannot compile code with holes; your code probably went through normalization even if it did not typecheck"),
        }
    }

    /// Visits a block. It It will add the nodes for that
    /// block in the CFG, and return the (entry) CFG label for it.
    ///
    /// Takes as arguments:
    /// * The expression itself (as a parser AST node)
    /// * The destination variable that will store the result of this expression
    /// * The map of structures declarations.
    fn visit_block<'tast>(
        &mut self,
        b: &'ctx mut tast::Block<'tast>,
        structs: &HashMap<&'ctx str, Struct<'ctx>>,
        ret_ptr: Pointer<'ctx>,
    ) -> (Reference<'ctx>, Block<'ctx>) {
        let mut stmts: Block = default();
        self.push_scope();
        for stmt in &mut b.stmts {
            self.visit_stmt(stmt, &mut stmts, structs, ret_ptr);
        }
        self.pop_scope();
        let reference =
            self.visit_expr(&mut stmts, &mut b.ret, Some(Mode::Moved), structs, ret_ptr);
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
        structs: &HashMap<&'ctx str, Struct<'ctx>>,
        ret_ptr: Pointer<'ctx>,
    ) {
        match s {
            tast::Stmt::DeclareS(vardef, expr) => {
                stmts.push(DeclareS(*vardef));
                let tmp = self.visit_expr(stmts, expr, None, structs, ret_ptr);
                stmts.push(AssignS(
                    Pointer::new(self.arena, self.arena.alloc(Place::VarP(vardef.name()))),
                    RValue::Place(tmp),
                ));
            }
            tast::Stmt::AssignS(place, expr) => {
                let rhs = self.visit_expr(stmts, expr, None, structs, ret_ptr);
                let lhs = match &mut place.kind {
                    tast::PlaceKind::VarP(ref var) => var.into(),
                    tast::PlaceKind::IndexP(box array, box ix) => {
                        let array = self.visit_expr(
                            stmts,
                            array,
                            Some(Mode::SMutBorrowed),
                            structs,
                            ret_ptr,
                        );

                        let ix =
                            self.visit_expr(stmts, ix, Some(Mode::SBorrowed), structs, ret_ptr);

                        Place::IndexP(array.as_ptr(), ix.as_ptr())
                    }
                    tast::PlaceKind::FieldP(box strukt, field) => {
                        let strukt = self.visit_expr(
                            stmts,
                            strukt,
                            Some(Mode::SMutBorrowed),
                            structs,
                            ret_ptr,
                        );

                        Place::FieldP(strukt.as_ptr(), field)
                    }
                    tast::PlaceKind::ElemP(box tuple, elem) => {
                        let tuple = self.visit_expr(
                            stmts,
                            tuple,
                            Some(Mode::SMutBorrowed),
                            structs,
                            ret_ptr,
                        );

                        // Write the element index as a string.
                        let field = self.arena.alloc(format!("{elem}"));

                        Place::FieldP(tuple.as_ptr(), field)
                    }
                };
                let lhs = Pointer::new(self.arena, self.arena.alloc(lhs));
                stmts.push(AssignS(lhs, RValue::Place(rhs)));
            }
            tast::Stmt::WhileS { cond, body } => {
                let mut cond_block = vec![];
                self.push_scope();
                let cond = self.visit_expr(
                    &mut cond_block,
                    cond,
                    Some(Mode::Borrowed),
                    structs,
                    ret_ptr,
                );
                self.pop_scope();

                let (_, body) = self.visit_block(body, structs, ret_ptr);

                stmts.push(WhileS {
                    cond_block,
                    cond,
                    body,
                });
            }
            tast::Stmt::ForS { .. } => {
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
            tast::Stmt::ExprS(e) => {
                self.visit_expr(stmts, e, Some(Mode::Moved), structs, ret_ptr);
            }
            tast::Stmt::HoleS => panic!("Normalization should not be run on a code with holes"),
            tast::Stmt::BreakS => stmts.push(BreakS),
            tast::Stmt::ContinueS => stmts.push(ContinueS),
            tast::Stmt::ReturnS(ret) => {
                let ret = self.visit_expr(stmts, ret, Some(Mode::Moved), structs, ret_ptr);
                stmts.push(AssignS(ret_ptr, RValue::Place(ret)));
                stmts.push(ReturnS(ret_ptr));
            }
        }
    }

    /// Visits a function.
    fn visit_fun<'tast>(
        &mut self,
        f: &'ctx mut tast::Fun<'tast>,
        structs: &HashMap<&'ctx str, Struct<'ctx>>,
    ) -> Fun<'ctx> {
        let vardef = self.fresh_vardef(f.ret_ty.kind, f.body.ret.span);
        let ret_ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()));

        // Declare the return variable
        let mut body = vec![DeclareS(vardef)];

        // Compute the body
        let (ret, inner_body) = self.visit_block(&mut f.body, structs, ret_ptr);
        body.extend(inner_body);

        // Move the result of the body into the final return value
        body.push(AssignS(ret_ptr, RValue::Place(ret)));
        body.push(ReturnS(ret_ptr));

        Fun {
            name: f.name,
            params: f.params.clone(),
            ret_v: Some(ret_ptr),
            body,
        }
    }

    /// Visits a program, recursively producing CFGs for each function.
    fn visit_program<'tast>(&mut self, p: &'ctx mut tast::Program<'tast>) -> Program<'ctx> {
        let tast::Program {
            funs,
            structs,
            enums,
            arena,
        } = p;

        // Note: order is important.
        // We must visit structures first.
        Program {
            funs: funs
                .iter_mut()
                .map(|(name, f)| (*name, self.visit_fun(f, structs)))
                .collect(),
            arena,
            structs,
            enums,
        }
    }
}
