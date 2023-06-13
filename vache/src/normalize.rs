//! Typing.

use std::collections::HashMap;
use std::default::default;

use Ty::*;

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

    /// Creates a fresh variable definition.
    fn fresh_vardef(&self, ty: Ty<'ctx>) -> VarDef<'ctx> {
        let var = VarUse::fresh(self.arena); // TODO: associate with the right codespan.
        VarDef {
            var,
            ty,
            stm: self.stm,
            span: default(), // TODO: associate with the right codespan.
        }
    }

    /// Visits an expression. It It will add the nodes for that
    /// block in the CFG, and return the CFG label for it.
    ///
    /// It takes as arguments:
    /// * The expression itself (as a parser AST node).
    /// * The destination variable that will store the result of this
    ///   expression.
    /// * `mode`: Preferred mode.
    /// * The map of structures declarations.
    fn visit_expr<'tast>(
        &mut self,
        stmts: &mut Vec<Stmt<'ctx>>,
        e: &'ctx mut tast::Expr<'tast>,
        mode: Mode,
        structs: &HashMap<&'ctx str, Struct<'ctx>>,
    ) -> Reference<'ctx> {
        match &mut e.kind {
            tast::ExprKind::UnitE => {
                let vardef = self.fresh_vardef(e.ty);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()));
                stmts.push(Stmt::Declare(vardef));
                stmts.push(Stmt::Assign(ptr, RValue::Unit));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::IntegerE(i) => {
                let vardef = self.fresh_vardef(e.ty);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()));
                stmts.push(Stmt::Declare(vardef));
                stmts.push(Stmt::Assign(ptr, RValue::Integer(i)));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::StringE(s) => {
                let vardef = self.fresh_vardef(e.ty);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()));
                stmts.push(Stmt::Declare(vardef));
                stmts.push(Stmt::Assign(ptr, RValue::String(s)));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::RangeE(start, end) => {
                let vardef = self.fresh_vardef(e.ty);
                let start_ptr = self.visit_expr(stmts, start, Mode::SBorrowed, structs);
                let end_ptr = self.visit_expr(stmts, end, Mode::MutBorrowed, structs);
                let final_ptr = Pointer::new(
                    self.arena,
                    self.arena
                        .alloc(Place::IndexP(start_ptr.as_ptr(), end_ptr.as_ptr())),
                );
                stmts.push(Stmt::Declare(vardef));
                stmts.push(Stmt::Assign(final_ptr, RValue::Range(start_ptr, end_ptr)));
                Reference::new_moved(final_ptr)
            }
            tast::ExprKind::PlaceE(place) => {
                place.mode = mode;
                match &mut place.kind {
                    tast::PlaceKind::VarP(v) => {
                        let ptr = Pointer::new(self.arena, self.arena.alloc(Place::from(*v)));
                        Reference::new(ptr, &mut place.mode)
                    }
                    tast::PlaceKind::FieldP(box strukt, field) => {
                        let strukt_ptr = self.visit_expr(stmts, strukt, Mode::Moved, structs);
                        let final_ptr = Pointer::new(
                            self.arena,
                            self.arena.alloc(Place::FieldP(strukt_ptr.as_ptr(), field)),
                        );
                        Reference::new(final_ptr, &mut place.mode)
                    }
                    tast::PlaceKind::IndexP(box e, box ix) => {
                        let e_ptr = self.visit_expr(stmts, e, Mode::MutBorrowed, structs);
                        let ix_ptr = self.visit_expr(stmts, ix, Mode::SBorrowed, structs);
                        let final_ptr = Pointer::new(
                            self.arena,
                            self.arena
                                .alloc(Place::IndexP(e_ptr.as_ptr(), ix_ptr.as_ptr())),
                        );
                        Reference::new(final_ptr, &mut place.mode)
                    }
                }
            }
            tast::ExprKind::CallE { name, args } => {
                let arg_vars = args
                    .iter_mut()
                    .map(|arg| self.visit_expr(stmts, arg, default(), structs))
                    .collect();

                let vardef = self.fresh_vardef(e.ty);
                let destination = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()));
                stmts.push(Stmt::Declare(vardef));

                stmts.push(Stmt::Call {
                    name: *name,
                    args: arg_vars,
                    destination: Some(destination),
                });

                Reference::new_moved(destination)
            }
            tast::ExprKind::VariantE { enun, variant, args } => {
                let args = args
                    .iter_mut()
                    .map(|arg| self.visit_expr(stmts, arg, default(), structs))
                    .collect();

                let vardef = self.fresh_vardef(e.ty);
                let ptr = Pointer::new(self.arena, self.arena.alloc(vardef.name().into()));
                stmts.push(Stmt::Declare(vardef));
                stmts.push(Stmt::Assign(ptr, RValue::Variant { enun, variant, args }));
                Reference::new_moved(ptr)
            }
            tast::ExprKind::IfE(box cond, box iftrue, box iffalse) => {
                // The switch variable
                let cond = self.visit_expr(stmts, cond, Mode::SBorrowed, structs);

                // Destination
                let dest_def = self.fresh_vardef(iftrue.ret.ty);
                let destination = Pointer::new(self.arena, self.arena.alloc(dest_def.name().into()));
                stmts.push(Stmt::Declare(dest_def));

                // Branches
                let (iftrue_ptr, mut iftrue) = self.visit_block(iftrue, structs);
                iftrue.push(Stmt::Assign(destination, RValue::Place(iftrue_ptr)));

                let (iffalse_ptr, mut iffalse) = self.visit_block(iffalse, structs);
                iffalse.push(Stmt::Assign(destination, RValue::Place(iffalse_ptr)));

                // If condition
                stmts.push(Stmt::If(cond, iftrue, iffalse));

                Reference::new_moved(destination)
            }
            tast::ExprKind::BlockE(box b) => {
                let (ret, block) = self.visit_block(b, structs);
                stmts.push(Stmt::Block(block));
                ret
            }
            tast::ExprKind::StructE {
                name: s_name,
                fields,
            } => {
                // Destination
                let dest_def = self.fresh_vardef(Ty::StructT(s_name));
                let destination = Pointer::new(self.arena, self.arena.alloc(dest_def.name().into()));
                stmts.push(Stmt::Declare(dest_def));

                let field_vars = fields
                    .iter_mut()
                    .map(|(name, field)| (*name, self.visit_expr(stmts, field, default(), structs)))
                    .collect();

                stmts.push(Stmt::Assign(
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
                let dest_def = self.fresh_vardef(array[0].ty);
                let destination = Pointer::new(self.arena, self.arena.alloc(dest_def.name().into()));
                stmts.push(Stmt::Declare(dest_def));

                let array_vars = array
                    .iter_mut()
                    .map(|item| self.visit_expr(stmts, item, default(), structs))
                    .collect();

                stmts.push(Stmt::Assign(destination, RValue::Array(array_vars)));

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
    ) -> (Reference<'ctx>, Block<'ctx>) {
        let mut stmts: Block = default();
        self.push_scope();
        for stmt in &mut b.stmts {
            self.visit_stmt(stmt, &mut stmts, structs);
        }
        self.pop_scope();
        let reference = self.visit_expr(&mut stmts, &mut b.ret, Mode::Cloned, structs);
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
    ) {
        match s {
            tast::Stmt::DeclareS(vardef, expr) => {
                stmts.push(Stmt::Declare(*vardef));
                let tmp = self.visit_expr(stmts, expr, default(), structs);
                stmts.push(Stmt::Assign(
                    Pointer::new(self.arena, self.arena.alloc(Place::VarP(vardef.name()))),
                    RValue::Place(tmp),
                ));
            }
            tast::Stmt::AssignS(place, expr) => {
                let rhs = self.visit_expr(stmts, expr, default(), structs);
                let lhs = match &mut place.kind {
                    tast::PlaceKind::VarP(ref var) => var.into(),
                    tast::PlaceKind::IndexP(box array, box ix) => {
                        let array = self.visit_expr(stmts, array, Mode::MutBorrowed, structs);

                        let ix = self.visit_expr(stmts, ix, Mode::SBorrowed, structs);

                        Place::IndexP(array.as_ptr(), ix.as_ptr())
                    }
                    tast::PlaceKind::FieldP(box strukt, field) => {
                        let strukt = self.visit_expr(stmts, strukt, Mode::MutBorrowed, structs);

                        Place::FieldP(strukt.as_ptr(), field)
                    }
                };
                let lhs = Pointer::new(self.arena, self.arena.alloc(lhs));
                stmts.push(Stmt::Assign(lhs, RValue::Place(rhs)));
            }
            tast::Stmt::WhileS { cond, body } => {
                let mut cond_block = vec![];
                self.push_scope();
                let cond = self.visit_expr(&mut cond_block, cond, Mode::Borrowed, structs);
                self.pop_scope();

                let (_, body) = self.visit_block(body, structs);

                stmts.push(Stmt::While {
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
                block.push(Stmt::Declare(*item));
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
                self.visit_expr(stmts, e, Mode::Moved, structs);
            }
            tast::Stmt::HoleS => panic!("Normalization should not be run on a code with holes"),
        }
    }

    /// Visits a function.
    fn visit_fun<'tast>(
        &mut self,
        f: &'ctx mut tast::Fun<'tast>,
        structs: &HashMap<&'ctx str, Struct<'ctx>>,
    ) -> Fun<'ctx> {
        let (ret, mut body) = self.visit_block(&mut f.body, structs);
        body.push(Stmt::Return(ret.as_ptr()));

        Fun {
            name: f.name,
            params: f.params.clone(),
            ret_v: Some(ret.as_ptr()),
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
