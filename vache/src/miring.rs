//! CFG production.

use std::collections::HashMap;
use std::default::default;

use Branch::*;

use crate::anf;
use crate::mir::*;
use crate::utils::set::Set;
use crate::Arena;

/// ANF to MIR transformer.
pub(crate) struct MIRer<'mir, 'ctx> {
    /// The WIP CFG for the current function.
    cfg: CfgI<'mir, 'ctx>,
    /// Current stratum.
    stm: Stratum,
    /// Collect the variables in each stratum.
    strata: HashMap<Stratum, Set<Varname<'ctx>>>,
    /// Global arena.
    arena: &'ctx Arena<'ctx>,
}

impl<'mir, 'ctx> MIRer<'mir, 'ctx> {
    /// Creates a new MIR processor.
    pub(crate) fn new(arena: &'ctx Arena<'ctx>) -> Self {
        Self {
            cfg: Cfg::default(),
            stm: Stratum::static_stm(),
            strata: default(),
            arena,
        }
    }

    /// Generates a fresh CFG label, that has never been used in that CFG.
    #[must_use]
    fn fresh_label(&mut self) -> CfgLabel {
        self.cfg.add_node(Instr {
            kind: InstrKind::default(),
            span: default(),
            scope: self.stm,
        })
    }

    /// Generates an instruction with the right stratum.
    fn instr<'s>(&'s self, kind: InstrKind<'mir, 'ctx>, span: Span) -> Instr<'mir, 'ctx> {
        Instr {
            kind,
            span,
            scope: self.stm,
        }
    }

    /// Inserts an instruction in the CFG, returning its label in the graph.
    #[must_use]
    fn insert<'s>(
        &'s mut self,
        instr: Instr<'mir, 'ctx>,
        branches: impl IntoIterator<Item = (Branch<'ctx>, CfgLabel)>,
    ) -> CfgLabel {
        let from = self.cfg.add_node(instr);
        for (b, to) in branches {
            self.cfg.add_edge(from, to, b, ());
        }
        from
    }

    /// Inserts an instruction at a specific label in the CFG.
    ///
    /// # Panics
    /// Panics if the node labeled by that `label` already exists.
    fn insert_at<'s>(
        &'s mut self,
        label: CfgLabel,
        instr: Instr<'mir, 'ctx>,
        branches: impl IntoIterator<Item = (Branch<'ctx>, CfgLabel)>,
    ) {
        self.cfg[&label] = instr;
        for (b, to) in branches {
            self.cfg.add_edge(label, to, b, ());
        }
    }

    /// Pushes a scope, updating the current stratum id consequently.
    fn push_scope(&mut self) {
        self.stm = u64::from(self.stm).checked_add(1).unwrap().into();
    }

    /// Pops a scope, updating the current stratum id consequently.
    fn pop_scope(&mut self) {
        self.stm = u64::from(self.stm).checked_sub(1).unwrap().into();
    }

    /// Computes the MIR output of a typed program using that processor.
    pub fn gen_mir(&mut self, p: anf::Program<'mir, 'ctx>) -> Program<'mir, 'ctx> {
        self.visit_program(p)
    }

    /// Visits a block. It It will add the nodes for that
    /// block in the CFG, and return the (entry) CFG label for it.
    ///
    /// Takes as arguments:
    /// * The expression itself (as a parser AST node)
    /// * The label to jump in the CFG to after evaluating this expression
    /// * The exit label to return from the function
    /// * The break label to break from the current while loop (if any)
    /// * The map of structures declarations.
    fn visit_stmts(
        &mut self,
        b: anf::Block<'mir, 'ctx>,
        dest_l: CfgLabel,
        exit_l: CfgLabel,
        break_l: Option<CfgLabel>,
    ) -> CfgLabel {
        b.into_iter().rev().fold(dest_l, |dest_l, s| {
            self.visit_stmt(s, dest_l, exit_l, break_l)
        })
    }

    /// Visits a statements, producing the CFG nodes and returning the label for
    /// the entry CFG node for that statement.
    ///
    /// Takes as arguments:
    /// * The statement to visit.
    /// * The label to jump at in the CFG after the execution of the statement.
    /// * A map of structures declarations.
    fn visit_stmt(
        &mut self,
        s: anf::Stmt<'mir, 'ctx>,
        dest_l: CfgLabel,
        exit_l: CfgLabel,
        break_l: Option<CfgLabel>,
    ) -> CfgLabel {
        match s.kind {
            anf::StmtKind::AssignS(ptr, rvalue) => {
                if ptr.mode() == LhsMode::Declaring {
                    if let Place::VarP(var) = *ptr.place() {
                        self.strata.entry(self.stm).or_default().insert(var);
                    }
                }
                self.insert(
                    self.instr(InstrKind::Assign(ptr, rvalue), s.span),
                    [(DefaultB, dest_l)],
                )
            }
            anf::StmtKind::SwapS(place1, place2) => self.insert(
                self.instr(InstrKind::SwapS(place1, place2), s.span),
                [(DefaultB, dest_l)],
            ),
            anf::StmtKind::WhileS {
                cond,
                body,
                cond_block,
            } => {
                let loop_l = self.fresh_label();

                self.push_scope();

                // The block itself.
                let body_l = self.visit_stmts(body, loop_l, exit_l, Some(dest_l));

                // If statement
                let if_l = self.insert(
                    self.instr(InstrKind::Branch(cond), s.span),
                    [(BoolB(true), body_l), (BoolB(false), dest_l)],
                );

                // Compute the condition.
                let cond_l = self.visit_stmts(cond_block, if_l, exit_l, break_l);

                self.insert_at(
                    loop_l,
                    self.instr(InstrKind::Noop, s.span),
                    [(DefaultB, cond_l)],
                );

                self.pop_scope();

                loop_l
            }
            anf::StmtKind::CallS {
                name,
                args,
                destination,
            } => self.insert(
                self.instr(
                    InstrKind::Call {
                        name,
                        args,
                        destination,
                    },
                    s.span,
                ),
                [(DefaultB, dest_l)],
            ),
            anf::StmtKind::IfS(cond, iftrue, iffalse) => {
                self.push_scope();
                let iftrue = self.visit_stmts(iftrue, dest_l, exit_l, break_l);
                self.pop_scope();
                self.push_scope();
                let iffalse = self.visit_stmts(iffalse, dest_l, exit_l, break_l);
                self.pop_scope();
                self.insert(
                    self.instr(InstrKind::Branch(cond), s.span),
                    [(BoolB(true), iftrue), (BoolB(false), iffalse)],
                )
            }
            anf::StmtKind::MatchS(matched, branches) => {
                let branches: Vec<_> = branches
                    .into_iter()
                    .map(|(branch, stmts)| {
                        self.push_scope();
                        let branch_l = self.visit_stmts(stmts, dest_l, exit_l, break_l);
                        self.pop_scope();
                        (branch, branch_l)
                    })
                    .collect();
                self.insert(self.instr(InstrKind::Branch(matched), s.span), branches)
            }
            anf::StmtKind::BlockS(b) => {
                self.push_scope();
                let res = self.visit_stmts(b, dest_l, exit_l, break_l);
                self.pop_scope();
                res
            }
            anf::StmtKind::ReturnS => {
                self.insert(self.instr(InstrKind::Noop, s.span), [(DefaultB, exit_l)])
            }
            anf::StmtKind::BreakS => {
                let break_l = break_l.expect("break statement must be in a while loop");
                self.insert(self.instr(InstrKind::Noop, s.span), [(DefaultB, break_l)])
            }
            anf::StmtKind::ContinueS => todo!(),
        }
    }

    /// Visits a function.
    fn visit_fun(&mut self, f: anf::Fun<'mir, 'ctx>) -> Fun<'mir, 'ctx> {
        self.push_scope();
        // The return value is moved at the end of the function
        let ret_l = if let Some(ret_v) = f.ret_v {
            self.insert(
                self.instr(
                    InstrKind::PhantomUse(Reference::new_moved(ret_v)),
                    default(),
                ),
                [],
            )
        } else {
            self.fresh_label()
        };

        // The in-place arguments are marked as being mutably used after the function,
        // so that they be not moved during the function
        let ret_l = f
            .params
            .iter()
            .filter(|param| param.byref)
            .fold(ret_l, |ret_l, param| {
                self.insert(
                    self.instr(
                        InstrKind::PhantomUse(Reference::new(
                            Pointer::new(
                                self.arena,
                                self.arena.alloc(Place::VarP(param.name())),
                                param.span,
                            ),
                            self.arena.alloc_mut(Mode::MutBorrowed),
                        )),
                        default(),
                    ),
                    [(DefaultB, ret_l)],
                )
            });
        let entry_l = self.visit_stmts(f.body, ret_l, ret_l, None);
        self.pop_scope();

        let body = std::mem::take(&mut self.cfg);

        Fun {
            name: f.name,
            params: f.params,
            ret_l,
            ret_v: f.ret_v,
            entry_l,
            strata: std::mem::take(&mut self.strata),
            body,
        }
    }

    /// Visits a program, recursively producing CFGs for each function.
    fn visit_program(&mut self, p: anf::Program<'mir, 'ctx>) -> Program<'mir, 'ctx> {
        let anf::Program {
            funs,
            structs,
            enums,
        } = p;

        // Note: order is important.
        // We must visit structures first.
        Program {
            funs: funs
                .into_iter()
                .map(|(name, f)| (name, self.visit_fun(f)))
                .collect(),
            structs,
            enums,
        }
    }
}
