//! CFG production.

use Branch::*;

use crate::anf;
use crate::mir::*;

/// ANF to MIR transformer.
pub(crate) struct MIRer<'ctx> {
    /// The WIP CFG for the current function.
    cfg: CfgI<'ctx>,
    /// Current stratum.
    stm: Stratum,
}

impl<'ctx> MIRer<'ctx> {
    /// Creates a new MIR processor.
    pub fn new() -> Self {
        Self {
            cfg: Cfg::default(),
            stm: Stratum::static_stm(),
        }
    }

    /// Generates a fresh CFG label, that has never been used in that CFG.
    #[must_use]
    fn fresh_label(&mut self) -> CfgLabel {
        self.cfg.add_node(Instr {
            kind: InstrKind::default(),
            scope: self.stm,
        })
    }

    /// Generates an instruction with the right stratum.
    fn instr<'s>(&'s self, kind: InstrKind<'ctx>) -> Instr<'ctx> {
        Instr {
            kind,
            scope: self.stm,
        }
    }

    /// Inserts an instruction in the CFG, returning its label in the graph.
    #[must_use]
    fn insert<'s>(
        &'s mut self,
        instr: Instr<'ctx>,
        branches: impl IntoIterator<Item = (Branch, CfgLabel)>,
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
        instr: Instr<'ctx>,
        branches: impl IntoIterator<Item = (Branch, CfgLabel)>,
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
    pub fn gen_mir(&mut self, p: anf::Program<'ctx>) -> Program<'ctx> {
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
        b: anf::Block<'ctx>,
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
        s: anf::Stmt<'ctx>,
        dest_l: CfgLabel,
        exit_l: CfgLabel,
        break_l: Option<CfgLabel>,
    ) -> CfgLabel {
        match s {
            anf::Stmt::DeclareS(vardef) => {
                self.insert(self.instr(InstrKind::Declare(vardef)), [(DefaultB, dest_l)])
            }
            anf::Stmt::AssignS(ptr, rvalue) => self.insert(
                self.instr(InstrKind::Assign(ptr, rvalue)),
                [(DefaultB, dest_l)],
            ),
            anf::Stmt::WhileS {
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
                    self.instr(InstrKind::Branch(cond)),
                    [(TrueB, body_l), (FalseB, dest_l)],
                );

                // Compute the condition.
                let cond_l = self.visit_stmts(cond_block, if_l, exit_l, break_l);

                self.insert_at(loop_l, self.instr(InstrKind::Noop), [(DefaultB, cond_l)]);

                self.pop_scope();

                loop_l
            }
            anf::Stmt::CallS {
                name,
                args,
                destination,
            } => self.insert(
                self.instr(InstrKind::Call {
                    name,
                    args,
                    destination,
                }),
                [(DefaultB, dest_l)],
            ),
            anf::Stmt::IfS(cond, iftrue, iffalse) => {
                self.push_scope();
                let iftrue = self.visit_stmts(iftrue, dest_l, exit_l, break_l);
                self.pop_scope();
                self.push_scope();
                let iffalse = self.visit_stmts(iffalse, dest_l, exit_l, break_l);
                self.pop_scope();
                self.insert(
                    self.instr(InstrKind::Branch(cond)),
                    [(TrueB, iftrue), (FalseB, iffalse)],
                )
            }
            anf::Stmt::BlockS(b) => {
                self.push_scope();
                let res = self.visit_stmts(b, dest_l, exit_l, break_l);
                self.pop_scope();
                res
            }
            anf::Stmt::ReturnS(ret) => {
                self.insert(self.instr(InstrKind::Return(ret)), [(DefaultB, exit_l)])
            }
            anf::Stmt::BreakS => {
                let break_l = break_l.expect("break statement must be in a while loop");
                self.insert(self.instr(InstrKind::Noop), [(DefaultB, break_l)])
            }
            anf::Stmt::ContinueS => todo!(),
        }
    }

    /// Visits a function.
    fn visit_fun(&mut self, f: anf::Fun<'ctx>) -> Fun<'ctx> {
        let ret_l = self.fresh_label();

        self.push_scope();
        let entry_l = self.visit_stmts(f.body, ret_l, ret_l, None);
        self.pop_scope();

        let body = std::mem::take(&mut self.cfg);

        Fun {
            name: f.name,
            params: f.params,
            ret_l,
            ret_v: f.ret_v,
            entry_l,
            body,
        }
    }

    /// Visits a program, recursively producing CFGs for each function.
    fn visit_program(&mut self, p: anf::Program<'ctx>) -> Program<'ctx> {
        let anf::Program {
            funs,
            structs,
            enums,
            arena,
        } = p;

        // Note: order is important.
        // We must visit structures first.
        Program {
            funs: funs
                .into_iter()
                .map(|(name, f)| (name, self.visit_fun(f)))
                .collect(),
            arena,
            structs,
            enums,
        }
    }
}
