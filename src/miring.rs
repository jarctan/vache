//! Typing.

use std::collections::HashMap;
use std::sync::atomic::AtomicU64;

use Branch::*;
use Ty::*;

use crate::mir::*;
use crate::tast;

/// Fresh variable counter.
///
/// Global to all CFGs to avoid any confusion between variable names (since we
/// can access a variable created in one CFG from another CFG).
pub static VAR_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Typed AST to MIR transformer.
pub(crate) struct MIRer {
    /// The WIP CFG for the current function.
    cfg: Cfg,
    /// Current stratum.
    stm: Stratum,
}

impl MIRer {
    /// Creates a new MIR processor.
    pub fn new() -> Self {
        Self {
            cfg: Cfg::default(),
            stm: Stratum::static_stm(),
        }
    }

    /// Generates a fresh variable, that has never been used in *any* CFG.
    #[must_use]
    fn fresh_var(&mut self, ty: Ty, stm: Stratum) -> VarDef {
        let old_value = VAR_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        VarDef {
            name: Var::cfg(old_value),
            ty,
            stm,
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
    fn instr(&self, kind: InstrKind) -> Instr {
        Instr {
            kind,
            scope: self.stm,
        }
    }

    /// Inserts an instruction in the CFG, returning its label in the graph.
    #[must_use]
    fn insert(
        &mut self,
        instr: Instr,
        branches: impl IntoIterator<Item = (Branch, CfgLabel)>,
    ) -> CfgLabel {
        let from = self.cfg.add_node(instr);
        for (b, to) in branches {
            self.cfg.add_edge(from.clone(), to, b, ());
        }
        from
    }

    /// Inserts an instruction at a specific label in the CFG.
    ///
    /// # Panics
    /// Panics if the node labeled by that `label` already exists.
    fn insert_at(
        &mut self,
        label: CfgLabel,
        instr: Instr,
        branches: impl IntoIterator<Item = (Branch, CfgLabel)>,
    ) {
        self.cfg[&label] = instr;
        for (b, to) in branches {
            self.cfg.add_edge(label.clone(), to, b, ());
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
    pub fn gen_mir(&mut self, p: tast::Program) -> Program {
        self.visit_program(p)
    }

    /// Returns the variable with its suitable addressing:
    /// * requested `mode` by default
    /// * BUT if the stratum of the assigned value is clearly longer-lived, then
    ///   we know we must own the value.
    ///
    /// Further refinements to the addressing mode will be made by the next
    /// phases (liveness analysis for instance).
    fn visit_vardef(src: VarDef, mode: Mode, dest: Option<&VarDef>) -> VarMode {
        // If dest outlives, if must own the variable!
        let mode = if let Some(dest) = dest && dest.stm < src.stm {
            Mode::Cloned
        } else {
            mode
        };
        VarMode {
            var: src.name,
            mode,
        }
    }

    /// Inserts a `place = rvalue` instruction in the CFG, that branches to a
    /// list of labels after that instruction.
    fn insert_assign_instr(
        &mut self,
        place: impl Into<Place>,
        rvalue: impl Into<RValue>,
        branches: impl IntoIterator<Item = (Branch, CfgLabel)>,
    ) -> CfgLabel {
        let instr = self.instr(InstrKind::Assign(place.into(), rvalue.into()));
        self.insert(instr, branches)
    }

    /// Visits an expression. It It will add the nodes for that
    /// block in the CFG, and return the CFG label for it.
    ///
    /// It takes as arguments:
    /// * The expression itself (as a parser AST node).
    /// * The destination variable that will store the result of this
    ///   expression.
    /// * The label to jump in the CFG to after evaluating this expression.
    /// * The map of structures declarations.
    fn visit_expr(
        &mut self,
        e: tast::Expr,
        dest_v: Option<VarDef>,
        mode: Mode,
        dest_l: CfgLabel,
        structs: &HashMap<String, Struct>,
    ) -> CfgLabel {
        match e.raw {
            tast::ExprKind::UnitE => {
                if let Some(dest_v) = dest_v {
                    self.insert_assign_instr(dest_v.name, RValue::Unit, [(DefaultB, dest_l)])
                } else {
                    dest_l
                }
            }
            tast::ExprKind::IntegerE(i) => {
                if let Some(dest_v) = dest_v {
                    self.insert_assign_instr(dest_v.name, i, [(DefaultB, dest_l)])
                } else {
                    dest_l
                }
            }
            tast::ExprKind::StringE(s) => {
                if let Some(dest_v) = dest_v {
                    self.insert_assign_instr(dest_v.name, s, [(DefaultB, dest_l)])
                } else {
                    dest_l
                }
            }
            tast::ExprKind::VarE(v) => {
                if let Some(dest_v) = dest_v && dest_v != v {
                    let var_ref = Self::visit_vardef(v, mode, Some(&dest_v));
                    // Only do the assignment if the rhs and lhs are not the same! Otherwise optimize it away
                    self.insert_assign_instr(dest_v.name, var_ref,[(DefaultB, dest_l)])
                } else {
                    dest_l
                }
            }
            tast::ExprKind::CallE { name, args } => {
                // Find some temporary variables to hold the result of the evaluation of each
                // argument
                let arg_vars: Vec<VarDef> = args
                    .clone()
                    .into_iter()
                    .map(|param| self.fresh_var(param.ty, self.stm))
                    .collect();

                // The call itself in the CFG
                let call_l = self.insert(
                    self.instr(InstrKind::Call {
                        name,
                        args: arg_vars
                            .clone()
                            .into_iter()
                            .map(|arg| Self::visit_vardef(arg, Mode::default(), dest_v.as_ref()))
                            .collect(),
                        destination: dest_v.map(|dest| dest.name),
                    }),
                    [(DefaultB, dest_l)],
                );

                // The computation of the arguments in the CFG (in rev order!)
                let compute_l = args
                    .into_iter()
                    .rev()
                    .zip(arg_vars.iter().rev().cloned())
                    .fold(call_l, |dest_l, (arg_expr, arg_var)| {
                        self.visit_expr(arg_expr, Some(arg_var), mode, dest_l, structs)
                    });

                // Declaration of the arguments
                arg_vars.into_iter().rev().fold(compute_l, |dest_l, v| {
                    self.insert(self.instr(InstrKind::Declare(v)), [(DefaultB, dest_l)])
                })
            }
            tast::ExprKind::IfE(box cond, box iftrue, box iffalse) => {
                // Branches.
                let iftrue_l = self.visit_block(iftrue, dest_v.clone(), dest_l.clone(), structs);
                let iffalse_l = self.visit_block(iffalse, dest_v, dest_l, structs);

                // The switch
                let cond_var = self.fresh_var(BoolT, self.stm);
                let cond_l = self.insert(
                    self.instr(InstrKind::Branch(cond_var.name.clone())),
                    [(TrueB, iftrue_l), (FalseB, iffalse_l)],
                );

                // Evaluate the condition
                self.visit_expr(cond, Some(cond_var), mode, cond_l, structs)
            }
            tast::ExprKind::BlockE(box e) => self.visit_block(e, dest_v, dest_l, structs),
            tast::ExprKind::FieldE(box s, field) => {
                if let StructT(name) = &s.ty {
                    let field_ty = structs[name].get_field(&field).clone();
                    let struct_var = self.fresh_var(field_ty, self.stm);
                    let var_ref = Self::visit_vardef(struct_var.clone(), mode, dest_v.as_ref());
                    let dest_l = if let Some(dest_v) = dest_v {
                        self.insert_assign_instr(
                            dest_v.name,
                            RValue::Field(var_ref, field),
                            [(DefaultB, dest_l)],
                        )
                    } else {
                        dest_l
                    };
                    let dest_l =
                        self.visit_expr(s, Some(struct_var.clone()), mode, dest_l, structs);
                    self.insert(
                        self.instr(InstrKind::Declare(struct_var)),
                        [(DefaultB, dest_l)],
                    )
                } else {
                    panic!("Compiler error: field are only valid on structs");
                }
            }
            tast::ExprKind::StructE {
                name: s_name,
                fields,
            } => {
                // Find some temporary variables to hold the result of the evaluation of each
                // field
                let field_vars: HashMap<String, VarDef> = fields
                    .iter()
                    .map(|(name, e)| (name.clone(), self.fresh_var(e.ty.clone(), self.stm)))
                    .collect();

                // Struct instantiation in the CFG.
                // Only do if the destination exist! Otherwise, no usefulness to it.
                let struct_l = if let Some(dest_v) = dest_v {
                    self.insert_assign_instr(
                        dest_v.name.clone(),
                        RValue::Struct {
                            name: s_name,
                            fields: field_vars
                                .clone()
                                .into_iter()
                                .map(|(field, var)| {
                                    (
                                        field,
                                        Self::visit_vardef(var, Mode::default(), Some(&dest_v)),
                                    )
                                })
                                .collect(),
                        },
                        [(DefaultB, dest_l)],
                    )
                } else {
                    dest_l
                };

                // Compute the value of the tmp variables (remember the CFG is declared in rev
                // order!)
                let compute_l = fields
                    .into_iter()
                    .rev()
                    .fold(struct_l, |dest_l, (field, expr)| {
                        self.visit_expr(
                            expr,
                            Some(field_vars[&field].clone()),
                            mode,
                            dest_l,
                            structs,
                        )
                    });

                // Declare the temporary variables
                field_vars.into_values().fold(compute_l, |dest_l, var| {
                    self.insert(self.instr(InstrKind::Declare(var)), [(DefaultB, dest_l)])
                })
            }
            tast::ExprKind::IndexE(box e, box ix) => {
                let e_var = self.fresh_var(e.ty.clone(), self.stm);
                let ix_var = self.fresh_var(ix.ty.clone(), self.stm);
                let e_ref = Self::visit_vardef(e_var.clone(), Mode::default(), dest_v.as_ref());
                let ix_ref = Self::visit_vardef(ix_var.clone(), Mode::default(), dest_v.as_ref());
                let dest_l = if let Some(dest_v) = dest_v {
                    self.insert_assign_instr(
                        dest_v.name,
                        RValue::Index(e_ref, ix_ref),
                        [(DefaultB, dest_l)],
                    )
                } else {
                    dest_l
                };

                // Assign the two temporary variables
                // Note: ix is compute AFTER e, so appears first here
                let dest_l = self.visit_expr(ix, Some(ix_var.clone()), mode, dest_l, structs);
                let dest_l = self.visit_expr(e, Some(e_var.clone()), mode, dest_l, structs);

                // Declare temporary variables
                let dest_l =
                    self.insert(self.instr(InstrKind::Declare(ix_var)), [(DefaultB, dest_l)]);
                self.insert(self.instr(InstrKind::Declare(e_var)), [(DefaultB, dest_l)])
            }
            tast::ExprKind::ArrayE(array) => {
                // Find some temporary variables to hold the result of the evaluation of each
                // item in the array
                let array_vars: Vec<VarDef> = array
                    .iter()
                    .map(|e| self.fresh_var(e.ty.clone(), self.stm))
                    .collect();

                // Array creation
                // Only do if the destination exist! Otherwise, no usefulness to it
                let struct_l = if let Some(dest_v) = dest_v {
                    self.insert_assign_instr(
                        dest_v.name.clone(),
                        RValue::Array(
                            array_vars
                                .clone()
                                .into_iter()
                                .map(|var| Self::visit_vardef(var, Mode::default(), Some(&dest_v)))
                                .collect(),
                        ),
                        [(DefaultB, dest_l)],
                    )
                } else {
                    dest_l
                };

                // Compute the value of the tmp variables (remember the CFG is declared in
                // reversed order!)
                let compute_l = array_vars.iter().rev().zip(array.into_iter().rev()).fold(
                    struct_l,
                    |dest_l, (var, item)| {
                        self.visit_expr(item, Some(var.clone()), mode, dest_l, structs)
                    },
                );

                // Declare the temporary variables
                array_vars.into_iter().rev().fold(compute_l, |dest_l, var| {
                    self.insert(self.instr(InstrKind::Declare(var)), [(DefaultB, dest_l)])
                })
            }
        }
    }

    /// Visits a block. It It will add the nodes for that
    /// block in the CFG, and return the (entry) CFG label for it.
    ///
    /// Takes as arguments:
    /// * The expression itself (as a parser AST node)
    /// * The destination variable that will store the result of this expression
    /// * The label to jump in the CFG to after evaluating this expression
    /// * The map of structures declarations.
    fn visit_block(
        &mut self,
        b: tast::Block,
        dest_v: Option<VarDef>,
        dest_l: CfgLabel,
        structs: &HashMap<String, Struct>,
    ) -> CfgLabel {
        self.push_scope();
        let ret_l = self.visit_expr(b.ret, dest_v, Mode::Cloned, dest_l, structs);
        let entry_l = b
            .stmts
            .into_iter()
            .rev()
            .fold(ret_l, |dest_l, s| self.visit_stmt(s, dest_l, structs));
        self.pop_scope();
        entry_l
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
        s: tast::Stmt,
        dest_l: CfgLabel,
        structs: &HashMap<String, Struct>,
    ) -> CfgLabel {
        match s {
            tast::Stmt::Declare(vardef, expr) => {
                let expr_l =
                    self.visit_expr(expr, Some(vardef.clone()), Mode::default(), dest_l, structs);
                self.insert(self.instr(InstrKind::Declare(vardef)), [(DefaultB, expr_l)])
            }
            tast::Stmt::Assign(
                tast::Place {
                    kind: tast::PlaceKind::VarP(name),
                    ty,
                    stm,
                },
                expr,
            ) => self.visit_expr(
                expr,
                Some(VarDef { name, stm, ty }),
                Mode::default(),
                dest_l,
                structs,
            ),
            tast::Stmt::Assign(place, expr) => match place.kind {
                tast::PlaceKind::VarP(name) => self.visit_expr(
                    expr,
                    Some(VarDef {
                        name,
                        ty: place.ty,
                        stm: place.stm,
                    }),
                    Mode::default(),
                    dest_l,
                    structs,
                ),
                tast::PlaceKind::IndexP(box array, box ix) => {
                    let array_var = self.fresh_var(array.ty.clone(), self.stm);
                    let ix_var = self.fresh_var(ix.ty.clone(), self.stm);
                    let array_ty = if let ArrayT(box array_ty) = &array.ty {
                        array_ty.clone()
                    } else {
                        panic!("Compiler error")
                    };

                    // Create a temporary destination variable to hold the result of the rhs
                    let dest_v = self.fresh_var(array_ty, self.stm);

                    // Finally, assign to the indexed variable
                    let dest_l = self.insert_assign_instr(
                        Place::IndexP(array_var.name.clone(), ix_var.name.clone()),
                        Self::visit_vardef(dest_v.clone(), Mode::default(), Some(&array_var)),
                        [(DefaultB, dest_l)],
                    );

                    // Assign the two temporary variables
                    // Note: ix is compute AFTER e, so appears first here
                    let dest_l =
                        self.visit_expr(ix, Some(ix_var.clone()), Mode::default(), dest_l, structs);
                    let dest_l = self.visit_expr(
                        array,
                        Some(array_var.clone()),
                        Mode::MutBorrowed,
                        dest_l,
                        structs,
                    );

                    // Assign the tmp variable for the result of the index
                    let dest_l = self.visit_expr(
                        expr,
                        Some(dest_v.clone()),
                        Mode::default(),
                        dest_l,
                        structs,
                    );

                    // Declare temporary variables;
                    let dest_l =
                        self.insert(self.instr(InstrKind::Declare(ix_var)), [(DefaultB, dest_l)]);
                    let dest_l = self.insert(
                        self.instr(InstrKind::Declare(array_var)),
                        [(DefaultB, dest_l)],
                    );
                    self.insert(self.instr(InstrKind::Declare(dest_v)), [(DefaultB, dest_l)])
                }
                tast::PlaceKind::FieldP(box _strukt, box _field) => {
                    todo!()
                }
            },
            tast::Stmt::While { cond, body } => {
                let loop_l = self.fresh_label();

                // The block itself.
                let body_l = self.visit_block(body, None, loop_l.clone(), structs);

                // If statement
                let cond_v = self.fresh_var(BoolT, self.stm);
                let if_l = self.insert(
                    self.instr(InstrKind::Branch(cond_v.name.clone())),
                    [(TrueB, body_l), (FalseB, dest_l)],
                );

                // Compute the condition.
                let cond_l: CfgLabel =
                    self.visit_expr(cond, Some(cond_v.clone()), Mode::default(), if_l, structs);
                self.insert_at(
                    loop_l.clone(),
                    self.instr(InstrKind::Noop),
                    [(DefaultB, cond_l)],
                );

                // Declare the condition and loop
                self.insert(self.instr(InstrKind::Declare(cond_v)), [(DefaultB, loop_l)])
            }
            tast::Stmt::ExprS(e) => self.visit_expr(e, None, Mode::default(), dest_l, structs),
        }
    }

    /// Visits a function.
    fn visit_fun(&mut self, f: tast::Fun, structs: &HashMap<String, Struct>) -> Fun {
        let ret_l = self.fresh_label();
        // Reserve a fresh variable for the output, only if it's not the unit type
        let ret_v = if f.ret_ty != UnitT {
            Some(self.fresh_var(f.ret_ty, self.stm))
        } else {
            None
        };
        let entry_l = self.visit_block(f.body, ret_v.clone(), ret_l.clone(), structs);
        let body = std::mem::take(&mut self.cfg);

        Fun {
            name: f.name,
            params: f.params,
            ret_v,
            ret_l,
            entry_l,
            body,
        }
    }

    /// Visits a program, recursively producing CFGs for each function.
    fn visit_program(&mut self, p: tast::Program) -> Program {
        let tast::Program { funs, structs } = p;

        // Note: order is important.
        // We must visit structures first.
        Program {
            funs: funs
                .into_iter()
                .map(|(name, f)| (name, self.visit_fun(f, &structs)))
                .collect(),
            structs,
        }
    }
}
