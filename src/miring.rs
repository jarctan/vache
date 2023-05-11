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

/// Control-flow graph builder.
#[derive(Debug, Default)]
struct CfgBuilder {
    /// The WIP CFG for the current function.
    cfg: Cfg,
}

impl CfgBuilder {
    /// Generates a trash variable with a given type.
    ///
    /// Trash variables are never read. This fact allows for some optimizations
    /// in the backend.
    #[must_use]
    pub fn trash_var(&mut self, ty: Ty) -> VarDef {
        VarDef {
            name: Var::trash(),
            ty,
        }
    }

    /// Generates a fresh variable, that has never been used in *any* CFG.
    #[must_use]
    pub fn fresh_var(&mut self, ty: Ty) -> VarDef {
        let old_value = VAR_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        VarDef {
            name: Var::cfg(old_value),
            ty,
        }
    }

    /// Generates a fresh CFG label, that has never been used in that CFG.
    #[must_use]
    pub fn fresh_label(&mut self) -> CfgLabel {
        self.cfg.add_node(Instr::default())
    }

    /// Inserts an instruction in the CFG, returning its label in the graph.
    #[must_use]
    pub fn insert(
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
    pub fn insert_at(
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

    /// Finishes the construction and returns the resulting CFG.
    #[must_use]
    pub fn finish(self) -> Cfg {
        self.cfg
    }
}

/// Typed AST to MIR transformer.
pub(crate) struct MIRer {
    /// Current CFG builder.
    cfg: CfgBuilder,
}

impl MIRer {
    /// Creates a new MIR processor.
    pub fn new() -> Self {
        Self {
            cfg: CfgBuilder::default(),
        }
    }

    /// Computes the MIR output of a typed program using that processor.
    pub fn gen_mir(&mut self, p: tast::Program) -> Program {
        self.visit_program(p)
    }

    /// Visits an expression. It It will add the nodes for that
    /// block in the CFG, and return its stratum and the CFG label for it.
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
        dest_v: VarDef,
        dest_l: CfgLabel,
        structs: &HashMap<String, Struct>,
    ) -> CfgLabel {
        match e.raw {
            tast::RawExpr::UnitE => self.cfg.insert(
                Instr::Assign(dest_v.name, RValue::Unit),
                [(DefaultB, dest_l)],
            ),
            tast::RawExpr::IntegerE(i) => self.cfg.insert(
                Instr::Assign(dest_v.name, RValue::Integer(i)),
                [(DefaultB, dest_l)],
            ),
            tast::RawExpr::StringE(s) => self.cfg.insert(
                Instr::Assign(dest_v.name, RValue::String(s)),
                [(DefaultB, dest_l)],
            ),
            tast::RawExpr::VarE(v) => {
                if dest_v == v {
                    // If the rhs and lhs are the same variable, that's a no-op we can optimize away
                    dest_l
                } else {
                    self.cfg.insert(
                        Instr::Assign(dest_v.name, RValue::Var(v.name)),
                        [(DefaultB, dest_l)],
                    )
                }
            }
            tast::RawExpr::CallE { name, args } => {
                // Find some temporary variables to hold the result of the evaluation of each
                // argument
                let arg_vars: Vec<VarDef> = args
                    .clone()
                    .into_iter()
                    .map(|param| self.cfg.fresh_var(param.ty))
                    .collect();

                // The call itself in the CFG
                let call_l = self.cfg.insert(
                    Instr::Call {
                        name,
                        args: arg_vars.clone().into_iter().map(|x| x.into()).collect(),
                        destination: dest_v,
                    },
                    [(DefaultB, dest_l)],
                );

                // The computation of the arguments in the CFG (in rev order!)
                let compute_l = args
                    .into_iter()
                    .rev()
                    .zip(arg_vars.iter().rev().cloned())
                    .fold(call_l, |dest_l, (arg_expr, arg_var)| {
                        self.visit_expr(arg_expr, arg_var, dest_l, structs)
                    });

                // Declaration of the arguments
                arg_vars.into_iter().rev().fold(compute_l, |dest_l, v| {
                    self.cfg.insert(Instr::Declare(v), [(DefaultB, dest_l)])
                })
            }
            tast::RawExpr::IfE(box cond, box iftrue, box iffalse) => {
                // Branches.
                let iftrue_l = self.visit_block(iftrue, dest_v.clone(), dest_l.clone(), structs);
                let iffalse_l = self.visit_block(iffalse, dest_v, dest_l, structs);

                // The switch
                let cond_var = self.cfg.fresh_var(BoolT);
                let cond_l = self.cfg.insert(
                    Instr::Branch(cond_var.name.clone()),
                    [(TrueB, iftrue_l), (FalseB, iffalse_l)],
                );

                // Evaluate the condition
                self.visit_expr(cond, cond_var, cond_l, structs)
            }
            tast::RawExpr::BlockE(box e) => self.visit_block(e, dest_v, dest_l, structs),
            tast::RawExpr::FieldE(box s, field) => {
                if let StructT(name) = &s.ty {
                    let field_ty = structs.get(name).unwrap().get_field(&field).clone();
                    let struct_var = self.cfg.fresh_var(field_ty);
                    let dest_l = self.cfg.insert(
                        Instr::Assign(dest_v.name, RValue::Field(struct_var.name.clone(), field)),
                        [(DefaultB, dest_l)],
                    );
                    let dest_l = self.visit_expr(s, struct_var.clone(), dest_l, structs);
                    self.cfg
                        .insert(Instr::Declare(struct_var), [(DefaultB, dest_l)])
                } else {
                    panic!("Cannot get a field of something which is not a struct");
                }
            }
            tast::RawExpr::StructE {
                name: s_name,
                fields,
            } => {
                // Find some temporary variables to hold the result of the evaluation of each
                // field
                let field_vars: HashMap<String, VarDef> = fields
                    .iter()
                    .map(|(name, e)| (name.clone(), self.cfg.fresh_var(e.ty.clone())))
                    .collect();

                // Struct instantiation in the CFG
                let struct_l = self.cfg.insert(
                    Instr::Struct {
                        name: s_name,
                        fields: field_vars
                            .clone()
                            .into_iter()
                            .map(|(field, var)| (field, var.name))
                            .collect(),
                        destination: dest_v,
                    },
                    [(DefaultB, dest_l)],
                );

                // Compute the expressions in the fields in the CFG (rev order!)
                let compute_l = fields
                    .into_iter()
                    .rev()
                    .fold(struct_l, |dest_l, (field, expr)| {
                        self.visit_expr(expr, field_vars[&field].clone(), dest_l, structs)
                    });

                field_vars.into_values().fold(compute_l, |dest_l, var| {
                    self.cfg.insert(Instr::Declare(var), [(DefaultB, dest_l)])
                })
            }
        }
    }

    /// Visits a block. It It will add the nodes for that
    /// block in the CFG, and return its stratum and the CFG label for it.
    ///
    /// Takes as arguments:
    /// * The expression itself (as a parser AST node)
    /// * The destination variable that will store the result of this expression
    /// * The label to jump in the CFG to after evaluating this expression
    /// * The map of structures declarations.
    fn visit_block(
        &mut self,
        b: tast::Block,
        dest_v: VarDef,
        dest_l: CfgLabel,
        structs: &HashMap<String, Struct>,
    ) -> CfgLabel {
        let ret_l = self.visit_expr(b.ret, dest_v, dest_l.clone(), structs);
        b.stmts
            .into_iter()
            .rev()
            .fold(ret_l, |dest_l, s| self.visit_stmt(s, dest_l, structs))
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
                let expr_l = self.visit_expr(expr, vardef.clone(), dest_l, structs);
                self.cfg
                    .insert(Instr::Declare(vardef), [(DefaultB, expr_l)])
            }
            tast::Stmt::Assign(vardef, expr) => self.visit_expr(expr, vardef, dest_l, structs),
            tast::Stmt::While { cond, body } => {
                let loop_l = self.cfg.fresh_label();

                // The block itself.
                let trash_var = self.cfg.trash_var(UnitT);
                let body_l = self.visit_block(body, trash_var, loop_l.clone(), structs);

                // If statement
                let cond_v = self.cfg.fresh_var(BoolT);
                let if_l = self.cfg.insert(
                    Instr::Branch(cond_v.name.clone()),
                    [(TrueB, body_l), (FalseB, dest_l)],
                );

                // Compute the condition.
                let cond_l: CfgLabel = self.visit_expr(cond, cond_v.clone(), if_l, structs);
                self.cfg
                    .insert_at(loop_l.clone(), Instr::Noop, [(DefaultB, cond_l)]);

                // Declare the condition and loop
                self.cfg
                    .insert(Instr::Declare(cond_v), [(DefaultB, loop_l)])
            }
            tast::Stmt::ExprS(e) => {
                let trash_var = self.cfg.trash_var(UnitT);
                self.visit_expr(e, trash_var, dest_l, structs)
            }
        }
    }

    /// Visits a function.
    fn visit_fun(&mut self, f: tast::Fun, structs: &HashMap<String, Struct>) -> Fun {
        let ret_l = self.cfg.fresh_label();
        let ret_v = self.cfg.fresh_var(f.ret_ty);
        let entry_l = self.visit_block(f.body, ret_v.clone(), ret_l.clone(), structs);
        let body = std::mem::take(&mut self.cfg).finish();

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
