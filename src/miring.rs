//! Typing.

use std::collections::HashMap;
use std::sync::atomic::AtomicU64;

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
    /// Fresh label counter.
    label_counter: u64,
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
        let old_value = self.label_counter;
        self.label_counter = self.label_counter.checked_add(1).unwrap();
        CfgLabel::new(old_value)
    }

    /// Inserts an instruction in the CFG, returning its label in the graph.
    #[must_use]
    pub fn insert(&mut self, instr: Instr) -> CfgLabel {
        let entry_l = self.fresh_label();
        self.cfg.insert(entry_l.clone(), instr);
        entry_l
    }

    /// Asserts an instruction at a specific label in the CFG.
    ///
    /// # Panics
    /// Panics if the node labeled by that `label` already exists.
    pub fn insert_at(&mut self, label: CfgLabel, instr: Instr) {
        assert!(self.cfg.insert(label, instr).is_none());
    }

    /// Finishes the construction and returns the resulting CFG.
    #[must_use]
    pub fn finish(self) -> Cfg {
        self.cfg
    }
}

pub(crate) struct MIRer {
    /// Stack of control-flow graph builders.
    cfgs: Vec<CfgBuilder>,
}

impl MIRer {
    /// Creates a new MIR processor.
    pub fn new() -> Self {
        Self { cfgs: vec![] }
    }

    /// Computes the MIR output of a typed program using that processor.
    pub fn gen_mir(&mut self, p: tast::Program) -> Program {
        self.visit_program(p)
    }

    fn push_scope(&mut self) {
        self.cfgs.push(CfgBuilder::default());
    }

    fn pop_scope(&mut self) -> Cfg {
        self.cfgs.pop().unwrap().finish()
    }

    fn cfg(&mut self) -> &mut CfgBuilder {
        self.cfgs.last_mut().unwrap()
    }

    /// Visits an expression. It It will add the nodes for that
    /// block in the CFG, and return its stratum and the CFG label for it.
    ///
    /// Takes as arguments:
    /// * the expression itself (as a parser AST node)
    /// * the destination variable that will store the result of this expression
    /// * the label to jump in the CFG to after evaluating this expression
    fn visit_expr(&mut self, e: tast::Expr, dest_v: VarDef, dest_l: CfgLabel) -> CfgLabel {
        match e.raw {
            tast::RawExpr::UnitE => {
                self.cfg()
                    .insert(Instr::Assign(dest_v.name, RValue::Unit, dest_l))
            }
            tast::RawExpr::IntegerE(i) => {
                self.cfg()
                    .insert(Instr::Assign(dest_v.name, RValue::Integer(i), dest_l))
            }
            tast::RawExpr::StringE(s) => {
                self.cfg()
                    .insert(Instr::Assign(dest_v.name, RValue::String(s), dest_l))
            }
            tast::RawExpr::VarE(v) => {
                if dest_v == v {
                    // If the same variable, nothing to do
                    dest_l
                } else {
                    self.cfg()
                        .insert(Instr::Assign(dest_v.name, RValue::Var(v.name), dest_l))
                }
            }
            tast::RawExpr::CallE { name, args } => {
                // Find some temporary variables to hold the result of the evaluation of each
                // argument
                let arg_vars: Vec<VarDef> = args
                    .clone()
                    .into_iter()
                    .map(|param| self.cfg().fresh_var(param.ty))
                    .collect();

                // The call itself in the CFG
                let call_l = self.cfg().insert(Instr::Call {
                    name,
                    args: arg_vars.clone().into_iter().map(|x| x.into()).collect(),
                    destination: dest_v,
                    target: dest_l,
                });

                // The computation of the arguments in the CFG (in rev order!)
                let compute_l = args
                    .into_iter()
                    .rev()
                    .zip(arg_vars.iter().rev().cloned())
                    .fold(call_l, |dest_l, (arg_expr, arg_var)| {
                        self.visit_expr(arg_expr, arg_var, dest_l)
                    });

                // Declaration of the arguments
                arg_vars.into_iter().rev().fold(compute_l, |dest_l, v| {
                    self.cfg().insert(Instr::Declare(v, dest_l))
                })
            }
            tast::RawExpr::IfE(box cond, box iftrue, box iffalse) => {
                // Branches.
                let iftrue_l = self.visit_block(iftrue, dest_v.clone(), dest_l.clone());
                let iffalse_l = self.visit_block(iffalse, dest_v, dest_l);

                // The switch
                let cond_var = self.cfg().fresh_var(BoolT);
                let cond_l =
                    self.cfg()
                        .insert(Instr::Branch(cond_var.name.clone(), iftrue_l, iffalse_l));

                // Evaluate the condition
                self.visit_expr(cond, cond_var, cond_l)
            }
            tast::RawExpr::BlockE(box e) => self.visit_block(e, dest_v, dest_l),
            tast::RawExpr::FieldE(box _s, _field) => {
                /*let field_l = self.
                let (s, ty, stm) = self.visit_expr(s);
                if let StructT(name) = ty {
                    let strukt = self.get_struct(&name).unwrap();
                    let ty = strukt.get_field(&field).clone();
                    (CopyE(boxed(FieldE(boxed(s), field))), ty, stm) // Choose to copy every time. TODO: do not copy on last use
                } else {
                    panic!("Cannot get a field of something which is not a struct");
                }*/
                todo!()
            }
            tast::RawExpr::StructE {
                name: s_name,
                fields,
            } => {
                // Find some temporary variables to hold the result of the evaluation of each
                // field
                let field_vars: HashMap<String, VarDef> = fields
                    .iter()
                    .map(|(name, e)| (name.clone(), self.cfg().fresh_var(e.ty.clone())))
                    .collect();

                // Struct instantiation in the CFG
                let struct_l = self.cfg().insert(Instr::Struct {
                    name: s_name,
                    fields: field_vars
                        .into_iter()
                        .map(|(field, var)| (field, var.name))
                        .collect(),
                    destination: dest_v.clone(),
                    target: dest_l,
                });

                // Compute the expressions in the fields in the CFG (rev order!)
                fields
                    .into_iter()
                    .rev()
                    .fold(struct_l, |dest_l, (_, expr)| {
                        self.visit_expr(expr, dest_v.clone(), dest_l)
                    })
            }
        }
    }

    /// Visits a block. It It will add the nodes for that
    /// block in the CFG, and return its stratum and the CFG label for it.
    ///
    /// Takes as arguments:
    /// * the expression itself (as a parser AST node)
    /// * the destination variable that will store the result of this expression
    /// * the label to jump in the CFG to after evaluating this expression
    fn visit_block(&mut self, b: tast::Block, dest_v: VarDef, dest_l: CfgLabel) -> CfgLabel {
        self.push_scope();
        let exit_l = self.cfg().fresh_label();
        let ret_l = self.visit_expr(b.ret, dest_v, exit_l.clone());
        let entry_l = b
            .stmts
            .into_iter()
            .rev()
            .fold(ret_l, |dest_l, s| self.visit_stmt(s, dest_l));
        let cfg = self.pop_scope();
        self.cfg().insert(Instr::Scope {
            cfg,
            entry_l,
            exit_l,
            target: dest_l,
        })
    }

    /// Visits a statements, producing the CFG nodes and returning the label for
    /// the entry CFG node for that statement.
    fn visit_stmt(&mut self, s: tast::Stmt, dest_l: CfgLabel) -> CfgLabel {
        match s {
            tast::Stmt::Declare(vardef, expr) => {
                let expr_l = self.visit_expr(expr, vardef.clone(), dest_l);
                self.cfg().insert(Instr::Declare(vardef, expr_l))
            }
            tast::Stmt::Assign(vardef, expr) => self.visit_expr(expr, vardef, dest_l),
            tast::Stmt::While { cond, body } => {
                let loop_l = self.cfg().fresh_label();

                // The block itself.
                let trash_var = self.cfg().trash_var(UnitT);
                let body_l = self.visit_block(body, trash_var, loop_l.clone());

                // If statement
                let cond_v = self.cfg().fresh_var(BoolT);
                let if_l = self
                    .cfg()
                    .insert(Instr::Branch(cond_v.name.clone(), body_l, dest_l));

                // Compute the condition.
                let cond_l: CfgLabel = self.visit_expr(cond, cond_v.clone(), if_l);
                self.cfg().insert_at(loop_l.clone(), Instr::Goto(cond_l));

                // Declare the condition and loop
                self.cfg().insert(Instr::Declare(cond_v, loop_l))
            }
            tast::Stmt::ExprS(e) => {
                let trash_var = self.cfg().trash_var(UnitT);
                self.visit_expr(e, trash_var, dest_l)
            }
        }
    }

    /// Visits a function.
    fn visit_fun(&mut self, f: tast::Fun) -> Fun {
        self.push_scope();
        let ret_l = self.cfg().fresh_label();
        let ret_v = self.cfg().fresh_var(f.ret_ty);
        let entry_l = self.visit_block(f.body, ret_v.clone(), ret_l.clone());
        let body = self.pop_scope();

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
            structs,
            funs: funs
                .into_iter()
                .map(|(name, f)| (name, self.visit_fun(f)))
                .collect(),
        }
    }
}
