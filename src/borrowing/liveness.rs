//! Implementing the liveness analysis algorithm.

use super::flow::Flow;
use crate::borrowing::borrow::Borrow;
use crate::borrowing::ledger::Ledger;
use crate::examples::Var;
use crate::mir::{Cfg, CfgLabel, Instr, RValue};
use crate::utils::set::Set;

/// Variable liveness analysis.
///
/// Takes as arguments:
/// * The CFG.
/// * The entry label in the CFG.
/// * The exit label in the CFG.
///
/// Returns a map of live variables at the entry and exit of each node in the
/// CFG.
pub fn var_liveness(cfg: &Cfg, _entry_l: &CfgLabel, exit_l: &CfgLabel) -> Cfg<Flow<Set<Var>>> {
    // Bootstrap with empty environments.
    let mut var_flow: Cfg<Flow<Set<Var>>> = cfg.map_ref(|_, _| Flow::default(), |_| ());

    let mut updated = true;

    // Compute the fixpoint, iteratively.
    while updated {
        updated = false;

        for (label, instr) in cfg.bfs(exit_l) {
            let successors = cfg.neighbors(label);
            let outs: Set<Var> = successors.map(|x| var_flow[x].ins.clone()).sum();
            let ins: Set<Var> = match instr {
                Instr::Noop | Instr::PushScope | Instr::PopScope => outs.clone(),
                Instr::Declare(var) => outs.clone() - &var.name,
                Instr::Assign(lhs, RValue::Unit)
                | Instr::Assign(lhs, RValue::String(_))
                | Instr::Assign(lhs, RValue::Integer(_)) => outs.clone() - lhs,
                Instr::Assign(lhs, RValue::Var(rhs))
                | Instr::Assign(lhs, RValue::Field(rhs, _)) => outs.clone() - lhs + rhs.var.clone(),
                Instr::Call {
                    name: _,
                    args,
                    destination,
                } => {
                    outs.clone() - &destination.name
                        + Set::from_iter(args.iter().map(|arg| &arg.var).cloned())
                }
                Instr::Struct {
                    name: _,
                    fields,
                    destination,
                } => {
                    outs.clone() - &destination.name
                        + Set::from_iter(fields.values().map(|arg| &arg.var).cloned())
                }
                Instr::Branch(v) => outs.clone() + v.clone(),
            };

            let flow = Flow { ins, outs };
            if var_flow[label] != flow {
                var_flow[label] = flow;
                updated = true;
            }
        }
    }

    var_flow
}

/// Loan liveness analysis.
///
/// Takes as arguments:
/// * The CFG.
/// * The entry label in the CFG.
/// * The exit label in the CFG.
/// * The variable liveliness analysis for that CFG.
///
/// Returns a map of live loans at the entry and exit of each node in the
/// CFG.
fn loan_liveness(
    cfg: &Cfg,
    entry_l: &CfgLabel,
    _exit_l: &CfgLabel,
    var_flow: Cfg<Flow<Set<Var>>>,
) -> Cfg<Flow<Ledger>> {
    let out_of_scope = var_flow.map(|_, flow| flow.outs.clone() - &flow.ins, |_| ());

    // Same, for loans.
    let mut loan_flow: Cfg<Flow<Ledger>> = cfg.map_ref(|_, _| Flow::default(), |_| ());

    let mut updated = true;

    while updated {
        updated = false;

        for (label, instr) in cfg.bfs(entry_l) {
            let predecessors = cfg.preneighbors(label);
            let ins: Ledger = predecessors.map(|x| loan_flow[x].outs.clone()).sum();
            let outs: Ledger = match instr {
                Instr::Noop | Instr::PushScope | Instr::PopScope => ins.clone(),
                Instr::Declare(var) => ins.clone() - &var.name,
                Instr::Assign(lhs, RValue::Unit)
                | Instr::Assign(lhs, RValue::String(_))
                | Instr::Assign(lhs, RValue::Integer(_)) => ins.clone() - lhs,
                Instr::Assign(lhs, RValue::Var(rhs))
                | Instr::Assign(lhs, RValue::Field(rhs, _)) => {
                    ins.clone() - lhs + (lhs.clone(), ins.borrow(rhs.var.clone(), label.clone()))
                }
                Instr::Call {
                    name: _,
                    args,
                    destination,
                } => {
                    ins.clone() - &destination.name
                        + (
                            destination.name.clone(),
                            args.iter()
                                .map(|arg| ins.borrow(arg.var.clone(), label.clone()))
                                .sum::<Set<Borrow>>(),
                        )
                }
                Instr::Struct {
                    name: _,
                    fields,
                    destination,
                } => {
                    ins.clone() - &destination.name
                        + (
                            destination.name.clone(),
                            fields
                                .values()
                                .map(|field| ins.borrow(field.var.clone(), label.clone()))
                                .sum::<Set<Borrow>>(),
                        )
                }
                Instr::Branch(_) => ins.clone(),
            } - &out_of_scope[label];

            let flow = Flow { ins, outs };
            if loan_flow[label] != flow {
                loan_flow[label] = flow;
                updated = true;
            }
        }
    }

    loan_flow
}

/// Liveness analysis.
///
/// Takes as arguments:
/// * The CFG.
/// * The entry label in the CFG.
/// * The exit label in the CFG.
///
/// Performs liveliness analysis, determining which borrows are invalidated.
pub fn liveness(mut cfg: Cfg, entry_l: &CfgLabel, exit_l: &CfgLabel) -> Cfg {
    let var_flow = var_liveness(&cfg, entry_l, exit_l);
    let loan_flow = loan_liveness(&cfg, entry_l, exit_l, var_flow);

    // List all invalidated borrows.
    let mut invalidated: Set<&Borrow> = Set::new();
    for (label, instr) in cfg.bfs(entry_l) {
        if let Some(lhs) = instr.mutated_var() {
            if let Some(borrow) = loan_flow[label].ins.is_borrowed(lhs) {
                invalidated.insert(borrow);
            }
        }
    }

    for Borrow { label, var } in invalidated {
        cfg[label].state_as_owned(var);
    }

    cfg
}

/*
#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::var::vardef;
    use crate::ast::Ty;

    #[test]
    fn test_use() {
        // Labels
        let label = CfgLabel::new(0);

        // Variables
        let x = Var::from("x");
        let x_def = vardef("x", Ty::IntT);
        let y = Var::from("y");

        assert!(Instr::Goto(label.clone()).var_uses().is_empty());

        assert_eq!(
            Set::from_iter(Instr::Assign(x, RValue::Var(y.clone()), label.clone()).var_uses()),
            Set::from_iter([y])
        );

        assert!(Instr::Declare(x_def, label).var_uses().is_empty());
    }

    #[test]
    fn test_liveness() {
        // Labels
        let l0 = CfgLabel::new(0);
        let l1 = CfgLabel::new(1);
        let l2 = CfgLabel::new(2);
        let l3 = CfgLabel::new(3);
        let l_exit = CfgLabel::new(4);

        // Variables
        let x = Var::from("x");
        let x_def = vardef("x", Ty::IntT);
        let y = Var::from("y");
        let y_def = vardef("y", Ty::IntT);

        let cfg: Cfg = [
            (l0.clone(), Instr::Declare(y_def, l1.clone())),
            (
                l1,
                Instr::Assign(y.clone(), RValue::Integer(42.into()), l2.clone()),
            ),
            (l2.clone(), Instr::Declare(x_def, l3.clone())),
            (
                l3.clone(),
                Instr::Assign(x, RValue::Var(y.clone()), l_exit.clone()),
            ),
        ]
        .into_iter()
        .collect();

        let analysis = liveness(&cfg, &l_exit);

        // Entry and exit are trivial
        assert_eq!(analysis[&l0].ins.len(), 0);
        assert_eq!(analysis[&l_exit].ins.len(), 0);
        assert_eq!(analysis[&l_exit].outs.len(), 0);

        // During l2, we still need y
        assert_eq!(analysis[&l2].ins, Set::from_iter([y.clone()]));
        assert_eq!(analysis[&l2].outs, Set::from_iter([y.clone()]));

        // During l3, we still need y but not afterwards anymore
        assert_eq!(analysis[&l3].ins, Set::from_iter([y]));
        assert_eq!(analysis[&l3].outs.len(), 0);
    }
}
*/
