//! Implementing the liveness analysis algorithm.

use super::borrow::Borrows;
use super::flow::Flow;
use crate::borrowing::borrow::Borrow;
use crate::borrowing::ledger::Ledger;
use crate::mir::{Cfg, CfgLabel, InstrKind, Mode, RValue, Var, VarMode};
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

        for (label, instr) in cfg.bfs(exit_l, true) {
            let successors = cfg.neighbors(label);

            let outs: Set<Var> = successors.map(|x| var_flow[x].ins.clone()).sum();
            let ins: Set<Var> = match &instr.kind {
                InstrKind::Noop => outs.clone(),
                InstrKind::Declare(var) => outs.clone() - &var.name,
                InstrKind::Assign(lhs, RValue::Unit)
                | InstrKind::Assign(lhs, RValue::String(_))
                | InstrKind::Assign(lhs, RValue::Integer(_)) => {
                    outs.clone() - lhs.defs() + lhs.uses()
                }
                InstrKind::Assign(lhs, RValue::Var(rhs))
                | InstrKind::Assign(lhs, RValue::Field(rhs, _)) => {
                    outs.clone() - lhs.defs() + lhs.uses() + rhs.var.clone()
                }
                InstrKind::Assign(lhs, RValue::Index(array, index, _)) => {
                    outs.clone() - lhs.defs() + lhs.uses() + array.var.clone() + index.var.clone()
                }
                InstrKind::Assign(lhs, RValue::Struct { name: _, fields }) => {
                    outs.clone() - lhs.defs()
                        + lhs.uses()
                        + Set::from_iter(fields.values().map(|arg| &arg.var).cloned())
                }
                InstrKind::Assign(lhs, RValue::Array(array)) => {
                    outs.clone() - lhs.defs()
                        + lhs.uses()
                        + Set::from_iter(array.iter().map(|arg| &arg.var).cloned())
                }
                InstrKind::Call {
                    name: _,
                    args,
                    destination,
                } => {
                    let res = outs.clone() - destination.as_ref()
                        + Set::from_iter(args.iter().map(|arg| &arg.var).cloned());
                    res
                }
                InstrKind::Branch(v) => outs.clone() + v.clone(),
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
    var_flow: &Cfg<Flow<Set<Var>>>,
) -> Cfg<Flow<Ledger>> {
    let out_of_scope = var_flow.map_ref(|_, flow| flow.ins.clone() - &flow.outs, |_| ());

    // Same, for loans.
    let mut loan_flow: Cfg<Flow<Ledger>> = cfg.map_ref(|_, _| Flow::default(), |_| ());

    let mut updated = true;

    while updated {
        updated = false;

        for (label, instr) in cfg.bfs(entry_l, false) {
            let predecessors = cfg.preneighbors(label);
            let ins: Ledger = predecessors.map(|x| loan_flow[x].outs.clone()).sum();
            let outs: Ledger = match &instr.kind {
                InstrKind::Noop => ins.clone(),
                InstrKind::Declare(var) => ins.clone() - &var.name,
                InstrKind::Assign(lhs, RValue::Unit)
                | InstrKind::Assign(lhs, RValue::String(_))
                | InstrKind::Assign(lhs, RValue::Integer(_)) => ins.clone() - lhs.defs(),
                InstrKind::Assign(lhs, RValue::Var(rhs))
                | InstrKind::Assign(lhs, RValue::Field(rhs, _)) => {
                    let mut ledger = ins.clone();
                    if var_flow[label].outs.contains(lhs.defs()) {
                        ledger.add_borrows(lhs, ins.borrow(rhs, label.clone()));
                    } else {
                        ledger.remove(lhs.defs());
                    }
                    ledger
                }
                InstrKind::Assign(lhs, RValue::Index(array, index, _)) => {
                    let mut ledger = ins.clone();
                    if var_flow[label].outs.contains(lhs.defs()) {
                        ledger.add_borrows(
                            lhs,
                            ins.borrow(array, label.clone()) + ins.borrow(index, label.clone()),
                        );
                    } else {
                        ledger.remove(lhs.defs());
                    }
                    ledger
                }
                InstrKind::Assign(lhs, RValue::Struct { name: _, fields }) => {
                    let mut ledger = ins.clone();
                    if var_flow[label].outs.contains(lhs.defs()) {
                        ledger.add_borrows(
                            lhs,
                            fields
                                .values()
                                .map(|field| ins.borrow(field, label.clone()))
                                .sum::<Borrows>(),
                        );
                    } else {
                        ledger.remove(lhs.defs());
                    }
                    ledger
                }
                InstrKind::Assign(lhs, RValue::Array(array)) => {
                    let mut ledger = ins.clone();
                    if var_flow[label].outs.contains(lhs.defs()) {
                        ledger.add_borrows(
                            lhs,
                            array
                                .iter()
                                .map(|item| ins.borrow(item, label.clone()))
                                .sum::<Borrows>(),
                        );
                    } else {
                        ledger.remove(lhs.defs());
                    }
                    ledger
                }
                InstrKind::Call {
                    name: _,
                    args,
                    destination: Some(destination),
                } => {
                    let mut res = ins.clone() - destination;
                    if var_flow[label].outs.contains(destination) {
                        res = res
                            + (
                                destination.clone(),
                                args.iter()
                                    .map(|arg| ins.borrow(arg, label.clone()))
                                    .sum::<Borrows>(),
                            );
                    }
                    res
                }
                InstrKind::Branch(_)
                | InstrKind::Call {
                    destination: None, ..
                } => ins.clone(),
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

/// Check for the last-variable-use optimization on the use of `varmode`.
///
/// Inputs:
/// * `varmode` to optimize
/// * `outs`: set of variables that are alive at the end of the instruction that
///   uses that `varmode`.
///
/// Mutates in place `varmode` to switch its mode to `Moved` if it's the last
/// use of the variable.
fn optimize_last_use(varmode: &mut VarMode, outs: &Set<Var>) {
    match varmode.mode {
        Mode::Cloned => {
            if !outs.contains(&varmode.var) {
                varmode.mode = Mode::Moved;
            }
        }
        Mode::Borrowed | Mode::MutBorrowed => {
            if !outs.contains(&varmode.var) {
                varmode.mode = Mode::Moved;
            }
        }
        Mode::Moved => (),
    }
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
    // Compute the two analyses
    let var_flow = var_liveness(&cfg, entry_l, exit_l);
    let loan_flow = loan_liveness(&cfg, entry_l, exit_l, &var_flow);

    // Checking last variable use and replace with a move
    for (label, instr) in cfg.bfs_mut(entry_l, false) {
        match &mut instr.kind {
            InstrKind::Noop | InstrKind::Declare(_) | InstrKind::Branch(_) => (),
            InstrKind::Assign(_, RValue::Var(rhs)) => {
                optimize_last_use(rhs, &var_flow[&label].outs);
            }
            InstrKind::Assign(_, RValue::Struct { name: _, fields }) => {
                for field in fields.values_mut() {
                    optimize_last_use(field, &var_flow[&label].outs);
                }
            }
            InstrKind::Assign(_, RValue::Array(items)) => {
                for item in items.iter_mut() {
                    optimize_last_use(item, &var_flow[&label].outs);
                }
            }
            InstrKind::Assign(_, RValue::Index(array, index, mode)) => {
                let outs = &var_flow[&label].outs;
                optimize_last_use(array, outs);
                optimize_last_use(index, outs);

                // We can optimize last use only if the `array` refers to loans that are not
                // valid afterwards.
                let borrows = &loan_flow[&label].ins[&array.var];
                if borrows.iter().all(|borrow| !outs.contains(&borrow.var)) {
                    *mode = Mode::Moved;
                }
            }
            InstrKind::Assign(_, _) => (),
            InstrKind::Call {
                name: _,
                args,
                destination: _,
            } => {
                for arg in args {
                    optimize_last_use(arg, &var_flow[&label].outs);
                }
            }
        }
    }

    // List all invalidated borrows.
    let mut invalidated: Borrows = Borrows::new();
    for (label, instr) in cfg.bfs(entry_l, false) {
        for lhs in instr.mutated_var() {
            for borrow in loan_flow[label].ins.borrows(lhs) {
                invalidated.insert(borrow.clone());
            }
        }
    }

    // Transform all invalidated borrows into
    for Borrow { label, var } in invalidated {
        cfg[&label].force_clone(&var);
    }

    cfg
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mir::*;

    #[test]
    fn test_liveness() {
        let mut cfg = Cfg::default();
        let stm = Stratum::static_stm();

        // Variables
        let x = Var::from("x");
        let x_def = vardef("x", Ty::IntT, stm);
        let y = Var::from("y");
        let y_def = vardef("y", Ty::IntT, stm);

        // CFG
        let l = cfg.add_block(
            [
                instr(InstrKind::Declare(y_def), stm),
                instr(
                    InstrKind::Assign(y.clone().into(), RValue::Integer(42.into())),
                    stm,
                ),
                instr(InstrKind::Declare(x_def), stm),
                instr(
                    InstrKind::Assign(x.into(), RValue::Var(VarMode::refed(y.clone()))),
                    stm,
                ),
                instr(InstrKind::Noop, stm),
            ],
            (),
        );

        let analysis = var_liveness(&cfg, &l[0], l.last().unwrap());

        // Entry and exit are trivial
        assert_eq!(analysis[&l[0]].ins.len(), 0);
        assert_eq!(analysis[&l[4]].ins.len(), 0);
        assert_eq!(analysis[&l[4]].outs.len(), 0);

        // During l2, we still need y
        assert_eq!(analysis[&l[2]].ins, Set::from_iter([y.clone()]));
        assert_eq!(analysis[&l[2]].outs, Set::from_iter([y.clone()]));

        // During l3, we still need y but not afterwards anymore
        assert_eq!(analysis[&l[3]].ins, Set::from_iter([y]));
        assert_eq!(analysis[&l[3]].outs.len(), 0);
    }

    /// Checks with a simple example that we clone values when there is a
    /// loan invalidation.
    #[test]
    fn test_clone_on_invalidation() {
        let mut cfg = Cfg::default();
        let stm = Stratum::static_stm();

        // Variables
        let x = Var::from("x");
        let x_def = vardef("x", Ty::IntT, stm);
        let y = Var::from("y");
        let y_def = vardef("y", Ty::IntT, stm);

        // CFG
        let l = cfg.add_block(
            [
                instr(InstrKind::Declare(y_def), stm),
                instr(
                    InstrKind::Assign(y.clone().into(), RValue::Integer(42.into())),
                    stm,
                ),
                instr(InstrKind::Declare(x_def), stm),
                instr(
                    InstrKind::Assign(x.clone().into(), RValue::Var(VarMode::refed(y.clone()))), // We assign y to x
                    stm,
                ),
                instr(
                    InstrKind::Call {
                        name: "+".to_string(),
                        args: vec![VarMode::refed(y.clone()), VarMode::refed(y.clone())],  /* We mutate y
                                                                                        * afterwards,
                                                                                        * invalidating
                                                                                        * the loan because we also... */
                        destination: Some(y),
                    },
                    stm,
                ),
                instr(
                    InstrKind::Call {
                        name: "print".to_string(),
                        args: vec![VarMode::refed(x)], // ...use x after so x is live
                        destination: None,
                    },
                    stm,
                ),
                instr(InstrKind::Noop, stm),
            ],
            (),
        );

        let cfg = liveness(cfg, &l[0], l.last().unwrap());
        assert!(
            matches!(
                cfg[&l[3]].kind,
                InstrKind::Assign(
                    _,
                    RValue::Var(VarMode {
                        mode: Mode::Cloned,
                        ..
                    })
                )
            ),
            "y should be cloned here"
        );
    }

    /// Checks with a simple example that we DON'T clone if the value is not
    /// live afterwards.
    #[test]
    fn test_no_clone_if_not_live() {
        let mut cfg = Cfg::default();
        let stm = Stratum::static_stm();

        // Variables
        let x = Var::from("x");
        let x_def = vardef("x", Ty::IntT, stm);
        let y = Var::from("y");
        let y_def = vardef("y", Ty::IntT, stm);

        // CFG
        let l = cfg.add_block(
            [
                instr(InstrKind::Declare(y_def), stm),
                instr(
                    InstrKind::Assign(y.clone().into(), RValue::Integer(42.into())),
                    stm,
                ),
                instr(InstrKind::Declare(x_def), stm),
                instr(
                    InstrKind::Assign(x.into(), RValue::Var(VarMode::refed(y.clone()))), /* We assign y
                                                                                   * to x */
                    stm,
                ),
                instr(
                    InstrKind::Call {
                        name: "+".to_string(),
                        args: vec![VarMode::refed(y.clone()), VarMode::refed(y.clone())],  /* We mutate y
                                                                                        * afterwards,
                                                                                        * invalidating
                                                                                        * BUT don't invalidate since x
                                                                                        * is not live
                                                                                        * anymore. */
                        destination: Some(y),
                    },
                    stm,
                ),
                instr(InstrKind::Noop, stm),
            ],
            (),
        );

        let cfg = liveness(cfg, &l[0], l.last().unwrap());
        assert!(
            matches!(
                cfg[&l[3]].kind,
                InstrKind::Assign(
                    _,
                    RValue::Var(VarMode {
                        mode: Mode::Borrowed,
                        ..
                    })
                ),
            ),
            "y should be taken by reference here"
        );
    }

    /// Checks with a simple example that we move the value if not used
    /// afterwards.
    #[test]
    fn test_move_if_not_live() {
        let mut cfg = Cfg::default();
        let stm = Stratum::static_stm();

        // Variables
        let x = Var::from("x");
        let x_def = vardef("x", Ty::IntT, stm);
        let y = Var::from("y");
        let y_def = vardef("y", Ty::IntT, stm);

        // CFG
        let l = cfg.add_block(
            [
                instr(InstrKind::Declare(y_def), stm),
                instr(
                    InstrKind::Assign(y.clone().into(), RValue::Integer(42.into())),
                    stm,
                ),
                instr(InstrKind::Declare(x_def), stm),
                instr(
                    InstrKind::Assign(x.into(), RValue::Var(VarMode::refed(y.clone()))), /* We assign y
                                                                                   * to x */
                    stm,
                ),
                instr(InstrKind::Assign(y.into(), RValue::Integer(36.into())), stm), /* We mutate y
                                                                               * but we don't
                                                                               * need y,
                                                                               * so x can own it */
                instr(InstrKind::Noop, stm),
            ],
            (),
        );

        let cfg = liveness(cfg, &l[0], l.last().unwrap());
        assert!(
            matches!(
                cfg[&l[3]].kind,
                InstrKind::Assign(
                    _,
                    RValue::Var(VarMode {
                        mode: Mode::Moved,
                        ..
                    })
                ),
            ),
            "y should be moved here"
        );
    }
}