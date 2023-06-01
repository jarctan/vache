//! Implementing the liveness analysis algorithm.

use super::borrow::Borrows;
use super::flow::Flow;
use crate::borrowing::borrow::Borrow;
use crate::borrowing::ledger::Ledger;
use crate::mir::{Cfg, CfgI, CfgLabel, InstrKind, Mode, RValue, Var, VarMode};
use crate::utils::set::Set;

/// Variable liveness analysis.
///
/// Takes as arguments:
/// * The CFG.
/// * The entry label in the CFG.
///
/// Returns a map of live variables at the entry and exit of each node in the
/// CFG.
pub fn var_liveness<'ctx>(cfg: &CfgI<'ctx>, entry_l: CfgLabel) -> Cfg<Flow<Set<Var<'ctx>>>> {
    // Bootstrap with empty environments.
    let mut var_flow: Cfg<Flow<Set<Var>>> = cfg.map_ref(|_, _| Flow::default(), |_| ());

    let mut updated = true;

    // Compute the fixpoint, iteratively.
    while updated {
        updated = false;

        for (label, instr) in cfg.postorder(entry_l) {
            let successors = cfg.neighbors(label);

            let outs: Set<Var> = successors.map(|x| var_flow[&x].ins.clone()).sum();
            let ins: Set<Var> = match &instr.kind {
                InstrKind::Noop => outs.clone(),
                InstrKind::Declare(var) => outs.clone() - &var.name,
                InstrKind::Assign(lhs, RValue::Unit)
                | InstrKind::Assign(lhs, RValue::String(_))
                | InstrKind::Assign(lhs, RValue::Integer(_)) => {
                    outs.clone() - lhs.defs() + lhs.uses()
                }
                InstrKind::Assign(lhs, RValue::Var(rhs)) => {
                    outs.clone() - lhs.defs() + lhs.uses() + rhs.var
                }
                InstrKind::Assign(lhs, RValue::MovedVar(rhs)) => {
                    outs.clone() - lhs.defs() + lhs.uses() + *rhs
                }
                InstrKind::Assign(lhs, RValue::Field(rhs, _)) => {
                    outs.clone() - lhs.defs() + lhs.uses() + *rhs
                }
                InstrKind::Assign(lhs, RValue::Index(array, index)) => {
                    outs.clone() - lhs.defs() + lhs.uses() + *array + *index
                }
                InstrKind::Assign(lhs, RValue::Struct { name: _, fields }) => {
                    outs.clone() - lhs.defs()
                        + lhs.uses()
                        + Set::from_iter(fields.values().copied())
                }
                InstrKind::Assign(lhs, RValue::Array(array)) => {
                    outs.clone() - lhs.defs() + lhs.uses() + Set::from_iter(array.iter().copied())
                }
                InstrKind::Call {
                    name: _,
                    args,
                    destination,
                } => {
                    let res =
                        outs.clone() - destination.as_ref() + Set::from_iter(args.iter().copied());
                    res
                }
                InstrKind::Branch(v) => outs.clone() + *v,
            };

            let flow = Flow { ins, outs };
            if var_flow[&label] != flow {
                var_flow[&label] = flow;
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
/// * The variable liveliness analysis for that CFG.
///
/// Returns a map of live loans at the entry and exit of each node in the
/// CFG.
fn loan_liveness<'ctx>(
    cfg: &CfgI<'ctx>,
    entry_l: CfgLabel,
    var_flow: &Cfg<Flow<Set<Var<'ctx>>>>,
) -> Cfg<Flow<Ledger<'ctx>>> {
    let out_of_scope: Cfg<Set<Var>> =
        var_flow.map_ref(|_, flow| flow.ins.clone() - &flow.outs, |_| ());

    // Same, for loans.
    let mut loan_flow: Cfg<Flow<Ledger>> = cfg.map_ref(|_, _| Flow::default(), |_| ());

    let mut updated = true;

    while updated {
        updated = false;

        for (label, instr) in cfg.postorder(entry_l).rev() {
            let predecessors = cfg.preneighbors(label);
            let ins: Ledger = predecessors.map(|x| loan_flow[&x].outs.clone()).sum();
            let outs: Ledger = match &instr.kind {
                InstrKind::Noop => ins.clone(),
                InstrKind::Declare(var) => ins.clone() - &var.name,
                InstrKind::Assign(lhs, RValue::Unit)
                | InstrKind::Assign(lhs, RValue::String(_))
                | InstrKind::Assign(lhs, RValue::Integer(_)) => ins.clone() - lhs.defs(),
                InstrKind::Assign(lhs, RValue::MovedVar(rhs)) => {
                    let mut ledger = ins.clone();
                    if var_flow[&label].outs.contains(lhs.defs()) {
                        ledger.add_borrows(lhs, ins.move_var(*rhs));
                    } else {
                        ledger.remove(lhs.defs());
                    }
                    ledger
                }
                InstrKind::Assign(lhs, RValue::Var(rhs)) => {
                    let mut ledger = ins.clone();
                    if var_flow[&label].outs.contains(lhs.defs()) {
                        ledger.add_borrows(lhs, ins.borrow(rhs, label));
                    } else {
                        ledger.remove(lhs.defs());
                    }
                    ledger
                }
                InstrKind::Assign(lhs, RValue::Field(rhs, _)) => {
                    let mut ledger = ins.clone();
                    if var_flow[&label].outs.contains(lhs.defs()) {
                        ledger.add_borrows(lhs, ins.move_var(*rhs));
                    } else {
                        ledger.remove(lhs.defs());
                    }
                    ledger
                }
                InstrKind::Assign(lhs, RValue::Index(array, index)) => {
                    let mut ledger = ins.clone();
                    if var_flow[&label].outs.contains(lhs.defs()) {
                        ledger.add_borrows(lhs, ins.move_var(*array) + ins.move_var(*index));
                    } else {
                        ledger.remove(lhs.defs());
                    }
                    ledger
                }
                InstrKind::Assign(lhs, RValue::Struct { name: _, fields }) => {
                    let mut ledger = ins.clone();
                    if var_flow[&label].outs.contains(lhs.defs()) {
                        ledger.add_borrows(
                            lhs,
                            fields
                                .values()
                                .map(|field| ins.move_var(*field))
                                .sum::<Borrows>(),
                        );
                    } else {
                        ledger.remove(lhs.defs());
                    }
                    ledger
                }
                InstrKind::Assign(lhs, RValue::Array(array)) => {
                    let mut ledger = ins.clone();
                    if var_flow[&label].outs.contains(lhs.defs()) {
                        ledger.add_borrows(
                            lhs,
                            array
                                .iter()
                                .map(|item| ins.move_var(*item))
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
                    if var_flow[&label].outs.contains(destination) {
                        res = res
                            + (
                                *destination,
                                args.iter().map(|arg| ins.move_var(*arg)).sum::<Borrows>(),
                            );
                    }
                    res
                }
                InstrKind::Branch(_)
                | InstrKind::Call {
                    destination: None, ..
                } => ins.clone(),
            } - &out_of_scope[&label];

            let flow = Flow { ins, outs };
            if loan_flow[&label] != flow {
                loan_flow[&label] = flow;
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
fn optimize_last_use<'ctx>(varmode: &mut VarMode<'ctx>, outs: &Set<Var<'ctx>>) {
    match varmode.mode {
        Mode::Cloned => {
            if !outs.contains(&varmode.var) {
                *varmode.mode = Mode::Moved;
            }
        }
        Mode::Borrowed | Mode::MutBorrowed | Mode::SBorrowed => {
            if !outs.contains(&varmode.var) {
                *varmode.mode = Mode::Moved;
            }
        }
        Mode::Moved => (),
        Mode::Assigning => (),
    }
}

/// Liveness analysis.
///
/// Takes as arguments:
/// * The CFG.
/// * The entry label in the CFG.
///
/// Performs liveliness analysis, determining which borrows are invalidated.
pub fn liveness<'ctx>(mut cfg: CfgI<'ctx>, entry_l: CfgLabel) -> CfgI<'ctx> {
    // Compute the two analyses
    let var_flow = var_liveness(&cfg, entry_l);
    let loan_flow = loan_liveness(&cfg, entry_l, &var_flow);

    // Checking last variable use and replace with a move
    for (label, instr) in cfg.bfs_mut(entry_l, false) {
        match &mut instr.kind {
            InstrKind::Noop | InstrKind::Declare(_) | InstrKind::Branch(_) => (),
            InstrKind::Assign(_, RValue::Var(rhs)) => {
                optimize_last_use(rhs, &var_flow[&label].outs);
            }
            InstrKind::Assign(_, RValue::Array(..)) => (),
            InstrKind::Assign(_, RValue::Struct { .. }) => (),
            InstrKind::Assign(_, RValue::Index(..))
            | InstrKind::Assign(..)
            | InstrKind::Call { .. } => (),
        }
    }

    // List all invalidated borrows.
    let mut invalidated: Borrows<'ctx> = Borrows::new();
    for (label, instr) in cfg.bfs(entry_l, false) {
        for lhs in instr.mutated_var() {
            for borrow in loan_flow[&label].ins.borrows(lhs) {
                invalidated.insert(*borrow);
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
    use std::default::default;

    use super::*;
    use crate::mir::*;

    #[test]
    fn test_liveness() {
        let mut cfg = Cfg::default();
        let stm = Stratum::static_stm();

        // Constants
        let forty_two = 42.into();

        // Variables
        let x = Var::from("x");
        let x_def = vardef("x", Ty::IntT, stm);
        let y = Var::from("y");
        let y_def = vardef("y", Ty::IntT, stm);

        let mut y_mode = default();

        // CFG
        let l = cfg.add_block(
            [
                instr(InstrKind::Declare(y_def), stm),
                instr(
                    InstrKind::Assign(y.into(), RValue::Integer(&forty_two)),
                    stm,
                ),
                instr(InstrKind::Declare(x_def), stm),
                instr(
                    InstrKind::Assign(x.into(), RValue::Var(VarMode::new(y, &mut y_mode))),
                    stm,
                ),
                instr(InstrKind::Noop, stm),
            ],
            (),
        );

        let analysis = var_liveness(&cfg, l[0]);

        // Entry and exit are trivial
        assert_eq!(analysis[&l[0]].ins.len(), 0);
        assert_eq!(analysis[&l[4]].ins.len(), 0);
        assert_eq!(analysis[&l[4]].outs.len(), 0);

        // During l2, we still need y
        assert_eq!(analysis[&l[2]].ins, Set::from_iter([y]));
        assert_eq!(analysis[&l[2]].outs, Set::from_iter([y]));

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

        // Constants
        let forty_two = 42.into();

        // Variables
        let x = Var::from("x");
        let x_def = vardef("x", Ty::IntT, stm);
        let y = Var::from("y");
        let y_def = vardef("y", Ty::IntT, stm);

        let mut y_mode1 = default();

        // CFG
        let l = cfg.add_block(
            [
                instr(InstrKind::Declare(y_def), stm),
                instr(
                    InstrKind::Assign(y.into(), RValue::Integer(&forty_two)),
                    stm,
                ),
                instr(InstrKind::Declare(x_def), stm),
                instr(
                    InstrKind::Assign(
                        x.into(),
                        RValue::Var(VarMode::new(y, &mut y_mode1)),
                    ), // We assign y to x
                    stm,
                ),
                instr(
                    InstrKind::Call {
                        name: "+",
                        args: vec![y, y], /* We mutate y
                                                           * afterwards,
                                                           * invalidating
                                                           * the loan because we also... */
                        destination: Some(y),
                    },
                    stm,
                ),
                instr(
                    InstrKind::Call {
                        name: "print",
                        args: vec![x], // ...use x after so x is live
                        destination: None,
                    },
                    stm,
                ),
                instr(InstrKind::Noop, stm),
            ],
            (),
        );

        let cfg = liveness(cfg, l[0]);
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

        // Constants
        let forty_two = 42.into();

        // Variables
        let x = Var::from("x");
        let x_def = vardef("x", Ty::IntT, stm);
        let y = Var::from("y");
        let y_def = vardef("y", Ty::IntT, stm);

        let mut y_mode1 = default();

        // CFG
        let l =
            cfg.add_block(
                [
                    instr(InstrKind::Declare(y_def), stm),
                    instr(
                        InstrKind::Assign(y.into(), RValue::Integer(&forty_two)),
                        stm,
                    ),
                    instr(InstrKind::Declare(x_def), stm),
                    instr(
                        InstrKind::Assign(
                            x.into(),
                            RValue::Var(VarMode::new(y, &mut y_mode1)),
                        ), /* We assign y to x */
                        stm,
                    ),
                    instr(
                        InstrKind::Call {
                            name: "+",
                            args: vec![y, y], /* We mutate y
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

        let cfg = liveness(cfg, l[0]);
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

        // Constants
        let forty_two = 42.into();
        let thirty_six = 36.into();

        // Variables
        let x = Var::from("x");
        let x_def = vardef("x", Ty::IntT, stm);
        let y = Var::from("y");
        let y_def = vardef("y", Ty::IntT, stm);

        let mut y_mode = Mode::Borrowed;

        // CFG
        let l = cfg.add_block(
            [
                instr(InstrKind::Declare(y_def), stm),
                instr(
                    InstrKind::Assign(y.into(), RValue::Integer(&forty_two)),
                    stm,
                ),
                instr(InstrKind::Declare(x_def), stm),
                instr(
                    InstrKind::Assign(x.into(), RValue::Var(VarMode::new(y, &mut y_mode))), /* We assign y to x */
                    stm,
                ),
                instr(InstrKind::Assign(y.into(), RValue::Integer(&thirty_six)), stm), /* We mutate y
                                                                               * but we don't
                                                                               * need y,
                                                                               * so x can own it */
                instr(InstrKind::Noop, stm),
            ],
            (),
        );

        let cfg = liveness(cfg, l[0]);
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
