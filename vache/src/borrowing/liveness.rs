//! Implementing the liveness analysis algorithm.

use super::borrow::Borrows;
use super::flow::Flow;
use super::tree::LocTree;
use crate::borrowing::borrow::Borrow;
use crate::borrowing::ledger::Ledger;
use crate::mir::{Cfg, CfgI, CfgLabel, InstrKind, Loc, Mode, Place, RValue};

/// Variable liveness analysis.
///
/// Takes as arguments:
/// * The CFG.
/// * The entry label in the CFG.
///
/// Returns a map of live variables at the entry and exit of each node in the
/// CFG.
pub fn var_liveness<'ctx>(cfg: &CfgI<'ctx>, entry_l: CfgLabel) -> Cfg<Flow<LocTree<'ctx, ()>>> {
    // Bootstrap with empty environments.
    let mut var_flow: Cfg<Flow<LocTree<()>>> = cfg.map_ref(|_, _| Flow::default(), |_| ());

    let mut updated = true;

    // Compute the fixpoint, iteratively.
    while updated {
        updated = false;

        for (label, instr) in cfg.postorder(entry_l) {
            let successors = cfg.neighbors(label);

            let outs: LocTree<()> = successors.map(|x| var_flow[&x].ins.clone()).sum();
            let ins: LocTree<()> = match &instr.kind {
                InstrKind::Noop => outs.clone(),
                InstrKind::Declare(var) => outs.clone() - Loc::from(var.name()),
                InstrKind::Assign(lhs, RValue::Unit)
                | InstrKind::Assign(lhs, RValue::Bool(_))
                | InstrKind::Assign(lhs, RValue::String(_))
                | InstrKind::Assign(lhs, RValue::Integer(_)) => {
                    outs.clone() - lhs.place().def() + lhs.place().uses_as_lhs()
                }
                InstrKind::Assign(lhs, RValue::Place(rhs)) => {
                    outs.clone() - lhs.place().def()
                        + lhs.place().uses_as_lhs()
                        + rhs.place.uses_as_rhs()
                }
                InstrKind::Assign(lhs, RValue::Struct { name: _, fields }) => {
                    outs.clone() - lhs.place().def()
                        + lhs.place().uses_as_lhs()
                        + fields.values().flat_map(|field| field.place.uses_as_rhs())
                }
                InstrKind::Assign(
                    lhs,
                    RValue::Variant {
                        enun: _,
                        variant: _,
                        args,
                    },
                ) => {
                    outs.clone() - lhs.place().def()
                        + lhs.place().uses_as_lhs()
                        + args.iter().flat_map(|arg| arg.place.uses_as_rhs())
                }
                InstrKind::Assign(lhs, RValue::Array(array)) => {
                    outs.clone() - lhs.place().def()
                        + lhs.place().uses_as_lhs()
                        + array.iter().flat_map(|item| item.place.uses_as_rhs())
                }
                InstrKind::Assign(lhs, RValue::Range(start, end)) => {
                    outs.clone() - lhs.place().def()
                        + lhs.place().uses_as_lhs()
                        + start.place.uses_as_rhs()
                        + end.place.uses_as_rhs()
                }
                InstrKind::Call {
                    name: _,
                    args,
                    destination,
                } => {
                    outs.clone() - destination.as_ref().map(|x| *x.loc())
                        + args
                            .iter()
                            .flat_map(|x| x.place().uses_as_rhs().into_iter())
                }
                InstrKind::Branch(v) => outs.clone() + Loc::from(v),
                InstrKind::Return(v) => outs.clone() + Loc::from(v),
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
    var_flow: &Cfg<Flow<LocTree<'ctx, ()>>>,
) -> Cfg<Flow<Ledger<'ctx>>> {
    let out_of_scope: Cfg<LocTree<()>> =
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
                InstrKind::Declare(var) => ins.clone() - Place::from(var.name()),
                InstrKind::Assign(lhs, RValue::Unit)
                | InstrKind::Assign(lhs, RValue::String(_))
                | InstrKind::Assign(lhs, RValue::Bool(_))
                | InstrKind::Assign(lhs, RValue::Integer(_)) => ins.clone() - lhs.place(),
                InstrKind::Assign(lhs, RValue::Place(rhs)) => {
                    let mut ledger = ins.clone();
                    if var_flow[&label].outs.contains(lhs.loc()) {
                        let borrows = ledger.borrow(lhs.place(), rhs, label);
                        ledger.set_borrows(lhs.place(), borrows);
                    } else {
                        ledger.flush_place(lhs.place());
                    }
                    ledger
                }
                InstrKind::Assign(lhs, RValue::Struct { name: _, fields }) => {
                    let mut ledger = ins.clone();
                    if var_flow[&label].outs.contains(lhs.loc()) {
                        let borrows = fields
                            .values()
                            .map(|field| ledger.borrow(lhs.place(), field, label))
                            .sum::<Borrows>();
                        ledger.set_borrows(lhs.place(), borrows);
                    } else {
                        ledger.flush_place(lhs.place());
                    }
                    ledger
                }
                InstrKind::Assign(
                    lhs,
                    RValue::Variant {
                        enun: _,
                        variant: _,
                        args,
                    },
                ) => {
                    let mut ledger = ins.clone();
                    if var_flow[&label].outs.contains(lhs.loc()) {
                        let borrows = args
                            .iter()
                            .map(|arg| ledger.borrow(lhs.place(), arg, label))
                            .sum::<Borrows>();
                        ledger.set_borrows(lhs.place(), borrows);
                    } else {
                        ledger.flush_place(lhs.place());
                    }
                    ledger
                }
                InstrKind::Assign(lhs, RValue::Array(array)) => {
                    let mut ledger = ins.clone();
                    if var_flow[&label].outs.contains(lhs.loc()) {
                        let borrows = array
                            .iter()
                            .map(|item| ledger.borrow(lhs.place(), item, label))
                            .sum::<Borrows>();
                        ledger.set_borrows(lhs.place(), borrows);
                    } else {
                        ledger.flush_place(lhs.place());
                    }
                    ledger
                }
                InstrKind::Assign(lhs, RValue::Range(start, end)) => {
                    let mut ledger = ins.clone();
                    if var_flow[&label].outs.contains(lhs.loc()) {
                        let borrows = ledger.borrow(lhs.place(), start, label)
                            + ledger.borrow(lhs.place(), end, label);
                        ledger.set_borrows(lhs.place(), borrows);
                    } else {
                        ledger.flush_place(lhs.place());
                    }
                    ledger
                }
                InstrKind::Call {
                    name: _,
                    args,
                    destination: Some(destination),
                } => {
                    let mut ledger = ins.clone() - destination.place();
                    if var_flow[&label].outs.contains(destination.loc()) {
                        let borrows = args
                            .iter()
                            .map(|arg| ledger.borrow(destination.place(), arg, label))
                            .sum::<Borrows>();
                        ledger = ledger + (destination.place(), borrows);
                    }
                    ledger
                }
                InstrKind::Branch(_)
                | InstrKind::Return(_)
                | InstrKind::Call {
                    destination: None, ..
                } => ins.clone(),
            } - out_of_scope[&label].get_all_locs();

            let flow = Flow { ins, outs };
            if loan_flow[&label] != flow {
                loan_flow[&label] = flow;
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
///
/// Performs liveliness analysis, determining which borrows are invalidated.
pub fn liveness<'ctx>(mut cfg: CfgI<'ctx>, entry_l: CfgLabel, exit_l: CfgLabel) -> CfgI<'ctx> {
    // Compute the two analyses
    let var_flow = var_liveness(&cfg, entry_l);

    let mut updates = true;
    while updates {
        // Reset update flag
        updates = false;

        // Compute loan liveness for the first time
        let loan_flow = loan_liveness(&cfg, entry_l, &var_flow);

        // Check last variable use and replace with a move
        for (label, instr) in cfg.bfs_mut(entry_l, false) {
            let outs = &var_flow[&label].outs;
            let ledger = &loan_flow[&label].ins;
            match &mut instr.kind {
                InstrKind::Noop
                | InstrKind::Declare(_)
                | InstrKind::Branch(_)
                | InstrKind::Return(_) => (),
                InstrKind::Assign(_, RValue::Place(place)) => match place.mode() {
                    Mode::Cloned => {
                        if !outs.contains(place.loc())
                            && !ledger.has_loans(place.place())
                            && place.mode() != Mode::Moved
                        {
                            place.make_moved();
                            updates = true;
                        }
                    }
                    Mode::Borrowed | Mode::MutBorrowed | Mode::SBorrowed => {
                        if !outs.contains(place.loc())
                            && !ledger.has_loans(place.place())
                            && place.mode() != Mode::Moved
                        {
                            place.make_moved();
                            updates = true;
                        }
                    }
                    Mode::Moved => (),
                    Mode::Assigning => (),
                },
                InstrKind::Assign(_, RValue::Array(items)) => {
                    for item in items {
                        if !outs.contains(item.loc())
                            && !ledger.has_loans(item.place())
                            && item.mode() != Mode::Moved
                        {
                            item.make_moved();
                            updates = true;
                        }
                    }
                }
                InstrKind::Assign(_, RValue::Range(start, end)) => {
                    for item in [start, end] {
                        if !outs.contains(item.loc())
                            && !ledger.has_loans(item.place())
                            && item.mode() != Mode::Moved
                        {
                            item.make_moved();
                            updates = true;
                        }
                    }
                }
                InstrKind::Assign(_, RValue::Struct { name: _, fields }) => {
                    for field in fields.values_mut() {
                        if !outs.contains(field.loc())
                            && !ledger.has_loans(field.place())
                            && field.mode() != Mode::Moved
                        {
                            field.make_moved();
                            updates = true;
                        }
                    }
                }
                InstrKind::Assign(
                    _,
                    RValue::Variant {
                        enun: _,
                        variant: _,
                        args,
                    },
                ) => {
                    for arg in args {
                        if !outs.contains(arg.loc())
                            && !ledger.has_loans(arg.place())
                            && arg.mode() != Mode::Moved
                        {
                            arg.make_moved();
                            updates = true;
                        }
                    }
                }
                InstrKind::Call {
                    name: _,
                    args,
                    destination: _,
                } => {
                    for arg in args {
                        if !outs.contains(arg.loc())
                            && !ledger.has_loans(arg.place())
                            && arg.mode() != Mode::Moved
                        {
                            arg.make_moved();
                            updates = true;
                        }
                    }
                }
                InstrKind::Assign(
                    _,
                    RValue::Unit | RValue::Bool(..) | RValue::Integer(..) | RValue::String(..),
                ) => (),
            }
        }

        // If we did a last var use optimization, update the loan liveness.
        // Otherwise, keep it as is.
        let loan_flow = if updates {
            loan_liveness(&cfg, entry_l, &var_flow)
        } else {
            loan_flow
        };

        // List all borrows that are invalidated by mutation of the variable afterwards.
        let mut invalidated: Borrows<'ctx> = Borrows::new();
        for (label, instr) in cfg.bfs(entry_l, false) {
            for lhs in instr.mutated_place() {
                for &borrow in loan_flow[&label].ins.loans(lhs) {
                    invalidated.insert(borrow);
                }
            }
        }
        // Extend with those of the ledger. Take the exit_l to have them all
        invalidated.extend(loan_flow[&exit_l].outs.invalidations());

        // If we made no update, we reached the fixpoint of our optimizations, and there
        // is no invalidations anymore so it's fine
        updates |= !invalidated.is_empty();

        // Transform all invalidated borrows into clones
        for Borrow { label, place, .. } in invalidated {
            cfg[&label].force_clone(&place);
        }
    }

    cfg
}

#[cfg(test)]
mod tests {
    use std::default::default;

    use super::*;
    use crate::{mir::*, Arena};

    #[test]
    fn test_liveness() {
        let mut cfg = Cfg::default();
        let stm = Stratum::static_stm();
        let arena = Arena::new();

        // Constants
        let forty_two = 42.into();

        // Variables
        let x = VarUse::from("x");
        let x_def = vardef("x", Ty::IntT, stm);
        let x_ptr = Pointer::new(&arena, arena.alloc(x.into()));
        let y = VarUse::from("y");
        let y_def = vardef("y", Ty::IntT, stm);
        let y_ptr = Pointer::new(&arena, arena.alloc(y.into()));

        let mut y_mode = default();

        // CFG
        let l = cfg.add_block(
            [
                instr(InstrKind::Declare(y_def), stm),
                instr(InstrKind::Assign(y_ptr, RValue::Integer(&forty_two)), stm),
                instr(InstrKind::Declare(x_def), stm),
                instr(
                    InstrKind::Assign(x_ptr, RValue::Place(Reference::new(y_ptr, &mut y_mode))),
                    stm,
                ),
                instr(InstrKind::Noop, stm),
            ],
            (),
        );

        let analysis = var_liveness(&cfg, l[0]);

        // Entry and exit are trivial
        assert!(analysis[&l[0]].ins.get_all_locs().next().is_none());
        assert!(analysis[&l[4]].ins.get_all_locs().next().is_none());
        assert!(analysis[&l[4]].outs.get_all_locs().next().is_none());

        // During l2, we still need y
        assert_eq!(
            analysis[&l[2]].outs.get_all_locs().collect::<Vec<_>>(),
            [y.into()]
        );
        assert_eq!(
            analysis[&l[2]].ins.get_all_locs().collect::<Vec<_>>(),
            [y.into()]
        );

        // During l3, we still need y but not afterwards anymore
        assert_eq!(
            analysis[&l[3]].ins.get_all_locs().collect::<Vec<_>>(),
            [y.into()]
        );
        assert!(analysis[&l[3]].outs.get_all_locs().next().is_none());
    }

    /// Checks with a simple example that we clone values when there is a
    /// loan invalidation.
    #[test]
    fn test_clone_on_invalidation() {
        let mut cfg = Cfg::default();
        let stm = Stratum::static_stm();
        let arena = Arena::new();

        // Constants
        let forty_two = 42.into();

        // Variables
        let x = VarUse::from("x");
        let x_def = vardef("x", Ty::IntT, stm);
        let x_ptr = Pointer::new(&arena, arena.alloc(x.into()));
        let y = VarUse::from("y");
        let y_def = vardef("y", Ty::IntT, stm);
        let y_ptr = Pointer::new(&arena, arena.alloc(y.into()));

        let mut x_mode1 = default();
        let mut y_mode1 = default();
        let mut y_mode2 = default();
        let mut y_mode3 = default();

        // CFG
        let l =
            cfg.add_block(
                [
                    instr(InstrKind::Declare(y_def), stm),
                    instr(InstrKind::Assign(y_ptr, RValue::Integer(&forty_two)), stm),
                    instr(InstrKind::Declare(x_def), stm),
                    instr(
                        InstrKind::Assign(
                            x_ptr,
                            RValue::Place(Reference::new(y_ptr, &mut y_mode1)),
                        ), // We assign y to x
                        stm,
                    ),
                    instr(
                        InstrKind::Call {
                            name: "+".into(),
                            args: vec![
                                Reference::new(y_ptr, &mut y_mode2),
                                Reference::new(y_ptr, &mut y_mode3),
                            ], /* We mutate y
                                * afterwards,
                                * invalidating
                                * the loan because we also... */
                            destination: Some(y_ptr),
                        },
                        stm,
                    ),
                    instr(
                        InstrKind::Call {
                            name: "print".into(),
                            args: vec![Reference::new(x_ptr, &mut x_mode1)], /* ...use x after so
                                                                              * x is live */
                            destination: None,
                        },
                        stm,
                    ),
                    instr(InstrKind::Noop, stm),
                ],
                (),
            );

        let cfg = liveness(cfg, l[0], l[l.len() - 1]);
        assert!(
            matches!(
                cfg[&l[3]].kind,
                InstrKind::Assign(
                    _,
                    RValue::Place(ref rhs)
                )  if rhs.mode() == Mode::Cloned,
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
        let arena = Arena::new();

        // Constants
        let forty_two = 42.into();

        // Variables
        let x = VarUse::from("x");
        let x_def = vardef("x", Ty::IntT, stm);
        let x_ptr = Pointer::new(&arena, arena.alloc(x.into()));
        let y = VarUse::from("y");
        let y_def = vardef("y", Ty::IntT, stm);
        let y_ptr = Pointer::new(&arena, arena.alloc(y.into()));

        let mut x_mode1 = default();
        let mut y_mode1 = default();
        let mut y_mode2 = default();
        let mut y_mode3 = default();

        // CFG
        let l =
            cfg.add_block(
                [
                    instr(InstrKind::Declare(y_def), stm),
                    instr(InstrKind::Assign(y_ptr, RValue::Integer(&forty_two)), stm),
                    instr(InstrKind::Declare(x_def), stm),
                    instr(
                        InstrKind::Assign(
                            x_ptr,
                            RValue::Place(Reference::new(y_ptr, &mut y_mode1)),
                        ), /* We assign y to x */
                        stm,
                    ),
                    instr(
                        // We then use x non mutably so that we do not optimize away the assignment
                        // of x.
                        InstrKind::Call {
                            name: "print".into(),
                            args: vec![Reference::new(x_ptr, &mut x_mode1)],
                            destination: None,
                        },
                        stm,
                    ),
                    instr(
                        InstrKind::Call {
                            name: "+".into(),
                            args: vec![
                                Reference::new(y_ptr, &mut y_mode2),
                                Reference::new(y_ptr, &mut y_mode3),
                            ], /* We mutate y
                                * afterwards,
                                * invalidating
                                * BUT don't invalidate since x
                                * is not live
                                * anymore. */
                            destination: Some(y_ptr),
                        },
                        stm,
                    ),
                ],
                (),
            );

        let cfg = liveness(cfg, l[0], l[l.len() - 1]);
        assert!(
            matches!(
                cfg[&l[3]].kind,
                InstrKind::Assign(
                    _,
                    RValue::Place(ref rhs)
                ) if rhs.mode() == Mode::Borrowed,
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
        let arena = Arena::new();

        // Constants
        let forty_two = 42.into();
        let thirty_six = 36.into();

        // Variables
        let x = VarUse::from("x");
        let x_def = vardef("x", Ty::IntT, stm);
        let x_ptr = Pointer::new(&arena, arena.alloc(x.into()));
        let y = VarUse::from("y");
        let y_def = vardef("y", Ty::IntT, stm);
        let y_ptr = Pointer::new(&arena, arena.alloc(y.into()));

        let mut x_mode1 = default();
        let mut y_mode1 = default();

        // CFG
        let l = cfg.add_block(
            [
                instr(InstrKind::Declare(y_def), stm),
                instr(
                    InstrKind::Assign(y_ptr, RValue::Integer(&forty_two)),
                    stm,
                ),
                instr(InstrKind::Declare(x_def), stm),
                instr(
                    InstrKind::Assign(x_ptr, RValue::Place(Reference::new(y_ptr, &mut y_mode1))), /* We assign y to x */
                    stm,
                ),
                instr(InstrKind::Assign(y_ptr, RValue::Integer(&thirty_six)), stm), /* We mutate y
                                                                               * but we don't
                                                                               * need y,
                                                                               * so x can own it */
                instr(InstrKind::Call { name: "print".into(), args: vec![Reference::new(x_ptr, &mut x_mode1)], destination: None }, stm),
            ],
            (),
        );

        let cfg = liveness(cfg, l[0], l[l.len() - 1]);
        assert!(
            matches!(
                cfg[&l[3]].kind,
                InstrKind::Assign(
                    _,
                    RValue::Place(ref rhs)
                ) if rhs.mode() == Mode::Moved,
            ),
            "y should be moved here (instead it is {:?})",
            cfg[&l[3]].kind
        );
    }
}
