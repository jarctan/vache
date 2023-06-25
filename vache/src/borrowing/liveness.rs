//! Implementing the liveness analysis algorithm.

use std::collections::{HashMap, VecDeque};

use super::borrow::Borrows;
use super::flow::Flow;
use super::tree::LocTree;
use crate::borrowing::borrow::Borrow;
use crate::borrowing::ledger::Ledger;
use crate::codes::BORROW_ERROR;
use crate::mir::{Cfg, CfgI, CfgLabel, InstrKind, Loc, Mode, Stratum, Varname};
use crate::reporter::{Diagnostic, Diagnostics, Reporter};
use crate::utils::set::Set;

/// Variable liveness analysis.
///
/// Takes as arguments:
/// * The CFG.
/// * The entry label in the CFG.
///
/// Returns a map of live variables at the entry and exit of each node in the
/// CFG.
pub fn var_liveness<'ctx>(
    cfg: &CfgI<'ctx>,
    entry_l: CfgLabel,
) -> Cfg<'ctx, Flow<LocTree<'ctx, ()>>> {
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
                InstrKind::Declare(_) => outs.clone(),
                InstrKind::Assign(lhs, _)
                | InstrKind::Call {
                    name: _,
                    args: _,
                    destination: Some(lhs),
                } => {
                    outs.clone() - lhs.place().def()
                        + lhs.place().uses_as_lhs()
                        + instr
                            .references()
                            .flat_map(|item| item.place().uses_as_rhs())
                }
                InstrKind::Call {
                    name: _,
                    args,
                    destination: None,
                } => outs.clone() + args.iter().flat_map(|x| x.place().uses_as_rhs()),
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
/// * The set of variables for each stratum.
///
/// Returns a map of live loans at the entry and exit of each node in the
/// CFG.
fn loan_liveness<'ctx>(
    cfg: &CfgI<'ctx>,
    entry_l: CfgLabel,
    var_flow: &Cfg<Flow<LocTree<'ctx, ()>>>,
    strata: &HashMap<Stratum, Set<Varname<'ctx>>>,
) -> Cfg<'ctx, Flow<Ledger<'ctx>>> {
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
            let mut outs: Ledger = match &instr.kind {
                InstrKind::Declare(_) => ins.clone(),
                InstrKind::Assign(lhs, _)
                | InstrKind::Call {
                    name: _,
                    args: _,
                    destination: Some(lhs),
                } => {
                    let mut ledger = ins.clone();
                    ledger.flush_place(lhs.place(), true);
                    let borrows = instr
                        .references()
                        .map(|reference| ledger.borrow(*lhs.loc(), reference, label))
                        .collect::<Vec<_>>();
                    let borrows = ledger.flatten(borrows.into_iter());
                    ledger.set_borrows(lhs.place(), borrows);
                    ledger
                }
                InstrKind::Branch(_)
                | InstrKind::Return(_)
                | InstrKind::Call {
                    destination: None, ..
                }
                | InstrKind::Noop => ins.clone(),
            };

            // Optionally remove locations that go out of scope at the end of this
            // instruction
            outs.flush_locs(out_of_scope[&label].get_all_locs(), false);

            // Remove locations that can really be destroyed at the end of this instruction
            // To do so, compute the worst case stratum after that instruction, that is the
            // lowest stm that we can reach after that instruction
            let successor_stm = cfg
                .neighbors(label)
                .map(|label| cfg[&label].scope)
                .min()
                .unwrap_or(instr.scope);

            // We need to flush every variable between the successor stratum (not included)
            // and our stratum (included)
            for stm in successor_stm.higher()..=instr.scope {
                if let Some(set) = strata.get(&stm) {
                    outs.flush_locs(set.iter().map(Loc::from), true);
                }
            }

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
/// * The set of variables for each stratum.
///
/// Performs liveliness analysis, determining which borrows are invalidated.
pub fn liveness<'mir, 'ctx>(
    mut cfg: CfgI<'mir>,
    entry_l: CfgLabel,
    exit_l: CfgLabel,
    strata: &HashMap<Stratum, Set<Varname<'mir>>>,
    reporter: &mut Reporter<'ctx>,
) -> Result<CfgI<'mir>, Diagnostics<'ctx>> {
    // Compute the var analysis first
    let var_flow = var_liveness(&cfg, entry_l);

    // Check last variable use and replace with a move
    let mut invalidated = Borrows::new();
    for (label, instr) in cfg.bfs_mut(entry_l, false) {
        let outs = &var_flow[&label].outs;

        // Get all the references of that instruction.
        let references: Vec<_> = instr.references_mut().collect();

        // Get, for each reference, the set of locations that are used by any reference
        // after that one within the same instruction
        // For example, for `print(a, a)`, we will see that we cannot move the first
        // `a`, since it is used by a reference after that first `a`
        // To do that, we compute the set of locations that cannot be moved for each
        // reference in our `references`.
        let mut cannot_move: VecDeque<_> = VecDeque::from([Set::new()]);
        for r in references.iter().rev() {
            let el = cannot_move.back().unwrap().clone() + r.loc();
            cannot_move.push_front(el);
        }
        cannot_move.pop_front();
        debug_assert!(cannot_move.len() == references.len());

        for (i, reference) in references.into_iter().enumerate() {
            match reference.mode() {
                Mode::Borrowed | Mode::MutBorrowed | Mode::SMutBorrowed | Mode::SBorrowed => {
                    let loc = reference.loc();
                    if !outs.contains(loc) && !cannot_move[i].contains(loc) {
                        reference.set_mode(Mode::Moved);
                    }
                }
                Mode::Cloned => (), // We clone, there's a reason fot that. So you can't move
                Mode::Moved => (),  // already moved
                Mode::Assigning => (), // assigning modes can't be moved
            }
        }
    }

    // Now, compute loan analysis
    let loan_flow = loan_liveness(&cfg, entry_l, &var_flow, strata);

    // List all borrows that are invalidated by mutation of the variable afterwards.
    for (label, instr) in cfg.bfs(entry_l, false) {
        for lhs in instr.mutated_place() {
            for borrow in loan_flow[&label].ins.loans(lhs.root()) {
                invalidated.insert(borrow);
            }
        }
    }

    // Extend with the invalidations of the ledger. Take them at exit_l to have them
    // all
    invalidated.extend(loan_flow[&exit_l].outs.invalidations());

    if let Some(unrecoverables) = loan_flow[&exit_l].outs.unrecoverables() {
        for (borrow, contradicts) in unrecoverables {
            let mut labels = vec![borrow.span.into()];
            for contradicted in contradicts {
                labels.push(contradicted.span.as_secondary_label().with_message(format!(
                    "contradicts this {} borrow",
                    if borrow.mutable {
                        "mutable"
                    } else {
                        "immutable"
                    },
                )));
            }
            reporter.emit(
                Diagnostic::error()
                    .with_code(BORROW_ERROR)
                    .with_message(format!(
                        "cannot {} borrow `{}`",
                        if borrow.mutable { "mutably" } else { "" },
                        borrow.loc(),
                    ))
                    .with_labels(labels)
                    .with_notes(vec![format!(
                        "Debug information: {:?} {:?} {:?}",
                        borrow.label, borrow.borrower, borrow.ptr
                    )]),
            );
        }
    }

    // Transform all invalidated borrows into clones
    for Borrow { label, ptr, .. } in invalidated {
        cfg[&label].force_clone(&ptr);
    }

    Ok(cfg)
}

/*#[cfg(test)]
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
*/
