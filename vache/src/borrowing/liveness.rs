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
    cfg: &CfgI<'_, 'ctx>,
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
    var_flow.print_image("var");

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
    cfg: &CfgI<'_, 'ctx>,
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
        #[cfg(not(test))]
        println!("TRRRRRRRRRRRRRYYYYYY\n\n\n\n");
        updated = false;

        for (label, instr) in cfg.postorder(entry_l).rev() {
            #[cfg(not(test))]
            println!("{:?}", instr);
            let predecessors = cfg.preneighbors(label);
            let ins: Ledger = predecessors.map(|x| loan_flow[&x].outs.clone()).sum();
            let mut outs: Ledger = match &instr.kind {
                InstrKind::Assign(lhs, _)
                | InstrKind::Call {
                    name: _,
                    args: _,
                    destination: Some(lhs),
                } => {
                    let mut ledger = ins.clone();
                    let borrows = instr
                        .references()
                        .map(|reference| ledger.borrow(*lhs.loc(), reference, label))
                        .collect::<Vec<_>>();
                    let borrows = ledger.flatten(borrows.into_iter());
                    if var_flow[&label].outs.contains(lhs.loc()) {
                        ledger.set_borrows(lhs.place(), borrows);
                    } else {
                        ledger.flush_place(lhs.place(), true);
                    }
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
    loan_flow.print_image("loan");

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
    mut cfg: CfgI<'mir, 'ctx>,
    entry_l: CfgLabel,
    exit_l: CfgLabel,
    strata: &HashMap<Stratum, Set<Varname<'ctx>>>,
    reporter: &mut Reporter<'ctx>,
) -> Result<CfgI<'mir, 'ctx>, Diagnostics<'ctx>> {
    cfg.print_image("cfg");

    let mut invalidated = Borrows::new();

    // Compute the var analysis first
    let var_flow = var_liveness(&cfg, entry_l);

    // Loop until there is no change in moves.
    let loan_flow = loop {
        // Now, compute loan analysis
        let loan_flow = loan_liveness(&cfg, entry_l, &var_flow, strata);

        let mut updated = false;
        // Check last variable use and replace with a move
        for (label, instr) in cfg.bfs_mut(entry_l, false) {
            let var_outs = &var_flow[&label].outs;
            let borrows_outs = &loan_flow[&label].ins;

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
                        if !var_outs.contains(loc) && !cannot_move[i].contains(loc) {
                            let loans: Vec<_> = borrows_outs.loans(*reference.loc()).collect();
                            if loans.is_empty() || loans.iter().all(|loan| !loan.mutable) {
                                updated = true;
                                reference.set_mode(Mode::Moved);
                            } else {
                                #[cfg(not(test))]
                                println!("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
                                /*panic!(
                                    "Cannot move {loc:?} out  because {loans:?} are still active in {instr:?}"
                                );*/
                            }
                        }
                    }
                    Mode::Cloned => (), // We clone, there's a reason fot that. So you can't move
                    Mode::Moved => (),  // already moved
                }
            }
        }

        if !updated {
            break loan_flow;
        }
    };

    // List all borrows that are invalidated by mutation of the variable afterwards.
    for (label, instr) in cfg.bfs(entry_l, false) {
        for lhs in instr.mutated_place() {
            for borrow in loan_flow[&label].ins.loans(lhs.root()) {
                debug_assert!(!borrow.mutable);
                invalidated.insert(borrow);
            }
        }
    }

    // Extend with the invalidations of the ledger. Take them at exit_l to have them
    // all
    invalidated.extend(loan_flow[&exit_l].outs.invalidations());

    // If there are some unrecoverables loan issues in the final ledger, emit the
    // corresponding warnings.
    if let Some(unrecoverables) = loan_flow[&exit_l].outs.unrecoverables() {
        for (&borrow, &contradiction) in unrecoverables {
            reporter.emit(
                Diagnostic::error()
                    .with_code(BORROW_ERROR)
                    .with_message(format!(
                        "cannot use `{}`{} here",
                        borrow.loc(),
                        if borrow.mutable { " mutably" } else { "" },
                    ))
                    .with_labels(vec![
                        borrow.span.as_label().with_message(
                            if borrow.mutable && contradiction.mutable {
                                "second mutable use but L0 is still active"
                            } else {
                                "immutable use but mutable use L0 is still active"
                            },
                        ),
                        contradiction.span.as_secondary_label().with_message(
                            if borrow.mutable && contradiction.mutable {
                                "first mutable use L0"
                            } else {
                                "mutable use L0"
                            },
                        ),
                    ])
                    .with_notes(vec![
                        if borrow.mutable && contradiction.mutable {
                            "remember: there cannot be two simultaneously active mutable uses at any line"
                                .to_string()
                        } else {
                            "remember: there cannot be any immutable use while a mutable use is active"
                                .to_string()
                        },
                        if borrow.mutable && contradiction.mutable {
                            "help: consider removing one of the two mutable uses, or shorten the lifetime of the first one"
                                .to_string()
                        } else {
                            "help: consider not mutably borrowing, or remove the immutable uses while the mutable use is active"
                                .to_string()
                        },
                        format!(
                            "Debug information: {:?} {:?} {:?}",
                            borrow.label, borrow.borrower, borrow.ptr
                        ),
                    ]),
            );
        }
    }

    // Transform all invalidated borrows into clones
    for borrow @ Borrow {
        label,
        ptr,
        mutable,
        ..
    } in invalidated
    {
        #[cfg(not(test))]
        println!("Invalidation: {:?}", borrow);
        debug_assert!(!mutable, "Borrow {borrow:?} is mutable");
        cfg[&label].force_clone(&ptr);
    }

    Ok(cfg)
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
        let x_ptr = Pointer::new(&arena, arena.alloc(x.into()), default());
        let x_ref = LhsRef::declare(x_ptr);

        let y = VarUse::from("y");
        let y_ptr = Pointer::new(&arena, arena.alloc(y.into()), default());
        let y_ref = LhsRef::declare(y_ptr);

        let mut y_mode = default();

        // CFG
        let l = cfg.add_block(
            [
                instr(InstrKind::Assign(y_ref, RValue::Integer(&forty_two)), stm),
                instr(
                    InstrKind::Assign(x_ref, RValue::Place(Reference::new(y_ptr, &mut y_mode))),
                    stm,
                ),
                instr(InstrKind::Noop, stm),
            ],
            (),
        );

        let analysis = var_liveness(&cfg, l[0]);

        // Entry and exit are trivial
        assert!(analysis[&l[0]].ins.get_all_locs().next().is_none());
        assert!(analysis[&l[2]].ins.get_all_locs().next().is_none());
        assert!(analysis[&l[2]].outs.get_all_locs().next().is_none());

        // At the end of L0, we still need y
        assert_eq!(
            analysis[&l[0]].outs.get_all_locs().collect::<Vec<_>>(),
            [y.into()]
        );

        // At the start of L1, we still need y
        assert_eq!(
            analysis[&l[1]].ins.get_all_locs().collect::<Vec<_>>(),
            [y.into()]
        );
    }
}
