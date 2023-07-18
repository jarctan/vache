//! Implementing the liveness analysis algorithm.

use std::collections::HashMap;

use anyhow::Result;

use super::borrow::{Borrows, Loan};
use super::flow::Flow;
use super::ledger::Ledger;
use super::tree::LocTree;
use super::FunFlow;
use crate::codes::BORROW_ERROR;
use crate::mir::{Cfg, CfgI, CfgLabel, InstrKind, Loc, Mode, Stratum, Varname};
use crate::reporter::{Diagnostic, Reporter};
use crate::utils::MultiSet;
use crate::utils::Set;

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

    // Compute the fixpoint, iteratively.
    let mut updated = true;
    while updated {
        updated = false;

        for (label, instr) in cfg.postorder(entry_l) {
            let successors = cfg.neighbors(label);

            let outs: LocTree<()> = successors.map(|x| var_flow[&x].ins.clone()).sum();
            let mut ins: LocTree<()> = outs.clone();

            // Remove definitions
            for assigned in instr.mutated_places() {
                ins = ins - assigned.def();
            }

            // Add rhs uses (do that after we removed all defined places, since mutated
            // places should predominate and not be shadowed by defined variables)
            for assigned in instr.mutated_places() {
                ins = ins + assigned.uses_as_lhs();
            }

            // Add rhs uses
            for r in instr.references() {
                ins = ins + r.place().uses_as_rhs();
            }

            // Compare with the flow of the previous iteration and flag if we made any
            // change
            let flow = Flow { ins, outs };
            if var_flow[&label] != flow {
                var_flow[&label] = flow;
                updated = true;
            }
        }
    }

    //var_flow.print_image("var").unwrap();

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
    fun_flow: &HashMap<&'ctx str, FunFlow>,
    strata: &HashMap<Stratum, Set<Varname<'ctx>>>,
) -> Cfg<'ctx, Flow<Ledger<'ctx>>> {
    let out_of_scope: Cfg<LocTree<()>> =
        var_flow.map_ref(|_, flow| flow.ins.clone() - &flow.outs, |_| ());

    let mut loan_flow: Cfg<Flow<Ledger>> = cfg.map_ref(|_, _| Flow::default(), |_| ());

    // Compute the fixpoint, iteratively.
    let mut updated = true;
    while updated {
        updated = false;

        for (label, instr) in cfg.postorder(entry_l).rev() {
            let predecessors = cfg.preneighbors(label);

            let ins: Ledger = predecessors.map(|x| loan_flow[&x].outs.clone()).sum();
            let mut outs: Ledger = ins.clone();

            match &instr.kind {
                InstrKind::SwapS(r1, r2) => {
                    let (place1, place2) = (r1.place(), r2.place());
                    match (Loc::try_from(*r1.place()), Loc::try_from(*r2.place())) {
                        (Ok(loc1), Ok(loc2)) => {
                            outs.swap(loc1, loc2);
                        }
                        _ => {
                            if place1.root() == place2.root() {
                                // Simplest case: r1 and r2 have the same root:
                                // nothing to do, exchanging
                                // swapping elements in the same location is
                                // fine, since the borrows for that location
                                // stay the same
                                // We check that first since it is really easy
                                // to compute, compared to the other cases
                            }
                            if ins.borrows(place1).map(Loan::from).collect::<Set<_>>()
                                == ins.borrows(place2).map(Loan::from).collect::<Set<_>>()
                            {
                                // If they have the same borrows (modulo
                                // the name of the borrower, which is why we
                                // convert the `Borrow`s to `Loan`s), that's
                                // fine too, and we have nothing to do
                            } else {
                                panic!("Invalid swap between {place1:?} and {place2:?}");
                            }
                        }
                    }
                }
                _ => {
                    let flow = instr.flow(fun_flow);

                    for (assigned, refs) in flow {
                        let borrows: Borrows = refs
                            .iter()
                            .flat_map(|reference| outs.borrow(reference, label))
                            .map(|loan| loan.into_borrow(*assigned.loc()))
                            .collect();
                        if var_flow[&label].outs.contains(assigned.loc()) {
                            outs.set_borrows(assigned.place(), borrows);
                        } else {
                            outs.flush_place(assigned.place(), true);
                        }
                    }
                }
            }

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

    //loan_flow.print_image("loan").unwrap();

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
    fun_flow: &HashMap<&'ctx str, FunFlow>,
    strata: &HashMap<Stratum, Set<Varname<'ctx>>>,
    reporter: &mut Reporter<'ctx>,
) -> Result<CfgI<'mir, 'ctx>> {
    let cfg_flow_label_order = cfg
        .postorder(entry_l)
        .rev()
        .map(|(label, _)| label)
        .collect::<Vec<_>>();

    loop {
        // Compute the var analysis first
        let var_flow = var_liveness(&cfg, entry_l);

        // Loop until there is no change in moves.
        let loan_flow = loop {
            // Now, compute loan analysis
            let loan_flow = loan_liveness(&cfg, entry_l, &var_flow, fun_flow, strata);

            let mut updated = false;
            // Check last variable use and replace with a move
            for (label, instr) in cfg.bfs_mut(entry_l, false) {
                let var_outs = &var_flow[&label].outs;
                let borrows_in = &loan_flow[&label].ins;

                // Get all the references/pointers of that instruction.
                let ptrs: Vec<_> = instr.references().map(|r| r.as_ptr()).collect();
                let references_mut: Vec<_> = instr.references_mut().collect();

                for reference in references_mut.into_iter() {
                    match reference.mode() {
                        Mode::Borrowed | Mode::MutBorrowed => {
                            let loc = reference.loc();
                            // We cannot move if the location is still active after that
                            // instruction, or if some part of the
                            // location is used by some __other__ pointer in the same instruction.
                            // instruction For example, for `debug(a,
                            // a)`, we will see that we cannot move the first
                            // `a`, since it is used by a reference after that first `a`
                            // To do that, we compute the set of locations that cannot be moved for
                            // each reference in our `references`.
                            if !var_outs.contains(loc)
                                && !ptrs
                                    .iter()
                                    .filter(|ptr| ptr.id != reference.id)
                                    .any(|ptr| loc <= ptr.loc)
                            {
                                // Move only if we have no more borrows into ourselves (except the
                                // one we're about to free)
                                // Otherwise don't, since this move would trigger
                                // invalidations/clones
                                // for all these borrows
                                let loans =
                                    borrows_in.loans(*loc).filter(|b| b.ptr.id != reference.id);
                                if loans.count() == 0 {
                                    updated = true;
                                    reference.set_mode(Mode::Moved);
                                }
                            }
                        }
                        // These ones are used for intermediate variables and for the Rust backend
                        // and are not intended to be changed to `moved`
                        Mode::SMutBorrowed | Mode::SBorrowed => (),
                        // If we are cloned, there's a reason fot that. So you can't move
                        Mode::Cloned => (),
                        // If already moved, nothing to do
                        Mode::Moved => (),
                    }
                }
            }

            if !updated {
                break loan_flow;
            }
        };

        // Now, collect all invalidations.
        let mut invalidated = MultiSet::new();

        // List all borrows that are invalidated by mutation of the variable afterwards.
        for (label, instr) in cfg.bfs(entry_l, false) {
            for lhs in instr.mutated_places() {
                for borrow in loan_flow[&label].ins.loans(lhs.root()) {
                    invalidated.insert(borrow.into());
                }
            }
        }

        // List all borrows that are invalidated by mutation of a variable within the
        // same instruction.
        for (label, instr) in cfg.bfs_mut(entry_l, false) {
            // Get all the mutated places of the instruction
            let mutated_places = instr.mutated_places().collect::<Vec<_>>();
            // The borrows at the entry of the instruction.
            let borrows_in = &loan_flow[&label].ins;
            // Compute the mapping of places, and the list of borrows made on that place
            // that are used within the instruction.
            let mut refs: HashMap<_, Vec<_>> = HashMap::new();
            for r in instr.references() {
                for b in borrows_in.borrows(r.place()) {
                    refs.entry(b.borrowed_place()).or_default().push(b);
                }
            }
            // For each mutated place, invalidate all the borrows given by the mapping.
            for lhs in mutated_places {
                if let Some(borrows) = refs.get(&lhs) {
                    for &b in borrows {
                        invalidated.insert(b.into());
                    }
                }
            }
        }

        // Extend with the invalidations of the ledger. Take them at exit_l to have them
        // all
        invalidated.extend(loan_flow[&exit_l].outs.invalidations());

        match cfg_flow_label_order
            .iter()
            .filter_map(|&label| {
                invalidated
                    .most_represented()
                    .copied()
                    .find(|b| b.label == label)
            })
            .next()
        {
            Some(Loan { label, ptr, .. }) => {
                // Get the reference that must be cloned
                let to_clone = cfg[&label].find(&ptr);
                if matches!(to_clone.mode(), Mode::MutBorrowed | Mode::SMutBorrowed) {
                    // If the mode was mutably borrowed, that comes from some user annotation
                    // We won't fix it ourselves, so we report back to the user
                    reporter.emit(
                        Diagnostic::error()
                            .with_code(BORROW_ERROR)
                            .with_message(
                                "you cannot use this mutably here, the value must be cloned",
                            )
                            .with_labels(vec![to_clone.span.as_label()])
                            .with_notes(vec![format!("help: consider using the binding syntax `{:?}@new_var` to bind the result of the mutation to a new variable `new_var`", to_clone.place())]),
                    );
                    // This error cannot be recovered and makes us abort.
                    // But this is not a compiler internal error per se, so we do not return `Err`.
                    // Otherwise, that would abruptly terminate the compilation.
                    break Ok(cfg);
                } else {
                    // Otherwise, it's fine to clone.
                    to_clone.set_mode(Mode::Cloned);
                }
            }
            None => {
                // If we have no invalidated anymore, we reach a stable state and we can return.
                //cfg.print_image("cfg").unwrap();
                break Ok(cfg);
            }
        }
    }
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
