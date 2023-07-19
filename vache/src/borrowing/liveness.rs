//! Implementing the liveness analysis algorithm.

use std::collections::HashMap;

use anyhow::Result;
use itertools::Itertools;

use super::{Borrows, Flow, FunFlow, InvalidationReason, Invalidations, Ledger, Loan, LocTree};
use crate::codes::BORROW_ERROR;
use crate::mir::{Cfg, CfgI, CfgLabel, Fun, InstrKind, Loc, Mode};
use crate::reporter::Diagnostic;
use crate::utils::Set;
use crate::Context;

/// Variable flow.
type VarFlow<'ctx> = Cfg<'ctx, Flow<LocTree<'ctx, ()>>>;
/// Loan flow.
type LoanFlow<'ctx> = Cfg<'ctx, Flow<Ledger<'ctx>>>;

/// Variable liveness analysis.
///
/// Takes as arguments:
/// * The CFG.
/// * The entry label in the CFG.
///
/// Returns a map of live variables at the entry and exit of each node in the
/// CFG.
pub fn var_liveness<'ctx>(cfg: &CfgI<'_, 'ctx>, entry_l: CfgLabel) -> VarFlow<'ctx> {
    // Bootstrap with empty environments.
    let mut var_flow: VarFlow = cfg.map_ref(|_, _| Flow::default(), |_| ());

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
    f: &Fun<'_, 'ctx>,
    var_flow: &VarFlow<'ctx>,
    fun_flow: &HashMap<&'ctx str, FunFlow>,
) -> LoanFlow<'ctx> {
    let out_of_scope: Cfg<LocTree<()>> =
        var_flow.map_ref(|_, flow| flow.ins.clone() - &flow.outs, |_| ());

    let mut loan_flow: LoanFlow = f.body.map_ref(|_, _| Flow::default(), |_| ());

    // Compute the fixpoint, iteratively.
    let mut updated = true;
    while updated {
        updated = false;

        for (label, instr) in f.body.postorder(f.entry_l).rev() {
            // println!("Label: {label:?}");
            let predecessors = f.body.preneighbors(label);

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
            let successor_stm = f
                .body
                .neighbors(label)
                .map(|label| f.body[&label].scope)
                .min()
                .unwrap_or(instr.scope);

            // We need to flush every variable between the successor stratum (not included)
            // and our stratum (included)
            for stm in successor_stm.higher()..=instr.scope {
                if let Some(set) = f.strata.get(&stm) {
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
/// * The function in the CFG.
/// * The function flow of the previous iteration of the fixpoint.
/// * The compiler context.
///
/// Performs liveliness analysis, determining which borrows are invalidated.
/// Returns the new function signature.
pub fn liveness<'ctx>(
    f: &mut Fun<'_, 'ctx>,
    fun_flow: &HashMap<&'ctx str, FunFlow>,
    ctx: &mut Context<'ctx>,
) -> Result<FunFlow> {
    // We compute the natural ordering of the labels of the CFG, once and for all.
    let cfg_flow_label_order = f
        .body
        .postorder(f.entry_l)
        .rev()
        .map(|(label, _)| label)
        .collect::<Vec<_>>();

    let loan_flow: Result<LoanFlow> = loop {
        // Compute the var analysis first
        let var_flow = var_liveness(&f.body, f.entry_l);

        // Loop until there is no change in moves.
        // Note: `loan_flow` is mutable because we flush its invalidations after
        let mut loan_flow = loop {
            // Now, compute loan analysis
            let loan_flow = loan_liveness(f, &var_flow, fun_flow);

            let mut updated = false;
            // Check last variable use and replace with a move
            for (label, instr) in f.body.bfs_mut(f.entry_l, false) {
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
        let mut invalidated = Invalidations::new();

        // List all borrows that are invalidated by mutation of the variable afterwards.
        for (label, instr) in f.body.bfs(f.entry_l, false) {
            for lhs in instr.mutated_places() {
                for borrow in loan_flow[&label].ins.loans(lhs.root()) {
                    invalidated.insert(
                        borrow,
                        InvalidationReason::MutationWithLiveBorrow {
                            mutation_span: instr.span,
                        },
                    );
                }
            }
        }

        // List all borrows that are invalidated by mutation of a variable within the
        // same instruction.
        for (label, instr) in f.body.bfs_mut(f.entry_l, false) {
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
                    for &borrow in borrows {
                        invalidated.insert(
                            borrow,
                            InvalidationReason::MutationWithLiveBorrow {
                                mutation_span: instr.span,
                            },
                        );
                    }
                }
            }
        }

        // Extend with the invalidations of the ledger. Take them at exit_l to have them
        // all
        invalidated.extend(loan_flow[&f.ret_l].outs.invalidations());

        // Choose the invalidation that is the first in the flow order amongst the most
        // represented invalidations
        match cfg_flow_label_order
            .iter()
            .find_map(|&label| invalidated.most_represented().find(|b| b.label == label))
            .copied()
        {
            Some(loan @ Loan { label, ptr, .. }) => {
                // We have at least one candidate, let's invalidate it

                // If we asked to report invalidations, do it for the new invalidation
                if ctx.config.report_invalidations {
                    let reasons = invalidated
                        .remove(loan)
                        .into_iter()
                        .unique()
                        .map(|r| r.to_diagnostic(loan));
                    for reason in reasons {
                        ctx.emit(reason);
                    }
                }

                // Get the reference that must be cloned
                let to_clone = f.body[&label].find(&ptr);
                if matches!(to_clone.mode(), Mode::MutBorrowed | Mode::SMutBorrowed) {
                    // If the mode was mutably borrowed, that comes from some user annotation
                    // We won't fix it ourselves, so we report back to the user
                    ctx.emit(
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
                    break Ok(loan_flow);
                } else {
                    // Otherwise, it's fine to clone.
                    to_clone.set_mode(Mode::Cloned);
                }
            }
            None => {
                // If we have no invalidations anymore, we reached a stable state and we can
                // return
                break Ok(loan_flow);
            }
        }
    };
    let loan_flow = loan_flow?;
    // println!("{loan_flow:?}");

    // Now, compute the function flow
    // First, the flow of the returned value
    let ret_flow = if let Some(ret_v) = f.ret_v {
        let borrows = loan_flow[&f.ret_l].ins.borrows(*ret_v.place());
        let borrowed_vars = borrows.map(|b| b.borrowed_loc().root()).collect::<Vec<_>>();
        f.params
            .iter()
            .enumerate()
            .filter(|(_, p)| borrowed_vars.contains(&p.var.name()))
            .map(|(i, _)| i)
            .collect::<Vec<_>>()
    } else {
        vec![]
    };

    // Then, the flow of the arguments passed by reference
    let mut args_flow = HashMap::new();
    for (i, p) in f.params.iter().enumerate().filter(|(_, p)| p.byref) {
        let borrows = loan_flow[&f.ret_l].ins.borrows(p.var);
        let borrowed_vars = borrows.map(|b| b.borrowed_loc().root()).collect::<Vec<_>>();
        args_flow.insert(
            i,
            f.params
                .iter()
                .enumerate()
                .filter(|(_, p)| borrowed_vars.contains(&p.var.name()))
                .map(|(i, _)| i)
                .collect::<Vec<_>>(),
        );
    }

    Ok(FunFlow::new(args_flow, ret_flow))
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
