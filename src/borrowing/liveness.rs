//! Implementing the liveness analysis algorithm.

use std::collections::HashMap;

use super::Analysis;
use crate::mir::{Cfg, CfgLabel, Instr, RValue, Var};
use crate::utils::boxed;
use crate::utils::set::Set;

impl Instr {
    /// Returns the "list" of variables that are used (needed) in that
    /// instruction.
    ///
    /// Used for the liveness analysis algorithm, for instance.
    pub fn uses(&self) -> Box<dyn Iterator<Item = Var> + '_> {
        let raw: Box<dyn Iterator<Item = Var>> = match self {
            Instr::Goto(_)
            | Instr::Declare(..)
            | Instr::Assign(_, RValue::Integer(_), _)
            | Instr::Assign(_, RValue::String(_), _)
            | Instr::Assign(_, RValue::Unit, _) => boxed(std::iter::empty()),
            Instr::Assign(_, RValue::Field(v, _), _) | Instr::Assign(_, RValue::Var(v), _) => {
                boxed(std::iter::once(v.clone()))
            }
            Instr::Call { args, .. } => boxed(args.iter().cloned()),
            Instr::Struct { fields, .. } => boxed(fields.values().cloned()),
            Instr::Field { strukt, .. } => boxed(std::iter::once(strukt.clone())),
            Instr::Branch(cond, _, _) => boxed(std::iter::once(cond.clone())),
            Instr::Scope {
                cfg,
                entry_l,
                exit_l,
                ..
            } => boxed(
                liveness(cfg, exit_l)
                    .remove(entry_l) // Get the liveness analysis of the entry_l
                    .unwrap() // It should be there
                    .ins // We want ins since `ins=uses` here
                    .into_iter(),
            ),
        };
        boxed(raw.filter(|v| !v.is_trash()))
    }

    /// Returns the "list" of variables that are defined (overwritten) in that
    /// instruction.
    ///
    /// Used for the liveness analysis algorithm, for instance.
    pub fn defs(&self) -> Box<dyn Iterator<Item = Var>> {
        let raw: Box<dyn Iterator<Item = Var>> = match self {
            Instr::Goto(_) | Instr::Scope { .. } | Instr::Branch(_, _, _) => {
                boxed(std::iter::empty())
            }
            Instr::Declare(v, _) => boxed(std::iter::once(v.name.clone())),
            Instr::Assign(v, _, _) => boxed(std::iter::once(v.clone())),
            Instr::Call { destination, .. }
            | Instr::Struct { destination, .. }
            | Instr::Field { destination, .. } => boxed(std::iter::once(destination.name.clone())),
        };
        boxed(raw.filter(|v| !v.is_trash()))
    }
}

/// Liveliness analysis.
///
/// Takes a cfg, the exit label in that CFG, and returns a map of annotations on
/// each label in the graph.
pub fn liveness(cfg: &Cfg, exit_l: &CfgLabel) -> HashMap<CfgLabel, Analysis> {
    // Bootstrap with empty environments.
    let mut analyses: HashMap<CfgLabel, Analysis> = cfg
        .keys()
        .map(|label| (label.clone(), Analysis::default()))
        .collect();

    // One for the return label too.
    analyses.insert(exit_l.clone(), Analysis::default());

    // Compute the fixpoint, iteratively.
    let mut runs = 0;
    loop {
        let old_analyses = core::mem::take(&mut analyses);
        analyses.insert(exit_l.clone(), Analysis::default());
        for (label, instr) in cfg {
            let Analysis {
                ins: _,
                outs,
                borrows,
            } = &old_analyses[label];
            let defs: Set<Var> = instr.defs().collect();
            let uses: Set<Var> = instr.uses().collect();

            analyses.insert(
                label.clone(),
                Analysis {
                    ins: uses + (outs.clone() - defs),
                    outs: instr
                        .successors()
                        .map(|l| old_analyses[&l].ins.clone())
                        .sum(),
                    borrows: borrows.clone(),
                },
            );
        }
        runs += 1;
        if old_analyses == analyses {
            break;
        }
    }
    println!("In {runs} runs: {:#?}", analyses);

    analyses
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::var::vardef;
    use crate::ast::Ty;
    use crate::borrowing::liveness;

    #[test]
    fn test_use() {
        // Labels
        let label = CfgLabel::new(0);

        // Variables
        let x = Var::from("x");
        let x_def = vardef("x", Ty::IntT);
        let y = Var::from("y");

        assert_eq!(Instr::Goto(label.clone()).uses().count(), 0);

        assert_eq!(
            Set::from_iter(Instr::Assign(x, RValue::Var(y.clone()), label.clone()).uses()),
            Set::from_iter([y])
        );

        assert_eq!(Instr::Declare(x_def, label).uses().count(), 0);
    }

    #[test]
    fn test_liveliness() {
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
