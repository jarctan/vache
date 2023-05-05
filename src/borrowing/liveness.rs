//! Implementing the liveness analysis algorithm.

use std::collections::HashMap;

use super::{Flow, Flowable};
use crate::mir::{Cfg, CfgLabel, Instr, RValue, Var};
use crate::utils::set::Set;

/// Alias for the result of the variable liveness analysis.
pub type VarLiveliness = HashMap<CfgLabel, Flow<Set<Var>>>;

impl Instr {
    /// Returns the "list" of variables that are used (needed) in that
    /// instruction.
    ///
    /// Used for the liveness analysis algorithm, for instance.
    fn var_uses(&self) -> Set<Var> {
        let mut raw: Set<Var> = match self {
            Instr::Goto(_)
            | Instr::Declare(..)
            | Instr::Assign(_, RValue::Integer(_), _)
            | Instr::Assign(_, RValue::String(_), _)
            | Instr::Assign(_, RValue::Unit, _) => Set::default(),
            Instr::Assign(_, RValue::Field(v, _), _) | Instr::Assign(_, RValue::Var(v), _) => {
                Set::from([v.clone()])
            }
            Instr::Call { args, .. } => args.iter().cloned().collect(),
            Instr::Struct { fields, .. } => fields.values().cloned().collect(),
            Instr::Field { strukt, .. } => Set::from([strukt.clone()]),
            Instr::Branch(cond, _, _) => Set::from([cond.clone()]),
            Instr::Scope {
                cfg,
                entry_l,
                exit_l,
                ..
            } => {
                var_liveness(cfg, exit_l)
                    .remove(entry_l) // Get the liveness analysis of the entry_l
                    .unwrap() // It should be there
                    .ins
            }
        };
        raw.retain(|v| !v.is_trash());
        raw
    }

    /// Returns the "list" of variables that are defined (overwritten) in that
    /// instruction.
    ///
    /// Used for the liveness analysis algorithm, for instance.
    fn var_defs(&self) -> Set<Var> {
        let mut raw: Set<Var> = match self {
            Instr::Goto(_) | Instr::Scope { .. } | Instr::Branch(_, _, _) => Set::default(),
            Instr::Declare(v, _) => Set::from([v.name.clone()]),
            Instr::Assign(v, _, _) => Set::from([v.clone()]),
            Instr::Call { destination, .. }
            | Instr::Struct { destination, .. }
            | Instr::Field { destination, .. } => Set::from([destination.name.clone()]),
        };
        raw.retain(|v| !v.is_trash());
        raw
    }
}

/// Variable liveness analysis.
///
/// This is the standard liveness analysis.
///
/// Takes as arguments:
/// * The CFG.
/// * The exit label in that CFG.
///
/// Returns live variables on each label in the graph.
pub fn var_liveness(cfg: &Cfg, exit_l: &CfgLabel) -> VarLiveliness {
    liveness(cfg, exit_l, |_, i, _| i.var_defs(), |_, i, _| i.var_uses())
}

/// Liveness analysis.
///
/// Takes as arguments:
/// * A generic `S: Flowable` argument. Take `S` as something like a set.
/// * The CFG.
/// * The exit label in that CFG.
/// * A way to compute "sets" of definitions for a given instruction (you may
///   capture your environment to provide additional information since it's a
///   closure you're giving us).
/// * A way to compute "sets" of uses for a given instruction.
/// Returns a map of annotations on each label in the graph.
pub fn liveness<S: Flowable>(
    cfg: &Cfg,
    exit_l: &CfgLabel,
    defs: impl Fn(&CfgLabel, &Instr, &S) -> S,
    uses: impl Fn(&CfgLabel, &Instr, &S) -> S,
) -> HashMap<CfgLabel, Flow<S>> {
    // Bootstrap with empty environments.
    let mut analyses: HashMap<CfgLabel, Flow<_>> = cfg
        .keys()
        .map(|label| (label.clone(), Flow::default()))
        .collect();

    // One for the return label too.
    analyses.insert(exit_l.clone(), Flow::default());

    // Compute the fixpoint, iteratively.
    loop {
        let old_analyses: HashMap<CfgLabel, Flow<S>> = core::mem::take(&mut analyses);
        analyses.insert(exit_l.clone(), Flow::default());
        for (label, instr) in cfg {
            let Flow { ins, outs }: &Flow<S> = &old_analyses[label];

            analyses.insert(
                label.clone(),
                Flow {
                    ins: uses(label, instr, ins) | (outs.clone() - &defs(label, instr, ins)),
                    outs: instr
                        .successors()
                        .map(|l| old_analyses[&l].ins.clone())
                        .sum(),
                },
            );
        }
        if old_analyses == analyses {
            break;
        }
    }

    analyses
}

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

        let analysis = var_liveness(&cfg, &l_exit);

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
