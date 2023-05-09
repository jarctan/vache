//! Implementing the liveness analysis algorithm.

use std::collections::HashMap;

use petgraph::dot::Dot;
use petgraph::Graph;

use super::Flow;
use super::{borrow::Borrow, flow::Analysis};
use crate::mir::{Cfg, CfgLabel, Instr, RValue};
use crate::utils::set::Set;

/// Liveness analysis.
///
/// Takes as arguments:
/// * A generic `S: Flowable` argument. Take `S` as something like a set.
/// * The CFG.
/// * The exit label in that CFG.
///
/// Returns a map of annotations on each label in the graph.
pub fn liveness(cfg: &Cfg, exit_l: &CfgLabel) -> HashMap<CfgLabel, Flow> {
    // Bootstrap with empty environments.
    let mut analyses: HashMap<CfgLabel, Flow> = cfg
        .keys()
        .map(|label| (label.clone(), Flow::default()))
        .collect();

    // One for the return label too.
    analyses.insert(exit_l.clone(), Flow::default());

    // Compute the fixpoint, iteratively.
    loop {
        let old_analyses: HashMap<CfgLabel, Flow> = core::mem::take(&mut analyses);
        analyses.insert(exit_l.clone(), Flow::default());

        for (label, instr) in cfg {
            let Flow { ins, outs }: &Flow = &old_analyses[label];

            let new_flow = match instr {
                Instr::Goto(target) => Flow {
                    ins: Analysis {
                        vars: old_analyses[target].ins.vars.clone(),
                        loans: ins.loans.clone(),
                    },
                    outs: Analysis {
                        vars: old_analyses[target].ins.vars.clone(),
                        loans: ins.loans.clone(),
                    },
                },
                Instr::Declare(var, target) => Flow {
                    ins: Analysis {
                        vars: outs.vars.clone() - &var.name,
                        loans: ins.loans.clone(),
                    },
                    outs: Analysis {
                        vars: old_analyses[target].ins.vars.clone(),
                        loans: ins.loans.clone() - &var.name,
                    },
                },
                Instr::Assign(lhs, RValue::Unit, target)
                | Instr::Assign(lhs, RValue::String(_), target)
                | Instr::Assign(lhs, RValue::Integer(_), target) => Flow {
                    ins: Analysis {
                        vars: outs.vars.clone() - lhs,
                        loans: ins.loans.clone(),
                    },
                    outs: Analysis {
                        vars: old_analyses[target].ins.vars.clone(),
                        loans: ins.loans.clone() - lhs,
                    },
                },
                Instr::Assign(destination, RValue::Var(rhs), target)
                | Instr::Assign(destination, RValue::Field(rhs, _), target) => Flow {
                    ins: Analysis {
                        vars: outs.vars.clone() - destination + rhs.clone(),
                        loans: ins.loans.clone(),
                    },
                    outs: Analysis {
                        vars: old_analyses[target].ins.vars.clone(),
                        loans: ins.loans.clone()
                            + (
                                destination.clone(),
                                [Borrow {
                                    label: label.clone(),
                                    var: rhs.clone(),
                                }],
                            ),
                    },
                },
                Instr::Call {
                    name: _,
                    args,
                    destination,
                    target,
                } => Flow {
                    ins: Analysis {
                        vars: outs.vars.clone() - &destination.name
                            + Set::from_iter(args.iter().cloned()),
                        loans: ins.loans.clone()
                            + (
                                destination.name.clone(),
                                args.iter()
                                    .map(|arg| ins.loans.get(arg).cloned().unwrap_or_default())
                                    .sum::<Set<Borrow>>(),
                            ),
                    },
                    outs: Analysis {
                        vars: old_analyses[target].ins.vars.clone(),
                        loans: ins.loans.clone()
                            + (
                                destination.name.clone(),
                                args.iter()
                                    .map(|arg| ins.loans.get(arg).cloned().unwrap_or_default())
                                    .sum::<Set<Borrow>>(),
                            ),
                    },
                },
                Instr::Struct {
                    name: _,
                    fields,
                    destination,
                    target,
                } => Flow {
                    ins: Analysis {
                        vars: outs.vars.clone() - &destination.name
                            + Set::from_iter(fields.values().cloned()),
                        loans: ins.loans.clone(),
                    },
                    outs: Analysis {
                        vars: old_analyses[target].ins.vars.clone(),
                        loans: ins.loans.clone()
                            + (
                                destination.name.clone(),
                                fields
                                    .values()
                                    .map(|field| ins.loans[field].clone())
                                    .sum::<Set<Borrow>>(),
                            ),
                    },
                },
                Instr::Branch(v, iftrue, iffalse) => Flow {
                    ins: Analysis {
                        vars: outs.vars.clone() + v.clone(),
                        loans: outs.loans.clone(),
                    },
                    outs: Analysis {
                        vars: old_analyses[iftrue].ins.vars.clone()
                            + old_analyses[iffalse].ins.vars.clone(),
                        loans: outs.loans.clone(),
                    },
                },
                Instr::Scope {
                    cfg,
                    entry_l,
                    exit_l,
                    target,
                } => {
                    let mut inner = liveness(cfg, exit_l);
                    let Flow {
                        ins:
                            Analysis {
                                vars: inner_vars,
                                loans: _,
                            },
                        outs:
                            Analysis {
                                vars: _,
                                loans: inner_loans,
                            },
                    } = inner.remove(entry_l).unwrap();
                    Flow {
                        ins: Analysis {
                            vars: outs.vars.clone() + inner_vars,
                            loans: ins.loans.clone(),
                        },
                        outs: Analysis {
                            vars: old_analyses[target].ins.vars.clone(),
                            loans: ins.loans.clone() + inner_loans,
                        },
                    }
                }
            };
            analyses.insert(label.clone(), new_flow);
        }
        if old_analyses == analyses {
            break;
        }
    }

    for (label, instr) in cfg {
        if let Instr::Assign(lhs, _, _) = instr {
            let analysis = &analyses[label];
            if analysis.ins.loans.is_borrowed(lhs) {
                println!("Mutating {lhs} while borrowed!!!!!");
            }
        }
    }

    let mut graph = Graph::new();

    let mut t: HashMap<&CfgLabel, _> = cfg
        .iter()
        .map(|(l, instr)| (l, graph.add_node(instr)))
        .collect();

    let exit = Instr::Goto(exit_l.clone());
    t.insert(exit_l, graph.add_node(&exit));

    for (label, instr) in cfg {
        match instr {
            Instr::Goto(target)
            | Instr::Declare(_, target)
            | Instr::Assign(_, _, target)
            | Instr::Call { target, .. }
            | Instr::Scope { target, .. }
            | Instr::Struct { target, .. } => {
                graph.add_edge(t[label], t[target], &analyses[label].outs.vars)
            }
            Instr::Branch(_, iftrue, iffalse) => {
                graph.add_edge(t[label], t[iftrue], &analyses[label].outs.vars);
                graph.add_edge(t[label], t[iffalse], &analyses[label].outs.vars)
            }
        };
    }

    println!("Final analyses: {:?}", Dot::new(&graph));

    analyses
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
*/
