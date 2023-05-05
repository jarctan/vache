//! Declaring here the annotations to the CFG we compute during the analysis.

use std::collections::HashMap;
use std::iter::Sum;
use std::ops::{BitOr, Deref, DerefMut, Sub};

use super::borrow::{Borrow, Borrows};
use super::flow::Flow;
use super::liveness::liveness;
use crate::mir::{Cfg, CfgLabel, Instr, RValue, Var};
use crate::utils::set::Set;

/// Alias for the result of the loan liveness analysis.
pub type LoanLiveliness = HashMap<CfgLabel, Flow<Ledger>>;

/// A loan ledger.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ledger {
    /// Map between variables defined in this environment and their borrows.
    ///
    /// Invariant: these are _deep_ borrows, i.e. all borrow variables are
    /// variables defined _outside_ the scope.
    borrows: HashMap<Var, Borrows>,
}

impl Ledger {
    /// Creates a new, empty ledger.
    pub fn new() -> Self {
        Self {
            borrows: HashMap::new(),
        }
    }

    /// Gets the borrows of a variable.
    pub fn get_var(&self, v: impl AsRef<Var>) -> Option<&Borrows> {
        self.borrows.get(v.as_ref())
    }

    /// Defines a new variable in the context, stating its borrows.
    pub fn add_var(&mut self, var: impl Into<Var>, borrows: impl Into<Borrows>) {
        // Compute the set of deep borrows: borrows whose variable were defined OUTSIDE
        // this environment
        let mut deep_set = Set::new();
        for borrow in borrows.into().into_iter() {
            if let Some(borrows_lvl2) = self.borrows.get(&borrow.var) {
                for borrow_lvl2 in borrows_lvl2 {
                    deep_set += borrow_lvl2.clone();
                }
            } else {
                deep_set += Borrow::from(borrow.var.clone());
            }
        }
        self.borrows.insert(var.into(), deep_set);
    }

    pub fn close(self, vars: impl Iterator<Item = Var>) -> Set<Var> {
        // Compute the union of the deep borrows of all the variables.
        let mut res: Set<Var> = Set::new();
        for var in vars {
            if let Some(borrows) = self.borrows.get(&var) {
                for borrow in borrows {
                    res += borrow.var.clone();
                }
            } else {
                res += var;
            }
        }
        res
    }
}

impl Default for Ledger {
    fn default() -> Self {
        Self::new()
    }
}

impl BitOr for Ledger {
    type Output = Ledger;

    fn bitor(mut self, rhs: Self) -> Self {
        self.borrows.extend(rhs.borrows);
        self
    }
}

impl<'a> Sub<&'a Ledger> for Ledger {
    type Output = Ledger;

    fn sub(mut self, rhs: &Self) -> Self {
        for key in rhs.borrows.keys() {
            self.borrows.remove(key);
        }
        self
    }
}

impl Sum for Ledger {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|acc, el| acc | el).unwrap_or_default()
    }
}

impl Deref for Ledger {
    type Target = HashMap<Var, Borrows>;

    fn deref(&self) -> &Self::Target {
        &self.borrows
    }
}

impl DerefMut for Ledger {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.borrows
    }
}

impl Instr {
    /// Returns the ledger of loans that are used/needed for the
    /// instruction.
    ///
    /// The final, fixpoint ledger is processed iteratively. To do that, you
    /// must provide a `ins` parameter which is the current computation of
    /// the ledger at the entry of this instruction.
    ///
    /// You must also provide the map of variables that go out of scope during
    /// that time. This can be computed using the variable analysis.
    ///
    /// Used for the loan analysis algorithm.
    fn loan_uses(&self, ins: &Ledger, out_of_scope: &HashMap<CfgLabel, Set<Var>>) -> Ledger {
        match self {
            Instr::Goto(_) | Instr::Declare(_, _) | Instr::Branch(_, _, _) => Ledger::default(),
            Instr::Assign(lhs, RValue::Var(rhs), _) => {
                let mut ledger = Ledger::default();
                ledger.add_var(lhs.clone(), [Borrow::from(rhs.clone())]);
                ledger
            }
            Instr::Assign(_, _, _) => Ledger::default(),
            Instr::Call {
                args, destination, ..
            } => {
                let mut ledger = Ledger::default();
                let borrows: Borrows = args.iter().map(|v| ins[v].clone()).sum();
                ledger.add_var(destination.clone(), borrows);
                ledger
            }
            Instr::Struct {
                fields,
                destination,
                ..
            } => {
                let mut ledger = Ledger::default();
                let borrows: Borrows = fields.values().map(|v| ins[v].clone()).sum();
                ledger.add_var(destination.clone(), borrows);
                ledger
            }
            Instr::Field {
                strukt,
                destination,
                ..
            } => {
                let mut ledger = Ledger::default();
                ledger.add_var(destination.clone(), [Borrow::from(strukt.clone())]);
                ledger
            }
            Instr::Scope {
                cfg,
                entry_l,
                exit_l,
                ..
            } => {
                loan_liveness(cfg, exit_l, out_of_scope)
                    .remove(entry_l) // Get the liveness analysis of the entry_l
                    .unwrap() // It should be there
                    .ins
            }
        }
    }

    /// Returns the ledger for borrows that are removed during that instruction.
    ///
    /// The final, fixpoint ledger is processed iteratively. To do that, you
    /// must provide a `ins` parameter which is the current computation of
    /// the ledger at the entry of this instruction.
    ///
    /// You must also provide the map of variables that go out of scope during
    /// that time. This can be computed using the variable analysis.
    ///
    /// Used for the loan analysis algorithm.
    fn loan_discard(&self, ins: &Ledger, out_of_scope: &Set<Var>) -> Ledger {
        let mut ledger = ins.clone();
        ledger.retain(|var, _| out_of_scope.contains(var));
        ledger
    }
}

/// Loan liveness analysis.
///
/// Takes as arguments:
/// * The CFG.
/// * The exit label in that CFG.
///
/// Returns live loans for each label in the graph.
pub fn loan_liveness(
    cfg: &Cfg,
    exit_l: &CfgLabel,
    out_of_scope: &HashMap<CfgLabel, Set<Var>>,
) -> LoanLiveliness {
    liveness(
        cfg,
        exit_l,
        |l, i, ins| i.loan_discard(ins, &out_of_scope[l]),
        |_, i, ins| i.loan_uses(ins, out_of_scope),
    )
}
