//! Providing the general abstractions for the liveness analysis through the
//! definition of flows.
use std::fmt::Debug;
use std::ops::Add;

use super::ledger::Ledger;
use crate::mir::Var;
use crate::utils::set::Set;

/// Liveliness analysis result at a point in time.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Analysis {
    /// Set of live variables.
    pub vars: Set<Var>,
    /// Ledger of active loans.
    pub loans: Ledger,
}

impl Add for Analysis {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Self {
            vars: self.vars + other.vars,
            loans: self.loans + other.loans,
        }
    }
}

/// An analysis flow.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Flow {
    /// Ins.
    pub ins: Analysis,
    /// Outs.
    pub outs: Analysis,
}
