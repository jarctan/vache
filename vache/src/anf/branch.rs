//! Defining [`Branch`]es, which are discriminants that orient the control flow.

use std::fmt;

/// Branch/discriminant.
///
/// A branch is label that indicates the conditions to follow that path/jump to
/// the target node of that edge.
///
/// Example: an If statement has an outgoing edge to two other nodes. One of
/// them is labeled wit [`Branch::BoolB(true)`], the other
/// [`Branch::BoolB(false)`].
///
/// The unconditional jump is [`Branch::DefaultB`].
#[derive(Debug, PartialEq, Eq, Default, Hash, Clone)]
pub enum Branch<'ctx> {
    /// Branch based on a boolean.
    BoolB(bool),
    /// Branch based on the unit value.
    UnitB,
    /// Branch based on an integer value.
    IntB(u64),
    /// Branch based on a string value.
    StrB(&'ctx str),
    /// Default, always branch.
    #[default]
    DefaultB,
}

impl<'ctx> fmt::Display for Branch<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Branch::BoolB(true) => write!(f, "true"),
            Branch::BoolB(false) => write!(f, "false"),
            Branch::UnitB => write!(f, "()"),
            Branch::IntB(i) => write!(f, "{}", i),
            Branch::StrB(s) => write!(f, "{}", s),
            Branch::DefaultB => write!(f, "_"),
        }
    }
}
