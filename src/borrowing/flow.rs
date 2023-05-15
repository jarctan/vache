//! Providing the general abstractions for the liveness analysis through the
//! definition of flows.
use std::fmt::{self, Debug};
use std::iter::Sum;
use std::ops::{BitOr, Sub};

/// Elements that can be used a containers for flows.
///
/// Overall, somethings that behaves like a set: we can bit or, sum, subtract
/// between (same) implementers of that trait.
pub trait Flowable = Sized
    + Debug
    + Clone
    + Default
    + PartialEq
    + Eq
    + Sum
    + BitOr<Output = Self>
    + for<'a> Sub<&'a Self, Output = Self>;

/// A flow point: a collection of inputs, and a collection of outputs.
#[derive(Clone, PartialEq, Eq, Default)]
pub struct Flow<S: Flowable> {
    /// Set of variables.
    pub ins: S,
    /// Outs.
    pub outs: S,
}

impl<S: Flowable> fmt::Debug for Flow<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} --> {:?}", self.ins, self.outs)
    }
}
