//! Defining stratums: ids of lifetime frames in the IR.

use core::fmt;

/// Stratum/scope identifier.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct Stratum(u64);

impl ::std::iter::Step for Stratum {
    fn steps_between(start: &Self, end: &Self) -> Option<usize> {
        start.0.checked_sub(end.0)?.try_into().ok()
    }

    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        Some(Self(start.0.checked_add(count.try_into().ok()?)?))
    }

    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        Some(Self(start.0.checked_sub(count.try_into().ok()?)?))
    }
}

impl Stratum {
    /// The static stratum: this is always stratum `'0`.
    pub const fn static_stm() -> Self {
        Self(0)
    }

    /// Returns the next, higher stratum.
    pub const fn higher(self) -> Self {
        Self(self.0 + 1)
    }

    /// Returns the preceding, lower stratum.
    pub const fn lower(self) -> Self {
        Self(self.0 - 1)
    }
}

impl fmt::Debug for Stratum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}", self.0)
    }
}

impl fmt::Display for Stratum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl From<u64> for Stratum {
    fn from(value: u64) -> Self {
        Stratum(value)
    }
}

impl From<Stratum> for u64 {
    fn from(stm: Stratum) -> Self {
        stm.0
    }
}

impl From<Stratum> for usize {
    fn from(stm: Stratum) -> Self {
        stm.0.try_into().unwrap()
    }
}

impl TryFrom<usize> for Stratum {
    type Error = std::num::TryFromIntError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(Stratum(value.try_into()?))
    }
}
