//! Defining stratums: ids of lifetime frames in the IR.

use core::fmt;

/// Stratum/scope identifier.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Stratum(u64);

impl Stratum {
    /// The static stratum: this is always stratum `'0`.
    pub const fn static_stm() -> Stratum {
        Self(0)
    }

    /// Increases stratum by 1.
    ///
    /// # Unsafety
    ///
    /// Can be used to generate arbitrary stratum ids. Use with caution.
    pub(crate) unsafe fn incr(self) -> Stratum {
        Stratum(self.0.checked_add(1).unwrap())
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
