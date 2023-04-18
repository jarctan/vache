use std::fmt;
use std::sync::atomic::AtomicU64;

/// Global counter for fresh, abstract stratum ids.
static ABSTRACT_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Global counter for fresh, concrete stratum ids.
static CONCRETE_COUNTER: AtomicU64 = AtomicU64::new(0);

/// A stratum variable, i.e. an abstract stratum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct StratumVar(u64);

impl StratumVar {
    /// Creates a new, fresh, unique stratum variable.
    pub fn new() -> Self {
        // TODO: overflow breaks uniqueness!
        Self(ABSTRACT_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

impl Default for StratumVar {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for StratumVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}", self.0)
    }
}

/// A stratum: equivalent of a lifetime in our language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Stratum {
    /// Abstract stratum/stratum variable.
    Abstract(StratumVar),
    /// Concrete stratum that effectively corresponds to a stratum in memory.
    ///
    /// Concrete stratums must be tied to a block of code.
    Concrete(u64),
}
impl Stratum {
    /// Creates a new, fresh abstract stratum with a unique id.
    pub fn new_abstract() -> Self {
        Self::Abstract(StratumVar::new()) // TODO: integer overflow breaks uniqueness!
    }

    /// Creates a new, fresh abstract stratum with a unique id.
    pub fn new_concrete() -> Self {
        // TODO: uniqueness broken in case of overflow!
        Self::Concrete(CONCRETE_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

impl From<StratumVar> for Stratum {
    fn from(var: StratumVar) -> Self {
        Self::Abstract(var)
    }
}

impl fmt::Display for Stratum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Stratum::*;
        match self {
            Abstract(id) => id.fmt(f),
            Concrete(id) => write!(f, "'{}", id),
        }
    }
}
