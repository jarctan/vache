use std::fmt;
use std::sync::atomic::AtomicU64;

/// Global counter for fresh stratum ids.
static COUNTER: AtomicU64 = AtomicU64::new(0);

/// A stratum: equivalent of a lifetime in our language.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Stratum(u64);
impl Stratum {
    /// Creates a new, fresh stratum with a unique id.
    pub fn new() -> Self {
        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

impl fmt::Display for Stratum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}", self.0)
    }
}
