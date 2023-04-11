use std::sync::atomic::AtomicU64;

/// Global counter for fresh stratum ids.
static COUNTER: AtomicU64 = AtomicU64::new(0);

/// A stratum: equivalent of a lifetime in our language.
#[derive(Clone, Copy)]
pub struct Stratum(u64);
impl Stratum {
    /// Creates a new, fresh stratum with a unique id.
    pub fn new() -> Self {
        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}
