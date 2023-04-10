
use std::sync::atomic::AtomicU64;

static COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Copy)]
pub struct Stratum(u64);
impl Stratum {
    pub fn new() -> Self {
        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}