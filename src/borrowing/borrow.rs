//! Defining the notion of borrows.

use std::hash::{Hash, Hasher};
use std::sync::atomic::AtomicU64;

use crate::mir::Var;
use crate::utils::set::Set;

/// Global unique id provider for borrows.
static BORROW_CNT: AtomicU64 = AtomicU64::new(0);

/// A borrow: a variable that has been borrowed.
#[derive(Clone, Debug)]
pub struct Borrow {
    /// Unique id for that borrow.
    pub id: u64,
    /// Borrowed variable. NOT the borrower.
    pub var: Var,
}

impl PartialEq for Borrow {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Borrow {}

impl Hash for Borrow {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl From<Var> for Borrow {
    fn from(var: Var) -> Self {
        Borrow {
            id: BORROW_CNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            var,
        }
    }
}

/// Set of borrowed variables.
pub type Borrows = Set<Borrow>;
