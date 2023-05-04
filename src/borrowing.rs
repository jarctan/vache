//! Typing.

use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter::Sum;
use std::ops::AddAssign;
use std::ops::{Add, Sub};
use std::sync::atomic::AtomicU64;

use crate::mir::*;
use crate::utils::boxed;

/// Global unique id provider for borrows.
static BORROW_CNT: AtomicU64 = AtomicU64::new(0);

/// A borrow: a variable that has been borrowed.
#[derive(Clone, Debug)]
struct Borrow {
    /// Unique id for that borrow.
    id: u64,
    /// Borrowed variable. NOT the borrower.
    var: Var,
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

/// A set.
///
/// Wrapper around `HashSet` that is much more convenient to use.
#[derive(PartialEq, Eq)]
pub struct Set<T: Eq + Hash>(HashSet<T>);

impl<T: Eq + Hash> Set<T> {
    /// Creates an empty HashSet.
    /// The set is initialized with 0 entries.
    pub fn new() -> Self {
        Self(HashSet::new())
    }
}

impl<T: Eq + Hash + fmt::Debug> fmt::Debug for Set<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: Eq + Hash> Default for Set<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Eq + Hash + Clone> Clone for Set<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }

    fn clone_from(&mut self, other: &Self) {
        self.0.clone_from(&other.0);
    }
}

impl<T: Eq + Hash> Add for Set<T> {
    type Output = Set<T>;

    fn add(mut self, rhs: Self) -> Self {
        self.0.extend(rhs.0);
        self
    }
}

impl<T: Eq + Hash> AddAssign<T> for Set<T> {
    fn add_assign(&mut self, rhs: T) {
        self.0.insert(rhs);
    }
}

impl<T: Eq + Hash> Sub for Set<T> {
    type Output = Set<T>;

    fn sub(mut self, rhs: Self) -> Self {
        for i in &rhs.0 {
            self.0.remove(i);
        }
        self
    }
}

impl<T: Eq + Hash> Sum for Set<T> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|acc, el| acc + el).unwrap_or_default()
    }
}

impl<T: Eq + Hash> FromIterator<T> for Set<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(HashSet::from_iter(iter))
    }
}

impl<'a, T: Eq + Hash> IntoIterator for &'a Set<T> {
    type IntoIter = std::collections::hash_set::Iter<'a, T>;
    type Item = &'a T;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<T: Eq + Hash> IntoIterator for Set<T> {
    type IntoIter = std::collections::hash_set::IntoIter<T>;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

/// Set of borrowed variables.
type Borrows = Set<Borrow>;

/// A borrow analysis.
#[derive(Debug, PartialEq, Eq)]
struct Analysis {
    /// Map between variables defined in this environment and their borrows.
    ///
    /// Invariant: these are _deep_ borrows, i.e. all borrow variables are
    /// variables defined _outside_ the scope.
    borrows: HashMap<Var, Borrows>,
    /// Set of variables.
    ins: Set<Var>,
    /// Outs.
    outs: Set<Var>,
}

impl Analysis {
    /// Creates a new, empty environment.
    fn new() -> Self {
        Self {
            borrows: HashMap::new(),
            ins: Set::new(),
            outs: Set::new(),
        }
    }

    /// Gets the borrows of a variable.
    fn get_var(&self, v: impl AsRef<Var>) -> Option<&Borrows> {
        self.borrows.get(v.as_ref())
    }

    /// Defines a new variable in the context, stating its borrows.
    fn add_var(&mut self, var: impl Into<Var>, borrows: impl Into<Borrows>) {
        // Compute the set of deep borrows: borrows whose variable were defined OUTSIDE
        // this environment
        let mut deep_set = Set::new();
        for borrow in borrows.into().into_iter() {
            if let Some(borrows_lvl2) = self.borrows.get(&borrow.var) {
                for borrow_lvl2 in borrows_lvl2 {
                    deep_set += borrow_lvl2.clone();
                }
            } else {
                deep_set += Borrow::from(borrow.var.clone());
            }
        }
        self.borrows.insert(var.into(), deep_set);
    }

    fn close(self, vars: impl Iterator<Item = Var>) -> HashSet<Var> {
        // Compute the union of the deep borrows of all the variables.
        let mut res: HashSet<Var> = HashSet::new();
        for var in vars {
            if let Some(borrows) = self.borrows.get(&var) {
                for borrow in borrows {
                    res.insert(borrow.var.clone());
                }
            } else {
                res.insert(var);
            }
        }
        res
    }
}

impl Default for Analysis {
    fn default() -> Self {
        Self::new()
    }
}

impl Instr {
    /// Returns the "list" of variables that are used (needed) in that
    /// instruction.
    ///
    /// Used for the liveness analysis algorithm, for instance.
    pub fn uses(&self) -> Box<dyn Iterator<Item = Var> + '_> {
        let raw: Box<dyn Iterator<Item = Var>> = match self {
            Instr::Goto(_)
            | Instr::Declare(..)
            | Instr::Assign(_, RValue::Integer(_), _)
            | Instr::Assign(_, RValue::String(_), _)
            | Instr::Assign(_, RValue::Unit, _) => boxed(std::iter::empty()),
            Instr::Assign(_, RValue::Field(v, _), _) | Instr::Assign(_, RValue::Var(v), _) => {
                boxed(std::iter::once(v.clone()))
            }
            Instr::Call { args, .. } => boxed(args.iter().cloned()),
            Instr::Struct { fields, .. } => boxed(fields.values().cloned()),
            Instr::Field { strukt, .. } => boxed(std::iter::once(strukt.clone())),
            Instr::Branch(cond, _, _) => boxed(std::iter::once(cond.clone())),
            Instr::Scope {
                cfg,
                entry_l,
                exit_l,
                ..
            } => boxed(
                liveness(cfg, exit_l)
                    .remove(entry_l) // Get the liveness analysis of the entry_l
                    .unwrap() // It should be there
                    .ins // We want ins since `ins=uses` here
                    .into_iter(),
            ),
        };
        boxed(raw.filter(|v| !v.is_trash()))
    }

    /// Returns the "list" of variables that are defined (overwritten) in that
    /// instruction.
    ///
    /// Used for the liveness analysis algorithm, for instance.
    pub fn defs(&self) -> Box<dyn Iterator<Item = Var>> {
        let raw: Box<dyn Iterator<Item = Var>> = match self {
            Instr::Goto(_) | Instr::Scope { .. } | Instr::Branch(_, _, _) => {
                boxed(std::iter::empty())
            }
            Instr::Declare(v, _) => boxed(std::iter::once(v.name.clone())),
            Instr::Assign(v, _, _) => boxed(std::iter::once(v.clone())),
            Instr::Call { destination, .. }
            | Instr::Struct { destination, .. }
            | Instr::Field { destination, .. } => boxed(std::iter::once(destination.name.clone())),
        };
        boxed(raw.filter(|v| !v.is_trash()))
    }
}

/// Liveliness analysis.
///
/// Takes a cfg, the exit label in that CFG, and returns a map of annotations on
/// each label in the graph.
fn liveness(cfg: &Cfg, exit_l: &CfgLabel) -> HashMap<CfgLabel, Analysis> {
    // Bootstrap with empty environments.
    let mut analyses: HashMap<CfgLabel, Analysis> = cfg
        .keys()
        .map(|label| (label.clone(), Analysis::default()))
        .collect();

    // One for the return label too.
    analyses.insert(exit_l.clone(), Analysis::default());

    // Compute the fixpoint, iteratively.
    loop {
        let old_analyses = core::mem::take(&mut analyses);
        analyses.insert(exit_l.clone(), Analysis::default());
        for (label, instr) in cfg {
            let Analysis {
                ins: _,
                outs,
                borrows,
            } = &old_analyses[label];
            let defs: Set<Var> = instr.defs().collect();
            let uses: Set<Var> = instr.uses().collect();

            analyses.insert(
                label.clone(),
                Analysis {
                    ins: defs + (outs.clone() - uses),
                    outs: instr
                        .successors()
                        .map(|l| old_analyses[&l].ins.clone())
                        .sum(),
                    borrows: borrows.clone(),
                },
            );
        }
        if old_analyses == analyses {
            break;
        }
    }
    println!("{:#?}", analyses);

    analyses
}

/// The borrow-checker.
pub(crate) struct BorrowChecker {}

impl BorrowChecker {
    /// Creates a new borrow-checker.
    pub fn new() -> Self {
        Self {}
    }

    /// Borrow-checks a given program.
    pub fn check(&mut self, p: &Program) {
        self.visit_program(p);
    }

    /// Borrow-checks a function.
    fn visit_fun(&mut self, f: &Fun) {
        println!("{:?}", liveness(&f.body, &f.ret_l));
    }

    /// Borrow-checks a program.
    fn visit_program(&mut self, p: &Program) {
        for f in p.funs.values() {
            self.visit_fun(f);
        }
    }
}
