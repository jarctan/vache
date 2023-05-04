//! All the stuff for user-friendlier sets in Rust.

use std::collections::HashSet;
use std::fmt;
use std::hash::Hash;
use std::iter::Sum;
use std::ops::{Add, Sub};
use std::ops::{AddAssign, Deref};

/// A set.
///
/// Wrapper around `HashSet` that is much more convenient to use.
#[derive(PartialEq, Eq)]
pub struct Set<T: Eq + Hash>(HashSet<T>);

impl<T: Eq + Hash> Deref for Set<T> {
    type Target = HashSet<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

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
