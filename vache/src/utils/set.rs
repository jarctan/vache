//! All the stuff for user-friendlier sets in Rust.

use std::collections::HashSet;
use std::fmt;
use std::hash::Hash;
use std::iter::{Product, Sum};
use std::ops::{Add, AddAssign, Deref, DerefMut};
use std::ops::{BitOr, BitOrAssign, Sub};

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

impl<T: Eq + Hash> DerefMut for Set<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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

impl<T: Eq + Hash> Add<T> for Set<T> {
    type Output = Set<T>;

    fn add(mut self, rhs: T) -> Self {
        self.0.insert(rhs);
        self
    }
}

impl<T: Eq + Hash> AddAssign<T> for Set<T> {
    fn add_assign(&mut self, rhs: T) {
        self.0.insert(rhs);
    }
}

impl<T: Eq + Hash> BitOr for Set<T> {
    type Output = Set<T>;

    fn bitor(mut self, rhs: Self) -> Self {
        self.0.extend(rhs.0);
        self
    }
}

impl<T: Eq + Hash> BitOrAssign<T> for Set<T> {
    fn bitor_assign(&mut self, rhs: T) {
        self.0.insert(rhs);
    }
}

impl<'a, T: Eq + Hash> Sub<&'a Set<T>> for Set<T> {
    type Output = Set<T>;

    fn sub(mut self, rhs: &Self) -> Self {
        for i in &rhs.0 {
            self.0.remove(i);
        }
        self
    }
}

impl<'a, T: Eq + Hash> Sub<Option<&'a T>> for Set<T> {
    type Output = Set<T>;

    fn sub(mut self, rhs: Option<&T>) -> Self {
        if let Some(rhs) = rhs {
            self.0.remove(rhs);
        }
        self
    }
}

impl<T: Eq + Hash> Add<Option<T>> for Set<T> {
    type Output = Set<T>;

    fn add(mut self, rhs: Option<T>) -> Self {
        if let Some(rhs) = rhs {
            self.0.insert(rhs);
        }
        self
    }
}

impl<'a, T: Eq + Hash> Sub<&'a T> for Set<T> {
    type Output = Set<T>;

    fn sub(mut self, rhs: &T) -> Self {
        self.0.remove(rhs);
        self
    }
}

impl<T: Eq + Hash> Sum for Set<T> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|acc, el| acc + el).unwrap_or_default()
    }
}

impl<'a, T: Eq + Hash + Clone> Product<&'a Set<T>> for Set<T> {
    fn product<I: Iterator<Item = &'a Set<T>>>(mut iter: I) -> Self {
        let first: Set<T> = iter.next().expect("Cannot do empty product").clone();
        iter.fold(first, |acc, other| {
            acc.into_iter().filter(|x| other.contains(x)).collect()
        })
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

impl<T: Eq + Hash, const N: usize> From<[T; N]> for Set<T> {
    fn from(arr: [T; N]) -> Self {
        Self::from_iter(arr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn product_sets() {
        let set1: Set<_> = ["a", "e", "b", "c"].into_iter().collect();
        let set2: Set<_> = ["a", "e", "c"].into_iter().collect();
        let set3: Set<_> = ["c", "d", "f", "b", "e"].into_iter().collect();

        assert_eq!(
            [&set1, &set2, &set3].into_iter().product::<Set<_>>(),
            ["e", "c"].into_iter().collect()
        );
    }
}
