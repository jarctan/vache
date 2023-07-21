//! Multisets: sets with a count of the number of occurrences of each element.

use std::borrow::Borrow;
use std::collections::hash_map::IntoIter as HashMapIntoIter;
use std::collections::hash_map::Iter as HashMapIter;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::iter::Sum;
use std::ops::Add;

use super::boxed;
use super::set::Set;

/// Hash-based multiset.
///
/// Interface is inspired by https://docs.rs/multiset/latest/multiset/struct.HashMultiSet.html.
#[derive(Clone)]
pub struct MultiSet<K>(HashMap<K, usize>);

impl<K: Hash + Eq> PartialEq for MultiSet<K> {
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(&self.0, &other.0)
    }
}

impl<K> Default for MultiSet<K> {
    fn default() -> Self {
        Self(HashMap::new())
    }
}

impl<K: Hash + Eq> Eq for MultiSet<K> {}

#[allow(dead_code)]
impl<K: Hash + Eq> MultiSet<K> {
    /// Creates a new empty HashMultiSet.
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    /// Inserts an element (incrementing the number of occurrences of it in the
    /// multiset).
    pub fn insert(&mut self, value: K) {
        *self.0.entry(value).or_default() += 1;
    }

    /// Inserts an element n times.
    pub fn insert_times(&mut self, value: K, n: usize) {
        *self.0.entry(value).or_default() += n;
    }

    /// Returns true iff the multiset contains a value.
    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.0.contains_key(value)
    }

    /// Returns one of the elements with the biggest occurrence.
    pub fn most_represented<'a>(&'a self) -> Box<dyn Iterator<Item = &'a K> + 'a> {
        let max = self.0.values().max().copied();
        if let Some(max) = max {
            boxed(
                self.0
                    .iter()
                    .filter(move |(_, &v)| v == max)
                    .map(|(k, _)| k),
            )
        } else {
            boxed(std::iter::empty())
        }
    }

    /// An iterator visiting all elements in arbitrary order, including each
    /// duplicate. The iterator element type is `&'a K`.
    pub fn iter(&self) -> MultiSetIter<'_, K> {
        MultiSetIter {
            iter: self.0.iter(),
            borrow: None,
        }
    }

    /// The corresponding set for that multiset, without duplicates.
    pub fn as_set(&self) -> Set<&K> {
        self.0.keys().collect()
    }
}

// Extend `MultiSet` with `K`s
impl<K: Eq + Hash> Extend<K> for MultiSet<K> {
    fn extend<T: IntoIterator<Item = K>>(&mut self, iter: T) {
        for elem in iter {
            self.insert(elem);
        }
    }
}

impl<T: Eq + Hash> Add<MultiSet<T>> for MultiSet<T> {
    type Output = MultiSet<T>;

    fn add(mut self, rhs: MultiSet<T>) -> Self {
        for (val, n) in rhs.0 {
            self.insert_times(val, n);
        }
        self
    }
}

impl<K: Eq + Hash> Sum for MultiSet<K> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|acc, el| acc + el).unwrap_or_default()
    }
}

impl<K: Eq + Hash> From<Set<K>> for MultiSet<K> {
    fn from(set: Set<K>) -> Self {
        let mut res = MultiSet::new();
        res.extend(set.into_iter());
        res
    }
}

/// Consuming iterator over the `MultiSet` elements.
///
/// It can only be used when `K: Copy`, since it will possibly return `K`
/// multiple times.
pub struct MultiSetIntoIter<K> {
    /// The internal `HashMap` iterator.
    iter: HashMapIntoIter<K, usize>,
    /// The currently focused element in the set, with a count at how many times
    /// we must still return that same element.
    borrow: Option<(K, usize)>,
}

impl<K: Copy> Iterator for MultiSetIntoIter<K> {
    type Item = K;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((borrow, n)) = &mut self.borrow {
            if *n == 0 {
                self.borrow = None;
                self.next()
            } else {
                *n -= 1;
                Some(*borrow)
            }
        } else if let Some(next) = self.iter.next() {
            self.borrow = Some(next);
            self.next()
        } else {
            None
        }
    }
}

impl<K: Copy> IntoIterator for MultiSet<K> {
    type IntoIter = MultiSetIntoIter<K>;
    type Item = K;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            iter: self.0.into_iter(),
            borrow: None,
        }
    }
}

/// Iterator over the `MultiSet` elements.
pub struct MultiSetIter<'a, K> {
    /// The internal `HashMap` iterator.
    iter: HashMapIter<'a, K, usize>,
    /// The currently focused element in the set, with a count at how many times
    /// we must still return that same element.
    borrow: Option<(&'a K, usize)>,
}

impl<'a, K> Iterator for MultiSetIter<'a, K> {
    type Item = &'a K;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((borrow, n)) = &mut self.borrow {
            if *n == 0 {
                self.borrow = None;
                self.next()
            } else {
                *n -= 1;
                Some(*borrow)
            }
        } else if let Some((borrow, &n)) = self.iter.next() {
            self.borrow = Some((borrow, n));
            self.next()
        } else {
            None
        }
    }
}

impl<K: fmt::Debug> fmt::Debug for MultiSet<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::*;

    #[test]
    fn borrow_cnt_is_ok() {
        let mut borrow_cnt = MultiSet::default();
        borrow_cnt.insert(1);
        borrow_cnt.insert(2);
        borrow_cnt.insert(1);
        println!("{:?}", borrow_cnt);
        let res = borrow_cnt.into_iter().collect_vec();
        assert_eq!(res.len(), 3, "expected 3 elements, found {res:?}");
    }

    #[test]
    fn contains_is_ok() {
        let mut borrow_cnt = MultiSet::default();
        borrow_cnt.insert(1);
        borrow_cnt.insert(2);
        borrow_cnt.insert(1);
        assert!(borrow_cnt.contains(&1));
        assert!(borrow_cnt.contains(&2));
    }
}
