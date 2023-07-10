//! Module for all helper functions that are not related in particular to any
//! other module.

use std::collections::HashMap;
use std::hash::Hash;

pub mod arena;
pub mod multiset;
pub mod set;

pub use multiset::*;

/// Checks if the keys match a list (more precisely, an iterator) of keys.
///
/// [inspired from here](https://stackoverflow.com/questions/58615910/checking-two-hashmaps-for-identical-keyset-in-rust)
pub fn keys_match<'a, T: Eq + Hash + 'a, U>(
    map: &HashMap<T, U>,
    iter: impl Iterator<Item = &'a T>,
) -> bool {
    let mut len: usize = 0;
    for k in iter {
        if !map.contains_key(k) {
            return false;
        }
        len += 1;
    }
    map.len() == len
}

/// Alias for `Box::new()` to make it shorter and easier
/// to use in manually-created ASTs.
pub fn boxed<T>(t: T) -> Box<T> {
    Box::new(t)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keys_match_pass() {
        assert!(keys_match(
            &[("a", 12), ("b", 13)].into_iter().collect(),
            ["b", "a"].iter()
        ));

        assert!(keys_match(
            &[("a", 12), ("b", 13), ("c", 13)].into_iter().collect(),
            ["b", "c", "a"].iter()
        ));
    }

    #[test]
    fn keys_match_fail() {
        assert!(!keys_match(
            &[("a", 12), ("b", 13), ("c", 13)].into_iter().collect(),
            ["b", "a"].iter()
        ));

        assert!(!keys_match(
            &[("a", 12), ("b", 13)].into_iter().collect(),
            ["b", "c", "a"].iter()
        ));

        // same length, but not same keys
        assert!(!keys_match(
            &[("a", 12), ("b", 13), ("d", 13)].into_iter().collect(),
            ["b", "c", "a"].iter()
        ));
    }
}
