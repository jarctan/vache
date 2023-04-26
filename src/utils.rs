//! Module for all helper functions that are not related in particular to any other module.

use std::collections::HashMap;
use std::hash::Hash;

/// Checks if the keys match.
///
/// [source here](https://stackoverflow.com/questions/58615910/checking-two-hashmaps-for-identical-keyset-in-rust)
pub fn keys_match<T: Eq + Hash, U, V>(map1: &HashMap<T, U>, map2: &HashMap<T, V>) -> bool {
    map1.len() == map2.len() && map1.keys().all(|k| map2.contains_key(k))
}

/// Alias for `Box::new()` to make it shorter and easier
/// to use in manually-created ASTs.
pub fn boxed<T>(t: T) -> Box<T> {
    Box::new(t)
}
