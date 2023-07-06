//! Defining scoped hash tables.

use std::borrow::Borrow;
use std::collections::HashMap;
use std::default::default;
use std::fmt;
use std::hash::Hash;

/// Scoped hash table.
///
/// Hash table with different layers that can be pushed and pop'ed.
///
/// Invariant: always at least one scope in the [`Vec`].
#[derive(Clone)]
pub struct Scoped<K: Eq + Hash, V>(Vec<HashMap<K, V>>);

impl<K: Eq + Hash, V> Scoped<K, V> {
    /// Pushes a new scope onto the pile.
    pub fn push_scope(&mut self) {
        self.0.push(default());
    }

    /// Pops the latest scope.
    ///
    /// Returns [`Err()`] if there would be no scope left after this pop.
    pub fn pop_scope(&mut self) -> anyhow::Result<()> {
        if self.0.len() >= 2 {
            // We can unwrap only if there is at least one scope left afterward
            self.0.pop();
            Ok(())
        } else {
            bail!("Cannot unwrap the last scope left")
        }
    }

    /// Inserts a value in the latest scope. Returns the previous value from
    /// **that scope**, if any.
    pub fn insert(&mut self, key: K, v: V) -> Option<V> {
        self.0
            .last_mut()
            .expect("should always be at least one scope")
            .insert(key, v)
    }

    /// Inserts a value in the oldest, static scope. Returns the previous value
    /// from **that scope**, if any.
    #[allow(dead_code)]
    pub fn insert_in_static(&mut self, key: K, v: V) -> Option<V> {
        self.0
            .first_mut()
            .expect("should always be at least one scope")
            .insert(key, v)
    }

    /// Gets a value from a key.
    #[allow(dead_code)]
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.iter().rev().find_map(|scope| scope.get(key))
    }

    /// Gets a value from a key, and the scope/key in which it was found.
    pub fn get_and_scope<Q>(&self, key: &Q) -> Option<(usize, &V)>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.0
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, scope)| scope.get(key).map(|el| (i, el)))
    }

    /// Gets the number of strata in the [`Strated`].
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Iterates over the scopes directly.
    #[allow(dead_code)]
    pub fn iter_scopes(&self) -> impl Iterator<Item = impl Iterator<Item = (&'_ K, &'_ V)>> {
        self.0.iter().map(|scope| scope.iter())
    }

    /// Removes an element from the scopes.
    ///
    /// It will iterate scopes from the latest to the oldest to find the
    /// element, and will remove the first occurrence of that element.
    #[allow(dead_code)]
    pub fn remove<Q>(&mut self, key: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.0.iter_mut().rev().find_map(|scope| scope.remove(key))
    }

    /// Removes an element from ALL the scopes.
    ///
    /// Returns true iff at least one element was removed.
    #[allow(dead_code)]
    pub fn remove_all<Q>(&mut self, key: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let mut removed = false;
        for scope in self.0.iter_mut().rev() {
            removed |= scope.remove(key).is_some();
        }
        removed
    }

    /// Extends `self` with some vector of scopes of `(keys, values)`.
    ///
    /// Scopes must be from oldest to newest.
    #[allow(dead_code)]
    pub fn extend<I: IntoIterator<Item = (K, V)>>(&mut self, iter: Vec<I>) {
        self.0.resize_with(iter.len(), Default::default);
        // Extend in the latest scope.
        for (i, scope) in iter.into_iter().enumerate() {
            for (k, v) in scope {
                self.0[i].insert(k, v);
            }
        }
    }
}

// A [`Scoped`] from an iterator is just one scope with the elements of the
// iterator.
impl<K: Eq + Hash, V> FromIterator<(K, V)> for Scoped<K, V> {
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        Self(vec![iter.into_iter().collect()])
    }
}

impl<K: Eq + Hash, V> Default for Scoped<K, V> {
    fn default() -> Self {
        Self(vec![default()]) // By default, an empty scope
    }
}

impl<K: Eq + Hash + fmt::Debug, V: fmt::Debug> fmt::Debug for Scoped<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Result;

    use super::*;

    #[test]
    pub fn insert_value() -> Result<()> {
        let mut scoped = Scoped::default();

        scoped.insert(1, 2);
        ensure!(matches!(scoped.get(&1), Some(2)));
        Ok(())
    }

    /// Ensures that a value that is inserted is still accessible in an inner
    /// scope.
    #[test]
    pub fn value_still_in_scope() -> Result<()> {
        let mut scoped = Scoped::default();

        scoped.insert(1, 2);

        // Many pushes and pops but we're still in an inner scope
        scoped.push_scope();
        scoped.push_scope();
        scoped.pop_scope()?;
        scoped.push_scope();

        ensure!(matches!(scoped.get(&1), Some(2)));
        Ok(())
    }
}
