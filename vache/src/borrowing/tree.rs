//! Tree representation of memory, based on nested locations.

use std::collections::hash_map;
use std::collections::HashMap;
use std::default::default;
use std::iter::Extend;

use Loc::*;

use super::borrow::{Borrow, Borrows};
use crate::mir::Loc;
use crate::utils::boxed;

/// A localized tree node.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum LocTreeNode<'ctx, T> {
    /// Atomic location: cannot be further divided.
    AtomL(T),
    /// Compound location: is made up of several other locations.
    CompoundL(HashMap<&'ctx str, LocTreeNode<'ctx, T>>),
}

use LocTreeNode::*;

impl<T: Default> Default for LocTreeNode<'_, T> {
    fn default() -> Self {
        Self::AtomL(default())
    }
}

impl<T> From<T> for LocTreeNode<'_, T> {
    fn from(value: T) -> Self {
        Self::AtomL(value)
    }
}

/// A tree structured by `Loc`.
#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct LocTree<'ctx, T>(HashMap<&'ctx str, LocTreeNode<'ctx, T>>);

impl<'ctx, T> LocTree<'ctx, T> {
    /// Gets the node at a given location.
    pub fn get_node<'a>(&'a self, loc: Loc<'ctx>) -> Option<&'a LocTreeNode<'ctx, T>> {
        match loc {
            VarL(ref var) => self.0.get(var.as_str()),
            FieldL(strukt, field) => match self.get_node(*strukt)? {
                AtomL(_) => None,
                CompoundL(fields) => fields.get(field),
            },
        }
    }

    /// Gets the node _value_ at a given location.
    pub fn get(&self, loc: Loc<'ctx>) -> Option<&T> {
        match self.get_node(loc) {
            Some(AtomL(value)) => Some(value),
            _ => None,
        }
    }

    /// Mutably gets the node at a given location.
    pub fn get_node_mut<'a>(&'a mut self, loc: Loc<'ctx>) -> Option<&'a mut LocTreeNode<'ctx, T>> {
        match loc {
            VarL(ref var) => self.0.get_mut(var.as_str()),
            FieldL(strukt, field) => match self.get_node_mut(*strukt)? {
                AtomL(_) => None,
                CompoundL(fields) => fields.get_mut(field),
            },
        }
    }

    /// Gets the HashMap entry for a given location.
    ///
    /// For internal use only. Primarily by `remove()`.
    fn hashmap_entry<'a>(
        &'a mut self,
        loc: Loc<'ctx>,
    ) -> Option<hash_map::OccupiedEntry<&'ctx str, LocTreeNode<'ctx, T>>> {
        match loc {
            VarL(var) => match self.0.entry(var.as_str()) {
                hash_map::Entry::Occupied(entry) => Some(entry),
                hash_map::Entry::Vacant(_) => None,
            },
            FieldL(strukt, field) => match self.get_node_mut(*strukt)? {
                AtomL(_) => None,
                CompoundL(fields) => match fields.entry(field) {
                    hash_map::Entry::Occupied(entry) => Some(entry),
                    hash_map::Entry::Vacant(_) => None,
                },
            },
        }
    }

    /// Mutably gets the node _value_ at a given location.
    pub fn get_mut(&mut self, loc: Loc<'ctx>) -> Option<&mut T> {
        match self.get_node_mut(loc) {
            Some(AtomL(value)) => Some(value),
            _ => None,
        }
    }

    /// Removes the node at a given location.
    pub fn remove(&mut self, loc: Loc<'ctx>) -> Option<LocTreeNode<'ctx, T>> {
        match loc {
            VarL(var) => self.0.remove(var.as_str()),
            FieldL(strukt, field) => {
                let mut entry = self.hashmap_entry(*strukt)?;
                let node = entry.get_mut();
                let res = node.remove_field(field);

                // Clean up the entry if it's empty
                match node {
                    CompoundL(fields) if fields.is_empty() => {
                        entry.remove_entry();
                    }

                    AtomL(..) | CompoundL(..) => (),
                }
                Some(res)
            }
        }
    }
}

impl<'ctx, T: Default> LocTree<'ctx, T> {
    /// Mutably gets the node at a given location. If the path does not exist,
    /// creates one.
    ///
    /// # Panics
    /// Panics if the location cannot be created.
    pub fn get_node_mut_or_insert<'a>(
        &'a mut self,
        loc: Loc<'ctx>,
    ) -> &'a mut LocTreeNode<'ctx, T> {
        match loc {
            VarL(var) => self.0.entry(var.as_str()).or_default(),
            FieldL(strukt, field) => match self.get_node_mut_or_insert(*strukt) {
                AtomL(_) => panic!("Path cannot be created."),
                CompoundL(fields) => fields.entry(field).or_default(),
            },
        }
    }

    /// Mutably gets the node _value_ at a given location. If the path does not
    /// exist, creates one.
    pub fn get_mut_or_insert(&mut self, loc: Loc<'ctx>) -> &mut T {
        let node = self.get_node_mut_or_insert(loc);
        match node {
            AtomL(t) => t,
            CompoundL(_) => panic!("Path cannot be created."),
        }
    }
}

impl<'ctx, T> LocTreeNode<'ctx, T> {
    /// Removes the data for a single field/child of that node from the
    /// `LocTree`.
    ///
    /// # Panics
    /// Panics if `self` is a leaf node of if the field/child does not exist.
    fn remove_field(&mut self, field: &'ctx str) -> LocTreeNode<'ctx, T> {
        match self {
            AtomL(..) => panic!("Cannot remove field from AtomL"),
            CompoundL(map) => map.remove(field).unwrap(),
        }
    }
}

impl<'ctx> LocTree<'ctx, Borrows<'ctx>> {
    /// Gets the `Borrows` for a given path.
    pub fn get_all<'a>(&'a self, loc: Loc<'ctx>) -> Box<dyn Iterator<Item = Borrow<'ctx>> + 'a> {
        if let Some(node) = self.get_node(loc) {
            node.get_all()
        } else {
            boxed(std::iter::empty())
        }
    }

    /// Concatenates `self` and `other`.
    pub fn append(&mut self, other: Self) {
        for (var, other_tree) in other.0 {
            match self.0.entry(var) {
                hash_map::Entry::Occupied(mut entry) => {
                    entry.get_mut().append(other_tree);
                }
                hash_map::Entry::Vacant(entry) => {
                    entry.insert(other_tree);
                }
            };
        }
    }
}

impl<'ctx> LocTreeNode<'ctx, Borrows<'ctx>> {
    /// Gets *all* the `Borrows`.
    fn get_all<'a>(&'a self) -> Box<dyn Iterator<Item = Borrow<'ctx>> + 'a> {
        match self {
            AtomL(set) => boxed(set.iter().copied()),
            CompoundL(map) => map
                .values()
                .map(|v| v.get_all())
                .fold(boxed(std::iter::empty()), |acc, set| boxed(acc.chain(set))),
        }
    }

    /// Concatenates `self` and `other`.
    fn append(&mut self, other: Self) {
        match (self, other) {
            (AtomL(s), AtomL(o)) => s.extend(o.into_iter()),
            (CompoundL(s), CompoundL(o)) => {
                for (k, v) in o {
                    s.entry(k).or_default().append(v);
                }
            }
            (AtomL(..), CompoundL(..)) | (CompoundL(..), AtomL(..)) => panic!("Not append-able"),
        }
    }
}

impl<'ctx> From<LocTreeNode<'ctx, Borrows<'ctx>>> for Borrows<'ctx> {
    fn from(value: LocTreeNode<'ctx, Borrows<'ctx>>) -> Self {
        match value {
            AtomL(set) => set,
            CompoundL(map) => map.into_values().map(|v| v.into()).sum(),
        }
    }
}
