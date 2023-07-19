//! Tree representation of memory, based on nested locations.

use std::collections::hash_map;
use std::collections::HashMap;
use std::default::default;
use std::fmt;
use std::iter::Extend;
use std::iter::Sum;
use std::ops::Add;
use std::ops::BitOr;
use std::ops::Sub;

use Loc::*;

use super::{Borrow, BorrowCnt, Borrows};
use crate::mir::Loc;
use crate::utils::boxed;

/// A localized tree node.
#[derive(Clone, PartialEq, Eq)]
pub struct LocTreeNode<'ctx, T> {
    /// The kind of node.
    kind: LocTreeNodeKind<'ctx, T>,
    /// Location of the node in the tree.
    loc: Loc<'ctx>,
}

impl<T: fmt::Debug> fmt::Debug for LocTreeNode<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

/// The kind of tree node: leaf of interior node.
#[derive(Clone, PartialEq, Eq)]
pub enum LocTreeNodeKind<'ctx, T> {
    /// Atomic location: cannot be further divided.
    AtomL(T),
    /// Compound location: is made up of several other locations.
    CompoundL(HashMap<&'ctx str, LocTreeNode<'ctx, T>>),
}

use LocTreeNodeKind::*;

impl<T: fmt::Debug> fmt::Debug for LocTreeNodeKind<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Circumvent not to display quotes `"` for the keys.
        match self {
            AtomL(t) => write!(f, "{:?}", t),
            CompoundL(c) => {
                let mut s = f.debug_struct("");

                for (name, v) in c.iter() {
                    s.field(name, v);
                }
                s.finish()
            }
        }
    }
}

impl<T> Default for LocTreeNodeKind<'_, T> {
    fn default() -> Self {
        Self::CompoundL(default())
    }
}

impl<T> From<T> for LocTreeNodeKind<'_, T> {
    fn from(value: T) -> Self {
        Self::AtomL(value)
    }
}

/// A tree structured by `Loc`.
#[derive(Clone, PartialEq, Eq, Default)]
pub struct LocTree<'ctx, T>(HashMap<&'ctx str, LocTreeNode<'ctx, T>>);

impl<T: fmt::Debug> fmt::Debug for LocTree<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Circumvent not to display quotes `"` for the keys.
        if self.0.is_empty() {
            write!(f, "{{}}")
        } else {
            let mut s = f.debug_struct("");

            for (name, v) in self.0.iter() {
                s.field(name, v);
            }
            s.finish()
        }
    }
}

impl<'ctx, T> LocTree<'ctx, T> {
    /// Gets the node at a given location.
    pub fn get_node<'a>(&'a self, loc: impl Into<Loc<'ctx>>) -> Option<&'a LocTreeNode<'ctx, T>> {
        match loc.into() {
            VarL(ref var) => self.0.get(var.as_str()),
            FieldL(strukt, field) => match self.get_node(*strukt)?.kind {
                AtomL(_) => None,
                CompoundL(ref fields) => fields.get(field),
            },
        }
    }

    /// Gets the node _value_ at a given location.
    #[allow(dead_code)]
    pub fn get(&self, loc: Loc<'ctx>) -> Option<&T> {
        match self.get_node(loc)?.kind {
            AtomL(ref value) => Some(value),
            _ => None,
        }
    }

    /// Mutably gets the node at a given location.
    pub fn get_node_mut<'a>(
        &'a mut self,
        loc: impl Into<Loc<'ctx>>,
    ) -> Option<&'a mut LocTreeNode<'ctx, T>> {
        match loc.into() {
            VarL(ref var) => self.0.get_mut(var.as_str()),
            FieldL(strukt, field) => match self.get_node_mut(*strukt)?.kind {
                AtomL(_) => None,
                CompoundL(ref mut fields) => fields.get_mut(field),
            },
        }
    }

    /// Mutably gets the node _value_ at a given location.
    pub fn get_mut(&mut self, loc: Loc<'ctx>) -> Option<&mut T> {
        match self.get_node_mut(loc)?.kind {
            AtomL(ref mut value) => Some(value),
            _ => None,
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
            FieldL(strukt, field) => match self.get_node_mut(*strukt)?.kind {
                AtomL(_) => None,
                CompoundL(ref mut fields) => match fields.entry(field) {
                    hash_map::Entry::Occupied(entry) => Some(entry),
                    hash_map::Entry::Vacant(_) => None,
                },
            },
        }
    }

    /// Removes the node at a given location.
    pub fn remove(&mut self, loc: impl Into<Loc<'ctx>>) -> Option<LocTreeNode<'ctx, T>> {
        match loc.into() {
            VarL(var) => self.0.remove(var.as_str()),
            FieldL(strukt, field) => {
                let mut entry = self.hashmap_entry(*strukt)?;
                let node = entry.get_mut();
                let res = node.remove_field(field)?;

                // Clean up the entry if it's empty
                match node.kind {
                    CompoundL(ref mut fields) if fields.is_empty() => {
                        entry.remove_entry();
                    }

                    AtomL(..) | CompoundL(..) => (),
                }
                Some(res)
            }
        }
    }

    /// Gets all the locations that are represented in this tree.
    pub fn get_all_locs<'a>(&'a self) -> Box<dyn Iterator<Item = Loc<'ctx>> + 'a> {
        Box::new(self.0.values().flat_map(|node| node.get_all_locs()))
    }

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
            VarL(var) => self.0.entry(var.as_str()).or_insert(LocTreeNode {
                loc,
                kind: default(),
            }),
            FieldL(strukt, field) => {
                let node = self.get_node_mut_or_insert(*strukt);
                match node.kind {
                    AtomL(_) => node,
                    CompoundL(ref mut fields) => fields.entry(field).or_insert(LocTreeNode {
                        kind: default(),
                        loc,
                    }),
                }
            }
        }
    }

    /// Swaps two memory locations.
    pub(crate) fn swap(&mut self, loc1: Loc<'ctx>, loc2: Loc<'ctx>) {
        // If either one is not defined, that means that it does not have any borrow.
        // In which case, we create an empty entry when retrieving that location from
        // the tree. Hence the `get_node_mut_or_insert` instead of
        // `get_node_mut`.
        let pa: *mut _ = self.get_node_mut_or_insert(loc1);
        let pb: *mut _ = self.get_node_mut_or_insert(loc2);
        unsafe {
            std::ptr::swap(pa, pb);
        }
    }
}

impl<'ctx, T: Default> LocTree<'ctx, T> {
    /// Mutably gets the node _value_ at a given location. If the path does not
    /// exist, creates one.
    pub fn get_mut_or_insert(&mut self, loc: Loc<'ctx>) -> &mut T {
        let node = self.get_node_mut_or_insert(loc);
        match node.kind {
            AtomL(ref mut t) => t,
            CompoundL(ref s) if s.is_empty() => {
                node.kind = AtomL(T::default());
                if let AtomL(ref mut t) = node.kind {
                    t
                } else {
                    unreachable!()
                }
            }
            CompoundL(_) => panic!("Path for {loc:?} cannot be created."),
        }
    }
}

impl<'ctx, T> LocTreeNode<'ctx, T> {
    /// Removes the data for a single field/child of that node from the
    /// `LocTree`.
    ///
    /// # Panics
    /// Panics if `self` is a leaf node of if the field/child does not exist.
    fn remove_field(&mut self, field: &'ctx str) -> Option<LocTreeNode<'ctx, T>> {
        match self.kind {
            AtomL(..) => panic!("Cannot remove field from AtomL"),
            CompoundL(ref mut map) => map.remove(field),
        }
    }

    /// Gets all the locations that are represented in this tree.
    pub fn get_all_locs<'a>(&'a self) -> Box<dyn Iterator<Item = Loc<'ctx>> + 'a> {
        match self.kind {
            AtomL(_) => boxed(std::iter::once(self.loc)),
            CompoundL(ref map) if map.is_empty() => boxed(std::iter::once(self.loc)),
            CompoundL(ref map) => boxed(
                std::iter::once(self.loc).chain(map.values().flat_map(|node| node.get_all_locs())),
            ),
        }
    }
}

impl<'ctx> LocTree<'ctx, Borrows<'ctx>> {
    /// Gets the `Borrow`s for a given path.
    ///
    /// If the path is an alias, it will return the borrows of the aliased
    /// location.
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
    /// Gets *all* the `Borrow`s of `self`.
    ///
    /// If the location of `self` is an alias, it will return the borrows of the
    /// aliased location.
    fn get_all<'a>(&'a self) -> Box<dyn Iterator<Item = Borrow<'ctx>> + 'a> {
        match &self.kind {
            AtomL(set) => boxed(set.iter().copied()),
            CompoundL(map) => map
                .values()
                .map(|v| v.get_all())
                .fold(boxed(std::iter::empty()), |acc, set| boxed(acc.chain(set))),
        }
    }

    /// Concatenates `self` and `other`.
    fn append(&mut self, other: Self) {
        match (&mut self.kind, other.kind) {
            (AtomL(s), AtomL(o)) => s.extend(o.iter().copied()),
            (CompoundL(s), CompoundL(o)) => {
                for (k, v) in o {
                    s.entry(k)
                        .or_insert(LocTreeNode {
                            kind: default(),
                            loc: v.loc,
                        })
                        .append(v);
                }
            }
            (AtomL(..), CompoundL(..)) | (CompoundL(..), AtomL(..)) => panic!("Not append-able"),
        }
    }
}

impl<'ctx> From<LocTreeNode<'ctx, Borrows<'ctx>>> for Borrows<'ctx> {
    fn from(value: LocTreeNode<'ctx, Borrows<'ctx>>) -> Self {
        match value.kind {
            AtomL(borrows) => borrows,
            CompoundL(map) => map.into_values().map(Borrows::from).sum(),
        }
    }
}

impl<'ctx> From<LocTreeNode<'ctx, Borrows<'ctx>>> for BorrowCnt<'ctx> {
    fn from(value: LocTreeNode<'ctx, Borrows<'ctx>>) -> Self {
        match value.kind {
            AtomL(set) => BorrowCnt::from(set),
            CompoundL(map) => map.into_values().map(|v| v.into()).sum(),
        }
    }
}

impl<'ctx> LocTreeNode<'ctx, ()> {
    /// Concatenates `self` and `other`.
    fn append(&mut self, other: Self) {
        match (&mut self.kind, other.kind) {
            (AtomL(()), AtomL(())) => (),
            (CompoundL(s), CompoundL(o)) => {
                for (k, v) in o {
                    s.entry(k)
                        .or_insert(LocTreeNode {
                            kind: default(),
                            loc: v.loc,
                        })
                        .append(v);
                }
            }
            (AtomL(..), CompoundL(..)) | (CompoundL(..), AtomL(..)) => self.kind = AtomL(()),
        }
    }
}

impl<'ctx> LocTree<'ctx, ()> {
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

    /// Does this tree contain a node at this location?
    pub fn contains(&self, loc: impl Into<Loc<'ctx>>) -> bool {
        self.get_node(loc).is_some()
    }
}

impl<'ctx> Sum for LocTree<'ctx, ()> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut res: Self = default();
        for tree in iter {
            res.append(tree);
        }
        res
    }
}

impl<'ctx> BitOr for LocTree<'ctx, ()> {
    type Output = Self;

    fn bitor(mut self, rhs: Self) -> Self {
        self.append(rhs);
        self
    }
}

impl<'ctx, 'a, T> Sub<&'a Loc<'ctx>> for LocTree<'ctx, T> {
    type Output = Self;

    fn sub(mut self, loc: &'a Loc<'ctx>) -> Self {
        self.remove(*loc);
        self
    }
}

impl<'ctx, T> Sub<Loc<'ctx>> for LocTree<'ctx, T> {
    type Output = Self;

    fn sub(mut self, loc: Loc<'ctx>) -> Self {
        self.remove(loc);
        self
    }
}

impl<'ctx, 'a, T> Sub<&'a Self> for LocTree<'ctx, T> {
    type Output = Self;

    fn sub(mut self, rhs: &'a Self) -> Self {
        for loc in rhs.get_all_locs() {
            self.remove(loc);
        }
        self
    }
}

impl<'ctx, L: Into<Loc<'ctx>>, I: IntoIterator<Item = L>> Add<I> for LocTree<'ctx, ()> {
    type Output = Self;

    fn add(mut self, rhs: I) -> Self {
        for loc in rhs {
            self = self + loc.into();
        }
        self
    }
}

impl<'ctx, L: Into<Loc<'ctx>>, I: IntoIterator<Item = L>> Sub<I> for LocTree<'ctx, ()> {
    type Output = Self;

    fn sub(mut self, rhs: I) -> Self {
        for loc in rhs {
            self.remove(loc.into());
        }
        self
    }
}

impl<'ctx> Add<Loc<'ctx>> for LocTree<'ctx, ()> {
    type Output = Self;

    fn add(mut self, loc: Loc<'ctx>) -> Self {
        self.get_node_mut_or_insert(loc);
        self
    }
}
