//! Representing control flow graphs.

use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use std::hash::Hash;
use std::ops::Index;
use std::ops::{Deref, IndexMut};

use super::{Branch, Instr};

/// A node index in the graph.
#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub struct NodeIx(u64);

impl fmt::Debug for NodeIx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "§{}", self.0)
    }
}

/// Type alias for a label (node index) in the control flow graph.
pub type CfgLabel = NodeIx;

/// An edge index in the graph.
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash)]
struct EdgeIx(u64);

impl fmt::Debug for EdgeIx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "§{}", self.0)
    }
}

/// A node in the graph.
#[derive(PartialEq, Eq, Debug)]
struct Node<N> {
    /// Value/weight of that node.
    value: N,
    /// List of ingoing edges, indexed by their weight.
    ins: HashMap<Branch, EdgeIx>,
    /// List of outgoing edges, indexed by their weight.
    outs: HashMap<Branch, EdgeIx>,
}

/// An edge in the graph.
///
/// All edges are directed.
#[derive(PartialEq, Eq, Debug)]
struct Edge<E> {
    /// Node index of the origin of the edge.
    from: NodeIx,
    /// Node index of the target of the edge.
    to: NodeIx,
    /// Weight of the edge.
    weight: E,
}

impl<N> Deref for Node<N> {
    type Target = N;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<N> From<N> for Node<N> {
    fn from(value: N) -> Self {
        Self {
            value,
            ins: HashMap::default(),
            outs: HashMap::default(),
        }
    }
}

/// A control flow graph. It's a basically graph, tailored to our needs.
///
/// Generic parameters:
/// * `N` is the type of nodes weights
/// * `E` is the type of edges weights
#[derive(PartialEq, Eq, Default)]
pub struct Cfg<N = Instr, E = ()> {
    /// Map of node indexes to node data.
    node_map: HashMap<NodeIx, Node<N>>,
    /// Map of edge indexes to the edge data.
    edge_map: HashMap<EdgeIx, Edge<E>>,
    /// Fresh node index counter.
    node_ix_counter: u64,
    /// Fresh edge index counter.
    edge_ix_counter: u64,
}

impl<N, E> Cfg<N, E> {
    /// Starting from a node, takes a given branch/path. Returns the target
    /// node, if any.
    pub fn take_branch(&self, node: &CfgLabel, branch: &Branch) -> Option<&CfgLabel> {
        self.node_map[node]
            .outs
            .get(branch)
            .map(|e| &self.edge_map[e].to)
    }

    /// Adds a new node to the graph, returning its label in the CFG.
    pub fn add_node(&mut self, node: N) -> CfgLabel {
        let label = NodeIx(self.node_ix_counter);
        self.node_ix_counter = self.node_ix_counter.checked_add(1).unwrap();
        self.node_map.insert(label.clone(), node.into());
        label
    }

    /// Adds a new edge between `from` and `to` in the graph with weight
    /// `branch` and extra weight `E`.
    pub fn add_edge(&mut self, from: CfgLabel, to: CfgLabel, branch: Branch, weight: E) {
        let edge = Edge { from, to, weight };
        let ix = EdgeIx(self.edge_ix_counter);
        self.edge_ix_counter = self.edge_ix_counter.checked_add(1).unwrap();

        // Add at `from` and `to`
        assert!(self
            .node_map
            .get_mut(&edge.from)
            .unwrap()
            .outs
            .insert(branch.clone(), ix)
            .is_none());
        assert!(self
            .node_map
            .get_mut(&edge.to)
            .unwrap()
            .ins
            .insert(branch, ix)
            .is_none());

        // Add the edge in itself
        self.edge_map.insert(ix, edge);
    }

    /// Outgoing neighbors.
    pub fn neighbors<'a>(&'a self, node: &CfgLabel) -> impl Iterator<Item = &'a CfgLabel> + 'a {
        self.node_map[node]
            .outs
            .values()
            .map(|e| &self.edge_map[e].to)
    }

    /// Ingoing neighbors.
    pub fn preneighbors<'a>(&'a self, node: &CfgLabel) -> impl Iterator<Item = &'a CfgLabel> + 'a {
        self.node_map[node]
            .ins
            .values()
            .map(|e| &self.edge_map[e].from)
    }

    /// Returns an immutable BFS iterator over the graph.
    pub fn bfs<'a>(&'a self, start: &'a CfgLabel) -> Bfs<'a, N, E> {
        Bfs::new(self, start)
    }

    /// Returns an immutable DFS iterator over the graph.
    pub fn dfs<'a>(&'a self, start: &'a CfgLabel) -> Dfs<'a, N, E> {
        Dfs::new(self, start)
    }

    /// Maps into a new CFG.
    ///
    /// You must provide a mapping for nodes, and one for edges. Use `map_ref`
    /// if you only have a reference into the CFG.
    pub fn map<F, G, N2, E2>(self, mut node_map: F, mut edge_map: G) -> Cfg<N2, E2>
    where
        F: FnMut(NodeIx, N) -> N2,
        G: FnMut(E) -> E2,
    {
        Cfg {
            node_map: self
                .node_map
                .into_iter()
                .map(|(k, v)| {
                    (
                        k.clone(),
                        Node {
                            value: node_map(k, v.value),
                            ins: v.ins,
                            outs: v.outs,
                        },
                    )
                })
                .collect(),
            edge_map: self
                .edge_map
                .into_iter()
                .map(|(k, v)| {
                    (
                        k,
                        Edge {
                            weight: edge_map(v.weight),
                            from: v.from,
                            to: v.to,
                        },
                    )
                })
                .collect(),
            node_ix_counter: self.node_ix_counter,
            edge_ix_counter: self.edge_ix_counter,
        }
    }

    /// Maps into a new CFG.
    ///
    /// You must provide a mapping for nodes, and one for edges. Only needs a
    /// self reference, will clone items that are needed from the original
    /// CFG. Prefer to use `map()` whenever possible.
    pub fn map_ref<'a, F, G, N2, E2>(&'a self, mut node_map: F, mut edge_map: G) -> Cfg<N2, E2>
    where
        F: FnMut(NodeIx, &'a N) -> N2,
        G: FnMut(&'a E) -> E2,
    {
        Cfg {
            node_map: self
                .node_map
                .iter()
                .map(|(k, v)| {
                    (
                        k.clone(),
                        Node {
                            value: node_map(k.clone(), &v.value),
                            ins: v.ins.clone(),
                            outs: v.outs.clone(),
                        },
                    )
                })
                .collect(),
            edge_map: self
                .edge_map
                .iter()
                .map(|(k, v)| {
                    (
                        *k,
                        Edge {
                            weight: edge_map(&v.weight),
                            from: v.from.clone(),
                            to: v.to.clone(),
                        },
                    )
                })
                .collect(),
            node_ix_counter: self.node_ix_counter,
            edge_ix_counter: self.edge_ix_counter,
        }
    }
}

impl<N: fmt::Debug, E: fmt::Debug> fmt::Debug for Cfg<N, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "digraph G {{")?;
        for (ix, Node { value, .. }) in &self.node_map {
            writeln!(f, "\tn{} [label=\"{:?}\"];", ix.0, value)?;
        }
        for Edge { from, to, weight } in self.edge_map.values() {
            writeln!(f, "\tn{} -> n{} [label=\"{:?}\"];", from.0, to.0, weight)?;
        }
        write!(f, "}}")
    }
}

impl<N, E> Index<&CfgLabel> for Cfg<N, E> {
    type Output = N;

    fn index(&self, index: &CfgLabel) -> &N {
        &self.node_map[index].value
    }
}

impl<N, E> IndexMut<&CfgLabel> for Cfg<N, E> {
    fn index_mut(&mut self, index: &CfgLabel) -> &mut N {
        &mut self.node_map.get_mut(index).unwrap().value
    }
}

/// Breadth-First Search iterator over a graph.
pub struct Bfs<'a, N, E> {
    /// Queue of elements to visit.
    queue: VecDeque<&'a NodeIx>,
    /// Already visited elements.
    visited: HashSet<&'a NodeIx>,
    /// Reference to the graph itself.
    graph: &'a Cfg<N, E>,
}

impl<'a, N, E> Bfs<'a, N, E> {
    /// Creates a new iterator that starts from a given label and traverses a
    /// `graph`.
    fn new(graph: &'a Cfg<N, E>, start: &'a CfgLabel) -> Self {
        Self {
            queue: [start].into(),
            visited: HashSet::new(),
            graph,
        }
    }
}

impl<'a, N, E> Iterator for Bfs<'a, N, E> {
    type Item = (&'a NodeIx, &'a N);

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.queue.pop_front()?;
        for node in self.graph.neighbors(node) {
            if self.visited.insert(node) {
                // If `visited` did not contain this value before
                self.queue.push_back(node);
            }
        }
        Some((node, &self.graph[node]))
    }
}

/// Depth-First Search iterator over a graph.
pub struct Dfs<'a, N, E> {
    /// Stack of elements to visit.
    stack: Vec<&'a NodeIx>,
    /// Already visited elements.
    visited: HashSet<&'a NodeIx>,
    /// Reference to the graph itself.
    graph: &'a Cfg<N, E>,
}

impl<'a, N, E> Dfs<'a, N, E> {
    /// Creates a new iterator that starts from a given label and traverses a
    /// `graph`.
    fn new(graph: &'a Cfg<N, E>, start: &'a CfgLabel) -> Self {
        Self {
            stack: [start].into(),
            visited: HashSet::new(),
            graph,
        }
    }
}

impl<'a, N, E> Iterator for Dfs<'a, N, E> {
    type Item = (&'a NodeIx, &'a N);

    fn next(&mut self) -> Option<Self::Item> {
        let node = loop {
            let node = self.stack.pop()?;
            if self.visited.insert(node) {
                // If `visited` did not contain this value before
                for node in self.graph.neighbors(node) {
                    self.stack.push(node);
                }
                break node;
            }
        };
        Some((node, &self.graph[node]))
    }
}