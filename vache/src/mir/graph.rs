//! Representing control flow graphs.

use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use std::hash::Hash;
use std::ops::Index;
use std::ops::{Deref, IndexMut};

use priority_queue::PriorityQueue;

use super::{Branch, Instr, Stratum};
use crate::utils::boxed;

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
    ins: HashMap<EdgeIx, Branch>,
    /// List of outgoing edges, indexed by their weight.
    outs: HashMap<Branch, EdgeIx>,
}

/// An edge in the graph.
///
/// All edges are directed.
#[derive(PartialEq, Eq, Debug, Hash)]
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
#[derive(PartialEq, Eq)]
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
        assert!(
            self.node_map
                .get_mut(&edge.from)
                .unwrap()
                .outs
                .insert(branch.clone(), ix)
                .is_none(),
            "there is already an outgoing edge between {:?} and {:?} for branch {branch:?}",
            edge.from,
            edge.to
        );
        assert!(
            self.node_map
                .get_mut(&edge.to)
                .unwrap()
                .ins
                .insert(ix, branch)
                .is_none(),
            "graph error: already an ingoing edge between {:?} and {:?} with same id",
            edge.from,
            edge.to
        );

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
            .keys()
            .map(|e| &self.edge_map[e].from)
    }

    /// Returns an immutable BFS iterator over the graph.
    ///
    /// `rev_dir`: take arrows in opposite direction.
    pub fn bfs<'a>(&'a self, start: &'a CfgLabel, rev_dir: bool) -> Bfs<'a, N, E> {
        Bfs::new(self, start, rev_dir)
    }

    /// Returns a mutable BFS iterator over the graph.
    ///
    /// `rev_dir`: take arrows in opposite direction.
    pub fn bfs_mut<'a>(&'a mut self, start: &'a CfgLabel, rev_dir: bool) -> BfsMut<'a, N, E> {
        BfsMut::new(self, start.clone(), rev_dir)
    }

    /// Returns an immutable DFS iterator over the graph.
    ///
    /// `rev_dir`: take arrows in opposite direction.
    pub fn dfs<'a>(&'a self, start: &'a CfgLabel, rev_dir: bool) -> Dfs<'a, N, E> {
        Dfs::new(self, start, rev_dir)
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

impl Cfg {
    /// Turns it into a Control Flow Search iterator over the graph.
    pub fn into_cfs(self, start: CfgLabel) -> CfsSelf {
        CfsSelf::new(self, start)
    }
}

impl<N, E: Copy> Cfg<N, E> {
    /// Adds a block/linked-list of nodes.
    ///
    /// Weight must be `Copy`-able. Returns the labels for each element in the
    /// bloc, in order.
    pub fn add_block(&mut self, nodes: impl IntoIterator<Item = N>, weight: E) -> Vec<CfgLabel> {
        let labels: Vec<CfgLabel> = nodes.into_iter().map(|node| self.add_node(node)).collect();
        for [from, to] in labels.array_windows::<2>() {
            self.add_edge(from.clone(), to.clone(), Branch::DefaultB, weight);
        }
        labels
    }
}

impl<N, E> Default for Cfg<N, E> {
    fn default() -> Self {
        Self {
            node_map: HashMap::new(),
            edge_map: HashMap::new(),
            node_ix_counter: 0,
            edge_ix_counter: 0,
        }
    }
}

impl<N: fmt::Debug, E: fmt::Debug> fmt::Debug for Cfg<N, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "digraph G {{")?;
        for (ix, Node { value, .. }) in &self.node_map {
            writeln!(f, "\tn{} [label=\"#{} {:?}\"];", ix.0, ix.0, value)?;
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
    /// Take in reverse direction.
    rev_dir: bool,
}

impl<'a, N, E> Bfs<'a, N, E> {
    /// Creates a new iterator that starts from a given label and traverses a
    /// `graph`. Set `rev_dir` to true to traverse it in reverse direction.
    fn new(graph: &'a Cfg<N, E>, start: &'a CfgLabel, rev_dir: bool) -> Self {
        Self {
            queue: [start].into(),
            visited: HashSet::new(),
            graph,
            rev_dir,
        }
    }
}

impl<'a, N, E> Iterator for Bfs<'a, N, E> {
    type Item = (&'a NodeIx, &'a N);

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.queue.pop_front()?;
        let iter: Box<dyn Iterator<Item = &NodeIx>> = if self.rev_dir {
            boxed(self.graph.preneighbors(node))
        } else {
            boxed(self.graph.neighbors(node))
        };
        for node in iter {
            if self.visited.insert(node) {
                // If `visited` did not contain this value before
                self.queue.push_back(node);
            }
        }
        Some((node, &self.graph[node]))
    }
}

/// Breadth-First Search mutable iterator over a graph.
pub struct BfsMut<'a, N, E> {
    /// Queue of elements to visit.
    queue: VecDeque<NodeIx>,
    /// Already visited elements.
    visited: HashSet<NodeIx>,
    /// Reference to the graph itself.
    graph: &'a mut Cfg<N, E>,
    /// Take in reverse direction.
    rev_dir: bool,
}

impl<'a, N, E> BfsMut<'a, N, E> {
    /// Creates a new iterator that starts from a given label and traverses a
    /// `graph`. Set `rev_dir` to true to traverse it in reverse direction.
    fn new(graph: &'a mut Cfg<N, E>, start: CfgLabel, rev_dir: bool) -> Self {
        Self {
            queue: [start].into(),
            visited: HashSet::new(),
            graph,
            rev_dir,
        }
    }
}

impl<'a, N, E: fmt::Debug> Iterator for BfsMut<'a, N, E> {
    type Item = (NodeIx, &'a mut N);

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.queue.pop_front()?;
        let iter: Box<dyn Iterator<Item = NodeIx>> = if self.rev_dir {
            boxed(self.graph.preneighbors(&node).cloned())
        } else {
            boxed(self.graph.neighbors(&node).cloned())
        };
        for node in iter {
            if self.visited.insert(node.clone()) {
                // If `visited` did not contain this value before
                self.queue.push_back(node);
            }
        }

        // Hugely UNSAFE: we must prove that we will return mutable access to different
        // parts of the graph. The borrow checker is unable to see that nodes
        // that are returned here are always different. So the &mut N can be upgraded to
        // &'a mut N. Since we will NOT use `self.graph[&node]` for the rest of `a,
        // which is the lifetime of the iterator.
        Some((node.clone(), unsafe {
            &mut *(&mut self.graph[&node] as *mut N)
        }))
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
    /// Take in reverse direction.
    rev_dir: bool,
}

impl<'a, N, E> Dfs<'a, N, E> {
    /// Creates a new iterator that starts from a given label and traverses a
    /// `graph`. Set `rev_dir` to true to traverse it in reverse direction.
    fn new(graph: &'a Cfg<N, E>, start: &'a CfgLabel, rev_dir: bool) -> Self {
        Self {
            stack: [start].into(),
            visited: HashSet::new(),
            graph,
            rev_dir,
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
                let iter: Box<dyn Iterator<Item = &NodeIx>> = if self.rev_dir {
                    boxed(self.graph.preneighbors(node))
                } else {
                    boxed(self.graph.neighbors(node))
                };
                for node in iter {
                    self.stack.push(node);
                }
                break node;
            }
        };
        Some((node, &self.graph[node]))
    }
}

/// Control-flow Search iterator over a graph.
pub struct CfsSelf {
    /// Queue of elements to visit.
    queue: PriorityQueue<(Branch, NodeIx), Stratum>,
    /// Already visited elements.
    visited: HashSet<NodeIx>,
    /// The graph to traverse.
    graph: Cfg,
}

impl CfsSelf {
    /// Creates a new iterator that starts from a given label and traverses
    /// `graph`.
    fn new(graph: Cfg, start: CfgLabel) -> Self {
        let mut queue = PriorityQueue::new();
        let scope = graph[&start].scope;
        queue.push((Branch::default(), start), scope);
        Self {
            queue,
            visited: HashSet::new(),
            graph,
        }
    }
}

impl Iterator for CfsSelf {
    type Item = (Branch, bool, Instr);

    fn next(&mut self) -> Option<Self::Item> {
        let ((branch, label), _) = self.queue.pop()?;
        let node = self.graph.node_map.remove(&label).unwrap();
        let neighbors = node.outs;

        let is_loop = node
            .ins
            .iter()
            .any(|(edge, _)| self.graph.edge_map.get(edge).is_some());
        for (branch, edge) in neighbors {
            let edge = self.graph.edge_map.remove(&edge).unwrap();
            let neighbor = edge.to;
            if self.visited.insert(neighbor.clone()) {
                let scope = self.graph[&neighbor].scope;
                self.queue.push((branch, neighbor), scope);
            }
        }
        Some((branch, is_loop, node.value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Check that in edges matches out edges in a block.
    #[test]
    fn ins_and_outs_block() {
        let mut cfg = Cfg::default();
        let l = cfg.add_block([1, 1, 2, 3, 5, 8, 13], ());
        for i in 0..6 {
            assert_eq!(
                cfg.node_map[&l[i]].outs.values().collect::<Vec<_>>(),
                cfg.node_map[&l[i + 1]].ins.keys().collect::<Vec<_>>()
            );
        }
    }

    /// Check that in edges matches out edges in a cyclic graph.
    #[test]
    fn ins_and_outs_cyclic() {
        let mut cfg = Cfg::default();

        // Nodes
        let l = [cfg.add_node(0), cfg.add_node(1), cfg.add_node(2)];

        // Edges
        for i in 0..3 {
            cfg.add_edge(l[i].clone(), l[(i + 1) % 3].clone(), Branch::default(), ());
        }
        for i in 0..3 {
            assert_eq!(
                cfg.node_map[&l[i]].outs.values().collect::<Vec<_>>(),
                cfg.node_map[&l[(i + 1) % 3]].ins.keys().collect::<Vec<_>>()
            );
        }
    }

    #[test]
    fn multiple_ins_mir() {
        use crate::*;
        let mir = borrow_check(mir(check(crate::examples::while_loop())));
        let cfg = &mir.funs["main"].body;
        let edges = cfg.node_map[&NodeIx(4)]
            .ins
            .keys()
            .map(|ix| &cfg.edge_map[ix])
            .collect::<HashSet<_>>();
        assert_eq!(
            edges,
            [
                &Edge {
                    from: NodeIx(20),
                    to: NodeIx(4),
                    weight: ()
                },
                &Edge {
                    from: NodeIx(5),
                    to: NodeIx(4),
                    weight: ()
                }
            ]
            .into_iter()
            .collect::<HashSet<_>>()
        );
    }
}
