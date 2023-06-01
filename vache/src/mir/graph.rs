//! Representing control flow graphs.

use std::collections::{HashMap, HashSet, VecDeque};
use std::default::default;
use std::hash::Hash;
use std::ops::Index;
use std::ops::{Deref, IndexMut};
use std::{fmt, iter};

use super::{Branch, Instr};
use crate::utils::boxed;
use crate::utils::set::Set;

/// A node index in the graph.
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash)]
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
pub struct Cfg<N, E = ()> {
    /// Map of node indexes to node data.
    node_map: HashMap<NodeIx, Node<N>>,
    /// Map of edge indexes to the edge data.
    edge_map: HashMap<EdgeIx, Edge<E>>,
    /// Fresh node index counter.
    node_ix_counter: u64,
    /// Fresh edge index counter.
    edge_ix_counter: u64,
}

/// A CFG with instructions as nodes.
pub type CfgI<'ctx> = Cfg<Instr<'ctx>>;

impl<N, E> Cfg<N, E> {
    /// Starting from a node, takes a given branch/path. Returns the target
    /// node, if any.
    pub fn take_branch(&self, node: CfgLabel, branch: &Branch) -> Option<CfgLabel> {
        self.node_map[&node]
            .outs
            .get(branch)
            .map(|e| self.edge_map[e].to)
    }

    /// Adds a new node to the graph, returning its label in the CFG.
    pub fn add_node(&mut self, node: N) -> CfgLabel {
        let label = NodeIx(self.node_ix_counter);
        self.node_ix_counter = self.node_ix_counter.checked_add(1).unwrap();
        self.node_map.insert(label, node.into());
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
    pub fn neighbors(&self, node: CfgLabel) -> impl Iterator<Item = CfgLabel> + '_ {
        self.node_map[&node]
            .outs
            .values()
            .map(|e| self.edge_map[e].to)
    }

    /// Ingoing neighbors.
    pub fn preneighbors(&self, node: CfgLabel) -> impl Iterator<Item = CfgLabel> + '_ {
        self.node_map[&node]
            .ins
            .keys()
            .map(|e| self.edge_map[e].from)
    }

    /// Returns an immutable BFS iterator over the graph.
    ///
    /// `rev_dir`: take arrows in opposite direction.
    pub fn bfs(&self, start: CfgLabel, rev_dir: bool) -> Bfs<'_, N, E> {
        Bfs::new(self, start, rev_dir)
    }

    /// Returns a mutable BFS iterator over the graph.
    ///
    /// `rev_dir`: take arrows in opposite direction.
    pub fn bfs_mut(&mut self, start: CfgLabel, rev_dir: bool) -> BfsMut<'_, N, E> {
        BfsMut::new(self, start, rev_dir)
    }

    /// Returns an immutable DFS iterator over the graph.
    ///
    /// `rev_dir`: take arrows in opposite direction.
    pub fn dfs(&self, start: CfgLabel, rev_dir: bool) -> Dfs<'_, N, E> {
        Dfs::new(self, start, rev_dir)
    }

    /// Returns a postorder iterator over the graph.
    pub fn postorder(&self, start: CfgLabel) -> PostOrder<'_, N, E> {
        PostOrder::new(self, start)
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
                        k,
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
                        *k,
                        Node {
                            value: node_map(*k, &v.value),
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
}

impl<'ctx> CfgI<'ctx> {
    /// Returns the set of dominators for each node.
    ///
    /// Dominators for node `n` are the set of nodes through which you MUST pass
    /// to go from `start_l` to `n`.
    pub fn dominators(&self, start_l: CfgLabel) -> HashMap<CfgLabel, Set<CfgLabel>> {
        // Initially, the set of dominators are all labels, except for the start label
        // for which we already know its set of dominators (only itself).
        let initial_set: Set<NodeIx> = self.node_map.keys().copied().collect();
        let mut dominators: HashMap<NodeIx, Set<NodeIx>> = self
            .node_map
            .keys()
            .map(|k| (*k, initial_set.clone()))
            .collect();
        dominators.insert(start_l, [start_l].into_iter().collect());

        let mut changed = true;
        while changed {
            changed = false;
            for (label, _) in self.postorder(start_l).rev() {
                // Skip start label
                if label == start_l {
                    continue;
                }

                let mut new_set = iter::once(&initial_set)
                    .chain(self.preneighbors(label).map(|p| &dominators[&p]))
                    .product::<Set<NodeIx>>();
                new_set.insert(label);
                if new_set != dominators[&label] {
                    dominators.insert(label, new_set);
                    changed = true;
                }
            }
        }
        dominators
    }

    /// Returns the immediate dominator of each node, except for the starting
    /// label.
    ///
    /// Assuming the only entry point is at `start_l`.
    pub fn immediate_dominators(&self, start_l: CfgLabel) -> HashMap<CfgLabel, CfgLabel> {
        let mut dominators = self.dominators(start_l);
        let mut idoms: HashMap<CfgLabel, CfgLabel> = default();
        for (label, _) in self.postorder(start_l).rev() {
            let mut dominators: Set<NodeIx> = dominators.remove(&label).unwrap();
            if label == start_l {
                continue;
            }
            let prev_idoms: Vec<CfgLabel> = dominators
                .iter()
                .filter_map(|&dom| {
                    if dom != start_l && dom != label {
                        Some(
                            idoms
                                .get(&dom)
                                .copied()
                                .expect(&format!("No {dom:?} for {label:?}")),
                        )
                    } else {
                        None
                    }
                })
                .collect();
            dominators.remove(&label);
            for i in prev_idoms {
                dominators.remove(&i);
            }
            assert!(dominators.len() == 1, "Dominators: {:?}", dominators);
            idoms.insert(label, dominators.into_iter().next().unwrap());
        }
        idoms
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
            self.add_edge(*from, *to, Branch::DefaultB, weight);
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
    queue: VecDeque<NodeIx>,
    /// Already visited elements.
    visited: HashSet<NodeIx>,
    /// Reference to the graph itself.
    graph: &'a Cfg<N, E>,
    /// Take in reverse direction.
    rev_dir: bool,
}

impl<'a, N, E> Bfs<'a, N, E> {
    /// Creates a new iterator that starts from a given label and traverses a
    /// `graph`. Set `rev_dir` to true to traverse it in reverse direction.
    fn new(graph: &'a Cfg<N, E>, start: CfgLabel, rev_dir: bool) -> Self {
        Self {
            queue: [start].into(),
            visited: HashSet::new(),
            graph,
            rev_dir,
        }
    }
}

impl<'a, N, E> Iterator for Bfs<'a, N, E> {
    type Item = (NodeIx, &'a N);

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.queue.pop_front()?;
        let iter: Box<dyn Iterator<Item = NodeIx>> = if self.rev_dir {
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
        Some((node, &self.graph[&node]))
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

        // Hugely UNSAFE: we must prove that we will return mutable access to different
        // parts of the graph. The borrow checker is unable to see that nodes
        // that are returned here are always different. So the &mut N can be upgraded to
        // &'a mut N. Since we will NOT use `self.graph[&node]` for the rest of `a,
        // which is the lifetime of the iterator.
        Some((node, unsafe { &mut *(&mut self.graph[&node] as *mut N) }))
    }
}

/// Depth-First Search iterator over a graph.
pub struct Dfs<'a, N, E> {
    /// Stack of elements to visit.
    stack: Vec<NodeIx>,
    /// Already visited elements.
    visited: HashSet<NodeIx>,
    /// Reference to the graph itself.
    graph: &'a Cfg<N, E>,
    /// Take in reverse direction.
    rev_dir: bool,
}

impl<'a, N, E> Dfs<'a, N, E> {
    /// Creates a new iterator that starts from a given label and traverses a
    /// `graph`. Set `rev_dir` to true to traverse it in reverse direction.
    fn new(graph: &'a Cfg<N, E>, start: CfgLabel, rev_dir: bool) -> Self {
        Self {
            stack: [start].into(),
            visited: HashSet::new(),
            graph,
            rev_dir,
        }
    }
}

impl<'a, N, E> Iterator for Dfs<'a, N, E> {
    type Item = (NodeIx, &'a N);

    fn next(&mut self) -> Option<Self::Item> {
        let node = loop {
            let node = self.stack.pop()?;
            if self.visited.insert(node) {
                // If `visited` did not contain this value before
                let iter: Box<dyn Iterator<Item = NodeIx>> = if self.rev_dir {
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
        Some((node, &self.graph[&node]))
    }
}

/// Post-order iterator over a graph.
///
/// Its reverse is the standard flow control linearization you would expect: it
/// returns items in the order you would see them in a source code.
pub struct PostOrder<'a, N, E> {
    /// CFG to traverse.
    graph: &'a Cfg<N, E>,
    /// Entry label in the CFG.
    start: NodeIx,
    /// We'll store here the result of the post-order dfs search.
    result: VecDeque<(NodeIx, &'a N)>,
    /// Have we computed the dfs yet.
    ///
    /// We only compute the dfs once the first element of the iterator is
    /// consumed, not when it is created, which is why we need this flag.
    computed: bool,
}

impl<'a, N, E> PostOrder<'a, N, E> {
    /// Creates a new iterator that starts from a given label and traverses a
    /// `graph`. Set `rev_dir` to true to traverse it in reverse direction.
    fn new(graph: &'a Cfg<N, E>, start: CfgLabel) -> Self {
        Self {
            graph,
            start,
            result: VecDeque::new(),
            computed: false,
        }
    }

    /// Performs the postorder computation.
    ///
    /// Called lazily only when we consume the first element of our iterator.
    fn compute(&mut self) {
        /// Compute DFS postorder and output the result to `result`.
        fn dfs<'a, N, E>(
            graph: &'a Cfg<N, E>,
            label: CfgLabel,
            visited: &mut HashSet<NodeIx>,
            result: &mut VecDeque<(NodeIx, &'a N)>,
        ) {
            if visited.insert(label) {
                // If `visited` did not contain this value before
                for node in graph.neighbors(label) {
                    dfs(graph, node, visited, result);
                }
                result.push_back((label, &graph[&label]));
            }
        }
        let mut visited = HashSet::new();
        dfs(self.graph, self.start, &mut visited, &mut self.result);
        self.computed = true;
    }
}

impl<'a, N, E> Iterator for PostOrder<'a, N, E> {
    type Item = (NodeIx, &'a N);

    fn next(&mut self) -> Option<Self::Item> {
        // If we did not compute it, compute it now.
        if !self.computed {
            self.compute();
        }

        self.result.pop_front()
    }
}

impl<'a, N, E> DoubleEndedIterator for PostOrder<'a, N, E> {
    fn next_back(&mut self) -> Option<Self::Item> {
        // If we did not compute it, compute it now.
        if !self.computed {
            self.compute();
        }

        self.result.pop_back()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;

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
            cfg.add_edge(l[i], l[(i + 1) % 3], Branch::default(), ());
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
        let arena = Arena::new();
        let config = Config { input: "" };
        let mut ctx = Context::new(config, &arena);
        let mut checked = check(&mut ctx, crate::examples::while_loop());
        let mir = borrow_check(mir(&mut checked));
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

    /// Checks our dominator algorithm with a simple if-then-else flow.
    #[test]
    fn dominators() {
        use crate::*;
        let arena = Arena::new();
        let config = Config { input: "" };
        let mut ctx = Context::new(config, &arena);
        let mut checked = check(&mut ctx, crate::examples::simple_if());
        let mir = borrow_check(mir(&mut checked));
        let main_fn = &mir.funs["main"];
        let dominators = main_fn.body.dominators(main_fn.entry_l);

        // Dominators of the label _after_ the if statement are only the labels _before_
        // the if statement.
        let above_if: Vec<NodeIx> = (5..11).map(NodeIx).collect();
        assert_eq!(
            dominators[&main_fn.ret_l],
            iter::once(main_fn.ret_l)
                .chain(above_if.iter().copied())
                .collect()
        );
    }

    /// Checks our immediate dominator algorithm with a simple if-then-else
    /// flow.
    #[test]
    fn immediate_dominators() {
        use crate::*;
        let arena = Arena::new();
        let config = Config { input: "" };
        let mut ctx = Context::new(config, &arena);
        let mut checked = check(&mut ctx, crate::examples::simple_if());
        let mir = borrow_check(mir(&mut checked));
        println!("MIR: {mir:?}");
        let main_fn = &mir.funs["main"];
        let dominators = main_fn.body.immediate_dominators(main_fn.entry_l);

        // The immediate of the label _after_ the if statement can only be the one
        // just before the if statement. Confer the CFG of that function for
        // details.
        assert_eq!(dominators[&main_fn.ret_l], NodeIx(5));
    }
}
