//! The connection graph: an undirected adjacency structure over the
//! geosphere's cells, carrying natural travel routes (never built roads —
//! see `EdgeKind`) and their reachability under a conductance threshold.

use hornvale_kernel::CellId;
use std::collections::{BTreeMap, BTreeSet};

/// The kind of natural route an edge represents. These are derived from
/// terrain and hydrology, never authored infrastructure — hence no `Road`
/// variant.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EdgeKind {
    /// Two cells share a mesh boundary (bare terrain adjacency).
    Adjacency,
    /// A navigable water connection (river or coastal corridor).
    WaterRoute,
    /// An overland corridor easier than raw adjacency (a pass, a plain).
    LandRoute,
}

/// One directed half of an undirected connection between two cells.
/// `ConnectionGraph::add_edge` stores both halves.
/// type-audit: bare-ok(ratio: conductance)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Edge {
    /// The cell this edge leads to.
    pub to: CellId,
    /// What kind of natural route this edge represents.
    pub kind: EdgeKind,
    /// Dimensionless ease-of-travel: higher is easier. Compared against a
    /// caller-chosen threshold by `reachable_regions`; not a physical unit,
    /// so a bare `f64` is appropriate.
    pub conductance: f64,
}

/// The world's derived transport topology: an undirected graph over
/// `CellId`, backed by a `BTreeMap` adjacency list for determinism (no
/// `HashMap` — iteration and comparison order must never depend on hash
/// seed or insertion order).
#[derive(Clone, Debug)]
pub struct ConnectionGraph {
    adjacency: BTreeMap<CellId, Vec<Edge>>,
}

impl ConnectionGraph {
    /// Build an empty graph over `node_count` cells, `CellId(0)` through
    /// `CellId(node_count - 1)`, each starting with no edges.
    /// type-audit: bare-ok(count: node_count)
    pub fn new(node_count: usize) -> Self {
        let mut adjacency = BTreeMap::new();
        for i in 0..node_count {
            adjacency.insert(CellId(i as u32), Vec::new());
        }
        ConnectionGraph { adjacency }
    }

    /// Add an undirected edge: `edge` is appended to `from`'s adjacency
    /// list, and its mirror (pointing back to `from`, same kind and
    /// conductance) is appended to `edge.to`'s list.
    pub fn add_edge(&mut self, from: CellId, edge: Edge) {
        let to = edge.to;
        let mirror = Edge {
            to: from,
            kind: edge.kind,
            conductance: edge.conductance,
        };
        self.adjacency.entry(from).or_default().push(edge);
        self.adjacency.entry(to).or_default().push(mirror);
    }

    /// The edges leaving `from`, in insertion order. Empty (not absent) for
    /// a node with no edges yet.
    pub fn edges(&self, from: CellId) -> &[Edge] {
        self.adjacency.get(&from).map(Vec::as_slice).unwrap_or(&[])
    }

    /// Every node currently in the graph, in ascending `CellId` order (the
    /// `BTreeMap` key order).
    pub fn nodes(&self) -> impl Iterator<Item = CellId> + '_ {
        self.adjacency.keys().copied()
    }

    /// Connected components over edges whose `conductance >= min_conductance`.
    /// Each component is a `BTreeSet<CellId>`; the returned `Vec` is ordered
    /// by each component's minimum `CellId`, so the result is deterministic
    /// regardless of edge-insertion order.
    /// type-audit: bare-ok(ratio: min_conductance)
    pub fn reachable_regions(&self, min_conductance: f64) -> Vec<BTreeSet<CellId>> {
        let mut unvisited: BTreeSet<CellId> = self.adjacency.keys().copied().collect();
        let mut components: Vec<BTreeSet<CellId>> = Vec::new();

        while let Some(&start) = unvisited.iter().next() {
            let mut component = BTreeSet::new();
            let mut frontier = vec![start];
            unvisited.remove(&start);
            component.insert(start);

            while let Some(node) = frontier.pop() {
                for edge in self.edges(node) {
                    if edge.conductance >= min_conductance && unvisited.remove(&edge.to) {
                        component.insert(edge.to);
                        frontier.push(edge.to);
                    }
                }
            }

            components.push(component);
        }

        components.sort_by(|a, b| a.iter().next().cmp(&b.iter().next()));
        components
    }
}
