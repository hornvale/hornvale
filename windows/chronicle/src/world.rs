//! The derived synthetic state and its stable text digest.

use crate::config::{EventKind, NodeId, RoleHandle, SpeciesId};
use std::collections::BTreeMap;
use std::fmt::Write as _;

/// A synthetic species: placeholder trait weights, the two the dynamics read.
/// type-audit: bare-ok(ratio: carrying_need), bare-ok(ratio: frequency_weight)
#[derive(Clone, Copy, Debug)]
pub struct SpeciesStub {
    /// Per-head resource need (scales the pressure threshold).
    pub carrying_need: f64,
    /// Global commonality weight (rank-abundance placeholder).
    pub frequency_weight: f64,
}

/// A connection-graph edge out of a node.
/// type-audit: bare-ok(count: lag)
#[derive(Clone, Copy, Debug)]
pub struct Edge {
    /// The destination node.
    pub to: NodeId,
    /// Propagation lag in epochs (0 for a portal, more for a route).
    pub lag: u32,
    /// The edge's kind.
    pub kind: crate::config::EdgeKind,
}

/// One dated biography entry.
/// type-audit: bare-ok(count: epoch)
#[derive(Clone, Copy, Debug)]
pub struct BioEntry {
    /// The epoch the event occurred.
    pub epoch: u32,
    /// What happened.
    pub event: EventKind,
    /// The (lazily-expandable) individual the event implies.
    pub actor: RoleHandle,
}

/// A community: a persistent entity with a dated biography.
/// type-audit: bare-ok(count: population), bare-ok(flag: alive)
#[derive(Clone, Debug)]
pub struct Community {
    /// Which synthetic species.
    pub species: SpeciesId,
    /// Current population (an abstract head-count).
    pub population: f64,
    /// The graph node it occupies.
    pub node: NodeId,
    /// Its dated history.
    pub biography: Vec<BioEntry>,
    /// Whether it still stands (false once collapsed).
    pub alive: bool,
}

/// The trace a collapsed community leaves behind.
/// type-audit: bare-ok(count: epoch)
#[derive(Clone, Copy, Debug)]
pub struct Ruin {
    /// The node where it died.
    pub node: NodeId,
    /// The epoch of collapse.
    pub epoch: u32,
}

/// A fully-derived synthetic world (past baked, present at the final epoch).
/// type-audit: bare-ok(count: capacity)
#[derive(Clone, Debug)]
pub struct World {
    /// The synthetic species table (Y entries).
    pub species: Vec<SpeciesStub>,
    /// The synthetic communities (grows past Z as foundings fire).
    pub communities: Vec<Community>,
    /// The sparse connection graph (adjacency list).
    pub graph: BTreeMap<NodeId, Vec<Edge>>,
    /// The per-node synthetic carrying capacity.
    pub capacity: Vec<f64>,
    /// Ruins left by collapses.
    pub ruins: Vec<Ruin>,
}

/// A census of the *workload* a run produced — how many of each event
/// actually fired. This is the transparency-and-floor surface: a benchmark
/// that measures a phenomenon must prove the phenomenon happened at a
/// meaningful sample size, or its timings are measuring noise.
/// type-audit: bare-ok(count: grew), bare-ok(count: founded), bare-ok(count: raided), bare-ok(count: fled), bare-ok(count: collapsed), bare-ok(count: communities_total), bare-ok(count: communities_alive)
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Census {
    /// `Grew` events.
    pub grew: u64,
    /// `Founded` events.
    pub founded: u64,
    /// `Raided` events — the inter-community coupling firing.
    pub raided: u64,
    /// `Fled` events — deliveries actually landing (the coupling completing).
    pub fled: u64,
    /// `Collapsed` events — communities dying into ruins.
    pub collapsed: u64,
    /// Total communities ever created (initial + founded + refounded).
    pub communities_total: u64,
    /// Communities still standing at the end.
    pub communities_alive: u64,
}

impl Census {
    /// Total events of all kinds.
    /// type-audit: bare-ok(count: return)
    pub fn events(&self) -> u64 {
        self.grew + self.founded + self.raided + self.fled + self.collapsed
    }
}

/// Count the workload a baked world represents.
pub fn census(world: &World) -> Census {
    let mut c = Census {
        communities_total: world.communities.len() as u64,
        ..Census::default()
    };
    for community in &world.communities {
        if community.alive {
            c.communities_alive += 1;
        }
        for entry in &community.biography {
            match entry.event {
                EventKind::Grew => c.grew += 1,
                EventKind::Founded => c.founded += 1,
                EventKind::Raided => c.raided += 1,
                EventKind::Fled => c.fled += 1,
                EventKind::Collapsed => c.collapsed += 1,
            }
        }
    }
    c
}

/// A stable, byte-deterministic text rendering of every biography — the
/// determinism-test surface (timings are NOT part of this).
/// type-audit: bare-ok(prose: return)
pub fn biography_digest(world: &World) -> String {
    let mut out = String::new();
    for (i, c) in world.communities.iter().enumerate() {
        let _ = write!(
            out,
            "c{i} sp{} node{} pop{} alive{}",
            c.species.0, c.node.0, c.population as i64, c.alive
        );
        for e in &c.biography {
            let _ = write!(out, " [{}:{:?}:{}]", e.epoch, e.event, e.actor.0);
        }
        out.push('\n');
    }
    out
}
