//! The Sounding's input surface: six config fields, plus the small id and
//! enum types the derived state is built from.

use hornvale_kernel::Seed;

/// The entire input surface of one Sounding run. Everything else is derived
/// deterministically from `(seed, this)`.
/// type-audit: bare-ok(count: communities), bare-ok(count: species), bare-ok(count: epochs), bare-ok(ratio: avg_degree), bare-ok(ratio: long_range_fraction)
#[derive(Clone, Debug)]
pub struct SoundingConfig {
    /// Determinism source.
    pub seed: Seed,
    /// Z — how many synthetic communities to seed.
    pub communities: u32,
    /// Y — how many synthetic species.
    pub species: u32,
    /// A — history length, in epochs.
    pub epochs: u32,
    /// d̄ — mean connection-graph degree (edges per node).
    pub avg_degree: f64,
    /// Portal/route edges as a fraction of all edges (the long-range case).
    pub long_range_fraction: f64,
}

/// A node in the connection graph (a place a community occupies).
/// type-audit: bare-ok(index: 0)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeId(pub u32);

/// An index into the synthetic species table.
/// type-audit: bare-ok(index: 0)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SpeciesId(pub u32);

/// A lazily-expandable handle to the individual a biography event implies
/// (e.g. "the chieftain who led the flight"). Never expanded in the spike;
/// it stands in for the real engine's role-handle so its cost is present.
/// type-audit: bare-ok(identifier-text: 0)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RoleHandle(pub u64);

/// How a delivery finds the community occupying its target node — the axis
/// the benchmark exists to compare. `Scan` is the naive linear scan (O(Z) per
/// delivery, O(Z²) coupling); `Index` uses the maintained node→community
/// index (O(log Z)). With one alive community per node enforced, both find
/// the *same* community, so they produce byte-identical worlds and differ
/// only in speed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DeliveryMode {
    /// Naive linear scan for the alive community at the node — O(Z)/delivery.
    Scan,
    /// The maintained node→community index — O(log Z)/delivery.
    Index,
}

/// The kind of a connection-graph edge (its lag/character differs by kind).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EdgeKind {
    /// Spatial adjacency — the local, zero-ish-lag case.
    Adjacent,
    /// A road or sailing lane — a lagged medium-range edge.
    Route,
    /// A portal — an instant long-range edge.
    Portal,
}

/// A community-level event appended to a biography.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EventKind {
    /// Population grew under slack pressure.
    Grew,
    /// Spawned a new community (a founding).
    Founded,
    /// Raided a graph neighbour, displacing its people.
    Raided,
    /// Was displaced by a raid and relocated (refounding elsewhere).
    Fled,
    /// Collapsed under pressure, leaving a ruin.
    Collapsed,
}
