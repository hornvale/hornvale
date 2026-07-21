# The Sounding — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a computational feasibility benchmark — a representative core-loop spike of the derived-history engine over synthetic inputs — that sweeps (communities, species, epochs, coupling density) and reports the feasibility frontier, so the owner can *see* where each cost budget breaks instead of guessing.

**Architecture:** A new kernel-only crate `hornvale-chronicle` (`windows/chronicle`) holds a pure deterministic `run(&SoundingConfig) -> World` core loop: a `SoundingConfig` of six fields derives four kinds of synthetic state (species table, community table, sparse connection graph, capacity field), then an epoch loop resolves seeded structural-pressure events (grow/found/raid→displace/collapse) with graph-coupled displacement and a diffusive quantity, producing dated biographies. Three instrumented entry points measure genesis-bake / read / present-replay; a heavy-tier sweep harness fits scaling exponents against preregistered hypotheses and emits a census-style report.

**Tech Stack:** Rust (edition 2024), `hornvale-kernel` only (`Seed`/`Stream`/`quantize`), std. No new dependencies.

## Global Constraints

- **Dependencies:** `hornvale-kernel` + std only for the crate; **no** `serde`/`serde_json` needed (the report is written as plain text/CSV by hand). No new workspace crates in `Cargo.toml` beyond the auto-globbed `windows/chronicle`. (Dependency allowlist: `cli/tests/architecture.rs`.)
- **Layering:** `hornvale-chronicle` depends on **`hornvale-kernel` and nothing else** (it is window-tier by placement but kernel-only by dependency, because inputs are synthetic). Never depend on a domain.
- **Determinism (constitutional):** all randomness via `hornvale_kernel::Seed`/`Stream`; **no `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only; float ordering via `f64::total_cmp`; **no wall-clock in sim logic** (`WorldTime`-style abstract epochs are `u32`). Same seed + same config → **byte-identical** biographies.
- **The one sanctioned wall-clock exception:** the *benchmark harness* (T2/T3 timing only) uses `std::time::Instant` under a scoped `#[allow(clippy::disallowed_types)]` with a comment — it measures the loop, it is not sim logic. The core loop (T1) contains **no** timing and **no** `Instant`.
- **Quantize at emit:** any `f64` written into the report is passed through `hornvale_kernel::quantize` first.
- **Every crate sets `#![warn(missing_docs)]`;** every public item/field/variant gets a one-line doc comment.
- **Not genesis-changing:** no epoch, no stream-label save-format contract on the real world, no census regen. Synthetic and isolated.
- **`cargo fmt` is the final step before every commit.** The commit gate is `SKIP_CENSUS=1 make gate` (fmt + clippy + nextest + doctests); the heavy sweep is `#[ignore = "heavy: …make gate-full"]` and never runs in the commit gate.

---

## File Structure

- `windows/chronicle/Cargo.toml` — the crate manifest (kernel-only).
- `windows/chronicle/src/lib.rs` — crate root: `#![warn(missing_docs)]`, module wiring, re-exports.
- `windows/chronicle/src/config.rs` — `SoundingConfig` and the small id/enum types (`NodeId`, `SpeciesId`, `EdgeKind`, `EventKind`, `RoleHandle`).
- `windows/chronicle/src/world.rs` — the derived state types (`SpeciesStub`, `Edge`, `BioEntry`, `Community`, `Ruin`, `World`) and the biography serializer used by determinism tests.
- `windows/chronicle/src/generate.rs` — deriving the four synthetic-state kinds from `(seed, config)`.
- `windows/chronicle/src/simulate.rs` — the epoch loop: `run(&SoundingConfig) -> World`.
- `windows/chronicle/src/measure.rs` — the three instrumented directions (`bake`, `read`, `replay`) and their metric structs.
- `windows/chronicle/src/sweep.rs` — the parameter sweep, scaling-exponent fit, and report rendering (plain text).
- `windows/chronicle/tests/determinism.rs` — fast gate tests: byte-identical biographies, constitution cleanliness.
- `windows/chronicle/tests/sounding_sweep.rs` — the heavy-tier sweep entry (`#[ignore = "heavy: …"]`) that writes the report.
- `book/src/laboratory/generated/the-sounding/` — the committed report: `summary.md`, `rows.csv`, `sample-biographies.txt`.

---

## Task 1: The skeleton core loop

**Files:**
- Create: `windows/chronicle/Cargo.toml`
- Create: `windows/chronicle/src/lib.rs`
- Create: `windows/chronicle/src/config.rs`
- Create: `windows/chronicle/src/world.rs`
- Create: `windows/chronicle/src/generate.rs`
- Create: `windows/chronicle/src/simulate.rs`
- Test: `windows/chronicle/tests/determinism.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::{Seed, Stream, quantize}`.
- Produces:
  - `SoundingConfig { seed: Seed, communities: u32, species: u32, epochs: u32, avg_degree: f64, long_range_fraction: f64 }`
  - `run(config: &SoundingConfig) -> World`
  - `World { species: Vec<SpeciesStub>, communities: Vec<Community>, graph: BTreeMap<NodeId, Vec<Edge>>, capacity: Vec<f64>, ruins: Vec<Ruin> }`
  - `Community { species: SpeciesId, population: f64, node: NodeId, biography: Vec<BioEntry>, alive: bool }`
  - `BioEntry { epoch: u32, event: EventKind, actor: RoleHandle }`
  - `fn biography_digest(world: &World) -> String` — a stable text rendering of every community's biography, for determinism assertions.

- [ ] **Step 1: Create the crate manifest**

`windows/chronicle/Cargo.toml`:

```toml
[package]
name = "hornvale-chronicle"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "The Sounding: a feasibility spike of the derived-history (living-community) engine over synthetic inputs."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
```

- [ ] **Step 2: Scaffold the crate root and the config/type module**

`windows/chronicle/src/lib.rs`:

```rust
//! The Sounding — a computational feasibility spike of the derived-history
//! engine (frontier "the living community"). Synthetic inputs, placeholder
//! dynamics, real data structures and constitution — enough to measure the
//! loop's SCALING (especially the inter-community coupling), not the engine.
#![warn(missing_docs)]

mod config;
mod generate;
mod simulate;
mod world;

pub mod measure;
pub mod sweep;

pub use config::{EdgeKind, EventKind, NodeId, RoleHandle, SoundingConfig, SpeciesId};
pub use simulate::run;
pub use world::{biography_digest, BioEntry, Community, Edge, Ruin, SpeciesStub, World};
```

`windows/chronicle/src/config.rs`:

```rust
//! The Sounding's input surface: six config fields, plus the small id and
//! enum types the derived state is built from.

use hornvale_kernel::Seed;

/// The entire input surface of one Sounding run. Everything else is derived
/// deterministically from `(seed, this)`.
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeId(pub u32);

/// An index into the synthetic species table.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SpeciesId(pub u32);

/// A lazily-expandable handle to the individual a biography event implies
/// (e.g. "the chieftain who led the flight"). Never expanded in the spike;
/// it stands in for the real engine's role-handle so its cost is present.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RoleHandle(pub u64);

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
```

- [ ] **Step 3: Write the failing determinism test**

`windows/chronicle/tests/determinism.rs`:

```rust
//! Fast gate tests: the spike is byte-deterministic and non-trivial.

use hornvale_chronicle::{biography_digest, run, SoundingConfig};
use hornvale_kernel::Seed;

fn cfg(seed: u64) -> SoundingConfig {
    SoundingConfig {
        seed: Seed(seed),
        communities: 200,
        species: 8,
        epochs: 300,
        avg_degree: 4.0,
        long_range_fraction: 0.05,
    }
}

#[test]
fn same_seed_same_config_is_byte_identical() {
    let a = biography_digest(&run(&cfg(42)));
    let b = biography_digest(&run(&cfg(42)));
    assert_eq!(a, b, "same seed + config must produce byte-identical biographies");
}

#[test]
fn different_seeds_diverge() {
    let a = biography_digest(&run(&cfg(42)));
    let b = biography_digest(&run(&cfg(43)));
    assert_ne!(a, b, "a different seed must produce a different history");
}

#[test]
fn the_loop_actually_produces_events() {
    // Guard against a vacuous spike: a 200-community, 300-epoch run must
    // append a substantial number of events, and exercise the coupling
    // (at least one Raided/Fled pair) — else the benchmark measures nothing.
    let world = run(&cfg(42));
    let total: usize = world.communities.iter().map(|c| c.biography.len()).sum();
    assert!(total > 500, "expected many events, got {total}");
    let raids = world
        .communities
        .iter()
        .flat_map(|c| &c.biography)
        .filter(|e| matches!(e.event, hornvale_chronicle::EventKind::Raided))
        .count();
    assert!(raids > 0, "the coupling never fired: no raids in {total} events");
}
```

- [ ] **Step 4: Run the test to verify it fails**

Run: `cargo test -p hornvale-chronicle --test determinism 2>&1 | tail -20`
Expected: FAIL to compile (`run`, `biography_digest`, `World`, … not defined).

- [ ] **Step 5: Implement the world types**

`windows/chronicle/src/world.rs`:

```rust
//! The derived synthetic state and its stable text digest.

use crate::config::{EventKind, NodeId, RoleHandle, SpeciesId};
use std::collections::BTreeMap;
use std::fmt::Write as _;

/// A synthetic species: placeholder trait weights, the two the dynamics read.
#[derive(Clone, Copy, Debug)]
pub struct SpeciesStub {
    /// Per-head resource need (scales the pressure threshold).
    pub carrying_need: f64,
    /// Global commonality weight (rank-abundance placeholder).
    pub frequency_weight: f64,
}

/// A connection-graph edge out of a node.
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
#[derive(Clone, Copy, Debug)]
pub struct Ruin {
    /// The node where it died.
    pub node: NodeId,
    /// The epoch of collapse.
    pub epoch: u32,
}

/// A fully-derived synthetic world (past baked, present at the final epoch).
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

/// A stable, byte-deterministic text rendering of every biography — the
/// determinism-test surface (timings are NOT part of this).
pub fn biography_digest(world: &World) -> String {
    let mut out = String::new();
    for (i, c) in world.communities.iter().enumerate() {
        let _ = write!(out, "c{i} sp{} node{} pop{} alive{}", c.species.0, c.node.0, c.population as i64, c.alive);
        for e in &c.biography {
            let _ = write!(out, " [{}:{:?}:{}]", e.epoch, e.event, e.actor.0);
        }
        out.push('\n');
    }
    out
}
```

- [ ] **Step 6: Implement the synthetic-state generator**

`windows/chronicle/src/generate.rs`:

```rust
//! Derive the four kinds of synthetic state from (seed, config).

use crate::config::{EdgeKind, NodeId, SoundingConfig, SpeciesId};
use crate::world::{Community, Edge, SpeciesStub, World};
use std::collections::BTreeMap;

/// Build the initial world (before the epoch loop runs).
pub(crate) fn seed_world(config: &SoundingConfig) -> World {
    let z = config.communities as usize;

    // Species table.
    let mut s = config.seed.derive("chronicle/species").stream();
    let species: Vec<SpeciesStub> = (0..config.species)
        .map(|_| SpeciesStub {
            carrying_need: 0.5 + s.next_f64(),      // 0.5..1.5
            frequency_weight: s.next_f64(),          // 0..1
        })
        .collect();

    // Per-node capacity (one node per initial community; foundings reuse nodes).
    let mut cap = config.seed.derive("chronicle/capacity").stream();
    let capacity: Vec<f64> = (0..z).map(|_| 50.0 + 150.0 * cap.next_f64()).collect();

    // Communities: a species, a starting population, its own node.
    let mut cs = config.seed.derive("chronicle/communities").stream();
    let communities: Vec<Community> = (0..z)
        .map(|i| Community {
            species: SpeciesId(if config.species == 0 { 0 } else { cs.range_u32(0, config.species) }),
            population: 10.0 + 40.0 * cs.next_f64(),
            node: NodeId(i as u32),
            biography: Vec::new(),
            alive: true,
        })
        .collect();

    let graph = build_graph(config, z);

    World { species, communities, graph, capacity, ruins: Vec::new() }
}

/// A sparse connection graph: each node gets ~avg_degree edges, a
/// `long_range_fraction` of which jump to a distant node (Route/Portal).
fn build_graph(config: &SoundingConfig, z: usize) -> BTreeMap<NodeId, Vec<Edge>> {
    let mut g: BTreeMap<NodeId, Vec<Edge>> = BTreeMap::new();
    if z == 0 {
        return g;
    }
    let mut e = config.seed.derive("chronicle/graph").stream();
    let deg = config.avg_degree.max(0.0).round() as u32;
    for i in 0..z {
        let mut edges = Vec::new();
        for _ in 0..deg {
            let long_range = e.next_f64() < config.long_range_fraction;
            let (to, kind, lag) = if long_range {
                let j = e.range_u32(0, z as u32);
                // Half of long-range edges are instant portals, half lagged routes.
                if e.next_f64() < 0.5 {
                    (j, EdgeKind::Portal, 0)
                } else {
                    (j, EdgeKind::Route, 1 + e.range_u32(0, 3))
                }
            } else {
                // Local: an adjacent index (wrap), zero-ish lag.
                let j = ((i + 1 + e.range_u32(0, 3) as usize) % z) as u32;
                (j, EdgeKind::Adjacent, 0)
            };
            edges.push(Edge { to: NodeId(to), lag, kind });
        }
        g.insert(NodeId(i as u32), edges);
    }
    g
}
```

- [ ] **Step 7: Implement the epoch loop**

`windows/chronicle/src/simulate.rs`:

```rust
//! The epoch loop: pure, deterministic `run(&SoundingConfig) -> World`.

use crate::config::{EventKind, NodeId, RoleHandle, SoundingConfig};
use crate::generate::seed_world;
use crate::world::{BioEntry, Ruin, World};
use std::collections::BTreeMap;

/// Run the whole `A`-epoch history and return the baked world.
pub fn run(config: &SoundingConfig) -> World {
    let mut world = seed_world(config);
    // Pending displacements: (arrival_epoch, target_node, incoming_population).
    let mut pending: BTreeMap<u32, Vec<(NodeId, f64)>> = BTreeMap::new();
    let mut ev = config.seed.derive("chronicle/events").stream();

    for epoch in 0..config.epochs {
        // Deliver displacements that arrive this epoch (graph-coupled, lagged).
        if let Some(arrivals) = pending.remove(&epoch) {
            for (node, pop) in arrivals {
                deliver(&mut world, config, node, pop, epoch);
            }
        }

        let n = world.communities.len();
        for i in 0..n {
            if !world.communities[i].alive {
                continue;
            }
            let c = &world.communities[i];
            let cap = world.capacity[c.node.0 as usize % world.capacity.len().max(1)];
            let need = world.species[c.species.0 as usize].carrying_need;
            let pressure = (c.population * need) / cap.max(1.0);
            let handle = RoleHandle(ev.next_u64());

            if pressure < 0.6 {
                // Slack: grow, occasionally found a daughter along an edge.
                world.communities[i].population *= 1.05;
                world.communities[i].biography.push(BioEntry { epoch, event: EventKind::Grew, actor: handle });
                if ev.next_f64() < 0.02 {
                    found_daughter(&mut world, config, i, epoch, &mut ev);
                }
            } else if pressure > 1.0 {
                // Overshoot: raid a neighbour (coupling) or collapse.
                if let Some((target, lag)) = pick_neighbour(&world.graph, world.communities[i].node, &mut ev) {
                    let taken = world.communities[i].population * 0.25;
                    world.communities[i].biography.push(BioEntry { epoch, event: EventKind::Raided, actor: handle });
                    let arrival = epoch.saturating_add(lag).min(config.epochs);
                    pending.entry(arrival).or_default().push((target, taken));
                } else {
                    world.communities[i].alive = false;
                    world.communities[i].biography.push(BioEntry { epoch, event: EventKind::Collapsed, actor: handle });
                    world.ruins.push(Ruin { node: world.communities[i].node, epoch });
                }
            } // 0.6..=1.0: stable, no event this epoch.
        }
    }
    world
}

/// A displaced population arriving at a node: it flees into the community
/// there (or refounds if none stands), appending to the biography.
fn deliver(world: &mut World, config: &SoundingConfig, node: NodeId, pop: f64, epoch: u32) {
    let mut ev = config.seed.derive("chronicle/deliver").stream();
    let handle = RoleHandle(ev.next_u64() ^ ((node.0 as u64) << 32) ^ epoch as u64);
    if let Some(i) = world.communities.iter().position(|c| c.alive && c.node == node) {
        world.communities[i].population += pop;
        world.communities[i].biography.push(BioEntry { epoch, event: EventKind::Fled, actor: handle });
    } else {
        let species = world.communities.first().map(|c| c.species).unwrap_or(crate::config::SpeciesId(0));
        world.communities.push(crate::world::Community {
            species,
            population: pop,
            node,
            biography: vec![BioEntry { epoch, event: EventKind::Fled, actor: handle }],
            alive: true,
        });
    }
}

/// Found a daughter community on a graph neighbour's node.
fn found_daughter(world: &mut World, _config: &SoundingConfig, parent: usize, epoch: u32, ev: &mut hornvale_kernel::Stream) {
    let handle = RoleHandle(ev.next_u64());
    let node = pick_neighbour(&world.graph, world.communities[parent].node, ev).map(|(t, _)| t).unwrap_or(world.communities[parent].node);
    let species = world.communities[parent].species;
    let seed_pop = world.communities[parent].population * 0.2;
    world.communities[parent].population -= seed_pop;
    world.communities[parent].biography.push(BioEntry { epoch, event: EventKind::Founded, actor: handle });
    world.communities.push(crate::world::Community {
        species, population: seed_pop, node, biography: Vec::new(), alive: true,
    });
}

/// Pick a graph neighbour deterministically; returns (target_node, lag).
fn pick_neighbour(graph: &BTreeMap<NodeId, Vec<crate::world::Edge>>, from: NodeId, ev: &mut hornvale_kernel::Stream) -> Option<(NodeId, u32)> {
    let edges = graph.get(&from)?;
    if edges.is_empty() {
        return None;
    }
    let idx = ev.range_u32(0, edges.len() as u32) as usize;
    Some((edges[idx].to, edges[idx].lag))
}
```

- [ ] **Step 8: Run the tests to verify they pass**

Run: `cargo test -p hornvale-chronicle --test determinism 2>&1 | tail -12`
Expected: PASS — `3 passed`.

- [ ] **Step 9: fmt, clippy (incl. the no-HashMap ban), commit**

```bash
cargo fmt -p hornvale-chronicle
cargo clippy -p hornvale-chronicle --all-targets -- -D warnings
git add windows/chronicle/Cargo.toml windows/chronicle/src windows/chronicle/tests/determinism.rs
git commit -m "feat(chronicle): the Sounding skeleton — synthetic core loop + determinism (T1)"
```

Expected: clippy clean (no `HashMap`, no `Instant` in the loop); commit succeeds.

---

## Task 2: The three measured directions

**Files:**
- Create: `windows/chronicle/src/measure.rs`
- Modify: `windows/chronicle/src/lib.rs` (already exposes `pub mod measure;` from T1)
- Test: `windows/chronicle/tests/determinism.rs` (append structural assertions on read/replay)

**Interfaces:**
- Consumes: `run`, `World`, `Community`, `NodeId` from T1.
- Produces:
  - `BakeMetrics { nanos: u128, peak_bytes: usize, total_bio_bytes: usize, communities: usize, events: usize }`
  - `fn bake(config: &SoundingConfig) -> (World, BakeMetrics)`
  - `fn read_nanos_per_op(world: &World, ops: u32, seed: Seed) -> f64` — hot-loop avg of "sample a biography + expand a role-handle".
  - `fn replay_nanos(world: &World, region_node: NodeId, delta_epochs: u32, config: &SoundingConfig) -> u128` — derive one graph-neighbourhood forward, with a synthetic intervention delta.

- [ ] **Step 1: Write the failing structural test (read/replay are deterministic in RESULT, not timing)**

Append to `windows/chronicle/tests/determinism.rs`:

```rust
#[test]
fn replay_is_deterministic_in_result() {
    // Timings vary; the DERIVED forward state must not. We assert the
    // event-count a replay produces is stable across two calls.
    use hornvale_chronicle::measure::{replay_event_count};
    use hornvale_chronicle::NodeId;
    let world = run(&cfg(7));
    let a = replay_event_count(&world, NodeId(0), 20, &cfg(7));
    let b = replay_event_count(&world, NodeId(0), 20, &cfg(7));
    assert_eq!(a, b, "a replay's derived events must be reproducible");
}
```

- [ ] **Step 2: Run it — expect failure**

Run: `cargo test -p hornvale-chronicle --test determinism replay_is_deterministic 2>&1 | tail -8`
Expected: FAIL to compile (`measure::replay_event_count` undefined).

- [ ] **Step 3: Implement the measure module**

`windows/chronicle/src/measure.rs`:

```rust
//! The three instrumented cost directions. Timing uses `std::time::Instant`
//! under a scoped allow — this is the BENCHMARK HARNESS, not sim logic; the
//! core loop (`simulate.rs`) contains no wall-clock.

use crate::config::{NodeId, RoleHandle, SoundingConfig};
use crate::simulate::run;
use crate::world::{BioEntry, World};
use hornvale_kernel::Seed;

/// Metrics for the genesis-bake direction.
#[derive(Clone, Copy, Debug)]
pub struct BakeMetrics {
    /// Wall-time to derive the whole history, in nanoseconds.
    pub nanos: u128,
    /// A cheap proxy for peak memory: bytes held by the final world.
    pub peak_bytes: usize,
    /// Total bytes of biography (Σ entries × size_of::<BioEntry>).
    pub total_bio_bytes: usize,
    /// Final community count.
    pub communities: usize,
    /// Total events appended.
    pub events: usize,
}

/// Derive the world and its bake metrics.
pub fn bake(config: &SoundingConfig) -> (World, BakeMetrics) {
    #[allow(clippy::disallowed_types)] // benchmark harness: measuring the loop, not sim logic
    let start = std::time::Instant::now();
    let world = run(config);
    #[allow(clippy::disallowed_types)] // benchmark harness
    let nanos = start.elapsed().as_nanos();

    let events: usize = world.communities.iter().map(|c| c.biography.len()).sum();
    let total_bio_bytes = events * std::mem::size_of::<BioEntry>();
    let peak_bytes = world.communities.len() * std::mem::size_of::<crate::world::Community>()
        + total_bio_bytes
        + world.capacity.len() * std::mem::size_of::<f64>();
    (world, BakeMetrics { nanos, peak_bytes, total_bio_bytes, communities: world.communities.len(), events })
}

/// Hot-loop average nanoseconds per read op (sample a biography + expand a
/// role-handle into a synthetic persona hash). Returns ns/op.
pub fn read_nanos_per_op(world: &World, ops: u32, seed: Seed) -> f64 {
    if world.communities.is_empty() || ops == 0 {
        return 0.0;
    }
    let mut s = seed.stream();
    #[allow(clippy::disallowed_types)] // benchmark harness
    let start = std::time::Instant::now();
    let mut sink: u64 = 0;
    for _ in 0..ops {
        let i = s.range_u32(0, world.communities.len() as u32) as usize;
        let bio = &world.communities[i].biography;
        if let Some(e) = bio.last() {
            sink ^= expand_handle(e.actor); // "expand a role-handle to a persona"
            sink ^= e.epoch as u64;
        }
    }
    #[allow(clippy::disallowed_types)] // benchmark harness
    let nanos = start.elapsed().as_nanos();
    std::hint::black_box(sink);
    nanos as f64 / ops as f64
}

/// A synthetic role-handle → persona expansion (a few hashes; stands in for
/// deriving a name/traits/drives from the handle).
fn expand_handle(h: RoleHandle) -> u64 {
    let mut x = h.0.wrapping_mul(0x9E3779B97F4A7C15);
    x ^= x >> 29;
    x = x.wrapping_mul(0xBF58476D1CE4E5B9);
    x ^ (x >> 32)
}

/// Derive one graph-neighbourhood forward `delta_epochs` from the baked
/// world, applying a synthetic intervention delta (kill one neighbour), and
/// return the wall-time. The living-present cost.
pub fn replay_nanos(world: &World, region: NodeId, delta_epochs: u32, config: &SoundingConfig) -> u128 {
    #[allow(clippy::disallowed_types)] // benchmark harness
    let start = std::time::Instant::now();
    let _ = replay_event_count(world, region, delta_epochs, config);
    #[allow(clippy::disallowed_types)] // benchmark harness
    start.elapsed().as_nanos()
}

/// The deterministic core of a replay: how many events does deriving the
/// `region`'s neighbourhood forward `delta_epochs` produce (with one
/// neighbour killed as the intervention). Pure — the assertion surface.
pub fn replay_event_count(world: &World, region: NodeId, delta_epochs: u32, config: &SoundingConfig) -> usize {
    // Gather the region: `region` plus its graph neighbours.
    let mut nodes: Vec<NodeId> = vec![region];
    if let Some(edges) = world.graph.get(&region) {
        for e in edges {
            if !nodes.contains(&e.to) {
                nodes.push(e.to);
            }
        }
    }
    // Local copy of the communities on those nodes; kill the first neighbour
    // (the synthetic player intervention).
    let mut local: Vec<crate::world::Community> = world
        .communities
        .iter()
        .filter(|c| c.alive && nodes.contains(&c.node))
        .cloned()
        .collect();
    if local.len() > 1 {
        local[1].alive = false; // the intervention delta
    }
    // Replay the same pressure rule forward, counting appended events.
    let mut ev = config.seed.derive("chronicle/replay").stream();
    let mut appended = 0usize;
    for epoch in config.epochs..config.epochs.saturating_add(delta_epochs) {
        for c in local.iter_mut().filter(|c| c.alive) {
            let cap = world.capacity[c.node.0 as usize % world.capacity.len().max(1)];
            let need = world.species[c.species.0 as usize].carrying_need;
            let pressure = (c.population * need) / cap.max(1.0);
            let _ = RoleHandle(ev.next_u64());
            if pressure < 0.6 {
                c.population *= 1.05;
                c.biography.push(BioEntry { epoch, event: crate::config::EventKind::Grew, actor: RoleHandle(ev.next_u64()) });
                appended += 1;
            } else if pressure > 1.0 {
                c.alive = false;
                appended += 1;
            }
        }
    }
    appended
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-chronicle 2>&1 | tail -12`
Expected: PASS — `4 passed`.

- [ ] **Step 5: fmt, clippy, commit**

```bash
cargo fmt -p hornvale-chronicle
cargo clippy -p hornvale-chronicle --all-targets -- -D warnings
git add windows/chronicle/src/measure.rs windows/chronicle/tests/determinism.rs
git commit -m "feat(chronicle): the three measured directions — bake/read/replay (T2)"
```

Expected: clippy clean (the only `Instant` uses carry the scoped allow + comment); commit succeeds.

---

## Task 3: The sweep harness, the preregistered hypotheses, and the report

**Files:**
- Create: `windows/chronicle/src/sweep.rs`
- Create: `windows/chronicle/tests/sounding_sweep.rs`
- Modify: `windows/chronicle/src/lib.rs` (already exposes `pub mod sweep;`)

**Interfaces:**
- Consumes: `bake`, `read_nanos_per_op`, `replay_nanos`, `BakeMetrics` from T2.
- Produces:
  - `SweepRow { axis: &'static str, value: u64, bake_nanos: u128, read_ns_per_op: f64, replay_nanos: u128, peak_bytes: usize, events: usize }`
  - `fn sweep_axis(axis: &'static str, base: &SoundingConfig, values: &[u64]) -> Vec<SweepRow>`
  - `fn fit_exponent(rows: &[(f64, f64)]) -> f64` — log-log least-squares slope.
  - `fn render_report(rows: &[SweepRow], sample: &str) -> (String, String)` — returns `(summary_md, rows_csv)`.

- [ ] **Step 1: Write the failing exponent-fit test**

`windows/chronicle/tests/sounding_sweep.rs`:

```rust
//! The heavy-tier sweep. The exponent fit itself is a fast, checkable unit.

use hornvale_chronicle::sweep::fit_exponent;

#[test]
fn exponent_fit_recovers_a_known_power_law() {
    // y = x^2: log-log slope must be ~2.
    let pts: Vec<(f64, f64)> = (1..=10).map(|x| (x as f64, (x * x) as f64)).collect();
    let slope = fit_exponent(&pts);
    assert!((slope - 2.0).abs() < 1e-6, "expected slope ~2, got {slope}");
}
```

- [ ] **Step 2: Run it — expect failure**

Run: `cargo test -p hornvale-chronicle --test sounding_sweep exponent 2>&1 | tail -8`
Expected: FAIL to compile (`sweep::fit_exponent` undefined).

- [ ] **Step 3: Implement the sweep + fit + report**

`windows/chronicle/src/sweep.rs`:

```rust
//! The parameter sweep, the log-log exponent fit, and the report renderer.

use crate::config::{NodeId, SoundingConfig};
use crate::measure::{bake, read_nanos_per_op, replay_nanos};
use crate::world::biography_digest;
use hornvale_kernel::{quantize, Seed};
use std::fmt::Write as _;

/// One measured point of the sweep.
#[derive(Clone, Copy, Debug)]
pub struct SweepRow {
    /// Which axis was varied ("communities" | "species" | "epochs" | "avg_degree" | "long_range").
    pub axis: &'static str,
    /// The axis value at this point.
    pub value: u64,
    /// Genesis-bake wall-time (ns).
    pub bake_nanos: u128,
    /// Read cost (ns/op).
    pub read_ns_per_op: f64,
    /// Present-replay wall-time (ns).
    pub replay_nanos: u128,
    /// Peak-memory proxy (bytes).
    pub peak_bytes: usize,
    /// Events produced.
    pub events: usize,
}

/// Vary one axis, holding the base config fixed, and measure each point.
pub fn sweep_axis(axis: &'static str, base: &SoundingConfig, values: &[u64]) -> Vec<SweepRow> {
    values
        .iter()
        .map(|&v| {
            let mut c = base.clone();
            match axis {
                "communities" => c.communities = v as u32,
                "species" => c.species = v as u32,
                "epochs" => c.epochs = v as u32,
                "avg_degree" => c.avg_degree = v as f64,
                "long_range" => c.long_range_fraction = v as f64 / 100.0,
                _ => {}
            }
            let (world, m) = bake(&c);
            SweepRow {
                axis,
                value: v,
                bake_nanos: m.nanos,
                read_ns_per_op: read_nanos_per_op(&world, 100_000, Seed(c.seed.0 ^ 0xREAD)),
                replay_nanos: replay_nanos(&world, NodeId(0), 20, &c),
                peak_bytes: m.peak_bytes,
                events: m.events,
            }
        })
        .collect()
}

/// Least-squares slope of log(y) vs log(x) — the scaling exponent.
pub fn fit_exponent(rows: &[(f64, f64)]) -> f64 {
    let pts: Vec<(f64, f64)> = rows.iter().filter(|(x, y)| *x > 0.0 && *y > 0.0).map(|(x, y)| (x.ln(), y.ln())).collect();
    let n = pts.len() as f64;
    if n < 2.0 {
        return 0.0;
    }
    let (sx, sy) = pts.iter().fold((0.0, 0.0), |(ax, ay), (x, y)| (ax + x, ay + y));
    let (mx, my) = (sx / n, sy / n);
    let (mut num, mut den) = (0.0, 0.0);
    for (x, y) in &pts {
        num += (x - mx) * (y - my);
        den += (x - mx) * (x - mx);
    }
    if den == 0.0 { 0.0 } else { num / den }
}

/// Render the report: a markdown summary and the raw CSV.
pub fn render_report(rows: &[SweepRow], sample: &str) -> (String, String) {
    let mut csv = String::from("axis,value,bake_ns,read_ns_per_op,replay_ns,peak_bytes,events\n");
    for r in rows {
        let _ = writeln!(csv, "{},{},{},{},{},{},{}", r.axis, r.value, r.bake_nanos, quantize(r.read_ns_per_op), r.replay_nanos, r.peak_bytes, r.events);
    }

    let mut md = String::from("# The Sounding — feasibility frontier\n\n");
    md.push_str("Measured on ONE machine; timings are wall-clock and machine-dependent (the sample biographies below are byte-deterministic). Budgets are lines, not gates.\n\n");
    for axis in ["communities", "species", "epochs", "avg_degree", "long_range"] {
        let pts: Vec<(f64, f64)> = rows.iter().filter(|r| r.axis == axis).map(|r| (r.value as f64, r.bake_nanos as f64)).collect();
        if pts.len() >= 2 {
            let _ = writeln!(md, "- **bake vs {axis}**: scaling exponent ≈ {:.2}", fit_exponent(&pts));
        }
    }
    md.push_str("\n## Sample biographies (deterministic)\n\n```\n");
    md.push_str(sample);
    md.push_str("```\n");
    (md, csv)
}
```

Note: replace `0xREAD` with `0x1EAD` (a valid hex literal) in the implementation — the token `READ` is not hex; it is written illustratively here.

- [ ] **Step 4: Run the fit test to verify it passes**

Run: `cargo test -p hornvale-chronicle --test sounding_sweep exponent 2>&1 | tail -8`
Expected: PASS — `1 passed`.

- [ ] **Step 5: Write the heavy-tier sweep entry that emits the report**

Append to `windows/chronicle/tests/sounding_sweep.rs`:

```rust
#[test]
#[ignore = "heavy: the full parameter sweep (minutes) + report write; deferred from the commit gate to make gate-full"]
fn run_the_sounding_and_write_the_report() {
    use hornvale_chronicle::sweep::{render_report, sweep_axis, SweepRow};
    use hornvale_chronicle::{biography_digest, run, SoundingConfig};
    use hornvale_kernel::Seed;

    let base = SoundingConfig { seed: Seed(1), communities: 1_000, species: 16, epochs: 500, avg_degree: 4.0, long_range_fraction: 0.05 };

    // PREREGISTERED HYPOTHESES (frozen here BEFORE reading the fitted values):
    //   bake vs communities ~ 1.0 (linear)   bake vs epochs ~ 1.0 (linear)
    //   bake vs avg_degree  ~ 1.0 (coupling bounded-local, NOT quadratic)
    //   read ~ 0.0 (flat, O(1))               replay ~ flat in world size
    // The headline finding is the avg_degree exponent: > 1.5 would falsify
    // "bounded-local coupling" and is the architectural red flag.

    let mut rows: Vec<SweepRow> = Vec::new();
    rows.extend(sweep_axis("communities", &base, &[100, 300, 1_000, 3_000, 10_000, 30_000]));
    rows.extend(sweep_axis("species", &base, &[4, 16, 64, 256]));
    rows.extend(sweep_axis("epochs", &base, &[100, 300, 1_000, 3_000]));
    rows.extend(sweep_axis("avg_degree", &base, &[2, 4, 8, 16, 32]));
    rows.extend(sweep_axis("long_range", &base, &[0, 5, 20, 50]));

    let sample = biography_digest(&run(&base));
    let sample_head: String = sample.lines().take(12).collect::<Vec<_>>().join("\n") + "\n";
    let (md, csv) = render_report(&rows, &sample_head);

    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../book/src/laboratory/generated/the-sounding");
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("summary.md"), md).unwrap();
    std::fs::write(dir.join("rows.csv"), csv).unwrap();
    std::fs::write(dir.join("sample-biographies.txt"), sample_head).unwrap();
}
```

- [ ] **Step 6: Verify the heavy test is correctly gated (skipped by default) and the crate gate is green**

Run: `SKIP_CENSUS=1 cargo nextest run -p hornvale-chronicle 2>&1 | tail -6`
Expected: the sweep test shows as skipped (`#[ignore]`); the fast tests pass.

Run: `cargo test -p hornvale -- --exact ignore_reasons_use_heavy_token 2>&1 | tail -4` *(if `cli/tests/heavy_tier.rs` enumerates workspace ignore reasons, confirm the new token is accepted; otherwise skip.)*

- [ ] **Step 7: fmt, clippy, commit**

```bash
cargo fmt -p hornvale-chronicle
cargo clippy -p hornvale-chronicle --all-targets -- -D warnings
git add windows/chronicle/src/sweep.rs windows/chronicle/tests/sounding_sweep.rs
git commit -m "feat(chronicle): the sweep harness, exponent fit, preregistered hypotheses + report (T3)"
```

---

## Task 4: Run the sweep, write the report, and close

**Files:**
- Create: `book/src/laboratory/generated/the-sounding/{summary.md,rows.csv,sample-biographies.txt}` (produced by the harness)
- Create: `book/src/chronicle/the-sounding.md` + wire into `book/src/SUMMARY.md`
- Create: `docs/retrospectives/the-sounding.md`
- Modify: `book/src/frontier/idea-registry.md` (annotate SOC-10/UNI-30/MAP-60 with the measured result; SOC-10 stays `elaborated`, its Where gains "benchmarked by The Sounding")
- Modify: `book/src/laboratory/overview.md` (add a one-line pointer to the report, if the lab overview indexes studies)

**Interfaces:** none (docs + artifacts).

- [ ] **Step 1: Run the full sweep to produce the report**

Run: `cargo test -p hornvale-chronicle --test sounding_sweep run_the_sounding_and_write_the_report -- --ignored --nocapture 2>&1 | tail -8`
Expected: writes the three files under `book/src/laboratory/generated/the-sounding/`. Inspect `summary.md` — confirm each `bake vs <axis>` exponent printed, and that `bake vs avg_degree ≈ 1` (not ≥ 1.5).

- [ ] **Step 2: Read the report and record the finding**

Read `book/src/laboratory/generated/the-sounding/summary.md` and note, in the chronicle you write next, the measured exponents against the preregistered hypotheses — especially whether the coupling (`avg_degree`) stayed sub-quadratic, and where each budget line (read sub-ms, replay tens-ms, genesis generous, memory ~few GB) falls on the curve.

- [ ] **Step 3: Write the chronicle entry**

Create `book/src/chronicle/the-sounding.md` — one page: what The Sounding measured, the preregistered hypotheses, the measured exponents, and the honest scope (macro-history profile only; the fine-cognition and emergent-quality soundings remain). Wire it into `book/src/SUMMARY.md` under the Chronicle appendix (a `- [The Sounding](./chronicle/the-sounding.md)` line). Do **not** cite registry IDs (`SOC-10` etc.) in the chronicle — name the concepts (`docs_consistency` bans IDs outside `book/src/frontier/`).

- [ ] **Step 4: Write the retrospective and annotate the registry**

Create `docs/retrospectives/the-sounding.md` (one page, process lessons). In `book/src/frontier/idea-registry.md`, append to SOC-10's Where cell "; benchmarked by The Sounding (see the chronicle)" and, if the coupling stayed sub-quadratic, note the retired risk in the row.

- [ ] **Step 5: Gate, drift-check, commit**

```bash
SKIP_CENSUS=1 make gate
cargo test -p hornvale --test docs_consistency
mdbook build book
git add book/src docs/retrospectives/the-sounding.md
git commit -m "docs(the-sounding): report + chronicle + retro; annotate the frontier (T4 close)"
```

Expected: `make gate` green; `docs_consistency` green (SUMMARY wired, no ID leak); book builds.

- [ ] **Step 6: Close via `closing-a-campaign`**

Follow the `closing-a-campaign` skill: preflight, absorb `origin/main` (the frontier files are conflict-prone — resolve unions), re-gate, land the **frontier doc + spec + this campaign** to main via the guarded fast-forward, push. No census regen; no epoch.

---

## Self-Review

**Spec coverage:** §3.1 input surface → T1. §3.1 spike structure (communities/graph/loop/events/coupling) → T1. §3.2 three directions → T2. §3.3 sweep + exponents + preregistered hypotheses → T3. §3.4 report (summary + CSV + sample biographies) → T3/T4. §5 acceptance (determinism, preregistration, legible/honest report, constitution-clean) → T1 determinism tests, T3 hypotheses-frozen-first, T4 read-and-record. §6 (not genesis-changing, no epoch/census) → honored throughout; the wall-clock exception is scoped and documented. §7 non-goals → nothing here builds the engine, real dynamics, or the companion soundings. §8 crate-placement resolved (`windows/chronicle`, override-able). All covered.

**Correction surfaced (report is not byte-drift-checked):** the spec called the report "drift-checked in CI." The *timings* are wall-clock and machine-dependent, so the report is a **committed measurement artifact (evidence), regenerated by the opt-in heavy sweep — not a byte-identity golden.** Only the deterministic surface (biographies) is pinned, by the T1 determinism tests. The plan builds it that way; the spec's §3.4/§4 wording should be read accordingly.

**Placeholder scan:** the one intentional illustrative token (`0xREAD`) is called out with its fix (`0x1EAD`) in T3 Step 3. No TBDs.

**Type consistency:** `SoundingConfig`, `World`, `Community`, `BioEntry`, `NodeId`, `RoleHandle`, `EventKind`, `SweepRow`, `BakeMetrics`, `run`, `bake`, `read_nanos_per_op`, `replay_nanos`, `replay_event_count`, `fit_exponent`, `sweep_axis`, `render_report`, `biography_digest` are used consistently across T1–T4.
