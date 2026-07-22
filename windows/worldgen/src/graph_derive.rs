//! Sailing-lane + land-route derivation (The Connection Graph, Task 4):
//! assembles the [`ConnectionGraph`] from real geography -- bare mesh
//! adjacency (Task 1's structure), ocean-current sailing lanes, and bounded
//! least-cost land corridors between settlements (Task 2's `least_cost` over
//! Task 3's `traversal_cost` field). Lives at `windows/worldgen` (the
//! composition root) because it reads terrain, climate, and settlement data
//! together -- no domain crate may depend on a sibling domain.
//!
//! Purely derived, never committed to the ledger: no epoch, no seed draw, no
//! wall-clock read. Same inputs (geosphere, elevation, biome, current,
//! settlement cells, config) always produce a byte-identical graph -- every
//! candidate iteration/selection below is ordered by `CellId` (ascending, no
//! `HashMap`/`HashSet`) and every float choice is tie-broken by
//! `f64::total_cmp`.

use crate::traversal::traversal_cost;
use hornvale_climate::Biome;
use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation};
use hornvale_topology::route::least_cost;
use hornvale_topology::{ConnectionGraph, Edge, EdgeKind};
use std::collections::BTreeSet;

/// Tunable bounds controlling [`connection_graph`]'s derivation: how far (in
/// bare-adjacency hops) a land-route candidate pair may be apart before it's
/// not even attempted, the pathfinding search budget, the cost ceiling a
/// land corridor must beat to become an edge, and how many ocean-cell hops a
/// water-current trace may take before giving up. Coarse-tuned (not
/// census-calibrated), like `traversal::BASE_COST`/`SLOPE_SCALE` -- see
/// [`GraphConfig::default`].
/// type-audit: bare-ok(count: land_route_radius), bare-ok(count: astar_budget), bare-ok(count: corridor_max_cost), bare-ok(count: water_route_max_steps)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GraphConfig {
    /// Settlement pairs farther than this many hops apart over the bare
    /// mesh adjacency (`Geosphere::hops_between`, cost-blind) are never even
    /// attempted with `least_cost` -- the bound that keeps this derivation
    /// from searching all O(N^2) settlement pairs.
    pub land_route_radius: u32,
    /// The `astar` node-expansion budget passed to `least_cost` for each
    /// attempted settlement pair.
    pub astar_budget: usize,
    /// A land corridor whose total traversal cost is not strictly below this
    /// ceiling is discarded -- the pass/peak distinction: an ordinary pass
    /// stays under it, a real peak's slope surcharge blows through it.
    pub corridor_max_cost: u64,
    /// The maximum number of ocean-cell hops a current-following water-route
    /// trace may take before giving up on reaching another coastal cell.
    pub water_route_max_steps: u32,
}

impl Default for GraphConfig {
    /// A generous 12-hop settlement search radius, an `astar` budget (2000
    /// node expansions) well above what a search that shallow could need, a
    /// corridor ceiling of 600 (60 flat-terrain hops at
    /// `traversal::BASE_COST` -- generous for an ordinary pass, easily blown
    /// by a real peak's slope surcharge, which dwarfs `BASE_COST` per the
    /// same reasoning `traversal::SLOPE_SCALE`'s doc comment gives), and a
    /// 20-step current trace. Deferred until a consumer needs census-tuned
    /// values, exactly as Task 3 deferred `BASE_COST`/`SLOPE_SCALE`.
    fn default() -> Self {
        GraphConfig {
            land_route_radius: 12,
            astar_budget: 2000,
            corridor_max_cost: 600,
            water_route_max_steps: 20,
        }
    }
}

/// Derive the world's [`ConnectionGraph`] from real geography: bare mesh
/// adjacency, ocean-current sailing lanes, and bounded least-cost land
/// corridors between settlements. Purely derived -- no seed draw, no
/// wall-clock read, never committed to the ledger (no epoch). Internally
/// rebuilds the traversal-cost field (`crate::traversal::traversal_cost`)
/// from `elevation`/`biome` rather than taking a pre-built one, matching
/// this task's brief signature.
/// type-audit: bare-ok(diagnostic-value: current)
pub fn connection_graph(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    biome: &CellMap<Biome>,
    current: &CellMap<[f64; 3]>,
    settlements: &[CellId],
    cfg: &GraphConfig,
) -> ConnectionGraph {
    let cost = traversal_cost(geo, elevation, biome);
    let mut graph = ConnectionGraph::new(geo.cell_count());

    add_adjacency_edges(geo, &cost, &mut graph);
    add_water_routes(geo, biome, current, cfg, &mut graph);
    add_land_routes(geo, &cost, settlements, cfg, &mut graph);

    graph
}

/// Bare mesh adjacency: one `Adjacency` edge per unordered neighbor pair.
/// `geo.neighbors` is symmetric, so iterating every cell's every neighbor
/// would otherwise add each pair twice -- only the lower-`CellId` side of a
/// pair adds it, canonically. Conductance is the reciprocal of the pair's
/// average traversal cost, or zero if either endpoint is impassable
/// (`u64::MAX`, e.g. ocean).
fn add_adjacency_edges(geo: &Geosphere, cost: &CellMap<u64>, graph: &mut ConnectionGraph) {
    for cell in geo.cells() {
        for &neighbor in geo.neighbors(cell) {
            if neighbor.0 <= cell.0 {
                continue;
            }
            let conductance = cost_conductance(*cost.get(cell), *cost.get(neighbor));
            graph.add_edge(
                cell,
                Edge {
                    to: neighbor,
                    kind: EdgeKind::Adjacency,
                    conductance,
                },
            );
        }
    }
}

/// Conductance from a pair of per-cell traversal costs: the reciprocal of
/// their average, or zero if either endpoint is impassable (`u64::MAX`) --
/// never a near-zero float from averaging in a saturated cost, which would
/// be numerically fine but far less legible.
fn cost_conductance(a: u64, b: u64) -> f64 {
    if a == u64::MAX || b == u64::MAX {
        return 0.0;
    }
    1.0 / ((a as f64 + b as f64) / 2.0)
}

/// A coastal cell's launch point onto the water: its own lowest-`CellId`
/// marine neighbor (`geo.neighbors` is already ascending, so this is a
/// stable, deterministic pick when a coastal cell borders more than one
/// ocean cell). `None` if `cell` is not coastal (no marine neighbor at all).
fn first_marine_neighbor(geo: &Geosphere, biome: &CellMap<Biome>, cell: CellId) -> Option<CellId> {
    geo.neighbors(cell)
        .iter()
        .copied()
        .find(|&n| biome.get(n).is_marine())
}

/// The neighbor of `cell` whose direction best aligns with `vector` (the
/// max-dot-product pick) -- the "downstream" step a current-following trace
/// advances to. Mirrors `hornvale_climate`'s (crate-private) upwind-neighbor
/// pattern, which picks the neighbor most OPPOSED to a wind vector; this
/// picks the one most ALIGNED with a current vector instead. Ties are
/// broken by `f64::total_cmp` via `Iterator::max_by`, which returns the
/// LAST maximum on a tie -- since `geo.neighbors` is ascending, a tie
/// resolves toward the higher `CellId`, deterministically (e.g. a dead
/// current pocket, where every neighbor scores the same zero alignment).
fn downstream_neighbor(geo: &Geosphere, cell: CellId, vector: [f64; 3]) -> Option<CellId> {
    let here = geo.position(cell);
    geo.neighbors(cell).iter().copied().max_by(|&a, &b| {
        let pa = geo.position(a);
        let pb = geo.position(b);
        let sa = (pa[0] - here[0]) * vector[0]
            + (pa[1] - here[1]) * vector[1]
            + (pa[2] - here[2]) * vector[2];
        let sb = (pb[0] - here[0]) * vector[0]
            + (pb[1] - here[1]) * vector[1]
            + (pb[2] - here[2]) * vector[2];
        sa.total_cmp(&sb)
    })
}

/// Follow `current` downstream from `start` (an ocean cell) across marine
/// cells, at most `max_steps` hops, to the first non-marine cell reached. A
/// `visited` cycle guard stops a stalled trace (e.g. a dead current pocket,
/// where every neighbor ties at zero alignment) from looping within the
/// step budget instead of terminating. `None` if no coastal cell is reached
/// in time.
fn follow_current(
    geo: &Geosphere,
    biome: &CellMap<Biome>,
    current: &CellMap<[f64; 3]>,
    start: CellId,
    max_steps: u32,
) -> Option<CellId> {
    let mut visited: BTreeSet<CellId> = BTreeSet::new();
    visited.insert(start);
    let mut cell = start;
    for _ in 0..max_steps {
        let vector = *current.get(cell);
        let next = downstream_neighbor(geo, cell, vector)?;
        if !biome.get(next).is_marine() {
            return Some(next);
        }
        if !visited.insert(next) {
            return None;
        }
        cell = next;
    }
    None
}

/// The magnitude of a 3-vector -- `current_at`'s tangent-vector strength,
/// used directly as a `WaterRoute` edge's conductance.
fn vector_magnitude(v: [f64; 3]) -> f64 {
    (v[0] * v[0] + v[1] * v[1] + v[2] * v[2]).sqrt()
}

/// Sailing lanes: for every coastal cell (a non-marine cell with at least
/// one marine neighbor), launch a current-following trace
/// ([`follow_current`]) from its lowest-`CellId` marine neighbor
/// ([`first_marine_neighbor`]); if it reaches another, distinct coastal cell
/// within `cfg.water_route_max_steps`, add a `WaterRoute` edge, conductance
/// the current's strength at the launch cell.
fn add_water_routes(
    geo: &Geosphere,
    biome: &CellMap<Biome>,
    current: &CellMap<[f64; 3]>,
    cfg: &GraphConfig,
    graph: &mut ConnectionGraph,
) {
    for cell in geo.cells() {
        if biome.get(cell).is_marine() {
            continue;
        }
        let Some(launch) = first_marine_neighbor(geo, biome, cell) else {
            continue;
        };
        let Some(destination) =
            follow_current(geo, biome, current, launch, cfg.water_route_max_steps)
        else {
            continue;
        };
        if destination == cell {
            continue;
        }
        let conductance = vector_magnitude(*current.get(launch));
        graph.add_edge(
            cell,
            Edge {
                to: destination,
                kind: EdgeKind::WaterRoute,
                conductance,
            },
        );
    }
}

/// Bounded natural land corridors between settlements: every pair within
/// `cfg.land_route_radius` hops (`Geosphere::hops_between`, checked BEFORE
/// the costlier `least_cost` search -- the bound that keeps this derivation
/// from searching all O(N^2) settlement pairs) is searched with
/// `hornvale_topology::route::least_cost` over the traversal-cost field,
/// budget `cfg.astar_budget`; a path costing strictly less than
/// `cfg.corridor_max_cost` becomes a `LandRoute` edge, conductance the
/// reciprocal of its total cost. Settlements are sorted and deduplicated
/// first so pair iteration is deterministic regardless of the input
/// slice's order (settlement facts commit in ledger order, not `CellId`
/// order).
fn add_land_routes(
    geo: &Geosphere,
    cost: &CellMap<u64>,
    settlements: &[CellId],
    cfg: &GraphConfig,
    graph: &mut ConnectionGraph,
) {
    let mut sorted: Vec<CellId> = settlements.to_vec();
    sorted.sort();
    sorted.dedup();

    for (i, &a) in sorted.iter().enumerate() {
        for &b in &sorted[i + 1..] {
            if geo.hops_between(a, b, cfg.land_route_radius).is_none() {
                continue;
            }
            let Some((_, total)) = least_cost(geo, cost, a, b, cfg.astar_budget) else {
                continue;
            };
            if total >= cfg.corridor_max_cost {
                continue;
            }
            let conductance = 1.0 / (total.max(1) as f64);
            graph.add_edge(
                a,
                Edge {
                    to: b,
                    kind: EdgeKind::LandRoute,
                    conductance,
                },
            );
        }
    }
}
