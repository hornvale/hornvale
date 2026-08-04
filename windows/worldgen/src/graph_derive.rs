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
use hornvale_climate::snowpack::DEFAULT_SNOWPACK;
use hornvale_climate::substrate::SubstrateField;
use hornvale_climate::wetness::{DEFAULT_WETNESS, receptivity};
use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation, Value, World};
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
/// type-audit: bare-ok(count: land_route_radius), bare-ok(count: astar_budget), bare-ok(count: corridor_max_cost), bare-ok(count: water_route_max_steps), bare-ok(diagnostic-value: day)
#[derive(Clone, Copy, Debug, PartialEq)]
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
    /// The day to gate conductance on, if any. `None` -- the default and
    /// what every pre-Mire caller gets -- derives the unweathered graph,
    /// byte-identically to before this campaign.
    pub day: Option<f64>,
}

impl Default for GraphConfig {
    /// A generous 12-hop settlement search radius, an `astar` budget (2000
    /// node expansions) well above what a search that shallow could need, a
    /// corridor ceiling of 600 (60 flat-terrain hops at
    /// `traversal::BASE_COST` -- generous for an ordinary pass, easily blown
    /// by a real peak's slope surcharge, which dwarfs `BASE_COST` per the
    /// same reasoning `traversal::SLOPE_SCALE`'s doc comment gives), and a
    /// 20-step current trace. Deferred until a consumer needs census-tuned
    /// values, exactly as Task 3 deferred `BASE_COST`/`SLOPE_SCALE`. `day`
    /// defaults to `None`: no weather gating unless a caller opts in.
    fn default() -> Self {
        GraphConfig {
            land_route_radius: 12,
            astar_budget: 2000,
            corridor_max_cost: 600,
            water_route_max_steps: 20,
            day: None,
        }
    }
}

/// How much weather's memory in the ground impedes travel, `[0,1]` -- a
/// multiplier on an edge's conductance.
///
/// Mud and lying snow each impede. A **frozen** mire recovers most of what
/// its wetness cost, because hard ground travels well; that asymmetry is
/// deliberate and is what distinguishes this from a monotone penalty
/// wearing a physics costume.
/// type-audit: bare-ok(ratio: wetness), bare-ok(diagnostic-value: snow_mm), bare-ok(flag: frozen), bare-ok(ratio: return)
pub fn weather_conductance_factor(wetness: f64, snow_mm: f64, frozen: bool) -> f64 {
    let w = wetness.clamp(0.0, 1.0);
    let mud = if frozen {
        w * MUD_PENALTY * FROZEN_RELIEF
    } else {
        w * MUD_PENALTY
    };
    let snow = (snow_mm.max(0.0) / SNOW_IMPEDING_MM).clamp(0.0, 1.0) * SNOW_PENALTY;
    (1.0 - mud - snow).clamp(0.0, 1.0)
}

/// Saturated unfrozen ground costs this share of an edge's conductance.
const MUD_PENALTY: f64 = 0.6;
/// Freezing returns most of the mud penalty -- hard ground travels well.
const FROZEN_RELIEF: f64 = 0.15;
/// Lying snow at or above this depth (mm water equivalent) costs the full
/// snow penalty.
const SNOW_IMPEDING_MM: f64 = 300.0;
/// A fully snowed-in edge costs this share of its conductance.
const SNOW_PENALTY: f64 = 0.7;

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

    let marine = CellMap::from_fn(geo, |c| biome.get(c).is_marine());
    add_adjacency_edges(geo, &cost, &mut graph);
    add_water_routes(geo, &marine, current, cfg, &mut graph);
    add_land_routes(geo, &cost, settlements, cfg, &mut graph);

    graph
}

/// The era-aware connection graph: like [`connection_graph`] but a cell is
/// ocean iff `elevation < sea_level`, so a glacial low-stand exposes the shelf
/// as passable land (the land bridges The Sundering's diaspora crosses).
/// Adjacency + sailing lanes, plus era-aware land routes if `settlements` is
/// non-empty (their corridors are costed at this era's `sea_level`, not the
/// present). The Sundering's bake calls this with no settlements, so it derives
/// adjacency + sailing lanes only. Derived, never committed.
/// type-audit: bare-ok(diagnostic-value: current)
pub fn connection_graph_at(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    current: &CellMap<[f64; 3]>,
    settlements: &[CellId],
    cfg: &GraphConfig,
) -> ConnectionGraph {
    let marine = CellMap::from_fn(geo, |c| elevation.get(c).get() < sea_level.get());
    let cost = crate::traversal::traversal_cost_at(geo, elevation, sea_level);
    let mut graph = ConnectionGraph::new(geo.cell_count());
    add_adjacency_edges(geo, &cost, &mut graph);
    add_water_routes(geo, &marine, current, cfg, &mut graph);
    add_land_routes(geo, &cost, settlements, cfg, &mut graph);
    graph
}

/// Derive a world's [`ConnectionGraph`] directly from a built [`World`] --
/// the World-to-inputs adapter over [`connection_graph`] (Task 5's real-world
/// entry point; Task 6's legibility surface and Task 7's DoD check reuse it
/// too). Reconstructs terrain and climate (`crate::terrain_of` /
/// `crate::climate_from`), reads the current field pointwise
/// (`GeneratedClimate::current_at`, no `current_map()` accessor exists) into
/// a `CellMap`, and reads each settlement's `cell-id` fact
/// (`hornvale_settlement::CELL_ID`) into the `Vec<CellId>` `connection_graph`
/// wants -- then calls `connection_graph`. Derivation logic stays there;
/// this function is only the adapter, so it never duplicates
/// `connection_graph`'s edge-assembly.
///
/// If `cfg.day` is `Some(day)` (The Mire, Task 6), the assembled graph is
/// then weather-gated: two `SubstrateField`s (surface wetness, snowpack) are
/// computed once -- never per edge -- and every `Adjacency`/`LandRoute`
/// edge's conductance is scaled by the mean of its two endpoints'
/// [`weather_conductance_factor`]. `WaterRoute` edges are left untouched
/// (sea ice is a future consumer's business). `cfg.day == None` is
/// byte-identical to the pre-Mire behaviour -- the scaling pass never runs.
///
/// # Panics
///
/// `world` must have been built through at least `BuildDepth::Settlements`
/// (true of any world `build_world`/`build_world_to` returned at that depth
/// or deeper): panics if terrain or climate fails to reconstruct, or if any
/// committed settlement lacks its `cell-id` fact.
// Named construction site (decision 0092): sculpts/fits once for its own
// connection-graph readout.
#[allow(clippy::disallowed_methods)]
pub fn connection_graph_of(world: &World, cfg: &GraphConfig) -> ConnectionGraph {
    let terrain = crate::terrain_of(world)
        .expect("world was built with terrain (BuildDepth::Terrain or deeper)");
    let climate = crate::climate_from(world, &terrain)
        .expect("world was built with climate (BuildDepth::Terrain or deeper)");
    let geo = terrain.geosphere();
    let elevation = &terrain.globe().elevation;
    let biome = climate.biome_map();
    let current = CellMap::from_fn(geo, |c| climate.current_at(c));

    let settlements: Vec<CellId> = hornvale_settlement::all_settlements(world)
        .iter()
        .map(
            |s| match world.ledger.value_of(s.id, hornvale_settlement::CELL_ID) {
                Some(Value::Number(n)) => CellId(*n as u32),
                _ => panic!("settlement {} has no cell-id fact", s.id.0),
            },
        )
        .collect();

    let mut graph = connection_graph(geo, elevation, &biome, &current, &settlements, cfg);

    if let Some(day) = cfg.day {
        // Computed ONCE per call, never per edge -- each spins up every
        // cell's periodic year (Task 5 measured ~90-105ms per field at
        // 2562 cells), so re-deriving inside the per-edge closure below
        // would be the same mistake in miniature. `compute_pair` additionally
        // shares each cell's `year_of_day_contexts` build across both
        // substrates rather than rebuilding it once per field (the-mire-perf
        // follow-up, change 2) -- arithmetically identical to two separate
        // `SubstrateField::compute` calls, just without the duplicate year.
        let (wetness_field, snow_field) =
            SubstrateField::compute_pair(&climate, &DEFAULT_WETNESS, &DEFAULT_SNOWPACK);
        let factor_at = |cell: CellId| -> f64 {
            let wetness_mm = wetness_field.at(cell, day);
            let snow_mm = snow_field.at(cell, day);
            let frozen = climate.is_frozen_at(cell, day);
            weather_conductance_factor(
                receptivity(wetness_mm, DEFAULT_WETNESS.field_capacity_mm),
                snow_mm,
                frozen,
            )
        };
        graph.scale_conductance(|from, edge| match edge.kind {
            EdgeKind::WaterRoute => 1.0,
            EdgeKind::Adjacency | EdgeKind::LandRoute => {
                (factor_at(from) + factor_at(edge.to)) / 2.0
            }
        });
    }

    graph
}

/// The number of settlement pairs [`connection_graph`]'s land-route
/// derivation actually attempts with `least_cost` -- every pair whose bare-
/// adjacency hop distance (`Geosphere::hops_between`) is within
/// `cfg.land_route_radius`. Mirrors `add_land_routes`'s own sort/dedup/bound
/// check exactly (same settlement dedup, same hop-radius test), so this is
/// the precise attempt count, not an estimate -- exposed so the cost gate
/// (Task 5) can measure and bound the size-risk without re-instrumenting
/// `connection_graph` itself.
/// type-audit: bare-ok(count: return)
pub fn land_route_attempt_count(
    geo: &Geosphere,
    settlements: &[CellId],
    cfg: &GraphConfig,
) -> usize {
    let mut sorted: Vec<CellId> = settlements.to_vec();
    sorted.sort();
    sorted.dedup();

    let mut attempts = 0usize;
    for (i, &a) in sorted.iter().enumerate() {
        for &b in &sorted[i + 1..] {
            if geo.hops_between(a, b, cfg.land_route_radius).is_some() {
                attempts += 1;
            }
        }
    }
    attempts
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
fn first_marine_neighbor(geo: &Geosphere, marine: &CellMap<bool>, cell: CellId) -> Option<CellId> {
    geo.neighbors(cell)
        .iter()
        .copied()
        .find(|&n| *marine.get(n))
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
    marine: &CellMap<bool>,
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
        if !*marine.get(next) {
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
    marine: &CellMap<bool>,
    current: &CellMap<[f64; 3]>,
    cfg: &GraphConfig,
    graph: &mut ConnectionGraph,
) {
    for cell in geo.cells() {
        if *marine.get(cell) {
            continue;
        }
        let Some(launch) = first_marine_neighbor(geo, marine, cell) else {
            continue;
        };
        let Some(destination) =
            follow_current(geo, marine, current, launch, cfg.water_route_max_steps)
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
        // The bare-adjacency reachable set from `a`, out to
        // `cfg.land_route_radius` hops, computed ONCE for `a` rather than
        // re-walked per candidate `b` (see `cells_within_hops`'s doc comment
        // -- this is the same predicate `Geosphere::hops_between(a, b,
        // radius).is_some()` would answer per pair, batched).
        let reachable = cells_within_hops(geo, a, cfg.land_route_radius);
        for &b in &sorted[i + 1..] {
            if !reachable.contains(&b) {
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

/// Every cell reachable from `from` within `radius` hops of `geo`'s bare
/// mesh adjacency -- the batched form of [`Geosphere::hops_between`]'s
/// per-target bounded BFS. `hops_between(from, b, radius).is_some()` iff `b`
/// is in this set: bare-adjacency reachability within a hop bound is a pure
/// function of `(from, radius)` alone, not of which target is being asked
/// about, and the frontier expansion below is the identical BFS
/// `hops_between` runs (same `visited`/`frontier` bookkeeping, same
/// depth bound, same early-break on an exhausted frontier) -- it just
/// answers the predicate for every candidate at once instead of re-walking
/// the same bounded neighborhood from scratch per candidate. `add_land_routes`
/// calls this once per `a`, replacing what was previously one
/// `hops_between` call per `(a, b)` pair -- a genuine reduction in repeated
/// work, not a change to which pairs pass the filter.
fn cells_within_hops(geo: &Geosphere, from: CellId, radius: u32) -> BTreeSet<CellId> {
    let mut visited: BTreeSet<CellId> = BTreeSet::new();
    visited.insert(from);
    let mut frontier: Vec<CellId> = vec![from];
    for _ in 1..=radius {
        let mut next: Vec<CellId> = Vec::new();
        for &c in &frontier {
            for &n in geo.neighbors(c) {
                if visited.insert(n) {
                    next.push(n);
                }
            }
        }
        if next.is_empty() {
            break;
        }
        frontier = next;
    }
    visited
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Seed 42, built to `BuildDepth::Settlements` -- deep enough for
    /// `connection_graph_of` to reconstruct terrain, climate, and read every
    /// committed settlement's `cell-id` fact. Mirrors
    /// `windows/worldgen/tests/graph_byte_identity.rs`'s `build_settlements`
    /// exactly (that file is a separate compilation unit -- an integration
    /// test -- so its helper is not reachable from here).
    fn sample_world() -> World {
        let wc = crate::WorldComponents::assemble().expect("canonical registries are well-formed");
        crate::build_world_to(
            hornvale_kernel::Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            crate::SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &crate::SettlementPins::default(),
            &wc,
            crate::BuildDepth::Settlements,
        )
        .expect("seed 42 builds to Settlements")
    }

    #[test]
    fn dry_ground_is_unmodified() {
        assert_eq!(weather_conductance_factor(0.0, 0.0, false), 1.0);
    }

    #[test]
    fn mud_and_snow_each_impede_travel() {
        let dry = weather_conductance_factor(0.0, 0.0, false);
        let muddy = weather_conductance_factor(1.0, 0.0, false);
        let snowy = weather_conductance_factor(0.0, 400.0, false);
        assert!(muddy < dry, "mud did not impede travel");
        assert!(snowy < dry, "snow did not impede travel");
    }

    #[test]
    fn a_frozen_mire_travels_better_than_a_wet_one() {
        // THE asymmetry. If the modifier were a monotone penalty dressed up
        // as physics, freezing saturated ground would change nothing. Hard
        // ground travels well, and that is the specific guard against a
        // debuff wearing a physics costume.
        let wet = weather_conductance_factor(1.0, 0.0, false);
        let frozen = weather_conductance_factor(1.0, 0.0, true);
        assert!(
            frozen > wet,
            "a frozen mire ({frozen}) did not out-travel a wet one ({wet})"
        );
    }

    #[test]
    fn the_factor_never_leaves_the_unit_interval() {
        for wetness in [0.0, 0.5, 1.0] {
            for snow in [0.0, 100.0, 5000.0] {
                for frozen in [false, true] {
                    let f = weather_conductance_factor(wetness, snow, frozen);
                    assert!((0.0..=1.0).contains(&f), "factor {f} out of range");
                }
            }
        }
    }

    #[test]
    // Named construction site (decision 0092): this test's own reference
    // graph re-sculpts/re-fits, mirroring `connection_graph_of`'s own
    // allow, to build an independent comparison graph.
    #[allow(clippy::disallowed_methods)]
    fn a_day_less_config_leaves_the_graph_byte_identical() {
        // The compatibility guard: every existing caller passes no day, and
        // must get exactly the graph it got before this campaign -- i.e.
        // exactly what the un-gated `connection_graph` itself produces from
        // the same inputs, not merely two `day: None` `connection_graph_of`
        // calls agreeing with EACH OTHER. That weaker comparison cannot
        // fail even if the gating pass ran unconditionally regardless of
        // `cfg.day` (both `day: None` variants would drift identically) --
        // caught by mutation-testing this test itself, so the reference
        // graph here is built independently, by reconstructing terrain and
        // climate exactly as `connection_graph_of` does and calling the
        // low-level `connection_graph` directly.
        let world = sample_world();
        let cfg = GraphConfig::default();

        let terrain = crate::terrain_of(&world).expect("world built with terrain");
        let climate = crate::climate_from(&world, &terrain).expect("world built with climate");
        let geo = terrain.geosphere();
        let elevation = &terrain.globe().elevation;
        let biome = climate.biome_map();
        let current = CellMap::from_fn(geo, |c| climate.current_at(c));
        let settlements: Vec<CellId> = hornvale_settlement::all_settlements(&world)
            .iter()
            .map(
                |s| match world.ledger.value_of(s.id, hornvale_settlement::CELL_ID) {
                    Some(Value::Number(n)) => CellId(*n as u32),
                    _ => panic!("settlement {} has no cell-id fact", s.id.0),
                },
            )
            .collect();
        let reference = connection_graph(geo, elevation, &biome, &current, &settlements, &cfg);

        let adapted = connection_graph_of(&world, &cfg);
        for cell in reference.nodes() {
            assert_eq!(
                reference.edges(cell),
                adapted.edges(cell),
                "cell {cell:?} drifted"
            );
        }
    }

    #[test]
    fn gating_on_a_day_changes_some_edge_conductance() {
        // The reader actually reads. A gated graph that equals the ungated
        // one would mean the substrate is still latent.
        let world = sample_world();
        let plain = connection_graph_of(&world, &GraphConfig::default());
        let winter = connection_graph_of(
            &world,
            &GraphConfig {
                day: Some(0.0),
                ..GraphConfig::default()
            },
        );
        let changed = plain.nodes().any(|c| plain.edges(c) != winter.edges(c));
        assert!(changed, "weather gating altered no edge in the whole world");
    }
}
