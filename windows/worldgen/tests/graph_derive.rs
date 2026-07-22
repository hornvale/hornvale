//! Integration test for `connection_graph` (The Connection Graph, Task 4):
//! assembles water routes and bounded land routes from real geography over a
//! small fixture world built on a real `Geosphere::new(1)` (42 cells, its
//! fields are private so there is no hand-built toy-grid constructor -- see
//! `domains/topology/tests/route.rs` and `windows/worldgen/tests/
//! traversal.rs` for the same pattern).
//!
//! The fixture (all cell ids picked by dumping the level-1 mesh's real
//! adjacency and verifying costs with `least_cost` directly, exactly as
//! Task 2's and Task 3's own fixtures were built):
//!
//! - A **peninsula**: settlement `s3` (`CellId(6)`) sits on a spit of land
//!   whose only non-ocean neighbor is `CellId(28)`, the sole isthmus back to
//!   the mainland (`s3`'s other four neighbors, `29`/`37`/`38`/`39`, are all
//!   ocean -- a hard bottleneck, not a probabilistic one: `least_cost`'s
//!   search space skips an ocean cell entirely as a successor, so there is
//!   no way off the peninsula except through `28`). `28` is elevated to
//!   5000m above its neighbors -- "the peak" -- so the one path off the
//!   peninsula costs far more than any reasonable corridor ceiling.
//! - A **low pass**: `s1` (`CellId(9)`) and `s2` (`CellId(41)`) are direct
//!   mesh neighbors over flat terrain -- a trivial, cheap corridor.
//! - A **bay**: a separate "polar sea" (`CellId(25)`, disconnected from the
//!   peninsula's ocean cells) borders both `coast_west` (`CellId(4)`) and
//!   `coast_east` (`CellId(5)`). The current at the sea cell is built (from
//!   real `geo.position` values, not a hand-picked vector) to point toward
//!   `coast_east`, so a trace launched from `coast_west`'s marine neighbor
//!   reaches `coast_east` in one step.

use hornvale_climate::Biome;
use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation};
use hornvale_topology::EdgeKind;
use hornvale_worldgen::graph_derive::{GraphConfig, connection_graph, land_route_attempt_count};

/// Test-only helper: a validated `ReferenceElevation`.
fn e(m: f64) -> ReferenceElevation {
    ReferenceElevation::new(m).unwrap()
}

/// The mainland settlement on one side of the low pass.
fn s1() -> CellId {
    CellId(9)
}

/// The mainland settlement on the other side of the low pass (`s1`'s direct
/// mesh neighbor).
fn s2() -> CellId {
    CellId(41)
}

/// The peninsula settlement, cut off from the mainland except through the
/// elevated isthmus `CellId(28)`.
fn s3() -> CellId {
    CellId(6)
}

fn coast_west() -> CellId {
    CellId(4)
}

fn coast_east() -> CellId {
    CellId(5)
}

/// The fixture world's fields, bundled to keep `fixture`'s signature legible:
/// the mesh, elevation, biome, ocean-current, and settlement-cell inputs
/// `connection_graph` takes.
type Fixture = (
    Geosphere,
    CellMap<ReferenceElevation>,
    CellMap<Biome>,
    CellMap<[f64; 3]>,
    Vec<CellId>,
);

fn fixture() -> Fixture {
    let geo = Geosphere::new(1); // 42 cells

    // The peninsula-isolating sea (surrounds `s3` except for the isthmus)
    // plus the separate polar sea (the bay's water route). Verified
    // disjoint: neither cluster is adjacent to the other.
    let ocean: Vec<CellId> = [25, 29, 37, 38, 39].into_iter().map(CellId).collect();
    let peak = CellId(28); // the sole isthmus off the peninsula

    let elevation = CellMap::from_fn(&geo, |c| {
        if c == peak {
            e(3000.0)
        } else if ocean.contains(&c) {
            e(-100.0)
        } else {
            e(50.0)
        }
    });
    let biome = CellMap::from_fn(&geo, |c| {
        if ocean.contains(&c) {
            Biome::Epipelagic
        } else {
            Biome::TemperateGrassland
        }
    });

    // The sea's current points toward `coast_east`, computed from real
    // positions (not a hand-picked vector) so it stays valid if the mesh's
    // exact layout ever shifts.
    let sea = CellId(25);
    let p_sea = geo.position(sea);
    let p_east = geo.position(coast_east());
    let current_vec = [
        p_east[0] - p_sea[0],
        p_east[1] - p_sea[1],
        p_east[2] - p_sea[2],
    ];
    let current = CellMap::from_fn(&geo, |c| {
        if c == sea {
            current_vec
        } else {
            [0.0, 0.0, 0.0]
        }
    });

    // Guard the fixture's own load-bearing assumptions.
    assert!(
        geo.neighbors(s1()).contains(&s2()),
        "s1/s2 must be direct neighbors (the low pass)"
    );
    assert!(
        geo.neighbors(s3())
            .iter()
            .all(|n| *n == peak || ocean.contains(n)),
        "s3's every neighbor must be the isthmus or ocean (the hard bottleneck)"
    );
    assert!(
        geo.neighbors(sea).contains(&coast_west()) && geo.neighbors(sea).contains(&coast_east()),
        "the polar sea must border both coasts"
    );

    (geo, elevation, biome, current, vec![s1(), s2(), s3()])
}

/// A generous, small-world-scale config: `land_route_radius`/`astar_budget`
/// wide enough that every settlement pair here is actually attempted (the
/// mesh's diameter is a handful of hops), and a `corridor_max_cost` that
/// sits strictly between the low pass's real cost (10 -- one flat-terrain
/// hop) and the peak crossing's real cost (over 9000 -- verified directly
/// against `least_cost` while designing this fixture).
fn cfg() -> GraphConfig {
    GraphConfig {
        land_route_radius: 10,
        astar_budget: 2000,
        corridor_max_cost: 500,
        water_route_max_steps: 10,
    }
}

#[test]
fn a_water_route_links_the_two_coasts() {
    let (geo, elevation, biome, current, settlements) = fixture();
    let graph = connection_graph(&geo, &elevation, &biome, &current, &settlements, &cfg());
    let kinds: Vec<EdgeKind> = graph
        .edges(coast_west())
        .iter()
        .filter(|edge| edge.to == coast_east())
        .map(|edge| edge.kind)
        .collect();
    assert!(
        kinds.contains(&EdgeKind::WaterRoute),
        "expected a WaterRoute edge from the west coast to the east coast, got {kinds:?}"
    );
}

#[test]
fn a_land_route_links_the_pass_connected_settlements() {
    let (geo, elevation, biome, current, settlements) = fixture();
    let graph = connection_graph(&geo, &elevation, &biome, &current, &settlements, &cfg());
    let kinds: Vec<EdgeKind> = graph
        .edges(s1())
        .iter()
        .filter(|edge| edge.to == s2())
        .map(|edge| edge.kind)
        .collect();
    assert!(
        kinds.contains(&EdgeKind::LandRoute),
        "expected a LandRoute edge across the low pass, got {kinds:?}"
    );
}

#[test]
fn no_land_route_crosses_the_peak() {
    let (geo, elevation, biome, current, settlements) = fixture();
    let graph = connection_graph(&geo, &elevation, &biome, &current, &settlements, &cfg());
    for (from, to) in [(s1(), s3()), (s2(), s3())] {
        let has_land_route = graph
            .edges(from)
            .iter()
            .any(|edge| edge.to == to && edge.kind == EdgeKind::LandRoute);
        assert!(
            !has_land_route,
            "expected no LandRoute edge between {from:?} and {to:?} across the peak"
        );
    }
}

/// The light in-gate half of the cost gate (The Connection Graph, Task 5):
/// on this fixture's small pinned world (`Geosphere::new(1)`, 42 cells, 3
/// settlements), the land-route attempt count -- settlement pairs within
/// `cfg().land_route_radius` hops, exactly what `add_land_routes` would run
/// `least_cost` on -- must stay a small constant. This runs in `make gate`
/// (no live worldgen build); the heavy battery in `cli/tests/graph_cost.rs`
/// measures the same quantity on a real seed-42 world with many more
/// settlements and asserts the size-risk bound `make gate-full` needs.
#[test]
fn land_route_attempts_are_bounded_on_the_fixture() {
    let (geo, _elevation, _biome, _current, settlements) = fixture();
    let attempts = land_route_attempt_count(&geo, &settlements, &cfg());
    // Measured: all 3 possible pairs among 3 settlements sit within
    // `cfg().land_route_radius` (10 hops) on this small mesh, so the exact
    // count is C(3,2) = 3 -- an equality assertion (not just an upper bound)
    // so a regression that silently widened or narrowed the radius check
    // still trips this. The fixture is too small to prove the O(N^2)-vs-
    // bounded distinction itself; the heavy battery in
    // `cli/tests/graph_cost.rs` measures that on a real seed-42 world with
    // many more settlements.
    assert_eq!(
        attempts, 3,
        "expected exactly 3 land-route attempts on this 3-settlement fixture, got {attempts}"
    );
}

#[test]
fn a_shelf_joins_two_uplands_only_at_the_glacial_low_stand() {
    use hornvale_worldgen::graph_derive::connection_graph_at;
    let geo = Geosphere::new(1);
    // Two upland blobs (+100 m) around cells 0 and 30; the two-ring boundary
    // between them sits on a shelf at -50 m (ocean at present, land at -120 m).
    let ring2 = |seed: CellId| {
        let mut s = std::collections::BTreeSet::new();
        s.insert(seed);
        for &n in geo.neighbors(seed) {
            s.insert(n);
        }
        for c in s.clone() {
            for &n in geo.neighbors(c) {
                s.insert(n);
            }
        }
        s
    };
    let up_a = ring2(CellId(0));
    let far = geo
        .cells()
        .filter(|c| !up_a.contains(c))
        .max_by_key(|&c| geo.hops_between(CellId(0), c, 16).unwrap_or(0))
        .unwrap();
    let up_b: std::collections::BTreeSet<_> = ring2(far).difference(&up_a).copied().collect();
    let elevation = CellMap::from_fn(&geo, |c| {
        if up_a.contains(&c) || up_b.contains(&c) {
            e(100.0)
        } else {
            e(-50.0)
        }
    });
    let current = CellMap::from_fn(&geo, |_| [0.0, 0.0, 0.0]); // no lanes — test the bridge
    let cfg = GraphConfig::default();
    let present = connection_graph_at(&geo, &elevation, e(0.0), &current, &[], &cfg);
    let glacial = connection_graph_at(&geo, &elevation, e(-120.0), &current, &[], &cfg);
    // Present: the shelf is ocean, so the two uplands are separate components.
    assert!(
        present
            .reachable_regions(1e-9)
            .iter()
            .filter(|c| c.len() > 1)
            .count()
            >= 2,
        "present sea should leave the uplands sundered"
    );
    // Glacial: the shelf is land, bridging them into one big component.
    let glacial_big = glacial
        .reachable_regions(1e-9)
        .into_iter()
        .map(|c| c.len())
        .max()
        .unwrap();
    assert!(
        glacial_big
            > present
                .reachable_regions(1e-9)
                .into_iter()
                .map(|c| c.len())
                .max()
                .unwrap(),
        "the glacial low-stand must merge the uplands via the exposed shelf"
    );
}

#[test]
fn the_graph_is_deterministic_across_rebuilds() {
    let (geo, elevation, biome, current, settlements) = fixture();
    let config = cfg();
    let a = connection_graph(&geo, &elevation, &biome, &current, &settlements, &config);
    let b = connection_graph(&geo, &elevation, &biome, &current, &settlements, &config);
    for cell in geo.cells() {
        assert_eq!(
            a.edges(cell),
            b.edges(cell),
            "cell {} edges diverged across rebuilds",
            cell.0
        );
    }
}
