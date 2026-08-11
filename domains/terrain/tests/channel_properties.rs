//! Channel-network properties on a real seed-42 world (The Ford, Task 5):
//! the provider builds and exposes `ChannelNetwork`, and building it must
//! never perturb determinism.

use hornvale_kernel::{CellId, Geosphere, Seed, math};
use hornvale_terrain::{GeneratedTerrain, TerrainPins, generate};

/// Level 5: the minimum subdivision that actually accumulates
/// `RIVER_MIN_DRAINAGE` anywhere for seed 42 — level 4 has zero river cells
/// (see `channel.rs`'s own `a_built_network_is_well_formed_and_deterministic`
/// test comment). Level 6 (the canonical grid) is Task 6's measurement, not
/// this task's, and `transverse_at` is a known, already-reviewed O(total
/// vertices)-per-query cost, so this file keeps both the mesh and its
/// sample counts modest rather than paying for the canonical grid.
const TEST_LEVEL: u32 = 5;

fn build_seed_42_terrain() -> GeneratedTerrain {
    let geo = Geosphere::new(TEST_LEVEL);
    let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
    GeneratedTerrain::new(geo, outcome)
}

/// Mean angular separation of `c` from its neighbours — the same quantity
/// `channel.rs`'s private `cell_spacing` computes, re-derived here from two
/// adjacent cell positions because `Geosphere` has no public
/// `mean_cell_edge()` and one integration-test caller does not earn it a
/// new kernel method.
fn local_cell_edge(geo: &hornvale_kernel::Geosphere, c: CellId) -> f64 {
    let neighbors = geo.neighbors(c);
    assert!(!neighbors.is_empty(), "cell {c:?} has no neighbours");
    let p = geo.position(c);
    let sum: f64 = neighbors
        .iter()
        .map(|&n| {
            let q = geo.position(n);
            let dot = p[0] * q[0] + p[1] * q[1] + p[2] * q[2];
            math::acos(dot.clamp(-1.0, 1.0))
        })
        .sum();
    sum / neighbors.len() as f64
}

#[test]
fn seed_42_has_channels_and_they_are_narrower_than_a_cell() {
    let terrain = build_seed_42_terrain();
    let net = terrain.channels();
    assert!(!net.polylines.is_empty(), "seed 42 has no channels at all");
    let widest = net.widest_half_width() * 2.0;
    let cell_edge = local_cell_edge(terrain.geosphere(), CellId(0));
    assert!(
        widest < cell_edge / 10.0,
        "widest channel {widest} is not far narrower than a cell edge {cell_edge}"
    );
}

/// PIN ISOLATION, strengthened.
///
/// The brief's own version of this test built two terrains from the same
/// seed and queried `.channels()` on only one, then compared
/// `elevation_at`/`drainage_at` between them. That does not discriminate:
/// `GeneratedTerrain::new` builds the channel network UNCONDITIONALLY at
/// construction (Task 5's own design — "not lazily per call"), so both
/// terrains already have a built network before either is queried, and
/// `elevation_at`/`drainage_at` read fields `generate()` populated entirely
/// BEFORE `GeneratedTerrain::new` (and therefore the channel network) ever
/// runs. There is also no shared mutable RNG state in this codebase — every
/// `Seed`/`Stream` is an explicit, pure value threaded by the caller — so
/// two independent `generate()` calls from the same seed are guaranteed
/// byte-identical regardless of anything the channel network does. The
/// original test would pass even if `ChannelNetwork::build` secretly
/// consumed a stream draw; it was asserting ordinary determinism, not pin
/// isolation.
///
/// What WOULD actually indicate a stray draw or hidden global state is a
/// later, wholly independent `generate()` call from the SAME seed
/// diverging from a baseline that never touched the channel network at
/// all. That is what this test checks: build a totally untouched baseline
/// first, then separately build a terrain, exercise its channel network
/// hard (build, `transverse_at` at many positions, `meander_at`), and
/// finally call `generate()` again — a fresh, independent call — and
/// diff it against the untouched baseline. A `static`/`thread_local`
/// leak from channel-building into a later genesis call is exactly the
/// class of defect this catches that the brief's version could not.
#[test]
fn querying_the_network_consumes_no_draws() {
    let geo = Geosphere::new(TEST_LEVEL);
    let baseline = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();

    let terrain = build_seed_42_terrain();
    let net = terrain.channels();
    assert!(!net.polylines.is_empty(), "seed 42 has no channels at all");
    // Exercise the network hard: every vertex of every polyline, plus a
    // spread of off-line positions, so a stray draw anywhere in
    // `transverse_at`/`meander_at` has many chances to show up.
    for line in &net.polylines {
        for &v in &line.points {
            let _ = net.transverse_at(v);
            let _ = net.meander_at(v);
        }
    }
    for c in terrain.geosphere().cells().step_by(97) {
        let _ = terrain.transverse_at(terrain.geosphere().position(c));
    }

    let after = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
    assert_eq!(
        after.globe, baseline.globe,
        "an independent generate() call after heavy channel-network use diverged \
         from a baseline that never touched the network — a stray draw or hidden \
         global state"
    );

    // The brief's original comparison, kept as a cheap sanity check: the
    // two terrains built from the same seed still agree everywhere.
    let other = build_seed_42_terrain();
    for c in terrain.geosphere().cells() {
        assert_eq!(
            terrain.elevation_at(c),
            other.elevation_at(c),
            "cell {c:?} diverged"
        );
        assert_eq!(
            terrain.drainage_at(c),
            other.drainage_at(c),
            "cell {c:?} diverged"
        );
    }
}

/// The provider's `transverse_at` delegates to the network's own — a
/// smoke test that the two agree pointwise, since `provider.rs` must never
/// silently diverge from `ChannelNetwork::transverse_at`.
#[test]
fn provider_transverse_at_agrees_with_the_network() {
    let terrain = build_seed_42_terrain();
    let net = terrain.channels();
    for c in terrain.geosphere().cells().step_by(53) {
        let p = terrain.geosphere().position(c);
        assert_eq!(terrain.transverse_at(p), net.transverse_at(p));
    }
}
