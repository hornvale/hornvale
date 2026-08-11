//! Channel-network properties on a real seed-42 world (The Ford, Task 5):
//! the provider builds and exposes `ChannelNetwork`. See the comment above
//! `provider_transverse_at_agrees_with_the_network` for why this file does
//! not carry a pin-isolation test of its own.

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

// WHY THIS FILE HAS NO PIN-ISOLATION TEST.
//
// The brief's version built two terrains from the same seed, queried
// `.channels()` on only one, and compared `elevation_at`/`drainage_at`
// between them. It cannot discriminate: `GeneratedTerrain::new` builds the
// channel network UNCONDITIONALLY at construction (Task 5's own design —
// "not lazily per call"), so both terrains already have a built network
// before either is queried, and `elevation_at`/`drainage_at` read fields
// `generate()` populated entirely BEFORE `GeneratedTerrain::new` (and
// therefore the channel network) ever runs.
//
// A first replacement attempt tried diffing a `generate()` baseline that
// never touched the network against one taken after heavy channel-network
// use, reasoning that a hidden global/`static` leak would show up there.
// Review mutation-proved that one vacuous too: inserting a literal stray
// draw at the top of `ChannelNetwork::build` (`seed.stream().next_f64()`)
// left it green, because `build` takes `Seed` by value — a `Copy` type with
// no interior mutability — so a draw off a `Stream` built from it is
// structurally unobservable outside that call frame. What the test then
// actually asserted (two `generate()` calls from the same seed agree) is
// already covered by
// `tectonic_properties.rs::genesis_is_deterministic_across_the_sweep`
// (which includes seed 42), so it added cost without adding detection
// power. A test whose name promises a property it cannot detect is worse
// than no test — deleted rather than kept for appearances.
//
// The property this file's provider changes actually rely on — that no
// caller of `channel_seed`/`channel_noise_seed` can derive any stream but
// `streams::CHANNEL_MEANDER` — is enforced by the type itself: the field
// stores the already-derived leg (see `TectonicGlobe::channel_seed`'s doc
// in `globe.rs`), not the terrain-root seed, so there is no live draw
// sequence left for a caller here to perturb even in principle.

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
