//! The scene APIs' **cost gate** (The Sextant §3.2): the client-facing
//! surface the Orrery renders through.
//!
//! Every `windows/scene` entry point re-derives terrain and climate from the
//! `World` (`terrain_of` + `climate_from` at the top of `tiles_scene`,
//! `temperature_grid`, and `tiles_region_scene`). That is ~638 ms of fixed
//! overhead per call, and the Orrery pays it once per LOD tile on every
//! camera move. This battery does not fix that — it pins it, so it cannot
//! get worse unnoticed while the fix is pending.
//!
//! Budgets here are **falsification ceilings, not targets**, in the sense
//! `graph_cost.rs` established: set above the measured value so only a real
//! regression trips them.
//!
//! **Ceilings ratchet DOWN freely. Raising one is an explicit, reviewed act**
//! and must be recorded in that constant's doc comment with the reason
//! (The Sextant §3.3). `graph_cost.rs`'s own history — 2.6 s → 90 s as the
//! world grew — is why this rule is written down rather than assumed.
//!
//! `#[ignore]`d: a live-worldgen build takes minutes, so this is deferred
//! from the commit gate (`make gate`) to `make gate-full`.
//!
//! ## Measured
//!
//! **Dev profile (this box, `lefford`, `cargo test -p hornvale --test
//! scene_cost -- --ignored --nocapture`, 2026-07-28) — the ceiling basis,
//! since `make gate-full` runs the heavy tier without `--release`
//! (`scripts/gate-full-heavy.sh:47`, no `--release`)**. Three runs; the
//! slowest per metric (not necessarily the same run) was taken as the
//! ceiling basis:
//!
//! ```text
//! run 1: genesis 6275.8 ms, tiles(512)+json 5400.2 ms, region per tile 1545.8 ms
//! run 2: genesis 6248.2 ms, tiles(512)+json 5435.8 ms, region per tile 1494.5 ms
//! run 3: genesis 6442.8 ms, tiles(512)+json 5444.0 ms, region per tile 1498.6 ms
//! slowest per metric: genesis 6442.8 ms, tiles(512)+json 5444.0 ms, region per tile 1545.8 ms
//! ```
//!
//! **Release profile (Task 1's reference measurement, seed 42, 8 region
//! tiles, `cargo run -p hornvale-scene --example profile_scene -- 8`,
//! `/tmp/sextant-baseline.txt`) — the campaign's reference measurement
//! (spec §1), NOT the ceiling basis (a release-based ceiling is roughly 2x
//! too tight for the dev profile `gate-full` actually runs):**
//!
//! ```text
//! scene profile (seed 42, 8 region tiles):
//!   hw_new                        1848.9 ms
//!   hw_scene_tiles(512) build      930.6 ms  (17313 KB)
//!   hw_scene_tiles json            553.4 ms
//!   system+moons+neigh+ecl           0.3 ms  (12712 B)
//!   hw_scene_tiles_region x8      4651.6 ms  (2335 KB)
//!     ... per tile                 581.5 ms
//!   TOTAL                         7984.7 ms
//! ```

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

// The measurement harness times derivation calls for a diagnostic (never sim
// logic, never a fact, never seeded from wall-clock) -- exempt from the
// wall-clock ban (clippy.toml / decision 0001), same pattern as
// `cli/tests/graph_cost.rs`.
#[allow(clippy::disallowed_types)]
use std::time::Instant;

/// The Orrery's `TILE_QUADS` (orrery `src/views/cubeSphere.ts:11`).
const SAMPLES: u32 = 64;
/// The Orrery's `REGION_MIN_LEVEL` (orrery `src/views/globe.ts:346`).
const REGION_LEVEL: u32 = 3;
/// Region patches measured. Small: each one currently costs ~700 ms.
const REGION_TILES: usize = 4;

/// Wall-time budget for ONE `tiles_region_scene` call on a seed-42 world, at
/// the Orrery's own tile geometry.
///
/// Measured 1545.8 ms/tile on 2026-07-28 (slowest of three runs), host
/// `lefford` (40 cores), dev profile, as `gate-full` runs it, via `cargo
/// test -p hornvale --test scene_cost -- --ignored --nocapture`.
///
/// **This ceiling is deliberately above a known-bad number.** ~91% of that
/// measurement is redundant re-derivation of terrain and climate (The
/// Sextant §1). The ceiling locks in "no worse than today" while the fix is
/// pending; the fix campaign ratchets it down. Budgeted at ~2x the
/// measurement — tight enough that doubling the per-call work trips it.
const REGION_PER_TILE_BUDGET_MS: f64 = 3100.0;

/// Wall-time budget for one `hw_new`-equivalent `build_world` at
/// `BuildDepth::Full`, which is what the catalog's `hw_new` performs.
///
/// Measured 6442.8 ms on 2026-07-28 (slowest of three runs), host
/// `lefford`, dev profile, as `gate-full` runs it. Budgeted at ~2x.
const GENESIS_BUDGET_MS: f64 = 13000.0;

/// Wall-time budget for `tiles_scene(512)` — the globe base export — and its
/// JSON serialization, measured together.
///
/// Measured 5444.0 ms build+json on 2026-07-28 (slowest of three runs), host
/// `lefford`, dev profile, as `gate-full` runs it. Budgeted at ~2x the sum.
/// Build and JSON are summed rather than budgeted separately because they
/// regress together: both scale with `width`, the one knob that changes
/// this document's size.
const TILES_BUDGET_MS: f64 = 11000.0;

/// The cost gate. Prints every measured number (`--nocapture`) so a future
/// re-baselining does not need to re-derive the harness.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn scene_api_cost_is_bounded_on_seed_42() {
    #[allow(clippy::disallowed_types)] // benchmark harness
    let start = Instant::now();
    let world = build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds");
    #[allow(clippy::disallowed_types)] // benchmark harness
    let genesis_ms = start.elapsed().as_secs_f64() * 1000.0;

    #[allow(clippy::disallowed_types)] // benchmark harness
    let start = Instant::now();
    let scene = hornvale_scene::tiles_scene(&world, 512).expect("tiles scene");
    let json = hornvale_scene::scene_json(&scene);
    #[allow(clippy::disallowed_types)] // benchmark harness
    let tiles_ms = start.elapsed().as_secs_f64() * 1000.0;
    assert!(!json.is_empty(), "the tiles document is non-empty");

    #[allow(clippy::disallowed_types)] // benchmark harness
    let start = Instant::now();
    for i in 0..REGION_TILES {
        let ix = (i as u32) % (1 << REGION_LEVEL);
        let scene = hornvale_scene::tiles_region_scene(&world, 0, REGION_LEVEL, ix, 0, SAMPLES)
            .expect("region scene");
        assert!(
            !hornvale_scene::region_json(&scene).is_empty(),
            "the region document is non-empty"
        );
    }
    #[allow(clippy::disallowed_types)] // benchmark harness
    let region_ms = start.elapsed().as_secs_f64() * 1000.0;
    let per_tile_ms = region_ms / REGION_TILES as f64;

    println!("genesis            {genesis_ms:9.1} ms (budget {GENESIS_BUDGET_MS})");
    println!("tiles(512)+json    {tiles_ms:9.1} ms (budget {TILES_BUDGET_MS})");
    println!("region per tile    {per_tile_ms:9.1} ms (budget {REGION_PER_TILE_BUDGET_MS})");

    assert!(
        genesis_ms < GENESIS_BUDGET_MS,
        "genesis took {genesis_ms:.1} ms, over the {GENESIS_BUDGET_MS} ms ceiling"
    );
    assert!(
        tiles_ms < TILES_BUDGET_MS,
        "tiles_scene(512)+json took {tiles_ms:.1} ms, over the {TILES_BUDGET_MS} ms ceiling"
    );
    assert!(
        per_tile_ms < REGION_PER_TILE_BUDGET_MS,
        "tiles_region_scene took {per_tile_ms:.1} ms/tile, over the \
         {REGION_PER_TILE_BUDGET_MS} ms ceiling"
    );
}
