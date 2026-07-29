//! Committed scene profiler (The Sextant §3.1). Drives the scene API in the
//! shape the Orrery drives it — genesis once, then the six scene documents,
//! then a fan of region patches — and prints per-operation wall time.
//!
//! The workload is named after the CONSUMER on purpose. Exercising each
//! scene function once (the `profile_build.rs` shape) shows a large but
//! unremarkable per-call cost; the redundancy this profiler exists to expose
//! is only visible when region calls REPEAT, because each one re-derives the
//! whole planet.
//!
//! Run: `cargo run -p hornvale-scene --example profile_scene -- [TILES]`
//! (TILES defaults to 8 region patches.)

// The profiler measures wall-clock durations for a committed diagnostic
// only — it never touches WorldTime or facts, so it is exempt from the
// no-wall-clock-in-the-sim rule (the sanctioned Instant use for this crate).
#[allow(clippy::disallowed_types)]
use std::time::Instant;

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

/// The canonical fixture seed.
const SEED: u64 = 42;
/// The Orrery's `TILE_QUADS` (orrery `src/views/cubeSphere.ts:11`). The
/// region node grid is `(samples + 1)²`.
const SAMPLES: u32 = 64;
/// The Orrery's tiles export width (orrery `src/sim/worker.ts`).
const TILES_WIDTH: u32 = 512;
/// The Orrery's `REGION_MIN_LEVEL` (orrery `src/views/globe.ts:346`) — the
/// shallowest quadtree level that requests a region patch.
const REGION_LEVEL: u32 = 3;

/// Milliseconds elapsed since `t`.
#[allow(clippy::disallowed_types)] // benchmark harness: diagnostic timing only
fn ms(t: Instant) -> f64 {
    t.elapsed().as_secs_f64() * 1000.0
}

/// A deterministic fan of the first `n` level-`REGION_LEVEL` tile addresses in
/// face-major order: face 0's `(1 << REGION_LEVEL)²` = 64 tiles in row-major
/// order, then face 1, and so on. So the default fan of 8 — and even a fan of
/// 24 — is a contiguous run of addresses **on face 0 alone**.
///
/// That is deliberate and sufficient for what this profiler measures. The
/// finding is per-tile cost that does not fall as the fan grows, and per-tile
/// cost was measured flat across fan sizes 1 / 8 / 24 (687.3 / 700.5 / 701.8
/// ms — spec §1), which is the redundancy signature. Nothing here claims
/// per-tile cost is independent of *address*: that was never measured, and
/// every number this campaign reports comes from cube face 0.
fn tile_fan(n: usize) -> Vec<(u32, u32, u32)> {
    let per_edge = 1u32 << REGION_LEVEL;
    let mut out = Vec::with_capacity(n);
    'outer: for face in 0..6u32 {
        for iy in 0..per_edge {
            for ix in 0..per_edge {
                out.push((face, ix, iy));
                if out.len() >= n {
                    break 'outer;
                }
            }
        }
    }
    out
}

/// Build the seed-42 world the way the catalog's `hw_new` does.
fn genesis() -> (World, f64) {
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t = Instant::now();
    let world = build_world(
        Seed(SEED),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let elapsed = ms(t);
    (world, elapsed)
}

fn main() {
    let tiles: usize = std::env::args()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(8);

    let (world, genesis_ms) = genesis();

    #[allow(clippy::disallowed_types)] // benchmark harness
    let t = Instant::now();
    let tiles_scene = hornvale_scene::tiles_scene(&world, TILES_WIDTH).expect("tiles scene");
    let tiles_build_ms = ms(t);

    #[allow(clippy::disallowed_types)] // benchmark harness
    let t = Instant::now();
    let tiles_bytes = hornvale_scene::scene_json(&tiles_scene).len();
    let tiles_json_ms = ms(t);

    #[allow(clippy::disallowed_types)] // benchmark harness
    let t = Instant::now();
    let mut small_bytes = 0usize;
    small_bytes +=
        hornvale_scene::system_json(&hornvale_scene::system_scene(&world).expect("system scene"))
            .len();
    small_bytes +=
        hornvale_scene::moons_json(&hornvale_scene::moons_scene(&world).expect("moons scene"))
            .len();
    small_bytes += hornvale_scene::neighbors_json(
        &hornvale_scene::neighbors_scene(&world).expect("neighbors scene"),
    )
    .len();
    small_bytes += hornvale_scene::eclipses_json(
        &hornvale_scene::eclipses_scene(&world, 0.0, 365.0).expect("eclipses scene"),
    )
    .len();
    let small_ms = ms(t);

    let fan = tile_fan(tiles);
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t = Instant::now();
    let mut region_bytes = 0usize;
    for (face, ix, iy) in &fan {
        let scene =
            hornvale_scene::tiles_region_scene(&world, *face, REGION_LEVEL, *ix, *iy, SAMPLES)
                .expect("region scene");
        region_bytes += hornvale_scene::region_json(&scene).len();
    }
    let region_ms = ms(t);
    let per_tile_ms = region_ms / fan.len() as f64;

    let total = genesis_ms + tiles_build_ms + tiles_json_ms + small_ms + region_ms;
    println!("scene profile (seed {SEED}, {} region tiles):", fan.len());
    println!("  {:<26} {genesis_ms:9.1} ms", "hw_new");
    println!(
        "  {:<26} {tiles_build_ms:9.1} ms  ({} KB)",
        format!("hw_scene_tiles({TILES_WIDTH}) build"),
        tiles_bytes / 1024
    );
    println!("  {:<26} {tiles_json_ms:9.1} ms", "hw_scene_tiles json");
    println!(
        "  {:<26} {small_ms:9.1} ms  ({small_bytes} B)",
        "system+moons+neigh+ecl"
    );
    println!(
        "  {:<26} {region_ms:9.1} ms  ({} KB)",
        format!("hw_scene_tiles_region x{}", fan.len()),
        region_bytes / 1024
    );
    println!(
        "  {:<26} {per_tile_ms:9.1} ms  <-- the per-tile figure",
        "  ... per tile"
    );
    println!("  {:<26} {total:9.1} ms", "TOTAL");
}
