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
//! Since The Cistern the profiler runs the **same workload twice** and prints
//! both halves side by side:
//!
//! - the `&World` path — every terrain-facing call builds its own
//!   [`hornvale_scene::SceneContext`] internally, which is the pre-Cistern
//!   behaviour preserved by delegation;
//! - the `SceneContext` path — one context built for the world, then passed
//!   to the `_in` variants, which is what a real client (the Orrery's
//!   catalog) does.
//!
//! Two paths in one run is the honest instrument: the before and the after
//! are measured on the same box, the same build, and the same world, so the
//! ratio between them is not confounded by anything the machine was doing
//! between two separate runs.
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

/// The per-tile layers the Orrery's `parseTiles` actually extracts (orrery
/// `src/sim/scene.ts:521-532`), verified against that function rather than
/// against any spec's transcription of it: `numberArray`/`booleanArray` are
/// called for exactly these eight names.
///
/// Two corrections to the campaign spec's §1.1, both found by reading the
/// source: `features` is document metadata (always emitted, never selectable),
/// and `water` is not read anywhere in the Orrery's `src/` — so the client's
/// read set is **eight** per-tile arrays, not ten, and **eleven** layers go
/// unread rather than nine.
const ORRERY_FIELDS: &[&str] = &[
    "elevation_m",
    "ocean",
    "biome",
    "plate",
    "unrest",
    "t_mean_c",
    "t_swing_c",
    "moisture",
];

/// How many times each serialization is timed. Serialization is
/// allocation-heavy and the first pass warms the allocator, so one sample is
/// not a measurement; the report prints every run and the median.
const SERIALIZE_RUNS: usize = 3;

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

/// One measured pass over the scene workload: the timings a single traversal
/// of the six documents plus the region fan produced.
struct Pass {
    /// Cost of building the shared [`hornvale_scene::SceneContext`], or `None`
    /// on the `&World` path where each call builds its own internally.
    ctx_build_ms: Option<f64>,
    /// `tiles_scene` build time.
    tiles_build_ms: f64,
    /// Size of the serialized tiles document.
    tiles_bytes: usize,
    /// `scene_json` serialization time for the tiles document.
    tiles_json_ms: f64,
    /// The four astronomical documents, built and serialized as one aggregate.
    small_ms: f64,
    /// Their combined serialized size.
    small_bytes: usize,
    /// The whole region fan, built and serialized.
    region_ms: f64,
    /// The fan's combined serialized size.
    region_bytes: usize,
}

impl Pass {
    /// Everything this pass spent, excluding genesis.
    fn subtotal_ms(&self) -> f64 {
        self.ctx_build_ms.unwrap_or(0.0)
            + self.tiles_build_ms
            + self.tiles_json_ms
            + self.small_ms
            + self.region_ms
    }

    /// The headline: mean cost of one region patch, build plus serialization.
    fn per_tile_ms(&self, tiles: usize) -> f64 {
        self.region_ms / tiles as f64
    }
}

/// The four terrain-free documents. They read `sky_of` only, take no context,
/// and are measured identically in both passes so the two totals compare.
#[allow(clippy::disallowed_types)] // benchmark harness
fn small_docs(world: &World) -> (f64, usize) {
    let t = Instant::now();
    let mut bytes = 0usize;
    bytes +=
        hornvale_scene::system_json(&hornvale_scene::system_scene(world).expect("system scene"))
            .len();
    bytes +=
        hornvale_scene::moons_json(&hornvale_scene::moons_scene(world).expect("moons scene")).len();
    bytes += hornvale_scene::neighbors_json(
        &hornvale_scene::neighbors_scene(world).expect("neighbors scene"),
    )
    .len();
    bytes += hornvale_scene::eclipses_json(
        &hornvale_scene::eclipses_scene(world, 0.0, 365.0).expect("eclipses scene"),
    )
    .len();
    (ms(t), bytes)
}

/// The pre-Cistern shape: every terrain-facing call derives its own planet.
#[allow(clippy::disallowed_types)] // benchmark harness
fn world_pass(world: &World, fan: &[(u32, u32, u32)]) -> Pass {
    let t = Instant::now();
    let tiles_scene = hornvale_scene::tiles_scene(world, TILES_WIDTH).expect("tiles scene");
    let tiles_build_ms = ms(t);

    let t = Instant::now();
    let tiles_bytes = hornvale_scene::scene_json(&tiles_scene).len();
    let tiles_json_ms = ms(t);

    let (small_ms, small_bytes) = small_docs(world);

    let t = Instant::now();
    let mut region_bytes = 0usize;
    for (face, ix, iy) in fan {
        let scene =
            hornvale_scene::tiles_region_scene(world, *face, REGION_LEVEL, *ix, *iy, SAMPLES)
                .expect("region scene");
        region_bytes += hornvale_scene::region_json(&scene).len();
    }
    let region_ms = ms(t);

    Pass {
        ctx_build_ms: None,
        tiles_build_ms,
        tiles_bytes,
        tiles_json_ms,
        small_ms,
        small_bytes,
        region_ms,
        region_bytes,
    }
}

/// The client shape: one context per world, then the `_in` variants.
#[allow(clippy::disallowed_types)] // benchmark harness
fn context_pass(world: &World, fan: &[(u32, u32, u32)]) -> Pass {
    let t = Instant::now();
    let ctx = hornvale_scene::SceneContext::build(world).expect("scene context");
    let ctx_build_ms = ms(t);

    let t = Instant::now();
    let tiles_scene =
        hornvale_scene::tiles_scene_in(world, &ctx, TILES_WIDTH).expect("tiles scene");
    let tiles_build_ms = ms(t);

    let t = Instant::now();
    let tiles_bytes = hornvale_scene::scene_json(&tiles_scene).len();
    let tiles_json_ms = ms(t);

    let (small_ms, small_bytes) = small_docs(world);

    let t = Instant::now();
    let mut region_bytes = 0usize;
    for (face, ix, iy) in fan {
        let scene = hornvale_scene::tiles_region_scene_in(
            world,
            &ctx,
            *face,
            REGION_LEVEL,
            *ix,
            *iy,
            SAMPLES,
        )
        .expect("region scene");
        region_bytes += hornvale_scene::region_json(&scene).len();
    }
    let region_ms = ms(t);

    Pass {
        ctx_build_ms: Some(ctx_build_ms),
        tiles_build_ms,
        tiles_bytes,
        tiles_json_ms,
        small_ms,
        small_bytes,
        region_ms,
        region_bytes,
    }
}

/// Print one pass in the profiler's established column layout.
fn report(pass: &Pass, tiles: usize, genesis_ms: f64) {
    if let Some(ctx_ms) = pass.ctx_build_ms {
        println!("  {:<26} {ctx_ms:9.1} ms", "SceneContext::build");
    }
    println!(
        "  {:<26} {:9.1} ms  ({} KB)",
        format!("hw_scene_tiles({TILES_WIDTH}) build"),
        pass.tiles_build_ms,
        pass.tiles_bytes / 1024
    );
    println!(
        "  {:<26} {:9.1} ms",
        "hw_scene_tiles json", pass.tiles_json_ms
    );
    println!(
        "  {:<26} {:9.1} ms  ({} B)",
        "system+moons+neigh+ecl", pass.small_ms, pass.small_bytes
    );
    println!(
        "  {:<26} {:9.1} ms  ({} KB)",
        format!("hw_scene_tiles_region x{tiles}"),
        pass.region_ms,
        pass.region_bytes / 1024
    );
    println!(
        "  {:<26} {:9.1} ms  <-- the per-tile figure",
        "  ... per tile",
        pass.per_tile_ms(tiles)
    );
    println!(
        "  {:<26} {:9.1} ms",
        "TOTAL",
        genesis_ms + pass.subtotal_ms()
    );
}

/// Time `scene_json_selected` over `fields` [`SERIALIZE_RUNS`] times, returning
/// the document's size and every run's milliseconds (ascending).
#[allow(clippy::disallowed_types)] // benchmark harness
fn timed_serialize(
    scene: &hornvale_scene::TilesScene,
    fields: &hornvale_scene::TileFields,
) -> (usize, Vec<f64>) {
    let mut bytes = 0usize;
    let mut runs = Vec::with_capacity(SERIALIZE_RUNS);
    for _ in 0..SERIALIZE_RUNS {
        let t = Instant::now();
        let json = hornvale_scene::scene_json_selected(scene, fields);
        let elapsed = ms(t);
        bytes = json.len();
        runs.push(elapsed);
    }
    runs.sort_by(f64::total_cmp);
    (bytes, runs)
}

/// The median of an ascending, non-empty slice.
fn median(sorted: &[f64]) -> f64 {
    let n = sorted.len();
    if n % 2 == 1 {
        sorted[n / 2]
    } else {
        (sorted[n / 2 - 1] + sorted[n / 2]) / 2.0
    }
}

/// Every run, as `a / b / c`.
fn runs_text(runs: &[f64]) -> String {
    runs.iter()
        .map(|r| format!("{r:.1}"))
        .collect::<Vec<_>>()
        .join(" / ")
}

/// The Winnowing: what a projected document costs against the full one.
///
/// Also prints the measured per-field composition. Each layer's contribution is
/// `len(that layer alone) - len(metadata only)`, which is the layer's bytes in
/// *any* document containing it — that is the independence property §1.3 rests
/// on, and the printed residual (composition sum + metadata against the full
/// document) is a direct check of it at width 512 rather than an assumption.
fn projection_report(scene: &hornvale_scene::TilesScene) {
    let all = hornvale_scene::TileFields::all();
    let orrery = hornvale_scene::TileFields::only(ORRERY_FIELDS).expect("Orrery field names exist");
    let none = hornvale_scene::TileFields::only(&[]).expect("the empty selection is valid");

    let (full_bytes, full_runs) = timed_serialize(scene, &all);
    let (proj_bytes, proj_runs) = timed_serialize(scene, &orrery);
    let meta_bytes = hornvale_scene::scene_json_selected(scene, &none).len();

    println!("  -- The Winnowing: projection at width {TILES_WIDTH} --");
    println!("  metadata only (no layers)  {meta_bytes:>12} B");
    println!("  composition, one layer at a time (bytes in any document carrying it):");
    let mut sum = 0usize;
    for name in hornvale_scene::TileFields::ALL_NAMES {
        let one = hornvale_scene::TileFields::only(&[name]).expect("ALL_NAMES is selectable");
        let contribution = hornvale_scene::scene_json_selected(scene, &one).len() - meta_bytes;
        sum += contribution;
        let read = if ORRERY_FIELDS.contains(name) {
            "read"
        } else {
            "UNREAD"
        };
        println!(
            "    {name:<20} {contribution:>10} B  {:>5.1}%  {read}",
            100.0 * contribution as f64 / full_bytes as f64
        );
    }
    let residual = full_bytes as i64 - (sum + meta_bytes) as i64;
    println!(
        "    {:<20} {:>10} B  (residual against the full document: {residual} B)",
        "sum + metadata",
        sum + meta_bytes
    );

    println!();
    println!(
        "  full document       {full_bytes:>12} B   serialize {} ms  (median {:.1})",
        runs_text(&full_runs),
        median(&full_runs)
    );
    println!(
        "  Orrery's {} layers   {proj_bytes:>12} B   serialize {} ms  (median {:.1})",
        ORRERY_FIELDS.len(),
        runs_text(&proj_runs),
        median(&proj_runs)
    );
    let byte_ratio = proj_bytes as f64 / full_bytes as f64;
    let time_ratio = median(&proj_runs) / median(&full_runs);
    println!(
        "  bytes    {:.1}% of full ({:+.1}%)   serialize {:.1}% of full ({:+.1}%)",
        100.0 * byte_ratio,
        100.0 * (byte_ratio - 1.0),
        100.0 * time_ratio,
        100.0 * (time_ratio - 1.0)
    );
    println!(
        "  proportionality: time/byte ratio = {:.3} (1.000 = serialize fell exactly with bytes)",
        time_ratio / byte_ratio
    );
}

fn main() {
    let tiles: usize = std::env::args()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(8);

    let (world, genesis_ms) = genesis();
    let fan = tile_fan(tiles);
    let n = fan.len();

    // The `&World` pass runs first so it cannot benefit from anything the
    // context pass warmed; if either order biased the result it would be this
    // one, and it biases AGAINST the campaign's claim.
    let before = world_pass(&world, &fan);
    let after = context_pass(&world, &fan);

    println!("scene profile (seed {SEED}, {n} region tiles):");
    println!("  {:<26} {genesis_ms:9.1} ms", "hw_new");
    println!();
    println!("  -- &World path: one planet derived per call --");
    report(&before, n, genesis_ms);
    println!();
    println!("  -- SceneContext path: one planet derived per world --");
    report(&after, n, genesis_ms);
    println!();

    // The projection measurement gets its own freshly built scene so it is not
    // reading a document either pass left warm.
    let ctx = hornvale_scene::SceneContext::build(&world).expect("scene context");
    let tiles_scene =
        hornvale_scene::tiles_scene_in(&world, &ctx, TILES_WIDTH).expect("tiles scene");
    projection_report(&tiles_scene);
    println!();

    let before_tile = before.per_tile_ms(n);
    let after_tile = after.per_tile_ms(n);
    println!(
        "  per tile   {before_tile:.1} ms -> {after_tile:.1} ms   ({:.1}x)",
        before_tile / after_tile
    );
    let before_total = genesis_ms + before.subtotal_ms();
    let after_total = genesis_ms + after.subtotal_ms();
    println!(
        "  TOTAL      {before_total:.1} ms -> {after_total:.1} ms   ({:.1}x)",
        before_total / after_total
    );
}
