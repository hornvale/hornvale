//! The scene APIs' **cost gate** (The Sextant §3.2): the client-facing
//! surface the Orrery renders through.
//!
//! Every **terrain-facing** `windows/scene` entry point used to re-derive
//! terrain and climate from the `World` — ~638 ms of fixed overhead per call,
//! paid by the Orrery once per LOD tile on every camera move. The Sextant
//! pinned that cost; **The Cistern removed it** (2026-07-29). The derivation
//! now happens once per world, in `SceneContext::build`, and the `_in` entry
//! points take the context as an argument.
//!
//! So this battery measures **the path the client takes**: one
//! `SceneContext::build`, then `tiles_scene_in` and `tiles_region_scene_in`.
//! The `&World`-only wrappers still exist and still cost context-build plus
//! per-call work, but by construction — each one delegates to its `_in` form
//! — so pinning them separately would measure the same two numbers summed.
//!
//! `CONTEXT_BUDGET_MS` is where the old 638 ms went. It was added with the
//! ratchet below rather than left implicit: taking the derivation out of the
//! per-call ceilings would otherwise have left the single most expensive
//! operation on this surface with no ceiling over it at all.
//!
//! The purely astronomical documents — `system_scene`, `moons_scene`,
//! `neighbors_scene`, `eclipses_scene` — derive no terrain, which is exactly
//! why the four of them together cost under a millisecond. That is what
//! `SMALL_DOCS_BUDGET_MS` guards: if one of them ever acquired a `terrain_of`
//! call, its cost would go from a fraction of a millisecond to ~638 ms, and
//! the other three ceilings here would not notice.
//!
//! Budgets here are **falsification ceilings, not targets**, in the sense
//! `graph_cost.rs` established: set above the measured value so only a real
//! regression trips them.
//!
//! **Ceilings ratchet DOWN freely. Raising one is an explicit, reviewed act**
//! and must be recorded in that constant's doc comment with the reason
//! (The Sextant §3.3). `graph_cost.rs`'s own history — 2.6 s → 90 s as the
//! world grew — is why this rule is written down rather than assumed. The
//! Cistern is the first campaign to exercise the ratchet in its designed
//! direction; every constant below whose value changed names the Sextant
//! figure it descends from.
//!
//! `#[ignore]`d: a live-worldgen build takes minutes, so this is deferred
//! from the commit gate (`make gate`) to `make gate-full`.
//!
//! ## The observed failure mode is contention, and it is legible
//!
//! Every ceiling here is a wall time, so a pathological co-run trips them
//! together. Seen once during The Cistern, when a second `gate-full`'s
//! processes survived their parent and overlapped this one:
//!
//! ```text
//! genesis              24220.9 ms (budget 13000)   <- untouched Sextant ceiling
//! SceneContext::build   4068.2 ms (budget 2700)
//! tiles(512)+json      11114.8 ms (budget 8700)
//! small docs+json          6.8 ms (budget 5.2)     <- untouched Sextant ceiling
//! region per tile        565.9 ms (budget 420)
//! ```
//!
//! Read the whole print-out before suspecting the code. **All five inflated by
//! roughly 3x, and the two the code has not changed since 2026-07-28 blew
//! their ceilings by the widest margins** — `genesis` by 86%, against 35% for
//! the tightest new one. A real regression is local: one or two metrics move
//! and the rest hold. A uniform 3x across metrics with unrelated causes is the
//! machine. Re-run on an idle box before touching a constant, and never raise
//! one from a contended reading — that is precisely the ratchet-upward the
//! rule above exists to make deliberate.
//!
//! ## Measured — The Cistern (2026-07-29), the current ceiling basis
//!
//! **Dev profile (this box, `lefford`, `cargo test -p hornvale --test
//! scene_cost -- --ignored --nocapture`, 2026-07-29), the context path.**
//! Three runs; the slowest per metric was taken as the ceiling basis:
//!
//! ```text
//! run 1: genesis 6262.5, context 1290.0, tiles(512)+json 4319.9, small 2.7, region/tile 198.1
//! run 2: genesis 6178.5, context 1291.3, tiles(512)+json 4149.6, small 2.5, region/tile 206.1
//! run 3: genesis 6318.6, context 1308.0, tiles(512)+json 4181.7, small 2.6, region/tile 196.7
//! slowest: genesis 6318.6, context 1308.0, tiles+json 4319.9, small 2.7, region/tile 206.1
//! ```
//!
//! Taken under ~80-way load on the 40-core box, which is what makes them
//! comparable to The Sextant's basis below rather than to its unloaded
//! outliers: the two metrics this campaign did **not** touch reproduced it
//! within noise (genesis 6318.6 against 6442.8; small docs 2.7 against 2.6).
//! That agreement is the control — it is the evidence that the region and
//! tiles figures fell because the code changed, not because the box was idle.
//!
//! Release profile, same day, same box and load, measured within one run by
//! the two-pass profiler (`cargo run --release -p hornvale-scene --example
//! profile_scene -- 8`): region **902.3 ms/tile through the `&World` path
//! against 81.4 ms/tile through the context path — 11.1x**, against spec §5's
//! hypothesised ~11x. This is the run the chronicle
//! (`book/src/chronicle/the-cistern.md`) headlines and reproduces in full; an
//! immediately preceding run of the same binary gave 897.7 against 83.4 —
//! **10.8x** — which is the second observation, not a competing headline. A
//! within-run ratio is the load-robust statistic here; the absolute
//! milliseconds are not.
//!
//! ## Measured — The Sextant (2026-07-28), the superseded basis
//!
//! Kept because every ceiling below names the value it ratcheted down from,
//! and the provenance chain has to stay readable.
//!
//! **Dev profile (this box, `lefford`, `cargo test -p hornvale --test
//! scene_cost -- --ignored --nocapture`, 2026-07-28) — the then ceiling basis,
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
//! The small-documents aggregate was added later the same day, same box and
//! profile. Three unloaded runs gave 1.7 / 1.8 / 1.8 ms — but an unloaded run
//! is NOT comparable to the basis above, which the same three runs showed to
//! have been taken under load (they returned genesis 3819–3899 ms against the
//! recorded 6442.8). Re-run under 40-way CPU load the box reproduced the
//! basis (genesis 6876.8 ms, tiles+json 5620.0 ms, region per tile 1508.2 ms)
//! and the aggregate cost **2.6 ms**. That loaded figure is the ceiling basis,
//! so this constant carries the same ~2x headroom, against the same
//! conditions, as its three siblings.
//!
//! **Release profile (The Sextant's reference measurement, seed 42, 8 region
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
/// Region patches measured, sharing one context.
const REGION_TILES: usize = 4;

/// Wall-time budget for ONE `tiles_region_scene_in` call on a seed-42 world,
/// at the Orrery's own tile geometry, against a context already built.
///
/// **Ratcheted DOWN by The Cistern: 3100.0 → 420.0 ms.** Measured 206.1
/// ms/tile on 2026-07-29 (slowest of three runs), host `lefford` (40 cores),
/// dev profile, as `gate-full` runs it, via `cargo test -p hornvale --test
/// scene_cost -- --ignored --nocapture`. The Sextant's value was 3100.0,
/// from 1545.8 ms/tile measured on 2026-07-28 through the re-deriving path.
/// Budgeted at ~2x, unchanged in method.
///
/// This is the campaign's headline number. The old measurement was ~91%
/// redundant re-derivation (The Sextant §1); what remains is the sampling
/// and serialization the client actually asked for.
const REGION_PER_TILE_BUDGET_MS: f64 = 420.0;

/// Wall-time budget for one `hw_new`-equivalent `build_world` at
/// `BuildDepth::Full`, which is what the catalog's `hw_new` performs.
///
/// Measured 6442.8 ms on 2026-07-28 (slowest of three runs), host
/// `lefford`, dev profile, as `gate-full` runs it. Budgeted at ~2x.
///
/// **Unchanged by The Cistern, deliberately.** Re-measured 6318.6 ms on
/// 2026-07-29 under the same conditions — world building is not on this
/// campaign's path, and the agreement is the control that makes the two
/// ratchets below attributable to the code rather than to the box. Moving
/// 13000.0 to 12700.0 would be chasing noise.
const GENESIS_BUDGET_MS: f64 = 13000.0;

/// Wall-time budget for one `SceneContext::build` — the terrain and climate
/// derivation, the two nearest-cell indices, and the biome map, built once per
/// world and reused by every terrain-facing entry point.
///
/// **New in The Cistern; no Sextant counterpart.** Measured 1308.0 ms on
/// 2026-07-29 (slowest of three runs), host `lefford`, dev profile, as
/// `gate-full` runs it. Budgeted at ~2x.
///
/// The other ceilings in this file fell because this cost moved here. Adding
/// it is what keeps the ratchet honest: without it, the derivation would have
/// left the per-call ceilings and landed nowhere, and a regression inside
/// `terrain_of` or `climate_from` would trip nothing on this surface. It is
/// paid once per world rather than once per call, which is the whole change.
const CONTEXT_BUDGET_MS: f64 = 2700.0;

/// Wall-time budget for `tiles_scene(512)` — the globe base export — and its
/// JSON serialization, measured together.
///
/// **Ratcheted DOWN by The Cistern: 11000.0 → 8700.0 ms.** Measured 4319.9 ms
/// build+json on 2026-07-29 (slowest of three runs), host `lefford`, dev
/// profile, as `gate-full` runs it, now through `tiles_scene_in` against a
/// context already built. The Sextant's value was 11000.0, from 5444.0 ms
/// measured on 2026-07-28 through the re-deriving path. Budgeted at ~2x the
/// sum. Build and JSON are summed rather than budgeted separately because
/// they regress together: both scale with `width`, the one knob that changes
/// this document's size.
///
/// It falls by ~1.1 s rather than by the full derivation, and that is the
/// expected shape: serialization is untouched and is now the larger half of
/// this number (spec §2 — the JSON size problem survives this campaign).
const TILES_BUDGET_MS: f64 = 8700.0;

/// Wall-time budget for the four small scene documents — `system_scene`,
/// `moons_scene`, `neighbors_scene`, `eclipses_scene` — and their JSON
/// serialization, measured together as one aggregate (spec §3.2).
///
/// **Untouched by The Cistern**, which is the point: these four documents are
/// outside its scope and their cost did not move (2.7 ms re-measured on
/// 2026-07-29 against 2.6 ms, same box, same load band).
///
/// Measured 2.6 ms on 2026-07-28, host `lefford`, dev profile, as `gate-full`
/// runs it — the loaded run, matching the conditions under which the three
/// ceilings above were measured (unloaded the same aggregate costs 1.7–1.8 ms;
/// see the module doc). Budgeted at ~2x.
///
/// **The highest-signal ceiling in this file.** These four are cheap
/// *because* they derive no terrain — they read the sky only. Acquiring a
/// `terrain_of` call is this campaign's exact defect class, and it would take
/// this aggregate from a fraction of a millisecond to ~638 ms per document
/// while every other ceiling here stayed green.
const SMALL_DOCS_BUDGET_MS: f64 = 5.2;

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

    // The derivation, now paid ONCE per world rather than once per call.
    // This is where the ~638 ms that used to sit inside every terrain-facing
    // ceiling below went; it is measured here so that removing it from the
    // per-call ceilings does not leave it unguarded.
    #[allow(clippy::disallowed_types)] // benchmark harness
    let start = Instant::now();
    let ctx = hornvale_scene::SceneContext::build(&world).expect("scene context");
    #[allow(clippy::disallowed_types)] // benchmark harness
    let context_ms = start.elapsed().as_secs_f64() * 1000.0;

    #[allow(clippy::disallowed_types)] // benchmark harness
    let start = Instant::now();
    let scene = hornvale_scene::tiles_scene_in(&world, &ctx, 512).expect("tiles scene");
    let json = hornvale_scene::scene_json(&scene);
    #[allow(clippy::disallowed_types)] // benchmark harness
    let tiles_ms = start.elapsed().as_secs_f64() * 1000.0;
    assert!(!json.is_empty(), "the tiles document is non-empty");

    // The four terrain-free documents, timed as one aggregate: individually
    // they are far below timer resolution, and their shared property (no
    // `terrain_of`) is what the ceiling is guarding.
    #[allow(clippy::disallowed_types)] // benchmark harness
    let start = Instant::now();
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
    #[allow(clippy::disallowed_types)] // benchmark harness
    let small_ms = start.elapsed().as_secs_f64() * 1000.0;
    assert!(small_bytes > 0, "the small documents are non-empty");

    #[allow(clippy::disallowed_types)] // benchmark harness
    let start = Instant::now();
    for i in 0..REGION_TILES {
        let ix = (i as u32) % (1 << REGION_LEVEL);
        let scene =
            hornvale_scene::tiles_region_scene_in(&world, &ctx, 0, REGION_LEVEL, ix, 0, SAMPLES)
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
    println!("SceneContext::build {context_ms:9.1} ms (budget {CONTEXT_BUDGET_MS})");
    println!("tiles(512)+json    {tiles_ms:9.1} ms (budget {TILES_BUDGET_MS})");
    println!(
        "small docs+json    {small_ms:9.1} ms (budget {SMALL_DOCS_BUDGET_MS}) [{small_bytes} B]"
    );
    println!("region per tile    {per_tile_ms:9.1} ms (budget {REGION_PER_TILE_BUDGET_MS})");

    assert!(
        genesis_ms < GENESIS_BUDGET_MS,
        "genesis took {genesis_ms:.1} ms, over the {GENESIS_BUDGET_MS} ms ceiling"
    );
    assert!(
        context_ms < CONTEXT_BUDGET_MS,
        "SceneContext::build took {context_ms:.1} ms, over the {CONTEXT_BUDGET_MS} ms ceiling"
    );
    assert!(
        tiles_ms < TILES_BUDGET_MS,
        "tiles_scene(512)+json took {tiles_ms:.1} ms, over the {TILES_BUDGET_MS} ms ceiling"
    );
    assert!(
        small_ms < SMALL_DOCS_BUDGET_MS,
        "the four small scene documents took {small_ms:.1} ms, over the \
         {SMALL_DOCS_BUDGET_MS} ms ceiling — the likeliest cause is one of them \
         acquiring a terrain or climate derivation"
    );
    assert!(
        per_tile_ms < REGION_PER_TILE_BUDGET_MS,
        "tiles_region_scene took {per_tile_ms:.1} ms/tile, over the \
         {REGION_PER_TILE_BUDGET_MS} ms ceiling"
    );
}
