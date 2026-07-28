# The Sextant Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land a committed profiler for the client-facing scene APIs, shaped
like the Orrery's own session, plus falsification ceilings in the heavy tier
so the measured cost cannot silently regress.

**Architecture:** Two artifacts and a ledger row. An `examples/` binary in
`windows/scene` (the crate that owns the code) drives `build_world` once and
then the six scene documents plus a fan of region patches, printing a
per-operation table. A `#[ignore]`d battery in `cli/tests/` asserts each
measured cost stays under a documented ceiling. Neither commits a
timing-shaped artifact.

**Tech Stack:** Rust 2024, workspace crates only. No new dependencies — the
`serde` + `serde_json` allowlist (`cli/tests/architecture.rs`) is unchanged
by this campaign.

## Global Constraints

- **No new dependencies.** `windows/scene` already depends on
  `hornvale-worldgen`, `-astronomy`, `-terrain`, `-climate`
  (`windows/scene/Cargo.toml`). Nothing else may be added.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only
  (`clippy.toml` `disallowed-types`).
- **No wall-clock in the sim.** `std::time::Instant` is banned workspace-wide
  and needs a scoped `#[allow(clippy::disallowed_types)]` with a justifying
  comment at every use site. Copy the wording from
  `domains/terrain/examples/profile_terrain.rs:13-16` or
  `cli/tests/graph_cost.rs:64-70`.
- **`#![warn(missing_docs)]`** is set on every crate: every public item gets
  a one-line doc comment. (Examples and integration tests declare no public
  items, but module-level `//!` docs are the house style and both existing
  profilers have them.)
- **`cargo fmt` is the final step before every commit.** Fmt-gate skips are
  the most common review finding in this repo.
- **Heavy-tier reason string is verbatim or the gate fails.** Exactly:
  `heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full`
  — enforced by `cli/tests/heavy_tier.rs`.
- **Seed 42** is the canonical fixture seed throughout.

---

### Task 1: The scene profiler

**Files:**
- Create: `windows/scene/examples/profile_scene.rs`

**Interfaces:**
- Consumes: `hornvale_worldgen::build_world(seed, &SkyPins, SkyChoice,
  &TerrainPins, &SettlementPins) -> Result<World, BuildError>`
  (`windows/worldgen/src/lib.rs:5569`); `hornvale_scene::{tiles_scene,
  scene_json, system_scene, system_json, moons_scene, moons_json,
  neighbors_scene, neighbors_json, eclipses_scene, eclipses_json,
  tiles_region_scene, region_json}` (all at the crate root —
  `windows/scene/src/lib.rs:16` re-exports `region::*`).
- Produces: nothing importable. Task 2 depends only on the *numbers* this
  prints, not on any symbol.

- [ ] **Step 1: Write the example**

Create `windows/scene/examples/profile_scene.rs`:

```rust
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

/// A deterministic fan of `n` level-`REGION_LEVEL` tile addresses, walked
/// face-major so a small `n` still spans more than one cube face.
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
    small_bytes += hornvale_scene::system_json(
        &hornvale_scene::system_scene(&world).expect("system scene"),
    )
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
    println!("  {:<26} {per_tile_ms:9.1} ms  <-- the per-tile figure", "  ... per tile");
    println!("  {:<26} {total:9.1} ms", "TOTAL");
}
```

- [ ] **Step 2: Verify it compiles and runs**

Run: `cargo run --release -p hornvale-scene --example profile_scene -- 4`

`--release` is required: a debug run measures 30-90% higher and will not
match spec §1's reference figures, which were taken on an optimized build.

Expected: a table with six rows. On this box (`lefford`) the per-tile figure
should land near **700 ms** and `hw_new` near **1800–2100 ms**; the reference
scratch-harness figures are in the spec §1. If the per-tile figure is wildly
different (say under 100 ms), STOP — either the fix has landed already or the
workload is not doing what the spec measured, and the ceilings in Task 2
would be meaningless.

- [ ] **Step 3: Check fmt and clippy**

Run: `cargo fmt && cargo clippy -p hornvale-scene --all-targets -- -D warnings`
Expected: clean. If clippy flags an `Instant` use, the scoped allow is
missing at that site — add it with the justifying comment, do not widen an
existing allow to module scope.

- [ ] **Step 4: Record the measurement**

Run: `cargo run --release -p hornvale-scene --example profile_scene -- 8 2>&1 | tee /tmp/sextant-baseline.txt`

Keep this output. Task 2 quotes it verbatim in the test's module doc, and
the numbers become the ceilings' provenance.

- [ ] **Step 5: Commit**

```bash
git add windows/scene/examples/profile_scene.rs
git commit -m "feat(the-sextant): a committed profiler shaped like the Orrery's session

Drives genesis, the six scene documents, and a fan of region patches at
the Orrery's own LOD constants. The workload is consumer-shaped on
purpose: the per-call redundancy is only visible when region calls
repeat."
```

---

### Task 2: The cost ceilings

**Files:**
- Create: `cli/tests/scene_cost.rs`

**Interfaces:**
- Consumes: the measured numbers from Task 1 Step 4, and the same worldgen /
  scene entry points Task 1 uses.
- Produces: nothing importable.

**Reference pattern:** `cli/tests/graph_cost.rs` — read it before writing
this. It establishes the whole shape: measured numbers in the module doc,
budget constants carrying their own provenance, the scoped `Instant` allow,
and the verbatim ignore reason.

**PROFILE — read this before setting any number.** `make gate-full` runs the
heavy tier via `cargo nextest run --workspace --run-ignored only`
(`scripts/gate-full-heavy.sh:47`) — **no `--release`**. Heavy batteries
therefore execute in the **dev profile**, where `hornvale-kernel`,
`-terrain`, `-climate`, `-worldgen` and `-language` carry `opt-level = 2`
(root `Cargo.toml` `TOOL-hot-crate-opt`) but `hornvale-scene` does **not**.
`graph_cost.rs`'s 90 s budget is a dev-profile number for the same reason.

Consequence: **do not derive these ceilings from Task 1's `--release`
figures.** A release-based ceiling is roughly 2× too tight and the battery
will fail. Ceilings come from Step 2's own run of this test. Task 1's
release numbers are the campaign's *reference* measurement (they are what
spec §1 quotes) and go in the module doc clearly labelled as such — never as
the ceiling basis.

- [ ] **Step 1: Write the test**

Create `cli/tests/scene_cost.rs`. Leave every `<MEASURED-*>` as a deliberate
over-estimate for now (e.g. `100_000.0`) so the test compiles and runs;
Step 2 replaces them with real values. These are the only placeholders in
this plan and they exist because the values are the output of running the
test itself.

```rust
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
//! ## Measured (seed 42, this box, `cargo run -p hornvale-scene --example
//! profile_scene -- 8`)
//!
//! ```text
//! <PASTE THE TASK 1 STEP 4 OUTPUT VERBATIM HERE>
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
/// Measured <MEASURED-PER-TILE> ms/tile on <DATE>, host `lefford` (40 cores),
/// via `cargo run -p hornvale-scene --example profile_scene -- 8`.
///
/// **This ceiling is deliberately above a known-bad number.** ~91% of that
/// measurement is redundant re-derivation of terrain and climate (The
/// Sextant §1). The ceiling locks in "no worse than today" while the fix is
/// pending; the fix campaign ratchets it down. Budgeted at ~2x the
/// measurement — tight enough that doubling the per-call work trips it.
const REGION_PER_TILE_BUDGET_MS: f64 = <MEASURED-PER-TILE * 2, ROUNDED>;

/// Wall-time budget for one `hw_new`-equivalent `build_world` at
/// `BuildDepth::Full`, which is what the catalog's `hw_new` performs.
///
/// Measured <MEASURED-GENESIS> ms on <DATE>, host `lefford`.
/// Budgeted at ~2x.
const GENESIS_BUDGET_MS: f64 = <MEASURED-GENESIS * 2, ROUNDED>;

/// Wall-time budget for `tiles_scene(512)` — the globe base export — and its
/// JSON serialization, measured together.
///
/// Measured <MEASURED-TILES-BUILD> ms build + <MEASURED-TILES-JSON> ms json
/// on <DATE>, host `lefford`. Budgeted at ~2x the sum. Build and JSON are
/// summed rather than budgeted separately because they regress together: both
/// scale with `width`, the one knob that changes this document's size.
const TILES_BUDGET_MS: f64 = <(MEASURED-TILES-BUILD + MEASURED-TILES-JSON) * 2, ROUNDED>;

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
```

- [ ] **Step 2: Measure in the test's own profile, then set the ceilings**

Run: `cargo test -p hornvale --test scene_cost -- --ignored --nocapture`
Expected: PASS against the placeholder ceilings, printing three
measured/budget pairs. **These printed values are the ceiling basis.**

Run it three times and take the slowest of the three — this box runs
parallel campaign sessions, and a ceiling set from a quiet-box run will
flake under contention (`docs/timings.md`'s `cpu_ratio` column exists
because of exactly this).

Now set each constant to ~2× the slowest measured value, rounded to
something legible, and record in each constant's doc comment: the measured
value, the date, the host (`lefford`), and **"dev profile, as `gate-full`
runs it"**. Re-run once more and confirm PASS against the real ceilings.

For the module doc's `## Measured` block, record BOTH, labelled:
the dev-profile figures from this step (the ceiling basis) and Task 1's
release-profile figures from `/tmp/sextant-baseline.txt` (the campaign's
reference, matching spec §1).

- [ ] **Step 3: Falsify it — prove the ceiling can actually fire**

A budget test that cannot fail is worse than no test, because it reads as
coverage. Temporarily set `REGION_PER_TILE_BUDGET_MS` to `1.0`, re-run, and
confirm it FAILS with the "over the ... ceiling" message naming the real
measured value.

Run: `cargo test -p hornvale --test scene_cost -- --ignored --nocapture`
Expected: FAIL, message reads `tiles_region_scene took <real> ms/tile, over
the 1 ms ceiling`.

**Then restore the real budget and re-run to confirm PASS again.** Do not
commit the mutated constant.

- [ ] **Step 4: Confirm the heavy-tier string is exact**

Run: `cargo test -p hornvale --test heavy_tier`
Expected: PASS. This test scans the tree for `#[ignore]` reasons and fails if
the new one deviates from the canonical string by even a character.

- [ ] **Step 5: Confirm the commit gate is unaffected**

Run: `cargo nextest run -p hornvale 2>&1 | tail -20`
Expected: the new battery reports as skipped, not run.

- [ ] **Step 6: fmt, clippy, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add cli/tests/scene_cost.rs
git commit -m "test(the-sextant): falsification ceilings for the scene APIs

Heavy-tier battery pinning genesis, tiles(512), and per-tile region cost
on seed 42. Ceilings sit above today's measured values -- ~91% of the
region figure is redundant re-derivation -- so they lock in no-worse
while the fix is pending. Ceilings ratchet down freely; raising one is an
explicit reviewed act recorded in the constant's doc."
```

---

### Task 3: Close the campaign

**Files:**
- Create: `book/src/chronicle/the-sextant.md`
- Create: `docs/retrospectives/the-sextant.md`
- Modify: `book/src/SUMMARY.md` (add the chronicle entry)
- Modify: `docs/timings.md` (via `scripts/timed.sh`, not by hand)
- Create: `.superpowers/sdd/followups.md` (the deferred structural guard)

**Interfaces:** none — documentation and ledger only.

- [ ] **Step 1: Record a timed profiler run in the ledger**

The `--` separator is required (`scripts/timed.sh:16-17`: `scripts/timed.sh
<label> -- <command...>`). Note the run's own trailing `-- 8` is a second,
unrelated `--` passed through to cargo:

```bash
bash scripts/timed.sh scene-profile -- cargo run --release -p hornvale-scene --example profile_scene -- 8
```

Then confirm the row landed: `make timings LABEL=scene-profile`
Expected: one new row with `wall_s`, `user_s`, `sys_s`, `cpu_ratio`,
`commit`, `branch`, `host`, `cores`.

The ledger is explicitly **not** drift-checked and never gates the build
(`scripts/timed.sh:8-10`) — it is a record you read, which is exactly why
it is the right home for timings and a committed artifact is not.

- [ ] **Step 2: Write the followup register entry**

Create `.superpowers/sdd/followups.md` recording the deferred structural
guard, verbatim reasoning from spec §3.5: the "scene layer derives terrain at
most once per world" assertion has no seam to observe against today's
`&World`-taking signatures, and lands with the fix campaign. This is promoted
into the retrospective's follow-up section at close.

- [ ] **Step 3: Write the chronicle entry**

Create `book/src/chronicle/the-sextant.md` and add it to
`book/src/SUMMARY.md`. Book prose is "technical and mathematical,
comprehensible without reading the code it may show" (root `CLAUDE.md`).

Cover: the measurement, the 91% attribution, the two levers measured and
ruled out (`BuildDepth` 37 ms, `opt-level="z"` ~23%), and why the instrument
landed before the fix. **Cross-reference `the-frame-budget.md`** — that
chapter says the halo fix "lives in the world's generator, off-limits to a
client-only campaign", and this campaign is the producer-side counterpart to
it. Note explicitly that the client-side harness could not have found this:
it measured `buildTiles` correctly, and the redundancy is on the other side
of the wasm boundary.

- [ ] **Step 4: Freshness sweep**

Run: `grep -rn "tiles_region_scene\|scene API\|Orrery" book/src/ --include=*.md | grep -v chronicle/the-sextant`

Read every hit and fix any claim this campaign falsified. Check
`book/src/open-questions.md` for a Confidence Gradient bet this campaign
moves; if one is affected, re-score that chapter (decision 0030).

- [ ] **Step 5: Write the retrospective**

Create `docs/retrospectives/the-sextant.md` (decision 0020) — process
lessons, not product. Candidates observed during the spec: the initial
`tools/scene-bench` + criterion recommendation was overturned by searching
repo precedent; the obvious per-function workload would have missed the
finding entirely; and the strongest guard turned out to be coupled to the fix,
which was only discovered while writing the design.

- [ ] **Step 6: Full gate**

Run: `make gate`
Expected: green.

Run: `make gate-full`
Expected: green, including `scene_api_cost_is_bounded_on_seed_42`.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add book/src/chronicle/the-sextant.md book/src/SUMMARY.md \
        docs/retrospectives/the-sextant.md docs/timings.md
git commit -m "docs(the-sextant): close — chronicle, retro, timing ledger"
```

---

## Self-review

**Spec coverage.** §3.1 profiler → Task 1. §3.2 ceilings → Task 2. §3.3
ratchet discipline → Task 2 Step 1 (module doc + per-constant docs). §3.4
timing ledger and the no-committed-timings rule → Task 3 Step 1, and by
construction (nothing in Tasks 1–2 writes a file). §3.5 deferred structural
guard → Task 3 Step 2. §4 verification → distributed across Task 1 Steps 2–3,
Task 2 Steps 2–5, Task 3 Step 6. §6 acceptance criteria → all covered; the
book/chronicle criterion is Task 3 Steps 3–5.

**Placeholders.** The `<MEASURED-*>` slots in Task 2 are the only ones, and
they are unavoidable: the ceilings' values are the output of Task 1. Every
one names its source command. No "add error handling"-class vagueness.

**Type consistency.** `tiles_region_scene(&World, u32, u32, u32, u32, u32)`
and `region_json(&RegionScene)` are used identically in Tasks 1 and 2 and
match `windows/scene/src/region.rs:300` and `:411`. `build_world`'s five
arguments match `windows/worldgen/src/lib.rs:5569`. `SkyPins` and
`TerrainPins` are imported from the domain crates directly, as
`cli/tests/graph_cost.rs:56-58` does — **not** re-exported through
`hornvale_worldgen`, where they are private.

**Known risk.** Task 1's `tile_fan` returns `(face, ix, iy)` while Task 2
inlines its own simpler address walk. That is deliberate — the test does not
depend on the example — but it means the two measure slightly different tile
sets. Both stay at `REGION_LEVEL` with the same `SAMPLES`, and per-tile cost
is flat in address (spec §1: 687.3 / 700.5 / 701.8 ms across three different
fan sizes), so the ceiling transfers. If a future change makes per-tile cost
address-dependent, this assumption breaks and the two must be unified.
