# Lab Performance Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Cut lab and CI turnaround before Sculpting by pushing worldgen's projection down (build only to the rung a study's metrics read), tiering the CI drift regen, and adding two free-riding correctness batteries — every change provably byte-identical in its outputs.

**Architecture:** Four independent tracks. **A** (Rust worldgen+lab): a committed build profiler (Stage 1) proves the cost shares, then a view-typed build-depth ladder (Stage 2, "MAP-25") makes `build_world_to(depth)` stop the pipeline at a rung and hands each metric extractor the narrowest view type it reads, so under-building is a *compile error*. **B** (CI/infra, Stage 3): a `git diff` denylist tiers the expensive drift regen, and `flock` serializes concurrent local gates. **C** (correctness, Stage 4): O(1)-per-world invariants and a pin-reaffirmation battery ride worlds a census already builds.

**Tech Stack:** Rust (edition 2024), `serde`/`serde_json` only, the kernel's `Seed`/`Stream`, `std::thread::scope` (already the runner's parallel engine), GitHub Actions bash steps, `make`, `flock`.

## Global Constraints

Copied verbatim from the spec and CLAUDE.md; every task's requirements implicitly include this section.

- **Byte-identity is the acceptance gate.** Same seed + same pins → byte-identical worlds, metrics, and artifacts. A faster census that returns a different number is a regression, not a win. Every stage is judged against a byte-identity guard, not an assertion.
- **Dependencies:** `serde` + `serde_json` only, workspace-wide. No new crates (no rand, chrono, clap, thiserror). CLI/scripts are std-only / bash-only.
- **No `HashMap`/`HashSet`** anywhere — `BTreeMap`/`BTreeSet`/`Vec` only. Float sorting uses `total_cmp`.
- **No wall-clock time in the sim.** `std::time::Instant` and `std::time::SystemTime` are banned by `clippy.toml`. The profiler measures wall-clock durations by definition, so its `Instant` uses carry a scoped `#[allow(clippy::disallowed_types)]` with a one-line comment justifying it (diagnostic-only, never touches `WorldTime` or facts). This is the *only* sanctioned `Instant` in the tree.
- **Layering (enforced by `cli/tests/architecture.rs`):** `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain never depends on another domain or on a window. `windows/worldgen` is the composition root. Terrain-internal instrumentation (Stage 1 Task 5) lives *inside* `domains/terrain` — it must not reach up to worldgen.
- **Every crate sets `#![warn(missing_docs)]`;** every public item, field, and variant gets a one-line doc comment.
- **`cargo fmt` is the final step before every commit.** The full gate is `cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings && cargo test --workspace`. Iterate cost-ordered (fmt/clippy first, scope tests to the changed crate); the workspace gate is the final pre-commit step, not every intermediate run.
- **Quantization stays at the emit boundary only** (`Ledger::commit`, `render_csv`, scene/ephemeris JSON). Never move it into the compute path; never introduce a new serialization boundary without quantizing it.
- **Save-format contracts are frozen:** stream-consumption order, seed-derivation labels, noise/hash constants, physics formulas. Nothing in this campaign changes what facts a *full* build commits or in what order — the depth ladder only stops *early*, committing a byte-identical prefix of the same facts.

---

## Stage sequencing

The four stages are independent (they share no state) and **B** and **C** can be dispatched in parallel with **A**. But within **A**, **Stage 1 gates Stage 2**: Stage 1's profile answers two questions whose answers change Stage 2's shape (the astronomy-split and the `strongest()`-collapse). Do not begin Stage 2's conditional tasks until Stage 1's readout (Stage 1 Task 6) is recorded.

**Merge note (read before executing):** at time of writing, main's CI is red from a pre-existing cross-platform divergence bug (out of this campaign's scope). Branch work and local gates proceed regardless, but **do not merge to main until main is green** — the campaign absorbs main at each plan-stage boundary and a red main poisons that. Surface this at the first stage boundary.

---

# Track A — Stage 1: The profiler

**Goal:** a committed, flag-gated build profiler; per-stage and terrain-internal cost shares recorded; the astronomy-split and `strongest()`-collapse questions answered with numbers.

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (add the profiler sink + stage spans)
- Create: `windows/worldgen/examples/profile_build.rs`
- Create: `domains/terrain/examples/profile_terrain.rs`
- Test: `windows/worldgen/tests/profile.rs`
- Modify: `docs/superpowers/plans/2026-07-12-lab-performance.md` (record the readout in Stage 1 Task 6)

**Interfaces:**
- Produces: `hornvale_worldgen::BuildProfile { stages: Vec<(&'static str, Duration)> }`; `hornvale_worldgen::profiled<T>(f: impl FnOnce() -> T) -> (T, BuildProfile)`; the five stage labels `"astronomy"`, `"terrain"`, `"climate+settlements"`, `"culture+religion+species"`, `"deep-time"`.

### Task 1: Profiler sink (thread-local, zero normal-path cost)

**Files:**
- Modify: `windows/worldgen/src/lib.rs`
- Test: `windows/worldgen/tests/profile.rs`

- [ ] **Step 1: Write the failing test**

```rust
// windows/worldgen/tests/profile.rs
//! The build profiler records per-stage spans without altering the build.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_worldgen::{BuildProfile, SettlementPins, SkyChoice, build_world, profiled};
use hornvale_terrain::TerrainPins;

#[test]
fn profiled_records_the_five_stages() {
    let (world, profile): (_, BuildProfile) = profiled(|| {
        build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 42 builds")
    });
    let labels: Vec<&str> = profile.stages.iter().map(|(l, _)| *l).collect();
    assert_eq!(
        labels,
        vec![
            "astronomy",
            "terrain",
            "climate+settlements",
            "culture+religion+species",
            "deep-time",
        ]
    );
    // The build still produced a real world.
    assert!(world.ledger.len() > 0);
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-worldgen --test profile`
Expected: FAIL — `BuildProfile` / `profiled` unresolved.

- [ ] **Step 3: Implement the sink**

In `windows/worldgen/src/lib.rs`, add near the top of the module:

```rust
use std::cell::RefCell;
// The profiler measures wall-clock stage durations for a committed diagnostic
// (`profile_build` example); it never reads `WorldTime` and never touches a
// fact, so it is exempt from the wall-clock ban (clippy.toml / decision 0001).
#[allow(clippy::disallowed_types)]
use std::time::{Duration, Instant};

thread_local! {
    // `Some` only inside a `profiled(..)` scope; `None` on the normal build
    // path, so the stage spans compile to a single thread-local read + branch.
    static PROFILE: RefCell<Option<Vec<(&'static str, Duration)>>> = const { RefCell::new(None) };
}

/// Per-stage wall-clock spans recorded during one profiled world build.
/// type-audit: bare-ok(prose: stages)
#[derive(Clone, Debug, Default)]
pub struct BuildProfile {
    /// `(stage label, elapsed)` in pipeline order.
    pub stages: Vec<(&'static str, Duration)>,
}

/// Run `f` with stage profiling active and return its result plus the profile.
/// Nesting is not supported (the inner scope's spans replace the outer's); the
/// lab/CLI call it at the top of one build.
pub fn profiled<T>(f: impl FnOnce() -> T) -> (T, BuildProfile) {
    PROFILE.with(|p| *p.borrow_mut() = Some(Vec::new()));
    let out = f();
    let stages = PROFILE.with(|p| p.borrow_mut().take().unwrap_or_default());
    (out, BuildProfile { stages })
}

/// Record `label` with the time `f` took, but only when a `profiled` scope is
/// active. Off the profiled path this is one thread-local read and a call.
#[allow(clippy::disallowed_types)]
fn stage<T>(label: &'static str, f: impl FnOnce() -> T) -> T {
    let active = PROFILE.with(|p| p.borrow().is_some());
    if !active {
        return f();
    }
    let start = Instant::now();
    let out = f();
    let elapsed = start.elapsed();
    PROFILE.with(|p| {
        if let Some(v) = p.borrow_mut().as_mut() {
            v.push((label, elapsed));
        }
    });
    out
}
```

- [ ] **Step 4: Run to verify the test now fails only on the missing stages**

Run: `cargo test -p hornvale-worldgen --test profile`
Expected: FAIL — labels vec is empty (spans not wired yet). This confirms `profiled` compiles and runs; Task 2 wires the spans.

- [ ] **Step 5: Commit**

```bash
git add windows/worldgen/src/lib.rs windows/worldgen/tests/profile.rs
git commit -m "feat(worldgen): profiler sink (thread-local, off-path zero cost)"
```

### Task 2: Wire the five stage spans

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (`build_world_with_roster`, lines ~1627–1935)

- [ ] **Step 1: Wrap each pipeline stage in `stage(..)`**

Wrap the existing pipeline segments in `build_world_with_roster` — do **not** move or reorder any statement; only wrap. The five spans, by current line ranges:

1. **`"astronomy"`** — the `if let SkyChoice::Generated = sky { generate(..); facts::genesis(..) }` block (~1627–1630).
2. **`"terrain"`** — the terrain-pin commits through `hornvale_terrain::facts::genesis(..)` (~1632–1645).
3. **`"climate+settlements"`** — from the `terrain_of`/`climate_of` reconstruction through the name-gloss commit loop (~1664–1845).
4. **`"culture+religion+species"`** — the per-species culture/religion loop plus `hornvale_species::genesis_in`/`people` (~1847–1930).
5. **`"deep-time"`** — the `paleoclimate_of` + `hornvale_paleoclimate::genesis` block (~1932–1935).

Because `stage(..)` takes a closure that returns the block's value and each block already ends with `?`-propagating statements, wrap each as e.g.:

```rust
// Stage 2 example — terrain. `stage` returns the closure's Result, then `?`.
stage("terrain", || -> Result<(), BuildError> {
    for pin_string in hornvale_terrain::pin_strings(terrain_pins) {
        world.ledger.commit(
            scenario_fact(world_entity, hornvale_terrain::facts::TERRAIN_PIN, Value::Text(pin_string)),
            &world.registry,
        )?;
    }
    let level = terrain_pins.globe_level.unwrap_or(GLOBE_LEVEL);
    let terrain_outcome = hornvale_terrain::generate(seed, &geosphere_for(level), terrain_pins)
        .map_err(BuildError::TerrainGenesis)?;
    hornvale_terrain::facts::genesis(&mut world, world_entity, &terrain_outcome)?;
    Ok(())
})?;
```

Apply the same wrapping to the other four segments. Segments that produce values later statements need (e.g. `terrain`, `climate`, `placements`, `ids`) must return those values *out* of the closure so the outer bindings still see them — e.g. `let (terrain, climate, ids, placed, placements) = stage("climate+settlements", || { ... Ok((terrain, climate, ids, placed, placements)) })?;`. Keep the borrow structure identical; the closure is a pure lexical wrapper.

- [ ] **Step 2: Run the profiler test**

Run: `cargo test -p hornvale-worldgen --test profile`
Expected: PASS — the five labels appear in order.

- [ ] **Step 3: Prove byte-identity of the normal path is untouched**

Run: `cargo test -p hornvale-worldgen`
Expected: PASS — every existing determinism/golden test in worldgen still passes (the wrapping changed no facts). If `proto_goblinoid_golden` or any determinism test fails, a statement moved semantically — revert and re-wrap without reordering.

- [ ] **Step 4: fmt + clippy**

Run: `cargo fmt && cargo clippy -p hornvale-worldgen --all-targets -- -D warnings`
Expected: clean (the scoped `#[allow]` silences the `Instant`/`Duration` bans).

- [ ] **Step 5: Commit**

```bash
git add windows/worldgen/src/lib.rs
git commit -m "feat(worldgen): time the five build stages under the profiler"
```

### Task 3: `profile_build` aggregator example

**Files:**
- Create: `windows/worldgen/examples/profile_build.rs`

- [ ] **Step 1: Write the example**

```rust
//! Committed build profiler (spec §7). Runs a representative seed sample,
//! sums each stage's wall-clock time, and prints per-stage shares. Kept in
//! the tree so the profile stays honest as later epochs add stages.
//!
//! Run: `cargo run -p hornvale-worldgen --example profile_build -- [SAMPLE]`
//! (SAMPLE defaults to 24 seeds starting at 0.)

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{BuildProfile, SettlementPins, SkyChoice, build_world, profiled};

fn main() {
    let sample: u64 = std::env::args()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(24);

    let mut totals: Vec<(&'static str, f64)> = Vec::new();
    for seed in 0..sample {
        let (result, profile): (_, BuildProfile) = profiled(|| {
            build_world(
                Seed(seed),
                &SkyPins::default(),
                SkyChoice::Generated,
                &TerrainPins::default(),
                &SettlementPins::default(),
            )
        });
        if result.is_err() {
            continue; // a genesis refusal contributes no timing
        }
        for (label, dur) in profile.stages {
            let secs = dur.as_secs_f64();
            match totals.iter_mut().find(|(l, _)| *l == label) {
                Some((_, acc)) => *acc += secs,
                None => totals.push((label, secs)),
            }
        }
    }

    let grand: f64 = totals.iter().map(|(_, s)| *s).sum();
    println!("build profile over {sample} seeds (total {grand:.3}s):");
    for (label, secs) in &totals {
        let share = if grand > 0.0 { 100.0 * secs / grand } else { 0.0 };
        println!("  {label:<28} {secs:8.3}s  {share:5.1}%");
    }
}
```

- [ ] **Step 2: Run it**

Run: `cargo run -p hornvale-worldgen --example profile_build -- 24`
Expected: prints five rows with shares summing to ~100%. Record the output for Task 6.

- [ ] **Step 3: Commit**

```bash
git add windows/worldgen/examples/profile_build.rs
git commit -m "feat(worldgen): profile_build example — per-stage cost shares"
```

### Task 4: Terrain-internal micro-profiler (answers the `strongest()`-collapse question)

**Files:**
- Create: `domains/terrain/examples/profile_terrain.rs`

**Interfaces:**
- Consumes: the terrain crate's public build entry (`hornvale_terrain::generate`) and per-cell readers (`thickness_at`, `age_at`, `continental_at` on `GeneratedTerrain`). Confirm exact names with `grep -n "pub fn" domains/terrain/src/lib.rs domains/terrain/src/crust.rs` before writing; adjust the calls to match.

- [ ] **Step 1: Write the example**

The `strongest()`-collapse hypothesis (spec §4 conditional sibling): `thickness_at`, `age_at`, and `continental_at` each recompute the full per-cell craton sweep, so a cell read by all three pays up to 3×. This example times (a) a full terrain `generate`, and (b) the triple-read vs a single hypothetical sweep, over every cell, to decide whether the collapse is material.

```rust
//! Terrain-internal micro-profiler (spec §4 / Stage 1 Task 4). Answers: is
//! the `strongest()` triple-recompute (thickness_at, age_at, continental_at)
//! a material share of the terrain stage? Stays in-domain (no worldgen dep).
//!
//! Run: `cargo run -p hornvale-terrain --example profile_terrain -- [SAMPLE]`

// The profiler measures wall-clock durations for a committed diagnostic only.
#[allow(clippy::disallowed_types)]
use std::time::Instant;

use hornvale_kernel::Seed;
use hornvale_terrain::{TerrainPins, generate, geosphere_for_level}; // confirm exact names

fn main() {
    let sample: u64 = std::env::args().nth(1).and_then(|s| s.parse().ok()).unwrap_or(8);
    let pins = TerrainPins::default();
    let level = pins.globe_level.unwrap_or(6); // confirm the GLOBE_LEVEL default

    let (mut gen_secs, mut triple_secs) = (0.0_f64, 0.0_f64);
    for seed in 0..sample {
        let geo = geosphere_for_level(level); // confirm the geosphere constructor name
        #[allow(clippy::disallowed_types)]
        let t0 = Instant::now();
        let terrain = generate(Seed(seed), &geo, &pins).expect("terrain builds");
        gen_secs += t0.elapsed().as_secs_f64();

        // Triple read: every cell through all three craton-sweeping readers.
        #[allow(clippy::disallowed_types)]
        let t1 = Instant::now();
        let mut sink = 0.0_f64;
        for cell in terrain.geosphere().cells() {
            sink += terrain.thickness_at(cell);
            sink += terrain.age_at(cell);
            sink += if terrain.continental_at(cell) { 1.0 } else { 0.0 };
        }
        std::hint::black_box(sink);
        triple_secs += t1.elapsed().as_secs_f64();
    }
    println!("terrain profile over {sample} seeds:");
    println!("  generate            {gen_secs:8.3}s");
    println!("  triple per-cell read {triple_secs:8.3}s  ({:.1}% of generate)",
        if gen_secs > 0.0 { 100.0 * triple_secs / gen_secs } else { 0.0 });
    println!("(If the triple-read share is large, the single-sweep collapse is worth Stage 2 Task 8.)");
}
```

- [ ] **Step 2: Adjust names to match the crate, then run**

Run: `grep -n "pub fn" domains/terrain/src/lib.rs domains/terrain/src/crust.rs` and fix the imports/calls, then `cargo run -p hornvale-terrain --example profile_terrain -- 8`.
Expected: prints the generate time and the triple-read share. Record for Task 6.

- [ ] **Step 3: Commit**

```bash
git add domains/terrain/examples/profile_terrain.rs
git commit -m "feat(terrain): micro-profiler for the strongest() triple-read"
```

### Task 5: Full-gate green

- [ ] **Step 1:** Run `make gate` (fmt + clippy + `cargo test --workspace`). Expected: PASS.
- [ ] **Step 2:** Commit any fmt fixups.

### Task 6: Readout — answer the three questions (the fail-fast gate)

**Files:**
- Modify: this plan (append a "## Stage 1 readout" block at the end) **and** `docs/superpowers/specs/2026-07-12-lab-performance-design.md` (record the numbers next to §7's three questions).

- [ ] **Step 1:** Run both profilers over a representative sample (≥24 seeds for `profile_build`, ≥8 for `profile_terrain`; release mode for realistic shares: append `--release`).
- [ ] **Step 2:** Record the numbers and decide, explicitly:
  1. **MAP-25 premise:** is `climate+settlements` + the tail really the dominant share for a terrain census (i.e. is terrain a small fraction of a full build)? If terrain already dominates, the ladder's win shrinks — reconsider Stage 2's scope before building it.
  2. **Astronomy-split:** is `astronomy` cheap next to `terrain`? If yes (expected), keep the ladder strictly linear (Stage 2 builds astronomy before terrain). If astronomy is a material share, add the astronomy-as-incomparable-branch task to Stage 2.
  3. **`strongest()`-collapse:** is the triple-read share (Task 4) material? If yes, Stage 2 Task 8 (single-sweep collapse) is in; if not, drop it.
- [ ] **Step 3:** Write the three decisions into the plan and spec. **This is the gate: do not start Stage 2 until this is recorded.**
- [ ] **Step 4: Commit**

```bash
git add docs/superpowers/plans/2026-07-12-lab-performance.md docs/superpowers/specs/2026-07-12-lab-performance-design.md
git commit -m "docs(lab-perf): Stage 1 readout — cost shares + Stage 2 decisions"
```

## Stage 1 Readout (recorded 2026-07-12)

**Measured** (release; `profile_build` 24 seeds / `profile_terrain` 8 seeds). Debug shares matched within <1pt, so these are robust:

```
build profile (release, 24 seeds, total 63.571s):
  astronomy                  0.000s    0.0%
  terrain                   15.548s   24.5%
  climate+settlements       31.493s   49.5%
  culture+religion+species   0.014s    0.0%
  deep-time                 16.515s   26.0%

terrain micro (release, 8 seeds):
  generate                   4.470s
  triple per-cell read       0.868s   19.4% of generate
```

**Decision 1 — MAP-25 premise: VALIDATED (but reframed).** A terrain census builds to Terrain depth = astronomy+terrain = **24.5%** of a full build; it skips **~75%** (climate+settlements 49.5% + deep-time 26.0%). The spec §7's "dominated by the language tail" wording is **wrong**: `culture+religion+species` is ~0% — the per-species lexicon build is folded into `climate+settlements` (naming), and the real skippable mass is climate+settlements + **deep-time (26%)**. Consequence: the ladder's big lever is confirmed, *and* the distinct **Settlements rung earns its place** — a settlement census skips deep-time's quarter. Stage 2 proceeds as designed.

**CORRECTION (2026-07-12, discovered during Task 5 review) — WHICH studies win.** The readout above (and spec §4) wrongly implied `census-lands-drift` builds terrain-only. It does not: `census-lands-drift` selects `"metrics": "all"` (500 seeds), so it includes Full-rung metrics (`belief-kind`, pantheons, …) and its required depth is **Full** — MAP-25 gives it **no speedup**. The runner builds one world per seed to the study's *deepest selected metric's* rung, so the win lands only on studies that select a shallow metric **subset**:
- **`census-of-skies`** — 14 metrics, all **Astronomy**, **10,000 seeds** → builds to Astronomy depth ≈ **0%** of a full build. The dominant win: ~10k near-free builds instead of ~10k full builds.
- **`census-of-coasts`** (7 Terrain metrics, 10,000 seeds) and **`census-of-coasts-tuning`** (7 Terrain, 2,000 seeds) → Terrain depth ≈ 24.5% → **~4×**.
- Every `"all"` study (incl. `census-lands-drift`, `census-of-the-meeting`, `branches-family` — the three the CI drift-regen runs) builds **Full** and is unaffected by MAP-25; their CI cost is addressed by **Stage 3's path-tiering** instead, not the depth ladder. This is complementary, not a loss: MAP-25 targets the big 10k author-time subset censuses; Stage 3 targets the CI regen.
- **Considered and declined:** re-scoping `census-lands-drift` to terrain-only metrics would earn it the ~4× *and* make it a focused terrain census, but it would change the study's committed `rows.csv` (fewer columns) — a census-output change this campaign's byte-identity mandate forbids. A study-design decision for its owner, out of scope here.
- **Guard implication:** Task 6's metamorphic guard has real teeth only on the subset studies (`census-of-skies` at Astronomy, `census-of-coasts` at Terrain), where depth-scoped ≠ trivially-Full — it MUST include both.

**Decision 2 — Astronomy-split: DROP (keep ladder strictly linear).** Astronomy is **0.000s** — free. A terrain census paying one genesis costs nothing. **Stage 2 Task 8 is removed.**

**Decision 3 — `strongest()`-collapse: DROP (recommended; Nathan may override).** The triple-read is 19.4% of the terrain stage *worst case*, but production's `globe::generate` does only a **double** read per cell (`thickness_at`+`continental_at`; `age_at` is unused), ≈13%. Terrain is 24.5% of a full build, so the collapse saves ≈13% × 24.5% ≈ **3% of a full build** (≈13% of a terrain-only census). The readers live on `crust::CrustField` (keyed by raw point), **not** `GeneratedTerrain` as Task 7 sketched, so the collapse would restructure byte-identity-critical *frozen* crust code for a modest gain. Not worth the risk relative to the depth-ladder win. **Stage 2 Task 7 is dropped.**

**Bonus finding — `pin_enumeration` is a free rider on MAP-25.** The `windows/worldgen/tests/pin_enumeration.rs` test (~505s debug, the gate's single elephant) does ~96 *full* builds to check sky+terrain **pin isolation** — pins that only touch astronomy+terrain. Two clean, byte-identity-preserving wins fall out: **depth-scope** it to `BuildDepth::Terrain` (~4×, a one-line change once Stage 2 Task 2 lands — added as **Task 10** below) and **parallelize** the 48 independent combos with `std::thread::scope` (~cores×, available now — landed as a standalone Stage-1 sidecar commit). Together ≈505s → ≈20-30s without weakening the determinism guarantee or the cross-product coverage. (Two further axes — single-build+golden, and sum-not-product coverage — were considered and **declined**: each trades a guarantee away.)

---

# Track A — Stage 2: MAP-25 view-typed build-depth ladder

**Goal:** `build_world_to(depth)` stops the pipeline at a rung; each metric extractor takes the narrowest view type it reads; the runner builds once to the deepest selected metric's rung. Under-building is a compile error; the metamorphic guard is the runtime backstop.

**Prerequisite:** Stage 1 Task 6 recorded. If Stage 1 disproved the premise, revisit this stage's scope first.

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (add `BuildDepth` + `build_world_to`; refactor `build_world_with_roster` to stop early)
- Modify: `windows/lab/src/metrics.rs` (narrowed view types + coercions; retype every extractor; `Metric` carries a rung)
- Modify: `windows/lab/src/runner.rs` (`build_row` builds to the study's required depth and dispatches)
- Test: `windows/lab/tests/depth_ladder.rs` (metamorphic guard)

**Interfaces:**
- Produces: `hornvale_worldgen::BuildDepth { Astronomy, Terrain, Settlements, Full }` (Climate is a *view* rung, not a build stop — climate commits no facts, it is reconstructed from the terrain-depth world); `build_world_to(seed, pins, sky, terrain_pins, settlement_pins, roster, depth) -> Result<World, BuildError>`.
- In lab: `AstronomyView`, `TerrainView`, `ClimateView`, `SettlementView`, `FullView` in a subset chain with `AsRef` coercions; `enum Extractor` tagging each metric's rung; `Metric::rung() -> BuildDepth`.

### Task 1: Enumerate every metric's rung (the primary control)

**Files:**
- Create: `docs/superpowers/plans/lab-perf-rung-map.md` (the enumeration; deleted at Stage 2 close once encoded in types)

The spec (§4) requires the exact rung of *every* metric in `windows/lab/src/metrics.rs` be reviewed explicitly, not inferred — a metric mapped too shallow is the one hazard this design eliminates. The rung is *which WorldView field the extractor reads*:

| Reads | Rung |
|---|---|
| `v.system`, `v.calendar`, `v.notes` only | **Astronomy** |
| `v.globe`, `v.terrain` (+ astronomy) | **Terrain** |
| `v.climate` (+ terrain) | **Climate** |
| `v.world` for settlement/culture facts (`flagship_of`, `places`, `castes_of`, `POPULATION`, `CELL_ID`, `subsistence_of`) | **Settlements** |
| `v.world` for religion/language/species facts (`beliefs_*`, `pantheon_sig`, `lexicon_*`, `phonotactic_*`, `homophony_*`, `hue_depth`, name-gloss) | **Full** |

- [ ] **Step 1:** For each metric in the registry (`registry()` in `windows/lab/src/metrics.rs`, ~90 entries), record `name → rung` in the enumeration file by reading its `extract` closure and the helper it calls. Metrics that call helpers (`pantheon_sig`, `flagship_surplus`, `phonotactic_validity`, `lexicon_regular`, …) inherit the deepest field their helper reads — read each helper once.
- [ ] **Step 2:** Sanity-check the buckets: every `star-*`/`*-neighbor`/`moons-*`/`obliquity-*`/`year-*`/`day-*`/`tide`/`month`/`belief-kind`/`genesis-note-count` metric is Astronomy; every `*-continent*`/`plate-*`/`ocean-fraction`/`mountain-coverage`/`unrest-coverage`/`endorheic-*`/`shoreline-*`/`hypsometric-*`/`shelf-*`/`landmass-*` metric is Terrain; `band-count`/`habitable-fraction`/`dominant-land-biome`/`mean-land-temperature-c` are Climate; `settlement-count`/`mean-population`/`flagship-*` (subsistence/biome/coastal/structure/population/surplus)/`*-settlement-count` are Settlements; everything religion/language (`pantheon-*`/`cult-form-*`/`head-deity-*`/`*-flagship-roles`/`blind-attribution-*`/`phonotactic-*`/`epithet-*`/`name-length-*`/`name-collision-rate`/`name-gloss-true`/`lexicon-*`/`exposure-*`/`hue-depth-*`/`monophyly-*`/`clean-outgroup-*`/`inventory-closure-*`/`divergence-*`/`homophony-*` and the functional-load block) is Full.
- [ ] **Step 3: Commit** the enumeration.

```bash
git add docs/superpowers/plans/lab-perf-rung-map.md
git commit -m "docs(lab-perf): per-metric rung enumeration (Stage 2 control)"
```

### Task 2: `BuildDepth` + `build_world_to` (stop the pipeline early)

**Files:**
- Modify: `windows/worldgen/src/lib.rs`
- Test: `windows/worldgen/tests/depth.rs`

- [ ] **Step 1: Write the failing test — a shallow build is a fact-prefix of the full build**

```rust
// windows/worldgen/tests/depth.rs
//! `build_world_to(depth)` commits a byte-identical *prefix* of the full
//! build's facts — stopping early never changes a fact it does commit.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{BuildDepth, SettlementPins, SkyChoice, build_world, build_world_to};

fn shallow(depth: BuildDepth) -> hornvale_kernel::World {
    build_world_to(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &hornvale_worldgen::default_roster(),
        depth,
    )
    .expect("seed 42 builds")
}

#[test]
fn terrain_depth_is_a_prefix_of_full() {
    let full = build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap();
    let terrain = shallow(BuildDepth::Terrain);
    // Every fact the terrain-depth world committed is present, byte-identical,
    // in the full world (same subject/predicate/object/day), and there are
    // strictly fewer of them.
    assert!(terrain.ledger.len() < full.ledger.len());
    let full_json = serde_json::to_string(&full).unwrap();
    let terrain_facts = serde_json::to_string(&terrain.ledger).unwrap();
    // Structural prefix check: reserialize the terrain ledger's facts and
    // assert each appears in the full serialization. (Exact helper depends on
    // Ledger's API; use whatever fact-iteration the ledger exposes.)
    let _ = (full_json, terrain_facts);
}
```

Refine the prefix assertion to the ledger's actual fact-iteration API (`grep -n "pub fn" kernel/src/ledger.rs`); the load-bearing claim is *facts committed at depth D equal the first N facts of the full build*.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-worldgen --test depth`
Expected: FAIL — `BuildDepth` / `build_world_to` unresolved.

- [ ] **Step 3: Add `BuildDepth` and refactor the pipeline to stop early**

Add the enum:

```rust
/// How deep to build the world's fact-committing pipeline. Earlier rungs are
/// a byte-identical prefix of later ones (the pipeline is linear: a rung reads
/// only earlier rungs' facts). Climate is *not* a rung — it commits no facts;
/// it is reconstructed on demand from a Terrain-depth world.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum BuildDepth {
    /// Sky genesis only.
    Astronomy,
    /// …plus terrain genesis.
    Terrain,
    /// …plus settlement placement, naming, and glosses.
    Settlements,
    /// …plus culture, religion, species, and deep time (today's full build).
    Full,
}
```

Refactor `build_world_with_roster` so its body is `build_to(.., BuildDepth::Full)`. Extract an inner `fn build_to(...) -> Result<World, BuildError>` that runs the same statements but returns `Ok(world)` at each stop:

- after the `"astronomy"` stage if `depth == Astronomy`;
- after the `"terrain"` stage if `depth <= Terrain`;
- after the `"climate+settlements"` stage (the name-gloss commit loop, ~line 1845) if `depth <= Settlements`;
- otherwise run culture/religion/species/deep-time and return at the end.

Guard with `if depth >= BuildDepth::Settlements { … }` around the settlement block and `if depth == BuildDepth::Full { … }` around the tail. **Preserve every statement's order and borrows** — the only change is early `return Ok(world)` points. Expose:

```rust
/// Build a world only as deep as `depth` (spec §4 / MAP-25). At any depth the
/// committed facts are a byte-identical prefix of the full build's.
#[allow(clippy::too_many_arguments)]
pub fn build_world_to(
    seed: Seed,
    pins: &SkyPins,
    sky: SkyChoice,
    terrain_pins: &TerrainPins,
    settlement_pins: &SettlementPins,
    roster: &[hornvale_species::SpeciesDef],
    depth: BuildDepth,
) -> Result<World, BuildError> {
    build_to(seed, pins, sky, terrain_pins, settlement_pins, roster, depth)
}
```

and make `build_world_with_roster` delegate with `BuildDepth::Full`.

- [ ] **Step 4: Run the depth + determinism tests**

Run: `cargo test -p hornvale-worldgen`
Expected: PASS — the prefix test passes and every existing determinism/golden test still passes (Full delegates to the same statements in the same order).

- [ ] **Step 5: Commit**

```bash
git add windows/worldgen/src/lib.rs windows/worldgen/tests/depth.rs
git commit -m "feat(worldgen): build_world_to(depth) — stop the pipeline at a rung"
```

### Task 3: Narrowed view types + coercions

**Files:**
- Modify: `windows/lab/src/metrics.rs`

- [ ] **Step 1: Define the nested view chain**

Replace the single `WorldView` with a subset chain. Each deeper view *contains* the shallower one, so coercion is a field borrow (no recompute):

```rust
/// Astronomy rung: star system, calendar, genesis notes.
pub struct AstronomyView {
    /// The world ledger (astronomy-depth facts).
    pub world: World,
    /// The reconstructed or constant star system.
    pub system: StarSystem,
    /// The calendar derived from the system.
    pub calendar: Calendar,
    /// Genesis notes recorded during sky generation.
    pub notes: Vec<String>,
    /// The species roster this view was built from.
    pub roster: Vec<hornvale_species::SpeciesDef>,
}

/// Terrain rung: astronomy + the tectonic globe.
pub struct TerrainView {
    /// The astronomy rung this view extends.
    pub astronomy: AstronomyView,
    /// The tectonic globe summary.
    pub globe: GlobeSummary,
    /// The full tectonic globe.
    pub terrain: hornvale_terrain::GeneratedTerrain,
}

/// Climate rung: terrain + reconstructed climate (no extra facts).
pub struct ClimateView {
    /// The terrain rung this view extends.
    pub terrain: TerrainView,
    /// The derived climate.
    pub climate: GeneratedClimate,
}

/// Settlement rung: climate + a world built to settlement depth.
pub struct SettlementView {
    /// The climate rung this view extends.
    pub climate: ClimateView,
}

/// Full rung: a world built to full depth (culture/religion/species/deep-time).
pub struct FullView {
    /// The settlement rung this view extends.
    pub settlement: SettlementView,
}
```

Note `world` lives on `AstronomyView` so every rung can reach the ledger it was built to — but a *shallow* view's `world` only holds that rung's facts, so a religion metric handed a `SettlementView` cannot be *written* (it takes `&FullView`), and a settlement metric on a `SettlementView` reads a world whose religion facts do not exist. The type enforces the write-side boundary; the metamorphic guard (Task 6) is the read-side backstop for the two ledger-backed rungs that share `world` shape.

Add `AsRef` coercions down the chain (deeper → shallower is free):

```rust
impl AsRef<AstronomyView> for TerrainView {
    fn as_ref(&self) -> &AstronomyView { &self.astronomy }
}
impl AsRef<TerrainView> for ClimateView {
    fn as_ref(&self) -> &TerrainView { &self.terrain }
}
impl AsRef<AstronomyView> for ClimateView {
    fn as_ref(&self) -> &AstronomyView { self.terrain.as_ref() }
}
impl AsRef<ClimateView> for SettlementView {
    fn as_ref(&self) -> &ClimateView { &self.climate }
}
impl AsRef<TerrainView> for SettlementView {
    fn as_ref(&self) -> &TerrainView { self.climate.as_ref() }
}
impl AsRef<AstronomyView> for SettlementView {
    fn as_ref(&self) -> &AstronomyView { self.climate.as_ref() }
}
impl AsRef<SettlementView> for FullView {
    fn as_ref(&self) -> &SettlementView { &self.settlement }
}
impl AsRef<ClimateView> for FullView {
    fn as_ref(&self) -> &ClimateView { self.settlement.as_ref() }
}
impl AsRef<TerrainView> for FullView {
    fn as_ref(&self) -> &TerrainView { self.settlement.as_ref() }
}
impl AsRef<AstronomyView> for FullView {
    fn as_ref(&self) -> &AstronomyView { self.settlement.as_ref() }
}
```

- [ ] **Step 2: Constructors that build to each rung**

```rust
impl AstronomyView {
    pub fn build(seed: Seed, pins: &SkyPins, roster: Vec<hornvale_species::SpeciesDef>)
        -> Result<AstronomyView, BuildError> { Self::build_to(seed, pins, roster, BuildDepth::Astronomy) }
    // internal: build world to `depth`, reconstruct system/calendar/notes.
    fn build_to(seed, pins, roster, depth) -> Result<AstronomyView, BuildError> {
        let world = build_world_to(seed, pins, SkyChoice::Generated,
            &TerrainPins::default(), &SettlementPins::default(), &roster, depth)?;
        let Sky::Generated(sky) = sky_of(&world)? else { return Err(BuildError::Pins(..)); };
        Ok(AstronomyView { world, system: sky.system().clone(),
            calendar: sky.calendar().clone(), notes: sky.notes().to_vec(), roster })
    }
}
```

`TerrainView::build` calls `AstronomyView::build_to(.., BuildDepth::Terrain)` then adds `terrain_of`/`summarize`. `ClimateView::build` adds `climate_of`. `SettlementView::build` builds to `BuildDepth::Settlements`. `FullView::build` builds to `BuildDepth::Full`. Each reuses the world its inner view built — do **not** rebuild the world per rung (that would defeat the win); build once at the target depth and reconstruct the cheap derived pieces (terrain/climate reconstruction is already how the current `WorldView::build` works).

- [ ] **Step 3: Keep the old `WorldView` as a `FullView` type alias temporarily** so the existing extractors and tests compile while Task 5 retypes them. Add `pub type WorldView = FullView;`? No — `FullView` has no `system`/`terrain`/`climate` fields directly. Instead, keep the current `WorldView` struct and its `build`/`build_with_roster` untouched in this task; the new views live alongside it. Task 5 migrates extractors off `WorldView`, Task 6 deletes it.

- [ ] **Step 4: Compile**

Run: `cargo build -p hornvale-lab`
Expected: clean — new types added, nothing yet consumes them.

- [ ] **Step 5: Commit**

```bash
git add windows/lab/src/metrics.rs
git commit -m "feat(lab): narrowed view chain (Astronomy⊂…⊂Full) with AsRef coercions"
```

### Task 4: `Extractor` enum — a rung-tagged extractor

> **SEQUENCING CORRECTION (2026-07-12, compile-atomicity).** The original
> Tasks 4/5/6 changed `Metric.extract`'s type in Task 4 and fixed its 110
> uses + the runner in later tasks — leaving non-compiling intermediate
> commits, which violates the "every commit compiles" rule. Re-decomposed so
> each commit compiles: **Task 4** adds `Extractor`/`BuiltView`/dispatch as
> *additive* types (re-exported from `lib.rs` to avoid `dead_code`, exactly as
> Task 3 did) and does NOT touch `Metric`; **Task 5** is the *atomic swap* —
> change `Metric.extract` → `Extractor`, retype all 110 closures, AND update
> the runner (`build_row`) to build a `BuiltView` and dispatch — but build
> **always-`Full`** (`BuiltView::Full(FullView::build(..))`), so the swap is
> behavior-preserving and the existing calibration/drift tests prove every
> value is unchanged; **Task 6** changes the runner from always-`Full` to the
> study's *required* depth (the perf win) and adds the metamorphic guard.

**Files:**
- Modify: `windows/lab/src/metrics.rs` (add the enum/dispatch), `windows/lab/src/lib.rs` (re-export the new public types so they aren't dead code)

- [ ] **Step 1: ADD (do not yet consume) a rung-tagged extractor enum**

Add `Extractor`, `BuiltView`, `Extractor::rung`, `Extractor::apply`, the `BuiltView` accessors, and a `BuiltView::build_to(seed, pins, roster, depth)` constructor (dispatches to the right view constructor and wraps in the matching variant). Re-export `Extractor` and `BuiltView` from `lib.rs`. **Do NOT change `Metric.extract` in this task** — that is Task 5's atomic swap. This task is purely additive and must compile clean.

```rust
/// A metric's extractor, tagged by the view rung it reads. The tag *is* the
/// metric's build-depth: the runner builds each study only as deep as its
/// deepest selected metric. Under-building is impossible — an extractor
/// physically cannot name a field its view type does not expose.
pub enum Extractor {
    /// Reads astronomy only.
    Astronomy(fn(&AstronomyView) -> MetricValue),
    /// Reads terrain (+ astronomy).
    Terrain(fn(&TerrainView) -> MetricValue),
    /// Reads climate (+ terrain).
    Climate(fn(&ClimateView) -> MetricValue),
    /// Reads settlement/culture facts.
    Settlement(fn(&SettlementView) -> MetricValue),
    /// Reads religion/language/species facts.
    Full(fn(&FullView) -> MetricValue),
}

impl Extractor {
    /// The build depth this extractor requires.
    pub fn rung(&self) -> BuildDepth {
        match self {
            Extractor::Astronomy(_) => BuildDepth::Astronomy,
            Extractor::Terrain(_) | Extractor::Climate(_) => BuildDepth::Terrain,
            Extractor::Settlement(_) => BuildDepth::Settlements,
            Extractor::Full(_) => BuildDepth::Full,
        }
    }
}
```

(Climate maps to `BuildDepth::Terrain` because climate commits no facts — a Climate-rung metric needs a Terrain-depth *world* plus the climate reconstruction, which the `ClimateView` constructor does.)

(The `Metric.extract` field type change and `Metric::rung()` belong to Task 5's atomic swap, NOT this task — see the SEQUENCING CORRECTION box above. This task only *adds* `Extractor`; nothing consumes it yet.)

- [ ] **Step 2: Add the dispatcher — apply an `Extractor` to the deepest built view**

The runner builds one `BuiltView` at the study's required depth:

```rust
/// The view a study was built to — the runner's single per-world artifact.
pub enum BuiltView {
    Astronomy(AstronomyView),
    Terrain(TerrainView),
    Climate(ClimateView),
    Settlement(SettlementView),
    Full(FullView),
}

impl Extractor {
    /// Apply to a built view. The built view is always ≥ the extractor's rung
    /// (the runner guarantees it by building to the max selected rung), so the
    /// needed narrower view is reachable by `AsRef`. A shallower built view than
    /// the extractor's rung is a runner bug and panics loudly.
    pub fn apply(&self, view: &BuiltView) -> MetricValue {
        match (self, view) {
            (Extractor::Astronomy(f), v) => f(v.astronomy()),
            (Extractor::Terrain(f), v) => f(v.terrain()),
            (Extractor::Climate(f), v) => f(v.climate()),
            (Extractor::Settlement(f), v) => f(v.settlement()),
            (Extractor::Full(f), BuiltView::Full(fv)) => f(fv),
            (Extractor::Full(_), _) => panic!("Full extractor on a shallow view: runner bug"),
        }
    }
}
```

with `BuiltView` accessors that coerce via `AsRef`:

```rust
impl BuiltView {
    fn astronomy(&self) -> &AstronomyView {
        match self {
            BuiltView::Astronomy(v) => v,
            BuiltView::Terrain(v) => v.as_ref(),
            BuiltView::Climate(v) => v.as_ref(),
            BuiltView::Settlement(v) => v.as_ref(),
            BuiltView::Full(v) => v.as_ref(),
        }
    }
    // terrain(), climate(), settlement() analogous, panicking on a strictly
    // shallower variant (runner-guaranteed unreachable).
}
```

Also add `BuiltView::build_to(seed, pins, roster, depth) -> Result<BuiltView, BuildError>` that matches on `depth` and calls the matching view constructor (`AstronomyView::build_with_roster`/`TerrainView::…`/…), wrapping the result in the corresponding `BuiltView` variant. Task 5's runner uses it.

- [ ] **Step 3: Compile — additive, must be clean.** `cargo build -p hornvale-lab` + `cargo clippy -p hornvale-lab --all-targets -- -D warnings`. The new types are unused until Task 5, so re-export `Extractor` and `BuiltView` from `windows/lab/src/lib.rs` to avoid `dead_code` (Task 3's precedent). Commit: `feat(lab): add rung-tagged Extractor + BuiltView dispatch (additive)`.

### Task 5: Retype every extractor to its rung (compiler-guided)

**Files:**
- Modify: `windows/lab/src/metrics.rs`

**This is the atomic swap — ONE commit that must compile and pass all tests.** It changes `Metric.extract`'s type, retypes all 110 closures, AND updates the runner, because those are inseparable (the field type, its 110 uses, and the runner call site all change together). The runner builds **always-`Full`** here (no depth optimization yet — that is Task 6), so this commit is *behavior-preserving*: every metric is still computed on a full-depth world, just dispatched through `Extractor`. The existing calibration + drift tests are the proof — they must pass **unchanged** (byte-identical census values), which is the whole safety argument for the retype.

**Files:** `windows/lab/src/metrics.rs` (Metric + retype), `windows/lab/src/runner.rs` (`build_row`), `docs/superpowers/plans/lab-perf-rung-map.md` (the authoritative rung per metric).

- [ ] **Step 1:** Change `Metric.extract` to `pub extract: Extractor`; add `pub fn rung(&self) -> BuildDepth { self.extract.rung() }` to `Metric`.
- [ ] **Step 2:** For every metric, per the corrected rung map (`lab-perf-rung-map.md` — note `belief-kind` is **Full**, `hue-depth-*` are **Astronomy**), change `extract: |v| …` to `extract: Extractor::<Rung>(|v: &<Rung>View| …)` and fix the field paths:
  - Astronomy metrics: `v.system` / `v.calendar` / `v.notes` / `v.roster` (all on `AstronomyView`).
  - Terrain metrics: `v.terrain` / `v.globe`.
  - Climate metrics: `v.climate` on `ClimateView`; terrain fields via passthroughs.
  - Settlement/Full metrics: `v.world` etc. via passthrough accessors (Step 3).
- [ ] **Step 3:** Add passthrough accessors on `ClimateView`/`SettlementView`/`FullView` (`world()`, `terrain()`, `globe()`, `climate()`, `system()`, `calendar()`, `notes()`, `roster()` as each rung needs) so the deep-rung closures read `v.world()` etc. instead of deep field chains — keeps the ~80 deep closures legible and their diffs minimal.
- [ ] **Step 4:** In `runner.rs` `build_row`, replace `WorldView::build_with_roster(Seed(seed_value), pins, roster)` + `(m.extract)(&view)` with: build `BuiltView::Full(FullView::build_with_roster(Seed(seed_value), pins, roster)?)` (always Full for now) and `values: metrics.iter().map(|m| m.extract.apply(&built)).collect()`. Add a `pub fn run_forced_full(study) -> Result<RunResult, StudyError>` that is the current always-Full runner (Task 6's guard compares against it).
- [ ] **Step 5: Compile — the compiler now proves the rung map.** `cargo build -p hornvale-lab`. **Every compile error here is a metric whose rung was wrong (it reads a field its view lacks) — fix the rung/the map, not by widening the view.**
- [ ] **Step 6: Prove behavior-preserving.** `cargo test -p hornvale-lab` → PASS, **especially the calibration + fixture tests** (they assert census values against the committed `rows.csv` — an unchanged pass proves the Extractor dispatch computes byte-identical values). `cargo fmt && cargo clippy -p hornvale-lab --all-targets -- -D warnings`.
- [ ] **Step 7: Commit** `feat(lab): swap Metric to rung-tagged Extractor; retype all 110 (runner always-Full)`.

### Task 6: Runner builds to the required depth + metamorphic guard

> **Scope after the sequencing correction:** Task 5 already routed the runner through `BuiltView` + `Extractor::apply`, building **always-`Full`**. This task changes only the *depth*: compute `required = max rung over selected metrics` and build `BuiltView::build_to(.., required)` instead of always `Full` — that is where the census speedup finally lands. Then add the metamorphic guard proving the depth-scoped values equal the always-`Full` values (`run_forced_full`, added in Task 5). The guard MUST include a study that selects `belief-kind` (Full) and one selecting `hue-depth-*` (Astronomy) so the rung-map corrections are machine-checked.

**Files:**
- Modify: `windows/lab/src/runner.rs` (`build_row` — always-`Full` → `required` depth)
- Test: `windows/lab/tests/depth_ladder.rs`

- [ ] **Step 1: Write the metamorphic guard (the acceptance gate)**

```rust
// windows/lab/tests/depth_ladder.rs
//! Depth-scoped metric values are byte-identical to full-build values, per
//! study. This is Stage 2's acceptance gate (spec §4): the ladder may build
//! shallow, but a shallow build must return the exact number a full build does.

use hornvale_lab::{load_study, run};
use std::path::Path;

/// For each committed study, run it (which now builds to the required rung)
/// and compare against a forced-Full run of the same study.
#[test]
fn depth_scoped_metrics_match_full_build() {
    for study_file in [
        "census-lands-drift", "census-of-skies", "census-of-lands",
        "census-of-coasts", "census-of-peoples", "census-of-faiths",
        "census-of-tongues", "census-of-words", "census-of-eyes",
        "census-of-the-meeting", "branches-family",
    ] {
        let study = load_study(Path::new(&format!("../../studies/{study_file}.study.json")))
            .expect("study loads");
        let scoped = run(&study).expect("scoped run");
        let full = hornvale_lab::run_forced_full(&study).expect("full run");
        assert_eq!(
            scoped.rows, full.rows,
            "study {study_file}: depth-scoped rows differ from full-build rows"
        );
    }
}
```

Add a test-only `run_forced_full` (behind `#[doc(hidden)] pub` or a `pub(crate)` re-exported for the test) that ignores the rung and always builds `FullView`, so the guard compares scoped-vs-full on identical study inputs. Keep the seed count small in the studies used, or gate this test `#[ignore]` if any study is 500+ seeds and run it in the artifact step. (Match the calibration suite's fixture-load posture — see decision `calibration-loads-the-census-fixture`.)

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-lab --test depth_ladder`
Expected: FAIL — `run_forced_full` unresolved (and `build_row` still forces Full).

- [ ] **Step 3: Make `build_row` build to the study's required depth**

In `runner.rs`, compute the required depth once per run (max rung over selected metrics) and thread it into `build_row`:

```rust
let required = metrics.iter().map(|m| m.rung()).max().unwrap_or(BuildDepth::Astronomy);
```

In `build_row`, replace `WorldView::build_with_roster(Seed(seed_value), pins, roster)` with a builder that returns a `BuiltView` at `required`:

```rust
match BuiltView::build_to(Seed(seed_value), pins, roster, required) {
    Ok(view) => Ok(Row {
        seed: seed_value,
        pin_set: label.to_string(),
        values: metrics.iter().map(|m| m.extract.apply(&view)).collect(),
        refusal: None,
    }),
    Err(BuildError::Genesis(e)) => Ok(Row { /* Absent row, unchanged */ }),
    Err(e) => Err(StudyError { /* unchanged */ }),
}
```

Add `BuiltView::build_to(seed, pins, roster, depth)` that dispatches to the right view constructor by `depth` and wraps it in the matching `BuiltView` variant. Add `run_forced_full` that clones the study path but forces `BuildDepth::Full`.

- [ ] **Step 4: Run the guard + the calibration suite**

Run: `cargo test -p hornvale-lab`
Expected: PASS — `depth_ladder` passes (scoped == full for every study), and the calibration/fixture tests still pass (the fixtures were produced by the full build; the scoped build must reproduce them byte-for-byte).

- [ ] **Step 5: Prove the win — `census-lands-drift` builds terrain-only**

Run: `cargo run -p hornvale-worldgen --example profile_build` is not the check here; instead add a debug assertion or a one-off: confirm `census-lands-drift`'s `required` is `BuildDepth::Terrain` (all its metrics are Terrain-rung). Assert it in the guard test: `assert_eq!(required_depth_of("census-lands-drift"), BuildDepth::Terrain)`.

- [ ] **Step 6: Delete the obsolete `WorldView`** (and its `build`/`build_with_roster`) now that nothing references it. Confirm with `grep -rn "WorldView" windows/ cli/`.

- [ ] **Step 7: Regenerate artifacts + drift check**

Run: `make rebaseline` then `git diff --exit-code book/src/laboratory/`
Expected: **empty diff** — the depth ladder changed *how* worlds are built, not *what* the censuses measure. Any diff is a byte-identity regression; stop and fix.

- [ ] **Step 8: fmt + clippy + commit**

```bash
cargo fmt && cargo clippy -p hornvale-lab --all-targets -- -D warnings
git add windows/lab/src/runner.rs windows/lab/tests/depth_ladder.rs windows/lab/src/metrics.rs
git commit -m "feat(lab): runner builds to the study's required rung; metamorphic guard"
```

### Task 7 — DROPPED per Stage 1 readout (collapse the `strongest()` triple-read)

**Status: DROPPED.** Stage 1 measured the triple-read at ≈13% of the terrain stage in production (double-read; `age_at` unused) ≈ 3% of a full build, and the readers live on `crust::CrustField` (raw point), not `GeneratedTerrain` — the collapse would restructure frozen, byte-identity-critical crust code for a modest gain. Not worth the risk vs. the depth-ladder win. Retained below for the record; do not implement unless Nathan reopens it.

**Files:**
- Modify: `domains/terrain/src/crust.rs`
- Test: `domains/terrain/tests/tectonic_properties.rs` (add a byte-identity case)

Only do this task if Stage 1 Task 4 showed the triple-read is a material share. `strongest()` (`crust.rs:~400`) is recomputed by `thickness_at`, `age_at`, and `continental_at`.

- [ ] **Step 1:** Add a single `strongest_at(p) -> Option<(f64, CratonId)>` (or reuse `strongest`) and a per-cell cache/precompute so a cell read for thickness+age+continental sweeps the cratons once. The cleanest byte-identical form: precompute `strongest` per cell during `generate` and store it on `GeneratedTerrain`, then have the three readers index it. **Byte-identity is mandatory** — `strongest()==None` and `Some((0.0,_))` must yield identical thickness/age/continental (they already do per the spec's #3 equivalence).
- [ ] **Step 2:** Add a test asserting the collapsed readers return byte-identical `thickness_at`/`age_at`/`continental_at` for every cell of a sample world vs the pre-collapse values (capture a golden before the change).
- [ ] **Step 3:** `make rebaseline` → empty `book/src/laboratory/` diff. Commit.

### Task 8 — DROPPED per Stage 1 readout (split astronomy off the linear ladder)

**Status: DROPPED.** Stage 1 measured astronomy at 0.000s — genesis is free, so a terrain census paying one genesis costs nothing. Keep the ladder strictly linear. Retained for the record only.

### Task 9: Stage 2 full gate

- [ ] Run `make gate`. Expected: PASS. Absorb main if a stage boundary (per CLAUDE.md; not mid-measurement). Commit fmt fixups.

### Task 10: Free rider — depth-scope `pin_enumeration` (gated on Task 2)

**Files:**
- Modify: `windows/worldgen/tests/pin_enumeration.rs`

The enumerated pins (sky choice, rotation, neighbor, supercontinent) only write astronomy+terrain facts, so a terrain-depth build commits the exact fact prefix the determinism assert compares. Once Task 2's `build_world_to` exists, this test builds ~4× cheaper with an unchanged guarantee. (Parallelization across the 48 combos was landed separately as a Stage-1 sidecar commit; this task only changes build *depth*.)

- [ ] **Step 1:** In `pin_enumeration.rs`'s `build(combo)`, replace `build_world(Seed(42), &sky_pins, sky_choice, &terrain_pins, &SettlementPins::default())` with `build_world_to(Seed(42), &sky_pins, sky_choice, &terrain_pins, &SettlementPins::default(), &default_roster(), BuildDepth::Terrain)`. The determinism `assert_eq!` compares two serialized ledgers — now terrain-depth ledgers, which is the correct scope for these pins.
- [ ] **Step 2:** Run `cargo test -p hornvale-worldgen --test pin_enumeration`. Expected: PASS, and materially faster than the pre-change wall time recorded in the test's module doc (update that doc's timing note).
- [ ] **Step 3:** Confirm the built/refused split (reported, not asserted) is unchanged from the full-build run — a terrain-depth build must refuse or succeed identically for these pins (refusals come from astronomy/terrain genesis, both inside the terrain rung).
- [ ] **Step 4:** `cargo fmt && cargo clippy -p hornvale-worldgen --all-targets -- -D warnings`; commit.

---

# Track B — Stage 3: CI path-tiering + `flock`

**Goal:** the expensive drift regen runs only when a diff can change a drift-checked artifact; concurrent local gates serialize instead of contending.

**Files:**
- Modify: `.github/workflows/ci.yml` (gate the "Artifacts are current" step)
- Create: `scripts/inert-diff.sh` (the denylist classifier, shared source of truth)
- Create: `scripts/with-lock.sh` (the flock wrapper)
- Modify: `Makefile` (route `gate` and `rebaseline` through `with-lock.sh`)
- Test: `cli/tests/` or a bash self-test `scripts/tests/inert-diff-test.sh`

### Task 1: The denylist classifier (fail-safe: unknown ⇒ run)

**Files:**
- Create: `scripts/inert-diff.sh`

- [ ] **Step 1: Write the classifier**

```bash
#!/usr/bin/env bash
# Exit 0 ("inert — skip the drift regen") iff EVERY changed path is provably
# unable to change a drift-checked artifact. Exit 1 ("run the regen") if any
# path is a world-affecting or unknown path. Fail-safe by construction: an
# unclassified path defaults to RUN (spec §5).
set -euo pipefail

# The authoritative generated set — the SAME list ci.yml's drift check covers.
# A diff touching any of these must RUN the regen so the drift check can
# confirm the committed bytes match a fresh regeneration. Keep in sync with the
# `git diff --exit-code` args in .github/workflows/ci.yml (Stage 3 requirement).
GENERATED=(
  "book/src/gallery/"
  "book/src/reference/"
  "book/src/laboratory/"
  "clients/orrery/testdata/"
  "docs/audits/"
)

base="${1:?usage: inert-diff.sh <base-ref>}"
changed="$(git diff --name-only "$base"...HEAD)"
[ -z "$changed" ] && exit 0   # no changes ⇒ inert

is_inert() {
  local p="$1"
  # Any generated path is NOT inert — the drift check must run on it.
  for g in "${GENERATED[@]}"; do
    case "$p" in "$g"*) return 1 ;; esac
  done
  # Inert prefixes/suffixes (spec §5): prose, specs, decisions, client source,
  # narrative book pages, top-level docs. Everything else ⇒ NOT inert.
  case "$p" in
    book/*|docs/*|clients/*|*.md|LICENSE|README|README.md) return 0 ;;
    *) return 1 ;;
  esac
}

for p in $changed; do
  if ! is_inert "$p"; then
    echo "inert-diff: '$p' can affect a drift-checked artifact ⇒ RUN regen" >&2
    exit 1
  fi
done
echo "inert-diff: all $(echo "$changed" | wc -l | tr -d ' ') changed paths are inert ⇒ SKIP regen" >&2
exit 0
```

Run `shellcheck scripts/inert-diff.sh` (CLAUDE.md tooling rule) and fix any findings.

- [ ] **Step 2: Self-test the classifier**

Create `scripts/tests/inert-diff-test.sh` asserting representative cases with a throwaway git repo or `git diff` mocking: `docs/foo.md` ⇒ skip; `book/src/gallery/x.md` ⇒ run; `domains/terrain/src/crust.rs` ⇒ run; `clients/atlas/main.ts` ⇒ skip; an unknown top-level `foo.rs` ⇒ run. Run it; expect all pass.

- [ ] **Step 3: Commit**

```bash
git add scripts/inert-diff.sh scripts/tests/inert-diff-test.sh
git commit -m "feat(ci): denylist diff classifier (fail-safe: unknown ⇒ run)"
```

### Task 2: Gate the CI drift step

**Files:**
- Modify: `.github/workflows/ci.yml`

- [ ] **Step 1:** Wrap the "Artifacts are current" `run:` body so it early-exits when `inert-diff.sh` says inert. The cheap gate (`cargo test`/`fmt`/`clippy`) stays unconditional — only this step is tiered.

```yaml
      - name: Artifacts are current (determinism check)
        run: |
          # Skip the expensive drift regen when the diff cannot change any
          # drift-checked artifact (spec §5). Fail-safe: unknown path ⇒ run.
          if bash scripts/inert-diff.sh "${{ github.event.pull_request.base.sha || github.event.before }}"; then
            echo "Diff is inert; skipping drift regen."
            exit 0
          fi
          bash scripts/regenerate-artifacts.sh
          cargo test -p hornvale --test release_determinism -- --ignored
          cargo run --manifest-path tools/type-audit/Cargo.toml -- check
          git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ clients/orrery/testdata/ docs/audits/ \
            ':(exclude)book/src/gallery/*.png' \
            ':(exclude)book/src/gallery/scene-tiles-seed-42.json'
```

Ensure `actions/checkout` fetches enough history for the base ref (`fetch-depth: 0` or a merge-base fetch) — add it to the job's checkout step if not present. Confirm the base-sha expression works for both `push` (`github.event.before`) and `pull_request` (`base.sha`) triggers.

- [ ] **Step 2:** Push a prose-only branch and a worldgen-touching branch (or simulate locally with `inert-diff.sh`) to confirm skip-vs-run. Commit.

### Task 3: `flock`-serialize `gate` and `rebaseline`

**Files:**
- Create: `scripts/with-lock.sh`
- Modify: `Makefile`

**Note:** macOS does not ship `flock` (it's util-linux). The script must detect it and `brew install flock` if missing (or degrade to running unlocked with a warning) — flag this to the reviewer; the spec ratified `flock` but the dev platform is darwin.

- [ ] **Step 1: Write the lock wrapper**

```bash
#!/usr/bin/env bash
# Serialize an expensive recipe across concurrent local sessions on a repo-local
# lockfile, with a visible "waiting" message and a timeout (spec §5). gate-fast
# is intentionally NOT wrapped — it is the un-serialized fast path.
set -euo pipefail
cmd="${1:?usage: with-lock.sh <command-string>}"
root="$(git rev-parse --show-toplevel)"
lock="$root/.git/hornvale-gate.lock"     # repo-local, inside .git (never committed)
timeout="${HV_LOCK_TIMEOUT:-1800}"

if ! command -v flock >/dev/null 2>&1; then
  echo "with-lock: 'flock' not found (macOS: 'brew install flock'); running UNLOCKED." >&2
  bash -c "$cmd"; exit $?
fi

exec 9>"$lock"
if ! flock -n 9; then
  echo "with-lock: another session holds the gate lock; waiting up to ${timeout}s…" >&2
  if ! flock -w "$timeout" 9; then
    echo "with-lock: timed out after ${timeout}s waiting on $lock" >&2
    exit 1
  fi
fi
bash -c "$cmd"
```

`shellcheck scripts/with-lock.sh`.

- [ ] **Step 2: Route `gate` and `rebaseline` through it**

```makefile
gate: ## The full commit gate (fmt + clippy + workspace tests), serialized across sessions
	@bash scripts/with-lock.sh "cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings && cargo test --workspace"

rebaseline artifacts: ## Regenerate every committed generated artifact, serialized across sessions
	@bash scripts/with-lock.sh "bash scripts/regenerate-artifacts.sh"
```

Leave `gate-fast`, `quick`, `fmt-check`, `clippy`, `test` unlocked (the fast/iteration paths).

- [ ] **Step 3: Test the lock**

Run `make gate` in two terminals; confirm the second prints the "waiting" message and proceeds after the first releases. On macOS without `flock`, confirm the "running UNLOCKED" warning path.

- [ ] **Step 4: Commit**

```bash
git add scripts/with-lock.sh Makefile
git commit -m "feat(make): flock-serialize gate/rebaseline (gate-fast stays unlocked)"
```

---

# Track C — Stage 4: Correctness batteries

**Goal:** two free-riding batteries — O(1)-per-world census invariants and a pin-reaffirmation battery — on worlds a census already builds.

**Files:**
- Create: `windows/lab/tests/census_invariants.rs`
- Create: `windows/lab/tests/pin_reaffirmation.rs` (or under `domains/*/tests/` if pin metering lives there)

### Task 1: census-piggyback invariants (separate target, O(1)/world, gates but never blocks regen)

**Files:**
- Create: `windows/lab/tests/census_invariants.rs`

- [ ] **Step 1: Write the invariants over an existing census's worlds**

Ride a small census (e.g. `census-lands-drift`'s seed prefix). The invariants live in a **separate test target** (not in `render_csv`/the write path), are O(1) per world, and *gate* the suite without blocking artifact regen.

```rust
// windows/lab/tests/census_invariants.rs
//! Cheap universal invariants over every world a census builds (spec §6).
//! Separate target, O(1)/world, gates the suite but never touches the census
//! write path — an invariant failure blocks the gate, not a refreeze.

use hornvale_lab::{load_study, run};
use std::path::Path;

#[test]
fn census_worlds_satisfy_universal_invariants() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    for row in &result.rows {
        // Refusal ⇒ every metric Absent (no half-built world leaks numbers).
        if row.refusal.is_some() {
            assert!(row.values.iter().all(|v| matches!(v, hornvale_lab::MetricValue::Absent)),
                "seed {}: refusal must zero every metric", row.seed);
            continue;
        }
        for (name, v) in result.metric_names.iter().zip(&row.values) {
            if let hornvale_lab::MetricValue::Number(n) = v {
                assert!(!n.is_nan(), "seed {}: metric {name} is NaN", row.seed);
                // Monotone/percentile/thickness≥0 style bounds: assert the
                // ones that hold for THIS study's metrics (e.g. fractions in
                // [0,1], counts ≥ 0). Enumerate per metric name.
            }
        }
    }
}
```

Enumerate the per-metric bounds concretely (fractions ∈ [0,1], counts ≥ 0, `ocean-fraction` ∈ [0,1], etc.) for the census used.

- [ ] **Step 2: Verify an injected violation fails the battery without blocking regen**

Temporarily make one invariant assert something false, run `cargo test -p hornvale-lab --test census_invariants` (expect FAIL), then confirm `make rebaseline` still regenerates artifacts (the invariants are not on the write path). Revert the injection.

- [ ] **Step 3: Commit**

```bash
git add windows/lab/tests/census_invariants.rs
git commit -m "test(lab): census-piggyback invariants (separate O(1) target)"
```

### Task 2: pin-reaffirmation battery

**Files:**
- Create: `windows/lab/tests/pin_reaffirmation.rs`

- [ ] **Step 1: Enumerate the metered pins**

The battery reads the value the *unpinned* world drew (from the pin-metered genesis notes — e.g. `pinned continents N (seed draws D)`), pins that exact value, and asserts the pinned world is byte-identical to the unpinned one (a pin set to the drawn value is a no-op). First, `grep -rn "seed draws\|pinned" domains/astronomy/src domains/terrain/src` to enumerate which pins expose their drawn value. Record un-metered pins as gaps (do not silently skip).

- [ ] **Step 2: Write the battery for the metered pins × a seed sample**

For each metered pin and each seed in a small sample: build unpinned, parse the drawn value from the notes, build with that pin set, assert `serde_json::to_string(&pinned) == serde_json::to_string(&unpinned)`. Model the structure on the existing pin-isolation tests in `domains/astronomy/tests/genesis_properties.rs` and `domains/terrain/tests/tectonic_properties.rs`.

- [ ] **Step 3:** Run; expect PASS. A failure means a pin set to its drawn value is *not* a no-op — a real pin-isolation bug (stop and investigate, don't weaken the test).

- [ ] **Step 4: Commit**

```bash
git add windows/lab/tests/pin_reaffirmation.rs
git commit -m "test(lab): pin-reaffirmation battery (pin=drawn ⇒ byte-identical)"
```

---

# Definition of Done (whole campaign)

- [ ] All four stages' tasks complete; `make gate` green on the branch.
- [ ] `make rebaseline` produces an **empty** `book/src/laboratory/` diff (byte-identity preserved end to end).
- [ ] Book: a chronicle entry (`book/src/chronicle/`) + freshness sweep of stale chapters; re-score any Confidence Gradient bet this campaign moved (`book/src/open-questions.md`).
- [ ] A one-page retrospective in `docs/retrospectives/` (decision 0020).
- [ ] The Living-Globe / lab registry rows and the `lab-perf-rung-map.md` scratch enumeration reconciled (delete the scratch file once encoded in types).
- [ ] Merge only after main is green (the pre-existing cross-platform divergence is fixed by its owners).

---

## Self-review notes (author checklist, run against the spec)

- **§3-stage-1 / §7 profiler** → Stage 1 Tasks 1–6. ✓ (flag-gated, committed, byte-identity test, three questions answered in Task 6.)
- **§4 MAP-25** → Stage 2 Tasks 1–9. ✓ (view types, `build_world_to`, per-metric rung enumeration as the primary control, compile-error under-building, metamorphic guard, conditional `strongest()`-collapse Task 7, conditional astronomy-split Task 8.)
- **§5 CI tiering + flock** → Stage 3 Tasks 1–3. ✓ (denylist fail-safe, shared generated-path list, no new CI action, flock with visible wait + timeout, gate-fast unlocked; macOS-flock caveat flagged.)
- **§6 correctness batteries** → Stage 4 Tasks 1–2. ✓ (separate O(1) target that gates but doesn't block regen; pin-reaffirmation over metered pins with gap-recording.)
- **Cut items** (§3): none re-added. Cross-study sharing, terrain levers #2/#3, #17 screening, seed-parallel #5, release-mode CI all correctly omitted.
- **Type consistency:** `BuildDepth` (worldgen) is the single depth type; `Extractor`/`BuiltView`/the five `*View`s (lab) are the single view vocabulary; `build_world_to` is the single depth entry. No name drift across tasks.
- **Known soft spot:** the Settlements-vs-Full boundary shares `world` shape, so the compile-time guarantee is full only for astronomy/terrain/climate (the rungs that carry the census win); the metamorphic guard (Stage 2 Task 6) is the runtime backstop for the two ledger-backed rungs, exactly as the spec intends.
