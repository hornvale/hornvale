# Rift-and-Fit (Terrain Epoch v4) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Terrain epoch v4 — every world's continents become displaced fragments of an assembled supercontinent whose conjugate coastlines fit, with a probe-first evidence program (synthetic-D probe, Earth-mask anchor) governing which cell-scale rift textures are built for the inherited open shoreline-development band.

**Architecture:** The drawn craton set stays byte-identical; a draw-free *contact configuration* derives the assembly frame; one new stream (`terrain/rift`) draws a global spreading rate, and per-pair fracture curves come from hash sub-derivations; each craton's cap is clipped to its side of every seam it shares, evaluated in the assembly frame via the inverse of its Euler-pole rotation to its final (unchanged) position. The carve runs downstream untouched. Spec: `docs/superpowers/specs/2026-07-16-rift-and-fit-design.md` (approved at G3 2026-07-16).

**Tech Stack:** Rust edition 2024, `hornvale-kernel` (`Seed`/`noise`/`math`/`Geosphere`/`CellMap`), cargo-nextest gate, one out-of-workspace tool (`tools/earth-mask`, serde_json only).

## Global Constraints

Copied from the spec and CLAUDE.md; every task's requirements implicitly include these:

- **This IS an epoch (v4).** World bytes change. Every byte-changing task re-pins the drifting fixtures **in the same commit** (world-identity fixtures, golden pins, committed almanac/map/lab artifacts via `SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh` — confirm the exact invocation in the script header) — never deferred to close. Determinism *within* a commit always holds: `genesis_is_deterministic` passes at every commit.
- **Existing streams survive with identical consumption order.** Exactly ONE new sequential-draw label: `terrain/rift` (one spreading-rate draw). All other rift geometry derives from hash sub-derivations (`terrain_seed.derive(RIFT).derive("seam-{a}-{b}")`), which consume no sequential draws — the `craton-{id}` lobing-kernel convention. Pin isolation: pinned and unpinned paths consume identical draws per label.
- **Knobs are global; heterogeneity comes only from fields and drawn geometry.** Tuning constants are named `pub const`s in the module that owns them.
- **No new crates in the workspace. No `HashMap`/`HashSet`. No wall-clock.** Float sorting via `total_cmp`; per-cell iteration ascending `CellId`. All transcendentals via `hornvale_kernel::math` (libm), never `f64::sin/cos/acos` methods (decision 0041). `tools/earth-mask` sits OUTSIDE the workspace (decision 0027/0028 shape) and may use serde_json.
- **Quantization at the emit boundary only** — assembly, seams, clips, rotations all compute at full precision.
- Every `pub` item gets a one-line doc comment and every `pub` primitive a `type-audit:` verdict tag; regenerate `docs/audits/type-audit-report.md` in any commit that changes the pub API (`cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`).
- **`cargo fmt` is the final step before every commit.** Cost-order: fmt+clippy → `cargo test -p hornvale-terrain` scoped → full `make gate` only at commit points. Run once, capture (`2>&1 | tee /tmp/hv-test.txt`), never re-run to grep.
- **Censuses are NEVER regenerated locally.** The v3 canonical regen ran before this campaign (G3 ruling); mid-campaign the only tolerated red is stale-census drift, and the v4 canonical regen happens once, at close, on the AWS box with Nathan's authorization.
- **Campaign Autopilot is engaged.** G5 auto-continues on green; hard stops for Nathan: the Task-6 perf readout if over budget, any fidelity cut, the tuning-season band verdict / supersession adoption (Task 11), and all census/AWS spend. Ledger in the worktree's `.superpowers/sdd/decision-ledger.md` (seeded from the brainstorm ledger in the session scratchpad).
- **Absorb main at every stage boundary** (`make preflight` from the branch; merge main INTO the branch on ancestry NO-GO) — but never mid-measurement: Tasks 10–11 form one measurement window (baseline and readouts see the same physics).
- **Banked-mechanism discipline (decision 0057):** Task 10's mechanisms are built ONLY on measured demand, constants from sweeps against worst seeds, never guessed.
- Commit-message trailer (every commit):
  `Claude-Session: https://claude.ai/code/session_01Rq3ZtUtwfASsKUYdSet8Sx`

---

### Task 1: Stage-0 synthetic-D probe (the preregistered instrument)

Read-only against v3. Measures, per rift mechanism, the shoreline-development delta a *synthetic* injection produces on real default worlds — the activation evidence Task 10 consumes. No generator code may land before this task's readout is committed.

**Files:**
- Create: `domains/terrain/tests/rift_probe.rs` (`#[ignore]`d instrument)
- Create: `book/src/laboratory/census-of-coasts-iv.md` (prologue: probe tables)
- Modify: `book/src/SUMMARY.md` (add the chapter under Laboratory, after Census of Coasts III)

**Interfaces:**
- Consumes: `hornvale_worldgen` world build (mirror how `windows/lab` builds worlds — find its build call and reuse the same path at default pins), `hornvale_terrain::shape::shoreline_development(geo, elevation, sea_level) -> Option<f64>`, `TerrainProvider` accessors (`elevation_at`, `sea_level`, `carve_delta_at`).
- Produces: the probe tables (markdown in the lab page) and three named injection functions inside the test file (not pub API): `inject_slivers`, `inject_arms`, `inject_crenulation`.

- [ ] **Step 1: Write the instrument** (it is its own test; "failing first" here means run it and confirm it prints — the deliverable is the readout, not an assertion)

```rust
//! Stage-0 rift probe (rift-and-fit spec §6, preregistered): synthetic
//! injections of each banked rift mechanism into real v3 land masks,
//! measuring the shoreline-development delta each produces in isolation.
//! Run by hand: cargo test -p hornvale-terrain --release --test rift_probe -- --ignored --nocapture

use hornvale_kernel::{CellId, CellMap, Geosphere};

/// Seeds probed: the sculpting-probe study's first 20 (release build keeps
/// the hand-run under ~10 min; the probe ranks mechanisms, it is not a census).
const PROBE_SEEDS: u64 = 20;

/// Flip an ocean cell to just-above-sea land (sliver) or a land cell to
/// just-below-sea ocean (arm/crenulation) — the synthetic injections
/// operate on a copied elevation map so the estimator sees a real mask.
fn flip(elev: &mut CellMap<hornvale_terrain::ReferenceElevation>, /* confirm the exact elevation type + constructor in shape.rs's signature */ cell: CellId, above: bool, sea: f64) { /* set to sea ± 1.0 m */ }

// inject_slivers: for a swept fraction f in {0.05, 0.15, 0.30} of coastal
// OCEAN cells (ocean cells with >=1 land neighbor), in ascending CellId,
// flip every k-th candidate to land where k = ceil(1/f) — alternating,
// never adjacent (skip a candidate whose neighbor was already flipped).
// inject_arms: for n_arms in {4, 8, 16}: from every m-th coastal LAND cell,
// walk inland via the highest-elevation neighbor for depth in {3, 6} cells,
// flipping each visited cell to ocean (a 1-cell-wide bay).
// inject_crenulation: re-run Sculpting's parity experiment: for fraction f
// of coastal land cells (every k-th, ascending CellId), flip to ocean —
// single-hex alternation along the existing coast.

#[test]
#[ignore = "probe: Stage-0 rift instrument, run by hand (spec §6)"]
fn rift_probe_tables() {
    // For each seed: build the default world once; compute baseline D;
    // for each mechanism x sweep point: copy elevation, inject, recompute
    // D with the SAME sea level; print a markdown table row.
    // Also print the fit-degradation proxy: fraction of coastal cells whose
    // land/ocean classification differs between (elevation - carve_delta)
    // and elevation, both judged against the final sea level — the carve's
    // coastal blur, recorded unbanded (spec §6, ledger #10).
}
```

The exact injection code is the implementer's to write inside the test file; the sweep points, orderings (ascending `CellId`, deterministic), and the just-above/just-below-sea flip convention above are fixed by this plan. No workspace source file changes.

- [ ] **Step 2: Run the probe (release) and capture**

Run: `cargo test -p hornvale-terrain --release --test rift_probe -- --ignored --nocapture 2>&1 | tee /tmp/hv-rift-probe.txt`
Expected: per-seed tables; median ΔD per mechanism per sweep point.

- [ ] **Step 3: Write `book/src/laboratory/census-of-coasts-iv.md`** — prologue only: the preregistration statement (this table was measured before any generator code), the probe tables (medians over the 20 seeds), the fit-degradation proxy number, and the activation reading: which mechanisms show band-relevant ceilings (expected order per spec §5: slivers high, arms moderate, crenulation small-moderate — record what was MEASURED, especially if it falsifies the expectation). Add to `SUMMARY.md`.

- [ ] **Step 4: Gate the touched surface**

Run: `cargo fmt --check && cargo clippy -p hornvale-terrain --all-targets -- -D warnings && cargo test -p hornvale-terrain --test rift_probe` (non-ignored = no-op pass) and `cargo test -p hornvale --test docs_consistency`.
Expected: green; the ignored probe does not run in the gate.

- [ ] **Step 5: Commit**

```bash
git add domains/terrain/tests/rift_probe.rs book/src/laboratory/census-of-coasts-iv.md book/src/SUMMARY.md
git commit -m "probe(rift): Stage-0 synthetic-D instrument + Census of Coasts IV prologue"
```

### Task 2: Earth mask at L6 and `D_earth` (the anchor decontamination)

**Files:**
- Create: `tools/earth-mask/Cargo.toml`, `tools/earth-mask/src/main.rs` (outside the workspace — confirm `tools/type-audit/Cargo.toml` has no `[workspace]` membership and mirror it, including a `[workspace]` empty table to sever inheritance)
- Create: `tools/earth-mask/data/ne_110m_land.geojson` (Natural Earth 110m land, public domain — the coordinator fetches once; record source URL + sha256 in the tool's README section of `main.rs` doc comment)
- Create: `book/src/laboratory/generated/earth-mask-l6/rows.csv` (committed fixture: `cell,land` per L6 cell, ascending)
- Create: `windows/lab/tests/earth_anchor.rs`
- Modify: `domains/terrain/src/shape.rs` (factor a mask-based estimator core)
- Modify: `book/src/laboratory/census-of-coasts-iv.md` (record `D_earth` and the preregistered proposed band)

**Interfaces:**
- Consumes: `hornvale_kernel::Geosphere` (path dependency from the tool: `hornvale-kernel = { path = "../../kernel" }`), existing `shape::shoreline_development`.
- Produces:
  - `pub fn shoreline_development_of_mask(geo: &Geosphere, land: &CellMap<bool>) -> Option<f64>` in `shape.rs`; the existing `shoreline_development` delegates to it (compute the land mask from elevation/sea, then call — byte-identical results, prove with a test).
  - The committed fixture and the pinned `D_earth` golden in `earth_anchor.rs`.

- [ ] **Step 1: Failing test — the estimator factors without drift**

```rust
// in shape.rs tests
#[test]
fn mask_estimator_matches_elevation_estimator() {
    // Build one default world's elevation + sea level (same build path as
    // other shape tests in this file — follow the file's existing helper).
    // Compute D both ways; assert exactly equal (same arithmetic, same order).
}
```

Run: `cargo test -p hornvale-terrain mask_estimator` — FAIL (function absent).

- [ ] **Step 2: Factor `shoreline_development_of_mask`** — move the loop body of `shoreline_development` (LOC `shape.rs:28-52`, confirm) into the mask version; the elevation version computes `land = elevation >= sea_level` per cell and delegates. Keep iteration order and float accumulation order IDENTICAL (byte-identity of every existing metric consumer). Run: test passes. Also run `cargo test -p hornvale-lab 2>&1 | tee /tmp/hv-t2.txt` (metric consumers).

- [ ] **Step 3: The tool.** `tools/earth-mask` reads the GeoJSON (serde_json untyped `Value`), extracts polygons+holes, and for each L6 cell center (lat/lon from `Geosphere` — confirm the accessor for cell position and convert) runs an even-odd ray-cast point-in-polygon in lon/lat degrees (dateline: normalize lon to [-180,180); the 110m dataset splits at the dateline already; poles: cells with |lat| > 85 test against polygon bounding boxes first — Antarctica is a polygon, it resolves normally). Writes `cell,land` CSV ascending. Deterministic: pure arithmetic over committed data — byte-stable output. Fetch data (coordinator, once):

```bash
curl -L -o tools/earth-mask/data/ne_110m_land.geojson \
  https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_110m_land.geojson
shasum -a 256 tools/earth-mask/data/ne_110m_land.geojson  # record in main.rs doc
cargo run --manifest-path tools/earth-mask/Cargo.toml --release > book/src/laboratory/generated/earth-mask-l6/rows.csv
```

- [ ] **Step 4: Failing test — `earth_anchor.rs`**

```rust
//! The Earth-mask anchor (rift-and-fit spec §7): Earth's own coastline,
//! rasterized to the canonical L6 mesh, measured by the unchanged
//! estimator. The band proposal derives from THIS number and was
//! preregistered before the mask was built.

#[test]
fn earth_mask_d_is_pinned() {
    // Load book/src/laboratory/generated/earth-mask-l6/rows.csv relative
    // to CARGO_MANIFEST_DIR/../../; build the L6 Geosphere; compute
    // shoreline_development_of_mask; assert against the pinned golden to
    // 8 significant digits (kernel quantize convention).
    // FIRST RUN: print the value, then pin it here.
    // Sanity floor: Earth is not a circle — assert d > 1.5 before pinning.
}
```

- [ ] **Step 5: Run, pin `D_earth`, record.** Pin the golden; compute the preregistered proposal `[D_earth/1.6, 1.6*D_earth]`; append both to `census-of-coasts-iv.md` with the sentence: "the multipliers were committed in the spec before the mask existed; adoption of this band is Nathan's ruling, reserved to the season close." Run `cargo test -p hornvale-lab --test earth_anchor` — PASS.

- [ ] **Step 6: Gate + commit**

```bash
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings 2>&1 | tail -3
cargo test -p hornvale-terrain -p hornvale-lab 2>&1 | tee /tmp/hv-t2b.txt | tail -5
git add tools/earth-mask book/src/laboratory/generated/earth-mask-l6 windows/lab/tests/earth_anchor.rs domains/terrain/src/shape.rs book/src/laboratory/census-of-coasts-iv.md
git commit -m "feat(earth-anchor): Earth land mask at L6 + D_earth via the unchanged estimator (spec §7)"
```

**STAGE BOUNDARY** — `make preflight`; absorb main if it moved (the v3 census regen lands around now).

### Task 3: The assembly frame (contact configuration)

**Files:**
- Modify: `domains/terrain/src/crust.rs`
- Test: `domains/terrain/src/crust.rs` tests

**Interfaces:**
- Consumes: `Craton { id, center, radius_rad, age }`, `slerp` (crust.rs:399), `plates::{dot, normalize}`, `math::acos`.
- Produces:
  - `pub const CONTACT_FACTOR: f64 = 0.85;` (fraction of `r_i + r_j` at which two cratons read as sutured; < 1 so lobed envelopes merge — tuned only if the Task-6 pinned-world suture test demands)
  - `pub fn assemble_cratons(cratons: &[Craton]) -> Vec<[f64; 3]>` — assembly centers, index-aligned with `cratons`; draw-free, deterministic.

- [ ] **Step 1: Failing tests**

```rust
#[test]
fn assembly_is_identity_for_single_craton_and_sutures_multis() {
    let seed = Seed(42).derive(crate::streams::ROOT);
    let mut notes = Vec::new();
    let pins = TerrainPins::default();
    let ocean_target = default_ocean_target(seed);
    let cratons = draw_cratons(seed, &pins, ocean_target, &mut notes);
    let assembly = assemble_cratons(&cratons);
    assert_eq!(assembly.len(), cratons.len());
    // Every craton i > 0 touches at least one earlier craton at the
    // contact separation (within 1e-9 rad) and none sits closer than
    // contact to ANY earlier craton (no deep overlap).
    for i in 1..cratons.len() {
        let mut touched = false;
        for j in 0..i {
            let sep = math::acos(dot(assembly[i], assembly[j]).clamp(-1.0, 1.0));
            let contact = CONTACT_FACTOR * (cratons[i].radius_rad + cratons[j].radius_rad);
            assert!(sep >= contact - 1e-9, "craton {i} overlaps {j}");
            if sep <= contact + 1e-9 { touched = true; }
        }
        assert!(touched, "craton {i} floats free of the assembly");
    }
    // Single craton: identity.
    let one = vec![cratons[0].clone()];
    assert_eq!(assemble_cratons(&one), vec![cratons[0].center]);
    // Determinism.
    assert_eq!(assembly, assemble_cratons(&cratons));
}
```

Run: `cargo test -p hornvale-terrain assembly_is_identity` — FAIL.

- [ ] **Step 2: Implement.** For `i` in `1..n`, id order: binary-search the pull parameter `t ∈ [0, 1]` along `slerp(cratons[i].center, anchor, t)` (anchor = `assembly[0]` = craton 0's center) for the smallest `t` at which `min_j (sep(i,j) - contact(i,j)) <= 0` over placed `j < i`, then settle at the `t` where the minimum hits exactly 0 (64 bisection iterations — deterministic, no tolerance tuning). If the craton already touches at `t = 0`, push it AWAY (reuse the `repel` extrapolation shape, `slerp` with `t > 1` from the violating neighbor) until contact — the same bisection on the outward arc. Zero stream draws. Doc comment: this is repulsion run in reverse (spec §3), and the assembly is derived — never drawn, never serialized.

- [ ] **Step 3: Run scoped tests, fmt, commit**

```bash
cargo test -p hornvale-terrain assemble 2>&1 | tail -3 && cargo fmt
git add domains/terrain/src/crust.rs
git commit -m "feat(rift): contact-configuration assembly frame (draw-free; spec §3.1)"
```

### Task 4: The rift draw — stream, seams, rotations

**Files:**
- Modify: `domains/terrain/src/streams.rs` (RIFT label)
- Create: `domains/terrain/src/rift.rs` (new module; register in `lib.rs`)
- Test: `domains/terrain/src/rift.rs` tests

**Interfaces:**
- Consumes: `assemble_cratons`, `Craton`, `lobed_envelope`-style seeded noise (`hornvale_kernel::noise::sphere_fbm01` — confirm the exact helper name used by `crust.rs:106`), `plates::{cross, normalize, dot}`, `math`.
- Produces:
  - `streams::RIFT: &str = "rift"` — doc: "Rift draws (rift-and-fit, spec §3): ONE global spreading-rate draw. Per-seam geometry uses hash sub-derivations (`seam-{a}-{b}`) — no sequential draws, no draw-count variance. A NEW label — epoch v4 save-format contract."
  - `pub struct Seam { pub a: u32, pub b: u32, pub noise_seed: Seed }` (a < b, craton ids)
  - `pub struct RiftHistory { pub assembly: Vec<[f64;3]>, pub seams: Vec<Seam>, pub spreading_rate: f64 }`
  - `pub fn draw_rift(terrain_seed: Seed, cratons: &[Craton]) -> RiftHistory` — assembly via `assemble_cratons`; seams = every pair within `SEAM_CONTACT_FACTOR * (r_a + r_b)` at assembly (use `CONTACT_FACTOR + SEAM_SLACK` so bisection endpoints count; `pub const SEAM_SLACK: f64 = 0.02;`), ascending `(a, b)`; ONE `next_f64` for `spreading_rate` in `[SPREAD_MIN, SPREAD_MAX]` = `[0.3, 1.2]` rad-per-unit-age (narrative scalar only).
  - `pub fn seam_side(seam: &Seam, cratons: &[Craton], assembly: &[[f64;3]], p: [f64;3]) -> f64` — signed: positive on `a`'s side. Definition (both margins share it exactly): `env_a(p) - env_b(p) + FRACTURE_AMP * (fbm01(seam.noise_seed, p) - 0.5)` where `env_x` is the plain (unlobed) quartic envelope `(1 - (angle_x / r_x)^2)^2` clamped at 0, evaluated against ASSEMBLY centers, and `pub const FRACTURE_AMP: f64 = 0.35;`, fbm at `FRACTURE_FREQ: f64 = 6.0`, 4 octaves (multi-cell-scale crenulation ships in the core; the cell-scale octave is Task 10's banked knob).
  - `pub fn rotation_for(assembly_center: [f64;3], final_center: [f64;3]) -> ([f64;3], f64)` — Euler pole (normalized cross product; any fixed axis when near-parallel — return `([0.0,0.0,1.0], 0.0)`) and angle; `pub fn rotate(pole: [f64;3], angle: f64, p: [f64;3]) -> [f64;3]` — Rodrigues, via `math::{sin, cos}`; inverse = negate angle.

- [ ] **Step 1: Failing tests**

```rust
#[test]
fn rift_draws_one_scalar_and_derives_seams_per_pair() {
    let seed = Seed(42).derive(crate::streams::ROOT);
    let cratons = /* draw as in crust tests */;
    let rift = draw_rift(seed, &cratons);
    assert!((SPREAD_MIN..=SPREAD_MAX).contains(&rift.spreading_rate));
    assert!(!rift.seams.is_empty(), "a multi-craton assembly must seam");
    for s in &rift.seams { assert!(s.a < s.b); }
    // Per-pair stability: a seam's noise seed depends only on (a, b) —
    // recompute and compare.
    assert_eq!(draw_rift(seed, &cratons), rift);
}

#[test]
fn seam_side_is_shared_and_signed() {
    // At craton a's assembly center: positive. At b's: negative.
    // Rodrigues round-trip: rotate then inverse-rotate returns p within 1e-12.
}
```

Run: `cargo test -p hornvale-terrain rift_` — FAIL (module absent).

- [ ] **Step 2: Implement `rift.rs`** per the interface block (every function above fully specified; `derive` the seam seed as `terrain_seed.derive(streams::RIFT).derive(&format!("seam-{a}-{b}"))`). `#[derive(Debug, Clone, PartialEq)]` on both structs. Doc comments + type-audit tags on every pub item (`bare-ok(index: a)`, `bare-ok(index: b)`, `bare-ok(ratio: spreading_rate)` — check `tools/type-audit -- check` accepts the classes).

- [ ] **Step 3: Run, fmt, audit, commit**

```bash
cargo test -p hornvale-terrain rift_ 2>&1 | tail -3 && cargo fmt
cargo run --manifest-path tools/type-audit/Cargo.toml -- check 2>&1 | tail -2
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git add domains/terrain/src/rift.rs domains/terrain/src/streams.rs domains/terrain/src/lib.rs docs/audits/type-audit-report.md
git commit -m "feat(rift): terrain/rift stream, seams, Euler rotations (spec §3.2/§3.4)"
```

### Task 5: The clip (pure, unwired — v3 bytes still untouched)

**Files:**
- Modify: `domains/terrain/src/crust.rs` (`CrustField` grows an optional rift)
- Modify: `domains/terrain/src/rift.rs` (clip evaluation)
- Test: both files' tests

**Interfaces:**
- Consumes: Task 4's `RiftHistory`, `seam_side`, `rotation_for`, `rotate`.
- Produces:
  - `pub const CLIP_TAPER: f64 = 0.08;` (seam_side half-width over which the clip falls 1 → 0; in envelope-difference units)
  - `pub fn clip_at(rift: &RiftHistory, cratons: &[Craton], craton_id: u32, p_final: [f64;3]) -> f64` — inverse-rotate `p_final` by craton `craton_id`'s rotation, evaluate every seam involving it: `clip = min over seams of smoothstep((side_toward_self) / CLIP_TAPER)` clamped `[0,1]` (`side_toward_self` = `seam_side` if self is `a`, else negated); cratons with no seams return 1.0.
  - `pub fn CrustField::new_with_rift(terrain_seed: Seed, cratons: Vec<Craton>, terranes: Vec<Terrane>, rift: Option<RiftHistory>, major_count: usize) -> CrustField` — existing constructors delegate with `None` (all call sites compile unchanged; `major_count` bounds clip application to majors — microcontinents ride along unclipped since they are appended after majors in the concat list, spec §3 integration note). `strongest` multiplies a MAJOR craton's envelope by its clip before the max.

- [ ] **Step 1: Failing tests**

```rust
#[test]
fn empty_rift_is_byte_identical_to_v3_field() {
    // CrustField::new_with_terranes(...) vs new_with_rift(..., None, n):
    // thickness_at equal (==, not approx) on 200 fibonacci-sphere points.
}

#[test]
fn conjugate_margins_fit_by_construction() {
    // For one seam (a, b): sample 50 points on the seam curve in the
    // ASSEMBLY frame (walk the great circle between assembly centers,
    // bisect seam_side to 0 along cross-arcs — helper local to the test).
    // Rotate each curve point forward by a's rotation: clip_at(a) there
    // is 0.5 within 1e-6. Rotate the SAME point by b's rotation: clip_at(b)
    // is 0.5 within 1e-6. One shared curve, two displaced margins —
    // the fit battery (spec §8).
}

#[test]
fn clip_is_one_far_from_seams_and_single_craton_is_uncut() { /* ... */ }
```

Run: `cargo test -p hornvale-terrain clip -- fit` — FAIL.

- [ ] **Step 2: Implement**; smoothstep is `x.clamp(0,1)` → `3x²-2x³` centered so `side = 0` gives 0.5 (i.e. `smoothstep(0.5 + side / (2*CLIP_TAPER))`). Keep `strongest`'s tie-break (`total_cmp` then id) untouched.

- [ ] **Step 3: Run, fmt, commit** — `git commit -m "feat(rift): assembly-frame clip on CrustField, conjugate-fit battery green (spec §3.3, §8)"`

### Task 6: Wire into generate — THE epoch commit (bytes drift; everything re-pins)

**Files:**
- Modify: `domains/terrain/src/globe.rs` (draw + thread rift; retain on `TectonicGlobe`)
- Modify: `domains/terrain/src/crust.rs` (`draw_cratons`: delete the `--supercontinent` slerp-cluster block — the pin's transform moves to the rift stage)
- Modify: `domains/terrain/src/pins.rs` (supercontinent doc: "holds the world at the assembly frame — sutured supercontinent, seams visible")
- Modify: every drifting fixture/golden (see Step 4)
- Test: `domains/terrain/tests/tectonic_properties.rs` (pin isolation for RIFT; supercontinent suture property)

**Interfaces:**
- Consumes: everything from Tasks 3–5.
- Produces:
  - `TectonicGlobe.rift: RiftHistory` (doc: recomputed at genesis, never serialized)
  - Pipeline (globe.rs, after `draw_terranes`, before `CrustField` construction): `let rift = rift::draw_rift(terrain_seed, &cratons);` then final centers resolve: **unpinned** — cratons keep their drawn/repelled centers, `rift` provides assembly + rotations; **pinned `supercontinent=Some(true)`** — each major craton's center is REPLACED by its assembly position (displacement zero; rotations become identity; repulsion was already skipped under the pin, unchanged). Draw counts identical on both paths (the replacement is post-draw arithmetic). `CrustField::new_with_rift(terrain_seed, [cratons, micro].concat(), terranes, Some(rift.clone()), cratons.len())`.

- [ ] **Step 1: Failing property tests** (in `tectonic_properties.rs`, following the file's existing pin-isolation test shape — read it first and mirror):

```rust
#[test]
fn rift_pin_isolation_supercontinent_consumes_identical_draws() {
    // Build seed 42 with supercontinent Some(true), Some(false), None:
    // the metering notes and a downstream draw probe (e.g., terrane set,
    // which draws AFTER cratons) must be identical across all three —
    // the pin never changes consumption.
}

#[test]
fn pinned_supercontinent_is_sutured() {
    // supercontinent=true, seed 42: every major craton's final center
    // touches an earlier one at CONTACT_FACTOR separation (the Task-3
    // assertion, now on the WORLD's cratons).
}

#[test]
fn default_world_carries_a_rift_history() {
    // outcome.globe.rift.seams non-empty on seed 42 default pins.
}
```

- [ ] **Step 2: Wire (per the Produces block), run scoped terrain tests.** Expected: the three new tests pass; `genesis_is_deterministic` passes; **fixture-comparing tests now FAIL — that is the epoch**.

- [ ] **Step 3: Perf budget (spec §3, preregistered).** Build the release CLI twice (main vs branch): `time cargo run --release -p hornvale -- new --seed 42 --out /tmp/hv-v4.json` ×5, median. Budget ≤ 1.05× main's median. Over budget → STOP, Nathan carve-out. Record the two medians in the commit message.

- [ ] **Step 4: Re-pin the world in the same commit.** `SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh` (regenerates almanacs, map, registry/manifest dumps, lab studies — confirm against the script; census fixtures stay stale, tolerated mid-campaign). Then any golden-pin tests still red (`cargo nextest run --workspace 2>&1 | tee /tmp/hv-t6.txt`): re-pin each in place (lens_purity world-identity, calibration floors — follow each test's own re-pin doc comment). The heavy tier stays ignored. Everything except stale-census must be green.

- [ ] **Step 5: Full gate + commit**

```bash
make gate 2>&1 | tail -5   # expect green (stale-census is heavy-tier, not in gate)
git add -A
git commit -m "feat(rift)!: wire rift-and-fit into genesis — terrain epoch v4 (spec §3); supercontinent holds at assembly (spec §4); artifacts re-pinned; perf 1.0Nx (budget 1.05x)"
```

**STAGE BOUNDARY** — `make preflight`; absorb main.

### Task 7: CLI + book wording for the superseded pin

**Files:**
- Modify: `cli/src/` wherever `--supercontinent` help text lives (grep `supercontinent` under `cli/src/`); update wording: "hold the world at its pre-breakup assembly (sutured supercontinent; rift seams visible)"
- Modify: the book's worldgen/pins chapter if it describes the old clustering (grep `supercontinent` under `book/src/`, excluding generated/)
- Test: existing CLI help test if one pins the text (grep `hornvale help` in `cli/tests/`)

- [ ] **Step 1–3:** Update text; `cargo test -p hornvale 2>&1 | tail -3`; `cargo test -p hornvale --test docs_consistency`; fmt; commit `"docs(rift): supercontinent pin wording — assembly-hold semantics"`.

### Task 8: Rift facts

**Files:**
- Modify: `domains/terrain/src/facts.rs` (predicates + emission in `genesis`)
- Modify: wherever terrain registers predicate defs (follow `TERRAIN_PIN`'s registration — grep its definition and registry insertion)
- Modify: regenerate `book/src/reference/` concepts dump (drift-checked artifact)
- Test: `facts.rs` tests (mirror `genesis_commits_the_summary_facts`)

**Interfaces:**
- Produces predicates (concept-registry naming per the book's conventions chapter — verify prefix style against existing `TERRAIN_*` constants): `terrain/rifted-from` (Text: `"craton-{a} ↔ craton-{b}"`, one fact per seam), `terrain/breakup-age` (Number: relative displacement angle between the pair's rotations / spreading_rate, per seam — derived, spec §3.5), `terrain/spreading-rate` (Number, one fact). Wording honesty (ledger #10): the `rifted-from` predicate's registered doc line says "conjugate margins fit up to subsequent erosion."

- [ ] **Steps:** failing test (facts present on a default seed-42 world; ZERO rift facts on a `--continents 1` world) → implement → regenerate concepts artifact (`cargo run -p hornvale -- concepts > ...` — confirm exact redirect target from `.github/workflows/ci.yml`'s artifact step) → scoped tests + fmt → commit `"feat(rift): breakup facts via terrain facts machinery (spec §9)"`.

### Task 9: Companion multi-scale roughness metric

**Files:**
- Modify: `windows/lab/src/metrics.rs` (new metric), `windows/lab/src/roster.rs` (register)
- Modify: `domains/terrain/src/shape.rs` if a downsample helper is cleaner there
- Test: metrics tests (follow the file's existing metric-test shape)

**Interfaces:**
- Produces: metric `coast-roughness-slope` (unbanded, spec §7): for k in {4, 5, 6}, build `Geosphere` at level k, classify each k-cell land iff its nearest L6 cell (via `NearestCellIndex` — confirm construction cost; build once per level) is land at the world's final elevation/sea; `D_k = shoreline_development_of_mask(...)`; emit the least-squares slope of `ln D_k` against `k as f64` (three points; slope > 0 = roughness concentrated at fine scales; document the reading in the metric's description field). Skip-emit (`None`/Absent — follow how `shoreline-development` signals Absent, metrics.rs:2017-2026) when any level has no shoreline.

- [ ] **Steps:** failing metric test (seed-42 world emits a finite slope; a metric-count assertion elsewhere may pin the roster size — re-pin it in this commit) → implement → `cargo test -p hornvale-lab 2>&1 | tail -3` → regenerate lab-study artifacts if any committed study includes all-metrics rows (`SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh`; stale-census unchanged) → fmt → commit `"feat(lab): coast-roughness-slope companion metric, unbanded (spec §7; MAP-50)"`.

**STAGE BOUNDARY** — `make preflight`; absorb main. Tasks 10–11 are one measurement window: no absorption inside.

### Task 10: The tuning season (banked mechanisms on measured demand)

The protocol is Sculpting Task 14's, inherited verbatim with this campaign's levers. **Instrument:** `studies/sculpting-probe.study.json` (100 seeds, release) — rename-free reuse; readout via `cargo run --release -p hornvale -- lab run studies/sculpting-probe.study.json` (confirm the study includes shoreline-development, shelf-fraction, the four stayers, and the reroute diagnostic; it does — it was Sculpting's tuning instrument).

**Files:** `domains/terrain/src/rift.rs` (mechanisms), `crust.rs` (sliver incorporation), `book/src/laboratory/census-of-coasts-iv.md` (iteration table), per-iteration fixture re-pins.

**Protocol (each iteration = one commit, autopilot G5 on green):**

- [ ] **Step 1: Baseline readout.** v4-untuned probe medians vs Census III's v3 finals (7.3202 etc.). Record as iteration-0 row. Guards all green: stayers inside, shelf-fraction inside, single-craton floor (median/min per its battery), reroute median < 0.10.
- [ ] **Step 2+: Iterate, demand-ordered by the Stage-0 probe tables.** For each mechanism, in descending measured ceiling, while shoreline-development sits below the governing floor AND the previous iteration moved < +0.5 is NOT yet the stop state:
  - **Sliver strings** (expected first): per seam, from `seam.noise_seed.derive("slivers")`, place `SLIVER_SPACING`-separated points along the seam curve's arc in the assembly frame, alternating offset to each side by `SLIVER_OFFSET_RAD`; each becomes a small unlobed cap (radius `SLIVER_RADIUS_RAD ≈` one L6 cell ≈ 0.032 rad) added to the crust as terrane-style additive kernels that ride the NEAREST side's rotation (stranded on one margin — Seychelles). Hash-derived: zero stream draws. Constants from a three-point sweep against the worst probe seeds (0057 discipline; document the sweep in the lab page).
  - **Failed rift arms:** at each triple junction (assembly points where three seam curves' pairwise `seam_side` all ≈ 0 — locate by scanning seam-pair intersections), extend a polyline into the interior of the craton whose envelope is LOWEST there (the yielding side), length `ARM_LEN_RAD`, width one cell; implemented as a negative clip channel (`clip *= 1 - arm_mask`). Hash-derived from `derive("arm-{a}-{b}-{c}")`.
  - **Cell-scale crenulation octave:** add one octave at `FRACTURE_FREQ * 8` with amplitude knob `CRENULATION_AMP`, swept.
  - Each iteration: implement + unit tests → probe readout → append the iteration row (shoreline, shelf, reroute, single-craton floor, stayers) → re-pin drifting fixtures → `make gate` → commit. A mechanism whose measured movement contradicts its Stage-0 ceiling is a falsification finding — record it, don't chase it (Sculpting's iteration-3 precedent).
- [ ] **Stop conditions (preregistered, spec §8):** floor reached → done; all probe-approved mechanisms built and last movement < +0.5 → stop, band-supersession package (Task 11). Guards breaking → the iteration is BLOCKED (Sculpting iteration-1 precedent): diagnose in the same commit or revert.

### Task 11: Census of Coasts IV, the verdict, and the G6 package

**Files:** `book/src/laboratory/census-of-coasts-iv.md` (final tables), `book/src/chronicle/rift-and-fit.md`, `docs/retrospectives/` (one page), `book/src/frontier/idea-registry.md` (MAP-21 status flip), `book/src/open-questions.md` (re-score the coast bet if moved), `docs/decisions/` (band supersession decision IF Nathan adopts; epoch-v4 decision record mirroring 0056)

- [ ] **Step 1:** Final probe tables: v3 → v4 movement per band; fit battery statement; fit-degradation number; `D_earth` and the proposed band beside the measured median; the verdict against BOTH floors (9.51 and the Earth-anchored proposal) — recorded exactly as measured.
- [ ] **Step 2:** **HARD STOP (G6): present Nathan** the post-G3 ledger digest (save-format entries first), the band-supersession adoption question with all numbers, and the v4 canonical census regen authorization request.
- [ ] **Step 3 (post-ruling):** decision records; chronicle; retro (promote the followup register); registry flips; book freshness sweep; `make gate` green.
- [ ] **Step 4:** Close via **closing-a-campaign** (absorb main, keystone refreeze, merge, the AWS regen per Nathan's authorization, push per standing practice).

## Self-review notes (writing-plans checklist)

- Spec coverage: §1→T1/T2/T10; §2→globals+T6; §3→T3-T6; §4→T6/T7; §5→T10; §6→T1; §7→T2/T9; §8→T5/T6/T10; §9→T8; §10 non-goals honored (no plates task, no estimator change); §11 staging matches.
- Type consistency: `RiftHistory`/`Seam`/`seam_side`/`rotation_for`/`rotate`/`clip_at`/`new_with_rift`/`assemble_cratons` names uniform across T3–T10.
- Known verify-on-site items are marked "confirm" inline (exact regenerate-artifacts invocation, predicate registration site, NearestCellIndex ctor, study metric list) — each is a read, not a design gap.
