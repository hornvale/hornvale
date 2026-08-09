# The Hollow Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make a cave's existence, kind, and depth derive from three different
fields instead of one, so that all three `CaveKind` variants and a range of
depths can actually occur, and so the presence gate is a real probability.

**Architecture:** Invert the derivation order to match the sibling
`deposit_kind`: select the kind *first* from the field its process requires,
then gate existence on that kind's own proneness through an fbm sample warped
monotonically to uniform, then read depth from the 5-band stratigraphic column.
All of it stays a pure hash-noise point process — no stream draws, no facts, no
epoch.

**Tech Stack:** Rust 2024, `domains/terrain` (the model),
`windows/worldgen` (the readout battery). No new dependencies — the CDF warp
uses `hornvale_kernel::math::tanh`, already routed through the pinned `libm`.

**Spec:** `docs/superpowers/specs/2026-08-05-the-hollow-design.md` (G3-approved
2026-08-05, commit `2808f59d`).

**Status: COMPLETE** — code-complete, awaiting merge. Tasks 0–7 executed. Three
of this plan's own instructions were amended during execution and the
amendments are recorded in
[the retrospective](../../retrospectives/the-hollow.md): Task 1's delete
instruction had a live caller, Task 5's "constants only" restriction was not
executable (two formulas were structurally wrong), and Task 2's uniformity test
measured one globe and would have driven an over-fit to seed 42.

## Global Constraints

- **No new crates.** Dependency allowlist is `serde`, `serde_json`, `libm`
  (`ALLOWED_EXTERNAL` in `cli/tests/architecture.rs`; decisions 0004/0041).
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only. Float sorting
  uses `total_cmp` with a deterministic tie-break (`clippy.toml`
  `disallowed-types`, enforced workspace-wide **including test code**).
- **No `std::time::Instant`**, no wall-clock anywhere, including tests.
- **Every transcendental goes through `hornvale_kernel::math`** — never
  `f64::tanh` directly. `floor`/`sqrt` stay intrinsic.
- **Every public item, field and variant gets a one-line doc comment**
  (`#![warn(missing_docs)]` on every crate).
- **Every primitive at a `pub` boundary carries a `type-audit:` verdict tag**
  (`bare-ok(<class>)` / `waiver(<reason>)`). Tags go stale silently on a
  signature change — re-check the tag whenever you touch a signature.
- **`cargo fmt` is the final step before every commit.** Skipping it is the
  single most common review finding in this repo.
- **No epoch, no new stream label.** `cave_at` is a pure query; nothing here
  touches a save-format contract.
- **Do not modify `sphere_fbm01`, `deposit_at`, or `prehuman_scar_at`.** The
  spec's §3.2 and §5 turn on leaving all three alone.

---

## File Structure

| File | Responsibility | Task |
|---|---|---|
| `windows/worldgen/tests/hollow_readout.rs` | **create** — the campaign's measuring instrument: five numbers over 30 seeds | 0, 5 |
| `docs/superpowers/specs/2026-08-05-the-hollow-design.md` | **modify** — append §10, the measured baseline | 0 |
| `domains/terrain/src/features.rs` | **modify** — per-kind proneness, the kind-first selector, the CDF warp, `Cave`'s field | 1, 2, 3 |
| `domains/terrain/src/provider.rs` | **modify** — rewire `cave_at` onto the new pieces | 4 |
| `domains/terrain/src/lib.rs` | **modify** — re-export the new public functions | 1 |

`domains/terrain/src/render.rs` needs **no change**: `cave_color` reads only
`cave.kind`, and `CaveKind`'s variants are unchanged.

---

### Task 0: The measuring instrument, and the frozen baseline

Builds the readout that every later task is judged by, and records what the
model does *today* — before any behavioural change — so the campaign's claims
are diffs against a committed number rather than against memory.

**Files:**
- Create: `windows/worldgen/tests/hollow_readout.rs`
- Modify: `docs/superpowers/specs/2026-08-05-the-hollow-design.md` (append §10)

**Interfaces:**
- Consumes: `hornvale_worldgen::{BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts}`, `hornvale_terrain::{CaveKind, TerrainPins}`, `hornvale_astronomy::SkyPins`, `hornvale_kernel::{CellId, Seed}`
- Produces: `fn measure(seeds) -> Readout` and a printed report. Task 5 adds an
  assertion test to this same file.

- [ ] **Step 1: Write the reporting battery**

Create `windows/worldgen/tests/hollow_readout.rs`. It measures all five §4
numbers and prints them; it asserts only that the harness itself ran (land
cells were found), so it is green before and after the fix. The preregistered
*criteria* land in Task 5.

```rust
//! The Hollow's measuring instrument: the five numbers the campaign moves.
//!
//! Deliberately a REPORT, not a judgement — the preregistered criteria live in
//! `cave_substrate_meets_preregistered_criteria` (Task 5) and in the spec's §4
//! table. This battery exists so the baseline and the readout are produced by
//! the identical code path.
//!
//! **Land** is `!terrain.is_ocean(cell)` — the predicate `cave_at` itself gates
//! on internally, so no second land test is introduced.
//!
//! Built to `BuildDepth::Terrain`, the shallowest rung producing terrain:
//! caves are a terrain-only feature and nothing here reads climate or
//! settlements.
//!
//! Test fixture (decision 0092): calls the derivation entry point directly,
//! the sanctioned test-fixture posture.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{CellId, Seed};
use hornvale_terrain::{CaveKind, TerrainPins};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};
use std::collections::BTreeSet;

/// Seeds measured. Matches C2a's `deep_realm_substrate.rs` so the two
/// campaigns' numbers are directly comparable.
const SEEDS: std::ops::RangeInclusive<u64> = 1..=30;

/// The `presence_prob` buckets the gate-calibration readout reports, as
/// `[low, high)` pairs. Chosen to match the spec's §2.3 table exactly.
const PROB_BUCKETS: [(f64, f64); 6] = [
    (0.00, 0.05),
    (0.20, 0.25),
    (0.25, 0.30),
    (0.30, 0.35),
    (0.35, 0.40),
    (0.40, 0.45),
];

/// Everything the campaign measures, accumulated over all seeds.
#[derive(Default)]
struct Readout {
    /// Worlds measured.
    worlds: usize,
    /// Worlds with no cave at all.
    caveless_worlds: usize,
    /// Land cells (`!is_ocean`) across all worlds.
    land: usize,
    /// Land cells carrying a cave.
    caves: usize,
    /// Per-world cave fraction of land, one entry per seed.
    per_world_fraction: Vec<f64>,
    /// Cave cells by kind, in `CaveKind` declaration order.
    kinds: [usize; 3],
    /// Cave cells by `deepest_band`, in `BandKind` declaration order
    /// (Regolith, Cover, Basement, Roots, Underneath).
    bands: [usize; 5],
    /// Cave cells with at least one caved neighbour.
    clustered: usize,
    /// Cave cells with no caved neighbour.
    solitary: usize,
    /// Per `PROB_BUCKETS` entry: (land cells in bucket, caves in bucket).
    gate: [(usize, usize); 6],
}
```

Note the `bands` field is `[usize; 5]` from the start — Task 3 changes `Cave`'s
depth field to a `BandKind`, and having the readout already shaped for it
avoids rewriting the instrument mid-campaign. Until Task 3 lands, populate it
from the current `depth_reach_bands` (`1..=4` maps to index `band - 1`).

- [ ] **Step 2: Write the measurement body**

Append to the same file:

```rust
/// Verbatim copies of `features::belt_weight` / `presence_prob`, which are
/// `pub` inside `hornvale_terrain` but not re-exported from its crate root.
/// If either formula changes, this harness must change with it — the gate
/// readout is meaningless otherwise.
fn belt_weight(hops: Option<u32>) -> f64 {
    match hops {
        Some(h) => (1.0 / (1.0 + h as f64 * 0.1)).max(0.3),
        None => 0.3,
    }
}

/// See [`belt_weight`].
fn presence_prob(field: f64, belt: f64) -> f64 {
    (field * (0.4 + 0.6 * belt)).clamp(0.0, 1.0)
}

/// Build one seed to `BuildDepth::Terrain` and fold its land cells into `out`.
fn measure_one(seed: Seed, wc: &WorldComponents, out: &mut Readout) {
    let artifacts = build_world_to_with_artifacts(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        BuildDepth::Terrain,
    )
    .unwrap_or_else(|e| panic!("{seed:?} failed to build: {e:?}"));
    let terrain = artifacts
        .terrain
        .unwrap_or_else(|| panic!("{seed:?} at BuildDepth::Terrain produced no terrain"));
    let geo = terrain.geosphere();

    let mut cave_set: BTreeSet<CellId> = BTreeSet::new();
    let (mut world_land, mut world_caves) = (0usize, 0usize);

    for cell in geo.cells() {
        if terrain.is_ocean(cell) {
            continue;
        }
        world_land += 1;

        let prob = presence_prob(
            terrain.cave_proneness_at(cell),
            belt_weight(terrain.boundary_distance_at(cell)),
        );
        let bucket = PROB_BUCKETS
            .iter()
            .position(|&(lo, hi)| prob >= lo && prob < hi);

        let cave = terrain.cave_at(cell);
        if let Some(cave) = cave {
            world_caves += 1;
            cave_set.insert(cell);
            out.kinds[match cave.kind {
                CaveKind::Karst => 0,
                CaveKind::LavaTube => 1,
                CaveKind::Fracture => 2,
            }] += 1;
            out.bands[(cave.depth_reach_bands.clamp(1, 4) - 1) as usize] += 1;
        }
        if let Some(b) = bucket {
            out.gate[b].0 += 1;
            if cave.is_some() {
                out.gate[b].1 += 1;
            }
        }
    }

    for &cell in &cave_set {
        if geo.neighbors(cell).iter().any(|nb| cave_set.contains(nb)) {
            out.clustered += 1;
        } else {
            out.solitary += 1;
        }
    }

    out.worlds += 1;
    out.land += world_land;
    out.caves += world_caves;
    if world_caves == 0 {
        out.caveless_worlds += 1;
    }
    out.per_world_fraction.push(if world_land == 0 {
        0.0
    } else {
        world_caves as f64 / world_land as f64
    });
}

/// Measure every seed in `SEEDS`.
fn measure() -> Readout {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let mut out = Readout::default();
    for seed in SEEDS {
        measure_one(Seed(seed), &wc, &mut out);
    }
    out
}
```

- [ ] **Step 3: Write the report printer and the battery test**

```rust
/// Print the five numbers, in the spec's §4 order.
fn report(r: &Readout) {
    println!("== The Hollow readout — {} worlds, {} land cells", r.worlds, r.land);
    println!(
        "prevalence: {} caves = {:.4}% of land; {} of {} worlds have NO cave",
        r.caves,
        100.0 * r.caves as f64 / r.land as f64,
        r.caveless_worlds,
        r.worlds
    );

    let mut sorted = r.per_world_fraction.clone();
    sorted.sort_by(f64::total_cmp);
    let pct = |q: f64| -> f64 {
        if sorted.is_empty() {
            return 0.0;
        }
        let idx = ((sorted.len() - 1) as f64 * q) as usize;
        sorted[idx]
    };
    println!(
        "per-world cave fraction: p50={:.5} p90={:.5} max={:.5}",
        pct(0.5),
        pct(0.9),
        pct(1.0)
    );

    let names = ["Karst", "LavaTube", "Fracture"];
    for (i, name) in names.iter().enumerate() {
        println!(
            "kind {name}: {} ({:.4}% of caves)",
            r.kinds[i],
            if r.caves == 0 { 0.0 } else { 100.0 * r.kinds[i] as f64 / r.caves as f64 }
        );
    }

    let bands = ["Regolith", "Cover", "Basement", "Roots", "Underneath"];
    for (i, name) in bands.iter().enumerate() {
        println!(
            "band {name}: {} ({:.4}% of caves)",
            r.bands[i],
            if r.caves == 0 { 0.0 } else { 100.0 * r.bands[i] as f64 / r.caves as f64 }
        );
    }

    let placed = r.clustered + r.solitary;
    println!(
        "clustering: {} clustered / {} solitary = {:.4}%",
        r.clustered,
        r.solitary,
        if placed == 0 { 0.0 } else { 100.0 * r.clustered as f64 / placed as f64 }
    );

    println!("gate calibration — nominal presence_prob vs realized hit rate:");
    for (i, &(lo, hi)) in PROB_BUCKETS.iter().enumerate() {
        let (cells, hits) = r.gate[i];
        if cells == 0 {
            continue;
        }
        println!(
            "  [{lo:.2},{hi:.2})  cells={cells:>8}  caves={hits:>7}  realized={:.5}  nominal~{:.3}",
            hits as f64 / cells as f64,
            (lo + hi) / 2.0
        );
    }
}

#[test]
fn report_cave_substrate() {
    let r = measure();
    report(&r);
    assert!(r.land > 0, "the harness found no land cells — it is measuring nothing");
    assert_eq!(r.worlds, 30, "expected 30 worlds");
}
```

- [ ] **Step 4: Run it and capture the baseline**

Run:
```bash
cargo test -p hornvale-worldgen --test hollow_readout -- --nocapture 2>&1 | tee /tmp/hollow-baseline.txt
```
Expected: PASS, with the report printed. Expect roughly the spec's §2 numbers
(0.26% of land, 100% Karst, 100% one band, ~96.7% clustered, 3/30 caveless).

**If the runtime exceeds 60 seconds**, add
`#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]`
to `report_cave_substrate` — that reason string is matched **verbatim** by
`cli/tests/heavy_tier.rs`, not as a prefix, so do not reword it. Record the
measured runtime in the commit message either way.

- [ ] **Step 5: Append the baseline to the spec**

Add a `## 10. Baseline, measured` section to
`docs/superpowers/specs/2026-08-05-the-hollow-design.md` containing the printed
report verbatim inside a fenced block, prefixed with the commit SHA it was
measured at and the command that produced it. Do not edit §4's criteria.

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt
make gate
git add windows/worldgen/tests/hollow_readout.rs docs/superpowers/specs/2026-08-05-the-hollow-design.md
git commit -m "test(hollow): the measuring instrument, and the baseline it froze"
```

`make gate` on this box takes 15-37 minutes — budget a 3600000 ms timeout.

---

### Task 1: Per-kind proneness, and the kind-first selector

Replaces "gate on carbonate, then ask kind" with "ask which process this rock
supports, then gate on that process". This is the task that makes `LavaTube`
and `Fracture` reachable.

**Files:**
- Modify: `domains/terrain/src/features.rs`
- Modify: `domains/terrain/src/lib.rs` (re-export)

**Interfaces:**
- Consumes: `crate::lithology::{MaterialBuffer, cave_proneness}`, `crate::boundaries::BoundaryKind`
- Produces:
  - `pub fn lavatube_proneness(buf: &MaterialBuffer, crust_age: f64) -> f64`
  - `pub fn fracture_proneness(buf: &MaterialBuffer, boundary_distance: Option<u32>) -> f64`
  - `pub fn cave_process(buf: &MaterialBuffer, drainage: f64, crust_age: f64, boundary_distance: Option<u32>) -> Option<(CaveKind, f64)>`
  - Task 4 calls `cave_process`; Task 3 calls nothing from here.

- [ ] **Step 1: Write the failing tests**

Add to `domains/terrain/src/features.rs`'s `mod tests`:

The existing `mod tests` already has a `buf(carbonate, silica) -> MaterialBuffer`
helper (features.rs:236) which sets `grain: 0.5, induration: 0.5,
metamorphic_grade: 0.0, porosity: 0.5`. **Reuse it and mutate the fields each
test is about** — do not write a second fixture builder.

```rust
#[test]
fn each_kind_is_selectable_by_its_own_field() {
    // Carbonate platform, wet, porous -> Karst.
    let mut karst = buf(0.7, 0.5);
    karst.porosity = 0.8;
    let (kind, p) = cave_process(&karst, 500.0, 0.5, Some(4)).expect("karst rock hosts a cave");
    assert_eq!(kind, CaveKind::Karst);
    assert!(p > 0.0, "selected kind must carry a positive proneness");

    // Young mafic fine-grained rock, no carbonate -> LavaTube.
    let mut lava = buf(0.0, 0.1);
    lava.grain = 0.1;
    let (kind, _) = cave_process(&lava, 0.0, 0.05, None).expect("young basalt hosts a cave");
    assert_eq!(kind, CaveKind::LavaTube);

    // Hard unmetamorphosed rock right on a plate contact -> Fracture.
    let mut frac = buf(0.0, 0.7);
    frac.induration = 0.95;
    let (kind, _) = cave_process(&frac, 0.0, 0.9, Some(0)).expect("brittle fault rock hosts a cave");
    assert_eq!(kind, CaveKind::Fracture);
}

#[test]
fn a_cell_supporting_no_process_hosts_no_cave() {
    // Nothing to dissolve, fully felsic (no tube), perfectly plastic (nothing
    // to fracture).
    let mut inert = buf(0.0, 1.0);
    inert.induration = 0.0;
    assert_eq!(cave_process(&inert, 0.0, 0.9, None), None);
}

#[test]
fn selection_takes_the_strongest_process_not_a_fixed_order() {
    // Weak carbonate against strong fracture conditions: fracture must win,
    // which a Karst-first priority ladder would get wrong.
    let mut b = buf(0.05, 0.7);
    b.porosity = 0.05;
    b.induration = 1.0;
    let (kind, _) = cave_process(&b, 0.0, 0.9, Some(0)).expect("hosts a cave");
    assert_eq!(kind, CaveKind::Fracture);
}
```

Note `buf`'s defaults already give `metamorphic_grade: 0.0` (fully brittle) and
`margin`/`soil_depth`/`basement` values that none of these functions read.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-terrain --lib features::tests`
Expected: FAIL — `cannot find function cave_process in this scope`.

- [ ] **Step 3: Implement**

Replace `cave_kind` in `domains/terrain/src/features.rs` with the following.
Delete `cave_kind` entirely — the old presence-then-kind function has no
remaining caller after Task 4, and leaving it would leave the dead-branch bug
reachable.

```rust
/// Felsic index at or below which rock reads as mafic enough to have flowed
/// as basalt. Matches the `silica < 0.3` boundary the retired `cave_kind`
/// used, kept so the taxonomy's meaning does not silently shift.
const MAFIC_SILICA_MAX: f64 = 0.3;

/// Lava-tube proneness, `[0,1]`: a drained basaltic flow. Needs mafic rock
/// (low `silica`), extrusive texture (fine `grain` — a pluton never flowed),
/// and young crust, because old tubes collapse and are buried.
/// type-audit: bare-ok(ratio: crust_age), bare-ok(ratio: return)
pub fn lavatube_proneness(buf: &MaterialBuffer, crust_age: f64) -> f64 {
    let mafic = ((MAFIC_SILICA_MAX - buf.silica) / MAFIC_SILICA_MAX).clamp(0.0, 1.0);
    let extrusive = (1.0 - buf.grain).clamp(0.0, 1.0);
    let youth = (1.0 - crust_age).clamp(0.0, 1.0);
    (mafic * extrusive * youth).clamp(0.0, 1.0)
}

/// Fracture proneness, `[0,1]`: a fault void. Needs stress (proximity to a
/// plate contact) and rock that breaks rather than flows — hard and
/// unmetamorphosed. Reuses [`belt_weight`] for the stress term so fracture
/// caves and ore belts read the same lineament field.
/// type-audit: bare-ok(count: boundary_distance), bare-ok(ratio: return)
pub fn fracture_proneness(buf: &MaterialBuffer, boundary_distance: Option<u32>) -> f64 {
    let stress = belt_weight(boundary_distance);
    let brittle = buf.induration * (1.0 - buf.metamorphic_grade);
    (stress * brittle).clamp(0.0, 1.0)
}

/// The void-opening process this cell's rock best supports, with that
/// process's own proneness — `None` where no process operates.
///
/// **Kind is chosen BEFORE existence is tested**, mirroring [`deposit_kind`].
/// The retired `cave_kind` was asked only after a carbonate-gated existence
/// test had already passed, so its two non-`Karst` branches — both of which
/// require carbonate to be LOW — were unreachable (The Hollow, spec §2.1).
///
/// Selection is argmax over the three prononesses rather than a priority
/// ladder, so the mix follows the fields instead of a hand-chosen order.
/// Ties break by `total_cmp` with declaration order as the deterministic
/// tie-break.
/// type-audit: bare-ok(count: drainage), bare-ok(ratio: crust_age), bare-ok(count: boundary_distance)
pub fn cave_process(
    buf: &MaterialBuffer,
    drainage: f64,
    crust_age: f64,
    boundary_distance: Option<u32>,
) -> Option<(CaveKind, f64)> {
    let candidates = [
        (CaveKind::Karst, crate::lithology::cave_proneness(buf, drainage)),
        (CaveKind::LavaTube, lavatube_proneness(buf, crust_age)),
        (CaveKind::Fracture, fracture_proneness(buf, boundary_distance)),
    ];
    let best = candidates
        .iter()
        .copied()
        .enumerate()
        // max_by returns the LAST maximum on a tie; negate the index so the
        // earliest-declared kind wins instead.
        .max_by(|(ia, (_, a)), (ib, (_, b))| {
            a.total_cmp(b).then_with(|| ib.cmp(ia))
        })
        .map(|(_, kv)| kv)?;
    if best.1 <= 0.0 { None } else { Some(best) }
}
```

The test module needs `use crate::lithology::{Basement, MarginPolarity, SoilDepth};`
added to its imports if not already present — check the existing `mod tests`
header before assuming.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-terrain --lib features::tests`
Expected: PASS. The old `cave_kind` tests at features.rs:253-255 now reference
a deleted function — **delete those three assertions**, they tested the
inverted-branch behaviour this task removes.

- [ ] **Step 5: Re-export**

In `domains/terrain/src/lib.rs`, add `cave_process`, `lavatube_proneness` and
`fracture_proneness` to the `pub use features::{...}` list. Remove `cave_kind`
if it appears there.

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
make gate
git add domains/terrain/src/features.rs domains/terrain/src/lib.rs
git commit -m "feat(terrain): choose a cave's kind before testing its existence"
```

---

### Task 2: A gate that is actually a probability

Warps the fbm sample through its own CDF so `noise_u < prob` is a genuine
Bernoulli trial. Monotone, so the spatial clustering is preserved by
construction.

**Files:**
- Modify: `domains/terrain/src/features.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::math::tanh`
- Produces: `pub fn uniformize(noise: f64) -> f64`. Task 4 calls it.

- [ ] **Step 1: Write the failing tests**

Add to `domains/terrain/src/features.rs`'s `mod tests`. The uniformity test
samples the *real* noise field over a real globe, because the whole defect was
an assumption about a distribution nobody measured.

```rust
#[test]
fn uniformize_is_monotone_and_bounded() {
    let mut prev = -1.0;
    for i in 0..=1000 {
        let x = i as f64 / 1000.0;
        let u = uniformize(x);
        assert!((0.0..=1.0).contains(&u), "uniformize({x}) = {u} escaped [0,1]");
        assert!(u >= prev, "uniformize is not monotone at {x}: {u} < {prev}");
        prev = u;
    }
}

#[test]
fn uniformize_turns_the_cave_gate_noise_into_a_uniform_variate() {
    use crate::provider::GeneratedTerrain;
    use crate::{TerrainPins, generate};
    use hornvale_kernel::{Geosphere, Seed};

    let geo = Geosphere::new(5);
    let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
    let terrain = GeneratedTerrain::new(geo.clone(), outcome);

    let mut deciles = [0usize; 10];
    let mut n = 0usize;
    for cell in geo.cells() {
        let raw = crate::crust::sphere_fbm01(
            terrain.globe().features_noise_seed(),
            geo.position(cell),
            CAVE_GATE_FREQ,
            CAVE_GATE_OCTAVES,
        );
        let u = uniformize(raw);
        deciles[((u * 10.0) as usize).min(9)] += 1;
        n += 1;
    }

    for (i, &count) in deciles.iter().enumerate() {
        let share = count as f64 / n as f64;
        assert!(
            (share - 0.1).abs() < 0.035,
            "decile {i} holds {share:.4} of samples, not ~0.10 — the warp did \
             not uniformize the field (n={n}, deciles={deciles:?})"
        );
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-terrain --lib features::tests::uniformize`
Expected: FAIL — `cannot find function uniformize in this scope`.

- [ ] **Step 3: Implement**

Add to `domains/terrain/src/features.rs`:

```rust
/// Spatial frequency of the cave presence gate's noise field. Named here
/// rather than inlined at the call site so the calibration in [`uniformize`]
/// and the field it calibrates against cannot drift apart.
pub const CAVE_GATE_FREQ: f64 = 5.0;
/// Octave count of the cave presence gate's noise field. See [`CAVE_GATE_FREQ`].
pub const CAVE_GATE_OCTAVES: u32 = 4;

/// Mean of `sphere_fbm01` at [`CAVE_GATE_FREQ`]/[`CAVE_GATE_OCTAVES`],
/// measured — not assumed. See [`uniformize`].
const GATE_NOISE_MEAN: f64 = 0.495;
/// Standard deviation of the same field, measured. See [`uniformize`].
const GATE_NOISE_SD: f64 = 0.058;

/// Map an fbm sample onto a uniform `[0,1]` variate, so that comparing it
/// against a probability is a genuine Bernoulli trial.
///
/// **Why this exists.** `sphere_fbm01` returns values massed near 0.5, not
/// spread uniformly: measured over land, `P(noise < 0.35) = 0.014` and
/// `P(noise < 0.40) = 0.051`. Comparing a probability directly against it —
/// which the model did from The Lode until The Hollow — makes
/// [`presence_prob`] a probability in name only, firing a nominal 0.35 at
/// 1.4% (spec §2.3).
///
/// **Why a monotone warp specifically.** The noise serves two purposes at
/// once: it sets the presence *rate* and it makes features *cluster*. A
/// monotone transform preserves the spatial ordering exactly, so clustering
/// is untouched by construction while the marginal is corrected — the one
/// repair that fixes the first purpose without touching the second.
///
/// **Why it is applied here and not inside `sphere_fbm01`.** Two other
/// callers depend on the raw distribution: `deposit_at` feeds the sample to
/// `deposit_grade_tonnage` as a *value*, and `prehuman_scar_at` compares it
/// against a threshold calibrated against exactly this marginal. Changing the
/// shared function would break both.
///
/// The transform is the normal CDF via the standard tanh approximation
/// (accurate to ~1e-4), which needs only `hornvale_kernel::math::tanh` and so
/// stays on the pinned `libm` path.
/// type-audit: bare-ok(ratio: noise), bare-ok(ratio: return)
pub fn uniformize(noise: f64) -> f64 {
    /// Coefficient of the tanh approximation to the normal CDF.
    const A: f64 = 0.7988;
    /// Cubic correction term of the same approximation.
    const B: f64 = 0.044_17;
    let z = (noise - GATE_NOISE_MEAN) / GATE_NOISE_SD;
    (0.5 * (1.0 + hornvale_kernel::math::tanh(A * z * (1.0 + B * z * z)))).clamp(0.0, 1.0)
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-terrain --lib features::tests::uniformize -- --nocapture`
Expected: PASS.

**If the decile test fails**, the printed `deciles` array is the correction:
re-fit `GATE_NOISE_MEAN`/`GATE_NOISE_SD` to the measured field and re-run.
Record the measured mean and SD in the commit message. This is calibration
against a measurement, which is the point — do not widen the tolerance to make
it pass.

- [ ] **Step 5: Gate and commit**

```bash
cargo fmt
make gate
git add domains/terrain/src/features.rs
git commit -m "fix(terrain): make the cave presence gate an actual probability"
```

---

### Task 3: Depth from the column, typed like its sibling

Replaces `depth_reach_bands: u32` with `deepest_band: BandKind`, so a
subsurface feature's depth is named geology — the form `deposit_depth` already
established.

**Files:**
- Modify: `domains/terrain/src/features.rs`

**Interfaces:**
- Consumes: `crate::strata::{BandKind, StratigraphicColumn}`
- Produces:
  - `pub struct Cave { pub kind: CaveKind, pub deepest_band: BandKind }`
  - `pub fn cave_depth(kind: CaveKind, column: &StratigraphicColumn, proneness: f64) -> BandKind`
  - Task 4 constructs `Cave` with the new field.

- [ ] **Step 1: Write the failing tests**

`strata::column` takes **seven** arguments, verified at
`domains/terrain/src/strata.rs:157`:
`column(crust_thickness_km, crust_age, continental, sediment_m, soil_depth_m,
surface_rock: RockClass, basement: Basement)`.

`unconformity` fires when `soil_depth_m + sediment_m < 200.0 && crust_age >
0.6` (strata.rs), which is what makes the two fixtures below differ.

```rust
#[test]
fn cave_depth_differs_by_kind() {
    use crate::strata::{BandKind, column};
    // Thick cover (401 m) on young crust: no unconformity.
    let thick = column(35.0, 0.3, true, 400.0, 1.0, RockClass::Sandstone, Basement::Continental);
    assert!(!thick.unconformity, "fixture must NOT be an unconformity");
    assert_eq!(cave_depth(CaveKind::Karst, &thick, 0.2), BandKind::Cover);
    assert_eq!(cave_depth(CaveKind::LavaTube, &thick, 0.9), BandKind::Cover);
    assert_eq!(cave_depth(CaveKind::Fracture, &thick, 0.2), BandKind::Basement);
}

#[test]
fn a_strong_process_reaches_one_band_deeper() {
    use crate::strata::{BandKind, column};
    let thick = column(35.0, 0.3, true, 400.0, 1.0, RockClass::Sandstone, Basement::Continental);
    assert_eq!(cave_depth(CaveKind::Karst, &thick, 0.9), BandKind::Basement);
    assert_eq!(cave_depth(CaveKind::Fracture, &thick, 0.9), BandKind::Roots);
}

#[test]
fn karst_on_thin_cover_reaches_the_basement_contact() {
    use crate::strata::{BandKind, column};
    // Thin cover (11 m) on ancient basement (age 0.9): an unconformity, so
    // dissolution reaches the contact however weak the process is.
    let thin = column(35.0, 0.9, true, 10.0, 1.0, RockClass::ReefLimestone, Basement::Continental);
    assert!(thin.unconformity, "fixture must actually be an unconformity");
    assert_eq!(cave_depth(CaveKind::Karst, &thin, 0.1), BandKind::Basement);
}

#[test]
fn a_lava_tube_never_leaves_the_cover() {
    use crate::strata::{BandKind, column};
    let thin = column(35.0, 0.9, true, 10.0, 1.0, RockClass::Basalt, Basement::Continental);
    assert_eq!(cave_depth(CaveKind::LavaTube, &thin, 1.0), BandKind::Cover);
}
```

`RockClass` and `Basement` are already imported by the test module
(features.rs:233-234). `BandKind` derives `Eq`, so `assert_eq!` works, and it
is re-exported from `hornvale_terrain`'s root (lib.rs:41) for Task 3 Step 5.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-terrain --lib features::tests::cave_depth`
Expected: FAIL — `cannot find function cave_depth in this scope`.

- [ ] **Step 3: Implement**

```rust
/// Proneness at or above which a process is strong enough to reach one band
/// deeper than its host.
const DEEP_PROCESS_PRONENESS: f64 = 0.5;

/// The deepest band a cave of this kind penetrates, given the cell's column.
///
/// Mirrors [`deposit_depth`], which types an ore body's depth as a named
/// [`BandKind`] rather than a count. The retired `depth_reach_bands` was
/// `1 + (cave_proneness * 3.0) as u32`, which could not reach band 3 (it
/// needed proneness >= 2/3 against a theoretical ceiling of 0.573) nor band 4
/// (it needed exactly 1.0), so every cave in every world sat at band 2
/// (spec §2.2). A band derived from bands cannot reproduce that failure.
///
/// This restores The Lode's own §5 intent — "depth-reach from `cave_proneness`
/// x the cover/carbonate band depth" — whose band-depth half was never
/// implemented.
/// type-audit: bare-ok(ratio: proneness)
pub fn cave_depth(
    kind: CaveKind,
    column: &crate::strata::StratigraphicColumn,
    proneness: f64,
) -> crate::strata::BandKind {
    use crate::strata::BandKind;
    let strong = proneness >= DEEP_PROCESS_PRONENESS;
    match kind {
        // Dissolution works the sedimentary cover, and reaches the basement
        // contact where the cover is thin on ancient rock (an unconformity)
        // or where the process is strong.
        CaveKind::Karst => {
            if strong || column.unconformity {
                BandKind::Basement
            } else {
                BandKind::Cover
            }
        }
        // A tube is the flow it drained out of, so it never leaves the cover.
        CaveKind::LavaTube => BandKind::Cover,
        // Faults cut crystalline rock, and deep ones reach the roots.
        CaveKind::Fracture => {
            if strong {
                BandKind::Roots
            } else {
                BandKind::Basement
            }
        }
    }
}
```

Then change the struct:

```rust
/// A located cave at a cell.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cave {
    /// Which process opened it.
    pub kind: CaveKind,
    /// The deepest band of the cell's column the void penetrates.
    pub deepest_band: crate::strata::BandKind,
}
```

Note `Cave` can now derive `Eq` (both fields are enums), where the old `u32`
+ `PartialEq` form could not. Confirm `BandKind` derives `Eq`; if it does not,
leave `Cave` on `PartialEq` alone rather than widening `BandKind`'s derives.
The struct's old `type-audit: bare-ok(count: depth_reach_bands)` tag must be
**deleted** — no primitive remains on the boundary.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-terrain --lib features::tests`
Expected: FAIL to compile — `provider.rs` still constructs `depth_reach_bands`.
That is Task 4. To keep this task independently committable, apply the
one-line provider change now:

```rust
// domains/terrain/src/provider.rs — inside cave_at, temporary until Task 4
let deepest_band = crate::features::cave_depth(kind, &self.column_at(id), self.cave_proneness_at(id));
Some(crate::features::Cave { kind, deepest_band })
```

Then re-run. Expected: PASS.

- [ ] **Step 5: Update the readout instrument**

`windows/worldgen/tests/hollow_readout.rs` reads `cave.depth_reach_bands`.
Change that line to index on the band:

```rust
out.bands[match cave.deepest_band {
    hornvale_terrain::BandKind::Regolith => 0,
    hornvale_terrain::BandKind::Cover => 1,
    hornvale_terrain::BandKind::Basement => 2,
    hornvale_terrain::BandKind::Roots => 3,
    hornvale_terrain::BandKind::Underneath => 4,
}] += 1;
```

Confirm `BandKind` is re-exported from `hornvale_terrain`'s crate root; if it
is not, add it to `lib.rs`'s `pub use`.

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt
make gate
git add domains/terrain/src/features.rs domains/terrain/src/provider.rs windows/worldgen/tests/hollow_readout.rs
git commit -m "refactor(terrain): a cave's depth is a band, not a count"
```

---

### Task 4: Rewire `cave_at`

Puts Tasks 1-3 into the live path. This is the commit where the world's caves
actually change.

**Files:**
- Modify: `domains/terrain/src/provider.rs:264-284`

**Interfaces:**
- Consumes: `features::{cave_process, uniformize, cave_depth, presence_prob, belt_weight, CAVE_GATE_FREQ, CAVE_GATE_OCTAVES}`
- Produces: the new `cave_at` behaviour. Task 5 measures it.

- [ ] **Step 1: Write the failing test**

Add to `domains/terrain/src/provider.rs`'s `mod tests`:

```rust
#[test]
fn cave_at_agrees_with_the_kind_first_gate() {
    let geo = Geosphere::new(3);
    let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
    let terrain = GeneratedTerrain::new(geo.clone(), outcome);
    for cell in geo.cells() {
        let expected = if terrain.is_ocean(cell) {
            None
        } else {
            crate::features::cave_process(
                &terrain.material_at(cell),
                terrain.drainage_at(cell),
                terrain.crust_age_at(cell),
                terrain.boundary_distance_at(cell),
            )
            .and_then(|(kind, proneness)| {
                let belt = crate::features::belt_weight(terrain.boundary_distance_at(cell));
                let prob = crate::features::presence_prob(proneness, belt);
                let noise = crate::features::uniformize(crate::crust::sphere_fbm01(
                    terrain.globe().features_noise_seed(),
                    geo.position(cell),
                    crate::features::CAVE_GATE_FREQ,
                    crate::features::CAVE_GATE_OCTAVES,
                ));
                (noise < prob).then(|| crate::features::Cave {
                    kind,
                    deepest_band: crate::features::cave_depth(
                        kind,
                        &terrain.column_at(cell),
                        proneness,
                    ),
                })
            })
        };
        assert_eq!(terrain.cave_at(cell), expected, "cell {cell:?} disagrees");
    }
}
```

This mirrors `prehuman_scar_at_matches_the_ancient_crust_and_noise_gate`, the
existing house pattern for pinning a provider query against its own
composition.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-terrain --lib provider::tests::cave_at_agrees`
Expected: FAIL — `cave_at` still gates on `cave_proneness_at` and raw noise, so
the two disagree on many cells.

- [ ] **Step 3: Implement**

Replace `cave_at`'s body (`domains/terrain/src/provider.rs`):

```rust
    /// The cave at a cell, if the fluid-flow point process places one.
    ///
    /// Kind is selected BEFORE existence is tested (`features::cave_process`),
    /// existence is gated on that kind's own proneness against a uniformized
    /// noise sample, and depth reads the cell's stratigraphic column — the
    /// three repairs of The Hollow (spec §3).
    pub fn cave_at(&self, id: CellId) -> Option<crate::features::Cave> {
        if self.is_ocean(id) {
            return None;
        }
        let (kind, proneness) = crate::features::cave_process(
            &self.material_at(id),
            self.drainage_at(id),
            self.crust_age_at(id),
            self.boundary_distance_at(id),
        )?;
        let belt = crate::features::belt_weight(self.boundary_distance_at(id));
        let prob = crate::features::presence_prob(proneness, belt);
        let pos = self.geosphere.position(id);
        let noise = crate::features::uniformize(crate::crust::sphere_fbm01(
            self.globe.features_noise_seed(),
            pos,
            crate::features::CAVE_GATE_FREQ,
            crate::features::CAVE_GATE_OCTAVES,
        ));
        if noise >= prob {
            return None;
        }
        Some(crate::features::Cave {
            kind,
            deepest_band: crate::features::cave_depth(kind, &self.column_at(id), proneness),
        })
    }
```

`cave_proneness_at` stays exactly as it is: its doc says "Cave/**karst**
void-proneness", which remains true — it is now the Karst term specifically,
and `render.rs`/the almanac do not read it.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-terrain`
Expected: PASS, including the pin-isolation battery in
`domains/terrain/tests/tectonic_properties.rs:726`
(`assert_eq!(base.cave_at(cell), pinned.cave_at(cell))`) — that test asserts a
pin consumes the same draws as the unpinned path, and this change consumes no
draws at all, so it must still hold. **If it fails, stop**: something in this
change touched stream consumption, which is a save-format contract.

- [ ] **Step 5: Gate and commit**

```bash
cargo fmt
make gate
git add domains/terrain/src/provider.rs
git commit -m "feat(terrain): cave_at derives kind, existence and depth from three fields"
```

---

### Task 5: Calibrate, then read out

The only task permitted to move a constant, and it does so against **stated
geological targets**, not against the §4 criteria. §4 then tests whether the
design can express those targets — which is not a test at all if the constants
were fitted to §4 directly.

**Files:**
- Modify: `domains/terrain/src/features.rs` (constants only)
- Modify: `windows/worldgen/tests/hollow_readout.rs` (add the assertion test)

- [ ] **Step 1: Run the readout against the new model**

Run:
```bash
cargo test -p hornvale-worldgen --test hollow_readout -- --nocapture 2>&1 | tee /tmp/hollow-after.txt
```
Record the output. Do not change anything yet.

- [ ] **Step 2: Calibrate against the geological targets**

The targets, stated before looking at step 1's output:

| Kind | Target share of caves | Why |
|---|---|---|
| `Karst` | dominant, 45-70% | karst is the canonical cave-forming process; carbonate platforms are ~13.6% of this model's land |
| `Fracture` | 20-45% | fault voids are widespread but individually small |
| `LavaTube` | 5-20% | genuinely rare, concentrated in young mafic provinces |

Overall prevalence target: **5-20% of land carries a cave**, from real karst
terrain's ~10-15% of continental land plus the two non-karst processes.

Adjust only `MAFIC_SILICA_MAX`, `DEEP_PROCESS_PRONENESS`, and the multiplicative
structure of `lavatube_proneness`/`fracture_proneness` to hit those targets.
**Do not adjust `GATE_NOISE_MEAN`/`GATE_NOISE_SD`** — those are measurements of
the noise field, pinned by Task 2's decile test, not free parameters.

Re-run step 1 after each change. Record every constant that moved, its before
and after value, and the target it was moved toward.

- [ ] **Step 3: Write the preregistered assertion test**

Append to `windows/worldgen/tests/hollow_readout.rs`. The thresholds are copied
**verbatim from the spec's §4 table** — do not soften one to make it pass.

```rust
/// The spec's §4 preregistered criteria, frozen at commit `2808f59d` before
/// any behavioural change. A failure here is a finding, not a defect to be
/// tuned away: see the campaign chronicle before touching a threshold.
#[test]
fn cave_substrate_meets_preregistered_criteria() {
    let r = measure();
    report(&r);

    // H1 — every kind occurs at >= 5% of cave cells.
    let names = ["Karst", "LavaTube", "Fracture"];
    for (i, name) in names.iter().enumerate() {
        let share = r.kinds[i] as f64 / r.caves as f64;
        assert!(share >= 0.05, "H1: {name} is {share:.4} of caves, under the 0.05 floor");
    }

    // H2 — at least 3 distinct bands occur, and the mode is under 90%.
    let distinct = r.bands.iter().filter(|&&c| c > 0).count();
    assert!(distinct >= 3, "H2: only {distinct} distinct depth bands occur");
    let modal = *r.bands.iter().max().expect("five bands") as f64 / r.caves as f64;
    assert!(modal < 0.90, "H2: the modal band holds {modal:.4} of caves");

    // H3 — prevalence off the floor, with an absurd-high ceiling.
    assert_eq!(r.caveless_worlds, 0, "H3: {} worlds have no cave", r.caveless_worlds);
    let mut sorted = r.per_world_fraction.clone();
    sorted.sort_by(f64::total_cmp);
    let median = sorted[sorted.len() / 2];
    assert!(median >= 0.02, "H3: median cave fraction {median:.4} is below 0.02");
    assert!(median <= 0.5, "H3: median cave fraction {median:.4} is absurdly high");

    // H4 — realized hit rate tracks nominal probability.
    for (i, &(lo, hi)) in PROB_BUCKETS.iter().enumerate() {
        let (cells, hits) = r.gate[i];
        if cells < 500 {
            continue; // too few samples for a rate to mean anything
        }
        let realized = hits as f64 / cells as f64;
        let nominal = (lo + hi) / 2.0;
        assert!(
            (realized - nominal).abs() / nominal < 0.25,
            "H4: bucket [{lo:.2},{hi:.2}) realized {realized:.5} against nominal {nominal:.3}"
        );
    }

    // H5 — GUARD. Clustering must survive the monotone warp. If this fails,
    // the warp was not monotone or fbm's spatial structure did not survive it,
    // and spec §3.2's central claim is false.
    let placed = r.clustered + r.solitary;
    let clustered = r.clustered as f64 / placed as f64;
    assert!(clustered >= 0.90, "H5: clustering fell to {clustered:.4}, under the 0.90 guard");
}
```

- [ ] **Step 4: Run it**

Run: `cargo test -p hornvale-worldgen --test hollow_readout -- --nocapture`
Expected: PASS.

**If a row fails and calibration against the §2 targets cannot fix it, that is
the campaign's finding — report it, do not retune to §4.** A falsified
prediction ships as the headline here; several campaigns have done exactly
that. The 3-attempt rule applies: after three failed calibration attempts on
one row, stop and report.

- [ ] **Step 5: Gate and commit**

```bash
cargo fmt
make gate
git add domains/terrain/src/features.rs windows/worldgen/tests/hollow_readout.rs
git commit -m "test(hollow): the preregistered readout, and the constants it judged"
```

The commit message must list every constant moved in step 2 with its before
and after value.

---

### Task 6: Regenerate the artifacts

**Files:**
- Modify: `book/src/gallery/almanac-seed-42{,-locked,-sky}.md`, `book/src/gallery/features-seed-42.{png,md}`, `docs/audits/type-audit-report.md`, both censuses under `book/src/laboratory/generated/`

- [ ] **Step 1: Regenerate everything except censuses**

```bash
make rebaseline
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
```
Expected: the three almanacs, `features-seed-42.png`, and the type-audit report
move. `book/src/reference/` should NOT move — no concepts or stream labels
changed. **If it does, stop and find out why**: it would mean this campaign
touched a registry or a stream label, which the spec says it does not.

- [ ] **Step 2: Look at the features map**

Open `book/src/gallery/features-seed-42.png`. Before The Hollow every cave on
it was the same blue (`[100, 180, 255]`, Karst). It should now show orange
(`LavaTube`) and brown (`Fracture`) as well. This is a **visual check with no
automated equivalent** — a consistent, parseable, wrong-colored map passes
every test in the repo.

- [ ] **Step 3: Check the almanac line changed**

```bash
grep -n "cave country" book/src/gallery/almanac-seed-42.md
```
Expected: no longer "0% of the land is cave country."

- [ ] **Step 4: Refresh the censuses**

```bash
bash scripts/census-run.sh status   # confirm no heavy run holds the box
bash scripts/census-run.sh          # ~7 min local; decision 0081
make lab-diff STUDY=the-census
make census-check
```
Expected from `lab-diff`: `cave-fraction` moves; **no other metric should**.
Any other moving metric is a finding — caves feed nothing else, so a second
moving column means something unexpected reads them.

- [ ] **Step 5: Commit**

```bash
git add book/ docs/audits/
git commit -m "chore(artifacts): regenerate for the cave model"
```

---

### Task 7: Close the campaign

- [ ] **Step 1: Invoke the closing skill**

Use the `closing-a-campaign` skill and walk its Definition-of-Done. Do not
hand-roll this list.

- [ ] **Step 2: Campaign-specific DoD items**

Beyond the standard walk:

- Chronicle entry in `book/src/chronicle/`. **Registry IDs are banned outside
  `book/src/frontier/`** — `cli/tests/docs_consistency.rs` scans `book/src` and
  fails on `MAP-…` in a chronicle. Name the concept, don't cite the row.
- Retrospective in `docs/retrospectives/the-hollow.md`, carrying at minimum:
  **a field nothing reads cannot be observed to be wrong** (`depth_reach_bands`
  was write-only on main), and **the inherited diagnosis was a hypothesis**
  (C2a's Task 0 measured outcomes and misattributed the cause; the dominant
  defect was invisible to it).
- Flip `MAP-cave-model-miscalibrated` to `shipped`, repoint **Where** at the
  chronicle. Confirm `MAP-point-ore-gate` and `MAP-cave-kind-gaps` still read
  true after the fix.
- A decision record (next free number is **0104**) for the distribution-shape
  lesson, if it survives review as more than a restatement of
  `PSY-distribution-shape`. Cite the measured CDF.
- Re-score `book/src/open-questions.md` only if this moved one of the
  Confidence Gradient's bets.

- [ ] **Step 3: Record the C2a handoff**

Append to the retrospective, under a heading C2a will look for:

- `the-deep-realm` carries a stale `MAP-cave-model-miscalibrated` row — **drop
  C2a's copy on resumption**, keep main's.
- `deep_realm_substrate.rs` reads `cave.depth_reach_bands` and will not
  compile; it now reads `cave.deepest_band`, a `BandKind`.
- C2a's Task 0 gate should be re-run against the new substrate before its plan
  resumes. Its 10-task plan and 11-entry ledger stand.

- [ ] **Step 4: Preflight and merge**

```bash
make preflight      # from the branch
make gate-full      # the heavy tier; its exit code is a CONSTANT non-zero by
                    # design while disposition_calibration is deliberately red
                    # — read the FAILURE LIST, not the exit code
```

Then G6: present the post-G3 ledger digest to Nathan and wait. **G6 is a hard
stop.**

---

## Self-Review

**Spec coverage.** §3.1 → Task 1. §3.2 → Task 2. §3.3 → Task 3. Live path →
Task 4. §4 preregistration → Tasks 0 and 5. §5 non-goals → enforced by the
Global Constraints' "do not modify" line. §6 blast radius → Task 6. §8 DoD →
Task 7. No spec section is unimplemented.

**Type consistency.** `cave_process` returns `Option<(CaveKind, f64)>` in Task
1 and is destructured as `(kind, proneness)` in Task 4 — consistent.
`cave_depth(kind, &column, proneness) -> BandKind` in Task 3, called with the
same argument order in Tasks 3 and 4. `uniformize(f64) -> f64` in Task 2,
called on the `sphere_fbm01` result in Task 4. `Cave.deepest_band` is
introduced in Task 3 and read in Tasks 3 (readout) and 4.

**Assumptions resolved while writing this plan** (each was wrong on first
draft, and would have cost an implementer a compile cycle):
- `strata::column` takes **seven** arguments, not five — the two extra are
  `surface_rock: RockClass` and `basement: Basement`. Task 3's fixtures use
  the real signature.
- `Basement`'s variants are `Continental`/`Oceanic` — there is no `Craton`.
- `SoilDepth` is a newtype (`SoilDepth::new(f64)`), not an enum, and
  `MarginPolarity::Interior` is the neutral value. The plan now reuses the
  existing `buf()` fixture rather than hand-rolling a struct literal.
- `BandKind` derives `Eq` and IS re-exported from `hornvale_terrain`'s root.

**Assumptions that remain, and cannot be resolved without running code:**
- `GATE_NOISE_MEAN`/`GATE_NOISE_SD` are fitted from land-only bucket data.
  Task 2's decile test measures the true marginal and corrects them — this is
  expected, not a failure.
- Whether Task 0's battery exceeds 60 s. A 10-world probe ran in 4.9 s warm, so
  30 worlds should be ~15 s, but Task 0 measures and decides.
- Whether the per-kind proneness formulas hit the §2 geological targets at
  their first constants. Task 5 exists for exactly this and is the campaign's
  likeliest source of iteration.
