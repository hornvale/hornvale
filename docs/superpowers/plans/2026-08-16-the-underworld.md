# The Underworld Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the underworld a habitation depth coordinate that varies with
the thing that makes depth matter, populate it with communities expressed in
The Axes' basis, and restore Mountain-dwarf and Duergar onto it — if and only
if a measurement taken first says they separate.

**Architecture:** `BandKind` keeps its five stratigraphic rungs and its entire
archival job. A new **delve ladder**, spaced by temperature offset above the
surface datum via the already-shipped `geothermal_gradient`, carries habitation
depth; `ChamberAddr.band` is re-pointed at it. A derived water table splits
each column into vadose and phreatic. `subterranean_substrate` stops returning
constants. Underworld communities become `EnvironmentVector`s; `EnvironmentNiche`
becomes their species-side counterpart. The bake's `node_index` is re-keyed to
`(CellId, Rung)`.

**Tech Stack:** Rust 2024, `hornvale-kernel` / `hornvale-terrain` /
`hornvale-climate` / `hornvale-species` / `hornvale-demography` /
`hornvale-worldgen` / `hornvale-language`, `cargo nextest`, the project's own
type-audit tool.

**Spec:** [The Underworld](../specs/2026-08-16-the-underworld-design.md) ·
**Program:** [The Chorography](../specs/2026-08-12-the-chorography-metaplan.md)

## Global Constraints

- **No new dependencies.** `serde`, `serde_json`, `libm` only.
- **No `HashMap`/`HashSet`.** `BTreeMap` / `BTreeSet` / `Vec` only, enforced by
  `clippy.toml` `disallowed-types`.
- **No wall-clock time.** `std::time::Instant` is banned in test code too.
- **Every `pub` item gets a one-line doc comment** (`#![warn(missing_docs)]`).
- **Every primitive at a `pub` boundary carries a `type-audit:` verdict tag**,
  and `docs/audits/type-audit-report.md` is regenerated **in the same commit**
  that adds the `pub` item.
- **`cargo fmt` is the final step before every commit.**
- **A signature change and its call sites cannot be separate commits** — the
  pre-commit hook runs `make quick` workspace-wide and the intermediate state
  does not compile.
- **All transcendentals route through `hornvale_kernel::math`** (decision 0041).
- **Layering is constitutional**: `kernel/` → `domains/*` → `windows/*` →
  `cli/`. A domain crate may not depend on a sibling domain. `hornvale-climate`
  may NOT import `hornvale-terrain`; the `Stratum`/`BandKind` mirroring is
  hand-maintained under decision 0094.
- **The canonical heavy-tier ignore reason, verbatim, token for token** — the
  scanner matches the string, not its meaning:
  `#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]`
- **Absorb main at every task boundary** — `make sluice-stage BRANCH=campaign/the-underworld REF=<full-sha>`.
  A conflict is refused at the mouth in milliseconds; that is the signal to
  merge main locally and resubmit.
- **This campaign takes an epoch.** Goldens rebaseline and one census runs, at
  Task 10. Do not rebaseline piecemeal — see Task 10's branch table.

---

## File Structure

**Created:**

| path | responsibility |
|---|---|
| `domains/terrain/src/delve.rs` | the delve ladder: rungs, ΔT thresholds, `rung_at_depth`, `depth_range_of_rung`. Terrain owns it because it derives from the geothermal gradient, which terrain owns. |
| `domains/terrain/src/water_table.rs` | `water_table_depth_m`, and the vadose/phreatic predicate |
| `domains/climate/src/underworld.rs` | underworld community assignment — `EnvironmentVector`s for the cave corpus |
| `windows/worldgen/tests/underworld_ladder_probe.rs` | Task 1's measurement; the rung table is its output |
| `windows/worldgen/tests/underworld_separation.rs` | H1–H5 readout |

**Modified:**

| path | change |
|---|---|
| `domains/terrain/src/lib.rs` | export `delve`, `water_table` |
| `windows/worldgen/src/chamber.rs` | `ChamberAddr.band` indexes the delve ladder; `band_rank`/`band_of_rank`/`band_name`/`chamber_key` follow |
| `windows/worldgen/src/streams.rs` | `CHAMBER` → `chamber/v2` |
| `windows/worldgen/src/lib.rs` | `subterranean_substrate` gains a rung and stops returning constants |
| `windows/worldgen/src/history_bake.rs` | `node_index` re-keyed to `(CellId, Rung)` |
| `domains/species/src/lib.rs` | `EnvironmentNiche`; Mountain and Duergar |
| `domains/language/src/lib.rs`, `accession.rs` | two names re-enter cohort 9 |

---

## Task 1: Measure the ladder before authoring it

**Files:**
- Create: `windows/worldgen/tests/underworld_ladder_probe.rs`

**Interfaces:**
- Consumes: nothing — this task precedes every authored number.
- Produces: the rung table that Task 2 encodes. **No code in later tasks may
  hardcode a ΔT boundary until this probe has printed its distribution.**

This is the task The Delvers did not have. It asserts nothing about where rungs
*should* fall; it prints what the substrate does, so the table is an output.

- [ ] **Step 1: Write the probe**

```rust
//! THE UNDERWORLD, Task 1: what does the rock column look like in KELVIN?
//!
//! Measurement only. The delve ladder (spec §4.1) places its rungs at
//! temperature offsets above the surface datum rather than at round metres,
//! so the rung table cannot be authored until the distribution of
//! (depth, gradient) over cave-bearing cells is known. This probe prints it.
//!
//! It asserts nothing. Every check is a build/lookup `expect`; the result is
//! the printed table. Recorded into the module doc when it has been run.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_terrain::{BandKind, TerrainPins};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};

/// Seeds this campaign preregisters on (spec §5).
const SEEDS: [u64; 3] = [42, 7, 1234];

/// Percentile of an ascending slice.
fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    sorted[((sorted.len() - 1) as f64 * q).round() as usize]
}

/// claim: readout(off-gate, heavy:, prints only, no assertion) — the joint
/// distribution of cave depth and geothermal gradient over cave-bearing land
/// cells, expressed as ΔT above the surface datum. The input to spec §4.1's
/// rung table; not a gate on any value.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn how_hot_is_a_cave() {
    // The sanctioned test-fixture posture (decision 0092), copied from
    // `deep_realm_substrate.rs::measure_one` — seven arguments including the
    // assembled components, returning `BuildArtifacts` rather than a tuple.
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    for seed_value in SEEDS {
        let seed = hornvale_kernel::Seed(seed_value);
        let artifacts = build_world_to_with_artifacts(
            seed,
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Terrain,
        )
        .expect("probe seed builds");
        let terrain = artifacts
            .terrain
            .expect("terrain is Some at BuildDepth::Terrain");
        let geo = terrain.geosphere();

        let mut dt_samples: Vec<f64> = Vec::new();
        let mut gradients: Vec<f64> = Vec::new();
        let mut band_hist = [0usize; 5];

        for cell in geo.cells() {
            if terrain.is_ocean(cell) {
                continue;
            }
            let Some(cave) = terrain.cave_at(cell) else {
                continue;
            };
            let gradient = terrain.geothermal_gradient_at(cell).get();
            gradients.push(gradient);
            band_hist[match cave.deepest_band {
                BandKind::Regolith => 0,
                BandKind::Cover => 1,
                BandKind::Basement => 2,
                BandKind::Roots => 3,
                BandKind::Underneath => 4,
            }] += 1;
            // ΔT at the cave's deepest reach: the column's own top-depth for
            // that band, converted to km, times this cell's gradient.
            let column = terrain.column_at(cell);
            let depth_m = column.bands[match cave.deepest_band {
                BandKind::Regolith => 0,
                BandKind::Cover => 1,
                BandKind::Basement => 2,
                BandKind::Roots => 3,
                BandKind::Underneath => 4,
            }]
            .top_depth_m;
            dt_samples.push(gradient * (depth_m / 1000.0));
        }

        dt_samples.sort_by(f64::total_cmp);
        gradients.sort_by(f64::total_cmp);
        println!(
            "seed {seed_value}: caves={} bands(Reg,Cov,Bas,Roo,Und)={band_hist:?}",
            dt_samples.len()
        );
        println!(
            "  gradient K/km  p10={:.3} p50={:.3} p90={:.3}",
            pct(&gradients, 0.10),
            pct(&gradients, 0.50),
            pct(&gradients, 0.90)
        );
        for q in [0.10, 0.25, 0.50, 0.75, 0.90, 0.99] {
            println!("  deltaT p{:>2.0} = {:.3} K", q * 100.0, pct(&dt_samples, q));
        }
        // How many cave cells fall in each candidate band of the spec's
        // ILLUSTRATIVE table. Printed so the real boundaries can be chosen
        // against a distribution rather than against the illustration.
        for (lo, hi) in [(0.0, 2.0), (2.0, 10.0), (10.0, 25.0), (25.0, 50.0)] {
            let n = dt_samples.iter().filter(|d| **d >= lo && **d < hi).count();
            println!("  [{lo:>5.1}, {hi:>5.1}) K : {n}");
        }
        let over = dt_samples.iter().filter(|d| **d >= 50.0).count();
        println!("  [ 50.0,   inf) K : {over}");
    }
}
```

- [ ] **Step 2: Confirm the probe compiles and the accessors exist**

Run: `cargo test -p hornvale-worldgen --test underworld_ladder_probe --no-run`

Both calls this probe makes were verified against the tree while this plan was
written: `GeneratedTerrain::column_at(CellId) -> StratigraphicColumn`
(`domains/terrain/src/provider.rs:468`) and the seven-argument
`build_world_to_with_artifacts` returning `BuildArtifacts { world, terrain,
climate }` (`windows/worldgen/src/lib.rs:6405`, `:231`). The first draft of
this task got the second one wrong — six arguments and a tuple destructure —
so if it still does not compile, trust the tree and not this plan.

**Decision rule — do not predict, branch:**
- Compiles → go to Step 3.
- A signature mismatch → read the definition and match it; treat
  `deep_realm_substrate.rs::measure_one` as the reference idiom, since it is
  a working caller of the same function.
- `BandKind` match non-exhaustive → the ladder gained a variant since this
  plan was written. Add the arm; do not wildcard it.

- [ ] **Step 3: Run it and capture the table**

Run: `cargo test -p hornvale-worldgen --test underworld_ladder_probe -- --ignored --nocapture 2>&1 | tee /tmp/hv-ladder.txt`

Read `/tmp/hv-ladder.txt`. Do not re-run to grep a second line.

- [ ] **Step 4: Record the measured table into the module doc**

Paste the printed distribution into the `//!` block, dated, exactly as
`delver_depth_probe.rs` records its own. The probe must carry its result.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/worldgen/tests/underworld_ladder_probe.rs
git commit -m "test(the-underworld): measure the column in kelvin before authoring a rung"
```

---

## Task 2: The delve ladder

**Files:**
- Create: `domains/terrain/src/delve.rs`
- Modify: `domains/terrain/src/lib.rs`
- Test: `domains/terrain/src/delve.rs` (in-module `#[cfg(test)] mod tests`)

**Interfaces:**
- Consumes: Task 1's measured table.
- Produces:
  - `pub enum DelveRung` — the ladder, `Surface` first, then habitation rungs
    in descending order. Derives `Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord`.
  - `pub fn rung_at_delta_t(delta_t_k: f64) -> DelveRung`
  - `pub fn delta_t_range_of(rung: DelveRung) -> (f64, Option<f64>)`
  - `pub fn rung_at_depth(depth_m: f64, gradient: GeothermalGradient) -> DelveRung`
  - `pub const HABITABLE_CEILING_K: f64 = 50.0`
  - `pub fn rungs() -> &'static [DelveRung]`

**Constraint from spec §4.1:** the ladder has at least 4 and at most 6 rungs
*excluding* `Surface`, the top habitation rung begins at ΔT = 0, the bottom is
open-ended, and `HABITABLE_CEILING_K` is **50.0** — authored, not derived, and
its doc comment must say so.

`Surface` is a variant of this enum, not an absence of one (spec §4.6): the
overworld is a rung of the same ladder so the type is total and no reader can
mistake `None` for "surface".

- [ ] **Step 1: Write the failing tests**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::strata::GeothermalGradient;

    #[test]
    fn the_ladder_is_within_the_specs_bound() {
        // Spec §4.1: at least 4 and at most 6 habitation rungs.
        let habitation = rungs().iter().filter(|r| **r != DelveRung::Surface).count();
        assert!(
            (4..=6).contains(&habitation),
            "habitation rungs = {habitation}, spec §4.1 allows 4..=6"
        );
    }

    #[test]
    fn the_rungs_tile_the_line_with_no_gap_and_no_overlap() {
        // Every ΔT >= 0 resolves to exactly one rung, and consecutive rungs
        // share a boundary. This is the invariant that a hand-written
        // threshold table gets wrong.
        let habitation: Vec<DelveRung> =
            rungs().iter().copied().filter(|r| *r != DelveRung::Surface).collect();
        let (first_lo, _) = delta_t_range_of(habitation[0]);
        assert_eq!(first_lo, 0.0, "the top habitation rung must begin at 0 K");
        for pair in habitation.windows(2) {
            let (_, upper_hi) = delta_t_range_of(pair[0]);
            let (lower_lo, _) = delta_t_range_of(pair[1]);
            assert_eq!(
                upper_hi,
                Some(lower_lo),
                "{:?} must end exactly where {:?} begins",
                pair[0],
                pair[1]
            );
        }
        let (_, last_hi) = delta_t_range_of(*habitation.last().unwrap());
        assert_eq!(last_hi, None, "the bottom rung is open-ended");
    }

    #[test]
    fn rung_at_delta_t_agrees_with_the_declared_ranges() {
        for rung in rungs().iter().copied().filter(|r| *r != DelveRung::Surface) {
            let (lo, hi) = delta_t_range_of(rung);
            // A point just inside the low edge belongs to this rung.
            assert_eq!(rung_at_delta_t(lo), rung, "low edge of {rung:?}");
            if let Some(hi) = hi {
                // A point just below the high edge still belongs to it.
                assert_eq!(rung_at_delta_t(hi - 1e-9), rung, "high edge of {rung:?}");
                // The high edge itself belongs to the NEXT rung.
                assert_ne!(rung_at_delta_t(hi), rung, "{rung:?} must be half-open");
            }
        }
    }

    #[test]
    fn the_same_rung_sits_deeper_under_a_cooler_gradient() {
        // The campaign's whole point: a rung is a place-type, not a depth.
        let cool = GeothermalGradient::new(15.0);
        let hot = GeothermalGradient::new(30.0);
        let depth_m = 1000.0;
        let under_cool = rung_at_depth(depth_m, cool);
        let under_hot = rung_at_depth(depth_m, hot);
        assert!(
            under_hot >= under_cool,
            "at one depth the hotter gradient must be at or below the cooler \
             one on the ladder: cool={under_cool:?} hot={under_hot:?}"
        );
    }

    #[test]
    fn the_habitable_ceiling_is_the_authored_value() {
        assert_eq!(HABITABLE_CEILING_K, 50.0);
    }

    #[test]
    fn a_negative_or_nonfinite_delta_t_resolves_to_the_top_rung() {
        // Total, not panicking: a surface datum warmer than the rock is a
        // physical possibility the ladder must absorb rather than reject.
        let top = rungs().iter().copied().find(|r| *r != DelveRung::Surface).unwrap();
        assert_eq!(rung_at_delta_t(-5.0), top);
        assert_eq!(rung_at_delta_t(f64::NAN), top);
    }
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-terrain delve 2>&1 | tail -20`
Expected: FAIL — `DelveRung` and its functions do not exist.

- [ ] **Step 3: Implement the ladder**

Write `domains/terrain/src/delve.rs` with the rung boundaries **taken from
Task 1's measured table**, not from the spec's illustration. Every constant
gets a doc comment naming where its value came from; the ceiling's doc says
explicitly that 50.0 is an authored fidelity choice, not a derivation.

Add `pub mod delve;` and the re-exports to `domains/terrain/src/lib.rs`.

- [ ] **Step 4: Run to verify they pass**

Run: `cargo test -p hornvale-terrain delve 2>&1 | tail -20`
Expected: PASS, all six.

- [ ] **Step 5: Prove the tiling test can fail**

The tiling invariant is the one a hand-written threshold table gets wrong, so
it must be shown to fire. Perturb one boundary constant so two rungs overlap
or leave a gap, confirm `the_rungs_tile_the_line_with_no_gap_and_no_overlap`
goes RED, then restore.

**Do not use `git checkout --` to restore** — it reverts your uncommitted test
file along with the perturbation, and the test's absence then reads as a pass.
Edit the constant back by hand.

- [ ] **Step 6: Regenerate the type-audit report and commit**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
cargo fmt
git add domains/terrain/src/delve.rs domains/terrain/src/lib.rs docs/audits/type-audit-report.md
git commit -m "feat(the-underworld): the delve ladder, spaced by heat"
```

---

## Task 3: The water table

**Files:**
- Create: `domains/terrain/src/water_table.rs`
- Modify: `domains/terrain/src/lib.rs`

**Interfaces:**
- Consumes: `DelveRung` (Task 2).
- Produces:
  - `pub fn water_table_depth_m(drainage: f64, porosity: f64, height_asl_m: f64) -> f64`
  - `pub fn is_phreatic(depth_m: f64, water_table_m: f64) -> bool`

Pure; no draws, no committed facts.

- [ ] **Step 1: Write the failing tests**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn more_drainage_raises_the_table() {
        let dry = water_table_depth_m(0.0, 0.3, 500.0);
        let wet = water_table_depth_m(1.0, 0.3, 500.0);
        assert!(wet < dry, "wet={wet} should be shallower than dry={dry}");
    }

    #[test]
    fn more_porous_rock_drains_deeper() {
        let tight = water_table_depth_m(0.5, 0.05, 500.0);
        let porous = water_table_depth_m(0.5, 0.95, 500.0);
        assert!(porous > tight, "porous={porous} should be deeper than tight={tight}");
    }

    #[test]
    fn the_table_is_never_above_the_surface_and_always_finite() {
        for drainage in [0.0, 0.5, 1.0] {
            for porosity in [0.0, 0.5, 1.0] {
                for h in [-500.0, 0.0, 3000.0] {
                    let d = water_table_depth_m(drainage, porosity, h);
                    assert!(d.is_finite(), "non-finite at {drainage}/{porosity}/{h}");
                    assert!(d >= 0.0, "table above the surface at {drainage}/{porosity}/{h}");
                }
            }
        }
    }

    #[test]
    fn phreatic_is_below_the_table_and_vadose_above() {
        assert!(is_phreatic(100.0, 50.0), "100 m is below a 50 m table");
        assert!(!is_phreatic(10.0, 50.0), "10 m is above a 50 m table");
    }
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-terrain water_table 2>&1 | tail -20`
Expected: FAIL — module does not exist.

- [ ] **Step 3: Implement**

Monotone in both inputs, total over the whole input domain, transcendentals
via `hornvale_kernel::math`. Every constant carries a doc comment saying where
its value came from.

- [ ] **Step 4: Run to verify they pass**

Run: `cargo test -p hornvale-terrain water_table 2>&1 | tail -20`

- [ ] **Step 5: Regenerate the type-audit report and commit**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
cargo fmt
git add domains/terrain/src/water_table.rs domains/terrain/src/lib.rs docs/audits/type-audit-report.md
git commit -m "feat(the-underworld): a derived water table, splitting each column"
```

- [ ] **Step 6: Absorb main**

```bash
git rev-parse HEAD
make sluice-stage BRANCH=campaign/the-underworld REF=<that full sha>
```

---

## Task 4: Re-point `ChamberAddr.band` at the delve ladder

**Files:**
- Modify: `windows/worldgen/src/chamber.rs`, `windows/worldgen/src/streams.rs`

**Interfaces:**
- Consumes: `DelveRung`, `rung_at_depth` (Task 2).
- Produces: `ChamberAddr.band` indexing `DelveRung`; `Chamber` carrying both
  `rung: DelveRung` and `stratum: Stratum`.

**This task changes a save-format key.** `chamber_key`'s band spelling comes
from an explicit table, never a `Debug` impl — preserve that discipline
exactly. `CHAMBER` becomes `chamber/v2` in `windows/worldgen/src/streams.rs`.

- [ ] **Step 1: Write the failing tests**

```rust
// in windows/worldgen/tests/deep_realm_chamber.rs, or a sibling file
#[test]
fn a_chamber_reports_both_its_rung_and_its_stratum() {
    // The two ladders are independent: neither derives the other (spec §2).
    // A chamber must be able to say "granite, and warm" without one answer
    // being computable from the other.
    // ... build a cave, take a chamber, assert both fields are populated and
    // that two chambers exist which share a stratum and differ in rung.
}

#[test]
fn the_chamber_key_spells_the_rung_by_name_not_by_index() {
    // Guards the save-format discipline chamber_key's own doc states.
    // Assert the key contains the rung's declared name string.
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-worldgen --test deep_realm_chamber 2>&1 | tail -20`

- [ ] **Step 3: Implement**

Re-point `band_rank`/`band_of_rank`/`band_name` at `DelveRung`, keeping each
an exhaustive match so a new rung fails to compile rather than silently
misplacing. Bump `CHAMBER` to `chamber/v2`.

- [ ] **Step 4: Run the worldgen suite**

Run: `cargo nextest run -p hornvale-worldgen 2>&1 | tee /tmp/hv-t4.txt`

**Decision rule for the failures this will produce:**
- A test asserting a specific chamber key string → the key legitimately moved;
  re-pin it to the new value **in this commit**, and say so in the message.
- A test asserting a `BandKind` on a chamber → it is asking the archive
  question; leave it reading `stratum`, not `rung`.
- A test asserting chamber *counts* → the budget's meaning changed; re-derive
  the expected count by the test's own procedure, never by picking a number
  that passes.
- Anything in `domains/` failing → stop. This task must not reach terrain;
  that is a layering violation and means `band` is being derived in the wrong
  crate.

- [ ] **Step 5: Regenerate the stream manifest and the type-audit report**

```bash
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

**Decision rule:**
- Only `docs/audits/` and the stream manifest moved → expected; commit them
  with the code.
- A seeded almanac or the elevation map moved → also expected this campaign
  (the chamber epoch), but note it in the commit message explicitly.
- `book/src/domesday/` moved → STOP. That reads the committed census, which
  nothing in this task should touch.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(the-underworld)!: ChamberAddr.band indexes the delve ladder — chamber/v2"
```

---

## Task 5: Chamber conditions stop being constants

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (`subterranean_substrate`, ~line 2650)

**Interfaces:**
- Consumes: `DelveRung`, `rung_at_depth`, `water_table_depth_m`,
  `temperature_at_depth`, `geothermal_gradient`.
- Produces: `subterranean_substrate(surface: Substrate, rung: DelveRung, gradient: GeothermalGradient, water_table_m: f64) -> Substrate`

**The signature change and every call site must land in one commit.**
`per_species_suitability_masked` (`windows/worldgen/src/lib.rs:1482`) is the
live caller; `windows/worldgen/tests/deep_realm_rehome.rs` is a test caller.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn a_deeper_rung_is_warmer_under_the_same_surface() {
    // The defect this campaign exists to fix: temperature passed through
    // unchanged at every depth (metaplan §3.5).
}

#[test]
fn two_cells_at_one_rung_differ_when_their_crust_differs() {
    // Same rung, same surface temperature, different gradient -> different
    // chamber temperature. This is what makes chambers tellable apart.
}

#[test]
fn moisture_is_no_longer_a_world_constant() {
    // Two chambers with different water-table distances must not report the
    // identical moisture.
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-worldgen subterranean 2>&1 | tail -20`

- [ ] **Step 3: Implement, changing every call site in the same edit**

Find them first: `grep -rn "subterranean_substrate" --include=*.rs .`

- [ ] **Step 4: Run the full worldgen suite**

Run: `cargo nextest run -p hornvale-worldgen --no-fail-fast 2>&1 | tee /tmp/hv-t5.txt`

**Decision rule:** drow's suitability will move — it is the only shipped
Subterranean settled people. A moved drow number is expected and re-pinned
here. A moved *surface* kind's number is NOT expected and means the
`availability`/`affinity` factors have been disturbed; stop and diagnose
rather than re-pinning.

- [ ] **Step 5: Prove the depth dependence is real**

The property to demonstrate: **neutralising the depth term must be visible to
at least one assertion.** Find a mutation that demonstrates it — do not use a
prescribed one from this plan, because the plan's author does not know which
of these terms share a code path and the implementer does after reading.
Record which mutation was used and what went red.

- [ ] **Step 6: Commit**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
cargo fmt
git add -A
git commit -m "feat(the-underworld)!: a chamber's conditions vary with depth and crust"
```

- [ ] **Step 7: Absorb main**

```bash
git rev-parse HEAD
make sluice-stage BRANCH=campaign/the-underworld REF=<that full sha>
```

---

## Task 6: Underworld communities as `EnvironmentVector`s

**Files:**
- Create: `domains/climate/src/underworld.rs`
- Modify: `domains/climate/src/lib.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::ecology::{EnvironmentVector, EnvironmentAxis, PHYSIOGNOMY, ENERGY, WATER, SUBSTRATE, LIGHT}`.
  `EnvironmentVector::new(&[(EnvironmentAxis, f64)]) -> Result<Self, UnitError>`;
  values must be finite and within `[0, 1]`.
- Produces: `pub fn underworld_assignment() -> &'static [AssignedName]`, matching
  the shape `domains/climate/src/axes.rs::assignment()` already uses.

**Layering:** `hornvale-climate` may NOT import `hornvale-terrain`. Cave kind
and rock class reach this module as *arguments*, mirrored per decision 0094 —
never by importing the sibling.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn every_underworld_name_carries_the_axes_it_can() {
    // The corpus assigns PHYSIOGNOMY, ENERGY, WATER, SUBSTRATE for every
    // name; LIGHT is the axis expected to collapse (spec §5 H5).
}

#[test]
fn no_two_underworld_names_share_a_vector() {
    // The Axes' collision clause, applied to this corpus.
}

#[test]
fn light_takes_at_most_two_distinct_values() {
    // Spec §5 H5, PREREGISTERED. If this fails the prediction was wrong,
    // which is a finding: record the real count in the chronicle and do NOT
    // retune the assignment to rescue it.
}

#[test]
fn energy_is_not_monotone_in_depth() {
    // Spec §4.4: energy INVERTS — detrital import shallow, chemolithotrophy
    // deep. A corpus where energy falls monotonically with depth would mean
    // the inversion was not modelled.
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-climate underworld 2>&1 | tail -20`

- [ ] **Step 3: Implement the assignment**

Author the corpus. Every name's vector gets a comment saying which physical
input drove each axis value.

- [ ] **Step 4: Run to verify they pass**

Run: `cargo test -p hornvale-climate underworld 2>&1 | tail -20`

**Decision rule if `light_takes_at_most_two_distinct_values` fails:** the
prediction was falsified. Keep the test, change its expected count to the
measured one, and mark it in the test's own doc comment as a falsified
preregistration with the date. Do not adjust the assignment.

- [ ] **Step 5: Commit**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
cargo fmt
git add -A
git commit -m "feat(the-underworld): underworld communities as points in the axis basis"
```

---

## Task 7: `EnvironmentNiche`

**Files:**
- Modify: `domains/species/src/lib.rs`

**Interfaces:**
- Consumes: the kernel basis (Task 6's imports).
- Produces: `pub struct EnvironmentNiche` and
  `pub fn environment_fit(niche: &EnvironmentNiche, place: &EnvironmentVector) -> f64`
  returning `[0, 1]`.

The Axes' retrospective A-1 records that the invariant making this safe is
already pinned and proven. Read that pin before writing — it tells you what
must not move.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn a_niche_matching_a_place_exactly_scores_one() {}

#[test]
fn an_unassigned_place_vector_scores_a_defined_value_not_a_panic() {
    // The zero vector is LEGAL and means unassigned (kernel docs). A niche
    // scored against it must return a defined number.
}

#[test]
fn fit_is_bounded_in_zero_one_over_the_whole_basis() {}

#[test]
fn an_axis_the_niche_declines_does_not_constrain_the_fit() {
    // Mirrors the genus rule The Axes established: a niche silent on an axis
    // is indifferent to it, never zero on it.
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-species environment 2>&1 | tail -20`

- [ ] **Step 3: Implement**

- [ ] **Step 4: Run to verify they pass**

- [ ] **Step 5: Commit**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
cargo fmt
git add -A
git commit -m "feat(the-underworld): EnvironmentNiche — the consumer The Axes deferred"
```

---

## Task 8: Realm-aware capacity and the node-index re-key

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs` (`node_index` :1139, `vacant_habitable` :1377, and the insert/remove sites at :2221, :2261)

**Interfaces:**
- Consumes: `DelveRung` (Task 2, with its `Surface` variant).
- Produces: `node_index: BTreeMap<(CellId, DelveRung), usize>`.

**Surface density must not change.** That is this task's acceptance criterion
and the thing most likely to go wrong silently.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn a_cell_still_holds_exactly_one_surface_community() {
    // Spec §4.6. The surface is ONE rung, so surface density is unchanged.
}

#[test]
fn a_subterranean_community_no_longer_displaces_a_surface_one() {
    // The defect: a BTreeMap<CellId, _> cannot hold two polities (0102).
}

#[test]
fn two_underworld_communities_can_share_a_column_at_different_rungs() {
    // The capability Mountain and Duergar need to exist at all.
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-worldgen node_index 2>&1 | tail -20`

- [ ] **Step 3: Implement**

Every `node_index` site: :1139, :1377, :1537, :2221, :2261, :3098, :3186,
:3475, :3524, plus the test-fixture constructions at :3803 and :4061. Find the
current set with `grep -n "node_index" windows/worldgen/src/history_bake.rs`
rather than trusting these line numbers, which will have moved.

- [ ] **Step 4: Run the full workspace**

Run: `cargo nextest run --workspace --no-fail-fast 2>&1 | tee /tmp/hv-t8.txt`

**Decision rule:**
- Settlement counts on the surface unchanged → correct.
- Surface counts moved → the `Surface` rung is not being keyed consistently.
  Stop and diagnose; do not re-pin.
- History/migration tests moved → expected, the underworld now competes
  differently. Re-derive each witness by its own procedure.

- [ ] **Step 5: Prove the surface-invariance test can fail**

The property: **a test asserting surface density is unchanged must go red if
surface keying is broken.** Find a perturbation demonstrating it. Restore by
hand, not with `git checkout --`.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(the-underworld)!: the bake keys a place, not a cell (0102)"
```

- [ ] **Step 7: Absorb main**

```bash
git rev-parse HEAD
make sluice-stage BRANCH=campaign/the-underworld REF=<that full sha>
```

---

## Task 9: The separation readout — and only then, the two kinds

**Files:**
- Create: `windows/worldgen/tests/underworld_separation.rs`
- Modify (Step 5 onward, conditionally): `domains/species/src/lib.rs`,
  `domains/language/src/lib.rs`, `domains/language/src/accession.rs`

**This task is gated. Steps 1–4 measure; steps 5+ author only if H2's floor
holds.**

- [ ] **Step 1: Write the readout**

Evaluate spec §5 H1–H5 and print each with its bound beside it. Carry the
canonical heavy ignore string verbatim.

- [ ] **Step 2: Author both kinds as CANDIDATES, uncommitted to the roster**

Give Mountain and Duergar their niches in a test-local fixture so their
suitability can be measured **before** they enter `biosphere_registry()`.
Measuring after admission would make the roster the thing under test.

- [ ] **Step 3: Run the readout**

Run: `cargo test -p hornvale-worldgen --test underworld_separation -- --ignored --nocapture 2>&1 | tee /tmp/hv-sep.txt`

- [ ] **Step 4: The gate**

**Decision rule, and this is the campaign's fork:**
- **H2 floor holds** (modal rungs differ; each clears `hornvale_demography::FLOOR`
  on ≥1 cell of every seed) **and H2's minimum overlap ≥ 20%** → proceed to
  Step 5.
- **H2 floor fails** → **do not author the kinds.** Record the measured
  numbers, write the chronicle and retrospective around the null, and close
  the campaign at Task 10 with the ladder, water table, communities, niche and
  re-key shipped. This is a legitimate and preregistered outcome (spec §5).
- **H2 floor holds but overlap < 20%** → the axis separated them into
  different peoples rather than different dwarves. Report it; do not widen the
  threshold to admit them.

- [ ] **Step 5 (conditional): Admit both kinds**

`biosphere_registry`, `habitat_realm_registry`, psyche/perception/dispersion/
society, `family_of`, `KIND_CONCEPTS`, and both language registries. The
withdrawal commit `b3583640` is the exact inverse list — read it, and note its
warning that removing competitors is not the inverse of adding them, so the
pantheon, name-gloss, genesis-root and productivity counts will move and must
be re-derived rather than reverted to pre-Delvers values.

- [ ] **Step 6 (conditional): Cohort 9**

Two names re-enter. The surviving dwarves' own words will move, because
re-adding two names re-sorts the cohort. Expected; re-pin.

- [ ] **Step 7: Commit**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
cargo fmt
git add -A
git commit -m "feat(the-underworld): the separation readout, and its verdict"
```

---

## Task 10: Epoch, artifacts, census, and the book

**Files:** every generated path; `book/src/chronicle/the-underworld.md`;
`docs/retrospectives/the-underworld.md`; `book/src/open-questions.md`.

- [ ] **Step 1: Regenerate everything except censuses**

```bash
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

- [ ] **Step 2: Rebaseline the byte goldens**

```bash
make rebaseline-goldens
```

- [ ] **Step 3: Push, then dispatch the census on lefford**

The census is **authorized for this campaign** (spec §6). It runs on lefford
and nowhere else; `census-run.sh` fails closed on the hostname.

```bash
git push
git rev-parse HEAD
ssh lefford 'cd ~/Projects/hornvale && HV_CENSUS_WORKTREE=canonical HV_CENSUS_REF=<that full sha> scripts/census-run.sh'
```

Use a **full SHA**, never a branch name — `HV_CENSUS_REF` feeds `reset --hard`
and can land on a stale local branch of that name over there.

Commit the regenerated goldens **on lefford** — the canonical box authors
them — then push and fast-forward locally.

- [ ] **Step 4: Review what moved**

```bash
make lab-diff STUDY=the-census
make census-check
```

- [ ] **Step 5: Write the chronicle**

`book/src/chronicle/the-underworld.md`. The three findings worth leading with:
the bimodality was the ladder, not the caves; three shipped seams with no
producer closed at once (`EnvironmentNiche`, `ChamberOrigin`,
`temperature_at_depth`); and H1–H5's results, including any that came back
negative.

- [ ] **Step 6: Freshness sweep and the Confidence Gradient**

The underworld bet in `book/src/open-questions.md` was re-scored sideways by
The Fathom. This campaign moves it again — re-score it per decision 0030.

- [ ] **Step 7: The retrospective**

`docs/retrospectives/the-underworld.md`. Promote the decision ledger's
findings **before** the worktree is torn down; `.superpowers/sdd/` is
git-ignored and dies with it.

- [ ] **Step 8: Merge**

```bash
git rev-parse HEAD
make sluice BRANCH=campaign/the-underworld REF=<that full sha>
```

---

## Self-review notes

**Spec coverage:** §4.1 → Tasks 1–2. §4.2 → Task 3. §4.3 → Task 5. §4.4 →
Task 6. §4.5 → Task 7. §4.6 → Task 8. §4.7 → Task 9. §4.8 → Task 9 Step 6.
§5 H1–H5 → Task 9 Steps 1–4. §6 → Task 10. §7 non-goals are captured as
registry rows at `8d227dae` and are not tasks.

**Known gap, stated rather than hidden:** Task 4's and Task 8's test bodies
are described by the property they must hold rather than written out, because
both depend on fixture idioms (`build_world_to_with_artifacts` depth rungs,
the bake's test constructors) whose exact shape the implementer will read from
the neighbouring files. Every other task carries its test code literally.
Writing those two blind is how a plan ships a test that cannot compile from
where it runs — the mistake The Fathom made twice in one campaign, in the same
file, on the same unread line.
