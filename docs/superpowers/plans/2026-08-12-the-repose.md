# The Repose — Geohazards C0 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the loop on tectonic unrest — give Hornvale a hazard field, a
named mountain that persists without being stored, a drawn event stream from an
authored law, and a people's decaying memory of its own ground — then measure
whether settlements over-occupy violent ground, with a counterfactual arm that
makes a null decidable.

**Architecture:** Everything composes at `windows/worldgen` (the composition
root). `domains/terrain` gains one derived read (see the amendment below);
`domains/language` gains exactly one
`NameKind` variant on its own seed path. C0 is a **pure read**: nothing is
committed to the ledger, nothing draws from an existing stream, so no existing
world moves a byte. Volcano identity, the event stream, and knownness are all
derived on demand from `(seed, cell, …)` and never stored.

**Tech Stack:** Rust edition 2024, workspace crates only. Dependencies are
frozen to `serde` / `serde_json` / `libm` (the `ALLOWED_EXTERNAL` const in
`cli/tests/architecture.rs`) — this plan adds none.

**Spec:** `docs/superpowers/specs/2026-08-07-geohazards-c0-design.md`. Where
this plan and the spec disagree, the spec governs **except** where §0 below
records a verified amendment.

---

## 0. Verified amendments to the spec

The spec was written before its claims about the shipped code were executed.
Three were checked at plan-drafting time and two need the plan to differ from
the spec's literal text. Recorded here so an implementer does not "fix" the
plan back toward the spec.

### 0.1 Andosol fertility never reaches settlement siting

Spec §1 says volcanic ground "carries the world's best ore" and is "very
fertile", and §6.6 proposes neutralizing `andosol` fertility to see whether
siting moves. The ore half is true. **The fertility half is not connected.**

Evidence, run on `main` at `44d7fb9f`:

```
$ grep -rn "soil_of(" --include=*.rs kernel domains windows cli
windows/worldgen/src/alchemy.rs:188:    let soils = crate::soil_of(terrain, climate, geo);
windows/worldgen/src/lib.rs:3479:    let soils = soil_of(terrain, climate, geo);      # ground_lines_from — almanac prose
windows/worldgen/src/lib.rs:11452:        let soil = soil_of(&terrain, &climate, geo);  # a #[test]
windows/lab/src/metrics.rs:1740:                let soils = soil_of(v.terrain(), &v.climate, geo);
windows/lab/src/metrics.rs:1765:                let soils = soil_of(v.terrain(), &v.climate, geo);

$ grep -rn "SoilOrder\|classify_soil" domains/settlement/src/ domains/demography/src/
(no matches)
```

`hornvale_terrain::fertility` has exactly two consumers, both in
`windows/lab/src/metrics.rs`. The siting path's food term is
`hornvale_culture::fertility(class)` where `class` is a **biome** class from
`climate.biome_at`, not a `SoilOrder` (`windows/worldgen/src/lib.rs:7094` and
`:11553`). So `Andosol` enters at the almanac's "volcanic soils" line
(`GROUND_ANDOSOL_NOTABLE = 0.1`, `lib.rs:3455`) and stops.

**Consequence:** the spec's single-arm counterfactual would report "siting did
not move" *by construction*, and that green is indistinguishable from a
working measurement finding nothing. Task 2 therefore ships **three** arms; see
§0.2.

### 0.2 Unrest reaches siting through two opposed channels, both live

Not one unpriced gift — two priced channels pulling opposite ways:

| channel | path | direction |
|---|---|---|
| reward | `prospectivity()` weights `unrest` at 0.3 (`domains/terrain/src/lithology.rs:917`) → `terrain.prospectivity_at` → `mineral_supply_field` (`lib.rs:1003`) → the `MINERAL` supply axis in `per_species_suitability` | attracts |
| penalty | `hostility = terrain.unrest_at(cell).clamp(0.0, 1.0)` (`lib.rs:716`) → `CarryingInput.hostility` → `k = BASE * npp * bonus * (1.0 - hostility)` (`domains/demography/src/carrying_capacity.rs:124`) | repels |

`carrying_capacity.rs:9-16`'s own comment records that `hostility` "keeps only
what it was named for: tectonic unrest" since The Tilth. So the spec's §6.4
confound — "volcanic soil (attracts) vs mountains (repels)" — is misidentified.
The real cancelling pair is **mineral reward vs hostility penalty**, with
elevation third. Stratifying by elevation band (§6.4) is still required; it is
just not sufficient on its own.

### 0.3 `windows/worldgen/tests/exposure.rs` is taken

It exists and is about `ExposureClass` — a *perception* concept from The Words
/ The Vigil, nothing to do with hazard exposure. The probe in Task 1 is
`repose_exposure.rs`. Do not rename the existing file.

---

## Global Constraints

Every task's requirements implicitly include this section.

- **Layering** (`cli/tests/architecture.rs`): `kernel/` → `domains/*` →
  `windows/*` → `cli/`. A domain never depends on a sibling domain. All new
  code in this campaign lands in `windows/worldgen` except the one `NameKind`
  variant in `domains/language`.
- **No new dependencies.** `serde`, `serde_json`, `libm` only.
- **No `HashMap` / `HashSet`** — `BTreeMap` / `BTreeSet` / `Vec` only
  (enforced by `clippy.toml` `disallowed-types`).
- **No wall-clock time.** `std::time::Instant` is banned in test code too.
- **Float ordering** uses `total_cmp` with a deterministic tie-break.
- **Quantize at emit only.** `hornvale_kernel::quantize` at serialization
  boundaries; never in the compute path.
- **`#![warn(missing_docs)]`** in every crate: every public item, field, and
  variant gets a one-line doc comment.
- **Typed quantities.** A coherent physical quantity crossing a `pub` boundary
  is a newtype with a validating constructor. Recurrence intervals use
  `hornvale_kernel::units::Years` — the kernel's coarse span type, whose own
  doc names generation length as a user. Dimensionless ratios stay bare `f64`
  **with a `type-audit:` verdict tag**.
- **`type-audit` is in `make gate`.** Any new `pub` boundary primitive needs a
  tag, and `docs/audits/type-audit-report.md` drifts. Regenerate it in the
  **same commit** that moves the boundary:
  `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`
- **`cargo fmt` is the final step before every commit.** Fmt-gate skips are the
  most common review finding in this repo.
- **The heavy-tier ignore reason is matched VERBATIM** by
  `cli/tests/heavy_tier.rs` (`CANONICAL`, line 64). The only legal string is:
  `"heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"`
- **C0 commits nothing to the ledger.** No `Fact`, no new predicate, no new
  concept. If a task seems to need one, stop — that is C1.

---

## File Structure

**Created:**

| path | responsibility |
|---|---|
| `windows/worldgen/src/hazard.rs` | `Recurrence`, `hazard()`, `HazardEvent`, `events_in()`, the authored magnitude laws |
| `windows/worldgen/src/volcano.rs` | `Volcano`, `volcano_at()`, `volcano_name()` |
| `windows/worldgen/src/knownness.rs` | `Knownness`, `knownness()` — the source/decay/sink stock |
| `windows/worldgen/src/ablation.rs` | `ChannelMask` — the test-only seam Task 2's arms drive |
| `windows/worldgen/tests/repose_exposure.rs` | the exposure readout, its guards, and the three-arm counterfactual |
| `windows/worldgen/tests/fixtures/repose-exposure.csv` | the committed readout |
| `windows/worldgen/tests/repose_laws.rs` | §6.8's authored-law recovery check |

**Modified:**

| path | change |
|---|---|
| `domains/language/src/naming.rs` | `NameKind::Landform` + its `label()` and syllable-draw arms |
| `domains/terrain/src/` | one derived read publishing island-arc edifice presence — see the amendment under Task 4 |

> **AMENDMENT, 2026-08-12, made at Task 4's pre-dispatch verification.**
> The Architecture header originally read "`domains/terrain` gains nothing."
> That is **wrong about edifices**, and the error is mine.
>
> There is no edifice accessor on `GeneratedTerrain`. The gate is computed
> transiently inside `elevation.rs`'s sculpt closure
> (`arc_gate_fbm.sample(geo.position(source))`, `elevation.rs:464`),
> thresholded at `ARC_DUTY = 0.45`, sampled per **source** boundary cell so a
> whole edifice shares one value, and evaluated **only** for
> `BoundaryKind::IslandArc`. Nothing retains it and nothing publishes it.
>
> So `domains/terrain` **does** gain a read, and that is the correct layering
> outcome rather than a concession: an edifice is terrain's own concept, and
> re-deriving the gate inside `windows/worldgen` would duplicate a derivation
> that could then silently drift from the elevation it is meant to describe.
> It matches the shape terrain already publishes in `waterfalls()`,
> `prospectivity_at` and `cave_at` — derived reads over its own globe.
>
> **Two properties the accessor must have, and they are the whole risk:**
> it may not consume a draw (the gate is hash-noise with no draw-order
> contract, so re-sampling is free — but touching a `Stream` is an epoch), and
> it must agree with the elevation that shipped rather than becoming a second
> opinion about where edifices are. The mechanism is the implementer's to
> choose; those two properties are not.
| `windows/worldgen/src/streams.rs` | `VOLCANO` and `HAZARD_EVENT` stream labels |
| `windows/worldgen/src/lib.rs` | `mod` declarations and re-exports; `per_species_suitability` delegation for Task 2 |
| `book/src/reference/streams.md` (generated) | new stream labels, via `make rebaseline` |
| `docs/audits/type-audit-report.md` (generated) | new `pub` boundary, via `make rebaseline` |

Splitting by responsibility rather than layer: the hazard field and its event
draw change together (the events are the field's draw), so they share a file.
Volcano identity changes with naming, not with the field. Knownness changes
with neither — it is a fold over the event stream and nothing else.

---

## Task ordering and why

Task 1 comes first because spec §6.7 requires it: *"The reading is takeable on
`main` **today**… Take it before writing any geohazard code, so a '≈ 1' answer
is learned before anything is built on the premise."* Task 2 immediately
follows because a Task 1 reading of ≈ 1 is not interpretable without it.

Tasks 3–7 build the mechanism. Task 8 closes.

---

### Task 1: The baseline exposure readout

Take the reading on today's physics, with **no geohazard code in the tree**.

**Files:**
- Create: `windows/worldgen/tests/repose_exposure.rs`
- Create: `windows/worldgen/tests/fixtures/repose-exposure.csv` (generated in step 5)

**Interfaces:**
- Consumes: nothing from this campaign. Only shipped API —
  `hornvale_worldgen::{build_world, SkyChoice, SettlementPins, WorldComponents,
  terrain_of, climate_from, demography_report_from}`,
  `hornvale_terrain::{GeneratedTerrain, TerrainPins}`,
  `hornvale_astronomy::SkyPins`,
  `hornvale_demography::stack_condense::StackSettlement`.
- Produces: `render_repose_exposure(seeds: impl IntoIterator<Item = u64>) -> String`
  — the CSV body, consumed by Task 2 and Task 7. Also `ExposureRow` (below),
  which Task 2 extends with an `arm` column and Task 7 with a `knownness`
  column. **Both later tasks add columns; neither renames one.**

**Population (spec §6.2, fixed here, not negotiable at implementation time):**
seeds `1..=30` pooled, full build depth, filter = settleable land (land above
sea level with non-zero carrying capacity).

**The statistic (spec §6.3):** per unrest decile × elevation band, the
settlement share and population-weighted share divided by that stratum's
land-area share. Reported as the full vector, per people and pooled — never a
median.

- [ ] **Step 1: Write the failing drift check**

Create `windows/worldgen/tests/repose_exposure.rs`. Start with only the drift
check and the row type, so the first red is behavioural, not a missing file:

```rust
//! The Repose's exposure readout (spec §6): do settlements over-occupy
//! high-unrest ground relative to the land base rate?
//!
//! BASELINE FIRST (spec §6.7). This file is written and run BEFORE any
//! geohazard code exists, so a reading of ~1 is learned before anything is
//! built on the premise that it is not.
//!
//! Stratified by elevation band (spec §6.4) because unrest correlates with
//! two OPPOSED things at once — see the plan's §0.2: the mineral-reward
//! channel attracts and the hostility penalty repels, and an unstratified
//! ratio can read ~1 because they cancel, which is indistinguishable from no
//! effect.
//!
//! World-building idiom reused verbatim from `occupancy_readout.rs` and
//! `demesne.rs`.
#![allow(clippy::disallowed_methods)]

use std::collections::BTreeMap;

use hornvale_kernel::{CellId, KindId, Seed, World};
use hornvale_worldgen::{
    SettlementPins, SkyChoice, WorldComponents, build_world_from_components, climate_from,
    demography_report_from, terrain_of,
};

/// How many unrest deciles the readout stratifies into.
const DECILES: usize = 10;

/// Elevation bands, in metres above sea level, as (label, lower-inclusive
/// bound). The top band is open. Chosen to separate coastal plain from the
/// arc-and-edifice high ground that spec §6.4 names as the repelling half of
/// the confound.
const BANDS: [(&str, f64); 4] = [
    ("lowland", 0.0),
    ("upland", 250.0),
    ("highland", 1000.0),
    ("montane", 2500.0),
];

/// One stratum's readout: a (decile, band, people) cell of the design.
#[derive(Debug, Clone, PartialEq)]
struct ExposureRow {
    /// Unrest decile, 0..DECILES (0 = calmest tenth of settleable land).
    decile: usize,
    /// Elevation band label, from `BANDS`.
    band: &'static str,
    /// The people this row is for, or "pooled" for the all-peoples row.
    people: &'static str,
    /// Settleable land cells in this stratum, summed over seeds.
    land_cells: u64,
    /// Settlements whose attractor cell falls in this stratum.
    settlements: u64,
    /// Total headcount at those settlements.
    population: f64,
    /// settlement share / land-area share.
    exposure_ratio: f64,
    /// population share / land-area share.
    weighted_ratio: f64,
    /// Share of this stratum's land cells classified `Andosol` — the
    /// discrimination guard's input (spec §6.7).
    andosol_share: f64,
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn repose_exposure_readout_matches_the_committed_fixture() {
    let committed = include_str!("fixtures/repose-exposure.csv");
    let rendered = render_repose_exposure(1..=30);
    assert_eq!(
        rendered, committed,
        "repose exposure readout drifted — if this is intended, rewrite the \
         fixture with `cargo test -p hornvale-worldgen --test repose_exposure \
         -- --ignored rewrite_repose_exposure_fixture` and commit the diff \
         WITH the change that moved it"
    );
}
```

- [ ] **Step 2: Run it and confirm the red is behavioural, not a missing symbol**

```bash
cd .claude/worktrees/the-repose
cargo test -p hornvale-worldgen --test repose_exposure -- --ignored 2>&1 | tail -20
```

Expected: a compile error naming `render_repose_exposure` as undefined, and
`fixtures/repose-exposure.csv` as a missing include.

**Decision rule, not a prediction.** A compile error is *not* the red this step
wants — a RED from a compile error proves nothing about whether an assertion
would fire. So: create the fixture file **empty**, stub
`render_repose_exposure` to return `String::new()`, re-run, and confirm the
failure is now `assert_eq!` reporting a content mismatch. That is the
behavioural red. Only then proceed.

- [ ] **Step 3: Implement the readout**

Add to the same file. The world-building idiom is copied from `demesne.rs:314`
— `WorldComponents::assemble()` once, then per seed.

```rust
/// The seed-`n` world at full build depth, built through the composition
/// root exactly as `occupancy_readout.rs` does.
fn world_of(seed: u64, wc: &WorldComponents) -> World {
    build_world_from_components(
        Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
        wc,
    )
    .expect("seed builds at default pins")
}

/// Which elevation band a land cell falls in. Metres ABOVE SEA LEVEL, never
/// above the isostatic datum: `terrain.sea_level()` on seed 42 is
/// -2,936.17 m, and the two disagree on thousands of cells — the exact trap
/// `waterline_probe.rs`'s correction header documents.
fn band_of(terrain: &hornvale_terrain::GeneratedTerrain, cell: CellId) -> &'static str {
    let above = terrain.elevation_at(cell).get() - terrain.sea_level().get();
    let mut chosen = BANDS[0].0;
    for (label, lower) in BANDS {
        if above >= lower {
            chosen = label;
        }
    }
    chosen
}

/// The unrest decile of a cell, given the sorted settleable-land unrest
/// values for its world. Ties break to the LOWER decile so the mapping is a
/// deterministic function of the value, not of iteration order.
fn decile_of(sorted_unrest: &[f64], u: f64) -> usize {
    let n = sorted_unrest.len();
    if n == 0 {
        return 0;
    }
    let rank = sorted_unrest.partition_point(|v| *v < u);
    ((rank * DECILES) / n).min(DECILES - 1)
}
```

The accumulation loop, per seed:

1. `let terrain = terrain_of(&world)?; let climate = climate_from(&world, &terrain)?;`
2. `let geo = terrain.geosphere();`
3. Settleable land = `!terrain.is_ocean(cell)` **and** carrying capacity > 0.
   Build capacity via
   `hornvale_demography::carrying_capacity(geo, &hornvale_worldgen::carrying_inputs_of(geo, &terrain, &climate))`
   and take `.at(cell) > 0.0`. Use this predicate and no other — it is the one
   `ConditionNiche`'s corrected frame uses.
4. Collect settleable-land unrest into a `Vec<f64>`, sort with `total_cmp`,
   and use it for `decile_of`.
5. Soil: `let soils = hornvale_worldgen::soil_of(&terrain, &climate, geo);` —
   count `SoilOrder::Andosol` per stratum for the discrimination guard.
6. Settlements: `demography_report_from(&world, wc, &terrain, &climate)?`, then
   iterate `report.stack_settlements`. Each `StackSettlement` carries `cell`,
   `composition: Vec<(u32, f64)>`, `dominant: u32`, and
   `rendered: Vec<(u32, HeadcountRender)>`.
7. Per-people attribution: the `u32` in `composition`/`rendered` is a
   **build-local dense index, not identity** — it is the position in
   `wc.biosphere.iter()`'s ascending-`KindId` order. Rebuild the mapping once,
   outside the seed loop, exactly as `demesne.rs:316` does:
   `let kinds: Vec<KindId> = wc.biosphere.iter().map(|(k, _)| *k).collect();`
   Attribute a settlement to its `dominant` kind for the per-people rows, and
   also emit a `pooled` row summing all of them.

Render with `hornvale_kernel::quantize` on every float at the CSV boundary —
this is a serialization boundary, so quantization is required here and
forbidden anywhere upstream of it.

- [ ] **Step 4: Write the three guards of spec §6.7**

These are separate `#[test]` functions in the same file, each carrying the
verbatim heavy-tier reason. **Each guard states the direction it enforces in
its own doc comment** — a check that does not name its direction reads as total
to the next person.

```rust
/// DISCRIMINATION (spec §6.7). Enforces that the unrest deciles genuinely
/// SEPARATE on andosol share — direction: it catches a probe that has become
/// blind, never a world that has become uniform. Without this the readout
/// passes green while measuring nothing (The Benchmark's vacuous-and-green
/// failure).
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn unrest_deciles_differ_in_andosol_share() {
    let rows = exposure_rows(1..=30);
    let pooled: Vec<&ExposureRow> = rows.iter().filter(|r| r.people == "pooled").collect();
    let lo = pooled.iter().filter(|r| r.decile == 0).map(|r| r.andosol_share).sum::<f64>();
    let hi = pooled
        .iter()
        .filter(|r| r.decile == DECILES - 1)
        .map(|r| r.andosol_share)
        .sum::<f64>();
    assert!(
        hi > lo * 2.0,
        "top and bottom unrest deciles do not separate on andosol share \
         (bottom {lo:.4}, top {hi:.4}) — the probe is measuring nothing and \
         would pass green regardless"
    );
}

/// FLOOR AND CEILING (spec §6.7). Enforces BOTH directions: an absurdly LOW
/// exposure ratio and an absurdly HIGH one both fail. A floor alone cannot
/// catch a runaway, and a bound asserted only against the side you expect to
/// move is not a bound.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn exposure_ratios_are_within_absurdity_bounds() {
    for r in exposure_rows(1..=30).iter().filter(|r| r.land_cells > 0) {
        assert!(
            r.exposure_ratio < 20.0,
            "absurd-HIGH exposure ratio {:.2} at decile {} band {} people {} \
             — a runaway, not a finding",
            r.exposure_ratio, r.decile, r.band, r.people
        );
        assert!(
            r.exposure_ratio.is_finite(),
            "non-finite exposure ratio at decile {} band {} people {}",
            r.decile, r.band, r.people
        );
    }
}
```

- [ ] **Step 5: Add the fixture-rewrite test and generate the fixture**

Copy `occupancy_readout.rs:225-239`'s pattern exactly, including the reason
string (a rewrite test is **not** heavy-tier — it has its own class):

```rust
/// Rewrites the committed fixture. Deliberately NOT part of any gate: it
/// would silently rewrite the artifact the drift check exists to check.
#[test]
#[ignore = "regenerates the committed repose exposure fixture; run by hand - the drift check above is the gate"]
fn rewrite_repose_exposure_fixture() {
    std::fs::write(
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/repose-exposure.csv"),
        render_repose_exposure(1..=30),
    )
    .expect("write repose-exposure.csv fixture");
}
```

Run it, then re-run the drift check and confirm it now passes:

```bash
cargo test -p hornvale-worldgen --test repose_exposure -- --ignored rewrite_repose_exposure_fixture
cargo test -p hornvale-worldgen --test repose_exposure -- --ignored 2>&1 | tee /tmp/repose-t1.txt
```

- [ ] **Step 6: Read the result and record it in the module doc**

Open the fixture. Write the pooled exposure-ratio vector, the per-people
dispersion, and the elevation-stratified breakdown into the module doc comment
as a dated measurement — the readout's own record, so a later reader does not
have to re-run it to know what it said.

**This is a finding, not a gate.** All three outcomes in spec §6.5 are
informative. Do not tune anything to move the number. If it reads ≈ 1, that is
what Task 2 exists to disambiguate.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add windows/worldgen/tests/repose_exposure.rs windows/worldgen/tests/fixtures/repose-exposure.csv
git commit -m "probe(the-repose): the baseline exposure readout, taken before any hazard code

Spec §6.7 requires the reading on main's physics first, so a ~1 answer is
learned before anything is built on the premise that it is not."
```

**Note on the new fixtures directory entry:** `windows/worldgen/tests/fixtures/`
already exists and is tracked, so `git diff --exit-code` over it is not
vacuous. (The hazard only applies to a *new* generated directory with no index
entry — see CLAUDE.md's rebaseline block.)

---

### Task 2: The three-arm counterfactual

Makes a Task 1 null decidable — *true null* vs *mechanism absent* — which spec
§6.6 identifies as "the pair that matters".

**Files:**
- Create: `windows/worldgen/src/ablation.rs`
- Modify: `windows/worldgen/src/lib.rs` (add `mod ablation; pub use ablation::ChannelMask;`, and thread the mask through `per_species_suitability`)
- Modify: `windows/worldgen/tests/repose_exposure.rs` (the three arms)

**Interfaces:**
- Consumes: `render_repose_exposure` / `ExposureRow` from Task 1.
- Produces: `ChannelMask { hostility: bool, mineral_unrest: bool, andosol: bool }`
  with `ChannelMask::NONE` (all channels live) and
  `per_species_suitability_masked(…, mask: ChannelMask)`. Task 7 does not use
  these; nothing else in the campaign does.

**Why a shipped seam and not a hand-applied throwaway.** `waterline_probe.rs`
measured its counterfactual by editing the library, measuring, and reverting —
the precedent. That cannot serve here, because spec §6.6 declares this probe a
**regression tripwire**: it must re-run after any campaign touching settlement
siting or soil fertility. A reverted edit cannot re-run. So the mask ships, as
an empty channel that is an IEEE-754 no-op at `NONE` — the same deliberate
empty-channel pattern The Long Age used, and the delegation is proved
byte-identical in Step 3.

- [ ] **Step 1: Write the failing byte-identity test for the mask's identity element**

In `windows/worldgen/tests/repose_exposure.rs`:

```rust
/// The mask's identity element is an IEEE-754 no-op. Direction: this catches
/// the masked path DIVERGING from the unmasked one; it cannot catch the
/// masked path being wrong in a way the unmasked path shares.
///
/// Bit-level, not approximate: `to_bits()` equality, because "close enough"
/// is exactly the class of drift the determinism contract forbids.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn channel_mask_none_is_bit_identical_to_the_unmasked_path() {
    let wc = WorldComponents::assemble().expect("components assemble");
    for seed in [1u64, 42, 30] {
        let world = world_of(seed, &wc);
        let terrain = terrain_of(&world).unwrap();
        let climate = climate_from(&world, &terrain).unwrap();
        let plain = suitability_fields(&world, &wc, &terrain, &climate, ChannelMask::NONE);
        let base = suitability_fields_unmasked(&world, &wc, &terrain, &climate);
        assert_eq!(plain.len(), base.len(), "seed {seed}: field count moved");
        for (i, (a, b)) in plain.iter().zip(base.iter()).enumerate() {
            for cell in terrain.geosphere().cells() {
                assert_eq!(
                    a.get(cell).to_bits(),
                    b.get(cell).to_bits(),
                    "seed {seed}, species index {i}, cell {cell:?}: masked NONE diverged"
                );
            }
        }
    }
}
```

- [ ] **Step 2: Run it; confirm it fails for the right reason**

```bash
cargo test -p hornvale-worldgen --test repose_exposure -- --ignored channel_mask_none 2>&1 | tail -20
```

Expected: `ChannelMask` undefined. Then stub `ChannelMask` and
`per_species_suitability_masked` such that the masked path deliberately
returns `0.0` everywhere, re-run, and confirm the failure is the `assert_eq!`
on `to_bits()` — the behavioural red. Only then implement.

- [ ] **Step 3: Implement the mask**

`windows/worldgen/src/ablation.rs`:

```rust
//! Channel ablation for The Repose's counterfactual arm (spec §6.6).
//!
//! Unrest reaches settlement siting through two OPPOSED channels and soil
//! fertility reaches it through none (see the campaign plan §0.1/§0.2). A
//! single-channel ablation therefore cannot separate "no effect exists" from
//! "the wire was never connected" — the two live channels are the positive
//! control that proves the harness can see movement at all.
//!
//! `ChannelMask::NONE` is the identity: every channel live, and an IEEE-754
//! no-op against the unmasked path (pinned bit-for-bit by
//! `repose_exposure.rs`). Nothing in a shipped world path ever passes
//! anything else.

/// Which contributions to per-species suitability are suppressed.
///
/// A `true` field means the channel is ABLATED (zeroed), not that it is on —
/// the field names read as "suppress this", and `NONE` is all-false.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct ChannelMask {
    /// Suppress the unrest hostility penalty in carrying capacity
    /// (`k *= 1.0 - hostility`), leaving the mineral reward intact.
    /// type-audit: bare-ok(flag)
    pub hostility: bool,
    /// Suppress the unrest term inside mineral prospectivity, leaving the
    /// boundary-setting and metamorphic-grade terms intact.
    /// type-audit: bare-ok(flag)
    pub mineral_unrest: bool,
    /// Suppress andosol's fertility advantage. Expected to be inert —
    /// measuring THAT is the point (plan §0.1).
    /// type-audit: bare-ok(flag)
    pub andosol: bool,
}

impl ChannelMask {
    /// The identity: every channel live. The only value any shipped path uses.
    pub const NONE: ChannelMask = ChannelMask {
        hostility: false,
        mineral_unrest: false,
        andosol: false,
    };
}
```

In `lib.rs`, rename the existing body to `per_species_suitability_masked` with
a trailing `mask: ChannelMask` parameter, and make `per_species_suitability`
delegate with `ChannelMask::NONE`. Apply the mask at exactly two points:

- `hostility`: where `carrying_inputs_at` sets
  `let hostility = terrain.unrest_at(cell).clamp(0.0, 1.0);` (`lib.rs:716`) —
  substitute `0.0` when masked. This needs `carrying_inputs_at` to take the
  mask too; thread it, and give `carrying_inputs_of` a `ChannelMask::NONE`
  delegation of its own.
- `mineral_unrest`: where `mineral_supply_field` reads
  `terrain.prospectivity_at(c)` (`lib.rs:1012`) — when masked, recompute
  `hornvale_terrain::prospectivity(&terrain.material_at(c), terrain.boundary_at(c).map(|b| b.kind), 0.0)`
  instead. Note this zeroes **only the unrest term**; the 0.6 setting weight
  and 0.1 metamorphic term stay, which is what makes it an ablation of the
  channel rather than of the whole axis.
- `andosol`: there is no application point in the siting path. **Do not invent
  one.** The arm is implemented in Step 4 as an assertion about connectivity,
  not as an arithmetic substitution.

- [ ] **Step 4: Implement the three arms**

```rust
/// The counterfactual arm (spec §6.6, amended by plan §0.1/§0.2).
///
/// Direction: arms A and B are POSITIVE CONTROLS — they must MOVE siting, and
/// a green here means the harness can detect movement. Arm C is the null
/// under test. C alone would be an empty diff with no positive control, which
/// is exactly the evidence shape that has misled this project before.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn the_counterfactual_arms_separate_a_true_null_from_a_wiring_gap() {
    let wc = WorldComponents::assemble().expect("components assemble");
    let seeds: Vec<u64> = (1..=30).collect();

    let base = attractor_cells(&seeds, &wc, ChannelMask::NONE);

    let arm_a = attractor_cells(&seeds, &wc, ChannelMask { hostility: true, ..ChannelMask::NONE });
    let arm_b = attractor_cells(&seeds, &wc, ChannelMask { mineral_unrest: true, ..ChannelMask::NONE });

    let moved_a = symmetric_difference(&base, &arm_a);
    let moved_b = symmetric_difference(&base, &arm_b);

    // POSITIVE CONTROLS. If either of these is zero, the harness is blind and
    // arm C's null below means nothing.
    assert!(
        moved_a > 0,
        "arm A (hostility ablated) moved NO settlement across 30 seeds — the \
         ablation harness cannot see movement, so arm C proves nothing"
    );
    assert!(
        moved_b > 0,
        "arm B (mineral unrest ablated) moved NO settlement across 30 seeds — \
         the ablation harness cannot see movement, so arm C proves nothing"
    );

    // ARM C. Soil fertility has no application point in the siting path, so
    // the arm is a CONNECTIVITY assertion: no siting-path symbol reads a
    // SoilOrder. If this ever fails, someone wired The Ground into siting and
    // this probe's null is stale — which is precisely the shelf life spec §6.6
    // declares.
    let siting_sources = [
        include_str!("../src/lib.rs"),
        include_str!("../../../domains/demography/src/carrying_capacity.rs"),
        include_str!("../../../domains/demography/src/coexist.rs"),
    ];
    let reads_soil = siting_sources
        .iter()
        .any(|s| s.contains("classify_soil(") || s.contains("terrain::fertility("));
    assert!(
        !reads_soil,
        "a siting-path source now reads a soil order or soil fertility — The \
         Ground has been wired into siting, and this probe's arm-C null is \
         STALE. Re-take the reading and rewrite this assertion."
    );
}
```

**On the `include_str!` grep:** it is a coarse instrument and its limits go in
its own doc comment — it sees three files, matches two spellings, and would
miss a soil term reaching siting through a helper in a fourth file. It is not
a proof of absence; it is a tripwire on the specific wiring the arm's null
depends on. Say that in the comment. The `assert!(moved_a > 0)` controls are
what carry the real evidential weight.

- [ ] **Step 5: Run all of Task 2 and record the arm results**

```bash
cargo test -p hornvale-worldgen --test repose_exposure -- --ignored 2>&1 | tee /tmp/repose-t2.txt
```

Write the measured `moved_a` and `moved_b` counts into the module doc, dated.
They are the calibration of the instrument's sensitivity and a later reader
needs them to judge any future null.

- [ ] **Step 6: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git add windows/worldgen/src/ablation.rs windows/worldgen/src/lib.rs windows/worldgen/tests/repose_exposure.rs docs/audits/type-audit-report.md
git commit -m "probe(the-repose): three counterfactual arms, two of them positive controls

Spec §6.6 proposed ablating andosol fertility alone. Verified that soil never
reaches siting (plan §0.1), so that arm cannot move anything by construction —
its green would be an empty diff, not a null. The two live unrest channels
(hostility penalty, mineral reward) ablate as positive controls."
```

**Branch table for the type-audit report** — do not predict which way it goes:
- `docs/audits/type-audit-report.md` moved → expected, `ChannelMask` is a new
  `pub` boundary; commit it in **this** commit.
- Nothing else under `docs/audits/` moved → proceed.
- `book/src/gallery/` or `book/src/laboratory/` moved → **STOP.** A probe must
  not move a rendered artifact; something threaded the mask into a shipped
  path. Find it before committing.

---

### Task 3: `NameKind::Landform`

One variant, on its own seed path, so adding landform naming reseeds nothing.

**Files:**
- Modify: `domains/language/src/naming.rs:91-104` (the enum), `:110-116` (`label`), `:1144-1159` (syllable draw)
- Modify: `domains/language/tests/anthroponym.rs` (the save-format-contract test)

**Interfaces:**
- Produces: `hornvale_language::NameKind::Landform`, consumed by Task 5's
  `volcano_name`.

- [ ] **Step 1: Write the failing save-format-contract test**

`NameKind::label_for_test` exists (`naming.rs:123`) precisely for this. Follow
`Person`'s precedent in `domains/language/tests/anthroponym.rs`:

```rust
/// The landform seed-path label is a SAVE-FORMAT CONTRACT. Changing this
/// string silently reseeds every landform name in every saved world.
/// Direction: this pins the label against accidental edit; it cannot tell you
/// whether the label was the right choice in the first place.
#[test]
fn landform_seed_label_is_frozen() {
    assert_eq!(NameKind::Landform.label_for_test(), "landform");
}

/// A landform name draws off its OWN path, so adding landform naming to a
/// world moves no settlement, deity, epithet, or person name.
#[test]
fn landform_naming_does_not_disturb_the_other_kinds() {
    let seed = Seed(42);
    let ph = /* the fixture phonology this file already builds */;
    let namer = Namer::new(&seed, "goblin", &ph);
    let morph = /* this file's existing MorphOptions fixture */;
    let before: Vec<String> = [NameKind::Settlement, NameKind::Deity, NameKind::Person]
        .iter()
        .map(|k| namer.name(*k, 7, &morph).roman.clone())
        .collect();
    let _ = namer.name(NameKind::Landform, 7, &morph);
    let after: Vec<String> = [NameKind::Settlement, NameKind::Deity, NameKind::Person]
        .iter()
        .map(|k| namer.name(*k, 7, &morph).roman.clone())
        .collect();
    assert_eq!(before, after, "landform naming disturbed another kind's draws");
}
```

- [ ] **Step 2: Run it to verify it fails**

```bash
cargo test -p hornvale-language --test anthroponym 2>&1 | tail -20
```

Expected: `no variant named Landform found for enum NameKind`.

- [ ] **Step 3: Add the variant and let the compiler enumerate**

```rust
    /// A landform: a bare stem, drawn like a settlement's but off its own
    /// seed path so that adding landform naming to a world reseeds nothing
    /// that already exists. A landform has ONE identity and MANY names — one
    /// per people that has a word for it — so the name is keyed on
    /// `(seed, cell, species)` at the composition root, never folded into the
    /// landform's own identity.
    Landform,
```

Then:

```bash
cargo check --workspace --all-targets 2>&1 | tee /tmp/repose-widen.txt
grep -c "^error" /tmp/repose-widen.txt
```

Fix every non-exhaustive-match error the compiler reports. **Use no wildcard
arm** — the whole value of an exhaustive match is that it enumerates the
widening for you.

- [ ] **Step 4: Find the sites the compiler CANNOT see**

An `==` comparison or an `if let` on `NameKind` is not exhaustive, so the
compiler stays silent about it. Two are already known
(`naming.rs:1110` and `:1117`, both `kind == NameKind::Epithet`). Sweep for
the rest and decide each deliberately:

```bash
grep -rn "NameKind::" --include=*.rs domains/ windows/ cli/ | grep -v "match\b" | grep -E "==|!=|if let|matches!"
```

For each hit, record in the commit message whether `Landform` should fall on
the same side as the compared variant. (`kind == NameKind::Epithet` gating
honorifics is correct as-is: a landform takes no honorific.)

- [ ] **Step 5: Run the tests, then the language crate's full suite**

```bash
cargo test -p hornvale-language 2>&1 | tail -20
```

Expected: PASS, including both new tests.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add domains/language/src/naming.rs domains/language/tests/anthroponym.rs
git commit -m "feat(the-repose): NameKind::Landform on its own seed path

No landform in Hornvale has ever had a name. The variant copies Person's
precedent verbatim — its own label in the derive path, so no existing world's
names move."
```

---

### Task 4: The hazard field

`hazard(cell) -> Recurrence`, steady and non-accumulating.

**Files:**
- Create: `windows/worldgen/src/hazard.rs`
- Modify: `windows/worldgen/src/lib.rs` (`mod hazard; pub use hazard::{Recurrence, hazard_at};`)

**Interfaces:**
- Consumes: `hornvale_terrain::GeneratedTerrain` (`unrest_at`, `boundary_at`,
  `elevation_at`), `hornvale_kernel::units::Years`.
- Produces:
  - `Recurrence { seismic: Years, volcanic: Option<Years> }`
  - `hazard_at(terrain: &GeneratedTerrain, cell: CellId) -> Recurrence`
  - `has_edifice(terrain: &GeneratedTerrain, cell: CellId) -> bool`

  Task 5 consumes `has_edifice` and `Recurrence`; Task 6 consumes `Recurrence`.

**It does not accumulate.** No stress builds toward a threshold; no state
carries between events. This is constitutional, not a simplification —
`BIO-36` fixes tier-0 as a drawn stationary regime and the Lorenz guard-rail
forbids the integrator alternative outright.

- [ ] **Step 1: Write the failing tests**

```rust
/// A transform boundary is documented as "unrest, little relief" — seismic,
/// not volcanic. Direction: pins the KIND separation, not the magnitudes.
#[test]
fn a_transform_boundary_is_seismic_and_never_volcanic() { /* … */ }

/// Recurrence is a pure function of the cell's fields: same inputs, same
/// answer, every call, with no memory between calls.
#[test]
fn hazard_is_pure_and_carries_no_state() {
    let terrain = /* seed 42 */;
    let cell = /* a high-unrest land cell, found by scan */;
    let a = hazard_at(&terrain, cell);
    for _ in 0..100 {
        assert_eq!(hazard_at(&terrain, cell), a, "hazard accumulated state");
    }
}

/// Higher unrest means a SHORTER interval, monotonically. Direction: catches
/// an inverted sign, which is the defect that would make the whole campaign
/// measure backwards.
#[test]
fn higher_unrest_shortens_the_seismic_interval() { /* … */ }
```

- [ ] **Step 2: Run to verify they fail**

```bash
cargo test -p hornvale-worldgen hazard 2>&1 | tail -20
```

- [ ] **Step 3: Implement**

```rust
/// How often a cell's ground acts, as mean intervals between events.
///
/// A STEADY rate, never an accumulating stress (spec §3.1): nothing here
/// carries state between events, and the timeline of a Hornvale catastrophe
/// has no left half. That is the design, forced by `BIO-36`'s tier-0 rule and
/// the Lorenz guard-rail, not a shortcut.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Recurrence {
    /// Mean interval between seismic events at or above the catalogue's
    /// lower magnitude cutoff.
    pub seismic: Years,
    /// Mean interval between eruptions, or `None` where there is no edifice
    /// to erupt from.
    pub volcanic: Option<Years>,
}
```

`hazard_at` composes three shipped inputs and authors the mapping:

- `unrest = terrain.unrest_at(cell)` in `[0,1]`.
- `boundary = terrain.boundary_at(cell).map(|b| b.kind)` — a
  `BoundaryKind`, all six variants of which are matched exhaustively (no
  wildcard).
- edifice presence via `has_edifice`.

Volcanic recurrence is `Some` only where an edifice is present; the boundary
kinds that carry edifices are `IslandArc` and `CoastalRange`
(`domains/terrain/src/elevation.rs:204`). Every constant is **authored**, with
its authored status stated in its own doc comment, and each carries a
`type-audit:` tag.

- [ ] **Step 4: Run to verify they pass**

```bash
cargo test -p hornvale-worldgen hazard 2>&1 | tail -20
```

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git add windows/worldgen/src/hazard.rs windows/worldgen/src/lib.rs docs/audits/type-audit-report.md
git commit -m "feat(the-repose): the hazard field, steady and non-accumulating"
```

---

### Task 5: Volcano identity, and the many names of one mountain

**Files:**
- Create: `windows/worldgen/src/volcano.rs`
- Modify: `windows/worldgen/src/streams.rs` (the `VOLCANO` label)
- Modify: `windows/worldgen/src/lib.rs` (`mod volcano;` + re-exports)

**Interfaces:**
- Consumes: `hazard_at`/`has_edifice`/`Recurrence` (Task 4),
  `NameKind::Landform` (Task 3), `hornvale_kernel::{Seed, Stream, CellId}`.
- Produces:
  - `Volcano { cell: CellId, recurrence: Years, style: EruptionStyle }`
  - `volcano_at(seed: Seed, terrain: &GeneratedTerrain, cell: CellId) -> Option<Volcano>`
  - `volcano_name(seed: Seed, cell: CellId, species: &str, ph: &Phonology, morph: &MorphOptions) -> GeneratedName`

  Task 7 consumes `volcano_at`.

> **AMENDED 2026-08-14, after Task 5 shipped and was reviewed. The signatures
> immediately above are WRONG and were not built. Mine, both of them.**
>
> **`volcano_name` must not take a cell.** An edifice is 1–2 hops wide —
> measured on L6 seed 42: 187 cells at hop 0, 173 at hop 1, zero beyond, so
> 360 cells over roughly 187 gated source contacts. A name keyed on the
> *query* cell therefore gives the two halves of one mountain two different
> names, which is precisely the bug an identity rule exists to prevent. As
> built it takes `&Volcano`, so the name is keyed on the mountain's identity.
>
> **`Volcano` carries `source`, not `cell`,** for the same reason.
> `elevation.rs` samples the arc gate at the *source* boundary cell so that a
> whole edifice shares one value; keying identity anywhere else mints up to
> 360 volcanoes for ~187 cones on seed 42 alone.
>
> **A third terrain read was needed and is not in the file table above:**
> `edifice_source_at` in `domains/terrain/src/provider.rs`. `boundary_distance_at`
> discards the source (`.map(|(hops, _)| hops)`), so nothing published it. Same
> two constraints as Task 4's read, and both verified: it consumes no draw, and
> it is a strict restatement of `has_edifice` rather than a second opinion.
>
> **What actually keeps this honest** is `every_cell_of_one_edifice_resolves_to_one_volcano`,
> which is mutation-proven: keying on the query cell makes it report
> "CellId(38078) and CellId(39) resolve to different volcanoes".
>
> **A caution Task 7 inherits.** Identity is per *contact cell*, and 173 of
> 187 contacts abut another on seed 42 — so a continuous gate-on arc reads as
> a chain of separately named mountains, and a settlement's horizon can hold
> roughly ten of them along one ridge. Physically defensible at L6 (~120 km
> cells against 50–100 km real cone spacing), but not what "187 cones"
> suggests.

**The split is load-bearing.** Identity is a pure function of `(seed, cell)`;
the *name* is not, because `Namer::new(&seed, species, &phonology)` requires a
language and a mountain has no language of its own. One identity, many names —
one per people that has a word for it. The name's key `(seed, cell, species)`
is deliberately identical to knownness's holder key in Task 7: a people that
forgets its mountain also loses the name it had for it.

**Earthquakes get no identity.** Negating "localized" yields a belt with no
point of origin, which is what a quake is. Nobody names an earthquake. Do not
add a `Quake` struct.

- [ ] **Step 1: Write the failing persistence tests**

```rust
/// The SAME mountain on every recomputation — the property that makes
/// identity real without a save-format change (decision 0100's recompute
/// test puts this in the phenomenon register).
#[test]
fn a_volcano_is_identical_across_independent_recomputations() {
    let terrain_a = /* seed 42, built fresh */;
    let terrain_b = /* seed 42, built fresh AGAIN */;
    let cell = /* a known edifice cell */;
    assert_eq!(
        volcano_at(Seed(42), &terrain_a, cell),
        volcano_at(Seed(42), &terrain_b, cell),
        "a volcano changed between two independent derivations of the same seed"
    );
}

/// Different seeds give different mountains at the same cell — the guard
/// against a derivation that ignores its seed and looks stable for the wrong
/// reason.
#[test]
fn volcano_identity_actually_depends_on_the_seed() { /* … */ }

/// One mountain, many names. Direction: catches a name welded to identity;
/// it does not check that any particular name is good.
#[test]
fn one_volcano_carries_a_different_name_in_each_language() { /* … */ }
```

- [ ] **Step 2: Run to verify they fail**

- [ ] **Step 3: Add the stream label**

In `windows/worldgen/src/streams.rs`, inside the existing `stream_labels!`
block, following `CHAMBER`'s flat-path precedent:

```rust
    /// The volcano-identity derivation (The Repose). Keyed on a CELL — a
    /// place in the fixed geosphere, never a generation ordinal. The third
    /// time this project has met the "generation order is never an identity"
    /// wall (decision 0102, The Salt, The Tolerance); nothing here carries an
    /// ordinal so that mistake cannot recur.
    VOLCANO = "volcano/v1" => "the volcano-identity derivation, keyed on cell";
```

**New label = safe** (`domains/CLAUDE.md`: new label safe, changed or reused
label = an epoch). This draws from a fresh stream, so no existing world moves.

- [ ] **Step 4: Implement**

`volcano_at` returns `None` unless `has_edifice(terrain, cell)`. Where it
returns `Some`, it derives a `Stream` from `seed.derive(VOLCANO)` mixed with
the cell id, then draws the style; the recurrence comes from Task 4's
`hazard_at(...).volcanic`, not from a fresh draw (one source of truth for how
often the mountain acts).

`volcano_name` derives from the language crate's `Namer` with
`NameKind::Landform` and a salt built from the cell id.

- [ ] **Step 5: Run to verify they pass**

- [ ] **Step 6: Regenerate the stream manifest and commit**

Adding a stream label moves the generated manifest — this is the step
`few-and-many` learned to miss:

```bash
cargo run -p hornvale -- streams > /dev/null   # sanity: the dump builds
make rebaseline
git diff --stat book/src/reference/ docs/audits/
```

Branch table:
- `book/src/reference/streams.md` moved, showing exactly the one new
  `volcano/v1` row → expected; commit it here.
- An **existing** stream row moved → **STOP.** A label was changed or reused,
  which is an epoch event, not an addition.
- `book/src/gallery/` moved → **STOP.** C0 is a pure read; nothing rendered
  should move.

```bash
cargo fmt
git add windows/worldgen/src/volcano.rs windows/worldgen/src/streams.rs windows/worldgen/src/lib.rs book/src/reference/ docs/audits/
git commit -m "feat(the-repose): volcano identity, derived and never committed

One identity keyed on (seed, cell); the NAME is keyed on (seed, cell, species)
because a mountain has no language of its own. Earthquakes get no identity —
negating 'localized' yields a belt with no origin, and nobody names one."
```

---

### Task 6: The event stream and the authored-law recovery check

**Files:**
- Modify: `windows/worldgen/src/hazard.rs` (`HazardEvent`, `events_in`)
- Modify: `windows/worldgen/src/streams.rs` (the `HAZARD_EVENT` label)
- Create: `windows/worldgen/tests/repose_laws.rs`

**Interfaces:**
- Consumes: `Recurrence` and `hazard_at` (Task 4), `volcano_at` (Task 5).
- Produces:
  - `HazardEvent { day: WorldTime, kind: HazardKind, magnitude: f64 }`
  - `HazardKind { Seismic, Eruption }`
  - `events_in(seed: Seed, terrain: &GeneratedTerrain, cell: CellId, window: (WorldTime, WorldTime)) -> Vec<HazardEvent>`

  Task 7 consumes `events_in`.

**Authored, never predicted.** Gutenberg–Richter for seismicity, a VEI-shaped
law for eruptions, Poisson inter-event times given the rate. ETAS and
aftershocks are a **non-goal** (spec §2.2 and §7): ETAS is a branching process
whose control parameter is σ, the statistic `SOC-criticality` has already had
falsified twice (The Tumult, σ ≈ 0.051; The Tithe, σ ≈ 0.11 with the shape
unmoved). Adopting it would be numerically the same experiment on the same
statistic, not a new bet. If an implementer finds themselves adding a
triggering term, stop.

- [ ] **Step 1: Write the failing law-recovery test**

`windows/worldgen/tests/repose_laws.rs`:

```rust
//! §6.8's implementation check. This is `TOOL-analytic-limiting-case`
//! satisfied: the magnitude law is put in BY HAND, so recovering it proves
//! the IMPLEMENTATION carries the authored value — it proves nothing about
//! the world, and the chronicle must not present it as a finding.

/// Direction: catches a draw that does not realize the authored law. It
/// cannot catch the authored law being a poor model of real seismicity —
/// that is not a question this campaign asks.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn drawn_magnitudes_recover_the_authored_gutenberg_richter_b_value() {
    // Draw a large sample over a long window at a known-seismic cell, fit b
    // by the maximum-likelihood estimator b = log10(e) / (mean(m) - m_min),
    // and assert |b_fit - B_VALUE| < tolerance.
}

/// The inter-event times are Poisson given the rate: the mean interval
/// recovers the authored recurrence within tolerance over a long window.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn inter_event_times_recover_the_authored_recurrence() { /* … */ }
```

> **AMENDED 2026-08-14, after Task 6 shipped. The `#[ignore]` in the two
> sketches above was wrong and was correctly overruled by the implementer, on
> measurement.**
>
> These batteries run in **0.55 s** for all four tests (verified independently
> after the fact, against the implementer's reported 0.86 s). Two things follow,
> and the second is the one that matters:
>
> - The canonical heavy-tier reason string says **"(minutes)"** and is compared
>   verbatim by `cli/tests/heavy_tier.rs`. Attaching it to a sub-second test
>   puts a false claim inside a string whose whole purpose is to be exact.
> - `#[ignore]`-ing them removes the **only** check on the magnitude laws from
>   the commit gate, leaving `TOOL-analytic-limiting-case`'s guarantee to a
>   tier nobody runs on an ordinary commit.
>
> The heavy tier is for batteries that genuinely cost minutes. A cheap test
> earns its place in the gate, and this plan should not have assumed otherwise
> without measuring.

- [ ] **Step 3: Add the stream label**

```rust
    /// The per-cell hazard-event draw (The Repose). Keyed on cell and the
    /// event ordinal WITHIN a window whose bounds are part of the key, so the
    /// same query always yields the same events and no ordinal ever escapes
    /// the query that produced it.
    HAZARD_EVENT = "hazard/event/v1" => "the per-cell hazard-event draw";
```

- [ ] **Step 4: Implement**

Constants (`B_VALUE`, `M_MIN`, the VEI weights) are **authored**, each with a
doc comment saying so and a `type-audit:` tag. Draws come from
`seed.derive(HAZARD_EVENT)` mixed with the cell id and the window bounds.

> **AMENDED 2026-08-14, at Task 6's pre-dispatch verification. The sentence
> immediately above contradicts the sub-window property two paragraphs down,
> and the sentence above is the wrong half.**
>
> Keying the draw on the window bounds gives a *different stream per window*,
> so `events_in(seed, cell, (0, 100))` and `events_in(seed, cell, (0, 50))`
> would draw unrelated event sets and the second could not be a subset of the
> first. The two requirements cannot both hold as written.
>
> **The window must be a filter, not a key.** The event sequence for a
> `(seed, cell)` exists independently of who asks about it: draw inter-arrival
> times from a fixed origin, and let `events_in` walk that sequence and return
> the events falling inside the requested window. Sub-window consistency is
> then automatic rather than asserted, and any window is answerable from the
> same sequence.
>
> That is also the stronger reading of §3.3's "authored, never predicted" and
> `BIO-36`'s "narrated backwards, never forward-simulated": the events are a
> property of the world, not of the query. A design where the answer depends
> on how you asked is a simulation with extra steps.
>
> The mechanism — where the origin sits, how the sequence is indexed, how a
> long window stays cheap — is the implementer's to choose. The property is
> not: **a sub-window query returns exactly the enclosing window's events that
> fall inside it, and that must be demonstrated over real worlds rather than
> argued.**

`events_in` must be a pure function of its arguments: the same
`(seed, cell, window)` yields the same `Vec<HazardEvent>`, and a query for a
sub-window must return exactly the events of the enclosing window that fall
inside it. Add that as a test — it is the property that makes "narrated
backwards, never forward-simulated" real rather than aspirational.

- [ ] **Step 5: Run to verify they pass, and record the fitted values**

```bash
cargo test -p hornvale-worldgen --test repose_laws -- --ignored --nocapture 2>&1 | tee /tmp/repose-laws.txt
```

Write the fitted `b` and the recovered mean interval into the module doc,
dated, **alongside the sentence saying they were authored.**

- [ ] **Step 6: Commit**

```bash
cargo fmt
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
make rebaseline && git diff --stat book/src/reference/
git add windows/worldgen/src/hazard.rs windows/worldgen/src/streams.rs windows/worldgen/tests/repose_laws.rs book/src/reference/ docs/audits/
git commit -m "feat(the-repose): the event stream, drawn from an authored law

Gutenberg-Richter and a VEI-shaped law, put in by hand. ETAS is declined
deliberately: its control parameter is sigma, the statistic SOC-criticality
has had falsified twice, so adopting it would be the same experiment again."
```

---

### Task 7: Knownness, and a consumer that can see it be wrong

**Files:**
- Create: `windows/worldgen/src/knownness.rs`
- Modify: `windows/worldgen/src/lib.rs` (`mod knownness;` + re-exports)
- Modify: `windows/worldgen/tests/repose_exposure.rs` (the readout consumes it)
- Modify: `windows/worldgen/tests/fixtures/repose-exposure.csv` (a new column)

**Interfaces:**
- Consumes: `events_in` (Task 6), `volcano_at` (Task 5),
  `hornvale_species::allometry`'s `generation_length: Option<Years>`.
- Produces: `knownness(seed, terrain, species, cell, now) -> Knownness`, where
  `Knownness { stock: f64, holder: &'static str }`.

**The flow balance** `UNI-15` requires (spec §3.4):

```
  SOURCE   an eruption occurs        ->  stock := 1
  DECAY    time passes               ->  stock *= decay(half-life)
  SINK     no living memory remains  ->  stock -> 0
```

**It carries a holder** — decision 0100 requires that of anything in the myth
register — and it is explicitly permitted to contradict the hazard field. A
people may be wrong about its mountain; that is the point, not a defect.

**No cross-species memory claim is preregistered.** The half-life derives from
`generation_length`, and today `LifeSchedule::Paced` **ships with no occupant**
(`domains/species/tests/coverage.rs:284` asserts it sits at `Declared` with
zero witnesses). So lifespan is currently a function of mass, and a
long-lived-peoples prediction would be measuring **mass**. The coupling is
correct and inert now and becomes real for free when C2c lands. Do not
preregister an elves-remember/humans-forget hypothesis, and do not write one
into the chronicle.

- [ ] **Step 1: Write the failing flow-balance tests**

```rust
/// SOURCE then DECAY: knownness is 1 immediately after an eruption and
/// strictly less one half-life later.
#[test]
fn knownness_peaks_at_an_eruption_and_decays_after_it() { /* … */ }

/// SINK: after many half-lives with no event, knownness approaches zero.
#[test]
fn knownness_decays_to_nothing_when_the_mountain_is_quiet() { /* … */ }

/// It is ALLOWED to contradict the hazard field, and the readout must be
/// able to see that. Direction: this asserts the contradiction is
/// REPRESENTABLE, not that any particular world exhibits it.
#[test]
fn a_dangerous_mountain_can_be_wholly_forgotten() { /* … */ }
```

- [ ] **Step 2: Run to verify they fail**

- [ ] **Step 3: Implement**

The half-life is derived from the holder species' `generation_length`, with an
explicit fallback where it is `None`. The stock is a **fold over the event
stream** — `events_in` for the window ending at `now`, most recent eruption
wins, decay applied from its day. No state, no accumulation.

- [ ] **Step 4: Give it a consumer — extend the readout**

Spec §7 names "knownness ships with no consumer and cannot be seen to be
wrong" as a risk, mitigated by the readout consuming it (The Hollow's lesson).
Add a `knownness` column to `ExposureRow`: the population-weighted mean
knownness of the settlements in that stratum, for that people.

**Do not rename any existing column.** The fixture's existing columns must
diff cleanly so the readout's Task 1 numbers are still comparable — this is the
shared-column-diff discipline that proves a regen is additive.

- [ ] **Step 5: Regenerate the fixture and prove the regen was additive**

```bash
cargo test -p hornvale-worldgen --test repose_exposure -- --ignored rewrite_repose_exposure_fixture
git diff windows/worldgen/tests/fixtures/repose-exposure.csv | head -40
```

Branch table:
- Only the new `knownness` column appears; every shared column's values are
  byte-identical → the regen is additive. Proceed.
- A **shared** column moved → **STOP.** Task 7 changed something upstream of
  the readout that it had no business touching. Find it before committing.

Prove the shared columns explicitly rather than eyeballing the diff — cut both
versions to the pre-existing columns and compare:

```bash
git show HEAD:windows/worldgen/tests/fixtures/repose-exposure.csv | cut -d, -f1-9 > /tmp/repose-before.csv
cut -d, -f1-9 windows/worldgen/tests/fixtures/repose-exposure.csv > /tmp/repose-after.csv
diff /tmp/repose-before.csv /tmp/repose-after.csv && echo "ADDITIVE: shared columns byte-identical"
```

- [ ] **Step 6: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git add windows/worldgen/src/knownness.rs windows/worldgen/src/lib.rs windows/worldgen/tests/repose_exposure.rs windows/worldgen/tests/fixtures/repose-exposure.csv docs/audits/
git commit -m "feat(the-repose): knownness, and the readout that can see it be wrong

The stock UNI-15 requires: an eruption sets it to 1, time decays it, no living
memory sinks it. Carries a holder, and is permitted to contradict the hazard
field — a people may be wrong about its mountain. Half-life rides
generation_length and is deliberately inert until C2c gives Paced an occupant."
```

---

### Task 8: Byte-identity, the artifact sweep, and the gate

**Files:**
- Create: `windows/worldgen/tests/repose_byte_identity.rs`
- Modify: generated artifacts, via `make rebaseline`

- [ ] **Step 1: Write the byte-identity test**

Spec §5.1 and the DoD: the seed-42 almanac, the census row, and the scene
output must be identical before and after. C0 draws from no existing stream,
so this must hold by construction — the test is what makes "by construction"
checkable.

```rust
/// C0 IS A PURE READ. Direction: catches this campaign's code reaching a
/// shipped output path. It cannot catch a change that moves an output the
/// three probes below do not cover.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn seed_42_outputs_are_unmoved_by_the_repose() {
    // Compare the seed-42 almanac text, the scene JSON, and the world JSON
    // against the committed fixtures, exactly as the shipped drift checks do.
}
```

- [ ] **Step 2: Prove the test can fail — a positive control**

An empty diff needs a positive control. Temporarily make `hazard_at` write
into a shipped path (or simply assert against a deliberately corrupted
expected value), confirm the test goes RED, then revert. Record the observed
red in the commit message. Without this, a green here is indistinguishable
from a test that checks nothing.

- [ ] **Step 3: Run the full gate**

```bash
cd .claude/worktrees/the-repose
make gate 2>&1 | tee /tmp/repose-gate.txt; echo "gate exit: ${PIPESTATUS[0]}"
```

Budget ~8 minutes since decision 0113. **Stagger against the other worktrees**
— `the-docket` and `the-rill` are both live, and two concurrent gates on this
Mac cost about thirty minutes each and both look hung. Check `uptime` first; a
loadavg near or above the core count means the timings are contended and any
alarm from `make ci` is meaningless.

- [ ] **Step 4: Run the heavy tier**

The heavy tier is an **authoring** path and runs on lefford, dispatched from
the Mac with a full SHA — never a branch name:

```bash
git push -u origin the-repose
make heavy-remote REF=$(git rev-parse HEAD)
```

- [ ] **Step 5: The artifact sweep**

```bash
make rebaseline
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
```

Branch table:
- Clean → C0 really was a pure read. Proceed.
- Only `book/src/reference/streams.md` and `docs/audits/type-audit-report.md`
  moved, and both were already committed in Tasks 5–7 → expected; nothing to
  do.
- `book/src/gallery/` or `book/src/domesday/` moved → **STOP.** A pure read
  moved a rendered world. Find the leak.

- [ ] **Step 6: The Definition of Done sweep**

Spec §8, plus the standing campaign DoD:

- [ ] Chronicle entry in `book/src/chronicle/the-repose.md`, stating plainly
      that **the magnitude law was authored** — recovering it proved the
      implementation, not the world (spec §7's first risk row, and a DoD item).
- [ ] Book freshness sweep; re-score the Confidence Gradient in
      `book/src/open-questions.md` if a bet moved (decision 0030).
- [ ] Retrospective in `docs/retrospectives/the-repose.md` (decision 0020) —
      process lessons, not product. §0.1 and §0.2 of this plan belong in it.
- [ ] Registry rows in `book/src/frontier/idea-registry.md`: `DOM-15` status
      and decomposition; **new rows** for C1 consequence, C2 felt, C4 cave-in
      (anthropogenic ignition), tsunami-as-teleconnection, intraplate
      seismicity, and the shared "rare sourced-and-sunk episodic event"
      machine (registered as an observation, deliberately **not** built —
      YAGNI). No new numbered IDs (decision 0026); slugs only.
- [ ] `cargo test -p hornvale --test docs_consistency` green after the
      registry edits.

- [ ] **Step 7: Close**

Invoke the `closing-a-campaign` skill. Do not merge without it, and do not
remove the worktree until it says so.

---

## Self-Review

**Spec coverage.** Every §2.1 in-scope item maps to a task: hazard field → 4,
volcano identity → 5, event stream → 6, knownness → 7, exposure readout → 1
(with §6.6 in 2 and §6.8 in 6). Every §8 DoD checkbox appears in Task 8 or is
discharged by an earlier task's commit step. Every §2.2 non-goal is named at
the task that might drift into it (ETAS in 6, consequence facts in the global
constraints, feedback into siting in 2).

**Two places this plan deviates from the spec, both recorded in §0 with the
command output that forced them:** the counterfactual arm has three arms rather
than one, and §6.4's confound is re-identified as mineral-reward vs
hostility-penalty rather than fertility vs elevation. The stratification the
spec fixed in advance is kept unchanged.

**Type consistency.** `Recurrence` is produced in Task 4 and consumed in 5 and
6 under that name. `ChannelMask` is produced in 2 and used nowhere else.
`ExposureRow` gains columns in 2 and 7 and is never renamed. `volcano_at` takes
`(seed, terrain, cell)` in both its defining task and Task 7's use.

**Known soft spot, stated rather than hidden.** Task 2's arm-C connectivity
assertion is a source-text grep over three files and can be evaded by a helper
in a fourth. It is a tripwire on a specific wiring, not a proof of absence, and
the plan says so at the assertion. The evidential weight sits on arms A and B's
positive controls.
