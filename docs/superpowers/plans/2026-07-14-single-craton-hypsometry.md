# Single-Craton Hypsometry Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make single-continent worlds (`continents=1`) produce a real continent — bimodal hypsometry with a shelf — so `a_single_craton_world_has_a_shelf_and_a_bimodal_hypsometry` passes un-`#[ignore]`d, while every default world stays byte-identical.

**Architecture:** Route 1 of the spec (`docs/superpowers/specs/2026-07-13-single-craton-hypsometry-design.md`): when the craton set's *analytic continental supply* (a pure, grid-free function of the drawn cratons — the same accounting the Task 9 area-rescale already uses) falls far short of the ocean-fraction-implied land quota, soften the quota to `SHELF_BREAK_LAND_FACTOR × supply` land instead of forcing the percentile into the abyssal plain. This places the existing exact-percentile sea level at the craton's isostatic shelf break. Zero new stream draws, zero changes to the compute path for non-limited worlds: the fallback is a branch on two already-derived numbers, so default worlds (which draw 8–14 cratons and provably never trip the condition) keep the byte-identical percentile path.

**Tech Stack:** Rust edition 2024, `hornvale-terrain` crate only (plus docs/book close-out). No new dependencies.

## Global Constraints

- Determinism is constitutional: **no new stream draws, no reordering of existing draws** (`streams::CRATONS` / `streams::OCEAN_FRACTION` consumption is a save-format contract). The fallback must be pure arithmetic over already-derived values.
- **Default worlds must stay byte-identical.** The frozen 1000-seed census fixtures (`book/src/laboratory/generated/*/rows.csv`) include `hypsometric-bimodality` and `shelf-fraction` over default worlds and are NEVER regenerated locally; the seed-42 committed artifacts are CI drift-checked. If any default world changes, this plan has failed — stop and reassess, do not rebaseline.
- Out of scope (spec): the 0.6 rad clamp, the lobing model, multi-craton *default* sea-level behavior, fast-gate-tiers.
- No `HashMap`/`HashSet`; float sorts use `total_cmp`; quantization stays at the emit boundary only (this plan never touches an emit boundary).
- Every crate sets `#![warn(missing_docs)]`: every new public item gets a one-line doc comment and a `type-audit:` verdict tag (dimensionless ratios are `bare-ok(ratio)`).
- Every commit: `cargo fmt` (final step), `cargo clippy --workspace --all-targets -- -D warnings`, scoped tests green. Full `make gate` once, at the end (Task 5).
- Iterate cost-ordered: fmt + clippy first, tests scoped to `-p hornvale-terrain`, run once and inspect the captured output (`2>&1 | tee /tmp/hv-test.txt`) rather than re-running.

## File Structure

| File | Role in this plan |
| --- | --- |
| `domains/terrain/src/crust.rs` | New pure fn `continental_supply` + shared helper `craton_continental_steradians` (extracted from the rescale, byte-identical arithmetic) |
| `domains/terrain/src/elevation.rs` | New `effective_ocean_target` + the two constants; module-doc "Finding" paragraph rewritten; `#[ignore]` removed from the seed-3 test |
| `domains/terrain/src/globe.rs` | Two-line wiring in `generate`; one new genesis-level test |
| `domains/terrain/tests/tectonic_properties.rs` | The 1..=40 single-craton sweep + the 64-seed default-worlds separation guard |
| `docs/decisions/0053-ocean-fraction-is-a-target-under-supply-limited-crust.md` | The contract-change decision record |
| `book/src/chronicle/single-craton-hypsometry.md` (+ `book/src/SUMMARY.md`) | Chronicle entry |
| `docs/retrospectives/single-craton-hypsometry.md` | One-page process retro |

---

### Task 1: Analytic continental supply (`crust.rs`)

**Files:**
- Modify: `domains/terrain/src/crust.rs` (helper near `continental_cap_fraction` at ~line 258; rescale loop at ~lines 294–300; new pub fn after `continental_cap_fraction`; tests in the existing `tests` module)

**Interfaces:**
- Consumes: existing `Craton { radius_rad, age }`, `continental_cap_fraction(peak_km) -> f64`, private consts `PEAK_MIN_KM`/`PEAK_MAX_KM`.
- Produces: `pub fn continental_supply(cratons: &[Craton]) -> f64` — fraction of the sphere's area that is continental, in [0, 1). Task 2's `effective_ocean_target` and Task 4's separation guard consume this exact signature.

- [ ] **Step 1: Write the failing test**

Add to the `tests` module of `domains/terrain/src/crust.rs` (it already has the `default_ocean_target` helper):

```rust
#[test]
fn continental_supply_is_the_area_the_rescale_budgets() {
    // Empty set: no supply.
    assert_eq!(continental_supply(&[]), 0.0);
    // A lone pinned craton clamps at 0.6 rad: supply is capped below the
    // 0.6 rad cap area (1 - cos 0.6)/2 ~= 8.73% of the sphere times the
    // best-case (young, peak 45 km) continental fraction ~0.415 ~= 3.63%.
    for seed in 1..=8u64 {
        let terrain_seed = Seed(seed).derive(streams::ROOT);
        let ocean_target = default_ocean_target(terrain_seed);
        let pins = TerrainPins {
            continents: Some(1),
            ..TerrainPins::default()
        };
        let cratons = draw_cratons(terrain_seed, &pins, ocean_target, &mut Vec::new());
        let supply = continental_supply(&cratons);
        assert!(
            supply > 0.0 && supply < 0.037,
            "seed {seed}: supply {supply}"
        );
    }
    // Default draws (8-14 cratons): supply sits near the land budget,
    // an order of magnitude above the single-craton ceiling.
    for seed in 0..16u64 {
        let terrain_seed = Seed(seed).derive(streams::ROOT);
        let ocean_target = default_ocean_target(terrain_seed);
        let cratons = draw_cratons(
            terrain_seed,
            &TerrainPins::default(),
            ocean_target,
            &mut Vec::new(),
        );
        let supply = continental_supply(&cratons);
        assert!(
            (0.15..=0.60).contains(&supply),
            "seed {seed}: supply {supply}"
        );
    }
}
```

- [ ] **Step 2: Run it to verify it fails**

```bash
cargo test -p hornvale-terrain --lib continental_supply 2>&1 | tee /tmp/hv-test.txt
```
Expected: compile error — `continental_supply` not found.

- [ ] **Step 3: Implement**

In `domains/terrain/src/crust.rs`, immediately after `continental_cap_fraction` (~line 261), add:

```rust
/// Continental cap area of one craton, steradians: the spherical-cap
/// area `2π(1 − cos r)` times the fraction of that cap actually crossing
/// `CONTINENTAL_THRESHOLD_KM` (see `continental_cap_fraction`). One
/// arithmetic, two callers: the `draw_cratons` rescale sums it over
/// pre-rescale radii; `continental_supply` over the final set.
fn craton_continental_steradians(c: &Craton) -> f64 {
    let peak = PEAK_MIN_KM + (PEAK_MAX_KM - PEAK_MIN_KM) * (1.0 - c.age);
    std::f64::consts::TAU * (1.0 - math::cos(c.radius_rad)) * continental_cap_fraction(peak)
}

/// Analytic continental supply of a craton set: the fraction of the
/// sphere's area whose crust crosses `CONTINENTAL_THRESHOLD_KM`, summed
/// over the final (post-rescale, post-repulsion) radii. Cap overlaps are
/// not deducted — an upper estimate, consistent with the rescale's own
/// accounting. Grid-free and draw-free: a pure function of the drawn
/// set, so every grid level sees the same supply.
/// type-audit: bare-ok(ratio: return)
pub fn continental_supply(cratons: &[Craton]) -> f64 {
    cratons.iter().map(craton_continental_steradians).sum::<f64>() / (4.0 * std::f64::consts::PI)
}
```

Then make the rescale in `draw_cratons_unrepelled` (~lines 294–300) use the helper — the arithmetic is expression-for-expression identical, so this is byte-identity-safe. Replace:

```rust
    let continental_area: f64 = cratons
        .iter()
        .map(|c| {
            let peak = PEAK_MIN_KM + (PEAK_MAX_KM - PEAK_MIN_KM) * (1.0 - c.age);
            std::f64::consts::TAU * (1.0 - math::cos(c.radius_rad)) * continental_cap_fraction(peak)
        })
        .sum();
```

with:

```rust
    let continental_area: f64 = cratons.iter().map(craton_continental_steradians).sum();
```

- [ ] **Step 4: Run the crate tests to verify green (including the byte-identity-sensitive craton batteries)**

```bash
cargo test -p hornvale-terrain 2>&1 | tee /tmp/hv-test.txt
```
Expected: PASS, all tests (the existing `craton_draws_are_sequential_and_in_range`, `genesis_is_deterministic`, and pin-isolation batteries double as refactor guards).

- [ ] **Step 5: fmt, clippy, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add domains/terrain/src/crust.rs
git commit -m "feat(terrain): analytic continental supply of a craton set

Extracted from the Task 9 rescale (byte-identical arithmetic) and
exposed as a pure, grid-free function — the activation input for the
single-craton shelf-break fallback (single-craton-hypsometry plan,
Task 1)."
```

---

### Task 2: The shelf-break fallback (`elevation.rs` + `globe.rs` wiring)

**Files:**
- Modify: `domains/terrain/src/elevation.rs` (new constants + fn after `resolve_ocean_fraction`, ~line 237; unit tests in the `tests` module)
- Modify: `domains/terrain/src/globe.rs` (`generate`, ~lines 76–96; one new test)

**Interfaces:**
- Consumes: `crust::continental_supply(&[Craton]) -> f64` (Task 1).
- Produces: `pub const SUPPLY_SHORTFALL_FACTOR: f64`, `pub const SHELF_BREAK_LAND_FACTOR: f64`, and `pub fn effective_ocean_target(target: f64, supply: f64, notes: &mut Vec<String>) -> f64` in `hornvale_terrain::elevation`. Task 3 calibrates `SHELF_BREAK_LAND_FACTOR`; Task 4's guard asserts against `SUPPLY_SHORTFALL_FACTOR` and the pass-through equality.

- [ ] **Step 1: Write the failing unit tests**

Add to the `tests` module of `domains/terrain/src/elevation.rs`:

```rust
#[test]
fn effective_ocean_target_passes_ample_supply_through_silently() {
    let mut notes = Vec::new();
    assert_eq!(effective_ocean_target(0.65, 0.30, &mut notes), 0.65);
    // Exactly at the activation boundary: still the plain target.
    assert_eq!(
        effective_ocean_target(0.65, SUPPLY_SHORTFALL_FACTOR * 0.35, &mut notes),
        0.65
    );
    assert!(notes.is_empty(), "{notes:?}");
}

#[test]
fn effective_ocean_target_falls_back_to_the_shelf_break_and_meters_it() {
    let mut notes = Vec::new();
    let supply = 0.031;
    let effective = effective_ocean_target(0.65, supply, &mut notes);
    assert_eq!(effective, 1.0 - SHELF_BREAK_LAND_FACTOR * supply);
    assert_eq!(notes.len(), 1);
    assert!(notes[0].contains("shelf break"), "{}", notes[0]);
}
```

- [ ] **Step 2: Run to verify they fail**

```bash
cargo test -p hornvale-terrain --lib effective_ocean_target 2>&1 | tee /tmp/hv-test.txt
```
Expected: compile error — `effective_ocean_target` not found.

- [ ] **Step 3: Implement**

In `domains/terrain/src/elevation.rs`, after `resolve_ocean_fraction` (~line 237), add:

```rust
/// The shelf-break fallback activates when analytic continental supply
/// (`crust::continental_supply`) is below this fraction of the
/// ocean-fraction-implied land quota. The gap it bisects is wide and
/// empty: default draws (8-14 cratons) sit at supply/quota ≳ 0.7 (see
/// `default_worlds_never_trip_the_supply_fallback` in
/// `tectonic_properties.rs`), while a lone 0.6 rad-clamped craton sits
/// ≲ 0.15 — so default worlds provably keep the exact-percentile path
/// byte-identical.
/// type-audit: bare-ok(ratio)
pub const SUPPLY_SHORTFALL_FACTOR: f64 = 0.5;

/// Land granted to a supply-limited world, as a multiple of its
/// continental supply: 1.0 places the sea-level percentile where the
/// crust field crosses `crust::CONTINENTAL_THRESHOLD_KM` — the isostatic
/// shelf break. Calibrated in Task 3 of the single-craton-hypsometry
/// plan; the measured table replaces this sentence there.
/// type-audit: bare-ok(ratio)
pub const SHELF_BREAK_LAND_FACTOR: f64 = 1.0;

/// Soften the ocean-fraction target when the crust cannot honor it
/// (single-craton hypsometry spec, route 1): when the craton set's
/// analytic continental supply falls below `SUPPLY_SHORTFALL_FACTOR`
/// times the land quota `1 − target`, the exact-percentile mechanism
/// would drown into the abyssal plain to fill the quota — a broad flat
/// "land" with no shelf and no bimodality. Instead the world keeps
/// `SHELF_BREAK_LAND_FACTOR × supply` land, placing the percentile at
/// the craton's isostatic shelf break — the physically correct outcome
/// for a small-continent world. The ocean-fraction pin is thereby a
/// *target* a supply-limited world may not reach (decision 0053); the
/// softening is metered in `notes` as a degradation note. Pure — no
/// draws, so pin isolation and stream order are untouched.
/// type-audit: bare-ok(ratio: target), bare-ok(ratio: supply), bare-ok(prose: notes), bare-ok(ratio: return)
pub fn effective_ocean_target(target: f64, supply: f64, notes: &mut Vec<String>) -> f64 {
    let land_quota = 1.0 - target;
    if supply >= SUPPLY_SHORTFALL_FACTOR * land_quota {
        return target;
    }
    notes.push(format!(
        "land quota {land_quota:.2} exceeds continental supply {supply:.3}: \
         sea level set at the shelf break (ocean-fraction target {target:.2} unmet)"
    ));
    1.0 - SHELF_BREAK_LAND_FACTOR * supply
}
```

In `domains/terrain/src/globe.rs::generate`, thread it through — after the `draw_cratons` call (~line 77) insert the two new lines, and change the `derive_sea_level` call (~line 96) to use the effective target:

```rust
    let cratons = crust::draw_cratons(terrain_seed, pins, ocean_target, &mut notes);
    // Single-craton hypsometry: soften an unreachable land quota to the
    // shelf break instead of drowning the percentile into the abyss.
    let supply = crust::continental_supply(&cratons);
    let effective_ocean = elevation::effective_ocean_target(ocean_target, supply, &mut notes);
```

```rust
    let sea_level = elevation::derive_sea_level(&elevation_map, effective_ocean);
```

(`draw_cratons` still receives the raw `ocean_target` — the budget semantics are a save-format contract and do not change.)

- [ ] **Step 4: Write the genesis-level test**

Add to the `tests` module of `domains/terrain/src/globe.rs`:

```rust
#[test]
fn a_supply_limited_world_meters_the_shelf_break_fallback() {
    let geo = Geosphere::new(3);
    let pins = TerrainPins {
        continents: Some(1),
        ..TerrainPins::default()
    };
    let outcome = generate(Seed(3), &geo, &pins).expect("genesis");
    assert!(
        outcome.notes.iter().any(|n| n.contains("shelf break")),
        "fallback never engaged: {:?}",
        outcome.notes
    );
    // Default worlds never trip it — the genesis half of the
    // byte-identity guard (the craton-level half sweeps 64 seeds in
    // tectonic_properties.rs).
    let default = generate(Seed(3), &geo, &TerrainPins::default()).expect("genesis");
    assert!(!default.notes.iter().any(|n| n.contains("shelf break")));
}
```

- [ ] **Step 5: Run the crate suite**

```bash
cargo test -p hornvale-terrain 2>&1 | tee /tmp/hv-test.txt
```
Expected: PASS everywhere — in particular `pin_isolation_holds_at_the_globe_level`, `sea_level_hits_a_pinned_ocean_fraction`, and `every_default_globe_satisfies_every_invariant` must be untouched (they all run non-supply-limited scenarios, which take the pass-through branch).

- [ ] **Step 6: fmt, clippy, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add domains/terrain/src/elevation.rs domains/terrain/src/globe.rs
git commit -m "feat(terrain): supply-limited worlds set sea level at the shelf break

effective_ocean_target softens an unreachable land quota to
SHELF_BREAK_LAND_FACTOR x continental supply, metered as a degradation
note. Pure arithmetic over already-derived values: zero new draws,
default worlds take the byte-identical pass-through branch
(single-craton-hypsometry plan, Task 2)."
```

---

### Task 3: The land-normalized shelf metric; pin `SHELF_BREAK_LAND_FACTOR = 1.0` on the measured evidence

> **AMENDED 2026-07-14 (Nathan's decision, mid-execution).** The original
> Task 3 ran its κ-grid calibration probe and hit its pre-registered STOP
> rule: no κ reaches 40/40 because the whole-sphere shelf floor
> (`shelf_fraction > 0.02` of ALL cells) is geometrically unreachable for
> a ~3%-of-sphere continent — while bimodality passes everywhere (min D
> 1.71 across the whole grid) and the *land-normalized* shelf at κ = 1.0
> (median 0.17, min 0.075) overlaps and exceeds the default-world
> population's (median 0.13, min 0.097). Measured tables:
>
> Absolute-bound grid (seeds 1..=40, level 4, continents=1; pass = D>1.5
> && 0.02<shelf<0.5):
> ```
> kappa  passes/40  min-D   min-shelf  max-shelf
> 0.8            0  4.160  0.0020     0.0082
> 0.9            0  3.613  0.0027     0.0082
> 1              0  3.144  0.0023     0.0090
> 1.25           0  2.459  0.0027     0.0160
> 1.5            1  2.090  0.0035     0.0457
> 2              18  1.713  0.0078     0.0664
> ```
> Land-normalized shelf (shelf cells / land cells) at κ = 1.0, same sweep:
> min 0.0750, median 0.1705, max 0.3088 (D min 3.144, median 5.533);
> default-pins worlds seeds 0..=15: min 0.0972, median 0.1297, max 0.1649.
>
> Nathan's ruling: the metric's denominator, not the world, is wrong —
> re-express the test's shelf floor land-normalized (`shelf/land > 0.05`),
> keep `D > 1.5` and the absolute `shelf_fraction < 0.5` ceiling (which
> still guards the original drown-into-the-abyss failure), and keep
> `SHELF_BREAK_LAND_FACTOR = 1.0` — the physical shelf break, untuned.

**Files:**
- Modify: `domains/terrain/src/shape.rs` (new `shelf_land_ratio` + unit test)
- Modify: `domains/terrain/src/elevation.rs` (the constant's doc: replace the calibration-pending sentence with the measured evidence above)

**Interfaces:**
- Consumes: `SHELF_BAND_M`, `CellMap<ReferenceElevation>` — all existing.
- Produces: `pub fn shelf_land_ratio(elevation: &CellMap<ReferenceElevation>, sea_level: ReferenceElevation) -> Option<f64>` in `hornvale_terrain::shape`. Task 4's tests consume this exact signature. `SHELF_BREAK_LAND_FACTOR` stays `1.0` (value unchanged; doc updated). Do NOT register the new function as a lab metric — the census schema and the committed `lab list-metrics` reference dump are frozen.

- [ ] **Step 1: Write the failing test**

Add to the `tests` module of `domains/terrain/src/shape.rs`:

```rust
#[test]
fn shelf_land_ratio_normalizes_by_land_not_the_sphere() {
    let geo = Geosphere::new(3);
    let sea = ReferenceElevation::new(0.0).unwrap();
    // Elevation = 1000·z: land is z >= 0 (half the sphere by area), the
    // ±200 m band is |z| <= 0.2 (~20% of the sphere) → ratio ≈ 0.4.
    let e = CellMap::from_fn(&geo, |c| {
        ReferenceElevation::new(1000.0 * geo.position(c)[2]).unwrap()
    });
    let r = shelf_land_ratio(&e, sea).expect("has land");
    assert!((0.25..=0.55).contains(&r), "ratio {r}");
    // All-ocean: no land, absent.
    let ocean = CellMap::from_fn(&geo, |_| ReferenceElevation::new(-100.0).unwrap());
    assert_eq!(shelf_land_ratio(&ocean, sea), None);
}
```

- [ ] **Step 2: Run it to verify it fails**

```bash
cargo test -p hornvale-terrain --lib shelf_land_ratio 2>&1 | tee /tmp/hv-test.txt
```
Expected: compile error — `shelf_land_ratio` not found.

- [ ] **Step 3: Implement**

In `domains/terrain/src/shape.rs`, immediately after `shelf_fraction`, add (matching `shelf_fraction`'s comparison idiom exactly):

```rust
/// Shelf area relative to land area: cells within [`SHELF_BAND_M`] of sea
/// level (both sides of it), over cells at or above sea level. The
/// whole-sphere `shelf_fraction` silently assumes an Earth-scale land
/// fraction — a small-continent world can carry a proportionally healthy
/// shelf while clearing only a sliver of the sphere (decision 0053's
/// measured tables). `None` when the globe has no land.
/// type-audit: bare-ok(ratio: return)
pub fn shelf_land_ratio(
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
) -> Option<f64> {
    let mut shelf = 0usize;
    let mut land = 0usize;
    for (_, e) in elevation.iter() {
        if (*e - sea_level).abs() <= SHELF_BAND_M {
            shelf += 1;
        }
        if *e >= sea_level {
            land += 1;
        }
    }
    if land == 0 {
        return None;
    }
    Some(shelf as f64 / land as f64)
}
```

- [ ] **Step 4: Update the constant's doc with the measured evidence**

In `domains/terrain/src/elevation.rs`, replace the final sentence of `SHELF_BREAK_LAND_FACTOR`'s doc comment ("Calibrated in Task 3 … replaces this sentence there.") with the evidence, in the `REBALANCE_GAIN` house style:

```rust
/// Measured over the single-craton sweep (seeds 1..=40, level 4,
/// continents=1, drawn ocean fraction), κ grid {0.8, 0.9, 1.0, 1.25,
/// 1.5, 2.0}: no κ satisfies the whole-sphere shelf floor (best:
/// 18/40 at κ=2.0) because a ~3%-of-sphere continent cannot put 2% of
/// the sphere within ±200 m of sea level — while at κ = 1.0 the
/// land-normalized shelf (shelf cells / land cells, `shelf_land_ratio`)
/// spans 0.075–0.309 (median 0.171), overlapping and at the median
/// exceeding the default-world population's 0.097–0.165 (median 0.130),
/// with D in 3.14–5.5+. 1.0 is therefore retained — the physical shelf
/// break, untuned; the test floor is land-normalized instead
/// (decision 0053).
```

- [ ] **Step 5: Run the crate suite, fmt, clippy, commit**

```bash
cargo test -p hornvale-terrain 2>&1 | tee /tmp/hv-test.txt
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add domains/terrain/src/shape.rs domains/terrain/src/elevation.rs
git commit -m "feat(terrain): land-normalized shelf metric; SHELF_BREAK_LAND_FACTOR stays 1.0 on the measured evidence

The whole-sphere shelf floor is geometrically unreachable for a
~3%-of-sphere continent (0/40 at every kappa; best 18/40 at kappa=2);
the land-normalized shelf at the physical shelf break overlaps and
exceeds the default-world population's. Tables in the constant's doc
(single-craton-hypsometry plan, Task 3 as amended)."
```

---

### Task 4: Un-ignore the test; the sweep and the separation guard

**Files:**
- Modify: `domains/terrain/src/elevation.rs` (remove the `#[ignore]` at ~line 501; rewrite the module-doc paragraph at ~lines 26–31)
- Modify: `domains/terrain/tests/tectonic_properties.rs` (two new tests)

**Interfaces:**
- Consumes: everything from Tasks 1–3 (`continental_supply`, `effective_ocean_target`, `SUPPLY_SHORTFALL_FACTOR`, the calibrated constant). No new API.

- [ ] **Step 1: Un-ignore the seed-3 test and land-normalize its shelf floor (Task 3 amendment)**

In `domains/terrain/src/elevation.rs`, delete the entire `#[ignore = "Task 9 re-verification …"]` attribute (~lines 500–512), and replace the test body's two shelf assertions:

```rust
        let shelf = crate::shape::shelf_fraction(&globe.elevation, globe.sea_level);
        assert!(shelf > 0.02, "no shelf band: {shelf}");
        assert!(shelf < 0.5, "everything is shelf: {shelf}");
```

with the land-normalized floor plus the retained absolute ceiling:

```rust
        // Shelf floor is land-normalized (decision 0053): a ~3%-of-sphere
        // continent cannot clear an absolute whole-sphere floor, but its
        // shelf-to-land ratio matches or beats default worlds'. The
        // absolute ceiling stays — it guards the original failure mode
        // (sea level drowned into the abyssal plain, everything "shelf").
        let shelf_land = crate::shape::shelf_land_ratio(&globe.elevation, globe.sea_level)
            .expect("has land");
        assert!(shelf_land > 0.05, "no shelf band relative to land: {shelf_land}");
        let shelf = crate::shape::shelf_fraction(&globe.elevation, globe.sea_level);
        assert!(shelf < 0.5, "everything is shelf: {shelf}");
```

(The `d > 1.5` bimodality assertion is unchanged.)

- [ ] **Step 2: Run it to verify it now passes**

```bash
cargo test -p hornvale-terrain --lib a_single_craton_world_has_a_shelf_and_a_bimodal_hypsometry 2>&1 | tee /tmp/hv-test.txt
```
Expected: PASS. (Seed 3 is inside the measured sweep — its land-normalized shelf sits within the measured 0.075–0.309 range; if this fails, Task 3's evidence was misapplied — go back, don't retune here.)

- [ ] **Step 3: Rewrite the stale module-doc paragraph**

In the module doc of `domains/terrain/src/elevation.rs`, replace the final sentence of the "Finding" paragraph — from "One edge case remains structurally unfixable by these two knobs:" through "…with that math in its own annotation." (~lines 26–31) — with:

```
//! The last edge case — a *lone pinned* craton (`continents=1`) clamps
//! to exactly 0.6 rad, capping its cap area at ~8.7% of the sphere,
//! below any achievable land quota — is resolved by the shelf-break
//! fallback (`effective_ocean_target`, decision 0053): a supply-limited
//! world keeps `SHELF_BREAK_LAND_FACTOR × supply` land, placing the
//! percentile at the isostatic shelf break instead of the abyssal plain.
```

- [ ] **Step 4: Write the sweep and the separation guard**

Add to `domains/terrain/tests/tectonic_properties.rs`:

```rust
#[test]
fn single_craton_worlds_have_shelves_and_bimodal_hypsometry_across_the_sweep() {
    use hornvale_terrain::shape::{hypsometric_bimodality, shelf_fraction, shelf_land_ratio};
    let geo = Geosphere::new(4);
    let pins = TerrainPins {
        continents: Some(1),
        ..TerrainPins::default()
    };
    for seed in 1..=40u64 {
        let outcome =
            generate(Seed(seed), &geo, &pins).unwrap_or_else(|e| panic!("seed {seed}: {e}"));
        // The fallback must actually engage — a vacuous pass through the
        // percentile path would mean the activation condition drifted.
        assert!(
            outcome.notes.iter().any(|n| n.contains("shelf break")),
            "seed {seed}: fallback never engaged: {:?}",
            outcome.notes
        );
        let globe = &outcome.globe;
        let d = hypsometric_bimodality(&globe.elevation, globe.sea_level)
            .expect("has land and ocean");
        assert!(d > 1.5, "seed {seed}: hypsometry not bimodal: D = {d}");
        // Land-normalized floor + absolute ceiling (decision 0053): the
        // ceiling still guards the drowned-into-the-abyss failure mode.
        let shelf_land =
            shelf_land_ratio(&globe.elevation, globe.sea_level).expect("has land");
        assert!(
            shelf_land > 0.05,
            "seed {seed}: no shelf band relative to land: {shelf_land}"
        );
        let shelf = shelf_fraction(&globe.elevation, globe.sea_level);
        assert!(shelf < 0.5, "seed {seed}: everything is shelf: {shelf}");
    }
}

#[test]
fn default_worlds_never_trip_the_supply_fallback() {
    use hornvale_terrain::crust::{continental_supply, draw_cratons};
    use hornvale_terrain::elevation::{
        SUPPLY_SHORTFALL_FACTOR, effective_ocean_target, resolve_ocean_fraction,
    };
    // Craton-level (no genesis): cheap, and grid-free by construction —
    // this is the byte-identity proof that the fallback cannot rewrite
    // default worlds (whose frozen census fixtures and seed-42 artifacts
    // must not drift).
    for seed in 0..64u64 {
        let terrain_seed = Seed(seed).derive(streams::ROOT);
        let ocean_target =
            resolve_ocean_fraction(terrain_seed, &TerrainPins::default(), &mut Vec::new());
        let cratons = draw_cratons(
            terrain_seed,
            &TerrainPins::default(),
            ocean_target,
            &mut Vec::new(),
        );
        let supply = continental_supply(&cratons);
        let quota = 1.0 - ocean_target;
        assert!(
            supply >= SUPPLY_SHORTFALL_FACTOR * quota,
            "seed {seed}: default draw is supply-limited (supply {supply:.3} vs quota \
             {quota:.3}) — the fallback would rewrite default worlds and drift every \
             committed artifact"
        );
        let mut notes = Vec::new();
        assert_eq!(
            effective_ocean_target(ocean_target, supply, &mut notes),
            ocean_target,
            "seed {seed}: effective target diverged from the pinned-percentile path"
        );
        assert!(notes.is_empty(), "seed {seed}: {notes:?}");
    }
}
```

- [ ] **Step 5: Run the crate suite (lib + integration) and check the sweep's runtime**

```bash
cargo test -p hornvale-terrain 2>&1 | tee /tmp/hv-test.txt
```
Expected: PASS. The 40-seed level-4 sweep should cost about what the existing 64-seed default sweep costs. If (and only if) `single_craton_worlds_have_shelves_and_bimodal_hypsometry_across_the_sweep` alone exceeds ~60 s wall, mark it `#[ignore = "heavy: 40-seed level-4 single-craton sweep (~Ns); gate-full runs it"]` and rely on the un-ignored seed-3 test in the commit gate — note the choice in the commit message.

- [ ] **Step 6: fmt, clippy, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add domains/terrain/src/elevation.rs domains/terrain/tests/tectonic_properties.rs
git commit -m "test(terrain): single-craton hypsometry un-ignored; the 40-seed sweep and the default-supply separation guard

The guard proves default draws sit above SUPPLY_SHORTFALL_FACTOR x
quota, so the fallback cannot rewrite default worlds
(single-craton-hypsometry plan, Task 4)."
```

---

### Task 5: Decision record, artifacts check, book, close

**Files:**
- Create: `docs/decisions/0053-ocean-fraction-is-a-target-under-supply-limited-crust.md`
- Create: `book/src/chronicle/single-craton-hypsometry.md`; modify `book/src/SUMMARY.md` (chronicle section, alongside `the-uncommon-ground.md` etc.)
- Create: `docs/retrospectives/single-craton-hypsometry.md`
- Modify: `docs/superpowers/specs/2026-07-13-single-craton-hypsometry-design.md` (status → Shipped), this plan (statuses checked)
- Possibly modify: `docs/audits/type-audit-report.md` (regenerated)

- [ ] **Step 1: Write decision 0053**

`docs/decisions/0053-ocean-fraction-is-a-target-under-supply-limited-crust.md` (house format per `docs/decisions/README.md`; confirm 0053 is still the next free number with `ls docs/decisions/` and renumber if a parallel session claimed it):

```markdown
# 0053. The ocean-fraction pin is a target, not a guarantee, under supply-limited crust

**Status:** Accepted (2026-07-14) · **Decider:** Nathan (route 1 of the single-craton-hypsometry spec)

In the context of single-continent worlds (`continents=1`), whose lone
craton clamps at 0.6 rad (~8.7% of the sphere — below any legal
ocean-fraction-implied land quota), we decided that **when a world's
analytic continental supply falls below `SUPPLY_SHORTFALL_FACTOR`
(0.5) times its land quota, sea level is placed at the isostatic shelf
break — `SHELF_BREAK_LAND_FACTOR × supply` land via the existing
exact-percentile mechanism — instead of forcing the quota by drowning
into the abyssal plain.** The softening is deterministic (pure
arithmetic over already-derived values, zero new stream draws) and
metered as a degradation note in genesis notes.

**Context.** The percentile sea level hits the quota *exactly* by
construction; on a supply-limited world that lands it deep in the
abyssal plain — a broad flat "land" with no shelf and no hypsometric
bimodality. Raising the 0.6 rad clamp was rejected (it exists for
directed lobing; Sculpting owns that redesign), as was weakening the
test (a continent's shelf is a real property). Supply is analytic and
grid-free, so coarse-constrains-fine holds exactly; default worlds
(8-14 cratons) sit far above the activation threshold — the separation
guard (`default_worlds_never_trip_the_supply_fallback`) proves the
fallback cannot rewrite them, so committed artifacts and the frozen
censuses are untouched.

**Consequence.** For supply-limited worlds (a lone craton always; small
pinned counts sometimes), the achieved ocean fraction reported by
`summarize` exceeds a pinned `--ocean-fraction` — the pin conditions the
craton budget as before but the quota itself is honored only up to
supply. Non-limited worlds are byte-identical to before this decision.

**Amendment (same date): the shelf test floor is land-normalized.** The
pre-registered κ calibration hit its STOP rule: no κ satisfies the
whole-sphere shelf floor (`shelf_fraction > 0.02` of all cells; best
18/40 at κ=2.0) because a ~3%-of-sphere continent cannot put 2% of the
sphere within ±200 m of sea level — while at κ = 1.0 the land-normalized
shelf (`shape::shelf_land_ratio`) spans 0.075–0.309 (median 0.171),
overlapping and at the median exceeding the default-world population's
0.097–0.165 (median 0.130), with D ≥ 3.14. Nathan ruled the metric's
denominator, not the world, wrong: the single-craton tests assert
`shelf_land_ratio > 0.05` plus the retained absolute
`shelf_fraction < 0.5` ceiling (which still guards the original
drowned-into-the-abyss failure), and `SHELF_BREAK_LAND_FACTOR` stays 1.0
— the physical shelf break, untuned. This is a re-normalization with
measured evidence, not the "weaken the test" route the spec rejected:
the property "a continent has a shelf" is preserved and now scales with
the continent. The whole-sphere `shelf_fraction` metric itself is
unchanged (the frozen census depends on it); `shelf_land_ratio` is a new
unregistered shape function.
```

- [ ] **Step 2: Type-audit check (new pub items were tagged inline)**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git diff --stat docs/audits/type-audit-report.md
```
Expected: check passes; commit the report only if it changed.

- [ ] **Step 3: Artifact-freshness proof (expect zero churn)**

```bash
SKIP_CENSUS=1 scripts/regenerate-artifacts.sh
git status --short
```
Expected: **clean** — no committed artifact moves, because defaults never trip the fallback. If anything under `book/src/gallery/`, `book/src/reference/`, or `book/src/laboratory/` changed, STOP: the byte-identity guarantee is broken; revert and debug (do not rebaseline, do not touch censuses).

- [ ] **Step 4: Chronicle entry + freshness sweep**

Write `book/src/chronicle/single-craton-hypsometry.md` in the house altitude (technical prose, comprehensible without the code — model on `book/src/chronicle/the-uncommon-ground.md`): the 8.7% cap-area ceiling, why an exact percentile digs into the abyss, the shelf-break fallback, and the target-not-guarantee semantics. Add it to `book/src/SUMMARY.md`'s chronicle list.

Freshness sweep: `grep -rn "single craton\|single-craton\|8.7" book/src/ --include="*.md" | grep -v chronicle/single-craton` and update any chapter still claiming the test is ignored or the edge case unfixable. Check `book/src/open-questions.md` for a Confidence Gradient bet this touches (terrain believability); re-score only if one names this gap.

```bash
mdbook build book
cargo test -p hornvale --test docs_consistency
```
Expected: both green.

- [ ] **Step 5: Retro + spec/plan shipped markers**

Write `docs/retrospectives/single-craton-hypsometry.md` (one page, process lessons only — the STOP-rule firing and the land-normalization ruling are the story). Flip the spec's `**Status:**` line to `Shipped (2026-07-14)`, and append an amendment note at the end of the spec's Success criteria section:

```markdown
> **Amendment at execution (2026-07-14, Nathan):** criterion 1's shelf
> floor is land-normalized (`shape::shelf_land_ratio > 0.05`, absolute
> `shelf_fraction < 0.5` ceiling retained) — the measured sweep showed
> the whole-sphere 0.02 floor geometrically unreachable for a
> ~3%-of-sphere continent whose shelf-to-land ratio nonetheless matches
> or beats default worlds'. Evidence and ruling: decision 0053's
> amendment; tables in `SHELF_BREAK_LAND_FACTOR`'s doc.
```

Check off this plan's boxes.

- [ ] **Step 6: The full gate, then commit**

```bash
make gate 2>&1 | tail -20
git add docs/decisions/ docs/audits/ book/src/ docs/retrospectives/ docs/superpowers/
git commit -m "docs(terrain): single-craton hypsometry close — decision 0053, chronicle, retro

The ocean-fraction pin is a target, not a guarantee, under
supply-limited crust; committed artifacts verified unchanged."
```
Expected: gate green (~4 min). This plan does not push; merging/close follows the closing-a-campaign skill (preflight, absorb main if needed).

---

## Self-Review (done at plan-writing time)

- **Spec coverage:** criterion 1 → Task 4 step 1–2; criterion 2 → Task 4 step 4 (sweep); criterion 3 → Task 4 separation guard + Task 2 pass-through equality + Task 5 step 3 zero-churn proof; criterion 4 → Task 5 step 3 (expected nil; STOP rule if not); criterion 5 → Task 2 doc + decision 0053. "Settled first" scope question → resolved: default draws are 8–14 cratons (never 1) and the guard proves the supply condition never trips on defaults, so artifact churn is nil by construction. Out-of-scope items untouched (no clamp change, no lobing change, no multi-craton default behavior change).
- **Placeholder scan:** the one deliberately deferred value (`SHELF_BREAK_LAND_FACTOR`) ships with a concrete initial value (1.0), a pre-registered calibration grid, a decision rule, and a STOP rule — measured, not TBD.
- **Type consistency:** `continental_supply(&[Craton]) -> f64` (Tasks 1→2→4), `effective_ocean_target(f64, f64, &mut Vec<String>) -> f64` (Tasks 2→4), constants named identically throughout.
