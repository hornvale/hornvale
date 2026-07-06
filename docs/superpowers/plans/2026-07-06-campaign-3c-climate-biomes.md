# Campaign 3c: Climate & Biomes — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Finish Campaign 3 — give every world a **queryable global biome map** (land and sea) derived from the merged tectonic globe under the generated sky, with a deterministic biome-map artifact, a Census of Lands Lab study, and the C3 close-out book work.

**Architecture:** A new tier-1 `GeneratedClimate` provider inside `domains/climate` (kernel-only dependency): it consumes an elevation `CellMap<f64>` + sea level, a per-cell seafloor-feature map, and scalar stellar inputs (insolation, obliquity, rotation regime, year length), and computes temperature and moisture `CellMap`s, from which it derives a `Biome` field (land Whittaker + specials; marine depth/SST/seafloor/upwelling) and a habitability mask — all on demand, never committed as per-cell facts. The composition root (`windows/worldgen`) is the only place terrain and astronomy meet climate: it reconstructs the terrain globe and the sky, maps `terrain::BoundaryKind` → `climate::SeafloorFeature` and `astronomy::Rotation` → `climate::RotationRegime`, derives the insolation scalar from the star's `HabitableZone` and the anchor's orbit, and builds the climate provider. The tier-0 `UniformClimate` phenomena source and the hand-placed Vale keep feeding the social cascade unchanged (spec §8).

**Tech Stack:** Rust edition 2024; `hornvale-kernel` (`Geosphere`/`CellMap`/`CellId`, ledger/registry); std only (`VecDeque` for BFS/flood, no new deps); hand-rolled P6 PPM biome renderer in the First Light / elevation-map tradition.

## Global Constraints

- `serde` + `serde_json` only, workspace-wide. **No new crates** (no rand/chrono/clap/thiserror). Randomness is the kernel's `Seed`/`Stream`; climate draws essentially nothing (spec §5 model card).
- **Layering (constitutional):** `domains/climate` depends on `hornvale-kernel` and **nothing else** — never `domains/terrain` or `domains/astronomy`. All cross-domain values reach climate as bare kernel types (`CellMap<f64>`, `f64`) or climate-owned types (`SeafloorFeature`, `RotationRegime`), mapped at the composition root. Adding climate's tier-1 must not edit terrain or astronomy except for the three named openers (Tasks 1–3).
- **Determinism (constitutional):** seed-independent Geosphere; same seed + pins → byte-identical worlds, biome maps, and artifacts. No `HashMap`/`HashSet` in any ordered or serialized path (`BTreeMap`/`Vec`). Float sorting uses `total_cmp`. **No wall-clock** (`std::time` forbidden in kernel and domain code). Biome maps and habitability are recomputed, never serialized — no serde derives on `GeneratedClimate`/`Biome`/`SeafloorFeature`.
- **Biomes are a derived field, not facts** (spec §3, §6): per-cell biome is computed on demand from (temperature, moisture, elevation, seafloor). The tier-0 `biome` *fact* predicate stays with the Vale; the globe's biome is a field, so there is no registry conflict. Climate registers **no new predicate** in C3.
- **Stream labels are permanent save-format contracts.** Climate adds none in C3 (`stream_labels()` stays empty). If a future draw is added it is declared in `domains/climate/src/streams.rs`, returned fully-qualified from `stream_labels()`, and published through the crate array in `cli/src/streams.rs`.
- **Typed quantities:** elevation stays **bare f64 meters** (the C3b documented waiver — it crosses to climate without a terrain import). Temperature is bare `f64` °C and moisture a bare dimensionless `f64` in `[0, 1]` by the same convention (climate-internal derived quantities, documented in the module docs). The astronomy openers keep their newtypes (`Megameters`, `HabitableZone`).
- **Coordinate convention** (document wherever an inverse mapping or angle is built): latitude = `asin(z)`, longitude = `atan2(y, x)`, degrees; the substellar point of a locked world is the `+x` axis (`[1.0, 0.0, 0.0]`, i.e. latitude 0, longitude 0) by convention.
- Every crate sets `#![warn(missing_docs)]`; every public item, field, and variant gets a one-line `///` doc comment.
- Edition 2024; run `cargo fmt` as the final step of every task. **The full gate** (every commit must pass): `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`, plus the CI artifact drift check.
- **Baseline:** 326 tests pass on `main` at the start of this plan. Verify counts directly after each task — do not trust a summarized number.

---

## Cross-cutting design decisions (read before Task 1)

These resolve ambiguities in the spec so the executor never has to guess. They are binding for this plan.

1. **Rotation direction is fixed prograde** (declared approximation, added to the climate model card). Astronomy does not generate a spin-direction bit and the C3 openers do not add one; adding a save-format stream is out of scope. Prevailing winds come from the circulation band (equatorial easterly trades, mid-latitude westerlies, polar easterlies — the Earth convention). Retrograde worlds are deferred. This does not affect any exit criterion (§17): the showpiece pairs are spinning-vs-locked and fast-vs-slow.

2. **Biomes use annual-mean temperature and moisture** (stored `CellMap`s). Time-varying temperature (`temperature_at(cell, day)`) adds a seasonal term on top of the mean for the REPL/almanac; the biome field itself is time-independent, so the map artifact is deterministic without a time argument.

3. **The tier-0 cascade is untouched.** `UniformClimate` remains the phenomena source wired into `observed_phenomena` (the social cascade's climate seam). `GeneratedClimate` is an *additional* queryable capability (the biome map), reconstructed on demand at the root — exactly as `GeneratedTerrain` is additional while the Vale feeds settlement.

4. **Constant-sky worlds get an Earth-baseline climate** so the biome map exists for every world: insolation 1.0, obliquity 23.5°, spinning 24 h day, year 365.25 std days. Documented at the mapping site.

5. **Study & Census decisions** (Tasks 15–17): the metric registry becomes unified (existing 14 sky metrics + 7 new land metrics = 21). `studies/census-drift.study.json` is **renamed** to `studies/census-lands-drift.study.json` (the one unified CI drift study, `"all"` metrics, 500 seeds) — one study, not two, because the registry is unified. A new `studies/census-of-lands.study.json` (`"all"`, 10 000 seeds) is the author-time headline. `studies/census-of-skies.study.json` is left in place (author-time, historical) and its committed artifacts regenerated once in close-out for freshness. The band-count⇔rotation calibration and the belief⇔lock calibration both live in `windows/lab/tests/calibration.rs` and both load `census-lands-drift.study.json`.

---

## File Structure

| File | Action | Responsibility |
|---|---|---|
| `domains/astronomy/src/units.rs` | Modify | Rename `Mm` → `Megameters` (quantity macro invocation + label). |
| `domains/astronomy/src/moons.rs` | Modify | Use `Megameters` (was `Mm`). |
| `domains/astronomy/src/star.rs` | Modify | `HabitableZone` newtype (inner < outer invariant); `Star::habitable_zone: HabitableZone`. |
| `domains/astronomy/src/anchor.rs` | Modify | Read `HabitableZone` bounds via accessors (was tuple destructure). |
| `domains/astronomy/src/lib.rs` | Modify | Re-export `Megameters`, `HabitableZone` (drop `Mm`). |
| `domains/astronomy/tests/genesis_properties.rs` | Modify | Read zone via accessors. |
| `windows/worldgen/src/lib.rs` | Modify | `Sky::calendar()` accessor; `climate_of`, stellar-input mapping, seafloor mapping, biome/habitability queries, almanac land/biome lines, climate build wiring. |
| `domains/terrain/src/globe.rs` | Modify | Store the per-cell boundary field on `TectonicGlobe` (recomputed, not serialized). |
| `domains/terrain/src/provider.rs` | Modify | `boundary_at(cell) -> Option<CellBoundary>` accessor. |
| `domains/climate/src/lib.rs` | Modify | Module declarations, re-exports, keep tier-0 `UniformClimate`, keep `register_concepts`/`stream_labels`. |
| `domains/climate/src/circulation.rs` | Create | `RotationRegime`, `band_count_for`, per-cell prevailing-wind tangent. |
| `domains/climate/src/temperature.rs` | Create | Annual-mean temperature field (spinning + locked), seasonal amplitude, `temperature_at`. |
| `domains/climate/src/moisture.rs` | Create | Base moisture by band, ocean proximity, single-pass rain shadow (spinning); locked moisture. |
| `domains/climate/src/biome.rs` | Create | `Biome` enum, `SeafloorFeature`, land Whittaker + specials, marine classification, `classify`. |
| `domains/climate/src/habitability.rs` | Create | Per-cell habitability predicate + fraction. |
| `domains/climate/src/provider.rs` | Create | `GeneratedClimate`, `ClimateInputs`, `generate`, `biome_map`, `habitability`, `band_count`, `temperature_at`, `summarize`. |
| `domains/climate/src/render.rs` | Create | Deterministic P6 PPM + ASCII biome map (recolor of the elevation renderer). |
| `cli/src/main.rs` | Modify | `biome-map` command (markdown + PPM), usage text. |
| `cli/src/repl.rs` | Modify | `biome <lat> <lon>` and `biomes` commands. |
| `windows/lab/Cargo.toml` | Modify | Add `hornvale-climate` dependency. |
| `windows/lab/src/metrics.rs` | Modify | `WorldView` holds terrain globe + climate provider; 7 new land metrics. |
| `studies/census-drift.study.json` | Delete → rename | Becomes `studies/census-lands-drift.study.json`. |
| `studies/census-lands-drift.study.json` | Create | Unified CI drift study (500 seeds, `"all"`). |
| `studies/census-of-lands.study.json` | Create | Author-time headline census (10 000 seeds, `"all"`). |
| `windows/lab/tests/calibration.rs` | Modify | Load `census-lands-drift`; add band-count⇔rotation calibration. |
| `.github/workflows/ci.yml` | Modify | Biome-map renders (spinning + locked twin) + `census-lands-drift` rerun in the drift net; drop `census-drift`. |
| `book/src/gallery/biome-seed-42.md` / `.ppm` | Create (generated) | The biome-map artifact pair. |
| `book/src/gallery/biome-seed-42-locked.md` / `.ppm` | Create (generated) | The tidally-locked twin. |
| `book/src/laboratory/study-002.md` | Create | The Census of Lands chapter (comprehension-gated). |
| `book/src/chronicle/campaign-3c.md` | Create | Chronicle entry. |
| `book/src/domains/climate.md`, `book/src/domains/terrain.md` | Modify | Promote to tier 1 with model cards. |
| `book/src/domains/overview.md` | Modify | Update the tier-0 cascade table's climate/terrain rows. |
| `book/src/SUMMARY.md` | Modify | Gallery + laboratory + chronicle entries. |
| `book/src/laboratory/generated/census-lands-drift/*`, `book/src/laboratory/generated/census-drift/*` | Regenerate/remove | Rename generated dir. |
| `book/src/gallery/almanac-*.md`, `book/src/reference/*-generated.md` | Regenerate | Committed generated artifacts (new almanac biome lines; registry/manifest unchanged but regenerate to be safe). |

**Dataflow (acyclic, wired at the root):** terrain (elevation, sea level, boundary) + astronomy (insolation, obliquity, regime, year) → `climate_of` → `GeneratedClimate` → temperature + moisture → biome field + habitability → map artifact / almanac / REPL / Lab.

---

### Task 1: Astronomy opener — rename `Mm` → `Megameters`

**Files:**
- Modify: `domains/astronomy/src/units.rs`
- Modify: `domains/astronomy/src/moons.rs`
- Modify: `domains/astronomy/src/lib.rs`

**Interfaces:**
- Consumes: nothing new.
- Produces: `hornvale_astronomy::Megameters` (was `Mm`), same API (`new`, `get`, `pub(crate)` inner). `Mm` no longer exists.

This is a pure rename (the only abbreviated unit type; the mm-collision nit from the C2 close). No behavior changes.

- [ ] **Step 1: Update the failing expectation first (rename the type at its definition)**

In `domains/astronomy/src/units.rs`, change the `quantity!` invocation:

```rust
quantity!(
    Megameters,
    "megameters",
    positive,
    "Distance in Mm (1000 km; Luna orbits at 384.4)."
);
```

(Delete the old `Mm` invocation — the macro name is the type name.)

- [ ] **Step 2: Run the build to see every use site fail**

Run: `cargo build -p hornvale-astronomy 2>&1 | grep -c "cannot find type \`Mm\`"`
Expected: a non-zero count (uses in `moons.rs`, `lib.rs` re-export).

- [ ] **Step 3: Fix `moons.rs`**

In `domains/astronomy/src/moons.rs`, change the import and all `Mm(` / `: Mm` occurrences to `Megameters`:

```rust
use crate::units::{LunarMasses, Megameters, StdDays};
```
```rust
    /// Orbital distance in Mm (drawn within Roche/Hill bounds).
    pub distance: Megameters,
```
```rust
        distance: Megameters(distance),
```

(There are no other `Mm(` constructor sites; `hill_radius_mm` returns a bare `f64` and keeps its name.)

- [ ] **Step 4: Fix the `lib.rs` re-export**

In `domains/astronomy/src/lib.rs`, replace `Mm` with `Megameters` in the `pub use units::{...}` list:

```rust
pub use units::{
    Au, Degrees, EarthMasses, HabitableZone, LightYears, LocalDays, LunarMasses, Megameters,
    SolarLuminosities, SolarMasses, StdDays, UnitError,
};
```

(Note: `HabitableZone` is added in Task 2; if doing Task 1 alone, omit it here and add it in Task 2.)

- [ ] **Step 5: Run the astronomy tests and clippy**

Run: `cargo test -p hornvale-astronomy && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings`
Expected: PASS, no warnings. `cargo fmt`.

- [ ] **Step 6: Commit**

```bash
git add domains/astronomy/src/units.rs domains/astronomy/src/moons.rs domains/astronomy/src/lib.rs
git commit -m "refactor(astronomy): rename Mm -> Megameters (C2 opener)"
```

---

### Task 2: Astronomy opener — `HabitableZone` newtype

**Files:**
- Modify: `domains/astronomy/src/star.rs`
- Modify: `domains/astronomy/src/anchor.rs`
- Modify: `domains/astronomy/src/lib.rs`
- Modify: `domains/astronomy/tests/genesis_properties.rs`

**Interfaces:**
- Consumes: `Au` (existing).
- Produces: `HabitableZone { inner: Au, outer: Au }` with `HabitableZone::new(inner, outer) -> Result<HabitableZone, UnitError>` (rejects `inner >= outer` or non-finite), `.inner() -> Au`, `.outer() -> Au`, `.contains(orbit: Au) -> bool`, `.center() -> Au`. `Star::habitable_zone: HabitableZone` (was `(Au, Au)`).

Rationale (spec §13): inner < outer invariant enforced at construction; **not** `Option` — anchor-first placement makes a zone-less world unrepresentable. Consumed by climate's insolation baseline at the composition root (Task 13).

- [ ] **Step 1: Write the failing test in `units.rs`**

Add to `domains/astronomy/src/units.rs` tests:

```rust
#[test]
fn habitable_zone_enforces_inner_before_outer_and_answers_containment() {
    let zone = HabitableZone::new(Au::new(0.9).unwrap(), Au::new(1.4).unwrap()).unwrap();
    assert_eq!(zone.inner().get(), 0.9);
    assert_eq!(zone.outer().get(), 1.4);
    assert!(zone.contains(Au::new(1.0).unwrap()));
    assert!(!zone.contains(Au::new(2.0).unwrap()));
    assert!((zone.center().get() - 1.15).abs() < 1e-12);
    // inner must strictly precede outer
    assert!(HabitableZone::new(Au::new(1.4).unwrap(), Au::new(0.9).unwrap()).is_err());
    assert!(HabitableZone::new(Au::new(1.0).unwrap(), Au::new(1.0).unwrap()).is_err());
}
```

- [ ] **Step 2: Run it to see it fail**

Run: `cargo test -p hornvale-astronomy --lib habitable_zone_enforces 2>&1 | tail -5`
Expected: FAIL — `HabitableZone` not found.

- [ ] **Step 3: Implement `HabitableZone` in `units.rs`**

Add after the `Degrees` block in `domains/astronomy/src/units.rs`:

```rust
/// A star's circumstellar habitable zone: the annulus (in AU) where liquid
/// water is possible. Inner strictly precedes outer, enforced at
/// construction — anchor-first genesis makes a zone-less world
/// unrepresentable, so this is never `Option`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HabitableZone {
    inner: Au,
    outer: Au,
}

impl HabitableZone {
    /// Validating constructor: both bounds finite and `inner < outer`.
    pub fn new(inner: Au, outer: Au) -> Result<HabitableZone, UnitError> {
        if inner.0 >= outer.0 {
            return Err(UnitError {
                unit: "habitable zone",
                value: inner.0,
                reason: "inner bound must be strictly less than outer",
            });
        }
        Ok(HabitableZone { inner, outer })
    }
    /// The inner (hot) bound.
    pub fn inner(self) -> Au {
        self.inner
    }
    /// The outer (cold) bound.
    pub fn outer(self) -> Au {
        self.outer
    }
    /// The arithmetic center of the zone.
    pub fn center(self) -> Au {
        Au((self.inner.0 + self.outer.0) / 2.0)
    }
    /// Whether `orbit` lies within `[inner, outer]`.
    pub fn contains(self, orbit: Au) -> bool {
        (self.inner.0..=self.outer.0).contains(&orbit.0)
    }
}
```

- [ ] **Step 4: Adopt it in `star.rs`**

In `domains/astronomy/src/star.rs`, change the import to add `HabitableZone`, the field type, and construction:

```rust
use crate::units::{Au, HabitableZone, SolarLuminosities, SolarMasses};
```
```rust
    /// Habitable-zone bounds in AU (derived: 0.95√L inner, 1.37√L outer).
    pub habitable_zone: HabitableZone,
```
```rust
    Star {
        mass,
        luminosity,
        class_name,
        habitable_zone: HabitableZone::new(Au(0.95 * sqrt_l), Au(1.37 * sqrt_l))
            .expect("0.95√L < 1.37√L for all L > 0"),
    }
```

Update the `luminosity_and_zone_are_derived` test's destructure:

```rust
        let zone = s.habitable_zone;
        assert!((zone.inner().get() - 0.95 * expected_l.sqrt()).abs() < 1e-12);
        assert!((zone.outer().get() - 1.37 * expected_l.sqrt()).abs() < 1e-12);
        assert!(zone.inner().get() < zone.outer().get());
```

- [ ] **Step 5: Adopt it in `anchor.rs`**

In `domains/astronomy/src/anchor.rs`, replace the two `let (inner, outer) = star.habitable_zone;` destructures (and the test destructures) with accessor reads. In `generate_anchor`:

```rust
    let (inner, outer) = (star.habitable_zone.inner(), star.habitable_zone.outer());
```

In the tests (`anchor_is_deterministic_and_in_zone`, `pinned_year_places_orbit_by_kepler_or_fails_loudly`), replace `let (inner, outer) = s.habitable_zone;` with:

```rust
        let (inner, outer) = (s.habitable_zone.inner(), s.habitable_zone.outer());
```

- [ ] **Step 6: Fix `genesis_properties.rs`**

In `domains/astronomy/tests/genesis_properties.rs`, change `let (inner, outer) = system.star.habitable_zone;` to:

```rust
        let (inner, outer) = (system.star.habitable_zone.inner(), system.star.habitable_zone.outer());
```

- [ ] **Step 7: Re-export from `lib.rs`**

Ensure `pub use units::{...}` includes `HabitableZone` (see Task 1 Step 4).

- [ ] **Step 8: Run the full astronomy gate**

Run: `cargo test -p hornvale-astronomy && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

- [ ] **Step 9: Commit**

```bash
git add domains/astronomy/
git commit -m "feat(astronomy): HabitableZone newtype (inner<outer invariant; C2 opener)"
```

---

### Task 3: Worldgen opener — `Sky::calendar()` accessor

**Files:**
- Modify: `windows/worldgen/src/lib.rs`

**Interfaces:**
- Consumes: `hornvale_astronomy::Calendar` (already re-exported).
- Produces: `Sky::calendar(&self) -> Option<&hornvale_astronomy::Calendar>` — `Some` for `Generated`, `None` for `Constant`. Climate's year-length input (Task 13) reads it.

- [ ] **Step 1: Write the failing test**

Add to the `tests` module of `windows/worldgen/src/lib.rs`:

```rust
#[test]
fn sky_calendar_accessor_present_for_generated_absent_for_constant() {
    assert!(sky_of(&constant(42)).unwrap().calendar().is_none());
    let gen = sky_of(&generated(42)).unwrap();
    let cal = gen.calendar().expect("generated sky has a calendar");
    assert!(cal.year_length().get() > 0.0);
}
```

- [ ] **Step 2: Run it to see it fail**

Run: `cargo test -p hornvale-worldgen sky_calendar_accessor 2>&1 | tail -5`
Expected: FAIL — no method `calendar` on `Sky`.

- [ ] **Step 3: Add the accessor**

In `windows/worldgen/src/lib.rs`, add to `impl Sky` (next to `sky_at`):

```rust
    /// The derived calendar, if this world has a generated sky. `None` for
    /// the tier-0 constant sun, which has no cycles. Climate consumes this
    /// at the composition root (spec §13 opener).
    pub fn calendar(&self) -> Option<&hornvale_astronomy::Calendar> {
        match self {
            Sky::Constant(_) => None,
            Sky::Generated(sky) => Some(sky.calendar()),
        }
    }
```

- [ ] **Step 4: Run it**

Run: `cargo test -p hornvale-worldgen sky_calendar_accessor`
Expected: PASS. `cargo fmt`.

- [ ] **Step 5: Commit**

```bash
git add windows/worldgen/src/lib.rs
git commit -m "feat(worldgen): Sky::calendar() accessor for climate (C2 opener)"
```

---

### Task 4: Terrain — expose the per-cell boundary field

**Files:**
- Modify: `domains/terrain/src/globe.rs`
- Modify: `domains/terrain/src/provider.rs`

**Interfaces:**
- Consumes: `boundaries::boundary_field` (already computed inside `generate`, currently discarded after distance derivation).
- Produces: `TectonicGlobe.boundary: CellMap<Option<CellBoundary>>` (new public field) and `GeneratedTerrain::boundary_at(&self, id: CellId) -> Option<CellBoundary>`. Marine biomes need trench/ridge classification, mapped to `SeafloorFeature` at the root (Task 13).

The boundary field is recomputed at genesis and never serialized — same discipline as the rest of the globe.

- [ ] **Step 1: Write the failing test in `provider.rs`**

Add to `domains/terrain/src/provider.rs` tests:

```rust
#[test]
fn provider_exposes_boundary_classification() {
    let geo = Geosphere::new(3);
    let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
    let terrain = GeneratedTerrain::new(geo.clone(), outcome.clone());
    for cell in geo.cells() {
        assert_eq!(terrain.boundary_at(cell), *outcome.globe.boundary.get(cell));
    }
    // At least one cell is a classified boundary on a real globe.
    assert!(geo.cells().any(|c| terrain.boundary_at(c).is_some()));
}
```

- [ ] **Step 2: Run it to see it fail**

Run: `cargo test -p hornvale-terrain provider_exposes_boundary 2>&1 | tail -5`
Expected: FAIL — no field `boundary` / no method `boundary_at`.

- [ ] **Step 3: Add the field to `TectonicGlobe`**

In `domains/terrain/src/globe.rs`, add to the struct (import `CellBoundary`):

```rust
use crate::boundaries::{self, CellBoundary};
```
```rust
    /// The strongest cross-plate boundary contact per cell (`None` for plate
    /// interiors). Recomputed at genesis, never serialized; consumed by
    /// marine biomes via the composition root (spec §6).
    pub boundary: CellMap<Option<CellBoundary>>,
```

In `generate`, the `boundary_map` is already built; move it into the returned globe (it is currently consumed only by `boundary_distance`, which borrows it — clone into the struct, or reorder to build distances first then move). Change the assembly:

```rust
    Ok(GenesisOutcome {
        globe: TectonicGlobe {
            plate_of,
            elevation: elevation_map,
            unrest,
            sea_level,
            plates: plate_list,
            boundary: boundary_map,
        },
        notes,
    })
```

(`boundary_map` is used by `boundary_distance` earlier by shared reference `&boundary_map`; that borrow ends before the struct move, so no clone is needed. If the borrow checker complains, the two note-computing loops that read `boundary_map.iter()` must run before the move — keep them before the `Ok(...)`.)

- [ ] **Step 4: Add the provider accessor**

In `domains/terrain/src/provider.rs`, add to `impl GeneratedTerrain`:

```rust
    /// The strongest cross-plate boundary contact at a cell, if any.
    pub fn boundary_at(&self, id: CellId) -> Option<CellBoundary> {
        *self.globe.boundary.get(id)
    }
```

Add `CellBoundary` to the provider's imports:

```rust
use crate::boundaries::CellBoundary;
```

- [ ] **Step 5: Run terrain's gate**

Run: `cargo test -p hornvale-terrain && cargo clippy -p hornvale-terrain --all-targets -- -D warnings`
Expected: PASS (existing `PartialEq` derive on `TectonicGlobe` still holds — `CellMap<Option<CellBoundary>>` is `PartialEq`). `cargo fmt`.

- [ ] **Step 6: Commit**

```bash
git add domains/terrain/src/globe.rs domains/terrain/src/provider.rs
git commit -m "feat(terrain): expose per-cell boundary field for marine biomes"
```

---

### Task 5: Climate — circulation (band count from rotation + prevailing wind)

**Files:**
- Create: `domains/climate/src/circulation.rs`
- Modify: `domains/climate/src/lib.rs` (add `pub mod circulation;` and re-exports)

**Interfaces:**
- Consumes: `hornvale_kernel::{Geosphere, CellId, CellMap}`.
- Produces:
  - `enum RotationRegime { Spinning { day_std: f64 }, Locked }` (climate-owned; root maps `astronomy::Rotation`).
  - `fn band_count_for(regime: &RotationRegime) -> Option<u32>` — bands **per hemisphere**; `None` for `Locked`. The calibration function: `Spinning` day-length in hours `H` → `H >= 40 → 1`, `20 <= H < 40 → 3`, `10 <= H < 20 → 5`, `H < 10 → 7`.
  - `fn band_index(latitude_deg: f64, bands: u32) -> u32` — which band a latitude falls in, `0` at the equator.
  - `fn is_rising_band(band: u32) -> bool` — even bands (0, 2, …) rise (wet); odd sink (dry).
  - `fn wind_east_tangent(geo: &Geosphere, cell: CellId) -> [f64; 3]` — the unit eastward tangent at a cell (`normalize(cross([0,0,1], position))`; the poles fall back to `[0,0,0]`).
  - `fn prevailing_wind(geo: &Geosphere, cell: CellId, bands: u32) -> [f64; 3]` — the eastward tangent signed by band: westerly (`+east`) in odd bands, easterly (`-east`) in even bands (Earth trades at the equator are easterly), scaled to unit or zero at the poles.

- [ ] **Step 1: Write the failing tests**

Create `domains/climate/src/circulation.rs` with a tests module:

```rust
//! Atmospheric circulation: the number of banded overturning cells per
//! hemisphere is a heuristic function of rotation period (thermal-Rossby-
//! like), and each band carries a prevailing zonal wind. Prograde by
//! convention (spec §5 model card): equatorial easterly trades, mid-latitude
//! westerlies, polar easterlies. Tidal-lock is a distinct regime with no
//! bands (organized around the substellar point instead).

use hornvale_kernel::{CellId, Geosphere};

/// The rotation regime a climate is built under (climate-owned mirror of the
/// astronomy rotation; mapped at the composition root so climate imports no
/// domain).
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RotationRegime {
    /// Ordinary spin with a solar day of this many standard days.
    Spinning {
        /// Day length in standard days.
        day_std: f64,
    },
    /// Tidally locked: organized around the substellar point, no bands.
    Locked,
}

/// The number of circulation cells per hemisphere: `None` when tidally
/// locked; otherwise a step function of the solar day in hours (fast spin →
/// more, narrower bands). Earth's 24 h day yields exactly 3 (the
/// calibration point, spec §10).
pub fn band_count_for(regime: &RotationRegime) -> Option<u32> {
    match regime {
        RotationRegime::Locked => None,
        RotationRegime::Spinning { day_std } => {
            let hours = day_std * 24.0;
            Some(if hours >= 40.0 {
                1
            } else if hours >= 20.0 {
                3
            } else if hours >= 10.0 {
                5
            } else {
                7
            })
        }
    }
}

/// Which band a latitude falls in, `0` at the equator, increasing poleward.
pub fn band_index(latitude_deg: f64, bands: u32) -> u32 {
    let width = 90.0 / f64::from(bands);
    ((latitude_deg.abs() / width) as u32).min(bands - 1)
}

/// Rising (wet) bands are the even-indexed ones (equatorial, and every other
/// belt poleward); odd bands are sinking (dry) horse-latitude/polar belts.
pub fn is_rising_band(band: u32) -> bool {
    band % 2 == 0
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    #[test]
    fn earth_rotation_yields_three_bands_and_the_step_function_is_monotonic() {
        assert_eq!(band_count_for(&RotationRegime::Spinning { day_std: 1.0 }), Some(3));
        assert_eq!(band_count_for(&RotationRegime::Spinning { day_std: 48.0 / 24.0 }), Some(1));
        assert_eq!(band_count_for(&RotationRegime::Spinning { day_std: 15.0 / 24.0 }), Some(5));
        assert_eq!(band_count_for(&RotationRegime::Spinning { day_std: 6.0 / 24.0 }), Some(7));
        assert_eq!(band_count_for(&RotationRegime::Locked), None);
    }

    #[test]
    fn band_index_buckets_by_absolute_latitude() {
        assert_eq!(band_index(0.0, 3), 0);
        assert_eq!(band_index(45.0, 3), 1);
        assert_eq!(band_index(-75.0, 3), 2);
        assert_eq!(band_index(90.0, 3), 2); // clamped
        assert!(is_rising_band(0));
        assert!(!is_rising_band(1));
    }

    #[test]
    fn prevailing_wind_is_zonal_tangent_and_reverses_by_band() {
        let geo = Geosphere::new(3);
        // Some equatorial cell has a nonzero eastward tangent orthogonal to +z.
        let cell = geo
            .cells()
            .min_by(|a, b| geo.position(*a)[2].abs().total_cmp(&geo.position(*b)[2].abs()))
            .unwrap();
        let east = wind_east_tangent(&geo, cell);
        assert!((east[0] * east[0] + east[1] * east[1] + east[2] * east[2]).sqrt() > 0.5);
        // tangent is orthogonal to the local vertical (position)
        let p = geo.position(cell);
        assert!((east[0] * p[0] + east[1] * p[1] + east[2] * p[2]).abs() < 1e-9);
        // easterly at the equator (even band) points opposite the eastward tangent
        let wind = prevailing_wind(&geo, cell, 3);
        assert!(wind[0] * east[0] + wind[1] * east[1] + wind[2] * east[2] < 0.0);
    }
}
```

- [ ] **Step 2: Run to see it fail**

Run: `cargo test -p hornvale-climate circulation 2>&1 | tail -8`
Expected: FAIL — `wind_east_tangent` / `prevailing_wind` not defined.

- [ ] **Step 3: Implement the wind functions**

Append to `domains/climate/src/circulation.rs` (before the tests module):

```rust
/// Cross product a × b.
fn cross(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

/// The unit eastward tangent at a cell: `normalize(ẑ × position)`. Zero at
/// the poles, where east is undefined (their bands carry no rain-shadow
/// tracing). Coordinate convention: latitude = asin(z), longitude = atan2(y, x).
pub fn wind_east_tangent(geo: &Geosphere, cell: CellId) -> [f64; 3] {
    let east = cross([0.0, 0.0, 1.0], geo.position(cell));
    let len = (east[0] * east[0] + east[1] * east[1] + east[2] * east[2]).sqrt();
    if len < 1e-9 {
        [0.0, 0.0, 0.0]
    } else {
        [east[0] / len, east[1] / len, east[2] / len]
    }
}

/// The prevailing wind direction at a cell: the eastward tangent signed by
/// band. Even (rising) bands blow easterly (`-east`, e.g. equatorial trades);
/// odd (sinking) bands blow westerly (`+east`, e.g. mid-latitude westerlies).
/// Zero at the poles.
pub fn prevailing_wind(geo: &Geosphere, cell: CellId, bands: u32) -> [f64; 3] {
    let east = wind_east_tangent(geo, cell);
    let band = band_index(geo.coord(cell).latitude, bands);
    let sign = if is_rising_band(band) { -1.0 } else { 1.0 };
    [east[0] * sign, east[1] * sign, east[2] * sign]
}
```

- [ ] **Step 4: Wire the module and re-exports**

In `domains/climate/src/lib.rs`, add near the top (after the module doc):

```rust
pub mod circulation;

pub use circulation::{RotationRegime, band_count_for};
```

- [ ] **Step 5: Run climate tests + clippy**

Run: `cargo test -p hornvale-climate && cargo clippy -p hornvale-climate --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

- [ ] **Step 6: Commit**

```bash
git add domains/climate/src/circulation.rs domains/climate/src/lib.rs
git commit -m "feat(climate): circulation bands from rotation + prevailing wind"
```

---

### Task 6: Climate — temperature field (spinning + locked)

**Files:**
- Create: `domains/climate/src/temperature.rs`
- Modify: `domains/climate/src/lib.rs` (`pub mod temperature;`)

**Interfaces:**
- Consumes: `RotationRegime`, `Geosphere`, `CellMap<f64>` (elevation), `f64` (sea level, insolation, obliquity, year length).
- Produces:
  - `fn continentality(geo, elevation, sea_level, cell) -> f64` — `1.0` fully inland, lower with ocean neighbors (drives seasonal-swing damping), in `[0.2, 1.0]`.
  - `fn mean_temperature(geo, elevation, sea_level, insolation, regime) -> CellMap<f64>` — annual-mean °C per cell (spinning: latitude + lapse rate; locked: substellar cosine).
  - `fn seasonal_amplitude(geo, elevation, sea_level, obliquity_deg, cell) -> f64` — °C half-swing (0 for locked handled by caller via obliquity path).
  - `fn temperature_at(mean: &CellMap<f64>, geo, elevation, sea_level, obliquity_deg, year_length_std, regime, cell, day) -> f64` — mean plus the seasonal term (hemisphere-signed sinusoid on year phase); locked worlds have no seasonal term.

Constants (module): lapse rate `6.5 °C / 1000 m`; substellar convention `[1.0, 0.0, 0.0]`.

- [ ] **Step 1: Write failing tests**

Create `domains/climate/src/temperature.rs`:

```rust
//! Temperature over the globe (°C, bare f64 by documented convention).
//! Spinning worlds: an insolation baseline that falls with latitude and with
//! elevation (lapse rate), plus a hemisphere-signed seasonal swing set by
//! obliquity and damped near oceans. Tidally locked worlds: temperature is
//! organized around the substellar point (`+x`), hottest there and coldest
//! at the antistellar point. Declared approximations (spec §5 model card):
//! no ocean currents, smooth-sinusoid seasons, prograde-only.

use crate::circulation::RotationRegime;
use hornvale_kernel::{CellId, CellMap, Geosphere};

/// Dry-adiabatic-ish lapse rate: °C lost per meter of elevation above sea level.
const LAPSE_C_PER_M: f64 = 6.5 / 1000.0;
/// The substellar point of a locked world (spec convention: latitude 0,
/// longitude 0 → the +x axis).
const SUBSTELLAR: [f64; 3] = [1.0, 0.0, 0.0];

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    fn flat_ocean_then_land(geo: &Geosphere, sea: f64) -> CellMap<f64> {
        // Half the globe below sea level (x<0), half above — a crude land mask.
        CellMap::from_fn(geo, |c| if geo.position(c)[0] < 0.0 { sea - 1000.0 } else { sea + 200.0 })
    }

    #[test]
    fn temperature_falls_with_latitude_on_a_spinning_world() {
        let geo = Geosphere::new(4);
        let elev = CellMap::from_fn(&geo, |_| 0.0);
        let mean = mean_temperature(&geo, &elev, 0.0, 1.0, &RotationRegime::Spinning { day_std: 1.0 });
        let equator = geo.cells().min_by(|a, b| geo.coord(*a).latitude.abs().total_cmp(&geo.coord(*b).latitude.abs())).unwrap();
        let pole = geo.cells().max_by(|a, b| geo.coord(*a).latitude.abs().total_cmp(&geo.coord(*b).latitude.abs())).unwrap();
        assert!(mean.get(equator) > mean.get(pole), "equator must be warmer than pole");
    }

    #[test]
    fn temperature_falls_with_altitude() {
        let geo = Geosphere::new(3);
        let low = CellMap::from_fn(&geo, |_| 0.0);
        let high = CellMap::from_fn(&geo, |_| 3000.0);
        let regime = RotationRegime::Spinning { day_std: 1.0 };
        let mlow = mean_temperature(&geo, &low, 0.0, 1.0, &regime);
        let mhigh = mean_temperature(&geo, &high, 0.0, 1.0, &regime);
        for c in geo.cells() {
            assert!(mhigh.get(c) < mlow.get(c), "altitude must cool cell {}", c.0);
        }
    }

    #[test]
    fn locked_world_is_hottest_at_substellar_and_coldest_at_antistellar() {
        let geo = Geosphere::new(4);
        let elev = CellMap::from_fn(&geo, |_| 0.0);
        let mean = mean_temperature(&geo, &elev, 0.0, 1.0, &RotationRegime::Locked);
        let sub = geo.cells().max_by(|a, b| geo.position(*a)[0].total_cmp(&geo.position(*b)[0])).unwrap();
        let anti = geo.cells().min_by(|a, b| geo.position(*a)[0].total_cmp(&geo.position(*b)[0])).unwrap();
        assert!(mean.get(sub) > mean.get(anti) + 50.0, "substellar must tower over antistellar");
    }

    #[test]
    fn coastal_seasonal_swing_is_smaller_than_continental() {
        let geo = Geosphere::new(4);
        let sea = 0.0;
        let elev = flat_ocean_then_land(&geo, sea);
        // A land cell touching ocean vs a land cell deep inland at similar latitude.
        let coastal = geo.cells().find(|c| {
            elev.get(*c) >= &sea && geo.neighbors(*c).iter().any(|n| elev.get(*n) < &sea)
                && geo.coord(*c).latitude.abs() > 20.0 && geo.coord(*c).latitude.abs() < 60.0
        }).unwrap();
        let inland = geo.cells().find(|c| {
            elev.get(*c) >= &sea && geo.neighbors(*c).iter().all(|n| elev.get(*n) >= &sea)
                && geo.coord(*c).latitude.abs() > 20.0 && geo.coord(*c).latitude.abs() < 60.0
        }).unwrap();
        let ac = seasonal_amplitude(&geo, &elev, sea, 23.5, coastal);
        let ai = seasonal_amplitude(&geo, &elev, sea, 23.5, inland);
        assert!(ac < ai, "coastal swing {ac} not smaller than inland {ai}");
    }
}
```

- [ ] **Step 2: Run to see failure**

Run: `cargo test -p hornvale-climate temperature 2>&1 | tail -8`
Expected: FAIL — functions undefined.

- [ ] **Step 3: Implement**

Insert before the tests module in `domains/climate/src/temperature.rs`:

```rust
/// Continentality: `1.0` fully inland, dropping toward `0.2` as a cell gains
/// ocean neighbors. Damps the seasonal swing (the sea is a thermal buffer).
pub fn continentality(geo: &Geosphere, elevation: &CellMap<f64>, sea_level: f64, cell: CellId) -> f64 {
    let neighbors = geo.neighbors(cell);
    if neighbors.is_empty() {
        return 1.0;
    }
    let ocean = neighbors.iter().filter(|n| *elevation.get(**n) < sea_level).count();
    let land_fraction = 1.0 - ocean as f64 / neighbors.len() as f64;
    0.2 + 0.8 * land_fraction
}

/// Annual-mean temperature per cell, °C. Spinning: an insolation baseline
/// (equator warm, poles cold) minus lapse-rate cooling above sea level.
/// Locked: a substellar cosine, hottest at `+x` and floored on the night side.
pub fn mean_temperature(
    geo: &Geosphere,
    elevation: &CellMap<f64>,
    sea_level: f64,
    insolation: f64,
    regime: &RotationRegime,
) -> CellMap<f64> {
    // Equilibrium temperature scales as S^(1/4).
    let scale = insolation.max(0.0).powf(0.25);
    CellMap::from_fn(geo, |cell| {
        let above = (*elevation.get(cell) - sea_level).max(0.0);
        let lapse = LAPSE_C_PER_M * above;
        match regime {
            RotationRegime::Spinning { .. } => {
                let lat = geo.coord(cell).latitude.to_radians();
                // 30 °C equator, -30 °C pole at Earth insolation, scaled by S^(1/4).
                let base_k = 288.0 * scale;
                let lat_term = 30.0 - 60.0 * lat.sin() * lat.sin();
                (base_k - 273.15) + lat_term - lapse
            }
            RotationRegime::Locked => {
                let p = geo.position(cell);
                let cos_theta = p[0] * SUBSTELLAR[0] + p[1] * SUBSTELLAR[1] + p[2] * SUBSTELLAR[2];
                if cos_theta > 0.0 {
                    // Day side: hot at substellar, ~0 °C toward the terminator.
                    (-18.0 + 78.0 * cos_theta.powf(0.3) * scale) - lapse
                } else {
                    // Night side: a deep frozen floor.
                    -60.0 - lapse
                }
            }
        }
    })
}

/// The seasonal half-swing in °C at a cell: proportional to obliquity and to
/// continentality (coastal cells swing less). Zero when obliquity is zero.
pub fn seasonal_amplitude(
    geo: &Geosphere,
    elevation: &CellMap<f64>,
    sea_level: f64,
    obliquity_deg: f64,
    cell: CellId,
) -> f64 {
    let cont = continentality(geo, elevation, sea_level, cell);
    (obliquity_deg / 90.0) * 25.0 * cont
}

/// Temperature at a cell on a given day: the annual mean plus a
/// hemisphere-signed seasonal sinusoid on the year phase. Locked worlds have
/// no seasonal term (no year phase organizes their fixed day/night).
#[allow(clippy::too_many_arguments)]
pub fn temperature_at(
    mean: &CellMap<f64>,
    geo: &Geosphere,
    elevation: &CellMap<f64>,
    sea_level: f64,
    obliquity_deg: f64,
    year_length_std: f64,
    regime: &RotationRegime,
    cell: CellId,
    day: f64,
) -> f64 {
    let base = *mean.get(cell);
    match regime {
        RotationRegime::Locked => base,
        RotationRegime::Spinning { .. } => {
            if year_length_std <= 0.0 || obliquity_deg == 0.0 {
                return base;
            }
            let amp = seasonal_amplitude(geo, elevation, sea_level, obliquity_deg, cell);
            let phase = (day / year_length_std).rem_euclid(1.0);
            let hemi = geo.coord(cell).latitude.signum();
            base + amp * hemi * (std::f64::consts::TAU * phase).sin()
        }
    }
}
```

- [ ] **Step 4: Wire the module**

In `domains/climate/src/lib.rs` add `pub mod temperature;`.

- [ ] **Step 5: Run climate gate**

Run: `cargo test -p hornvale-climate && cargo clippy -p hornvale-climate --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

- [ ] **Step 6: Commit**

```bash
git add domains/climate/src/temperature.rs domains/climate/src/lib.rs
git commit -m "feat(climate): temperature field (spinning latitude + locked substellar)"
```

---

### Task 7: Climate — moisture field + rain shadow

**Files:**
- Create: `domains/climate/src/moisture.rs`
- Modify: `domains/climate/src/lib.rs` (`pub mod moisture;`)

**Interfaces:**
- Consumes: `circulation::{RotationRegime, band_count_for, band_index, is_rising_band, prevailing_wind}`, `Geosphere`, `CellMap<f64>` (elevation), `f64` (sea level).
- Produces: `fn moisture_field(geo, elevation, sea_level, regime) -> CellMap<f64>` — moisture in `[0, 1]` per cell. Spinning: band base (rising wet, sinking dry) + ocean-proximity bonus − rain-shadow drying (single upwind trace). Locked: peaked near the terminator (`1 - |cos θ|`), dry at substellar and antistellar.

Constants: `RAIN_SHADOW_STEPS: usize = 6` (cells traced upwind); `RAIN_SHADOW_SCALE_M: f64 = 3000.0` (barrier height fully drying a leeward cell).

- [ ] **Step 1: Write failing tests**

Create `domains/climate/src/moisture.rs`:

```rust
//! Moisture over the globe (dimensionless `[0, 1]`, bare f64). Spinning
//! worlds: a base set by the circulation band (rising belts wet, sinking
//! belts dry), raised near oceans, then dried in the lee of mountains by a
//! single upwind trace along the prevailing wind. Locked worlds: wettest near
//! the terminator, dry at the substellar and antistellar points. Declared
//! approximations (spec §5): single-pass rain shadow, no clouds/feedback.

use crate::circulation::{
    RotationRegime, band_count_for, band_index, is_rising_band, prevailing_wind,
};
use hornvale_kernel::{CellId, CellMap, Geosphere};

/// Cells traced upwind when looking for a rain-shadow barrier.
const RAIN_SHADOW_STEPS: usize = 6;
/// Barrier height (m, above the leeward cell) that fully dries it.
const RAIN_SHADOW_SCALE_M: f64 = 3000.0;
/// The substellar point (spec convention).
const SUBSTELLAR: [f64; 3] = [1.0, 0.0, 0.0];

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    #[test]
    fn moisture_is_bounded_and_wetter_in_rising_bands() {
        let geo = Geosphere::new(4);
        let elev = CellMap::from_fn(&geo, |_| 100.0); // all land, flat
        let m = moisture_field(&geo, &elev, 0.0, &RotationRegime::Spinning { day_std: 1.0 });
        for (_, v) in m.iter() {
            assert!((0.0..=1.0).contains(v));
        }
        // Equatorial (band 0, rising) mean vs horse-latitude (band 1, sinking) mean.
        let mean_band = |band: u32| {
            let cells: Vec<f64> = geo.cells()
                .filter(|c| band_index(geo.coord(*c).latitude, 3) == band)
                .map(|c| *m.get(c)).collect();
            cells.iter().sum::<f64>() / cells.len() as f64
        };
        assert!(mean_band(0) > mean_band(1), "rising band not wetter than sinking band");
    }

    #[test]
    fn leeward_of_a_ridge_is_drier_than_windward() {
        let geo = Geosphere::new(5);
        let sea = 0.0;
        // A single tall cell; compare its immediate upwind vs downwind neighbor.
        let ridge = geo.cells().nth(2000).unwrap();
        let mut elev = CellMap::from_fn(&geo, |_| 200.0);
        // Build a mutable copy via from_fn closure capturing the ridge id.
        elev = CellMap::from_fn(&geo, |c| if c == ridge { 5000.0 } else { 200.0 });
        let regime = RotationRegime::Spinning { day_std: 1.0 };
        let bands = band_count_for(&regime).unwrap();
        let wind = prevailing_wind(&geo, ridge, bands);
        // Windward neighbor: the one most upwind (opposite wind). Leeward: downwind.
        let score = |c: CellId, sign: f64| {
            let d = [
                geo.position(c)[0] - geo.position(ridge)[0],
                geo.position(c)[1] - geo.position(ridge)[1],
                geo.position(c)[2] - geo.position(ridge)[2],
            ];
            sign * (d[0] * wind[0] + d[1] * wind[1] + d[2] * wind[2])
        };
        let windward = *geo.neighbors(ridge).iter().max_by(|a, b| score(**a, -1.0).total_cmp(&score(**b, -1.0))).unwrap();
        let leeward = *geo.neighbors(ridge).iter().max_by(|a, b| score(**a, 1.0).total_cmp(&score(**b, 1.0))).unwrap();
        let m = moisture_field(&geo, &elev, sea, &regime);
        assert!(m.get(leeward) < m.get(windward), "leeward {} not drier than windward {}", m.get(leeward), m.get(windward));
    }

    #[test]
    fn locked_terminator_is_wetter_than_substellar_and_antistellar() {
        let geo = Geosphere::new(4);
        let elev = CellMap::from_fn(&geo, |_| 100.0);
        let m = moisture_field(&geo, &elev, 0.0, &RotationRegime::Locked);
        let sub = geo.cells().max_by(|a, b| geo.position(*a)[0].total_cmp(&geo.position(*b)[0])).unwrap();
        let anti = geo.cells().min_by(|a, b| geo.position(*a)[0].total_cmp(&geo.position(*b)[0])).unwrap();
        let term = geo.cells().min_by(|a, b| geo.position(*a)[0].abs().total_cmp(&geo.position(*b)[0].abs())).unwrap();
        assert!(m.get(term) > m.get(sub));
        assert!(m.get(term) > m.get(anti));
    }
}
```

- [ ] **Step 2: Run to see failure**

Run: `cargo test -p hornvale-climate moisture 2>&1 | tail -8`
Expected: FAIL — `moisture_field` undefined.

- [ ] **Step 3: Implement**

Insert before the tests module:

```rust
/// Ocean-proximity bonus: `+0.3` if the cell itself is ocean-adjacent (or is
/// ocean), tapering to `0.0` fully inland.
fn ocean_bonus(geo: &Geosphere, elevation: &CellMap<f64>, sea_level: f64, cell: CellId) -> f64 {
    if *elevation.get(cell) < sea_level {
        return 0.3;
    }
    let neighbors = geo.neighbors(cell);
    if neighbors.is_empty() {
        return 0.0;
    }
    let ocean = neighbors.iter().filter(|n| *elevation.get(**n) < sea_level).count();
    0.3 * (ocean as f64 / neighbors.len() as f64)
}

/// The rain-shadow drying at a land cell: trace up to `RAIN_SHADOW_STEPS`
/// cells upwind (each step hops to the neighbor most opposite the wind),
/// tracking the highest elevation crossed. A barrier above the cell dries it
/// in proportion to its height over `RAIN_SHADOW_SCALE_M`.
fn rain_shadow(
    geo: &Geosphere,
    elevation: &CellMap<f64>,
    cell: CellId,
    wind: [f64; 3],
) -> f64 {
    if wind == [0.0, 0.0, 0.0] {
        return 0.0;
    }
    let here = *elevation.get(cell);
    let mut current = cell;
    let mut barrier = here;
    for _ in 0..RAIN_SHADOW_STEPS {
        // Upwind neighbor = the one whose displacement is most opposite the wind.
        let cp = geo.position(current);
        let next = geo.neighbors(current).iter().copied().max_by(|a, b| {
            let da = geo.position(*a);
            let db = geo.position(*b);
            let sa = -((da[0] - cp[0]) * wind[0] + (da[1] - cp[1]) * wind[1] + (da[2] - cp[2]) * wind[2]);
            let sb = -((db[0] - cp[0]) * wind[0] + (db[1] - cp[1]) * wind[1] + (db[2] - cp[2]) * wind[2]);
            sa.total_cmp(&sb)
        });
        let Some(next) = next else { break };
        if next == current {
            break;
        }
        current = next;
        barrier = barrier.max(*elevation.get(current));
    }
    ((barrier - here) / RAIN_SHADOW_SCALE_M).clamp(0.0, 1.0)
}

/// Moisture per cell, `[0, 1]`. See the module doc for the model.
pub fn moisture_field(
    geo: &Geosphere,
    elevation: &CellMap<f64>,
    sea_level: f64,
    regime: &RotationRegime,
) -> CellMap<f64> {
    match band_count_for(regime) {
        None => {
            // Locked: wettest at the terminator (|cos θ| ≈ 0), dry at the poles
            // of the day/night axis.
            CellMap::from_fn(geo, |cell| {
                let p = geo.position(cell);
                let cos_theta = p[0] * SUBSTELLAR[0] + p[1] * SUBSTELLAR[1] + p[2] * SUBSTELLAR[2];
                let base = 0.7 * (1.0 - cos_theta.abs());
                (base + ocean_bonus(geo, elevation, sea_level, cell)).clamp(0.0, 1.0)
            })
        }
        Some(bands) => CellMap::from_fn(geo, |cell| {
            let band = band_index(geo.coord(cell).latitude, bands);
            let base = if is_rising_band(band) { 0.6 } else { 0.25 };
            let wind = prevailing_wind(geo, cell, bands);
            let dry = if *elevation.get(cell) >= sea_level {
                rain_shadow(geo, elevation, cell, wind)
            } else {
                0.0
            };
            let raw = base + ocean_bonus(geo, elevation, sea_level, cell) - 0.5 * dry;
            raw.clamp(0.0, 1.0)
        }),
    }
}
```

- [ ] **Step 4: Wire the module**

In `domains/climate/src/lib.rs` add `pub mod moisture;`.

- [ ] **Step 5: Run climate gate**

Run: `cargo test -p hornvale-climate && cargo clippy -p hornvale-climate --all-targets -- -D warnings`
Expected: PASS. Note: the `leeward_of_a_ridge` test uses level 5 (10 242 cells) to give the trace room; it is a couple ms. `cargo fmt`.

- [ ] **Step 6: Commit**

```bash
git add domains/climate/src/moisture.rs domains/climate/src/lib.rs
git commit -m "feat(climate): moisture field with banded base + rain shadow"
```

---

### Task 8: Climate — the `Biome` enum, `SeafloorFeature`, and land classification

**Files:**
- Create: `domains/climate/src/biome.rs`
- Modify: `domains/climate/src/lib.rs` (`pub mod biome;` + re-exports)

**Interfaces:**
- Consumes: nothing external (pure classification over scalars).
- Produces:
  - `enum SeafloorFeature { None, Trench, Ridge }` (climate-owned; root maps `terrain::BoundaryKind`).
  - `enum Biome { … }` — 12 land + 10 marine variants (see below), each `#[derive(Clone, Copy, Debug, PartialEq, Eq)]` with a `///` doc.
  - `impl Biome { fn is_marine(self) -> bool; fn name(self) -> &'static str; fn glyph(self) -> char; fn color(self) -> [u8; 3]; }` — `name` in kebab-case (Lab/CSV), `glyph`/`color` for the renderer (Task 12).
  - `fn classify_land(temp_c: f64, moisture: f64, elevation_m: f64, sea_level_m: f64, latitude_deg: f64) -> Biome` — specials (Ice/Alpine) then Whittaker.
  - `fn tree_line_m(latitude_deg: f64) -> f64` — `4000 - 40·|lat|`, floored at 0.

Land variants: `Ice, Tundra, Taiga, TemperateGrassland, Shrubland, TemperateForest, TemperateRainforest, Desert, Savanna, TropicalSeasonalForest, TropicalRainforest, Alpine`.
Marine variants: `SeaIce, CoralReef, KelpForest, HydrothermalVent, HadalTrench, Upwelling, Epipelagic, Mesopelagic, Bathypelagic, Abyssal`.

- [ ] **Step 1: Write failing tests**

Create `domains/climate/src/biome.rs`:

```rust
//! Biomes: a queryable field over the globe, derived per cell from
//! temperature, moisture, elevation, and (for the sea) depth, surface
//! temperature, and seafloor features. Land follows a Whittaker lookup with
//! ice/alpine specials; marine follows depth/SST/boundary/upwelling. Biomes
//! are never committed as facts (spec §3, §6) — the tier-0 `biome` fact stays
//! with the Vale.

/// A seafloor tectonic feature at an ocean cell (climate-owned; the
/// composition root maps `terrain::BoundaryKind` into this so climate imports
/// no domain).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SeafloorFeature {
    /// No notable boundary feature.
    None,
    /// A deep trench (ocean–ocean convergent subduction).
    Trench,
    /// A spreading ridge with hydrothermal vents (oceanic divergent).
    Ridge,
}

/// A biome class — terrestrial or marine.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Biome {
    /// Permanent land ice.
    Ice,
    /// Treeless cold ground.
    Tundra,
    /// Boreal coniferous forest.
    Taiga,
    /// Temperate grassland / steppe.
    TemperateGrassland,
    /// Dry temperate shrubland.
    Shrubland,
    /// Temperate broadleaf forest.
    TemperateForest,
    /// Wet temperate rainforest.
    TemperateRainforest,
    /// Hot desert.
    Desert,
    /// Tropical grassland with scattered trees.
    Savanna,
    /// Tropical forest with a dry season.
    TropicalSeasonalForest,
    /// Tropical rainforest.
    TropicalRainforest,
    /// Bare high ground above the tree line.
    Alpine,
    /// Frozen sea surface.
    SeaIce,
    /// Warm shallow reef.
    CoralReef,
    /// Cold shallow kelp forest.
    KelpForest,
    /// A hydrothermal-vent field on a spreading ridge.
    HydrothermalVent,
    /// A hadal ocean trench.
    HadalTrench,
    /// A wind-driven coastal upwelling zone (high productivity).
    Upwelling,
    /// Sunlit surface waters (0–200 m).
    Epipelagic,
    /// Twilight waters (200–1000 m).
    Mesopelagic,
    /// Dark waters (1000–4000 m).
    Bathypelagic,
    /// The abyssal plain (4000–6000 m).
    Abyssal,
}

/// The tree line in meters at a latitude: 4000 m at the equator, falling
/// 40 m per degree, floored at 0.
pub fn tree_line_m(latitude_deg: f64) -> f64 {
    (4000.0 - 40.0 * latitude_deg.abs()).max(0.0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn whittaker_hits_known_corners() {
        // Hot & wet → tropical rainforest; hot & dry → desert.
        assert_eq!(classify_land(27.0, 0.9, 300.0, 0.0, 0.0), Biome::TropicalRainforest);
        assert_eq!(classify_land(27.0, 0.05, 300.0, 0.0, 10.0), Biome::Desert);
        // Temperate mid-moisture → temperate forest.
        assert_eq!(classify_land(12.0, 0.5, 200.0, 0.0, 45.0), Biome::TemperateForest);
        // Cold → taiga/tundra.
        assert_eq!(classify_land(-2.0, 0.4, 100.0, 0.0, 60.0), Biome::Taiga);
    }

    #[test]
    fn specials_take_precedence() {
        // Below the ice threshold → Ice regardless of moisture.
        assert_eq!(classify_land(-25.0, 0.8, 100.0, 0.0, 80.0), Biome::Ice);
        // Above the tree line → Alpine.
        assert_eq!(classify_land(5.0, 0.5, 4500.0, 0.0, 0.0), Biome::Alpine);
    }

    #[test]
    fn names_are_kebab_and_unique() {
        let all = [
            Biome::Ice, Biome::Tundra, Biome::Taiga, Biome::TemperateGrassland,
            Biome::Shrubland, Biome::TemperateForest, Biome::TemperateRainforest,
            Biome::Desert, Biome::Savanna, Biome::TropicalSeasonalForest,
            Biome::TropicalRainforest, Biome::Alpine, Biome::SeaIce, Biome::CoralReef,
            Biome::KelpForest, Biome::HydrothermalVent, Biome::HadalTrench,
            Biome::Upwelling, Biome::Epipelagic, Biome::Mesopelagic,
            Biome::Bathypelagic, Biome::Abyssal,
        ];
        let mut names: Vec<&str> = all.iter().map(|b| b.name()).collect();
        for n in &names {
            assert!(n.chars().all(|c| c.is_ascii_lowercase() || c == '-'), "not kebab: {n}");
        }
        let len = names.len();
        names.sort();
        names.dedup();
        assert_eq!(names.len(), len, "duplicate biome names");
        assert_eq!(len, 22);
    }
}
```

- [ ] **Step 2: Run to see failure**

Run: `cargo test -p hornvale-climate biome 2>&1 | tail -8`
Expected: FAIL — `classify_land`, `name` undefined.

- [ ] **Step 3: Implement the accessors and land classifier**

Insert before the tests module (after `tree_line_m`):

```rust
/// Ice threshold: annual-mean below this is permanent ice.
const ICE_C: f64 = -20.0;

impl Biome {
    /// True for the marine variants.
    pub fn is_marine(self) -> bool {
        matches!(
            self,
            Biome::SeaIce | Biome::CoralReef | Biome::KelpForest | Biome::HydrothermalVent
                | Biome::HadalTrench | Biome::Upwelling | Biome::Epipelagic | Biome::Mesopelagic
                | Biome::Bathypelagic | Biome::Abyssal
        )
    }

    /// The canonical kebab-case name (Lab metrics, CSV, book prose).
    pub fn name(self) -> &'static str {
        match self {
            Biome::Ice => "ice",
            Biome::Tundra => "tundra",
            Biome::Taiga => "taiga",
            Biome::TemperateGrassland => "temperate-grassland",
            Biome::Shrubland => "shrubland",
            Biome::TemperateForest => "temperate-forest",
            Biome::TemperateRainforest => "temperate-rainforest",
            Biome::Desert => "desert",
            Biome::Savanna => "savanna",
            Biome::TropicalSeasonalForest => "tropical-seasonal-forest",
            Biome::TropicalRainforest => "tropical-rainforest",
            Biome::Alpine => "alpine",
            Biome::SeaIce => "sea-ice",
            Biome::CoralReef => "coral-reef",
            Biome::KelpForest => "kelp-forest",
            Biome::HydrothermalVent => "hydrothermal-vent",
            Biome::HadalTrench => "hadal-trench",
            Biome::Upwelling => "upwelling",
            Biome::Epipelagic => "epipelagic",
            Biome::Mesopelagic => "mesopelagic",
            Biome::Bathypelagic => "bathypelagic",
            Biome::Abyssal => "abyssal",
        }
    }
}

/// Classify a land cell. Specials first (ice below `ICE_C`, alpine above the
/// tree line), then a Whittaker lookup on (annual-mean temperature, moisture).
pub fn classify_land(
    temp_c: f64,
    moisture: f64,
    elevation_m: f64,
    sea_level_m: f64,
    latitude_deg: f64,
) -> Biome {
    if temp_c < ICE_C {
        return Biome::Ice;
    }
    if elevation_m - sea_level_m > tree_line_m(latitude_deg) {
        return Biome::Alpine;
    }
    if temp_c < 0.0 {
        // Cold: dry tundra, wetter taiga.
        if moisture < 0.35 { Biome::Tundra } else { Biome::Taiga }
    } else if temp_c < 7.0 {
        if moisture < 0.3 { Biome::Tundra } else { Biome::Taiga }
    } else if temp_c < 20.0 {
        // Temperate.
        if moisture < 0.25 {
            Biome::TemperateGrassland
        } else if moisture < 0.4 {
            Biome::Shrubland
        } else if moisture < 0.75 {
            Biome::TemperateForest
        } else {
            Biome::TemperateRainforest
        }
    } else {
        // Hot.
        if moisture < 0.2 {
            Biome::Desert
        } else if moisture < 0.45 {
            Biome::Savanna
        } else if moisture < 0.7 {
            Biome::TropicalSeasonalForest
        } else {
            Biome::TropicalRainforest
        }
    }
}
```

(The `glyph`/`color` accessors are added in Task 12, the renderer task, to keep this task's diff focused on classification. If you prefer them here, add them now — they must exist before Task 12's tests.)

- [ ] **Step 4: Wire the module + re-exports**

In `domains/climate/src/lib.rs`:

```rust
pub mod biome;

pub use biome::{Biome, SeafloorFeature};
```

- [ ] **Step 5: Run climate gate**

Run: `cargo test -p hornvale-climate && cargo clippy -p hornvale-climate --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

- [ ] **Step 6: Commit**

```bash
git add domains/climate/src/biome.rs domains/climate/src/lib.rs
git commit -m "feat(climate): Biome enum, SeafloorFeature, land Whittaker classifier"
```

---

### Task 9: Climate — marine classification + full `classify`

**Files:**
- Modify: `domains/climate/src/biome.rs`

**Interfaces:**
- Produces:
  - `fn classify_marine(depth_m: f64, sst_c: f64, feature: SeafloorFeature, upwelling: bool) -> Biome`.
  - `fn classify(temp_c, moisture, sst_c, elevation_m, sea_level_m, latitude_deg, feature, upwelling) -> Biome` — dispatches land vs. marine on `elevation < sea_level`.

Marine precedence: `SeaIce` (sst < −2) → `HadalTrench` (feature Trench, depth > 6000) → `HydrothermalVent` (feature Ridge) → `CoralReef` (depth < 200, sst > 20) → `KelpForest` (depth < 200, sst < 12) → `Upwelling` (upwelling && depth < 1000) → depth zone (`Epipelagic` < 200, `Mesopelagic` < 1000, `Bathypelagic` < 4000, `Abyssal` < 6000, else `HadalTrench`).

- [ ] **Step 1: Write failing tests**

Add to `domains/climate/src/biome.rs` tests module:

```rust
#[test]
fn marine_precedence_is_correct() {
    // Warm shallow → reef; cold shallow → kelp.
    assert_eq!(classify_marine(50.0, 25.0, SeafloorFeature::None, false), Biome::CoralReef);
    assert_eq!(classify_marine(50.0, 8.0, SeafloorFeature::None, false), Biome::KelpForest);
    // Frozen surface beats everything.
    assert_eq!(classify_marine(50.0, -3.0, SeafloorFeature::Ridge, false), Biome::SeaIce);
    // Ridge → vent; ocean-ocean trench (deep) → hadal.
    assert_eq!(classify_marine(3000.0, 4.0, SeafloorFeature::Ridge, false), Biome::HydrothermalVent);
    assert_eq!(classify_marine(7000.0, 2.0, SeafloorFeature::Trench, false), Biome::HadalTrench);
    // Upwelling on a productive shelf.
    assert_eq!(classify_marine(300.0, 15.0, SeafloorFeature::None, true), Biome::Upwelling);
    // Plain depth zones.
    assert_eq!(classify_marine(500.0, 10.0, SeafloorFeature::None, false), Biome::Mesopelagic);
    assert_eq!(classify_marine(5000.0, 3.0, SeafloorFeature::None, false), Biome::Abyssal);
}

#[test]
fn classify_dispatches_land_and_sea() {
    // Below sea level → marine.
    let m = classify(10.0, 0.5, 22.0, -50.0, 0.0, 20.0, SeafloorFeature::None, false);
    assert!(m.is_marine());
    // Above sea level → land.
    let l = classify(25.0, 0.9, 25.0, 300.0, 0.0, 0.0, SeafloorFeature::None, false);
    assert_eq!(l, Biome::TropicalRainforest);
}
```

- [ ] **Step 2: Run to see failure**

Run: `cargo test -p hornvale-climate marine_precedence 2>&1 | tail -6`
Expected: FAIL — `classify_marine` undefined.

- [ ] **Step 3: Implement**

Add after `classify_land`:

```rust
/// Frozen-surface threshold (°C).
const SEA_ICE_C: f64 = -2.0;

/// Classify a marine cell by depth, surface temperature, seafloor feature,
/// and upwelling, in precedence order (see the task's interface note).
pub fn classify_marine(
    depth_m: f64,
    sst_c: f64,
    feature: SeafloorFeature,
    upwelling: bool,
) -> Biome {
    if sst_c < SEA_ICE_C {
        return Biome::SeaIce;
    }
    if feature == SeafloorFeature::Trench && depth_m > 6000.0 {
        return Biome::HadalTrench;
    }
    if feature == SeafloorFeature::Ridge {
        return Biome::HydrothermalVent;
    }
    if depth_m < 200.0 {
        if sst_c > 20.0 {
            return Biome::CoralReef;
        }
        if sst_c < 12.0 {
            return Biome::KelpForest;
        }
    }
    if upwelling && depth_m < 1000.0 {
        return Biome::Upwelling;
    }
    if depth_m < 200.0 {
        Biome::Epipelagic
    } else if depth_m < 1000.0 {
        Biome::Mesopelagic
    } else if depth_m < 4000.0 {
        Biome::Bathypelagic
    } else if depth_m < 6000.0 {
        Biome::Abyssal
    } else {
        Biome::HadalTrench
    }
}

/// Classify any cell: marine when below sea level (depth = sea_level − elev),
/// otherwise land. `sst_c` is the surface temperature used for marine cells.
#[allow(clippy::too_many_arguments)]
pub fn classify(
    temp_c: f64,
    moisture: f64,
    sst_c: f64,
    elevation_m: f64,
    sea_level_m: f64,
    latitude_deg: f64,
    feature: SeafloorFeature,
    upwelling: bool,
) -> Biome {
    if elevation_m < sea_level_m {
        classify_marine(sea_level_m - elevation_m, sst_c, feature, upwelling)
    } else {
        classify_land(temp_c, moisture, elevation_m, sea_level_m, latitude_deg)
    }
}
```

- [ ] **Step 4: Run + fmt**

Run: `cargo test -p hornvale-climate biome && cargo clippy -p hornvale-climate --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

- [ ] **Step 5: Commit**

```bash
git add domains/climate/src/biome.rs
git commit -m "feat(climate): marine biome classification + unified classify"
```

---

### Task 10: Climate — habitability classification

**Files:**
- Create: `domains/climate/src/habitability.rs`
- Modify: `domains/climate/src/lib.rs` (`pub mod habitability;`)

**Interfaces:**
- Consumes: `CellMap<f64>` (elevation, mean temperature, moisture), `f64` (sea level).
- Produces:
  - `fn is_habitable(temp_c: f64, moisture: f64, elevation_m: f64, sea_level_m: f64) -> bool` — land, temperate band (`-5 ≤ T ≤ 35`), liquid water (`moisture ≥ 0.2`).
  - `fn habitability_map(geo, elevation, mean_temp, moisture, sea_level) -> CellMap<bool>`.
  - `fn habitable_fraction(map: &CellMap<bool>) -> f64`.

- [ ] **Step 1: Write failing tests**

Create `domains/climate/src/habitability.rs`:

```rust
//! Habitability: the non-opinionated "where a vale-like place could be" —
//! land, with liquid water available, in a tolerable temperature band
//! (spec §6). It pre-wires the embark seam and yields the Lab's habitable-
//! fraction unknown number (spec §10).

use hornvale_kernel::{CellMap, Geosphere};

/// Coldest tolerable annual-mean temperature (°C).
const HABITABLE_MIN_C: f64 = -5.0;
/// Hottest tolerable annual-mean temperature (°C).
const HABITABLE_MAX_C: f64 = 35.0;
/// Aridity floor: below this moisture there is no reliable liquid water.
const HABITABLE_MIN_MOISTURE: f64 = 0.2;

/// Whether a cell could host a vale-like settlement.
pub fn is_habitable(temp_c: f64, moisture: f64, elevation_m: f64, sea_level_m: f64) -> bool {
    elevation_m >= sea_level_m
        && (HABITABLE_MIN_C..=HABITABLE_MAX_C).contains(&temp_c)
        && moisture >= HABITABLE_MIN_MOISTURE
}

/// The per-cell habitability mask.
pub fn habitability_map(
    geo: &Geosphere,
    elevation: &CellMap<f64>,
    mean_temp: &CellMap<f64>,
    moisture: &CellMap<f64>,
    sea_level: f64,
) -> CellMap<bool> {
    CellMap::from_fn(geo, |cell| {
        is_habitable(*mean_temp.get(cell), *moisture.get(cell), *elevation.get(cell), sea_level)
    })
}

/// The fraction of cells that are habitable.
pub fn habitable_fraction(map: &CellMap<bool>) -> f64 {
    if map.is_empty() {
        return 0.0;
    }
    let count = map.iter().filter(|(_, h)| **h).count();
    count as f64 / map.len() as f64
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    #[test]
    fn ocean_and_extremes_are_uninhabitable_temperate_land_is_habitable() {
        assert!(!is_habitable(20.0, 0.5, -100.0, 0.0)); // ocean
        assert!(!is_habitable(50.0, 0.5, 100.0, 0.0)); // too hot
        assert!(!is_habitable(15.0, 0.05, 100.0, 0.0)); // too dry
        assert!(is_habitable(15.0, 0.5, 100.0, 0.0));
    }

    #[test]
    fn fraction_is_between_zero_and_one() {
        let geo = Geosphere::new(3);
        let elev = CellMap::from_fn(&geo, |c| if geo.position(c)[2] > 0.0 { 200.0 } else { -200.0 });
        let temp = CellMap::from_fn(&geo, |_| 15.0);
        let moist = CellMap::from_fn(&geo, |_| 0.5);
        let map = habitability_map(&geo, &elev, &temp, &moist, 0.0);
        let f = habitable_fraction(&map);
        assert!((0.0..=1.0).contains(&f));
        assert!(f > 0.0, "some northern land should be habitable");
    }
}
```

- [ ] **Step 2: Run to see it pass structurally / fail if wired wrong**

Run: `cargo test -p hornvale-climate habitab 2>&1 | tail -6`
Expected: FAIL until the module is declared (compile error), then PASS.

- [ ] **Step 3: Wire the module**

In `domains/climate/src/lib.rs` add `pub mod habitability;` and re-export:

```rust
pub use habitability::{habitable_fraction, is_habitable};
```

- [ ] **Step 4: Run + fmt**

Run: `cargo test -p hornvale-climate habitab && cargo clippy -p hornvale-climate --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

- [ ] **Step 5: Commit**

```bash
git add domains/climate/src/habitability.rs domains/climate/src/lib.rs
git commit -m "feat(climate): habitability classification + habitable fraction"
```

---

### Task 11: Climate — the `GeneratedClimate` provider

**Files:**
- Create: `domains/climate/src/provider.rs`
- Modify: `domains/climate/src/lib.rs` (`pub mod provider;` + re-exports)

**Interfaces:**
- Consumes: everything above + `terrain`-free inputs.
- Produces:
  - `struct ClimateInputs<'a> { geosphere: &'a Geosphere, elevation: &'a CellMap<f64>, sea_level: f64, seafloor: &'a CellMap<SeafloorFeature>, insolation: f64, obliquity_deg: f64, regime: RotationRegime, year_length_std: f64 }`.
  - `struct GeneratedClimate { … }` (owns a `Geosphere` clone + derived `CellMap`s + scalars; **no serde**).
  - `impl GeneratedClimate`: `generate(&ClimateInputs) -> GeneratedClimate`; `geosphere() -> &Geosphere`; `band_count() -> Option<u32>`; `temperature_at(cell, day) -> f64`; `mean_temperature_at(cell) -> f64`; `moisture_at(cell) -> f64`; `biome_at(cell) -> Biome`; `biome_map() -> CellMap<Biome>`; `habitability() -> &CellMap<bool>`; `habitable_fraction() -> f64`; `regime() -> RotationRegime`.
  - `struct ClimateSummary { band_count: Option<u32>, habitable_fraction: f64, land_biome_count: usize, marine_biome_count: usize }` + `fn summarize(&GeneratedClimate) -> ClimateSummary`.
- Upwelling: derived per ocean cell = the cell is coastal (has a land neighbor) and the prevailing wind has an offshore component (wind · (toward-land) < 0). Computed in `generate` into a `CellMap<bool>`.
- SST (sea-surface temperature) for marine classification = the annual-mean temperature at the cell (surface proxy).

- [ ] **Step 1: Write failing tests**

Create `domains/climate/src/provider.rs`:

```rust
//! The tier-1 climate provider: temperature, moisture, and the derived biome
//! and habitability fields over the shared Geosphere. Consumes an elevation
//! `CellMap` and scalar stellar inputs — never a terrain or astronomy type —
//! so climate stays kernel-only (spec §3). Recomputed on demand, never
//! serialized.

use crate::biome::{self, Biome, SeafloorFeature};
use crate::circulation::{RotationRegime, band_count_for, prevailing_wind};
use crate::habitability;
use crate::moisture::moisture_field;
use crate::temperature::{mean_temperature, temperature_at};
use hornvale_kernel::{CellId, CellMap, Geosphere};

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    fn inputs<'a>(
        geo: &'a Geosphere,
        elev: &'a CellMap<f64>,
        sea: &'a CellMap<SeafloorFeature>,
        regime: RotationRegime,
    ) -> ClimateInputs<'a> {
        ClimateInputs {
            geosphere: geo,
            elevation: elev,
            sea_level: 0.0,
            seafloor: sea,
            insolation: 1.0,
            obliquity_deg: 23.5,
            regime,
            year_length_std: 365.25,
        }
    }

    #[test]
    fn provider_answers_every_query_and_is_deterministic() {
        let geo = Geosphere::new(4);
        let elev = CellMap::from_fn(&geo, |c| if geo.position(c)[2] > 0.0 { 300.0 } else { -1000.0 });
        let sea = CellMap::from_fn(&geo, |_| SeafloorFeature::None);
        let regime = RotationRegime::Spinning { day_std: 1.0 };
        let a = GeneratedClimate::generate(&inputs(&geo, &elev, &sea, regime));
        let b = GeneratedClimate::generate(&inputs(&geo, &elev, &sea, regime));
        assert_eq!(a.band_count(), Some(3));
        assert_eq!(a.biome_map(), b.biome_map());
        assert!((0.0..=1.0).contains(&a.habitable_fraction()));
        // biome_map and biome_at agree.
        for c in geo.cells().take(50) {
            assert_eq!(*a.biome_map().get(c), a.biome_at(c));
        }
    }

    #[test]
    fn locked_provider_has_no_band_count() {
        let geo = Geosphere::new(3);
        let elev = CellMap::from_fn(&geo, |_| 200.0);
        let sea = CellMap::from_fn(&geo, |_| SeafloorFeature::None);
        let c = GeneratedClimate::generate(&inputs(&geo, &elev, &sea, RotationRegime::Locked));
        assert_eq!(c.band_count(), None);
    }

    #[test]
    fn ocean_cells_map_to_marine_biomes() {
        let geo = Geosphere::new(4);
        let elev = CellMap::from_fn(&geo, |c| if geo.position(c)[2] > 0.0 { 300.0 } else { -3000.0 });
        let sea = CellMap::from_fn(&geo, |_| SeafloorFeature::None);
        let c = GeneratedClimate::generate(&inputs(&geo, &elev, &sea, RotationRegime::Spinning { day_std: 1.0 }));
        for cell in geo.cells() {
            let marine = *elev.get(cell) < 0.0;
            assert_eq!(c.biome_at(cell).is_marine(), marine, "cell {} biome/ocean mismatch", cell.0);
        }
    }
}
```

- [ ] **Step 2: Run to see failure**

Run: `cargo test -p hornvale-climate provider 2>&1 | tail -8`
Expected: FAIL — types undefined.

- [ ] **Step 3: Implement**

Insert before the tests module:

```rust
/// The inputs the composition root supplies to build a climate (all bare
/// kernel types or climate-owned types — no terrain/astronomy imports).
pub struct ClimateInputs<'a> {
    /// The shared globe mesh.
    pub geosphere: &'a Geosphere,
    /// Elevation per cell, meters.
    pub elevation: &'a CellMap<f64>,
    /// Sea level, meters.
    pub sea_level: f64,
    /// Seafloor tectonic features per cell (mapped from terrain boundaries).
    pub seafloor: &'a CellMap<SeafloorFeature>,
    /// Stellar insolation relative to Earth (L / d², solar units / AU²).
    pub insolation: f64,
    /// Axial tilt, degrees.
    pub obliquity_deg: f64,
    /// The rotation regime.
    pub regime: RotationRegime,
    /// Year length in standard days (for time-varying temperature).
    pub year_length_std: f64,
}

/// The tier-1 climate: derived temperature/moisture/biome/habitability over
/// the globe. Owns its mesh so every query and CellMap agree on the cell
/// space. Recomputed on demand, never serialized.
#[derive(Debug, Clone, PartialEq)]
pub struct GeneratedClimate {
    geosphere: Geosphere,
    elevation: CellMap<f64>,
    sea_level: f64,
    mean_temp: CellMap<f64>,
    moisture: CellMap<f64>,
    biome: CellMap<Biome>,
    habitability: CellMap<bool>,
    band_count: Option<u32>,
    obliquity_deg: f64,
    year_length_std: f64,
    regime: RotationRegime,
}

/// Whether an ocean cell has a coastal upwelling: it borders land and the
/// prevailing wind carries surface water offshore (wind points away from the
/// mean direction to its land neighbors).
fn is_upwelling(
    geo: &Geosphere,
    elevation: &CellMap<f64>,
    sea_level: f64,
    cell: CellId,
    bands: Option<u32>,
) -> bool {
    if *elevation.get(cell) >= sea_level {
        return false;
    }
    let Some(bands) = bands else { return false };
    let p = geo.position(cell);
    // Mean direction toward land neighbors.
    let mut toward = [0.0, 0.0, 0.0];
    let mut land = 0;
    for &n in geo.neighbors(cell) {
        if *elevation.get(n) >= sea_level {
            let np = geo.position(n);
            toward = [toward[0] + np[0] - p[0], toward[1] + np[1] - p[1], toward[2] + np[2] - p[2]];
            land += 1;
        }
    }
    if land == 0 {
        return false;
    }
    let wind = prevailing_wind(geo, cell, bands);
    // Offshore wind: wind opposes the toward-land direction.
    wind[0] * toward[0] + wind[1] * toward[1] + wind[2] * toward[2] < 0.0
}

impl GeneratedClimate {
    /// Derive the full climate from inputs.
    pub fn generate(inputs: &ClimateInputs) -> GeneratedClimate {
        let geo = inputs.geosphere;
        let band_count = band_count_for(&inputs.regime);
        let mean_temp = mean_temperature(geo, inputs.elevation, inputs.sea_level, inputs.insolation, &inputs.regime);
        let moisture = moisture_field(geo, inputs.elevation, inputs.sea_level, &inputs.regime);
        let biome = CellMap::from_fn(geo, |cell| {
            let temp = *mean_temp.get(cell);
            let upwell = is_upwelling(geo, inputs.elevation, inputs.sea_level, cell, band_count);
            biome::classify(
                temp,
                *moisture.get(cell),
                temp, // SST proxy = surface annual-mean temperature
                *inputs.elevation.get(cell),
                inputs.sea_level,
                geo.coord(cell).latitude,
                *inputs.seafloor.get(cell),
                upwell,
            )
        });
        let habitability = habitability::habitability_map(geo, inputs.elevation, &mean_temp, &moisture, inputs.sea_level);
        GeneratedClimate {
            geosphere: geo.clone(),
            elevation: inputs.elevation.clone(),
            sea_level: inputs.sea_level,
            mean_temp,
            moisture,
            biome,
            habitability,
            band_count,
            obliquity_deg: inputs.obliquity_deg,
            year_length_std: inputs.year_length_std,
            regime: inputs.regime,
        }
    }

    /// The mesh this climate is computed over.
    pub fn geosphere(&self) -> &Geosphere {
        &self.geosphere
    }
    /// Circulation bands per hemisphere; `None` when tidally locked.
    pub fn band_count(&self) -> Option<u32> {
        self.band_count
    }
    /// The rotation regime.
    pub fn regime(&self) -> RotationRegime {
        self.regime
    }
    /// Annual-mean temperature at a cell, °C.
    pub fn mean_temperature_at(&self, cell: CellId) -> f64 {
        *self.mean_temp.get(cell)
    }
    /// Temperature at a cell on a given day, °C (mean plus the seasonal term).
    pub fn temperature_at(&self, cell: CellId, day: f64) -> f64 {
        temperature_at(
            &self.mean_temp,
            &self.geosphere,
            &self.elevation,
            self.sea_level,
            self.obliquity_deg,
            self.year_length_std,
            &self.regime,
            cell,
            day,
        )
    }
    /// Moisture at a cell, `[0, 1]`.
    pub fn moisture_at(&self, cell: CellId) -> f64 {
        *self.moisture.get(cell)
    }
    /// The biome at a cell.
    pub fn biome_at(&self, cell: CellId) -> Biome {
        *self.biome.get(cell)
    }
    /// The full biome field (a clone of the derived map).
    pub fn biome_map(&self) -> CellMap<Biome> {
        self.biome.clone()
    }
    /// The per-cell habitability mask.
    pub fn habitability(&self) -> &CellMap<bool> {
        &self.habitability
    }
    /// The fraction of cells that are habitable.
    pub fn habitable_fraction(&self) -> f64 {
        habitability::habitable_fraction(&self.habitability)
    }
}

/// Headline numbers of a climate, for the almanac and the Lab.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ClimateSummary {
    /// Circulation bands per hemisphere (`None` when locked).
    pub band_count: Option<u32>,
    /// Fraction of cells that are habitable.
    pub habitable_fraction: f64,
    /// Distinct land biomes present.
    pub land_biome_count: usize,
    /// Distinct marine biomes present.
    pub marine_biome_count: usize,
}

/// Summarize a climate's headline numbers (deterministic; ascending order).
pub fn summarize(climate: &GeneratedClimate) -> ClimateSummary {
    let mut land: Vec<&'static str> = Vec::new();
    let mut marine: Vec<&'static str> = Vec::new();
    for (_, b) in climate.biome.iter() {
        let list = if b.is_marine() { &mut marine } else { &mut land };
        if !list.contains(&b.name()) {
            list.push(b.name());
        }
    }
    ClimateSummary {
        band_count: climate.band_count,
        habitable_fraction: climate.habitable_fraction(),
        land_biome_count: land.len(),
        marine_biome_count: marine.len(),
    }
}
```

- [ ] **Step 4: Wire the module + re-exports**

In `domains/climate/src/lib.rs`:

```rust
pub mod provider;

pub use provider::{ClimateInputs, ClimateSummary, GeneratedClimate, summarize};
```

- [ ] **Step 5: Run climate gate**

Run: `cargo test -p hornvale-climate && cargo clippy -p hornvale-climate --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

- [ ] **Step 6: Commit**

```bash
git add domains/climate/src/provider.rs domains/climate/src/lib.rs
git commit -m "feat(climate): GeneratedClimate provider (biome + habitability fields)"
```

---

### Task 12: Climate — the biome-map renderer

**Files:**
- Modify: `domains/climate/src/biome.rs` (add `glyph`/`color` accessors if not already present)
- Create: `domains/climate/src/render.rs`
- Modify: `domains/climate/src/lib.rs` (`pub mod render;`)

**Interfaces:**
- Consumes: `Geosphere`, `CellMap<Biome>`, `Biome::{glyph, color}`.
- Produces:
  - `Biome::color(self) -> [u8; 3]`, `Biome::glyph(self) -> char`.
  - `fn biome_ppm(geo: &Geosphere, biomes: &CellMap<Biome>) -> Vec<u8>` — equirectangular P6 PPM, same projection as the elevation renderer (`MAP_WIDTH = 256`, height = 128).
  - `fn biome_ascii(geo: &Geosphere, biomes: &CellMap<Biome>) -> String` — 72×24 glyph map.

The nearest-cell projection reuses the same latitude-band index approach as `terrain::render` (30 bands of 6°); it is small and self-contained, so re-implement it here rather than exporting terrain's private helper (climate must not import terrain).

- [ ] **Step 1: Add `glyph`/`color` to `Biome`**

In `domains/climate/src/biome.rs`, add to `impl Biome`:

```rust
    /// A single ASCII glyph for the REPL biome map.
    pub fn glyph(self) -> char {
        match self {
            Biome::Ice | Biome::SeaIce => '*',
            Biome::Tundra => ',',
            Biome::Taiga => 't',
            Biome::TemperateGrassland => '"',
            Biome::Shrubland => ';',
            Biome::TemperateForest => 'f',
            Biome::TemperateRainforest => 'F',
            Biome::Desert => '.',
            Biome::Savanna => ':',
            Biome::TropicalSeasonalForest => 'w',
            Biome::TropicalRainforest => 'W',
            Biome::Alpine => '^',
            Biome::CoralReef => 'o',
            Biome::KelpForest => 'k',
            Biome::HydrothermalVent => 'v',
            Biome::HadalTrench => '#',
            Biome::Upwelling => '=',
            Biome::Epipelagic => '~',
            Biome::Mesopelagic => '-',
            Biome::Bathypelagic => '_',
            Biome::Abyssal => ' ',
        }
    }

    /// An RGB color for the PPM biome map.
    pub fn color(self) -> [u8; 3] {
        match self {
            Biome::Ice => [235, 235, 245],
            Biome::Tundra => [170, 175, 155],
            Biome::Taiga => [70, 105, 80],
            Biome::TemperateGrassland => [160, 180, 100],
            Biome::Shrubland => [155, 150, 95],
            Biome::TemperateForest => [60, 130, 70],
            Biome::TemperateRainforest => [35, 100, 60],
            Biome::Desert => [210, 195, 130],
            Biome::Savanna => [180, 165, 85],
            Biome::TropicalSeasonalForest => [90, 150, 65],
            Biome::TropicalRainforest => [25, 110, 45],
            Biome::Alpine => [150, 140, 135],
            Biome::SeaIce => [220, 230, 240],
            Biome::CoralReef => [230, 150, 160],
            Biome::KelpForest => [40, 90, 95],
            Biome::HydrothermalVent => [120, 60, 90],
            Biome::HadalTrench => [10, 15, 45],
            Biome::Upwelling => [60, 160, 170],
            Biome::Epipelagic => [70, 140, 200],
            Biome::Mesopelagic => [45, 95, 160],
            Biome::Bathypelagic => [25, 55, 110],
            Biome::Abyssal => [12, 30, 70],
        }
    }
```

Add a test to the biome tests module:

```rust
#[test]
fn every_biome_has_a_distinct_enough_glyph_and_a_color() {
    for b in [Biome::Desert, Biome::TropicalRainforest, Biome::Abyssal, Biome::CoralReef] {
        let _ = b.glyph();
        let _ = b.color();
    }
}
```

- [ ] **Step 2: Write the renderer tests**

Create `domains/climate/src/render.rs`:

```rust
//! Deterministic biome-map renders: an equirectangular P6 PPM and a 72×24
//! ASCII map, recolored from the elevation-map tradition. Same biome field,
//! same bytes — a changed artifact in review means changed behavior.
//! Pixel→cell lookup uses a latitude-band index (30 bands of 6°), the same
//! projection the elevation renderer uses.

use crate::biome::Biome;
use hornvale_kernel::{CellId, CellMap, Geosphere};

/// PPM image width in pixels; equirectangular, so height is `MAP_WIDTH / 2`.
pub const MAP_WIDTH: u32 = 256;
/// ASCII map width in characters.
pub const ASCII_WIDTH: u32 = 72;
/// ASCII map height in characters.
pub const ASCII_HEIGHT: u32 = 24;

/// Latitude bands in the nearest-cell index.
const BAND_COUNT: usize = 30;
/// Height of one band, degrees.
const BAND_DEGREES: f64 = 180.0 / BAND_COUNT as f64;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::biome::Biome;
    use hornvale_kernel::Geosphere;

    fn checker(geo: &Geosphere) -> CellMap<Biome> {
        CellMap::from_fn(geo, |c| {
            if geo.position(c)[2] > 0.0 { Biome::TropicalRainforest } else { Biome::Abyssal }
        })
    }

    #[test]
    fn ppm_is_well_formed_and_byte_deterministic() {
        let geo = Geosphere::new(4);
        let biomes = checker(&geo);
        let a = biome_ppm(&geo, &biomes);
        let b = biome_ppm(&geo, &biomes);
        assert_eq!(a, b);
        let header = format!("P6\n{} {}\n255\n", MAP_WIDTH, MAP_WIDTH / 2);
        assert!(a.starts_with(header.as_bytes()));
        assert_eq!(a.len(), header.len() + (MAP_WIDTH * (MAP_WIDTH / 2) * 3) as usize);
    }

    #[test]
    fn ascii_map_has_right_shape_and_shows_land_and_sea() {
        let geo = Geosphere::new(4);
        let biomes = checker(&geo);
        let map = biome_ascii(&geo, &biomes);
        assert_eq!(map.lines().count(), ASCII_HEIGHT as usize);
        for line in map.lines() {
            assert_eq!(line.chars().count(), ASCII_WIDTH as usize);
        }
        assert!(map.contains(Biome::TropicalRainforest.glyph()));
        assert_eq!(biome_ascii(&geo, &biomes), map);
    }
}
```

- [ ] **Step 3: Run to see failure**

Run: `cargo test -p hornvale-climate render 2>&1 | tail -6`
Expected: FAIL — `biome_ppm` undefined.

- [ ] **Step 4: Implement the index + renderers**

Insert before the tests module in `render.rs`:

```rust
/// Dot product.
fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// Latitude-band index for pixel→cell lookups (30 bands of 6°, built in
/// ascending cell order for determinism). Searching the query's band ± 1
/// always contains the nearest cell center at level ≥ 4.
struct LatBandIndex {
    bands: Vec<Vec<CellId>>,
}

impl LatBandIndex {
    fn new(geo: &Geosphere) -> LatBandIndex {
        let mut bands = vec![Vec::new(); BAND_COUNT];
        for cell in geo.cells() {
            let latitude = geo.coord(cell).latitude;
            let band = (((90.0 - latitude) / BAND_DEGREES) as usize).min(BAND_COUNT - 1);
            bands[band].push(cell);
        }
        LatBandIndex { bands }
    }

    /// The cell nearest a coordinate (degrees), by maximum dot product.
    fn nearest(&self, geo: &Geosphere, latitude: f64, longitude: f64) -> CellId {
        let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
        let target = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
        let band = (((90.0 - latitude) / BAND_DEGREES) as usize).min(BAND_COUNT - 1);
        let lo = band.saturating_sub(1);
        let hi = (band + 1).min(BAND_COUNT - 1);
        let mut best = CellId(0);
        let mut best_dot = f64::NEG_INFINITY;
        for cells in &self.bands[lo..=hi] {
            for &cell in cells {
                let d = dot(geo.position(cell), target);
                if d > best_dot {
                    best_dot = d;
                    best = cell;
                }
            }
        }
        best
    }
}

/// Render the biome field as an equirectangular binary P6 PPM. Same field,
/// same bytes.
pub fn biome_ppm(geo: &Geosphere, biomes: &CellMap<Biome>) -> Vec<u8> {
    let (width, height) = (MAP_WIDTH, MAP_WIDTH / 2);
    let index = LatBandIndex::new(geo);
    let mut out = format!("P6\n{width} {height}\n255\n").into_bytes();
    for py in 0..height {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
        for px in 0..width {
            let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
            let cell = index.nearest(geo, latitude, longitude);
            out.extend_from_slice(&biomes.get(cell).color());
        }
    }
    out
}

/// Render the biome field as a 72×24 ASCII map, one newline per row.
pub fn biome_ascii(geo: &Geosphere, biomes: &CellMap<Biome>) -> String {
    let index = LatBandIndex::new(geo);
    let mut out = String::with_capacity(((ASCII_WIDTH + 1) * ASCII_HEIGHT) as usize);
    for py in 0..ASCII_HEIGHT {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(ASCII_HEIGHT) * 180.0;
        for px in 0..ASCII_WIDTH {
            let longitude = (f64::from(px) + 0.5) / f64::from(ASCII_WIDTH) * 360.0 - 180.0;
            let cell = index.nearest(geo, latitude, longitude);
            out.push(biomes.get(cell).glyph());
        }
        out.push('\n');
    }
    out
}
```

- [ ] **Step 5: Wire the module**

In `domains/climate/src/lib.rs` add `pub mod render;`.

- [ ] **Step 6: Run climate gate**

Run: `cargo test -p hornvale-climate && cargo clippy -p hornvale-climate --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

- [ ] **Step 7: Commit**

```bash
git add domains/climate/src/biome.rs domains/climate/src/render.rs domains/climate/src/lib.rs
git commit -m "feat(climate): deterministic biome-map renderer (PPM + ASCII)"
```

---

### Task 13: Composition root — build and expose the climate

**Files:**
- Modify: `windows/worldgen/src/lib.rs`

**Interfaces:**
- Consumes: `terrain_of`, `sky_of`, `Sky::calendar`, terrain's `boundary_at`, astronomy's `Star`/`Anchor`/`HabitableZone`/`Rotation`.
- Produces:
  - `fn climate_of(world: &World) -> Result<hornvale_climate::GeneratedClimate, BuildError>` — the single construction site for the tier-1 climate (the `terrain_of`/`sky_of` pattern).
  - `fn biome_lines(world: &World) -> Result<Vec<String>, BuildError>` — headline biome/habitability lines for the almanac's Land section.
  - `AlmanacContext.biome_lines` field, populated in `almanac_context`.

Mapping (documented in the function): `astronomy::Rotation::Spinning{day}` → `RotationRegime::Spinning{day_std: day.get()}`, `Locked` → `Locked`; `terrain::BoundaryKind::IslandArc` → `SeafloorFeature::Trench`, `OceanicRidge` → `Ridge`, else `None`; insolation = `luminosity / orbit²`; constant-sky defaults per cross-cutting decision 4.

- [ ] **Step 1: Write failing tests**

Add to the `tests` module of `windows/worldgen/src/lib.rs`:

```rust
#[test]
fn climate_reconstructs_deterministically_and_maps_biomes() {
    let world = generated(42);
    let a = climate_of(&world).unwrap();
    let b = climate_of(&world).unwrap();
    assert_eq!(a.biome_map(), b.biome_map());
    assert_eq!(a.geosphere().level(), hornvale_terrain::GLOBE_LEVEL);
    // A generated spinning world has a band count; ocean cells are marine.
    assert!(a.band_count().is_some() || matches!(a.regime(), hornvale_climate::RotationRegime::Locked));
}

#[test]
fn locked_and_spinning_biome_maps_reorganize_from_the_same_land() {
    use hornvale_astronomy::RotationPin;
    let spinning = build_world(Seed(42), &SkyPins::default(), SkyChoice::Generated, &hornvale_terrain::TerrainPins::default()).unwrap();
    let locked = build_world(
        Seed(42),
        &SkyPins { rotation: Some(RotationPin::Locked), ..SkyPins::default() },
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
    ).unwrap();
    // Same land seed, different sky → different biome map.
    assert_ne!(climate_of(&spinning).unwrap().biome_map(), climate_of(&locked).unwrap().biome_map());
}

#[test]
fn biome_lines_describe_the_globe() {
    let lines = biome_lines(&generated(42)).unwrap();
    assert!(!lines.is_empty());
    assert!(lines.iter().any(|l| l.contains("habitable")));
}

#[test]
fn constant_sky_world_still_has_a_climate() {
    let world = constant(42);
    let climate = climate_of(&world).unwrap();
    assert!(climate.geosphere().cell_count() > 0);
}
```

- [ ] **Step 2: Run to see failure**

Run: `cargo test -p hornvale-worldgen climate_reconstructs 2>&1 | tail -6`
Expected: FAIL — `climate_of` undefined.

- [ ] **Step 3: Implement the mapping + `climate_of`**

In `windows/worldgen/src/lib.rs`, extend imports:

```rust
use hornvale_climate::{
    ClimateInputs, ClimateReport, GeneratedClimate, RotationRegime, SeafloorFeature, UniformClimate,
};
```

Add after `terrain_of` (and its helpers):

```rust
/// Map a terrain boundary contact to the seafloor feature climate consumes
/// (only ocean cells use it): ocean–ocean convergent arcs become trenches;
/// oceanic ridges become vent-bearing ridges; everything else is featureless.
fn seafloor_feature(boundary: Option<hornvale_terrain::CellBoundary>) -> SeafloorFeature {
    use hornvale_terrain::BoundaryKind;
    match boundary.map(|b| b.kind) {
        Some(BoundaryKind::IslandArc) => SeafloorFeature::Trench,
        Some(BoundaryKind::OceanicRidge) => SeafloorFeature::Ridge,
        _ => SeafloorFeature::None,
    }
}

/// The scalar stellar inputs climate needs, derived from this world's sky.
/// Constant-sky worlds get an Earth baseline so the biome map exists for
/// every world (spec: the coarse globe is generated for all).
fn stellar_inputs(sky: &Sky) -> (f64, f64, RotationRegime, f64) {
    match sky {
        Sky::Constant(_) => (1.0, 23.5, RotationRegime::Spinning { day_std: 1.0 }, 365.25),
        Sky::Generated(gen) => {
            let system = gen.system();
            let luminosity = system.star.luminosity.get();
            let orbit = system.anchor.orbit.get();
            // Insolation relative to Earth (L=1, d=1): L / d².
            let insolation = luminosity / (orbit * orbit);
            let obliquity = system.anchor.obliquity.get();
            let regime = match system.anchor.rotation {
                hornvale_astronomy::Rotation::Spinning { day } => {
                    RotationRegime::Spinning { day_std: day.get() }
                }
                hornvale_astronomy::Rotation::Locked => RotationRegime::Locked,
            };
            let year = gen.calendar().year_length().get();
            (insolation, obliquity, regime, year)
        }
    }
}

/// Reconstruct the tier-1 climate for this world: rebuild the terrain globe
/// and the sky, map their outputs into climate's kernel-only inputs, and
/// derive temperature/moisture/biome/habitability. The single construction
/// site for `GeneratedClimate` (the `terrain_of`/`sky_of` pattern).
pub fn climate_of(world: &World) -> Result<GeneratedClimate, BuildError> {
    let terrain = terrain_of(world)?;
    let sky = sky_of(world)?;
    let geo = terrain.geosphere();
    let elevation = &terrain.globe().elevation;
    let seafloor = hornvale_kernel::CellMap::from_fn(geo, |cell| {
        seafloor_feature(terrain.boundary_at(cell))
    });
    let (insolation, obliquity_deg, regime, year_length_std) = stellar_inputs(&sky);
    Ok(GeneratedClimate::generate(&ClimateInputs {
        geosphere: geo,
        elevation,
        sea_level: terrain.sea_level(),
        seafloor: &seafloor,
        insolation,
        obliquity_deg,
        regime,
        year_length_std,
    }))
}

/// Headline biome/habitability lines for the almanac's Land section.
pub fn biome_lines(world: &World) -> Result<Vec<String>, BuildError> {
    let climate = climate_of(world)?;
    let summary = hornvale_climate::summarize(&climate);
    let bands = match summary.band_count {
        Some(n) => format!("{n} circulation band(s) per hemisphere"),
        None => "a single day–night overturning (tidally locked)".to_string(),
    };
    Ok(vec![
        format!(
            "The air organizes into {bands}; {} land biomes and {} marine biomes cover the globe.",
            summary.land_biome_count, summary.marine_biome_count
        ),
        format!(
            "Some {:.0}% of the surface is habitable — land with water and a tolerable season.",
            summary.habitable_fraction * 100.0
        ),
    ])
}
```

Note: `terrain.globe().elevation` is a `CellMap<f64>` field — pass `&terrain.globe().elevation`. `CellMap` is re-exported from the kernel; use `hornvale_kernel::CellMap`.

Ensure `hornvale_terrain::{BoundaryKind, CellBoundary}` are reachable (they are re-exported from the terrain crate root — confirm and adjust the `use hornvale_terrain::{...}` line at the top to include them, or reference them fully-qualified as above).

- [ ] **Step 4: Add the almanac field and populate it**

In `windows/almanac/src/lib.rs`, add to `AlmanacContext`:

```rust
    /// The globe's biome/habitability headline lines, from the composition root.
    pub biome_lines: Vec<String>,
```

Render it in the Land section of `render` (after the existing `land_lines` block, before the places list):

```rust
    for line in &ctx.biome_lines {
        doc.push_str(&format!("{line}\n"));
    }
    if !ctx.biome_lines.is_empty() {
        doc.push('\n');
    }
```

Update the almanac's `sample_context()` test helper to include `biome_lines: vec![]` (or a sample line) and add an assertion in `render_contains_every_section_and_datum` if you supply a sample line.

In `windows/worldgen/src/lib.rs`, populate the field in `almanac_context`:

```rust
        biome_lines: biome_lines(world)?,
```

- [ ] **Step 5: Run the workspace gate**

Run: `cargo test -p hornvale-worldgen -p hornvale-almanac && cargo clippy --workspace --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

- [ ] **Step 6: Commit**

```bash
git add windows/worldgen/src/lib.rs windows/almanac/src/lib.rs
git commit -m "feat(worldgen): climate_of composition root + almanac biome lines"
```

---

### Task 14: CLI + REPL — biome queries and the biome-map command

**Files:**
- Modify: `cli/src/main.rs`
- Modify: `cli/src/repl.rs`

**Interfaces:**
- Consumes: `world_builder::climate_of`, `hornvale_climate::render`.
- Produces:
  - `hornvale biome-map [--world PATH] [--out PPM]` — a markdown page (title, biome/land lines, ASCII biome map) to stdout; with `--out`, the PPM to disk (deterministic, drift-checked).
  - REPL `biomes` (ASCII biome map) and `biome <lat> <lon>` (per-cell biome + temperature + moisture).

- [ ] **Step 1: Write the failing REPL tests**

Add to `cli/src/repl.rs` tests:

```rust
#[test]
fn biomes_and_biome_answer_queries() {
    let world = build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
    )
    .unwrap();
    let mut out = Vec::new();
    run(&world, "biomes\nbiome 5 -40\nquit\n".as_bytes(), &mut out).unwrap();
    let out = String::from_utf8(out).unwrap();
    assert!(out.lines().count() > 24, "no ascii biome map");
    assert!(out.contains("biome"), "no per-cell biome report");
}
```

- [ ] **Step 2: Run to see it fail**

Run: `cargo test -p hornvale biomes_and_biome 2>&1 | tail -6`
Expected: FAIL — commands unrecognized (output contains "unknown command").

- [ ] **Step 3: Implement the REPL commands**

In `cli/src/repl.rs`, extend the `HELP` string with two lines:

```rust
  biomes           biome map of the globe
  biome <lat> <lon> the biome at a coordinate (degrees)
```

Add match arms (after the `land` arm):

```rust
            "biomes" => match world_builder::climate_of(world) {
                Ok(climate) => write!(
                    output,
                    "{}",
                    hornvale_climate::render::biome_ascii(climate.geosphere(), &climate.biome_map())
                )?,
                Err(e) => writeln!(output, "error: {e}")?,
            },
            "biome" => {
                let coords = argument
                    .and_then(|lat| Some((lat, parts.next()?)))
                    .and_then(|(lat, lon)| Some((lat.parse::<f64>().ok()?, lon.parse::<f64>().ok()?)));
                match coords {
                    None => writeln!(output, "usage: biome <latitude> <longitude>")?,
                    Some((lat, lon)) => match (world_builder::terrain_of(world), world_builder::climate_of(world)) {
                        (Ok(terrain), Ok(climate)) => {
                            let cell = terrain.nearest_cell(lat, lon);
                            writeln!(
                                output,
                                "cell {}: {} — {:.0}°C, moisture {:.2}",
                                cell.0,
                                climate.biome_at(cell).name(),
                                climate.mean_temperature_at(cell),
                                climate.moisture_at(cell)
                            )?;
                        }
                        (Err(e), _) | (_, Err(e)) => writeln!(output, "error: {e}")?,
                    },
                }
            }
```

(`Biome::name` is already public from Task 8.)

- [ ] **Step 4: Write the failing CLI test + implement `biome-map`**

Add to `cli/src/main.rs` tests:

```rust
#[test]
fn usage_mentions_biome_map() {
    assert!(USAGE.contains("biome-map"));
}
```

In `cli/src/main.rs`: add `Some("biome-map") => cmd_biome_map(&args),` to the `main` dispatch, add a usage line to `USAGE`:

```
  hornvale biome-map [--world <PATH>] [--out <PPM>] render the biome map (markdown to stdout)
```

and implement (mirroring `cmd_map`):

```rust
/// Render the world's biome map: a markdown page (title, biome/land lines,
/// ASCII biome map) to stdout and, with `--out`, the PPM image to disk. Both
/// are deterministic; CI drift-checks the committed copies.
fn cmd_biome_map(args: &[String]) -> Result<(), String> {
    let world = load_world(args)?;
    let climate = world_builder::climate_of(&world).map_err(|e| e.to_string())?;
    let mut doc = format!("# The Biomes of Seed {}\n\n", world.seed.0);
    for line in world_builder::biome_lines(&world).map_err(|e| e.to_string())? {
        doc.push_str(&format!("{line}\n"));
    }
    doc.push_str("\n```text\n");
    doc.push_str(&hornvale_climate::render::biome_ascii(
        climate.geosphere(),
        &climate.biome_map(),
    ));
    doc.push_str("```\n\n");
    if let Some(out) = flag_value(args, "--out") {
        let ppm = hornvale_climate::render::biome_ppm(climate.geosphere(), &climate.biome_map());
        std::fs::write(out, ppm).map_err(|e| format!("writing {out}: {e}"))?;
        let name = std::path::Path::new(out)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or(out);
        doc.push_str(&format!("Full-color render: [`{name}`](./{name})\n\n"));
    }
    doc.push_str("---\n\n*Generated deterministically: this seed always yields this page.*\n");
    print!("{doc}");
    Ok(())
}
```

- [ ] **Step 5: Run the CLI gate**

Run: `cargo test -p hornvale && cargo clippy -p hornvale --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

- [ ] **Step 6: Commit**

```bash
git add cli/src/main.rs cli/src/repl.rs
git commit -m "feat(cli): biome-map command + REPL biome/biomes queries"
```

---

### Task 15: Lab — the Census of Lands metrics

**Files:**
- Modify: `windows/lab/Cargo.toml`
- Modify: `windows/lab/src/metrics.rs`

**Interfaces:**
- Consumes: `world_builder::{terrain_of, climate_of}`, `hornvale_terrain::summarize`, `hornvale_climate::summarize`.
- Produces: `WorldView` gains `globe: GlobeSummary`-ish data and a `climate: GeneratedClimate`; the metric `registry()` gains 7 land metrics (total 21). The runner and study format are unchanged (spec §10 "the runner never changes").

New metrics (all read the reconstructed terrain/climate held on the `WorldView`):
- `plate-count` (Categorical) — plate count as text.
- `ocean-fraction` (Numeric, edges `[0.3,0.4,0.5,0.6,0.7,0.8,0.9]`) — achieved ocean fraction.
- `mountain-coverage` (Numeric, edges `[0.0,0.02,0.05,0.1,0.2,0.3]`) — fraction of land cells above 2000 m.
- `band-count` (Categorical) — bands per hemisphere as text, `"locked"` when tidally locked.
- `habitable-fraction` (Numeric, edges `[0.0,0.05,0.1,0.2,0.3,0.4,0.5]`) — the unknown number.
- `unrest-coverage` (Numeric, edges `[0.0,0.05,0.1,0.2,0.3,0.5]`) — fraction of cells with unrest > 0.3.
- `dominant-land-biome` (Categorical) — the most common land biome's kebab name (spec's "biome distribution", summarized to its mode for a single categorical column).

- [ ] **Step 1: Add the dependency**

In `windows/lab/Cargo.toml`, add under `[dependencies]`:

```toml
hornvale-climate = { path = "../../domains/climate" }
```

- [ ] **Step 2: Write failing tests**

Add to `windows/lab/src/metrics.rs` tests:

```rust
#[test]
fn registry_has_twenty_one_metrics_after_lands() {
    assert_eq!(registry().len(), 21);
}

#[test]
fn land_metrics_extract_for_seed_42() {
    let view = WorldView::build(Seed(42), &SkyPins::default()).unwrap();
    let reg = registry();
    let m = |name: &str| (reg.iter().find(|m| m.name == name).unwrap().extract)(&view);
    assert!(matches!(m("plate-count"), MetricValue::Text(_)));
    assert!(matches!(m("ocean-fraction"), MetricValue::Number(f) if (0.0..=1.0).contains(&f)));
    assert!(matches!(m("habitable-fraction"), MetricValue::Number(f) if (0.0..=1.0).contains(&f)));
    assert!(matches!(m("band-count"), MetricValue::Text(_)));
    assert!(matches!(m("dominant-land-biome"), MetricValue::Text(_)));
}

#[test]
fn locked_world_band_count_metric_is_locked() {
    let pins = SkyPins {
        rotation: Some(hornvale_astronomy::pins::RotationPin::Locked),
        ..SkyPins::default()
    };
    let view = WorldView::build(Seed(42), &pins).unwrap();
    let reg = registry();
    let bc = (reg.iter().find(|m| m.name == "band-count").unwrap().extract)(&view);
    assert_eq!(bc, MetricValue::Text("locked".to_string()));
}
```

- [ ] **Step 3: Run to see failure**

Run: `cargo test -p hornvale-lab registry_has_twenty_one 2>&1 | tail -6`
Expected: FAIL — count is 14 / fields absent.

- [ ] **Step 4: Extend `WorldView`**

In `windows/lab/src/metrics.rs`, add imports and fields:

```rust
use hornvale_climate::GeneratedClimate;
use hornvale_terrain::GlobeSummary;
use hornvale_worldgen::{climate_of, terrain_of};
```

Add to `WorldView`:

```rust
    /// The tectonic globe summary (plates, ocean fraction, sea level, peak).
    pub globe: GlobeSummary,
    /// The full tectonic globe (for coverage metrics over cells).
    pub terrain: hornvale_terrain::GeneratedTerrain,
    /// The derived climate (biome + habitability).
    pub climate: GeneratedClimate,
```

In `WorldView::build`, after building `world` and before `Ok(WorldView { ... })`:

```rust
        let terrain = terrain_of(&world)?;
        let globe = hornvale_terrain::summarize(terrain.globe());
        let climate = climate_of(&world)?;
```

and add `globe, terrain, climate` to the returned struct.

- [ ] **Step 5: Add the 7 metrics to `registry()`**

Append these `Metric` entries to the `vec![...]` in `registry()`:

```rust
        Metric {
            name: "plate-count",
            doc: "Number of tectonic plates the globe drew or was pinned to",
            summary: SummaryKind::Categorical,
            extract: |v| MetricValue::Text(v.globe.plate_count.to_string()),
        },
        Metric {
            name: "ocean-fraction",
            doc: "Fraction of globe cells below sea level",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9],
            },
            extract: |v| MetricValue::Number(v.globe.ocean_fraction),
        },
        Metric {
            name: "mountain-coverage",
            doc: "Fraction of land cells standing above 2000 m over the sea",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.02, 0.05, 0.1, 0.2, 0.3],
            },
            extract: |v| {
                let geo = v.terrain.geosphere();
                let sea = v.terrain.sea_level();
                let (mut land, mut high) = (0usize, 0usize);
                for cell in geo.cells() {
                    let e = v.terrain.elevation_at(cell);
                    if e >= sea {
                        land += 1;
                        if e - sea > 2000.0 {
                            high += 1;
                        }
                    }
                }
                MetricValue::Number(if land == 0 { 0.0 } else { high as f64 / land as f64 })
            },
        },
        Metric {
            name: "band-count",
            doc: "Circulation bands per hemisphere; 'locked' if tidally locked",
            summary: SummaryKind::Categorical,
            extract: |v| match v.climate.band_count() {
                Some(n) => MetricValue::Text(n.to_string()),
                None => MetricValue::Text("locked".to_string()),
            },
        },
        Metric {
            name: "habitable-fraction",
            doc: "Fraction of cells that are habitable (land, water, tolerable season)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5],
            },
            extract: |v| MetricValue::Number(v.climate.habitable_fraction()),
        },
        Metric {
            name: "unrest-coverage",
            doc: "Fraction of cells with tectonic unrest above 0.3",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.05, 0.1, 0.2, 0.3, 0.5],
            },
            extract: |v| {
                let geo = v.terrain.geosphere();
                let total = geo.cell_count();
                let restless = geo.cells().filter(|c| v.terrain.unrest_at(*c) > 0.3).count();
                MetricValue::Number(if total == 0 { 0.0 } else { restless as f64 / total as f64 })
            },
        },
        Metric {
            name: "dominant-land-biome",
            doc: "The most common land biome by cell count, kebab-case",
            summary: SummaryKind::Categorical,
            extract: |v| {
                let biomes = v.climate.biome_map();
                // Count land biomes in ascending name order for determinism.
                let mut counts: std::collections::BTreeMap<&'static str, usize> = std::collections::BTreeMap::new();
                for (_, b) in biomes.iter() {
                    if !b.is_marine() {
                        *counts.entry(b.name()).or_insert(0) += 1;
                    }
                }
                match counts.into_iter().max_by(|a, b| a.1.cmp(&b.1).then(b.0.cmp(a.0))) {
                    Some((name, _)) => MetricValue::Text(name.to_string()),
                    None => MetricValue::Absent,
                }
            },
        },
```

Update the two count-asserting tests: `registry_has_fourteen_metrics` → rename/retarget to 21 (or delete and rely on the new `registry_has_twenty_one_metrics_after_lands`). Keep `registry_has_unique_names` and `render_metric_list_*` as they are (they iterate the registry).

- [ ] **Step 6: Run the Lab gate**

Run: `cargo test -p hornvale-lab && cargo clippy -p hornvale-lab --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

- [ ] **Step 7: Commit**

```bash
git add windows/lab/Cargo.toml windows/lab/src/metrics.rs
git commit -m "feat(lab): Census of Lands metrics (plates, biomes, habitable fraction)"
```

---

### Task 16: Studies, calibration, and CI

**Files:**
- Delete: `studies/census-drift.study.json`
- Create: `studies/census-lands-drift.study.json`
- Create: `studies/census-of-lands.study.json`
- Modify: `windows/lab/tests/calibration.rs`
- Modify: `.github/workflows/ci.yml`

**Interfaces:**
- Consumes: the unified registry (21 metrics), `world_builder::climate_of`.
- Produces: the renamed CI drift study, the author-time headline census, and the band-count⇔rotation calibration test.

- [ ] **Step 1: Rename the drift study**

```bash
git mv studies/census-drift.study.json studies/census-lands-drift.study.json
```

Edit `studies/census-lands-drift.study.json` (name must match the file stem; the runner writes to `lab-out/<name>/` and publishes to `.../<name>/`):

```json
{ "name": "census-lands-drift",
  "description": "Distributions of everything the land and sky generators produce over 500 seeds, plus the belief and band-count calibrations, rerun in CI as a determinism guard.",
  "seeds": { "from": 0, "count": 500 },
  "pin_sets": [ { "label": "default", "pins": [] } ],
  "metrics": "all" }
```

- [ ] **Step 2: Create the headline census**

Create `studies/census-of-lands.study.json`:

```json
{ "name": "census-of-lands",
  "description": "The full Census of Lands: every land and sky metric over 10,000 worlds — the habitable-fraction unknown number and the band-count calibration at author-time scale.",
  "seeds": { "from": 0, "count": 10000 },
  "pin_sets": [ { "label": "default", "pins": [] } ],
  "metrics": "all" }
```

- [ ] **Step 3: Update the calibration test path + add band-count calibration**

In `windows/lab/tests/calibration.rs`, change the study path in the existing test to `census-lands-drift.study.json`, and add the band-count calibration:

```rust
#[test]
fn band_count_matches_the_known_function_of_rotation() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (day_i, band_i) = (idx("day-length-hours"), idx("band-count"));
    for row in &result.rows {
        if row.refusal.is_some() {
            continue;
        }
        // Locked worlds report Absent day length and "locked" band count.
        let expected = match &row.values[day_i] {
            MetricValue::Number(hours) => {
                if *hours >= 40.0 {
                    "1".to_string()
                } else if *hours >= 20.0 {
                    "3".to_string()
                } else if *hours >= 10.0 {
                    "5".to_string()
                } else {
                    "7".to_string()
                }
            }
            _ => "locked".to_string(),
        };
        let actual = match &row.values[band_i] {
            MetricValue::Text(t) => t.clone(),
            other => panic!("seed {}: band-count not text: {other:?}", row.seed),
        };
        assert_eq!(actual, expected, "seed {}: band-count calibration violated", row.seed);
    }
}
```

Keep the existing `eternal_beliefs_coincide_exactly_with_tidal_locking` test but update its `load_study` path to `census-lands-drift.study.json`.

- [ ] **Step 4: Run the calibration tests**

Run: `cargo test -p hornvale-lab --test calibration`
Expected: PASS (both calibrations hold).

- [ ] **Step 5: Update CI**

In `.github/workflows/ci.yml`, in the "Artifacts are current" step, replace the `census-drift` run with `census-lands-drift`, add the two biome-map renders (spinning + locked twin), and extend the `git diff` net. The full command block becomes:

```yaml
      - name: Artifacts are current (determinism check)
        run: |
          cargo run -p hornvale-kernel --example first_light
          cargo run -p hornvale -- new --seed 42 --sky constant --out /tmp/hv-ci-42.json
          cargo run -p hornvale -- almanac --world /tmp/hv-ci-42.json > book/src/gallery/almanac-seed-42.md
          cargo run -p hornvale -- new --seed 42 --out /tmp/hv-ci-sky.json
          cargo run -p hornvale -- almanac --world /tmp/hv-ci-sky.json > book/src/gallery/almanac-seed-42-sky.md
          cargo run -p hornvale -- new --seed 42 --rotation locked --out /tmp/hv-ci-locked.json
          cargo run -p hornvale -- almanac --world /tmp/hv-ci-locked.json > book/src/gallery/almanac-seed-42-locked.md
          cargo run -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
          cargo run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
          cargo run -p hornvale -- map --world /tmp/hv-ci-sky.json --out book/src/gallery/elevation-seed-42.ppm > book/src/gallery/elevation-seed-42.md
          cargo run -p hornvale -- biome-map --world /tmp/hv-ci-sky.json --out book/src/gallery/biome-seed-42.ppm > book/src/gallery/biome-seed-42.md
          cargo run -p hornvale -- biome-map --world /tmp/hv-ci-locked.json --out book/src/gallery/biome-seed-42-locked.ppm > book/src/gallery/biome-seed-42-locked.md
          cargo run -p hornvale -- lab run studies/census-lands-drift.study.json
          git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
```

- [ ] **Step 6: Regenerate every committed artifact locally (stdout-only to avoid leaked cargo logs)**

Run these in a release-ish local shell; the `--out`/redirect forms match CI exactly. **Do not** pipe cargo's own build log into a committed file — the redirects below capture only program stdout because cargo's logs go to stderr:

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-sky.json
cargo run -q -p hornvale -- new --seed 42 --rotation locked --out /tmp/hv-locked.json
cargo run -q -p hornvale -- almanac --world /tmp/hv-sky.json > book/src/gallery/almanac-seed-42-sky.md
cargo run -q -p hornvale -- new --seed 42 --sky constant --out /tmp/hv-42.json
cargo run -q -p hornvale -- almanac --world /tmp/hv-42.json > book/src/gallery/almanac-seed-42.md
cargo run -q -p hornvale -- almanac --world /tmp/hv-locked.json > book/src/gallery/almanac-seed-42-locked.md
cargo run -q -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
cargo run -q -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
cargo run -q -p hornvale -- map --world /tmp/hv-sky.json --out book/src/gallery/elevation-seed-42.ppm > book/src/gallery/elevation-seed-42.md
cargo run -q -p hornvale -- biome-map --world /tmp/hv-sky.json --out book/src/gallery/biome-seed-42.ppm > book/src/gallery/biome-seed-42.md
cargo run -q -p hornvale -- biome-map --world /tmp/hv-locked.json --out book/src/gallery/biome-seed-42-locked.ppm > book/src/gallery/biome-seed-42-locked.md
cargo run -q -p hornvale -- lab run studies/census-lands-drift.study.json
```

Then remove the stale generated dir and re-verify cleanliness:

```bash
git rm -r book/src/laboratory/generated/census-drift
git status --porcelain book/src/gallery book/src/reference book/src/laboratory
```

- [ ] **Step 7: Full gate + commit**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: PASS.

```bash
git add studies/ windows/lab/tests/calibration.rs .github/workflows/ci.yml book/src/gallery/ book/src/reference/ book/src/laboratory/
git commit -m "feat(lab): census-lands studies, band-count calibration, CI biome drift"
```

---

### Task 17: The book — C3 close-out

**Files:**
- Create: `book/src/gallery/biome-seed-42.md` / `.ppm` (generated in Task 16 — verify present)
- Create: `book/src/gallery/biome-seed-42-locked.md` / `.ppm` (generated in Task 16 — verify present)
- Create: `book/src/laboratory/study-002.md`
- Create: `book/src/chronicle/campaign-3c.md`
- Modify: `book/src/domains/climate.md`, `book/src/domains/terrain.md`, `book/src/domains/overview.md`
- Modify: `book/src/SUMMARY.md`
- Regenerate: `book/src/laboratory/generated/census-of-skies/*` (freshness, Task step below)

**Interfaces:** none (prose + committed artifacts). Book altitude: technical and mathematical, comprehensible without reading the code (memory [[hornvale-project-book]]).

- [ ] **Step 1: Promote the terrain chapter to tier 1**

Rewrite the "tier ladder ahead" section of `book/src/domains/terrain.md` into a **Tier 1 — the tectonic globe** section with a model card mirroring spec §4 (drawn: plate count/seeds/kinds/motions/maturities/hotspots/ocean-fraction; derived: cell→plate, velocities, boundary types, elevation, sea level, unrest; approximated: static snapshot, no erosion/isostasy, instantaneous-motion classification, analytic uplift falloff). Reference the elevation-map artifact. Keep the tier-0 Vale section.

- [ ] **Step 2: Promote the climate chapter to tier 1**

Rewrite the "tier ladder ahead" section of `book/src/domains/climate.md` into a **Tier 1 — climate and biomes** section with a model card mirroring spec §5–§6 (drawn: essentially nothing; derived: band count and winds from rotation, temperature, moisture, biome, habitability; approximated: analytic bands not fluid dynamics, prograde-only rotation direction, no ocean currents, no cloud/albedo feedback, smooth-sinusoid seasons, single-pass rain shadow). Explain the astronomy cascade (spec §7) and reference the biome-map gallery. Keep the tier-0 uniform-climate section.

- [ ] **Step 3: Update the cascade overview table**

In `book/src/domains/overview.md`, update the Climate and Terrain rows' "Tier 0" cells to note the tier-1 deepening (e.g. Climate "uniform mildness → banded climate + biomes (tier 1)", Terrain "one hand-placed vale → tectonic globe (tier 1)"), and the Consumes/Contributes columns as appropriate (Climate now consumes elevation + sky; contributes the biome field).

- [ ] **Step 4: Write the Census of Lands chapter**

Create `book/src/laboratory/study-002.md` — the comprehension-gated analysis of `census-of-lands` (10 000 worlds): the **band-count⇔rotation calibration** (validated the day it shipped — the known step function reproduced exactly) and the **habitable-fraction** unknown number (the distribution's shape, its mean, what drives its tails: obliquity, ocean fraction, tidal locking). Include ocean-fraction, mountain-coverage, unrest-coverage, and dominant-land-biome distributions. Prose at book altitude; reference the published charts under `book/src/laboratory/generated/census-lands-drift/`.

Note: the author-time 10 000-world `census-of-lands` run is not in CI. Run it once locally to gather the numbers the chapter cites:

```bash
cargo run -q --release -p hornvale -- lab run studies/census-of-lands.study.json
```

Cite its `lab-out/census-of-lands/rows.csv` aggregates in the prose. (Its published charts land under `book/src/laboratory/generated/census-of-lands/`; decide whether to commit them — for parity with Study 001, commit the summary + charts and add them to the drift net only if you also add the 10k run to CI. Default: commit the census-of-lands summary/charts but keep the 10k run author-time, documenting that these particular artifacts are refreshed by hand, not CI — state this explicitly in the chapter so a stale artifact is never mistaken for drift.)

- [ ] **Step 5: Write the chronicle entry**

Create `book/src/chronicle/campaign-3c.md` — "Campaign 3c: Climate & Biomes" (the C3 finish): the astronomy cascade made visible, the biome map as the campaign's deliverable, the exit-demo pair (seed 42 spinning vs tidally-locked), the band-count calibration, the habitable-fraction number, and the three C2 openers finally landed. Note the prograde-only approximation and the deferred hooks (marine ecology, hydrology, evolved tectonics — spec §15).

- [ ] **Step 6: Update `SUMMARY.md`**

Add under The Gallery:

```markdown
- [The Biomes of Seed 42](./gallery/biome-seed-42.md)
- [The Biomes of Seed 42 (tidally locked)](./gallery/biome-seed-42-locked.md)
```

Add under The Laboratory:

```markdown
- [Study 002: The Census of Lands](./laboratory/study-002.md)
```

Add under The Chronicle:

```markdown
- [Campaign 3c: Climate & Biomes](./chronicle/campaign-3c.md)
```

- [ ] **Step 7: Regenerate the historical census-of-skies artifacts for freshness**

The registry grew, so `census-of-skies`' committed artifacts are stale. Regenerate once (author-time, not CI-checked):

```bash
cargo run -q -p hornvale -- lab run studies/census-of-skies.study.json
```

(If preferred, leave census-of-skies untouched and add a one-line note in `book/src/laboratory/study-001.md` that it predates the land metrics — but regenerating is cleaner. Choose one and state it.)

- [ ] **Step 8: Build the book + full gate**

Run: `mdbook build book && cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: book builds; gate green. Confirm `git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/` is clean after a fresh CI-equivalent regeneration.

- [ ] **Step 9: Commit**

```bash
git add book/
git commit -m "docs(book): C3 close-out — tier-1 terrain/climate, Census of Lands, biome gallery"
```

---

## Self-Review Notes

**Spec coverage:**
- §4 (boundary field for marine biomes) → Task 4. §5 climate (bands from rotation, temperature, moisture, rain shadow, tidal lock) → Tasks 5–7. §6 biomes (Whittaker + specials, marine, habitability) → Tasks 8–10. §7 astronomy cascade → wired in Task 13 (`stellar_inputs`) and documented in Task 17. §8 integration (tier-0 seam untouched) → cross-cutting decision 3, verified by leaving `observed_phenomena`/Vale intact. §9 pins/exit demo → terrain pins already exist (3b); the exit-demo pair is the two biome-map artifacts (Task 16) + the reorganization test (Task 13). §10 Census of Lands → Tasks 15–16. §11 CI → Task 16. §12 testing → per-task property tests + calibration (Task 16). §13 openers → Tasks 1–3. §14 book → Task 17. §17 exit criteria: (1) queryable biome map + REPL query + almanac → Tasks 13/14; (2) byte-identical artifact + locked twin reorganizes → Tasks 13/16; (3) habitable-fraction number + band-count calibration → Tasks 15/16/17; (4) book + gate + drift green → Task 17.

**Deliberate deviations from a literal reading of the spec (all documented in-plan):**
- Rotation *direction* is fixed prograde (cross-cutting decision 1) — astronomy generates no spin bit and the openers don't add one; declared in the climate model card. Not an exit criterion.
- One unified drift study (`census-lands-drift`) rather than two, because the metric registry is unified (cross-cutting decision 5). The spec's `census-lands-drift` name is honored; `census-drift` is renamed, not duplicated.
- "Biome distribution" is summarized to a single `dominant-land-biome` categorical column (Task 15) — the metrics framework is one-value-per-metric; the full distribution lives in the CSV and the book chapter.

**Type consistency check:** `RotationRegime`/`SeafloorFeature`/`Biome`/`ClimateInputs`/`GeneratedClimate` are defined once (Tasks 5/8/11) and consumed with the same names in worldgen (Task 13), CLI/REPL (Task 14), and Lab (Task 15). `Megameters`/`HabitableZone` (Tasks 1/2) are consumed only inside astronomy and by the root's `stellar_inputs` via accessors. `band_count()` returns `Option<u32>` everywhere; the Lab renders `None` as `"locked"`. `biome_map()` returns an owned `CellMap<Biome>` (cloned) at every call site.

**Watch items for the executor** (carried-forward lessons):
- Verify test counts directly after each task (`cargo test --workspace 2>&1 | grep -Eo "result: ok\. [0-9]+ passed" | awk '{s+=$1}...'`) — do not trust a cheap summary. Baseline is 326.
- Regenerate committed artifacts stdout-only (Task 16 Step 6) — cargo build logs go to stderr; never let them leak into a committed `.md`.
- After Task 4, confirm no `#[allow(dead_code)]` was left on the boundary field or accessor once Task 13 consumes it.
- The `#[allow(clippy::too_many_arguments)]` on `temperature_at`/`classify` is deliberate (physical-parameter functions); keep them, don't refactor into a struct just to appease the lint unless clippy is otherwise unhappy.
