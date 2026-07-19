# The Freshet Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Surface the salt/fresh water distinction — already implicit in the drainage substrate terrain computes — as a queryable derived `WaterKind`, so any consumer can ask "is this water drinkable?".

**Architecture:** A pure per-cell classification (`classify`) over globe fields terrain already derives (`elevation`, `sea_level`, `drainage`, `endorheic`, `downhill`), materialized once at genesis into a non-serialized `GlobeSummary.water_kind: CellMap<WaterKind>` (exactly like `drainage`/`endorheic`), and exposed to room-scale consumers as a discrete `LocaleFields.water`. No new physics, no seed draws, no committed facts, no epoch — the MAP-39/The Ground shape.

**Tech Stack:** Rust (edition 2024), std + serde only. Kernel: `Geosphere`, `CellId`, `CellMap<T>` (`from_fn`, `get`), `ReferenceElevation`. Terrain: `GlobeSummary`, `drainage_field`, `downhill_targets`. `cargo nextest`, `cargo test --doc`, `cargo clippy`, `cargo fmt`, `tools/type-audit`.

## Global Constraints

- **No new committed ledger fact; no seed draw; no epoch; no census regen.** `water_kind` is a pure projection over recomputed-at-genesis globe fields (themselves never serialized). Genesis stays byte-identical.
- **Determinism:** no `HashMap`/`HashSet` (`BTreeMap`/`BTreeSet`/`Vec`/`CellMap`); no RNG; no wall-clock; all `f64` ordering via `total_cmp` (never native `<`/`>=`).
- **Layering:** terrain depends only on the kernel; locale already depends on terrain. No new workspace dependency.
- **Docs + tags:** every public item/field/variant gets a one-line doc comment (`#![warn(missing_docs)]`); every primitive at a `pub` boundary carries a valid `type-audit:` tag (ratified classes only — see `tools/type-audit/src/tag.rs`; match existing conventions). Enums (`WaterKind`) are not bare primitives and need no `bare-ok` tag (cf. `Regime`, `Substrate`).
- **Run `cargo fmt` AND `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` before every commit** (type-audit is CI-only, invisible to `make gate`). Cost-order: fmt + clippy first, then scoped tests.
- **Golden/re-baseline discipline:** any committed artifact that renders `LocaleFields` re-baselines **in the commit that drifts it**, never deferred to close.

Authored constant: `RIVER_MIN_DRAINAGE` (drainage ≥ this ⇒ fresh flowing water), reference `carve.rs::WATERFALL_MIN_DRAINAGE = 80.0`; start at `20.0`, tuned in T2 so seed-42 settlements have fresh water within a short walk.

---

## File Structure

- `domains/terrain/src/water.rs` (create) — `WaterKind`, pure `classify`, `water_field` (builds the `CellMap`), `RIVER_MIN_DRAINAGE`; all classification tests.
- `domains/terrain/src/globe.rs` (modify) — `GlobeSummary.water_kind` field + its assembly after the final `drainage`/`endorheic`.
- `domains/terrain/src/lib.rs` (modify) — `pub mod water;` + re-export `WaterKind`.
- `windows/locale/src/lib.rs` (modify) — `LocaleFields.water`, re-export `WaterKind`, sample at the max-weight cell.
- Optional (T3): a `water` map subcommand + almanac "The Waters" line + book chronicle.

---

## Task 1: `WaterKind` classification + `GlobeSummary.water_kind`

**Files:**
- Create: `domains/terrain/src/water.rs`
- Modify: `domains/terrain/src/globe.rs` (add + populate the field), `domains/terrain/src/lib.rs` (module + re-export)
- Test: inline `#[cfg(test)]` in `water.rs`

**Interfaces:**
- Consumes: `CellId`, `Geosphere`, `CellMap<T>` (`from_fn`, `get`), `ReferenceElevation`; terrain `drainage_field`, `downhill_targets`.
- Produces:
  - `pub enum WaterKind { Ocean, SaltBasin, River, DryLand }` with `pub fn is_fresh(self) -> bool`
  - `pub const RIVER_MIN_DRAINAGE: f64`
  - `pub fn classify(elevation_m: f64, sea_level_m: f64, drainage: f64, endorheic: bool, is_terminal_sink: bool) -> WaterKind`
  - `pub fn water_field(geo: &Geosphere, elevation: &CellMap<ReferenceElevation>, sea_level: ReferenceElevation, drainage: &CellMap<f64>, endorheic: &CellMap<bool>, downhill: &[Option<CellId>]) -> CellMap<WaterKind>`
  - `GlobeSummary.water_kind: CellMap<WaterKind>`

- [ ] **Step 1: Write the failing classification tests** (`water.rs` `#[cfg(test)] mod tests`)

```rust
#[test]
fn ocean_below_sea_level_is_salt() {
    let k = classify(-50.0, 0.0, 0.0, false, false);
    assert_eq!(k, WaterKind::Ocean);
    assert!(!k.is_fresh());
}

#[test]
fn a_high_drainage_exorheic_cell_is_a_fresh_river() {
    let k = classify(100.0, 0.0, RIVER_MIN_DRAINAGE + 1.0, false, false);
    assert_eq!(k, WaterKind::River);
    assert!(k.is_fresh());
}

#[test]
fn an_endorheic_feeder_river_is_still_fresh() {
    // THE CORRECTNESS KEYSTONE: endorheic (drains to an interior basin) but NOT a
    // terminal sink — a river on its way to a salt lake is fresh (the Jordan).
    let k = classify(100.0, 0.0, RIVER_MIN_DRAINAGE + 1.0, true, false);
    assert_eq!(k, WaterKind::River, "an endorheic feeder river must read fresh");
    assert!(k.is_fresh());
}

#[test]
fn the_terminal_endorheic_sink_is_a_salt_basin() {
    // The evaporative salt lake: endorheic AND a terminal sink (local min, no
    // outlet). High drainage accumulates here, but it is salt, not a fresh river.
    let k = classify(90.0, 0.0, RIVER_MIN_DRAINAGE + 999.0, true, true);
    assert_eq!(k, WaterKind::SaltBasin, "the terminal salt sink must read salt");
    assert!(!k.is_fresh());
}

#[test]
fn low_drainage_exorheic_land_is_dry() {
    let k = classify(100.0, 0.0, RIVER_MIN_DRAINAGE - 1.0, false, false);
    assert_eq!(k, WaterKind::DryLand);
    assert!(!k.is_fresh());
}

#[test]
fn classify_orders_ocean_before_salt_basin_before_river() {
    // Precedence: below sea level is Ocean even if flagged endorheic/sink/high-drainage.
    assert_eq!(classify(-1.0, 0.0, 1e9, true, true), WaterKind::Ocean);
    // A terminal sink at/above sea level with huge drainage is SaltBasin, not River.
    assert_eq!(classify(5.0, 0.0, 1e9, true, true), WaterKind::SaltBasin);
}

#[test]
fn water_field_classifies_a_synthetic_globe_deterministically() {
    // A tiny real Geosphere; plant a below-sea cell and a high-drainage cell,
    // build the field twice, assert equal (determinism) and the two cells' kinds.
    let geo = hornvale_kernel::Geosphere::new(2);
    let sea = hornvale_kernel::ReferenceElevation::new(0.0);
    let elevation = hornvale_kernel::CellMap::from_fn(&geo, |c| {
        hornvale_kernel::ReferenceElevation::new(if c.0 == 0 { -100.0 } else { 100.0 })
    });
    let drainage = hornvale_kernel::CellMap::from_fn(&geo, |c| if c.0 == 1 { RIVER_MIN_DRAINAGE + 1.0 } else { 0.0 });
    let endorheic = hornvale_kernel::CellMap::from_fn(&geo, |_| false);
    let downhill: Vec<Option<hornvale_kernel::CellId>> = (0..geo.cell_count()).map(|_| None).collect();
    let a = water_field(&geo, &elevation, sea, &drainage, &endorheic, &downhill);
    let b = water_field(&geo, &elevation, sea, &drainage, &endorheic, &downhill);
    for c in geo.cells() {
        assert_eq!(a.get(c), b.get(c));
    }
    assert_eq!(*a.get(hornvale_kernel::CellId(0)), WaterKind::Ocean);
    // cell 1 is land + high drainage + not-a-sink (downhill None makes it a sink
    // ONLY if endorheic; endorheic is false here) -> River.
    assert_eq!(*a.get(hornvale_kernel::CellId(1)), WaterKind::River);
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-terrain --lib ocean_below_sea_level_is_salt`
Expected: FAIL (`WaterKind`/`classify` undefined).

- [ ] **Step 3: Implement `water.rs`**

```rust
//! Salt/fresh water classification (The Freshet, DOM-5 first slice): a pure
//! projection over the drainage substrate the globe already computes. No seed
//! draws, no stored state beyond a recomputed-at-genesis `CellMap` — the
//! MAP-39/The Ground shape.

use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation};

/// The kind of water (if any) at a cell. `Ocean`/`SaltBasin` are salt;
/// `River` is the only drinkable (fresh) class this slice.
/// type-audit: n/a (enum, not a bare-primitive boundary)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize)]
pub enum WaterKind {
    /// Below sea level — the salt ocean.
    Ocean,
    /// A terminal endorheic sink — the evaporative salt lake / playa.
    SaltBasin,
    /// Accumulated fresh runoff (a river; includes freshwater through-flow lakes
    /// and endorheic feeder rivers on their way to a salt sink).
    River,
    /// Land with no significant surface water.
    DryLand,
}

impl WaterKind {
    /// Whether this water is drinkable (fresh). Only `River` is fresh this slice.
    pub fn is_fresh(self) -> bool {
        matches!(self, WaterKind::River)
    }
}

/// Upstream drainage accumulation at or above which a cell carries fresh flowing
/// water. Reference: `carve::WATERFALL_MIN_DRAINAGE` (80.0); rivers are more
/// common than waterfalls, so this is lower. Tuned (The Freshet T2) so seed-42
/// settlements have fresh water within a short walk.
/// type-audit: bare-ok(count: drainage-threshold)
pub const RIVER_MIN_DRAINAGE: f64 = 20.0;

/// Classify one cell from the bits the globe already derives. Pure and total;
/// precedence Ocean > SaltBasin > River > DryLand. `is_terminal_sink` is
/// `endorheic && no-downhill` (a local minimum with no outlet).
pub fn classify(
    elevation_m: f64,
    sea_level_m: f64,
    drainage: f64,
    endorheic: bool,
    is_terminal_sink: bool,
) -> WaterKind {
    if elevation_m.total_cmp(&sea_level_m).is_lt() {
        WaterKind::Ocean
    } else if endorheic && is_terminal_sink {
        WaterKind::SaltBasin
    } else if drainage.total_cmp(&RIVER_MIN_DRAINAGE).is_ge() {
        WaterKind::River
    } else {
        WaterKind::DryLand
    }
}

/// Materialize the per-cell classification (recomputed at genesis, never
/// serialized). `downhill[c] == None` marks a local minimum; a terminal salt
/// sink is an endorheic local minimum.
pub fn water_field(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    drainage: &CellMap<f64>,
    endorheic: &CellMap<bool>,
    downhill: &[Option<CellId>],
) -> CellMap<WaterKind> {
    let sea = sea_level.get();
    CellMap::from_fn(geo, |c| {
        let terminal_sink = *endorheic.get(c) && downhill[c.0 as usize].is_none();
        classify(
            elevation.get(c).get(),
            sea,
            *drainage.get(c),
            *endorheic.get(c),
            terminal_sink,
        )
    })
}
```

(If `ReferenceElevation::new`/`.get()` differ, match the existing calls in `drainage.rs`/`globe.rs` — the elevation getter used there is `.get()`.)

- [ ] **Step 4: Run the `water.rs` tests to green**

Run: `cargo test -p hornvale-terrain --lib water::`
Expected: PASS (7 tests).

- [ ] **Step 5: Wire `GlobeSummary.water_kind` in `globe.rs`**

Add the field to the `GlobeSummary` struct (next to `drainage`/`endorheic`, ~line 45-47), with a doc comment and its `type-audit:` entry appended to the struct's tag line (enums need no `bare-ok` class — follow how a non-primitive field is handled, or omit it from the primitive tag list):

```rust
    /// Salt/fresh water classification per cell (The Freshet). Recomputed at
    /// genesis, never serialized; a pure projection over drainage/endorheic.
    pub water_kind: CellMap<WaterKind>,
```

Populate it right after the FINAL `drainage`/`endorheic` are computed (globe.rs ~line 399-400, before `GlobeSummary { … }` is constructed):

```rust
    let (drainage, endorheic) =
        crate::drainage::drainage_field(geosphere, &elevation_map, sea_level);
    let downhill_final = crate::drainage::downhill_targets(geosphere, &elevation_map, sea_level);
    let water_kind = crate::water::water_field(
        geosphere, &elevation_map, sea_level, &drainage, &endorheic, &downhill_final,
    );
```

and add `water_kind,` to the `GlobeSummary { … }` initializer. In `lib.rs`: `pub mod water;` and `pub use water::{WaterKind, RIVER_MIN_DRAINAGE};`.

- [ ] **Step 6: Build + full terrain suite + fmt/clippy/type-audit**

Run: `cargo test -p hornvale-terrain 2>&1 | tail -5` (expect green — `GlobeSummary` now carries `water_kind`; no existing test should break since nothing else reads it yet).
Then: `cargo fmt && cargo clippy -p hornvale-terrain --all-targets -- -D warnings && cargo run --manifest-path tools/type-audit/Cargo.toml -- check`

- [ ] **Step 7: Commit**

```bash
git add domains/terrain/src/water.rs domains/terrain/src/globe.rs domains/terrain/src/lib.rs
git commit -m "feat(terrain): WaterKind salt/fresh classification + GlobeSummary.water_kind — the-freshet T1"
```

---

## Task 2: expose `LocaleFields.water` + genesis byte-identity + tune `RIVER_MIN_DRAINAGE`

**Files:**
- Modify: `windows/locale/src/lib.rs` (`LocaleFields.water`, re-export, sample), `domains/terrain/src/water.rs` (`RIVER_MIN_DRAINAGE` final value)
- Test: inline in `windows/locale/src/lib.rs`; a genesis byte-identity check.

**Interfaces:**
- Consumes: `WaterKind` (terrain, T1), `GlobeSummary.water_kind`, the locale `best.0` max-weight cell.
- Produces: `LocaleFields.water: WaterKind`.

- [ ] **Step 1: Write the failing locale test**

```rust
#[test]
fn locale_water_field_varies_and_includes_fresh_water_on_seed_42() {
    // Wired to real geography (not a stuck constant) AND fresh water exists —
    // the sanity that unblocks The Surmise. Uses only the worldgen+locale API.
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    ).unwrap();
    let ctx = LocaleContext::build(&world).unwrap();
    let mut kinds: std::collections::BTreeSet<hornvale_terrain::WaterKind> = Default::default();
    let mut saw_fresh = false;
    for i in 0..400u32 {
        let t = i as f64;
        // a deterministic spread of directions over the sphere
        let dir = [(t * 0.017).cos(), (t * 0.023).sin() * 0.5, (t * 0.031).cos()];
        let addr = hornvale_kernel::RoomAddr::containing(dir, 6);
        if let Ok(loc) = ctx.describe(&addr, hornvale_kernel::WorldTime { day: 0.0 }) {
            kinds.insert(loc.fields.water);
            if loc.fields.water == hornvale_terrain::WaterKind::River {
                saw_fresh = true;
            }
        }
    }
    assert!(kinds.len() >= 2, "water must vary across the globe (wired to real geography), got {kinds:?}");
    assert!(saw_fresh, "seed 42 must have fresh water (River) reachable on land — else lower RIVER_MIN_DRAINAGE");
}
```

This single test is both the exposure check (the field is populated and varies) and the `RIVER_MIN_DRAINAGE` tuning gate (if `saw_fresh` fails, the threshold is too high).

- [ ] **Step 2: Run to verify failure** — FAIL (`loc.fields.water` does not exist).

- [ ] **Step 3: Implement the exposure.** Add to `LocaleFields`:

```rust
    /// Salt/fresh water at the room (max-weight cell — categorical, inherited,
    /// never blended). `water.is_fresh()` is the drinkable query.
    pub water: hornvale_terrain::WaterKind,
```

Re-export `pub use hornvale_terrain::WaterKind;` from the locale crate. In `describe`, after `best` is computed (the max-weight corner, ~line 174-182), set the field in the `LocaleFields { … }` initializer:

```rust
    water: *self.terrain.globe().water_kind.get(best.0),
```

- [ ] **Step 4: Tune `RIVER_MIN_DRAINAGE`.** Run `locale_water_field_varies_and_includes_fresh_water_on_seed_42`. If `saw_fresh` fails (or fresh water is implausibly sparse/dense when you spot-check the count), lower `RIVER_MIN_DRAINAGE` until seed-42 has a sensible river network, and document the chosen value's basis in its doc comment. Keep it a named constant.

- [ ] **Step 5: Genesis byte-identity + artifact re-baseline.** Confirm the ledger and non-locale artifacts are unchanged, and re-baseline the locale/scene artifacts that now carry `water`:

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/   # non-water artifacts: clean
# regenerate any committed artifact that renders LocaleFields (locale/room pages) per its documented command; review + stage the water-field additions
```

If a committed artifact serializes `LocaleFields`, its diff should show ONLY the added `water` field — re-pin it in THIS task's commit.

- [ ] **Step 6: fmt + clippy + type-audit + full suites**

Run: `cargo test -p hornvale-locale -p hornvale-terrain`, then `cargo fmt && cargo clippy -p hornvale-locale --all-targets -- -D warnings && cargo run --manifest-path tools/type-audit/Cargo.toml -- check`

- [ ] **Step 7: Commit**

```bash
git add windows/locale/src/lib.rs domains/terrain/src/water.rs book/
git commit -m "feat(locale): LocaleFields.water — the drinkable-water query; tune RIVER_MIN_DRAINAGE — the-freshet T2"
```

---

## Task 3: surfacing (optional map lens + almanac line) + book chronicle + close

**Files:**
- Optional: a `water` map subcommand (mirror the elevation/lithology map), an almanac "The Waters" line (mirror The Ground's almanac section).
- Create: `book/src/chronicle/the-freshet.md` (+ `SUMMARY.md`), `docs/retrospectives/the-freshet.md`.
- Modify: `book/src/frontier/idea-registry.md` (flip DOM-5, advance SEQ-2).

- [ ] **Step 1: (Optional) water map lens.** If it fits cheaply, add a `map --layer water` (or a `water` subcommand) that renders `globe.water_kind` as a PPM (ocean/salt-basin/river/dry colour key), mirroring the elevation map's structure. Add a golden/drift artifact if the repo pins map outputs. **No census** — do not add census metrics (that needs an AWS regen). Write a test asserting the renderer produces the four classes on seed-42.

- [ ] **Step 2: (Optional) almanac "The Waters" line.** Mirror The Ground's almanac section: a line reporting seed-42's fresh-water land fraction (computed from `globe.water_kind`). Regenerate + re-pin the almanac artifact in this commit.

- [ ] **Step 3: Book chronicle + retrospective.** Write `book/src/chronicle/the-freshet.md` (the salt/fresh story: the substrate already computed drainage/endorheic; the classification exposes potability; endorheic feeders are fresh, terminal sinks salt; no epoch; unblocks The Surmise). Wire into `SUMMARY.md`. Write `docs/retrospectives/the-freshet.md`. Promote `.superpowers/sdd/followups.md` into the retro.

- [ ] **Step 4: Registry flips.** In `book/src/frontier/idea-registry.md`: flip **DOM-5** to reflect the first slice shipped (classification exposed; crate split still pending) and repoint its Where; advance **SEQ-2**'s note (the water truth exists; infrastructure still pending). Never delete a row.

- [ ] **Step 5: Full gate + artifact drift.**

```bash
make gate
git diff --exit-code book/ docs/   # after regenerating any committed artifacts
```

- [ ] **Step 6: Commit.**

```bash
git add domains/ windows/ cli/ book/ docs/
git commit -m "docs(the-freshet): water map + almanac + chronicle + registry (close) — the-freshet T3"
```

- [ ] **Step 7: STOP — G6 is a hard stop.** Present the post-G3 ledger digest (the no-epoch/no-census save-format result #4 leading, the classification #3, the scope #2) to Nathan; the FF, push, pull, teardown are his calls under `closing-a-campaign`. **Then unpark The Surmise** (followup 6): absorb main into `worktree-the-surmise`, re-point its `is_water` at `LocaleFields.water.is_fresh()`, finish T5, close.

---

## Self-review notes (author)

- **Spec coverage:** §3.1 classification + materialization → T1; §3.2 locale exposure → T2; §3.3 optional surfacing → T3; §5 tests (1–5 classify keystones incl. endorheic-feeder-fresh + terminal-sink-salt, 6 determinism, 7 locale, 8 genesis byte-identity) → T1/T2; §6 save-format (no epoch/census, re-baseline) → T2 Step 5 + Global Constraints; §7 non-goals reserved (no entities/lake-physics/crate-split/scene/census); §8 constants → T1/T2.
- **No new predicate / no seed draw:** grep guard — `water.rs` calls no `register_predicate`, no `Stream`/`Seed` draw; it reads only globe fields. Genesis byte-identity pin in T2 Step 5 is the tripwire.
- **Type consistency:** `WaterKind` (4 variants, `is_fresh`), `classify(elevation_m, sea_level_m, drainage, endorheic, is_terminal_sink)`, `water_field(geo, elevation, sea_level, drainage, endorheic, downhill)`, `GlobeSummary.water_kind`, `LocaleFields.water` — identical across T1/T2/T3.
- **Terminal-sink source:** `downhill[c] == None` (reuse `downhill_targets`), not a re-derived neighbor scan — matches globe.rs's existing use.
