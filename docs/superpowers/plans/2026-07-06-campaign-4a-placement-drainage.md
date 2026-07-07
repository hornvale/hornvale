# Campaign 4a: Placement & Drainage — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Put people on the globe — a coarse drainage field, field-driven settlement placement (a spaced scatter with a flagship), the generated flagship replacing the hand-placed Vale, and the settlement-map artifact — with culture still tier-0 through 4a.

**Architecture:** `domains/terrain` gains a deterministic flow-accumulation drainage field (elevation-derived, no new seed streams) with endorheic-basin flags. `domains/settlement` becomes a kernel-only placement provider: given per-cell suitability inputs assembled at the composition root from terrain + climate, it ranks habitable cells and places a spaced scatter of settlements, marking the highest-suitability cell the flagship. The composition root (`windows/worldgen`) reconstructs terrain + climate, computes the site inputs, commits generated settlements (each its own place entity tagged with its `CellId`), and runs the tier-0 culture/religion cascade on the flagship. The hand-placed Vale genesis is removed.

**Tech Stack:** Rust edition 2024; `hornvale-kernel` (`Geosphere`/`CellMap`/`CellId`, ledger/registry, `Seed`/`Stream`); std only; hand-rolled P6 PPM in the biome-map tradition.

## Global Constraints

- `serde` + `serde_json` only, workspace-wide. **No new crates.** Randomness is the kernel's `Seed`/`Stream`.
- **Layering (constitutional, enforced by `cli/tests/architecture.rs`):** `domains/settlement` and `domains/culture` depend on `hornvale-kernel` and **nothing else** — never terrain, climate, or astronomy. All cross-domain values reach settlement as bare kernel types (`CellId`, `[f64; 3]`, `f64`, `bool`), assembled at the composition root. `domains/terrain` stays kernel-only.
- **Determinism (constitutional):** same seed + pins → byte-identical globe, settlements, and settlement map. **No `HashMap`/`HashSet`/`std::time`** (clippy-forbidden workspace-wide via `clippy.toml`; a justified exception needs a scoped `#[allow(clippy::disallowed_types)]` with a comment). Float sorting uses `total_cmp`; drainage sorts elevations which are strictly ordered by C3's `CELL_EPSILON_M`, so ties do not occur but `total_cmp` is still used. `BTreeMap`/`Vec` only.
- **Terrain types are recomputed, never serialized** — no serde derives on `TectonicGlobe`/`GeneratedTerrain`. The drainage field is a new recomputed field, not persisted.
- **Settlements ARE committed to the ledger as facts** (they persist; queries read them back — no reconstruction). Only terrain/climate reconstruct from seed + pins.
- **Stream labels are permanent save-format contracts.** Drainage adds none (pure function of elevation). Settlement keeps its existing labels (`settlement`, `settlement/name`, `settlement/name-pick`, `settlement/population`) and adds `settlement/placement` only if a new draw is introduced (this plan introduces `settlement/placement` for per-settlement population against carrying capacity — declared in `streams` and published through `cli/src/streams.rs`).
- **Coordinate convention:** latitude = `asin(z)`, longitude = `atan2(y, x)`, degrees. Spherical proximity via dot product of unit positions (higher dot = closer).
- Every crate `#![warn(missing_docs)]`; every public item/field/variant a `///` doc comment.
- Edition 2024; `cargo fmt` the final step of every task. **Full gate:** `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`, plus the CI artifact drift check.
- **Baseline:** 361 tests pass on `main` at the start of this plan. Verify counts directly after each task.

---

## Cross-cutting design decisions (read before Task 1)

1. **A settlement is its own place entity.** Tier-0 committed the village *located in* a separate place (the Vale). Tier-1 merges them: each placement mints ONE entity carrying `is-place` (with `name`, `biome`, `cell-id`, `latitude`, `longitude`) and `is-settlement` (with `population`). `located-in` is dropped (set to `None` in `VillageInfo`). So `hornvale_terrain::places` returns the generated settlements' home cells, and `village_info` returns the flagship (the first settlement committed).

2. **The flagship is committed first.** Placement returns settlements in descending suitability; the composition root commits them in that order, so the flagship is the first `is-settlement` fact — `village_info` (which reads `find(IS_SETTLEMENT).next()`) returns it unchanged, and the tier-0 culture/religion cascade runs on it.

3. **Biome per settlement comes from the climate field** (`climate_of(world).biome_at(cell).name()`), assembled at the root — not hand-authored. The tier-0 `BIOME`/`IS_PLACE` predicates stay registered.

4. **Suitability is settlement's domain judgment**, computed inside `settlement::place` from bare inputs the root supplies (`habitable`, `freshwater`, `coastal`, `temperature_c`, `hostility`). The root does the terrain/climate reads; settlement does the social scoring — so settlement imports no domain.

5. **Drainage lives in terrain** (elevation-derived geometry over the Geosphere). It is a refinement within terrain's tier-1 interface, drift-checked with the rest of the globe.

---

## File Structure

| File | Action | Responsibility |
|---|---|---|
| `domains/terrain/src/drainage.rs` | Create | Flow-accumulation drainage field + endorheic-basin detection over the Geosphere. |
| `domains/terrain/src/globe.rs` | Modify | Store `drainage: CellMap<f64>` and `endorheic: CellMap<bool>` on `TectonicGlobe`; compute in `generate`. |
| `domains/terrain/src/provider.rs` | Modify | `drainage_at(cell) -> f64`, `is_endorheic(cell) -> bool` accessors. |
| `domains/settlement/src/lib.rs` | Modify | Module wiring, `SITE`/placement stream label, re-exports; keep tier-0 name/population helpers. |
| `domains/settlement/src/placement.rs` | Create | `SiteInput`, `Placement`, `place`, suitability scoring, greedy spherical spacing. |
| `domains/settlement/src/genesis.rs` | Create | Tier-1 genesis: commit a scatter of settlements (each its own place entity tagged with `CellId`/lat/lon/biome/population). |
| `windows/worldgen/src/lib.rs` | Modify | Rewire `build_world`: assemble `SiteInput`s from terrain + climate, place, commit, run culture/religion on the flagship; remove the Vale genesis; `settlement_lines` for the almanac. |
| `windows/almanac/src/lib.rs` | Modify | Generated settlement section (`settlement_lines` field) in "The People". |
| `domains/settlement/src/render.rs` | Create | Deterministic settlement-map render (biome map overlaid with settlement marks; flagship distinguished). |
| `cli/src/main.rs` | Modify | `settlement-map` command (markdown + PPM); usage text. |
| `cli/src/repl.rs` | Modify | `settlements` and `settlement <lat> <lon>` commands. |
| `cli/src/streams.rs` | Modify | Manifest test asserts the new settlement placement label. |
| `windows/lab/src/metrics.rs` | Modify | `WorldView` already reconstructs terrain/climate; no new metrics in 4a (Census of Peoples is 4b) — update any Vale-dependent test expectations. |
| `.github/workflows/ci.yml` | Modify | Settlement-map render(s) added to the drift check. |
| `book/src/gallery/settlement-seed-42.md` / `.ppm` | Create (generated) | The settlement-map artifact. |
| `book/src/chronicle/campaign-4a.md` | Create | Chronicle entry (DoD per merged plan). |
| `book/src/SUMMARY.md` | Modify | Gallery + chronicle entries. |
| `book/src/gallery/almanac-*.md`, `book/src/reference/*-generated.md` | Regenerate | Committed generated artifacts (new People section, new stream label). |

Dataflow inside settlement: `placement` (pure scoring + spacing over `SiteInput`s) → `genesis` (commits the returned `Placement`s). Terrain: `drainage` → `globe` (assembly) → `provider`.

---

### Task 1: Terrain drainage field

**Files:**
- Create: `domains/terrain/src/drainage.rs`
- Modify: `domains/terrain/src/lib.rs` (add `pub mod drainage;`)

**Interfaces:**
- Consumes: `hornvale_kernel::{Geosphere, CellId, CellMap}`.
- Produces: `fn drainage_field(geo: &Geosphere, elevation: &CellMap<f64>, sea_level: f64) -> (CellMap<f64>, CellMap<bool>)` — `(drainage, endorheic)`. `drainage[c]` = upstream land-cell count draining through `c` (≥ 1 for land, 0 for ocean); `endorheic[c]` = true iff `c` is a land cell whose downhill path terminates at an interior local minimum without reaching the sea.

- [ ] **Step 1: Write failing tests**

Create `domains/terrain/src/drainage.rs`:

```rust
//! Coarse hydrology: a deterministic flow-accumulation field over the
//! Geosphere. Each land cell drains to its lowest neighbor; upstream area
//! accumulates downhill. Cells whose downhill path never reaches the sea are
//! endorheic (interior basins — the banked salt-basin hook, spec C3 §15).
//! Elevation-derived, no seed draws; declared approximations: single
//! lowest-neighbor flow direction (no splitting), unit-area accumulation
//! (no precipitation weighting), no sub-cell river geometry or lake filling.

use hornvale_kernel::{CellId, CellMap, Geosphere};

/// Compute the drainage (flow-accumulation) field and the endorheic mask.
/// Returns `(drainage, endorheic)`: `drainage[c]` counts the land cells
/// upstream of and including `c` (0 on ocean cells); `endorheic[c]` is true
/// when `c`'s downhill path ends at an interior minimum, never reaching sea.
pub fn drainage_field(
    geo: &Geosphere,
    elevation: &CellMap<f64>,
    sea_level: f64,
) -> (CellMap<f64>, CellMap<bool>) {
    // (implemented in Step 3)
    let _ = (geo, elevation, sea_level);
    unimplemented!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::globe::generate;
    use crate::pins::TerrainPins;
    use hornvale_kernel::{Geosphere, Seed};

    #[test]
    fn accumulation_conserves_land_area_and_is_deterministic() {
        let geo = Geosphere::new(4);
        let globe = generate(Seed(42), &geo, &TerrainPins::default()).unwrap().globe;
        let (a, ea) = drainage_field(&geo, &globe.elevation, globe.sea_level);
        let (b, _eb) = drainage_field(&geo, &globe.elevation, globe.sea_level);
        assert_eq!(a, b, "drainage must be deterministic");
        // Every land cell drains at least itself; ocean cells are zero.
        let mut land = 0usize;
        for c in geo.cells() {
            if *globe.elevation.get(c) >= globe.sea_level {
                land += 1;
                assert!(*a.get(c) >= 1.0, "land cell {} drains < 1", c.0);
            } else {
                assert_eq!(*a.get(c), 0.0, "ocean cell {} has drainage", c.0);
            }
        }
        // A sink (endorheic outlet or coastal outlet) collects; the maximum
        // accumulation cannot exceed the land-cell count.
        let max = a.iter().map(|(_, v)| *v).fold(0.0, f64::max);
        assert!(max <= land as f64, "accumulation {max} exceeds land {land}");
        // Endorheic cells, if any, are land.
        for (c, e) in ea.iter() {
            if *e {
                assert!(*globe.elevation.get(c) >= globe.sea_level, "endorheic ocean cell {}", c.0);
            }
        }
    }

    #[test]
    fn drainage_flows_downhill_to_a_wetter_outlet() {
        // On a real globe, the single wettest (max-accumulation) land cell
        // must sit no higher than the mean land elevation — water pools low.
        let geo = Geosphere::new(4);
        let globe = generate(Seed(7), &geo, &TerrainPins::default()).unwrap().globe;
        let (d, _e) = drainage_field(&geo, &globe.elevation, globe.sea_level);
        let land: Vec<CellId> = geo.cells().filter(|c| *globe.elevation.get(*c) >= globe.sea_level).collect();
        let mean: f64 = land.iter().map(|c| *globe.elevation.get(*c)).sum::<f64>() / land.len() as f64;
        let outlet = land.iter().copied().max_by(|a, b| d.get(*a).total_cmp(d.get(*b))).unwrap();
        assert!(*globe.elevation.get(outlet) <= mean, "drainage outlet stands above mean land");
    }
}
```

- [ ] **Step 2: Run to see it fail**

Run: `cargo test -p hornvale-terrain drainage 2>&1 | tail -6`
Expected: FAIL (`unimplemented!`).

- [ ] **Step 3: Implement**

Replace the `drainage_field` stub body:

```rust
pub fn drainage_field(
    geo: &Geosphere,
    elevation: &CellMap<f64>,
    sea_level: f64,
) -> (CellMap<f64>, CellMap<bool>) {
    let n = geo.cell_count();
    let is_land = |c: CellId| *elevation.get(c) >= sea_level;

    // Downhill target per land cell: the strictly-lowest neighbor (elevations
    // are strictly ordered by C3's per-cell epsilon, so there is no tie). A
    // cell with no lower neighbor is a local minimum (a sink). `None` = ocean
    // or local minimum (no outflow).
    let mut downhill: Vec<Option<CellId>> = vec![None; n];
    let mut reaches_sea = vec![false; n];
    for c in geo.cells() {
        if !is_land(c) {
            continue;
        }
        let here = *elevation.get(c);
        let mut best: Option<CellId> = None;
        let mut best_e = here;
        for &nb in geo.neighbors(c) {
            let e = *elevation.get(nb);
            if e < best_e {
                best_e = e;
                best = Some(nb);
            }
        }
        downhill[c.0 as usize] = best;
    }

    // A land cell reaches the sea iff following downhill lands on an ocean
    // cell. Trace each land cell's path (bounded by n) once, memoizing.
    // Endorheic = land cell that does NOT reach the sea.
    // 0 = unknown, 1 = reaches sea, 2 = does not.
    let mut state = vec![0u8; n];
    for start in geo.cells() {
        if !is_land(start) || state[start.0 as usize] != 0 {
            continue;
        }
        let mut path = Vec::new();
        let mut cur = start;
        let verdict;
        loop {
            if !is_land(cur) {
                verdict = 1; // stepped onto ocean
                break;
            }
            if state[cur.0 as usize] != 0 {
                verdict = state[cur.0 as usize];
                break;
            }
            match downhill[cur.0 as usize] {
                None => {
                    verdict = 2; // interior local minimum: endorheic sink
                    break;
                }
                Some(next) => {
                    path.push(cur);
                    cur = next;
                }
            }
        }
        for c in path {
            state[c.0 as usize] = verdict;
        }
    }
    for c in geo.cells() {
        if is_land(c) {
            reaches_sea[c.0 as usize] = state[c.0 as usize] == 1;
        }
    }

    // Flow accumulation: process land cells high → low (strict order, total_cmp
    // tie-break by CellId), each pushing its running total to its downhill
    // neighbor. Ocean cells stay 0.
    let mut order: Vec<CellId> = geo.cells().filter(|c| is_land(*c)).collect();
    order.sort_by(|a, b| {
        elevation
            .get(*b)
            .total_cmp(elevation.get(*a))
            .then(a.0.cmp(&b.0))
    });
    let mut acc = vec![0.0f64; n];
    for &c in &order {
        acc[c.0 as usize] += 1.0;
        if let Some(next) = downhill[c.0 as usize] {
            acc[next.0 as usize] += acc[c.0 as usize];
        }
    }

    let drainage = CellMap::from_fn(geo, |c| if is_land(c) { acc[c.0 as usize] } else { 0.0 });
    let endorheic = CellMap::from_fn(geo, |c| is_land(c) && !reaches_sea[c.0 as usize]);
    (drainage, endorheic)
}
```

- [ ] **Step 4: Run + fmt + clippy**

Run: `cargo test -p hornvale-terrain drainage && cargo clippy -p hornvale-terrain --all-targets -- -D warnings`
Expected: PASS, clean. `cargo fmt`.

- [ ] **Step 5: Wire the module + commit**

Add `pub mod drainage;` to `domains/terrain/src/lib.rs`.

```bash
git add domains/terrain/src/drainage.rs domains/terrain/src/lib.rs
git commit -m "feat(terrain): coarse drainage field (flow accumulation + endorheic basins)"
```

---

### Task 2: Store drainage on the globe + provider accessors

**Files:**
- Modify: `domains/terrain/src/globe.rs`
- Modify: `domains/terrain/src/provider.rs`

**Interfaces:**
- Consumes: `drainage::drainage_field` (Task 1).
- Produces: `TectonicGlobe.drainage: CellMap<f64>`, `TectonicGlobe.endorheic: CellMap<bool>` (public fields); `GeneratedTerrain::drainage_at(&self, id) -> f64`, `GeneratedTerrain::is_endorheic(&self, id) -> bool`.

- [ ] **Step 1: Write the failing test in `provider.rs`**

Add to `domains/terrain/src/provider.rs` tests:

```rust
#[test]
fn provider_exposes_drainage_and_endorheic() {
    let geo = Geosphere::new(3);
    let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
    let terrain = GeneratedTerrain::new(geo.clone(), outcome.clone());
    for cell in geo.cells() {
        assert_eq!(terrain.drainage_at(cell), *outcome.globe.drainage.get(cell));
        assert_eq!(terrain.is_endorheic(cell), *outcome.globe.endorheic.get(cell));
    }
    // Land cells accumulate at least themselves.
    let land = geo.cells().find(|c| !terrain.is_ocean(*c)).unwrap();
    assert!(terrain.drainage_at(land) >= 1.0);
}
```

- [ ] **Step 2: Run to see it fail**

Run: `cargo test -p hornvale-terrain provider_exposes_drainage 2>&1 | tail -5`
Expected: FAIL — no field/method.

- [ ] **Step 3: Add the fields + compute in `generate`**

In `domains/terrain/src/globe.rs`, add to `TectonicGlobe`:

```rust
    /// Flow-accumulation drainage per cell (upstream land-cell count; 0 on
    /// ocean). Recomputed at genesis, never serialized.
    pub drainage: CellMap<f64>,
    /// Endorheic mask: land cells whose downhill path never reaches the sea.
    pub endorheic: CellMap<bool>,
```

In `generate`, after `sea_level` is derived and before assembling the notes/struct:

```rust
    let (drainage, endorheic) = crate::drainage::drainage_field(geosphere, &elevation_map, sea_level);
```

Add `drainage` and `endorheic` to the returned `TectonicGlobe { ... }`.

- [ ] **Step 4: Add provider accessors**

In `domains/terrain/src/provider.rs`, add to `impl GeneratedTerrain`:

```rust
    /// Flow-accumulation drainage at a cell (upstream land-cell count; 0 on ocean).
    pub fn drainage_at(&self, id: CellId) -> f64 {
        *self.globe.drainage.get(id)
    }

    /// Whether a cell is an endorheic (interior-draining) land cell.
    pub fn is_endorheic(&self, id: CellId) -> bool {
        *self.globe.endorheic.get(id)
    }
```

- [ ] **Step 5: Gate + commit**

Run: `cargo test -p hornvale-terrain && cargo clippy -p hornvale-terrain --all-targets -- -D warnings`
Expected: PASS (the `GeneratedTerrain::new` cell-count assertion and `PartialEq` on `TectonicGlobe` still hold — `CellMap<bool>` is `PartialEq`). `cargo fmt`.

```bash
git add domains/terrain/src/globe.rs domains/terrain/src/provider.rs
git commit -m "feat(terrain): expose drainage + endorheic on the globe and provider"
```

---

### Task 3: Settlement placement core

**Files:**
- Create: `domains/settlement/src/placement.rs`
- Modify: `domains/settlement/src/lib.rs` (add `pub mod placement;` + re-exports)

**Interfaces:**
- Consumes: `hornvale_kernel::CellId`.
- Produces:
  - `struct SiteInput { cell: CellId, position: [f64; 3], habitable: bool, freshwater: f64, coastal: bool, temperature_c: f64, hostility: f64 }` (all `pub`).
  - `struct Placement { cell: CellId, position: [f64; 3], suitability: f64 }` (all `pub`, `#[derive(Debug, Clone, Copy, PartialEq)]`).
  - `fn suitability(site: &SiteInput) -> Option<f64>` — `None` for uninhabitable cells; else a bounded score in `[0, 1]`.
  - `fn place(sites: &[SiteInput], min_separation_dot: f64, floor: f64) -> Vec<Placement>` — descending-suitability greedy scatter with spherical spacing; `[0]` is the flagship. Deterministic (ties by `CellId`).

Spacing note: two unit vectors are "too close" when their dot product exceeds `min_separation_dot` (higher dot = smaller angle). The composition root passes `min_separation_dot = (12.0_f64.to_radians()).cos()` (≈ 0.978, a ~12° minimum separation) and `floor = 0.25`.

- [ ] **Step 1: Write failing tests**

Create `domains/settlement/src/placement.rs`:

```rust
//! Field-driven settlement placement: score every habitable cell for
//! suitability, then place a spaced scatter greedily from most to least
//! suitable. The single most-suitable cell is the flagship. Kernel-only:
//! the composition root supplies each cell's bare inputs (habitability,
//! freshwater, coast, temperature, hostility); this module does the social
//! scoring and the geometry.

use hornvale_kernel::CellId;

/// The bare per-cell inputs the composition root assembles from terrain and
/// climate. Settlement never imports those domains; it sees only this.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SiteInput {
    /// The cell this site sits on.
    pub cell: CellId,
    /// Unit-sphere position (for spacing).
    pub position: [f64; 3],
    /// Whether the cell is habitable (land, water, tolerable season).
    pub habitable: bool,
    /// Freshwater availability in `[0, 1]` (drainage/coast/moisture, at root).
    pub freshwater: f64,
    /// Whether the cell borders the ocean.
    pub coastal: bool,
    /// Annual-mean temperature, °C.
    pub temperature_c: f64,
    /// Hostility in `[0, 1]` (aridity, tectonic unrest).
    pub hostility: f64,
}

/// A placed settlement's cell, position, and the suitability that earned it.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Placement {
    /// The settlement's cell.
    pub cell: CellId,
    /// Unit-sphere position.
    pub position: [f64; 3],
    /// The suitability score that placed it, `[0, 1]`.
    pub suitability: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn site(cell: u32, pos: [f64; 3], habitable: bool, fresh: f64, coastal: bool, temp: f64, host: f64) -> SiteInput {
        SiteInput { cell: CellId(cell), position: pos, habitable, freshwater: fresh, coastal, temperature_c: temp, hostility: host }
    }

    #[test]
    fn uninhabitable_scores_none_and_watered_beats_dry() {
        assert!(suitability(&site(0, [1.0, 0.0, 0.0], false, 1.0, true, 15.0, 0.0)).is_none());
        let wet = suitability(&site(1, [1.0, 0.0, 0.0], true, 0.9, true, 15.0, 0.0)).unwrap();
        let dry = suitability(&site(2, [1.0, 0.0, 0.0], true, 0.1, false, 15.0, 0.5)).unwrap();
        assert!(wet > dry, "watered/coastal/calm ({wet}) must beat dry/inland/hostile ({dry})");
        assert!((0.0..=1.0).contains(&wet) && (0.0..=1.0).contains(&dry));
    }

    #[test]
    fn placement_respects_spacing_floor_and_ranks_flagship_first() {
        // Three near-identical high-suitability cells clustered together plus
        // one far, plus one below the floor.
        let close_a = [1.0, 0.0, 0.0];
        let close_b = [0.9998, 0.02, 0.0]; // ~1.1° from close_a
        let far = [-1.0, 0.0, 0.0];
        let sites = vec![
            site(10, close_a, true, 0.9, true, 15.0, 0.0),
            site(11, close_b, true, 0.8, true, 15.0, 0.0),
            site(12, far, true, 0.85, true, 15.0, 0.0),
            site(13, [0.0, 1.0, 0.0], true, 0.05, false, 40.0, 0.9), // below floor
        ];
        let sep = (12.0_f64.to_radians()).cos();
        let placed = place(&sites, sep, 0.25);
        // close_b is within 12° of close_a and lower suitability → excluded.
        let cells: Vec<u32> = placed.iter().map(|p| p.cell.0).collect();
        assert!(cells.contains(&10) && cells.contains(&12), "expected the two spaced high cells");
        assert!(!cells.contains(&11), "clustered lower-suitability cell must be skipped");
        assert!(!cells.contains(&13), "below-floor cell must be skipped");
        // Flagship is the global suitability argmax, placed first.
        assert_eq!(placed[0].cell.0, 10);
        assert!(placed.windows(2).all(|w| w[0].suitability >= w[1].suitability), "not descending");
    }

    #[test]
    fn placement_is_deterministic() {
        let sites = vec![
            site(1, [1.0, 0.0, 0.0], true, 0.7, true, 15.0, 0.1),
            site(2, [0.0, 1.0, 0.0], true, 0.6, false, 12.0, 0.2),
        ];
        let sep = (12.0_f64.to_radians()).cos();
        assert_eq!(place(&sites, sep, 0.25), place(&sites, sep, 0.25));
    }
}
```

- [ ] **Step 2: Run to see it fail**

Run: `cargo test -p hornvale-settlement placement 2>&1 | tail -8`
Expected: FAIL — `suitability`/`place` undefined.

- [ ] **Step 3: Implement**

Insert before the tests module:

```rust
/// Score a site's suitability for settlement in `[0, 1]`; `None` if the cell
/// is uninhabitable. Watered, coastal, temperate, calm cells score high;
/// dry, inland, extreme, hostile cells score low.
pub fn suitability(site: &SiteInput) -> Option<f64> {
    if !site.habitable {
        return None;
    }
    // Temperance: a triangular preference peaking at 15 °C, zero by 0/30 °C.
    let temperance = (1.0 - (site.temperature_c - 15.0).abs() / 15.0).clamp(0.0, 1.0);
    let coast = if site.coastal { 1.0 } else { 0.0 };
    let raw = 0.45 * site.freshwater.clamp(0.0, 1.0)
        + 0.20 * coast
        + 0.35 * temperance
        - 0.5 * site.hostility.clamp(0.0, 1.0);
    Some(raw.clamp(0.0, 1.0))
}

/// Dot product of two 3-vectors.
fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// Place a spaced scatter of settlements. Rank habitable sites by suitability
/// (descending; ties by ascending `CellId`), then greedily accept a site only
/// if it is at least `min_separation_dot`-far (smaller dot = farther) from
/// every already-placed settlement and its suitability is at least `floor`.
/// The first element is the flagship (global suitability argmax).
pub fn place(sites: &[SiteInput], min_separation_dot: f64, floor: f64) -> Vec<Placement> {
    let mut scored: Vec<(SiteInput, f64)> = sites
        .iter()
        .filter_map(|s| suitability(s).map(|score| (*s, score)))
        .filter(|(_, score)| *score >= floor)
        .collect();
    scored.sort_by(|a, b| b.1.total_cmp(&a.1).then(a.0.cell.0.cmp(&b.0.cell.0)));

    let mut placed: Vec<Placement> = Vec::new();
    for (site, score) in scored {
        let too_close = placed
            .iter()
            .any(|p| dot(p.position, site.position) > min_separation_dot);
        if too_close {
            continue;
        }
        placed.push(Placement {
            cell: site.cell,
            position: site.position,
            suitability: score,
        });
    }
    placed
}
```

- [ ] **Step 4: Wire the module + re-exports**

In `domains/settlement/src/lib.rs` add near the top:

```rust
pub mod placement;

pub use placement::{Placement, SiteInput, place, suitability};
```

- [ ] **Step 5: Gate + commit**

Run: `cargo test -p hornvale-settlement placement && cargo clippy -p hornvale-settlement --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

```bash
git add domains/settlement/src/placement.rs domains/settlement/src/lib.rs
git commit -m "feat(settlement): field-driven placement (suitability + spaced scatter)"
```

---

### Task 4: Tier-1 settlement genesis

**Files:**
- Create: `domains/settlement/src/genesis.rs`
- Modify: `domains/settlement/src/lib.rs` (declare module, add `PLACE`/`biome` handling, new stream label, re-exports; keep tier-0 name/population helpers reachable)

**Interfaces:**
- Consumes: `placement::Placement`; `hornvale_kernel::{World, EntityId, Fact, Value, Seed, Stream, LedgerError}`.
- Produces:
  - New predicates registered by settlement: `CELL_ID` (`"cell-id"`, functional, Number), `LATITUDE` (`"latitude"`, functional, Number), `LONGITUDE` (`"longitude"`, functional, Number). (Settlement continues to register `IS_SETTLEMENT`/`LOCATED_IN`/`POPULATION`; it now ALSO commits `is-place`/`biome`/`name` — those predicates are owned by terrain but committing facts against them from settlement is legal, exactly as the composition root wires domains.)
  - `struct PlacedSettlement { cell: CellId, latitude: f64, longitude: f64, biome: String, name: String, population: u32 }` (the per-cell data the root supplies alongside each `Placement`).
  - `fn genesis(world: &mut World, settlements: &[PlacedSettlement]) -> Result<Vec<EntityId>, LedgerError>` — commits each settlement as its own place entity (in the given order; `[0]` = flagship), returning their entity ids. Replaces the old `genesis(world, home)`.

Note: the old tier-0 `genesis(world, home) -> Result<EntityId, LedgerError>` is **removed**; the composition root supplies names/populations (drawn there via settlement's helpers) so this function is a pure ledger writer. The existing `candidate_names`/name-pick logic is exposed as `pub fn generate_name(seed: Seed, salt: u64) -> String` for the root to call per cell.

- [ ] **Step 1: Write failing tests**

Create `domains/settlement/src/genesis.rs`:

```rust
//! Tier-1 settlement genesis: commit a scatter of generated settlements, each
//! its own place entity tagged with its cell, coordinates, biome, name, and
//! population. The first entry is the flagship. Replaces the tier-0 single
//! hand-fed village.

use hornvale_kernel::{EntityId, Fact, LedgerError, Value, World};

/// The fully-resolved per-cell data the composition root hands to genesis
/// (placement geometry plus the name/biome/population the root drew or read).
#[derive(Debug, Clone, PartialEq)]
pub struct PlacedSettlement {
    /// The Geosphere cell id this settlement sits on.
    pub cell: u32,
    /// Latitude, degrees.
    pub latitude: f64,
    /// Longitude, degrees.
    pub longitude: f64,
    /// Biome name (from the climate field, at the root).
    pub biome: String,
    /// Generated settlement name.
    pub name: String,
    /// Population.
    pub population: u32,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::register_concepts;
    use hornvale_kernel::Seed;

    fn placed(cell: u32, name: &str, pop: u32) -> PlacedSettlement {
        PlacedSettlement {
            cell,
            latitude: 10.0,
            longitude: 20.0,
            biome: "temperate-forest".to_string(),
            name: name.to_string(),
            population: pop,
        }
    }

    #[test]
    fn genesis_commits_each_as_a_place_and_settlement_flagship_first() {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        let ids = genesis(&mut w, &[placed(5, "Bolgna", 300), placed(99, "Zagrak", 40)]).unwrap();
        assert_eq!(ids.len(), 2);
        // Flagship (first) is the first is-settlement fact.
        assert_eq!(w.ledger.find(crate::IS_SETTLEMENT).next().unwrap().subject, ids[0]);
        // Each is both a place and a settlement, tagged with its cell.
        for (id, cell) in ids.iter().zip([5u32, 99]) {
            assert_eq!(w.ledger.text_of(*id, hornvale_kernel::NAME).is_some(), true);
            assert!(matches!(w.ledger.value_of(*id, crate::CELL_ID), Some(Value::Number(n)) if *n as u32 == cell));
            assert!(w.ledger.value_of(*id, crate::genesis::IS_PLACE_TAG).is_some() || true); // is-place committed
        }
        // Population round-trips on the flagship.
        assert!(matches!(w.ledger.value_of(ids[0], crate::POPULATION), Some(Value::Number(n)) if *n as u32 == 300));
    }

    #[test]
    fn empty_placement_commits_nothing() {
        let mut w = World::new(Seed(1));
        register_concepts(&mut w.registry).unwrap();
        assert!(genesis(&mut w, &[]).unwrap().is_empty());
        assert!(w.ledger.find(crate::IS_SETTLEMENT).next().is_none());
    }
}
```

(Drop the placeholder `IS_PLACE_TAG` reference — see Step 3; the test line is illustrative and replaced there.)

- [ ] **Step 2: Run to see it fail**

Run: `cargo test -p hornvale-settlement genesis 2>&1 | tail -6`
Expected: FAIL — `genesis`/`PlacedSettlement`/`CELL_ID` undefined.

- [ ] **Step 3: Implement genesis + new predicates**

Replace the illustrative test line `assert!(w.ledger.value_of(*id, crate::genesis::IS_PLACE_TAG)...)` with:

```rust
            assert!(matches!(w.ledger.value_of(*id, hornvale_terrain_place_predicate()), _)); // see note
```

is wrong — settlement must not import terrain. Instead assert the biome text directly:

```rust
            assert_eq!(w.ledger.text_of(*id, crate::BIOME).as_deref(), Some("temperate-forest"));
```

and ensure `crate::BIOME`/`crate::IS_PLACE` constants exist in settlement (add them mirroring terrain's string values — the predicate *keys* are shared strings, registered by terrain; settlement references the same string constants). In `domains/settlement/src/lib.rs`:

```rust
/// Predicate key marking an entity a traversable place (owned/registered by
/// terrain; settlement commits facts against the same key for generated cells).
pub const IS_PLACE: &str = "is-place";
/// Predicate key giving a place's biome (shared key; see `IS_PLACE`).
pub const BIOME: &str = "biome";
/// Predicate: the Geosphere cell id a settlement sits on (functional, Number).
pub const CELL_ID: &str = "cell-id";
/// Predicate: latitude of a settlement, degrees (functional, Number).
pub const LATITUDE: &str = "latitude";
/// Predicate: longitude of a settlement, degrees (functional, Number).
pub const LONGITUDE: &str = "longitude";
```

Extend `register_concepts` to register `CELL_ID`, `LATITUDE`, `LONGITUDE` (functional Number). Do **not** re-register `IS_PLACE`/`BIOME` here (terrain registers them; double registration errors) — settlement only commits facts against them.

Implement in `genesis.rs`:

```rust
fn fact(subject: EntityId, predicate: &str, object: Value) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: Some(subject),
        day: Some(0.0),
        provenance: "settlement".to_string(),
    }
}

/// Commit a scatter of generated settlements, each its own place entity. The
/// first entry becomes the flagship (first `is-settlement` fact). Returns the
/// minted entity ids in the given order.
pub fn genesis(world: &mut World, settlements: &[PlacedSettlement]) -> Result<Vec<EntityId>, LedgerError> {
    let mut ids = Vec::with_capacity(settlements.len());
    for s in settlements {
        let id = world.ledger.mint_entity();
        world.ledger.commit(fact(id, hornvale_kernel::NAME, Value::Text(s.name.clone())), &world.registry)?;
        world.ledger.commit(fact(id, crate::IS_PLACE, Value::Flag(true)), &world.registry)?;
        world.ledger.commit(fact(id, crate::BIOME, Value::Text(s.biome.clone())), &world.registry)?;
        world.ledger.commit(fact(id, crate::IS_SETTLEMENT, Value::Flag(true)), &world.registry)?;
        world.ledger.commit(fact(id, crate::POPULATION, Value::Number(f64::from(s.population))), &world.registry)?;
        world.ledger.commit(fact(id, crate::CELL_ID, Value::Number(f64::from(s.cell))), &world.registry)?;
        world.ledger.commit(fact(id, crate::LATITUDE, Value::Number(s.latitude)), &world.registry)?;
        world.ledger.commit(fact(id, crate::LONGITUDE, Value::Number(s.longitude)), &world.registry)?;
        ids.push(id);
    }
    Ok(ids)
}
```

Expose the name generator for the root. In `domains/settlement/src/lib.rs`, refactor the existing tier-0 `candidate_names`/pick into:

```rust
/// Generate one settlement name deterministically from the world seed and a
/// per-settlement salt (e.g. the cell id), 2–3 capitalized goblin syllables.
pub fn generate_name(seed: hornvale_kernel::Seed, salt: u64) -> String {
    let mut stream = seed
        .derive(streams::ROOT)
        .derive(streams::NAME)
        .derive_index(salt)
        .stream();
    let count = stream.range_u32(2, 3);
    let raw: String = (0..count)
        .map(|_| *stream.pick(&SYLLABLES).expect("SYLLABLES is non-empty"))
        .collect();
    format!("{}{}", raw[..1].to_uppercase(), &raw[1..])
}

/// Draw a population against a carrying capacity from suitability, seeded per
/// settlement: `floor + (span × suitability)` jittered by a per-salt draw.
pub fn draw_population(seed: hornvale_kernel::Seed, salt: u64, suitability: f64) -> u32 {
    let mut stream = seed
        .derive(streams::ROOT)
        .derive(streams::PLACEMENT)
        .derive_index(salt)
        .stream();
    let base = 40.0 + 460.0 * suitability.clamp(0.0, 1.0); // 40..500 by suitability
    let jitter = 0.75 + 0.5 * stream.next_f64(); // ×[0.75, 1.25]
    (base * jitter).round() as u32
}
```

Add the `PLACEMENT` label to `streams` and `stream_labels()`:

```rust
    /// Per-settlement placement draws (population against carrying capacity).
    pub const PLACEMENT: &str = "placement";
```
```rust
        ("settlement/placement", "per-settlement population against carrying capacity"),
```

> **Interface check:** confirm `Seed` exposes `derive_index(u64)` (a seeded child by integer). If the kernel's `Seed` only has `derive(&str)`, use `seed.derive(streams::ROOT).derive(streams::NAME).derive(&salt.to_string())` instead, and likewise for `PLACEMENT` — pick whichever the merged kernel provides and use it consistently. Grep `kernel/src/seed.rs` for `pub fn derive` before writing.

Remove the old `pub fn genesis(world, home)` and its tier-0 name/pick body (superseded). Update the settlement lib tests that called the old `genesis(&mut w, home)` to build a `PlacedSettlement` and call the new `genesis(&mut w, &[...])`, asserting the same name/population invariants.

- [ ] **Step 4: Gate + commit**

Run: `cargo test -p hornvale-settlement && cargo clippy -p hornvale-settlement --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

```bash
git add domains/settlement/src/genesis.rs domains/settlement/src/lib.rs
git commit -m "feat(settlement): tier-1 genesis — settlements as cell-tagged place entities"
```

---

### Task 5: Composition-root rewiring (retire the Vale)

**Files:**
- Modify: `windows/worldgen/src/lib.rs`

**Interfaces:**
- Consumes: `climate_of`, `terrain_of` (merged), `settlement::{SiteInput, place, generate_name, draw_population, PlacedSettlement, genesis}`, `climate::habitability`/`biome_at`, `terrain::{drainage_at, is_endorheic}`.
- Produces: rewired `build_world` (no Vale genesis); `fn settlement_lines(world: &World) -> Vec<String>` for the almanac; `AlmanacContext.settlement_lines`.

Placement inputs at the root (per cell): `habitable` = `climate.habitability().get(cell)`; `freshwater` = `(drainage_norm).max(coastal as f64).max(moisture)` clamped, where `drainage_norm = (drainage_at(cell) / DRAINAGE_REF).min(1.0)` with `DRAINAGE_REF = 200.0`; `coastal` = any neighbor is ocean; `temperature_c` = `climate.mean_temperature_at(cell)`; `hostility` = `(unrest_at(cell)).max(aridity)` where `aridity = (0.2 - moisture).max(0.0) * 5.0` clamped. Constants live at the root.

- [ ] **Step 1: Write failing tests**

Add to the `tests` module of `windows/worldgen/src/lib.rs`:

```rust
#[test]
fn build_world_generates_settlements_and_no_vale() {
    let world = generated(42);
    let places = hornvale_terrain::places(&world);
    assert!(places.len() >= 3, "expected a scatter of settlements, got {}", places.len());
    assert!(!places.iter().any(|p| p.name == "the Vale"), "the Vale must be retired");
    // The flagship carries a settlement + population.
    let village = hornvale_settlement::village_info(&world).expect("flagship settlement");
    assert!(village.population >= 40);
    // The cascade still runs on the flagship.
    assert_eq!(
        hornvale_culture::castes_of(&world, village.id).len(),
        hornvale_culture::CASTES.len()
    );
    assert!(!hornvale_religion::beliefs_of(&world).is_empty());
}

#[test]
fn settlements_reorganize_between_spinning_and_locked() {
    use hornvale_astronomy::RotationPin;
    let spinning = generated(42);
    let locked = build_world(
        Seed(42),
        &SkyPins { rotation: Some(RotationPin::Locked), ..SkyPins::default() },
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
    ).unwrap();
    let cells = |w: &World| hornvale_terrain::places(w).iter().map(|p| p.name.clone()).collect::<Vec<_>>();
    assert_ne!(cells(&spinning), cells(&locked), "people must reorganize under a different sky");
}

#[test]
fn settlement_lines_describe_the_people() {
    let lines = settlement_lines(&generated(42)).unwrap();
    assert!(!lines.is_empty());
    assert!(lines.iter().any(|l| l.contains("settlement") || l.contains("village")));
}
```

- [ ] **Step 2: Run to see it fail**

Run: `cargo test -p hornvale-worldgen build_world_generates_settlements 2>&1 | tail -8`
Expected: FAIL (still builds the Vale / `settlement_lines` undefined).

- [ ] **Step 3: Rewire `build_world`**

Replace the tail of `build_world` (the `let vale = ...; let village = ...; culture; religion;` block) with:

```rust
    // Reconstruct terrain + climate, assemble per-cell site inputs, place a
    // spaced scatter of settlements, and commit each as its own place entity.
    let terrain = terrain_of(&world)?;
    let climate = climate_of(&world)?;
    let geo = terrain.geosphere();
    const DRAINAGE_REF: f64 = 200.0;
    let sites: Vec<hornvale_settlement::SiteInput> = geo
        .cells()
        .map(|cell| {
            let coastal = geo.neighbors(cell).iter().any(|n| terrain.is_ocean(*n));
            let moisture = climate.moisture_at(cell);
            let drainage_norm = (terrain.drainage_at(cell) / DRAINAGE_REF).min(1.0);
            let freshwater = drainage_norm.max(if coastal { 1.0 } else { 0.0 }).max(moisture).clamp(0.0, 1.0);
            let aridity = ((0.2 - moisture).max(0.0) * 5.0).clamp(0.0, 1.0);
            let hostility = terrain.unrest_at(cell).max(aridity).clamp(0.0, 1.0);
            hornvale_settlement::SiteInput {
                cell,
                position: geo.position(cell),
                habitable: *climate.habitability().get(cell),
                freshwater,
                coastal,
                temperature_c: climate.mean_temperature_at(cell),
                hostility,
            }
        })
        .collect();
    let min_sep = (12.0_f64.to_radians()).cos();
    let placements = hornvale_settlement::place(&sites, min_sep, 0.25);
    let placed: Vec<hornvale_settlement::PlacedSettlement> = placements
        .iter()
        .map(|p| {
            let coord = geo.coord(p.cell);
            hornvale_settlement::PlacedSettlement {
                cell: p.cell.0,
                latitude: coord.latitude,
                longitude: coord.longitude,
                biome: climate.biome_at(p.cell).name().to_string(),
                name: hornvale_settlement::generate_name(seed, u64::from(p.cell.0)),
                population: hornvale_settlement::draw_population(seed, u64::from(p.cell.0), p.suitability),
            }
        })
        .collect();
    let ids = hornvale_settlement::genesis(&mut world, &placed)?;
    if let Some(&flagship) = ids.first() {
        hornvale_culture::genesis(&mut world, flagship)?;
        let seen = observed_phenomena(&world, 0.0)?;
        hornvale_religion::genesis(&mut world, flagship, &seen)?;
    }
    Ok(world)
```

Remove the now-unused `hornvale_terrain::genesis` call. (`observed_phenomena` already uses `places(world).first()` — now the flagship — so it needs no change.)

- [ ] **Step 4: Add `settlement_lines` + almanac field**

Add to `windows/worldgen/src/lib.rs`:

```rust
/// Headline lines describing the world's people for the almanac: how many
/// settlements, and the flagship's name, population, and biome.
pub fn settlement_lines(world: &World) -> Result<Vec<String>, BuildError> {
    let places = hornvale_terrain::places(world);
    let mut lines = vec![format!(
        "The land holds {} settlement(s).",
        places.iter().filter(|_| true).count()
    )];
    if let Some(v) = hornvale_settlement::village_info(world) {
        let biome = places
            .iter()
            .find(|p| p.id == v.id)
            .map(|p| p.biome.clone())
            .unwrap_or_else(|| "unknown".to_string());
        lines.push(format!(
            "The chief settlement, {}, holds {} souls amid {}.",
            v.name, v.population, biome
        ));
    }
    Ok(lines)
}
```

Add `settlement_lines: Vec<String>` to `AlmanacContext` in `windows/almanac/src/lib.rs`, render it under "The People" (before the village block, or replacing the hardcoded goblin-village line — see Task 6), and populate `settlement_lines: settlement_lines(world)?` in `almanac_context`. Update the almanac `sample_context()` helper with `settlement_lines: vec![]`.

- [ ] **Step 5: Fix the fallout across the workspace**

The Vale's retirement breaks tests/fixtures that assumed it. Sweep and update:
- `windows/worldgen/src/lib.rs` tests: `build_world_produces_the_full_cascade` (assert ≥1 place + flagship village), `almanac_context_gathers_everything` (places non-empty still holds).
- `cli/src/repl.rs` tests: `facts` / `places` / `map`+`land` tests that referenced "the Vale" or a specific entity id — retarget to the flagship (query `village_info`), don't hardcode entity numbers.
- `windows/lab/src/metrics.rs`: `WorldView::build` already builds via `build_world`; confirm nothing asserts the Vale. No new metrics in 4a.

Run the whole suite and fix each failure to reflect generated settlements (never by weakening an assertion to trivial):

Run: `cargo test --workspace 2>&1 | grep -E "FAILED|test result" | tail -30`

- [ ] **Step 6: Full gate + commit**

Run: `cargo test --workspace && cargo clippy --workspace --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

```bash
git add windows/worldgen/src/lib.rs windows/almanac/src/lib.rs cli/src/repl.rs windows/lab/src/metrics.rs
git commit -m "feat(worldgen): generate settlements from fields; retire the hand-placed Vale"
```

---

### Task 6: The settlement-map artifact + REPL/CLI surfacing

**Files:**
- Create: `domains/settlement/src/render.rs`
- Modify: `domains/settlement/src/lib.rs` (`pub mod render;`)
- Modify: `cli/src/main.rs` (`settlement-map` command)
- Modify: `cli/src/repl.rs` (`settlements`, `settlement <lat> <lon>`)
- Modify: `windows/almanac/src/lib.rs` (People section, if not fully done in Task 5)

**Interfaces:**
- Consumes: a base biome `CellMap<Biome>`-colored map is climate's; settlement overlays marks. To keep settlement kernel-only, the renderer takes a **base PPM** (bytes) + settlement pixel coordinates and stamps marks — OR renders an ASCII overlay from a `&[(f64, f64)]` (lat/lon) list. Simplest kernel-only design: `fn settlement_ascii(width: u32, height: u32, sites: &[(f64, f64)], flagship: Option<(f64, f64)>) -> String` producing a lon/lat grid with `o` per settlement and `@` for the flagship; and `fn overlay_ppm(base: &[u8], width: u32, height: u32, sites: &[(f64, f64)], flagship: Option<(f64, f64)>) -> Vec<u8>` that stamps mark pixels onto a copy of `base` (the biome PPM produced by climate at the root).
- Produces: the two render functions; the CLI `settlement-map` command writes markdown (title + `settlement_lines` + ASCII overlay) and, with `--out`, the overlaid PPM.

- [ ] **Step 1: Write failing render tests**

Create `domains/settlement/src/render.rs` with the module doc, the two functions' signatures, and tests asserting: `settlement_ascii` has `height` lines of `width` chars, contains `@` when a flagship is given and `o` for others, and is deterministic; `overlay_ppm` returns a buffer the same length as `base` and changes at least the flagship pixel. (Full code: mirror `hornvale_climate::render`'s equirectangular projection — longitude −180→180 across, latitude 90→−90 down — to map each (lat, lon) to a pixel; stamp a small mark.)

- [ ] **Step 2–4: Implement, run, verify.** Equirectangular pixel mapping: `px = ((lon + 180)/360 * width)`, `py = ((90 - lat)/180 * height)`, clamped. ASCII: build a `height×width` grid of spaces, then set settlement cells to `o` and the flagship to `@`. PPM overlay: copy `base`, compute each mark's byte offset (`header_len + (py*width+px)*3`), write a high-contrast RGB (e.g. `[255, 0, 0]` for settlements, `[255, 255, 0]` for the flagship) — the header length is the bytes up to and including the third newline of the `P6\n{w} {h}\n255\n` header; compute it by finding the third `\n`.

Run: `cargo test -p hornvale-settlement render && cargo clippy -p hornvale-settlement --all-targets -- -D warnings`. `cargo fmt`.

- [ ] **Step 5: CLI `settlement-map` command**

In `cli/src/main.rs`: add `Some("settlement-map") => cmd_settlement_map(&args),`, a usage line, and (mirroring `cmd_biome_map`) a `cmd_settlement_map` that loads the world, builds `climate_of` for the base biome PPM (`hornvale_climate::render::biome_ppm`), collects settlement lat/lons from `hornvale_terrain::places` + their `LATITUDE`/`LONGITUDE` facts (the flagship = `village_info`), prints markdown (`# The Peoples of Seed N`, `settlement_lines`, the ASCII overlay), and with `--out` writes `overlay_ppm(&biome_ppm, ...)`. Add a `usage_mentions_settlement_map` test.

- [ ] **Step 6: REPL `settlements` / `settlement <lat> <lon>`**

In `cli/src/repl.rs`: extend `HELP`; add a `settlements` arm listing each place's name + population + biome; add a `settlement <lat> <lon>` arm using `terrain_of(world).nearest_cell(lat, lon)` and reporting whichever settlement sits nearest (or "no settlement on this cell"). Add a REPL test driving both.

- [ ] **Step 7: Gate + commit**

Run: `cargo test --workspace && cargo clippy --workspace --all-targets -- -D warnings`. `cargo fmt`.

```bash
git add domains/settlement/src/render.rs domains/settlement/src/lib.rs cli/src/main.rs cli/src/repl.rs windows/almanac/src/lib.rs
git commit -m "feat(cli): settlement-map artifact + REPL settlement queries"
```

---

### Task 7: CI, artifact regeneration, and the 4a chronicle

**Files:**
- Modify: `.github/workflows/ci.yml`
- Modify: `cli/src/streams.rs` (manifest test asserts the new label)
- Create: `book/src/gallery/settlement-seed-42.md` / `.ppm` (generated)
- Create: `book/src/chronicle/campaign-4a.md`
- Modify: `book/src/SUMMARY.md`
- Regenerate: `book/src/gallery/almanac-*.md`, `book/src/reference/*-generated.md`

- [ ] **Step 1: Manifest test.** In `cli/src/streams.rs` tests, assert the manifest contains `` | `settlement/placement` | ``. Run: `cargo test -p hornvale streams` (or the manifest test's crate). 

- [ ] **Step 2: CI drift check.** In `.github/workflows/ci.yml`, after the biome-map renders, add:

```yaml
          cargo run -p hornvale -- settlement-map --world /tmp/hv-ci-sky.json --out book/src/gallery/settlement-seed-42.ppm > book/src/gallery/settlement-seed-42.md
          cargo run -p hornvale -- settlement-map --world /tmp/hv-ci-locked.json --out book/src/gallery/settlement-seed-42-locked.ppm > book/src/gallery/settlement-seed-42-locked.md
```

(keep the existing `census-lands-drift` run and the `git diff --exit-code` net; the paths are already covered).

- [ ] **Step 3: Regenerate every committed artifact, stdout-only.** Run the exact CI block locally with `cargo run -q` and stdout redirects (cargo logs go to stderr — never let them leak into a committed file). Then confirm `grep -rlE "Compiling|Finished \`|Running \`target" book/src` is empty and, after a second regeneration, `git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/` is clean (byte-identical).

- [ ] **Step 4: Chronicle + SUMMARY.** Write `book/src/chronicle/campaign-4a.md` (the people arrive: field-driven placement, the drainage skeleton, the flagship succeeding the Vale, the settlement map; note culture stays tier-0 until 4b). Add to `book/src/SUMMARY.md`: gallery entries for the settlement maps and a chronicle entry `- [Campaign 4a: Placement & Drainage](./chronicle/campaign-4a.md)`.

- [ ] **Step 5: Full gate + book build + commit.**

Run: `mdbook build book && cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`, then the drift check. All green.

```bash
git add .github/workflows/ci.yml cli/src/streams.rs book/
git commit -m "docs(book): 4a chronicle, settlement-map gallery, CI drift; regenerate artifacts"
```

---

## Self-Review Notes

**Spec coverage (against §4, §5, §8, §13 of the C4 design):**
- §4 drainage (flow accumulation + endorheic + freshwater) → Tasks 1–2 (field/globe/provider) + Task 5 (freshwater assembled at root). §5 placement (suitability, greedy spacing, per-settlement name/population, flagship) → Tasks 3–4 + Task 5 wiring. §8 integration (Vale retires; settlement becomes the place; religion tier-0 on flagship) → Tasks 4–5. §13 staging (4a = placement + drainage + flagship + artifact; culture stays tier-0) → this whole plan; Census of Peoples + emergent culture explicitly deferred to 4b. Artifact + REPL/almanac → Task 6. DoD chronicle + drift → Task 7.

**Deferred to 4b (not this plan):** subsistence/emergent culture, Census of Peoples metrics + subsistence calibration, settlement pins, the tier-1 culture/settlement book chapters (4a ships a chronicle entry only).

**Type consistency:** `SiteInput`/`Placement`/`place`/`suitability` (Task 3) are consumed with the same names in Task 5's root wiring; `PlacedSettlement`/`genesis`/`generate_name`/`draw_population` (Task 4) likewise. `CELL_ID`/`LATITUDE`/`LONGITUDE` keys defined in Task 4 are read by Task 6's CLI. `drainage_at`/`is_endorheic` (Task 2) consumed in Task 5.

**Open interface check flagged for the implementer:** Task 4 depends on how the kernel `Seed` spawns a per-salt child (`derive_index(u64)` vs `derive(&str)`) — grep `kernel/src/seed.rs` and use whichever exists, consistently. This is the one place the plan cannot fix a name without seeing the merged kernel; it is called out at the task.

**Watch items (carried lessons):** verify test counts directly after each task (baseline 361); regenerate artifacts stdout-only; the Vale retirement (Task 5 Step 5) ripples into repl/worldgen/almanac tests — fix each honestly, never by trivializing an assertion; keep `#[allow(clippy::too_many_arguments)]` only where a physical/parameter function genuinely needs it.
