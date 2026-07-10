# The Measured Coast (Campaign 25) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Shape metrics + a 10,000-seed Census of Coasts (the terrain overhaul's preregistered baseline), and render-time coastline refinement — with zero save-format impact: world JSON, almanacs, and every stream draw stay byte-identical.

**Architecture:** Shape geometry lives in a new `domains/terrain/src/shape.rs` module of pure functions over narrow inputs (`Geosphere`, `CellMap<f64>`, sea level); the lab registers six thin metrics over them. The render refinement is terrain-domain-local: per-pixel Gaussian-weighted interpolation over the nearest cell and its neighbors, plus bounded seeded hash-noise displacement near sea level. Spec: `docs/superpowers/specs/2026-07-09-the-measured-coast-design.md`.

**Tech Stack:** Rust edition 2024, `serde`/`serde_json` only. Kernel primitives: `Geosphere`, `CellMap`, `NearestCellIndex`, `Seed::derive`, `noise::fbm_2d`, `png::encode_rgb`.

## Global Constraints

- Full gate on every commit: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`. Run `cargo fmt` (no `--check`) before each commit.
- No `HashMap`/`HashSet` (clippy-enforced): `BTreeMap`/`BTreeSet`/`Vec` only. Float sorting uses `total_cmp`.
- No new crates. No wall-clock time.
- Every public item, field, and variant gets a one-line doc comment (`#![warn(missing_docs)]` is on).
- **Zero save-format impact** (spec §1): no `Stream` may be created or consumed anywhere in this campaign. The one new seed label (`terrain/coast-render`) feeds stateless hash-noise only.
- Land convention: a cell is land iff `elevation >= sea_level` (ocean is *strictly below* — matches `globe.rs` and `derive_sea_level`).
- Regenerate drifting committed artifacts **in the same commit** that changes them (the CI "Artifacts are current" step is the authority; `.github/workflows/ci.yml` lines 25–49).
- End every commit message with:
  `Claude-Session: https://claude.ai/code/session_01E3KL1pn5171CwscVR2W7fT`

---

### Task 1: World-identity fixture guard (lens purity)

Commit the seed-42 world JSON as a fixture **before any other change**, with a test asserting byte-equality. Every later task's `cargo test` run then proves it didn't move world identity.

**Files:**
- Create: `cli/tests/lens_purity.rs`
- Create: `cli/tests/fixtures/world-seed-42.json` (generated, committed)

**Interfaces:**
- Consumes: `hornvale_worldgen::build_world_with_roster(Seed, &SkyPins, SkyChoice, &TerrainPins, &SettlementPins, &[SpeciesDef]) -> Result<World, BuildError>`, `hornvale_worldgen::default_roster()`, `World::to_json() -> String` (kernel).
- Produces: nothing later tasks call; a standing guard.

- [ ] **Step 1: Check how `cmd_new` builds and serializes**

Read `fn cmd_new` in `cli/src/main.rs`. Confirm (a) the exact build call and its default pin/choice arguments for a plain `new --seed 42` (no flags), and (b) that the file is written with `World::to_json()`. The test below must mirror both exactly; if the defaults differ from what's shown in Step 3, use `cmd_new`'s actual arguments.

- [ ] **Step 2: Generate the fixture**

```bash
mkdir -p cli/tests/fixtures
cargo run -p hornvale -- new --seed 42 --out cli/tests/fixtures/world-seed-42.json
```

- [ ] **Step 3: Write the test**

```rust
//! World-identity guard: the seed-42 default world's JSON is a committed
//! fixture. If this test fails, world identity drifted — that is a terrain
//! or sky epoch and must be deliberate: regenerate the fixture in the same
//! commit and record why in the chronicle. Campaign 25 (The Measured
//! Coast) is contractually lens-only; under it this test must never fail.

use hornvale_kernel::Seed;
use hornvale_worldgen::{SkyChoice, build_world_with_roster, default_roster};

#[test]
fn seed_42_world_json_matches_the_committed_fixture() {
    let world = build_world_with_roster(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
        &default_roster(),
    )
    .expect("seed 42 builds");
    let fixture = include_str!("fixtures/world-seed-42.json");
    assert_eq!(
        world.to_json(),
        fixture,
        "world identity drifted from the committed fixture — see this file's module doc"
    );
}
```

If `cli/Cargo.toml` doesn't already expose the needed crates to integration tests (check `[dependencies]` — `architecture.rs` already exercises some), add the missing ones as existing-workspace path dependencies only (no new external crates).

- [ ] **Step 4: Run the test — it must pass immediately**

Run: `cargo test -p hornvale --test lens_purity`
Expected: PASS. If it fails, the build call doesn't mirror `cmd_new` — fix the test's arguments (never the fixture) until generated bytes match, e.g. trailing newline: if `cmd_new` appends `\n` when writing, compare `format!("{}\n", world.to_json())` — match whatever `new` actually wrote.

- [ ] **Step 5: Commit**

```bash
cargo fmt && git add cli/tests/ && git commit -m "test(cli): world-identity fixture guard for the lens-only campaign

Campaign 25 (Measured Coast) spec §4: world JSON byte-identity is
asserted in-test, not just by CI drift. Committed before any code
change so every later task's test run proves lens purity."
```

---

### Task 2: Shape geometry I — shoreline development and shelf fraction

**Files:**
- Create: `domains/terrain/src/shape.rs`
- Modify: `domains/terrain/src/lib.rs` (add `pub mod shape;` alongside the existing module declarations)

**Interfaces:**
- Consumes: kernel `Geosphere` (`cells()`, `neighbors()`, `position()`, `cell_count()`), `CellMap<f64>` (`get()`, `iter()`, `len()`).
- Produces (Tasks 3–4 rely on these exact signatures):
  - `pub fn shoreline_development(geo: &Geosphere, elevation: &CellMap<f64>, sea_level: f64) -> Option<f64>`
  - `pub fn shelf_fraction(elevation: &CellMap<f64>, sea_level: f64) -> f64`
  - `pub const SHELF_BAND_M: f64`

- [ ] **Step 1: Write the failing tests** (bottom of the new `shape.rs`)

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{CellMap, Geosphere};

    /// Land where the cell's z-coordinate clears `z_min` — a polar cap.
    fn cap_elevation(geo: &Geosphere, z_min: f64) -> CellMap<f64> {
        CellMap::from_fn(geo, |c| if geo.position(c)[2] >= z_min { 100.0 } else { -100.0 })
    }

    #[test]
    fn a_compact_cap_scores_near_one_and_stripes_score_higher() {
        let geo = Geosphere::new(3);
        let cap = cap_elevation(&geo, 0.5);
        let d_cap = shoreline_development(&geo, &cap, 0.0).expect("cap has a shoreline");
        assert!((0.6..=1.4).contains(&d_cap), "compact cap D = {d_cap}");
        // Same latitude band, but land only in alternating longitude sectors:
        // far more shoreline for less area.
        let stripes = CellMap::from_fn(&geo, |c| {
            let p = geo.position(c);
            let sector = ((p[1].atan2(p[0]) + std::f64::consts::PI)
                / (std::f64::consts::PI / 6.0)) as i64;
            if p[2] >= 0.5 && sector % 2 == 0 { 100.0 } else { -100.0 }
        });
        let d_stripes = shoreline_development(&geo, &stripes, 0.0).expect("stripes shoreline");
        assert!(d_stripes > d_cap, "stripes {d_stripes} <= cap {d_cap}");
    }

    #[test]
    fn worlds_without_a_shoreline_are_absent() {
        let geo = Geosphere::new(2);
        let all_land = CellMap::from_fn(&geo, |_| 100.0);
        let all_ocean = CellMap::from_fn(&geo, |_| -100.0);
        assert_eq!(shoreline_development(&geo, &all_land, 0.0), None);
        assert_eq!(shoreline_development(&geo, &all_ocean, 0.0), None);
    }

    #[test]
    fn shelf_fraction_counts_the_band_around_sea_level() {
        let geo = Geosphere::new(3);
        // Elevation = 1000·z: the ±200 m band is |z| <= 0.2, ~20% of a
        // sphere by area (z is area-uniform).
        let e = CellMap::from_fn(&geo, |c| 1000.0 * geo.position(c)[2]);
        let f = shelf_fraction(&e, 0.0);
        assert!((0.12..=0.28).contains(&f), "shelf fraction {f}");
        let flat = CellMap::from_fn(&geo, |_| 50.0);
        assert_eq!(shelf_fraction(&flat, 0.0), 1.0);
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-terrain shape`
Expected: FAIL — module `shape` does not exist / functions not found.

- [ ] **Step 3: Implement**

```rust
//! Shape metrics over a generated globe: the quantitative "before
//! photograph" for the terrain overhaul (Campaign 25 spec §2). Pure
//! functions over narrow inputs so tests build synthetic globes cheaply.
//! Discrete estimators (documented per function) are declared
//! approximations; each is deterministic and consistent across metrics.

use hornvale_kernel::{CellMap, Geosphere};

/// Half-width of the shelf band around sea level, meters (Earth's
/// continental shelf lies within ~200 m of the sea surface).
pub const SHELF_BAND_M: f64 = 200.0;

/// Angular distance between two unit vectors, radians.
fn angle(a: [f64; 3], b: [f64; 3]) -> f64 {
    (a[0] * b[0] + a[1] * b[1] + a[2] * b[2]).clamp(-1.0, 1.0).acos()
}

/// Shoreline development index `D = L / (2 sqrt(pi A))`: coastline length
/// over the circumference of the circle with the land's area. 1 is
/// maximally compact; fjorded coasts score several times that. Estimators:
/// cell area is the equal-area approximation `4 pi / N`; the shared edge
/// between two neighboring cells is approximated as their center distance
/// over sqrt(3) (the regular-hexagon dual). `None` when the globe has no
/// land or no shoreline.
pub fn shoreline_development(
    geo: &Geosphere,
    elevation: &CellMap<f64>,
    sea_level: f64,
) -> Option<f64> {
    let cell_area = 4.0 * std::f64::consts::PI / geo.cell_count() as f64;
    let mut land_area = 0.0;
    let mut shoreline = 0.0;
    for cell in geo.cells() {
        let land = *elevation.get(cell) >= sea_level;
        if land {
            land_area += cell_area;
        }
        for &neighbor in geo.neighbors(cell) {
            // Each unordered pair once.
            if neighbor.0 <= cell.0 {
                continue;
            }
            let neighbor_land = *elevation.get(neighbor) >= sea_level;
            if land != neighbor_land {
                shoreline +=
                    angle(geo.position(cell), geo.position(neighbor)) / 3f64.sqrt();
            }
        }
    }
    if land_area == 0.0 || shoreline == 0.0 {
        return None;
    }
    Some(shoreline / (2.0 * (std::f64::consts::PI * land_area).sqrt()))
}

/// Fraction of all cells within [`SHELF_BAND_M`] of sea level — Earth's
/// hypsometry keeps a populated shelf here; a cliff-coast generator does
/// not.
pub fn shelf_fraction(elevation: &CellMap<f64>, sea_level: f64) -> f64 {
    let within = elevation
        .iter()
        .filter(|(_, e)| (**e - sea_level).abs() <= SHELF_BAND_M)
        .count();
    within as f64 / elevation.len() as f64
}
```

And in `domains/terrain/src/lib.rs`, next to the other `pub mod` lines: `pub mod shape;`.

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-terrain shape`
Expected: PASS (3 tests). If the cap bound misses, loosen only after checking the failure is level-3 discretization (print `d_cap`), not an estimator bug.

- [ ] **Step 5: Commit**

```bash
cargo fmt && git add domains/terrain/src/ && git commit -m "feat(terrain): shape module — shoreline development + shelf fraction"
```

---

### Task 3: Shape geometry II — bimodality, land components, Gini

**Files:**
- Modify: `domains/terrain/src/shape.rs`

**Interfaces:**
- Produces (Task 4 relies on these exact signatures):
  - `pub fn hypsometric_bimodality(elevation: &CellMap<f64>, sea_level: f64) -> Option<f64>`
  - `pub fn land_component_sizes(geo: &Geosphere, elevation: &CellMap<f64>, sea_level: f64) -> Vec<usize>` (descending)
  - `pub fn gini(counts: &[usize]) -> Option<f64>`

- [ ] **Step 1: Write the failing tests** (append inside `mod tests`)

```rust
    #[test]
    fn separated_modes_score_higher_than_a_split_unimodal_field() {
        let geo = Geosphere::new(3);
        // Earth-like: two tight modes far apart (tiny within-mode spread so
        // Ashman's denominator is nonzero).
        let bimodal = CellMap::from_fn(&geo, |c| {
            let z = geo.position(c)[2];
            if z >= 0.0 { 400.0 + z } else { -4000.0 + z }
        });
        // A single uniform ramp split at sea level.
        let unimodal = CellMap::from_fn(&geo, |c| 100.0 * geo.position(c)[2]);
        let d_bi = hypsometric_bimodality(&bimodal, 0.0).expect("bimodal");
        let d_uni = hypsometric_bimodality(&unimodal, 0.0).expect("unimodal");
        assert!(d_bi > 10.0 * d_uni, "bimodal {d_bi} vs unimodal {d_uni}");
        let all_land = CellMap::from_fn(&geo, |_| 100.0);
        assert_eq!(hypsometric_bimodality(&all_land, 0.0), None);
    }

    #[test]
    fn antipodal_caps_are_two_components_sorted_descending() {
        let geo = Geosphere::new(3);
        // North cap bigger than south cap.
        let e = CellMap::from_fn(&geo, |c| {
            let z = geo.position(c)[2];
            if z >= 0.5 || z <= -0.8 { 100.0 } else { -100.0 }
        });
        let sizes = land_component_sizes(&geo, &e, 0.0);
        assert_eq!(sizes.len(), 2, "components: {sizes:?}");
        assert!(sizes[0] > sizes[1]);
        let ocean = CellMap::from_fn(&geo, |_| -1.0);
        assert!(land_component_sizes(&geo, &ocean, 0.0).is_empty());
    }

    #[test]
    fn gini_is_zero_for_equal_counts_and_high_for_concentration() {
        assert_eq!(gini(&[5, 5, 5, 5]), Some(0.0));
        let g = gini(&[0, 0, 0, 10]).expect("nonzero total");
        assert!((g - 0.75).abs() < 1e-12, "gini {g}");
        assert_eq!(gini(&[]), None);
        assert_eq!(gini(&[0, 0]), None);
    }
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-terrain shape`
Expected: FAIL — functions not found.

- [ ] **Step 3: Implement** (append to `shape.rs`; add `use std::collections::VecDeque;` at the top)

```rust
/// Ashman's D between the land and ocean elevation populations:
/// `|mean_land - mean_ocean| / sqrt((var_land + var_ocean) / 2)` with
/// population variance. Earth's hypsometry is strongly bimodal (high D).
/// `None` when either population is empty or both are degenerate
/// (zero variance).
pub fn hypsometric_bimodality(elevation: &CellMap<f64>, sea_level: f64) -> Option<f64> {
    let mut land = Vec::new();
    let mut ocean = Vec::new();
    for (_, e) in elevation.iter() {
        if *e >= sea_level {
            land.push(*e);
        } else {
            ocean.push(*e);
        }
    }
    fn stats(values: &[f64]) -> Option<(f64, f64)> {
        if values.is_empty() {
            return None;
        }
        let n = values.len() as f64;
        let mean = values.iter().sum::<f64>() / n;
        let variance = values.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / n;
        Some((mean, variance))
    }
    let (mean_land, var_land) = stats(&land)?;
    let (mean_ocean, var_ocean) = stats(&ocean)?;
    let denominator = ((var_land + var_ocean) / 2.0).sqrt();
    if denominator <= f64::EPSILON {
        return None;
    }
    Some((mean_land - mean_ocean).abs() / denominator)
}

/// Sizes (cell counts) of connected land components, descending. BFS in
/// ascending cell-id order — fully deterministic. Empty when there is no
/// land.
pub fn land_component_sizes(
    geo: &Geosphere,
    elevation: &CellMap<f64>,
    sea_level: f64,
) -> Vec<usize> {
    let mut visited = vec![false; geo.cell_count()];
    let mut sizes = Vec::new();
    for start in geo.cells() {
        if visited[start.0 as usize] || *elevation.get(start) < sea_level {
            continue;
        }
        visited[start.0 as usize] = true;
        let mut queue = VecDeque::from([start]);
        let mut size = 0usize;
        while let Some(cell) = queue.pop_front() {
            size += 1;
            for &neighbor in geo.neighbors(cell) {
                if !visited[neighbor.0 as usize] && *elevation.get(neighbor) >= sea_level {
                    visited[neighbor.0 as usize] = true;
                    queue.push_back(neighbor);
                }
            }
        }
        sizes.push(size);
    }
    sizes.sort_unstable_by(|a, b| b.cmp(a));
    sizes
}

/// Gini coefficient over nonnegative counts (0 = equal, →1 = concentrated):
/// `G = 2 Σ i·x_i / (n Σ x) − (n+1)/n` over ascending x with 1-based i.
/// `None` for an empty slice or an all-zero total.
pub fn gini(counts: &[usize]) -> Option<f64> {
    if counts.is_empty() {
        return None;
    }
    let total: usize = counts.iter().sum();
    if total == 0 {
        return None;
    }
    let mut sorted = counts.to_vec();
    sorted.sort_unstable();
    let n = sorted.len() as f64;
    let weighted: f64 = sorted
        .iter()
        .enumerate()
        .map(|(i, x)| (i as f64 + 1.0) * *x as f64)
        .sum();
    Some(2.0 * weighted / (n * total as f64) - (n + 1.0) / n)
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-terrain shape`
Expected: PASS (6 tests).

- [ ] **Step 5: Commit**

```bash
cargo fmt && git add domains/terrain/src/shape.rs && git commit -m "feat(terrain): shape module — bimodality, land components, Gini"
```

---

### Task 4: The six lab metrics

**Files:**
- Modify: `windows/lab/src/metrics.rs` (append six entries at the end of the `vec![...]` in `registry()`)
- Regenerate: `book/src/laboratory/generated/census-lands-drift/`, `.../census-of-the-meeting/`, `.../census-of-skies/` (all three CI studies use `"metrics": "all"`, so their committed summaries gain six columns — same-commit regeneration, per the drift discipline)

**Interfaces:**
- Consumes: Task 2–3 signatures; `WorldView` fields `terrain: GeneratedTerrain` with `.globe() -> &TectonicGlobe` and `.geosphere() -> &Geosphere`; `TectonicGlobe` fields `elevation`, `sea_level`, `plate_of`, `plates`.
- Produces: metric names `shoreline-development`, `hypsometric-bimodality`, `shelf-fraction`, `continent-count`, `largest-continent-share`, `plate-size-gini` (Task 5's study JSON names them verbatim).

- [ ] **Step 1: Write the failing test** (in `metrics.rs`'s existing `#[cfg(test)] mod tests` — mimic its existing style)

```rust
    #[test]
    fn shape_metrics_are_present_deterministic_and_sane() {
        let names = [
            "shoreline-development",
            "hypsometric-bimodality",
            "shelf-fraction",
            "continent-count",
            "largest-continent-share",
            "plate-size-gini",
        ];
        let registry = registry();
        let a = WorldView::build(Seed(7), &SkyPins::default()).expect("seed 7");
        let b = WorldView::build(Seed(7), &SkyPins::default()).expect("seed 7 again");
        for name in names {
            let metric = registry
                .iter()
                .find(|m| m.name == name)
                .unwrap_or_else(|| panic!("metric {name} not registered"));
            let va = (metric.extract)(&a);
            assert_eq!(va, (metric.extract)(&b), "{name} not deterministic");
            if let MetricValue::Number(x) = va {
                assert!(x.is_finite(), "{name} not finite: {x}");
            }
        }
    }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-lab shape_metrics`
Expected: FAIL — "metric shoreline-development not registered".

- [ ] **Step 3: Register the metrics** (append before `registry()`'s closing `]`; the current final entry ends around the vec's tail — keep these last so existing summary column orders shift only by appending)

```rust
        Metric {
            name: "shoreline-development",
            doc: "Shoreline development index: coastline length over the \
                  circumference of the circle with the land's area (1 = \
                  maximally compact); Absent without a shoreline",
            summary: SummaryKind::Numeric {
                bucket_edges: &[1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 6.0],
            },
            extract: |v| {
                let globe = v.terrain.globe();
                match hornvale_terrain::shape::shoreline_development(
                    v.terrain.geosphere(),
                    &globe.elevation,
                    globe.sea_level,
                ) {
                    Some(d) => MetricValue::Number(d),
                    None => MetricValue::Absent,
                }
            },
        },
        Metric {
            name: "hypsometric-bimodality",
            doc: "Ashman's D between land and ocean elevation populations \
                  (Earth is strongly bimodal); Absent when a world lacks land \
                  or ocean",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 6.0],
            },
            extract: |v| {
                let globe = v.terrain.globe();
                match hornvale_terrain::shape::hypsometric_bimodality(
                    &globe.elevation,
                    globe.sea_level,
                ) {
                    Some(d) => MetricValue::Number(d),
                    None => MetricValue::Absent,
                }
            },
        },
        Metric {
            name: "shelf-fraction",
            doc: "Fraction of cells within the shelf band (±200 m) of sea \
                  level — the populated shelf Earth's hypsometry keeps",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.02, 0.05, 0.1, 0.15, 0.2, 0.3],
            },
            extract: |v| {
                let globe = v.terrain.globe();
                MetricValue::Number(hornvale_terrain::shape::shelf_fraction(
                    &globe.elevation,
                    globe.sea_level,
                ))
            },
        },
        Metric {
            name: "continent-count",
            doc: "Connected land components",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 6.0, 8.0, 12.0],
            },
            extract: |v| {
                let globe = v.terrain.globe();
                MetricValue::Number(hornvale_terrain::shape::land_component_sizes(
                    v.terrain.geosphere(),
                    &globe.elevation,
                    globe.sea_level,
                )
                .len() as f64)
            },
        },
        Metric {
            name: "largest-continent-share",
            doc: "Largest land component's share of all land cells; Absent \
                  on a landless world",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 0.9],
            },
            extract: |v| {
                let globe = v.terrain.globe();
                let sizes = hornvale_terrain::shape::land_component_sizes(
                    v.terrain.geosphere(),
                    &globe.elevation,
                    globe.sea_level,
                );
                let land: usize = sizes.iter().sum();
                match sizes.first() {
                    Some(largest) if land > 0 => {
                        MetricValue::Number(*largest as f64 / land as f64)
                    }
                    _ => MetricValue::Absent,
                }
            },
        },
        Metric {
            name: "plate-size-gini",
            doc: "Gini coefficient over plate cell counts (Earth's plate \
                  sizes are heavy-tailed; uniform Voronoi scores low)",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6],
            },
            extract: |v| {
                let globe = v.terrain.globe();
                let mut counts = vec![0usize; globe.plates.len()];
                for (_, plate) in globe.plate_of.iter() {
                    counts[*plate as usize] += 1;
                }
                match hornvale_terrain::shape::gini(&counts) {
                    Some(g) => MetricValue::Number(g),
                    None => MetricValue::Absent,
                }
            },
        },
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-lab`
Expected: PASS, including `shape_metrics_are_present_deterministic_and_sane`. Also check other lab tests didn't assume a fixed registry length; fix any count assertion by updating its expected number.

- [ ] **Step 5: Regenerate the three CI-checked study artifacts** (their `"all"` selection now includes the new metrics)

```bash
cargo run -p hornvale -- lab run studies/census-lands-drift.study.json
cargo run -p hornvale -- lab run studies/census-of-the-meeting.study.json
cargo run -p hornvale -- lab run studies/census-of-skies.study.json
git status book/src/laboratory/generated/
```

Expected: modified summaries/charts under all three study directories, nothing else.

- [ ] **Step 6: Commit** (code + regenerated artifacts together)

```bash
cargo fmt && git add windows/lab/src/metrics.rs book/src/laboratory/generated/ && git commit -m "feat(lab): six continental-shape metrics

shoreline-development, hypsometric-bimodality, shelf-fraction,
continent-count, largest-continent-share, plate-size-gini — the
Measured Coast baseline instruments. CI census artifacts regenerated
in-commit ('all'-metric studies gain the new columns)."
```

---

### Task 5: The Census of Coasts

**Files:**
- Create: `studies/census-of-coasts.study.json`
- Create: `book/src/laboratory/study-010.md`
- Modify: `book/src/SUMMARY.md` (after the Study 009 line)
- Generated: `book/src/laboratory/generated/census-of-coasts/` (committed)

**Interfaces:**
- Consumes: Task 4's metric names, verbatim.
- Produces: the committed baseline tables Campaigns B/C are judged against.

- [ ] **Step 1: Write the study**

```json
{ "name": "census-of-coasts",
  "description": "The Census of Coasts: continental-shape metrics over 10,000 worlds — the preregistered baseline the terrain overhaul (Crust, Sculpting) must move.",
  "seeds": { "from": 0, "count": 10000 },
  "pin_sets": [ { "label": "default", "pins": [] } ],
  "metrics": [ "shoreline-development", "hypsometric-bimodality", "shelf-fraction", "continent-count", "largest-continent-share", "plate-size-gini" ] }
```

- [ ] **Step 2: Validate on a small run first**

Temporarily copy with `"count": 50` to `/tmp/coasts-smoke.study.json`, then:
Run: `cargo run -p hornvale -- lab run /tmp/coasts-smoke.study.json`
Expected: "study census-of-coasts: 50 rows, 0 refusals" and a summary under `book/src/laboratory/generated/census-of-coasts/`. Inspect the summary — every metric column populated, Absent rare.

- [ ] **Step 3: The full author-time run** (10,000 world builds — expect this to take as long as the existing censuses; it is not added to CI)

Run: `cargo run -p hornvale -- lab run studies/census-of-coasts.study.json`
Expected: 10,000 rows, 0 refusals; fresh artifacts in `book/src/laboratory/generated/census-of-coasts/`.

- [ ] **Step 4: Write the book page** `book/src/laboratory/study-010.md`

Follow `study-008.md`'s voice (title, then interpretive prose around the generated tables/charts, `{{#include}}` or links matching how study-008/009 embed their `generated/` artifacts — mirror the existing mechanism exactly). Required content, at the book's deliberate altitude:

- What each of the six metrics measures and its formula, one short paragraph each.
- The baseline numbers for the current generator, read off the run.
- **The preregistered target directions** (spec §2): shoreline development up, shelf fraction up, plate Gini up, hypsometric bimodality retained, largest-continent-share more dispersed — recorded *now*, before Crust and Sculpting, so the later comparison is honest.
- A sentence naming the diagnosis this census quantifies (near-convex single-scale Voronoi continents).

Add to `book/src/SUMMARY.md` after Study 009:
```markdown
- [Study 010: The Census of Coasts](./laboratory/study-010.md)
```

- [ ] **Step 5: Build the book to verify wiring**

Run: `mdbook build book`
Expected: clean build, no missing-file warnings for study-010.

- [ ] **Step 6: Commit**

```bash
git add studies/census-of-coasts.study.json book/src/laboratory/ book/src/SUMMARY.md && git commit -m "feat(lab): the Census of Coasts — 10,000-seed shape baseline

Preregistered target directions recorded in Study 010 before any
generator change; author-time census (drift coverage of the metrics
already rides the three 'all'-metric CI studies)."
```

---

### Task 6: Render-time coastline refinement

**Files:**
- Modify: `domains/terrain/src/streams.rs` (one const), `domains/terrain/src/lib.rs` (`stream_labels()` entry), `domains/terrain/src/render.rs` (the refinement), `cli/src/main.rs:277` (call site)
- Regenerate: `book/src/gallery/elevation-seed-42.png` + `elevation-seed-42.md`, `book/src/reference/stream-manifest-generated.md`

**Interfaces:**
- Consumes: kernel `Seed::derive(&str)`, `noise::fbm_2d(Seed, f64, f64, u32)`, `NearestCellIndex::nearest`, `Geosphere::{position, neighbors}`.
- Produces: `pub fn elevation_png(geo: &Geosphere, globe: &TectonicGlobe, world_seed: Seed) -> Vec<u8>` (signature gains the seed). `elevation_ascii` is **unchanged**.

- [ ] **Step 1: Add the label**

`domains/terrain/src/streams.rs`:
```rust
/// Render-lens coastline noise. Hash-noise only — never consumed as a
/// `Stream`; the lens draws nothing (Campaign 25 spec §3).
pub const COAST_RENDER: &str = "coast-render";
```

`domains/terrain/src/lib.rs`, appended inside `stream_labels()`'s vec:
```rust
        (
            "terrain/coast-render",
            "render-lens coastline noise (hash-noise only; no stream draws)",
        ),
```

Run: `cargo test -p hornvale-terrain stream_labels`
Expected: PASS (the format test accepts the new fully-qualified row).

- [ ] **Step 2: Write the failing tests** (replace/extend `render.rs`'s `mod tests`)

```rust
    #[test]
    fn refinement_respects_the_prior() {
        let geo = Geosphere::new(4);
        let globe = generate(Seed(42), &geo, &TerrainPins::default())
            .unwrap()
            .globe;
        let index = NearestCellIndex::new(&geo);
        let noise_seed = Seed(42).derive(crate::streams::ROOT).derive(crate::streams::COAST_RENDER);
        let (width, height) = (128u32, 64u32);
        for py in 0..height {
            let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
            for px in 0..width {
                let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
                let interp = interpolated_elevation(&geo, &index, &globe, latitude, longitude);
                let refined = refined_elevation(&geo, &index, &globe, noise_seed, latitude, longitude);
                // Bounded displacement, always.
                assert!((refined - interp).abs() <= COAST_AMP_M + 1e-9);
                // Exactly the prior away from the coast.
                if (interp - globe.sea_level).abs() > 3.0 * COAST_ENVELOPE_M {
                    assert_eq!(refined, interp);
                }
                // A land/ocean flip only happens inside the displacement band.
                let flipped = (refined >= globe.sea_level) != (interp >= globe.sea_level);
                if flipped {
                    assert!((interp - globe.sea_level).abs() <= COAST_AMP_M + 1e-9);
                }
            }
        }
    }

    #[test]
    fn interpolation_stays_within_the_candidate_envelope() {
        let geo = Geosphere::new(4);
        let globe = generate(Seed(7), &geo, &TerrainPins::default()).unwrap().globe;
        let index = NearestCellIndex::new(&geo);
        for (latitude, longitude) in [(0.0, 0.0), (45.5, -120.25), (-67.0, 13.0), (89.0, 179.0)] {
            let interp = interpolated_elevation(&geo, &index, &globe, latitude, longitude);
            let nearest = index.nearest(&geo, latitude, longitude);
            let mut lo = *globe.elevation.get(nearest);
            let mut hi = lo;
            for &n in geo.neighbors(nearest) {
                lo = lo.min(*globe.elevation.get(n));
                hi = hi.max(*globe.elevation.get(n));
            }
            assert!((lo..=hi).contains(&interp), "interp {interp} outside [{lo}, {hi}]");
        }
    }
```

Also update the existing `png_is_well_formed_and_byte_deterministic` test: the two `elevation_png(&geo, &globe)` calls gain `, Seed(42)`, and keep the IHDR assertions (they read `MAP_WIDTH`, which changes value but not the assertion).

- [ ] **Step 3: Run tests to verify they fail**

Run: `cargo test -p hornvale-terrain render`
Expected: FAIL — `interpolated_elevation`/`refined_elevation`/constants not found.

- [ ] **Step 4: Implement** (in `render.rs`)

First check the kernel's latitude/longitude convention: read `Geosphere::coord` and `NearestCellIndex::nearest` in `kernel/src/geosphere.rs` and write `direction()` as the exact inverse of `coord()` (the code below assumes the standard convention — verify, don't trust).

```rust
use crate::streams;
use hornvale_kernel::{Seed, noise};

/// Peak coastal displacement, meters. Bounds |refined − interpolated|.
const COAST_AMP_M: f64 = 150.0;
/// Gaussian envelope width, meters: displacement fades as the interpolated
/// elevation leaves sea level and is exactly zero beyond three widths.
const COAST_ENVELOPE_M: f64 = 300.0;
/// Base spatial frequency of the coastline noise over unit-sphere
/// coordinates (features ~1/24 rad ≈ 2.4° at the base octave).
const COAST_FREQ: f64 = 24.0;
/// fBm octaves for the coastline noise (base 2.4° down to ~0.15°).
const COAST_OCTAVES: u32 = 5;

/// Unit vector for a latitude/longitude in degrees (inverse of the
/// kernel's `Geosphere::coord` convention).
fn direction(latitude: f64, longitude: f64) -> [f64; 3] {
    let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
    [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()]
}

/// Seam-free coastline noise in [−1, 1) at a unit-sphere position: the
/// mean of three orthogonal 2D fBm slices, recentred. Stateless hash-noise
/// under `terrain/coast-render` — no `Stream` is ever consumed.
fn coast_noise(noise_seed: Seed, p: [f64; 3]) -> f64 {
    let slices = [
        (noise_seed.derive("slice-0"), p[0], p[1]),
        (noise_seed.derive("slice-1"), p[1], p[2]),
        (noise_seed.derive("slice-2"), p[2], p[0]),
    ];
    let mean = slices
        .iter()
        .map(|(s, a, b)| noise::fbm_2d(*s, COAST_FREQ * a, COAST_FREQ * b, COAST_OCTAVES))
        .sum::<f64>()
        / 3.0;
    2.0 * mean - 1.0
}

/// Gaussian-weighted elevation over the nearest cell and its neighbors —
/// the refinement's prior, a convex combination of nearby cell values.
/// The weight width is half the nearest cell's mean neighbor spacing, so
/// candidates entering or leaving the set as the nearest cell flips carry
/// negligible weight (no visible seams).
fn interpolated_elevation(
    geo: &Geosphere,
    index: &NearestCellIndex,
    globe: &TectonicGlobe,
    latitude: f64,
    longitude: f64,
) -> f64 {
    let p = direction(latitude, longitude);
    let nearest = index.nearest(geo, latitude, longitude);
    let neighbors = geo.neighbors(nearest);
    let spacing = neighbors
        .iter()
        .map(|&n| angle(geo.position(nearest), geo.position(n)))
        .sum::<f64>()
        / neighbors.len() as f64;
    let sigma = spacing / 2.0;
    let mut weighted = 0.0;
    let mut total = 0.0;
    for cell in std::iter::once(nearest).chain(neighbors.iter().copied()) {
        let theta = angle(p, geo.position(cell));
        let weight = (-(theta * theta) / (sigma * sigma)).exp();
        weighted += weight * *globe.elevation.get(cell);
        total += weight;
    }
    weighted / total
}

/// Angular distance between two unit vectors, radians.
fn angle(a: [f64; 3], b: [f64; 3]) -> f64 {
    (a[0] * b[0] + a[1] * b[1] + a[2] * b[2]).clamp(-1.0, 1.0).acos()
}

/// The refined per-pixel elevation: the interpolated prior plus bounded
/// coastal displacement (coarse constrains fine, applied at the lens —
/// spec §3). Identical to the prior beyond three envelope widths.
fn refined_elevation(
    geo: &Geosphere,
    index: &NearestCellIndex,
    globe: &TectonicGlobe,
    noise_seed: Seed,
    latitude: f64,
    longitude: f64,
) -> f64 {
    let interp = interpolated_elevation(geo, index, globe, latitude, longitude);
    let d = (interp - globe.sea_level) / COAST_ENVELOPE_M;
    if d.abs() > 3.0 {
        return interp;
    }
    let envelope = (-d * d).exp();
    interp + COAST_AMP_M * envelope * coast_noise(noise_seed, direction(latitude, longitude))
}
```

Then rewire:
- `MAP_WIDTH`: `256` → `1024` (doc comment: "1024×512; pixel ≈ 0.35°, fine enough to show the refined coastline").
- `elevation_pixels(geo, globe)` → `elevation_pixels(geo, globe, world_seed: Seed)`: derive `let noise_seed = world_seed.derive(streams::ROOT).derive(streams::COAST_RENDER);` once before the loop, and replace the per-pixel `*globe.elevation.get(cell)` with `refined_elevation(geo, &index, globe, noise_seed, latitude, longitude)` (`geo` is already a reference inside `elevation_pixels`; the `color()` call keeps `globe.sea_level`).
- `elevation_png(geo, globe)` → `elevation_png(geo, globe, world_seed: Seed)`, passing through.
- `elevation_ascii`: untouched.
- `cli/src/main.rs:277`: `elevation_png(terrain.geosphere(), terrain.globe(), world.seed)`.

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo test -p hornvale-terrain && cargo test -p hornvale`
Expected: PASS — including `lens_purity` (Task 1's guard: the world fixture must be untouched) and the two new render properties. Note the property test renders 128×64 at level 4, not full size — keep it under a second.

- [ ] **Step 6: Regenerate the drifting artifacts, inspect, commit**

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-sky.json
cargo run -p hornvale -- map --world /tmp/hv-sky.json --out book/src/gallery/elevation-seed-42.png > book/src/gallery/elevation-seed-42.md
cargo run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
git status book/src/
```

Expected changes: the PNG (now 1024×512, wiggly coasts — **open it and look**), the manifest (one new `terrain/coast-render` row), possibly the map `.md` (only if the PNG filename line changed — the ASCII block must be byte-identical). The three almanac fixtures must NOT appear in `git status`.

```bash
cargo fmt && git add domains/terrain/ cli/src/main.rs book/src/gallery/elevation-seed-42.png book/src/gallery/elevation-seed-42.md book/src/reference/stream-manifest-generated.md
git commit -m "feat(terrain): render-time coastline refinement at 1024x512

Gaussian-weighted interpolation over nearest cells + bounded seeded
coastal displacement (hash-noise under terrain/coast-render; zero
Stream draws). Pure lens: world JSON and almanacs byte-identical,
guarded by cli/tests/lens_purity.rs."
```

---

### Task 7: Book close-out, spec amendment, retrospective

**Files:**
- Create: `book/src/chronicle/25-the-measured-coast.md`, `docs/retrospectives/` entry (match existing filenames there)
- Modify: `book/src/SUMMARY.md` (chronicle line after Campaign 23/24 — check whether Deep Time's entry has merged and slot accordingly), any stale chapter referencing the 256×256 map or cell-blocky coastlines (grep the book for `256` near map references and for the elevation-map image), `docs/superpowers/specs/2026-07-09-the-measured-coast-design.md` (one amendment), `docs/vision/idea-registry.md` (only if it indexes new-campaign rows — follow its existing row format if it has one for in-flight campaigns; otherwise skip)

- [ ] **Step 1: Amend the spec** — replace the `census-coasts-drift.study.json` bullet in §2 with:

```markdown
- Drift coverage of the new metrics needs no new study: the three CI-run
  studies (`census-lands-drift`, `census-of-the-meeting`, `census-of-skies`)
  select `"metrics": "all"`, so the new columns are drift-checked from the
  commit that adds them. (Amended during planning: the originally proposed
  `census-coasts-drift.study.json` would have double-run identical builds.)
```

And in §4, replace the "Census drift" bullet with: "Census drift — covered by the existing `all`-metric CI studies from the metrics commit onward."

- [ ] **Step 2: Chronicle entry** `book/src/chronicle/25-the-measured-coast.md` — follow `23-the-orrery.md`'s form and altitude. Must cover: the guitar-pick diagnosis (single-scale convex Voronoi), measurement-before-surgery, the six instruments and the baseline they recorded, the lens refinement (show the before/after idea: same world, better observation — coarse-constrains-fine at pixel scale), the world-identity fixture guard, and the road ahead (Crust, Sculpting — one sentence each, no promises of specifics). Add the SUMMARY.md line: `- [Campaign 25: The Measured Coast](./chronicle/25-the-measured-coast.md)`.

- [ ] **Step 3: Freshness sweep** — `grep -rn "elevation-seed-42\|256×128\|256x128\|guitar" book/src/` and read the terrain-related chapters (the concept-registry/terrain chapters and anything embedding the elevation map); update any prose that states the map's size or describes cell-scale coasts as current reality. The book may never lag merged reality.

- [ ] **Step 4: Retrospective** — one page in `docs/retrospectives/` (match the directory's existing naming), process lessons only (decision 0020). Candidate lessons to evaluate honestly: did the fixture-guard-first ordering pay off; was amending the spec during planning (drift-study redundancy) caught at the right stage; census runtime.

- [ ] **Step 5: The full gate, book build, and CI dry-run**

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
mdbook build book
```

Then run the CI artifact block end-to-end locally (every command from `.github/workflows/ci.yml`'s "Artifacts are current" step, in order) and finish with:

```bash
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
```

Expected: exit 0 — no drift anywhere.

- [ ] **Step 6: Commit**

```bash
git add book/ docs/ && git commit -m "docs(book): chronicle Campaign 25 — The Measured Coast

Chronicle entry, Study 010 wiring verified, freshness sweep, spec
amendment (drift coverage via existing all-metric CI studies),
retrospective (decision 0020)."
```

---

## Self-review notes

- **Spec coverage:** §1→Tasks 2–6; §2 metrics→Tasks 2–4, census→Task 5; §3 refinement→Task 6 (interpolation, envelope, label, `MAP_WIDTH`, cli); §4 tests→Tasks 1, 2, 3, 4, 6 (synthetic-globe correctness, determinism, prior-respect, byte-identity, drift); §5 DoD→Task 7 + per-task gates; §6 non-goals→no task touches `windows/worldgen` or any generator path. The spec's `census-coasts-drift` bullet is deliberately amended (Task 7 Step 1) — discovered redundant during planning because all three CI studies select `"metrics": "all"`.
- **Type consistency:** `shape` signatures in Task 2/3 match Task 4's call sites; `elevation_png(geo, globe, world_seed)` matches Task 6's cli call; metric names in Task 4 match Task 5's study JSON verbatim.
- **Known judgment calls for the executor:** the `direction()` convention must be verified against `kernel/src/geosphere.rs` (Task 6 Step 4 says so); loose synthetic-test bounds (Task 2) are discretization allowances, not tuning knobs.
