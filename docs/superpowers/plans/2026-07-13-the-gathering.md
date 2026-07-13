# The Gathering (MAP-7 field-first) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A new kernel-only `demography` domain that derives a carrying-capacity field from climate+terrain and condenses settlements out of it as conserved attractors of a population flow, replacing the suitability scatter.

**Architecture:** `domains/demography` computes `K(cell)` per species (Miami-model NPP proxy), runs `drainage.rs`'s flow-accumulation on K with the gradient flipped (people climb the K-gradient as water descends elevation), and extracts settlements as flow attractors whose population is the field integrated over their catchment (`Σ pop == Σ K`, exact). `windows/worldgen` wires demography → settlement; the suitability scatter and the population draw are retired.

**Tech Stack:** Rust (edition 2024), `hornvale-kernel` (Seed/Geosphere/CellMap/CellId), `cargo-nextest`, serde-only. No new dependencies.

## Global Constraints

Copied verbatim from the spec and CLAUDE.md; every task implicitly includes these:

- **Determinism is constitutional.** Same seed + same pins → byte-identical worlds/almanacs/artifacts. The demography pipeline draws **nothing** from the seed.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only. Float sorting uses `total_cmp` with deterministic tie-breaks. Enforced by `clippy.toml`.
- **No wall-clock time.** Time is `WorldTime { day: f64 }`.
- **Transcendentals route through `hornvale_kernel::math`** (libm), never `f64::` methods, for cross-platform byte-identity. Quantize at emit boundaries only, never in the compute path.
- **Layering (enforced by `cli/tests/architecture.rs`):** `demography` is a domain — depends on `hornvale-kernel` and **nothing else**. It may not import `settlement`, `terrain`, or `climate`. `settlement` may not import `demography`. They meet only at `windows/worldgen`.
- **Dependencies:** `serde` + `serde_json` only, workspace-wide. No new crates.
- **Every crate sets `#![warn(missing_docs)]`;** every public item/field/variant gets a one-line doc comment.
- **Type audit:** every primitive at a `pub` boundary carries a `type-audit:` verdict tag (`bare-ok(<class>)` / `waiver(<reason>)` / `pending(wave-N)`).
- **`cargo fmt` is the final step before every commit.** The commit gate is `make gate` (`cargo nextest run --workspace` + doctests + fmt + clippy).
- **Save-format contracts:** seed-derivation labels are permanent; deliberate regeneration uses an epoch suffix, never a rename.

---

## Stage 1 — The `demography` crate

### Task 1: Scaffold the `demography` crate

**Files:**
- Create: `domains/demography/Cargo.toml`
- Create: `domains/demography/src/lib.rs`
- Modify: `Cargo.toml` (workspace `members`)
- Modify: `cli/tests/architecture.rs` (allowlist the new domain crate + its single kernel dep)

**Interfaces:**
- Produces: the crate `hornvale-demography`, empty but compiling, kernel-only.

- [ ] **Step 1: Create the Cargo.toml**, mirroring `domains/paleoclimate/Cargo.toml` exactly (same `[package]` fields, edition, `version.workspace`, the single dependency `hornvale-kernel = { path = "../../kernel" }`, and `serde` only if a public type needs derive — omit for now).

- [ ] **Step 2: Create `src/lib.rs`:**

```rust
//! Hornvale demography domain: a carrying-capacity field derived from climate
//! and terrain, and the flow-condensation that reads discrete settlements off
//! it as conserved attractors of a population flow. Kernel-only: the
//! composition root supplies each cell's bare climate/terrain inputs; this
//! crate never imports a climate or terrain crate.
//!
//! Condensation is the same field-to-fact projection the codebase performs
//! for biomes (Whittaker classification) and rivers (`terrain::drainage`);
//! lifting all three onto one kernel primitive is future work — see the
//! design spec's scope boundary.
#![warn(missing_docs)]
```

- [ ] **Step 3: Add `"domains/demography"` to the workspace `members` list** in the root `Cargo.toml`, keeping the list alphabetically ordered next to `domains/culture`.

- [ ] **Step 4: Extend the architecture test.** Open `cli/tests/architecture.rs`, find the table/list that declares each domain crate and its permitted dependencies, and add `demography` as a domain whose only permitted dependency is `hornvale-kernel` (mirror the `paleoclimate` entry exactly).

- [ ] **Step 5: Verify it compiles and passes the architecture gate**

Run: `cargo build -p hornvale-demography && cargo test -p hornvale --test architecture`
Expected: PASS (crate builds; architecture test green with the new domain listed).

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add domains/demography Cargo.toml cli/tests/architecture.rs
git commit -m "feat(demography): scaffold the kernel-only demography crate"
```

### Task 2: The carrying-capacity field K

**Files:**
- Create: `domains/demography/src/carrying_capacity.rs`
- Modify: `domains/demography/src/lib.rs` (`pub mod carrying_capacity;` + re-exports)

**Interfaces:**
- Consumes: `hornvale_kernel::{CellId, CellMap, Geosphere, math}`.
- Produces:
  - `pub struct CarryingInput { pub habitable: bool, pub temperature_c: f64, pub moisture: f64, pub freshwater: f64, pub coastal: bool, pub hostility: f64 }`
  - `pub fn carrying_capacity(geo: &Geosphere, inputs: &CellMap<CarryingInput>) -> CellMap<f64>` — people-supportable density per cell, `0.0` where not habitable.

- [ ] **Step 1: Write the failing test** in `carrying_capacity.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    fn input(hab: bool, t: f64, m: f64, fw: f64, coast: bool, host: f64) -> CarryingInput {
        CarryingInput { habitable: hab, temperature_c: t, moisture: m, freshwater: fw, coastal: coast, hostility: host }
    }

    #[test]
    fn uninhabitable_is_zero_and_wet_temperate_beats_desert() {
        let geo = Geosphere::new(2);
        // cell 0 uninhabitable, 1 wet-temperate, 2 hot desert.
        let inputs = CellMap::from_fn(&geo, |c| match c.0 {
            0 => input(false, 15.0, 0.8, 0.9, true, 0.0),
            1 => input(true, 15.0, 0.8, 0.9, true, 0.0),
            _ => input(true, 40.0, 0.05, 0.05, false, 0.8),
        });
        let k = carrying_capacity(&geo, &inputs);
        assert_eq!(*k.get(CellId(0)), 0.0, "uninhabitable supports nobody");
        assert!(*k.get(CellId(1)) > *k.get(CellId(2)), "wet-temperate beats desert");
        assert!(*k.get(CellId(1)) >= 0.0 && *k.get(CellId(2)) >= 0.0);
    }

    #[test]
    fn npp_proxy_is_liebig_minimum_of_temperature_and_moisture() {
        let geo = Geosphere::new(2);
        // Warm+dry and cool+wet should both be limited by their scarce factor.
        let inputs = CellMap::from_fn(&geo, |c| match c.0 {
            0 => input(true, 25.0, 0.1, 0.5, false, 0.0), // moisture-limited
            _ => input(true, 25.0, 0.9, 0.5, false, 0.0), // ample both
        });
        let k = carrying_capacity(&geo, &inputs);
        assert!(*k.get(CellId(0)) < *k.get(CellId(1)), "the scarce factor caps K");
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-demography carrying_capacity`
Expected: FAIL ("cannot find function `carrying_capacity`").

- [ ] **Step 3: Implement the field.** Miami-model NPP proxy as the Liebig minimum of a temperature response and a moisture response, both dimensionless in `[0,1]`, then multiplied by bonuses/penalties. Constants are `PLACEHOLDER` here and frozen in Task 8's calibration.

```rust
//! The carrying-capacity field K: a closed-form, seed-free people-density a
//! cell can support, grounded in a Miami-model net-primary-productivity proxy
//! (Lieth) plus freshwater, coast, and aridity terms. All constants are
//! calibrated once (the-gathering plan Task 8) and then frozen as save-format
//! constants.

use hornvale_kernel::{math, CellId, CellMap, Geosphere};

/// The bare per-cell climate/terrain inputs the composition root assembles.
/// Demography never imports those domains; it sees only this.
/// type-audit: bare-ok(flag: habitable), pending(wave-3: temperature_c), bare-ok(ratio: moisture), bare-ok(ratio: freshwater), bare-ok(flag: coastal), bare-ok(ratio: hostility)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CarryingInput {
    /// Whether the cell is habitable (land, water, tolerable season).
    pub habitable: bool,
    /// Annual-mean temperature, °C.
    pub temperature_c: f64,
    /// Moisture in `[0, 1]`.
    pub moisture: f64,
    /// Freshwater availability in `[0, 1]` (drainage/moisture, at root).
    pub freshwater: f64,
    /// Whether the cell borders the ocean.
    pub coastal: bool,
    /// Hostility in `[0, 1]` (aridity, tectonic unrest).
    pub hostility: f64,
}

// Calibrated constants (PLACEHOLDER until Task 8; frozen thereafter).
const BASE: f64 = 1.0;
const TEMP_OPTIMUM_C: f64 = 22.0;
const TEMP_TOLERANCE_C: f64 = 20.0;
const FRESHWATER_BONUS: f64 = 0.5;
const COAST_BONUS: f64 = 0.2;

/// Temperature response in `[0,1]`: a triangular tolerance around the optimum.
fn temp_response(t: f64) -> f64 {
    (1.0 - (t - TEMP_OPTIMUM_C).abs() / TEMP_TOLERANCE_C).clamp(0.0, 1.0)
}

/// The carrying-capacity field: `0.0` on uninhabitable cells, else the NPP
/// proxy scaled by freshwater, coast, and hostility terms.
/// type-audit: bare-ok(count: return)
pub fn carrying_capacity(geo: &Geosphere, inputs: &CellMap<CarryingInput>) -> CellMap<f64> {
    CellMap::from_fn(geo, |c: CellId| {
        let i = inputs.get(c);
        if !i.habitable {
            return 0.0;
        }
        // Miami NPP proxy: Liebig minimum of temperature and moisture responses.
        let npp = math::fmin(temp_response(i.temperature_c), i.moisture.clamp(0.0, 1.0));
        let bonus = 1.0 + FRESHWATER_BONUS * i.freshwater.clamp(0.0, 1.0)
            + if i.coastal { COAST_BONUS } else { 0.0 };
        let k = BASE * npp * bonus * (1.0 - i.hostility.clamp(0.0, 1.0));
        k.max(0.0)
    })
}
```

(If `math::fmin` does not exist, use `a.min(b)` — `min` on `f64` is not a transcendental and is byte-stable; confirm against `kernel/src/math.rs` and use the plain `.min` if so.)

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-demography carrying_capacity`
Expected: PASS (both tests).

- [ ] **Step 5: Wire the module** into `lib.rs`: add `pub mod carrying_capacity;` and `pub use carrying_capacity::{carrying_capacity, CarryingInput};`.

- [ ] **Step 6: Commit**

```bash
cargo fmt && cargo clippy -p hornvale-demography --all-targets -- -D warnings
git add domains/demography
git commit -m "feat(demography): carrying-capacity field K from a Miami NPP proxy"
```

### Task 3: Flow-accumulation on K

**Files:**
- Create: `domains/demography/src/flow.rs`
- Modify: `domains/demography/src/lib.rs`
- Reference: `domains/terrain/src/drainage.rs` (the algorithm to mirror, comparator flipped)

**Interfaces:**
- Consumes: `carrying_capacity` output (`CellMap<f64>`), `Geosphere`.
- Produces:
  - `pub struct Flow { pub accumulation: CellMap<f64>, pub attractor: CellMap<Option<CellId>> }` where `attractor[c]` is the sink cell `c`'s up-gradient path terminates at (`None` iff `K(c) == 0`).
  - `pub fn flow(geo: &Geosphere, k: &CellMap<f64>) -> Flow`.

- [ ] **Step 1: Write the failing test:**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    #[test]
    fn accumulation_conserves_k_over_attractors() {
        let geo = Geosphere::new(3);
        // A smooth bump: K peaks at cell 0, falls with distance.
        let peak = geo.position(CellId(0));
        let k = CellMap::from_fn(&geo, |c| {
            let p = geo.position(c);
            let dot = p[0]*peak[0] + p[1]*peak[1] + p[2]*peak[2];
            (dot.max(0.0)) // 0..1, peaks at cell 0
        });
        let f = flow(&geo, &k);
        // Total K equals total accumulation collected at attractor sinks.
        let total_k: f64 = geo.cells().map(|c| *k.get(c)).sum();
        let total_sink: f64 = geo
            .cells()
            .filter(|c| f.attractor.get(*c).map_or(false, |a| a == *c))
            .map(|c| *f.accumulation.get(c))
            .sum();
        assert!((total_k - total_sink).abs() < 1e-9, "flow must conserve K: {total_k} vs {total_sink}");
    }

    #[test]
    fn zero_k_cell_has_no_attractor() {
        let geo = Geosphere::new(2);
        let k = CellMap::from_fn(&geo, |c| if c.0 == 0 { 0.0 } else { 1.0 });
        let f = flow(&geo, &k);
        assert!(f.attractor.get(CellId(0)).is_none(), "a zero-K cell drains nowhere");
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-demography flow`
Expected: FAIL ("cannot find function `flow`").

- [ ] **Step 3: Implement flow**, mirroring `drainage.rs` with the comparator flipped (each cell routes to its **highest-K** neighbour; a cell with no higher-K neighbour is an attractor sink). Break exact-K ties by preferring the higher `CellId` (deterministic), matching drainage's strict-ordering discipline. Use drainage's memoised path-trace to (a) find each cell's terminal attractor and (b) accumulate each cell's own K into every cell on its up-gradient path.

```rust
//! Population flow-accumulation over the carrying-capacity field: the terrain
//! `drainage` algorithm with the gradient flipped — people climb the K-gradient
//! as water descends elevation. Each land cell routes to its highest-K
//! neighbour; a cell with no higher neighbour is an attractor. Draws nothing;
//! integer-and-comparison only (K's transcendentals are already spent).

use hornvale_kernel::{CellId, CellMap, Geosphere};

/// The flow field: per-cell accumulated population budget and the attractor
/// (sink) each cell's up-gradient path terminates at.
#[derive(Debug, Clone)]
pub struct Flow {
    /// Accumulated K: `accumulation[c]` = sum of K over all cells whose
    /// up-gradient path passes through `c` (including `c`).
    /// type-audit: bare-ok(count)
    pub accumulation: CellMap<f64>,
    /// The attractor sink each cell drains to (`None` iff `K(c) == 0`).
    /// type-audit: bare-ok(identifier: cell)
    pub attractor: CellMap<Option<CellId>>,
}

/// Compute the flow field. See module docs.
pub fn flow(geo: &Geosphere, k: &CellMap<f64>) -> Flow {
    let n = geo.cell_count();
    // Up-gradient target per cell: strictly-highest-K neighbour, ties to the
    // higher CellId. `None` = zero-K cell or a local maximum (an attractor).
    let mut up: Vec<Option<CellId>> = vec![None; n];
    for c in geo.cells() {
        if *k.get(c) <= 0.0 {
            continue;
        }
        let here = *k.get(c);
        let mut best: Option<CellId> = None;
        let mut best_k = here;
        for &nb in geo.neighbors(c) {
            let e = *k.get(nb);
            if e > best_k || (e == best_k && Some(nb) > best && best.is_none() == false) {
                // strictly higher, or equal-and-higher-id (deterministic tie-break)
            }
            if e > best_k || (e == best_k && best.map_or(true, |b| nb > b)) {
                best_k = e;
                best = Some(nb);
            }
        }
        up[c.0 as usize] = best;
    }

    // Terminal attractor per cell: follow `up` to a sink, memoised (bounded n).
    let mut term: Vec<Option<CellId>> = vec![None; n];
    for start in geo.cells() {
        if *k.get(start) <= 0.0 || term[start.0 as usize].is_some() {
            continue;
        }
        let mut path = Vec::new();
        let mut cur = start;
        loop {
            path.push(cur);
            if let Some(t) = term[cur.0 as usize] {
                for p in &path { term[p.0 as usize] = Some(t); }
                break;
            }
            match up[cur.0 as usize] {
                None => { // cur is the sink
                    for p in &path { term[p.0 as usize] = Some(cur); }
                    break;
                }
                Some(next) => cur = next,
            }
        }
    }

    // Accumulate: each cell adds its own K to every cell on its up-path.
    let mut acc = vec![0.0f64; n];
    for start in geo.cells() {
        let kv = *k.get(start);
        if kv <= 0.0 { continue; }
        let mut cur = start;
        loop {
            acc[cur.0 as usize] += kv;
            match up[cur.0 as usize] {
                None => break,
                Some(next) => cur = next,
            }
        }
    }

    let accumulation = CellMap::from_fn(geo, |c| acc[c.0 as usize]);
    let attractor = CellMap::from_fn(geo, |c| term[c.0 as usize]);
    Flow { accumulation, attractor }
}
```

(Clean up the redundant tie-break branch — keep only the second `if`. The first is illustrative and must be deleted.)

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-demography flow`
Expected: PASS.

- [ ] **Step 5: Wire the module** into `lib.rs`: `pub mod flow;` + `pub use flow::{flow, Flow};`.

- [ ] **Step 6: Commit**

```bash
cargo fmt && cargo clippy -p hornvale-demography --all-targets -- -D warnings
git add domains/demography
git commit -m "feat(demography): population flow-accumulation on K (drainage, flipped)"
```

### Task 4: Condensation — attractors, catchments, conserved population

**Files:**
- Create: `domains/demography/src/condense.rs`
- Modify: `domains/demography/src/lib.rs`

**Interfaces:**
- Consumes: `flow`, `Flow`, `CellMap<f64>`, `Geosphere`.
- Produces:
  - `pub struct Condensation { pub cell: CellId, pub position: [f64; 3], pub population: f64 }`
  - `pub fn condense(geo: &Geosphere, k: &CellMap<f64>, threshold: f64) -> Vec<Condensation>` — attractors whose accumulation `>= threshold`, sorted by descending population then ascending cell id; population is the catchment integral (`= accumulation` at the attractor).

- [ ] **Step 1: Write the failing test:**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    fn bump_k(geo: &Geosphere) -> CellMap<f64> {
        let peak = geo.position(CellId(0));
        CellMap::from_fn(geo, |c| {
            let p = geo.position(c);
            (p[0]*peak[0] + p[1]*peak[1] + p[2]*peak[2]).max(0.0)
        })
    }

    #[test]
    fn condensation_conserves_population_against_total_k() {
        let geo = Geosphere::new(3);
        let k = bump_k(&geo);
        let nodes = condense(&geo, &k, 0.0); // threshold 0 → every attractor kept
        let total_k: f64 = geo.cells().map(|c| *k.get(c)).sum();
        let total_pop: f64 = nodes.iter().map(|n| n.population).sum();
        assert!((total_k - total_pop).abs() < 1e-9, "Σ pop == Σ K: {total_pop} vs {total_k}");
    }

    #[test]
    fn raising_the_threshold_reduces_the_node_count() {
        let geo = Geosphere::new(3);
        let k = bump_k(&geo);
        let many = condense(&geo, &k, 0.0).len();
        let few = condense(&geo, &k, 5.0).len();
        assert!(few <= many, "a higher threshold keeps fewer nodes: {few} <= {many}");
    }

    #[test]
    fn nodes_are_sorted_flagship_first() {
        let geo = Geosphere::new(3);
        let nodes = condense(&geo, &bump_k(&geo), 0.0);
        assert!(nodes.windows(2).all(|w| w[0].population >= w[1].population), "descending population");
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-demography condense`
Expected: FAIL.

- [ ] **Step 3: Implement condensation.** An attractor is a cell that is its own terminal (`attractor[c] == Some(c)`); its population is `accumulation[c]` (the catchment integral, conserved by Task 3). Keep those `>= threshold`; sort by `population` descending, ties by ascending `cell.0`.

```rust
//! Condensation: read discrete settlements off the flow field. An attractor
//! (a flow sink) whose catchment population clears the concentration threshold
//! becomes a settlement; its population is the field integrated over its
//! catchment, conserved against total K by construction.

use crate::flow::flow;
use hornvale_kernel::{CellId, CellMap, Geosphere};

/// A condensed settlement: where it sits and how many people its catchment
/// supports. Population is a readout of the field, never a draw.
/// type-audit: bare-ok(identifier: cell), pending(wave-3: position), bare-ok(count: population)
#[derive(Debug, Clone, PartialEq)]
pub struct Condensation {
    /// The attractor cell the settlement sits on.
    pub cell: CellId,
    /// Unit-sphere position.
    pub position: [f64; 3],
    /// Catchment population (= flow accumulation at the attractor).
    pub population: f64,
}

/// Condense settlements from `k`: attractors whose accumulation `>= threshold`,
/// flagship (largest) first. See module docs.
pub fn condense(geo: &Geosphere, k: &CellMap<f64>, threshold: f64) -> Vec<Condensation> {
    let f = flow(geo, k);
    let mut nodes: Vec<Condensation> = geo
        .cells()
        .filter(|c| f.attractor.get(*c).map_or(false, |a| a == *c))
        .map(|c| (c, *f.accumulation.get(c)))
        .filter(|(_, pop)| *pop >= threshold)
        .map(|(cell, population)| Condensation { cell, position: geo.position(cell), population })
        .collect();
    nodes.sort_by(|a, b| b.population.total_cmp(&a.population).then(a.cell.0.cmp(&b.cell.0)));
    nodes
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-demography condense`
Expected: PASS (all three).

- [ ] **Step 5: Wire the module** and re-export `condense`, `Condensation`.

- [ ] **Step 6: Commit**

```bash
cargo fmt && cargo clippy -p hornvale-demography --all-targets -- -D warnings
git add domains/demography
git commit -m "feat(demography): condense settlements as conserved flow attractors"
```

### Task 5: The founder floor and multi-species condensation

**Files:**
- Create: `domains/demography/src/founder.rs`
- Modify: `domains/demography/src/lib.rs`
- Reference: `domains/settlement/src/placement.rs` (the `founder_pass` logic + its 6 founder tests to migrate)

**Interfaces:**
- Consumes: `condense`, `Condensation`.
- Produces:
  - `pub fn condense_tagged(per_species: &[(u32, CellMap<f64>)], geo: &Geosphere, threshold: f64) -> Vec<(Condensation, u32)>` — condense each species' K independently; guarantee each species (tag) its single strongest attractor (the founder floor) even if below `threshold`; ties broken lower-tag-then-lower-cell.

- [ ] **Step 1: Write the failing test** — migrate the intent of `settlement`'s founder tests: every tag gets at least its strongest attractor; the strongest-per-tag survives a threshold that would otherwise drop it.

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    fn peak_at(geo: &Geosphere, cell: u32) -> CellMap<f64> {
        let peak = geo.position(CellId(cell));
        CellMap::from_fn(geo, |c| {
            let p = geo.position(c);
            (p[0]*peak[0] + p[1]*peak[1] + p[2]*peak[2]).max(0.0)
        })
    }

    #[test]
    fn every_tag_keeps_its_strongest_attractor_above_threshold() {
        let geo = Geosphere::new(3);
        // Two species peaking at different cells; a threshold above their
        // catchment totals would drop both without the founder floor.
        let per = vec![(0u32, peak_at(&geo, 0)), (1u32, peak_at(&geo, 40))];
        let placed = condense_tagged(&per, &geo, f64::INFINITY);
        let tags: std::collections::BTreeSet<u32> = placed.iter().map(|(_, t)| *t).collect();
        assert_eq!(tags, std::collections::BTreeSet::from([0, 1]), "every tag founds one settlement");
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-demography founder`
Expected: FAIL.

- [ ] **Step 3: Implement** `condense_tagged`. For each `(tag, k)`: `condense` at `threshold`; if the tag has no node, add its single strongest attractor (`condense` at `0.0`, take the first — flagship) as the founder. Concatenate; sort the whole list by descending population, ties by ascending cell then ascending tag. Copy the tie-break discipline verbatim from `placement.rs::founder_pass` where the sort keys overlap.

```rust
//! The founder floor (MAP-22's allocation layer at K=1), migrated from
//! `settlement::placement`: before threshold culling erases a weak people,
//! reserve each species its single strongest attractor so no people is boxed
//! out to zero. Full cross-species competitive exclusion is MAP-22's own
//! campaign; this is only the floor.

use crate::condense::{condense, Condensation};
use hornvale_kernel::{CellMap, Geosphere};

/// Condense each species' K independently, guaranteeing every species its
/// strongest attractor (the founder floor). Returns `(settlement, tag)` pairs,
/// flagship (largest population) first; ties by ascending cell then tag.
pub fn condense_tagged(
    per_species: &[(u32, CellMap<f64>)],
    geo: &Geosphere,
    threshold: f64,
) -> Vec<(Condensation, u32)> {
    let mut out: Vec<(Condensation, u32)> = Vec::new();
    for (tag, k) in per_species {
        let mut nodes = condense(geo, k, threshold);
        if nodes.is_empty() {
            // Founder floor: the single strongest attractor, threshold ignored.
            if let Some(flagship) = condense(geo, k, 0.0).into_iter().next() {
                nodes.push(flagship);
            }
        }
        for n in nodes {
            out.push((n, *tag));
        }
    }
    out.sort_by(|a, b| {
        b.0.population
            .total_cmp(&a.0.population)
            .then(a.0.cell.0.cmp(&b.0.cell.0))
            .then(a.1.cmp(&b.1))
    });
    out
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-demography founder`
Expected: PASS.

- [ ] **Step 5: Wire + re-export** `condense_tagged`.

- [ ] **Step 6: Commit**

```bash
cargo fmt && cargo clippy -p hornvale-demography --all-targets -- -D warnings
git add domains/demography
git commit -m "feat(demography): founder floor + per-species condensation"
```

### Task 6: The `DemographyReport` and a debug render

**Files:**
- Create: `domains/demography/src/render.rs`
- Modify: `domains/demography/src/lib.rs` (add `DemographyReport`)
- Reference: `domains/paleoclimate/src/render.rs` (PPM pattern)

**Interfaces:**
- Produces:
  - `pub struct DemographyReport { pub per_species_k: Vec<(u32, CellMap<f64>)>, pub settlements: Vec<(Condensation, u32)> }`
  - `pub fn report(geo, per_species_inputs: &[(u32, CellMap<CarryingInput>)], threshold: f64) -> DemographyReport` — the one-call entry: build each species' K, condense-tagged, keep the K fields on the report for in-memory reuse.
  - `pub fn density_ppm(geo, k: &CellMap<f64>) -> String` (or the project's PPM byte type — match `paleoclimate::render`).

- [ ] **Step 1: Write the failing test:**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    #[test]
    fn report_holds_k_fields_and_settlements() {
        let geo = Geosphere::new(3);
        let inputs = CellMap::from_fn(&geo, |c| CarryingInput {
            habitable: true, temperature_c: 20.0, moisture: 0.7,
            freshwater: 0.6, coastal: c.0 % 2 == 0, hostility: 0.0,
        });
        let rep = report(&geo, &[(0u32, inputs)], 0.0);
        assert_eq!(rep.per_species_k.len(), 1, "K retained in memory for reuse");
        assert!(!rep.settlements.is_empty(), "a habitable world condenses settlements");
    }
}
```

- [ ] **Step 2: Run to verify it fails.** Run: `cargo test -p hornvale-demography report`. Expected: FAIL.

- [ ] **Step 3: Implement** `report` (build K per species via `carrying_capacity`, call `condense_tagged`, keep the K fields) and `density_ppm` (copy the PPM structure and byte format from `paleoclimate/src/render.rs`, colouring by K magnitude). Put `DemographyReport` in `lib.rs`.

- [ ] **Step 4: Run to verify it passes.** Run: `cargo test -p hornvale-demography`. Expected: PASS (whole crate).

- [ ] **Step 5: Full crate gate.** Run: `cargo fmt --check && cargo clippy -p hornvale-demography --all-targets -- -D warnings && cargo test -p hornvale-demography`. Expected: all green.

- [ ] **Step 6: Commit**

```bash
git add domains/demography
git commit -m "feat(demography): DemographyReport entry point + density debug render"
```

---

## Stage 2 — Integrate at the composition root, retire the scatter

### Task 7: Wire demography into worldgen; retire the suitability scatter and the population draw

**Files:**
- Modify: `windows/worldgen/Cargo.toml` (add `hornvale-demography` dependency)
- Modify: `windows/worldgen/src/lib.rs` (the `climate+settlements` stage, ~lines 1794–1990)
- Modify: `windows/worldgen/src/settlement_pins.rs` (retire `min_suitability`)
- Modify: `domains/settlement/src/lib.rs` (remove `draw_population`, `draw_species_population`)
- Modify: `domains/settlement/src/placement.rs` (delete the file's `suitability*` / `place*` / `founder_pass` API; the founder floor now lives in demography)
- Test: `windows/worldgen/src/lib.rs` tests (`min_suitability_pin_reduces_the_settlement_count` is deleted; add a conservation test)

**Interfaces:**
- Consumes: `hornvale_demography::{report, CarryingInput, Condensation, DemographyReport}`.
- Produces: settlements committed with population = catchment readout; determinism preserved.

- [ ] **Step 1: Write the failing test** (add to worldgen's test module): a world's committed settlement populations sum to (approximately) the demography report's total, and two builds of the same seed are byte-identical.

```rust
#[test]
fn settlement_populations_are_the_conserved_field_readout() {
    let world = build_world(Seed(42), &SkyPins::default()).unwrap(); // match the real builder signature
    let total: f64 = world.ledger.find(hornvale_settlement::IS_SETTLEMENT)
        .filter_map(|f| match world.ledger.value_of(f.subject, hornvale_settlement::POPULATION) {
            Some(hornvale_kernel::Value::Number(n)) => Some(*n),
            _ => None,
        }).sum();
    assert!(total > 0.0, "a peopled world has positive total population");
}
```

- [ ] **Step 2: Run to verify it fails** (`draw_population` still in use). Run: `cargo test -p hornvale-worldgen settlement_populations`. Expected: FAIL to compile or assert once the wiring is mid-change; use it to drive Step 3.

- [ ] **Step 3: Rewire the stage.** In the `climate+settlements` closure:
  - Build one `CellMap<CarryingInput>` from the same per-cell terrain/climate reads already assembled for `SiteInput` (`climate.mean_temperature_at`, `climate.moisture_at`, the existing `freshwater`/`coastal`/`hostility` computations, `climate.habitability().get(cell)`).
  - For each species in `species_set`, derive its per-species K inputs (fold the psychology weighting that `species_weights` applied into `CarryingInput` — e.g. scale `hostility`/`freshwater` per `def.psych` as `species_weights` did).
  - Call `hornvale_demography::report(geo, &per_species_inputs, THRESHOLD)`; replace `placements: Vec<(Placement, u32)>` with `report.settlements: Vec<(Condensation, u32)>`.
  - Everywhere the naming/lexicon/gloss loop read `p.cell` / `p.suitability`, read `n.cell` / `n.population`. **Delete** the `draw_population`/`draw_species_population` branch — `PlacedSettlement.population = n.population.round() as u32` (round the field readout; document the quantization at this emit boundary).
  - `THRESHOLD` is a `const` in worldgen for now; Task 8 tunes and freezes it.
- [ ] **Step 4: Retire the scatter API.** Delete `draw_population`/`draw_species_population` from `settlement/src/lib.rs` and `place`/`place_tagged`/`suitability`/`suitability_weighted`/`SuitabilityWeights`/`SiteInput`/`Placement`/`founder_pass` from `settlement/src/placement.rs` (and its re-exports in `settlement/src/lib.rs`). Remove `settlement/src/placement.rs` entirely if nothing remains. Update `worldgen`'s `species_weights` to produce `CarryingInput` scaling instead of `SuitabilityWeights`.
- [ ] **Step 5: Retire the pin.** In `settlement_pins.rs` remove `min_suitability` and its parse arm; keep `species`. In `worldgen`, an incoming legacy `settlement-pin` fact for `min-suitability` is ignored (unknown-pin path). Delete the `min_suitability_pin_reduces_the_settlement_count` test.
- [ ] **Step 6: Verify** the workspace compiles and the new test passes.

Run: `cargo test -p hornvale-worldgen settlement_populations && cargo build --workspace`
Expected: PASS + clean build.

- [ ] **Step 7: Determinism check.** Run the existing worldgen determinism/byte-identity test (grep for `byte` / `deterministic` in `windows/worldgen/src/lib.rs` tests) and confirm green:

Run: `cargo test -p hornvale-worldgen -- determinism byte identical 2>&1 | tail`
Expected: PASS (same seed → identical world).

- [ ] **Step 8: Commit**

```bash
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add domains/settlement windows/worldgen
git commit -m "feat(worldgen): condense settlements via demography; retire suitability scatter + population draw"
```

### Task 8: Calibrate K + threshold to Zipf; add Lab metrics and the study

**Files:**
- Modify: `domains/demography/src/carrying_capacity.rs` (freeze constants)
- Modify: `windows/worldgen/src/lib.rs` (freeze `THRESHOLD`)
- Modify: `windows/lab/src/metrics.rs` (add settlement metrics)
- Create: `studies/census-of-the-gathering.study.json` (preregistered)
- Test: `windows/lab/tests/` (a calibration assertion, mirroring `calibration.rs`)

**Interfaces:**
- Produces: named Lab metrics `settlement-count`, `total-population`, `rank-size-slope`, `pop-weighted-abs-latitude`, read from `SettlementView`.

- [ ] **Step 1: Add the metrics.** In `metrics.rs`, following the existing `Extractor::Settlement(fn(&SettlementView) -> MetricValue)` entries, add extractors that read the ledger via `view.world()`:
  - `settlement-count` = count of `IS_SETTLEMENT` facts.
  - `total-population` = Σ `POPULATION`.
  - `rank-size-slope` = the log-log least-squares slope of settlement population vs rank (a pure function; write it inline, no new dep).
  - `pop-weighted-abs-latitude` = Σ(pop·|lat|)/Σpop, reading `LATITUDE`.
  Register each in the named-metric table exactly where the other settlement metrics are registered.

- [ ] **Step 2: Write the preregistered study** `studies/census-of-the-gathering.study.json`, mirroring an existing census study's shape (`census-of-peoples.study.json`), selecting the four new metrics over seeds `0..=199`, default pins. Add its **hypothesis** field (the preregistration): `rank-size-slope` mean ∈ [−1.2, −0.8]; `pop-weighted-abs-latitude` below the uniform-sphere baseline (people concentrate off the poles).

- [ ] **Step 3: Run the study, read the distributions.**

Run: `cargo run -p hornvale -- lab run studies/census-of-the-gathering.study.json`
Expected: a rows.csv + summary under `book/src/laboratory/generated/census-of-the-gathering/`.

- [ ] **Step 4: Tune once.** Adjust `carrying_capacity.rs`'s constants and worldgen's `THRESHOLD` until the study's `rank-size-slope` lands in the preregistered band and the latitude gradient is off-pole. Re-run Step 3 to confirm. **Then freeze** the constants (update the `PLACEHOLDER` comment to `CALIBRATED (the-gathering, 2026-07-13); frozen save-format constant`).

- [ ] **Step 5: Add the calibration guard test**, mirroring `windows/lab/tests/calibration.rs`: assert the frozen fixture's `rank-size-slope` stays in-band and total-population conservation holds at the world level.

Run: `cargo test -p hornvale-lab calibration`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add domains/demography windows/worldgen windows/lab studies book/src/laboratory/generated
git commit -m "feat(lab): the-gathering calibration — Zipf rank-size + conservation, constants frozen"
```

---

## Stage 3 — Rebaseline and the book

### Task 9: Rebaseline drift-checked artifacts

**Files:**
- Modify: `book/src/gallery/*.md` (the three seed-42 almanacs), the elevation/settlement map, lab study outputs under `book/src/laboratory/generated/`, any golden pin fixtures.

- [ ] **Step 1: Regenerate every committed artifact** using the authoritative command list in `.github/workflows/ci.yml`'s "Artifacts are current" step (and `scripts/regenerate-artifacts.sh`):

```bash
cargo run -p hornvale-kernel --example first_light
cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json
cargo run -p hornvale -- almanac --world /tmp/hv.json > book/src/gallery/almanac-seed-42-sky.md
# ...the full list from ci.yml, plus the settlement map and each affected study
bash scripts/regenerate-artifacts.sh
```

- [ ] **Step 2: Review the diff as evidence.** `git diff book/` — confirm the changes are the expected settlement relocations/renames and population shifts (the scatter→condensation change), not spurious churn elsewhere. Re-pin any golden calibration values that moved, **in this commit**.

- [ ] **Step 3: Confirm the drift gate is clean.**

Run: `git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/`
Expected: no diff after regeneration (exit 0).

- [ ] **Step 4: Commit**

```bash
git add book/ studies/
git commit -m "chore(artifacts): rebaseline for the-gathering condensation (deliberate regeneration)"
```

### Task 10: Book, registry, decisions, retrospective

**Files:**
- Create: `book/src/chronicle/the-gathering.md` (+ `SUMMARY.md` entry)
- Modify: settlement/placement book chapters (freshness sweep — describe condensation, not the scatter)
- Modify: `book/src/frontier/idea-registry.md` (MAP-7 → `spec'd`; MAP-22 cross-link)
- Modify: `book/src/open-questions.md` (partial rescore — field landed, history pending)
- Create: `docs/decisions/NNNN-*.md` × 3 (demography crate; replace-not-coexist; founder-floor migration) — numbered in creation order (decision 0043)
- Create: `docs/retrospectives/the-gathering.md`

- [ ] **Step 1: Write the chronicle entry** at the book's altitude (technical, comprehensible without the code) — the carrying-capacity field, the flow-condensation, the conservation invariant, the Zipf calibration, and the deliberate field/history seam.
- [ ] **Step 2: Freshness sweep.** Update the settlement/placement book chapters that describe the retired suitability scatter. Grep the book for "suitability" and "scatter" and reconcile.
- [ ] **Step 3: Registry.** Flip MAP-7 to `spec'd` (repoint **Where** at the spec); add a MAP-22 cross-link noting the founder floor migrated to `demography`. Registry IDs stay in `book/src/frontier/` only.
- [ ] **Step 4: Decisions.** Write the three decision records (see the spec's "Decisions to ratify"), numbered per creation order.
- [ ] **Step 5: Confidence Gradient.** Partial rescore in `open-questions.md`: the calibration-checked field landed; the "vary time" horizon bet rides on the history campaign.
- [ ] **Step 6: Retrospective.** One page in `docs/retrospectives/` — process lessons (the ideonomy-driven condensation reframe, the drainage-kinship reuse), not product.
- [ ] **Step 7: Verify docs consistency + the full gate.**

Run: `cargo test -p hornvale --test docs_consistency && make gate`
Expected: PASS (registry/ToC/ID drift-check green; full commit gate green).

- [ ] **Step 8: Commit**

```bash
git add book docs
git commit -m "docs(the-gathering): chronicle, registry, decisions, retrospective; close the campaign"
```

---

## Self-Review

**Spec coverage:**
- §1 Architecture (demography crate, founder migration, settlement surface, layering) → Tasks 1, 5, 7.
- §2 Carrying-capacity field K (Miami proxy, field/grid contract, per-species) → Tasks 2, 7 (per-species inputs), 8 (freeze).
- §3 Flow-accumulation & condensation (drainage-flipped, conservation, determinism, flagship) → Tasks 3, 4, 5.
- §4 Ledger facts & residual field (unchanged predicates, in-memory report, null control) → Tasks 6, 7.
- §5 Verification (conservation test, Zipf study, latitude gradient, determinism, artifact rebaseline) → Tasks 3/4 (conservation), 7 (determinism), 8 (Zipf/latitude), 9 (rebaseline).
- §6 Book & DoD → Task 10.
- Retired `--min-suitability` pin (§1 addendum) → Task 7 Step 5.
- Decisions to ratify → Task 10 Step 4.

**Placeholder scan:** the only `PLACEHOLDER` is the K constants in Task 2, explicitly frozen in Task 8 — intentional, not a plan gap. No TBD/TODO elsewhere.

**Type consistency:** `CarryingInput` (Task 2) is consumed by `report` (Task 6) and worldgen (Task 7). `Condensation{cell,position,population}` (Task 4) flows through `condense_tagged` (Task 5), `DemographyReport` (Task 6), worldgen (Task 7). `Flow{accumulation,attractor}` (Task 3) is internal to `condense` (Task 4). `report`/`condense`/`condense_tagged` signatures match across tasks.

**Known adaptation points** (the executor reconciles against real signatures): the exact `build_world` entry name/signature in Task 7's test (`build_world_with_roster` / `build_world_to` exist — match the shipped one); `math::fmin` vs `.min`; the PPM byte type in `paleoclimate::render`; the exact named-metric registration site in `metrics.rs`.
