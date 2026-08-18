# The Gazetteer Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Landscape features — landmasses, seas, salt lakes, rivers and
volcanoes — become individuated objects with draw-free identities and
per-culture names, so a later campaign can draw a map whose *names* are the
fogged layer.

**Architecture:** A `FeatureIndex` computed once per terrain and held on
`GeneratedTerrain` (the discipline `ChannelNetwork` already uses), built by
graph traversal over already-committed fields — connected components under
`Geosphere::neighbors` for landmass/sea/salt-lake, maximal subtrees of
`downhill_targets`' flow forest for rivers. Naming joins terrain to language at
the composition root (`windows/worldgen`), because a `domains/terrain` ->
`domains/language` edge would be a sibling dependency the architecture test
forbids.

**Tech Stack:** Rust 2024, no new dependencies (workspace allowlist is
`serde`, `serde_json`, `libm` only — decisions 0004/0041).

**Spec:** `docs/superpowers/specs/2026-08-18-the-gazetteer-design.md`

## Global Constraints

- **No `HashMap`/`HashSet`.** `BTreeMap`/`BTreeSet`/`Vec` only, enforced
  workspace-wide by `clippy.toml` `disallowed-types`.
- **No wall-clock time.** Same lint. `Instant` is banned in tests too.
- **No new stream label and no epoch.** Names reuse
  `language/<species>/name/landform`. Decisions 0083 (a label is declared per
  *algorithm*) and 0084 (an epoch only when a derivation moved).
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field and
  variant gets a one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` verdict tag.**
  Default-deny: an untagged pub-boundary primitive fails the gate.
- **`cargo fmt` is the final step before every commit.** Fmt-gate skips are the
  most common review finding.
- **Magnitudes are integers.** No float sort should appear anywhere in this
  campaign. If you reach for `total_cmp`, something has gone wrong — report it.
- **Gate:** `make gate-commit` before every commit (a `domains/`-layer edit
  costs ~84 s).
- **A SYNTHETIC-PREDICATE TEST MUST ASSERT ITS FIXTURE IS NON-DEGENERATE.**
  Every test in this plan that builds components from a toy predicate like
  `|c| c.0 % 7 < 4` must assert, in the test body, that the predicate actually
  yields enough components to exercise what the test claims to check:

  ```rust
  assert!(comps.len() >= N, "predicate must yield many components, got {}", comps.len());
  ```

  Pick `N` from what your predicate really produces — measure it, do not copy a
  number from another test. **This rule exists because the plan author got it
  wrong**: `!c.0.is_multiple_of(3)` on `Geosphere::new(3)` yields exactly ONE
  component, so the partition test written against it could not fail for the
  reason it was written, and `c.0 % 5 < 3` yields two, so the ordering test
  would pass about half the time under a shuffled-order bug. The predicates
  below Task 2 are from the same family and are assumed guilty until measured.
  A test that cannot fail is worse than no test, because it produces evidence.

  For any test about **ordering**, additionally confirm the component sizes are
  not monotonically related to identity order, and say so in the doc comment —
  a fixture where size order and identity order coincide cannot tell them apart.

- **Run the suite ONCE, inspect many.** Capture to a file and grep it; never
  re-run to ask a second question.

---

### Task 1: Probe — measure before choosing any floor

Settles spec F1 (committed prose contradicts itself: the chronicle says 123
named rivers, the Watershed spec says 115 at the naming tier) and F5 (cost).
**No floor anywhere in this campaign is chosen until this task reports.**

**Files:**
- Create: `windows/worldgen/examples/gazetteer_probe.rs`

**Interfaces:**
- Consumes: nothing (first task).
- Produces: measured counts that Tasks 3-5 read when choosing floors. No code
  any later task calls — this is an example binary, deleted at Task 10.

- [ ] **Step 1: Find the real world-building entry point FIRST**

The probe needs a seed-42 `GeneratedTerrain`. Do not guess the constructor.

Run: `grep -rn "GeneratedTerrain" windows/worldgen/src/volcano.rs | head -20`

`volcano.rs`'s test module builds a terrain provider directly and is the
closest existing idiom. Use it, or whatever is cheapest that yields a real
`GeneratedTerrain`, and **report which you used** — later tasks reuse it.

- [ ] **Step 2: Write the probe**

```rust
//! Throwaway probe (The Gazetteer, Task 1): how many components of each class
//! exist at seed 42 across a range of floors, and what the traversal costs.
//! Deleted at Task 10 — this measures, it does not ship.

use hornvale_kernel::{CellId, Geosphere};
use hornvale_terrain::water::WaterKind;
use std::collections::{BTreeMap, BTreeSet, VecDeque};

/// Connected components of the cells satisfying `member`, as cell sets.
fn components(geo: &Geosphere, member: &dyn Fn(CellId) -> bool) -> Vec<BTreeSet<CellId>> {
    let mut visited = vec![false; geo.cell_count()];
    let mut out = Vec::new();
    for start in geo.cells() {
        if visited[start.0 as usize] || !member(start) {
            continue;
        }
        visited[start.0 as usize] = true;
        let mut queue = VecDeque::from([start]);
        let mut set = BTreeSet::new();
        while let Some(cell) = queue.pop_front() {
            set.insert(cell);
            for &nb in geo.neighbors(cell) {
                if !visited[nb.0 as usize] && member(nb) {
                    visited[nb.0 as usize] = true;
                    queue.push_back(nb);
                }
            }
        }
        out.push(set);
    }
    out
}

fn report(label: &str, sizes: &mut Vec<usize>, floors: &[usize]) {
    sizes.sort_unstable_by(|a, b| b.cmp(a));
    println!("{label}: {} components", sizes.len());
    println!("  ten largest: {:?}", &sizes[..sizes.len().min(10)]);
    for f in floors {
        println!("  floor {f:>4}: {}", sizes.iter().filter(|s| *s >= f).count());
    }
}

fn main() {
    // Replace with the constructor found in Step 1.
    let terrain = build_seed_42_terrain();
    let geo = terrain.geosphere();
    let globe = terrain.globe();

    let land = |c: CellId| *globe.elevation.get(c) >= globe.sea_level;
    let ocean = |c: CellId| terrain.water_kind_at(c) == WaterKind::Ocean;
    let salt = |c: CellId| terrain.water_kind_at(c) == WaterKind::SaltBasin;

    for (label, pred) in [
        ("landmass", &land as &dyn Fn(CellId) -> bool),
        ("sea", &ocean),
        ("salt-lake", &salt),
    ] {
        let mut sizes: Vec<usize> = components(geo, pred).iter().map(BTreeSet::len).collect();
        report(label, &mut sizes, &[1, 5, 10, 20, 50, 100]);
    }

    // Rivers: partition land cells by the terminal of their downhill chain.
    let downhill =
        hornvale_terrain::drainage::downhill_targets(geo, &globe.elevation, globe.sea_level);
    let mut catchment: BTreeMap<CellId, usize> = BTreeMap::new();
    for c in geo.cells() {
        if !land(c) {
            continue;
        }
        let mut at = c;
        // Bounded by cell_count: the flow forest is acyclic by construction
        // (every hop strictly decreases elevation). Belt-and-braces.
        for _ in 0..geo.cell_count() {
            match downhill[at.0 as usize] {
                Some(next) => at = next,
                None => break,
            }
        }
        *catchment.entry(at).or_default() += 1;
    }
    let mut sizes: Vec<usize> = catchment.values().copied().collect();
    report("river (by catchment)", &mut sizes, &[4, 12, 24, 50, 100]);
}
```

- [ ] **Step 3: Run the probe, capturing its full output**

```bash
cargo run -p hornvale-worldgen --example gazetteer_probe > /tmp/hv-gazetteer-probe.txt 2>&1
echo "exit=$?"
cat /tmp/hv-gazetteer-probe.txt
```

Do NOT pipe through `tail` or `head`. A truncated tail is indistinguishable
from a clean run, and this output IS the deliverable.

- [ ] **Step 4: Time the traversal separately from the world build**

The world build dominates and is not this campaign's cost. Print the elapsed
traversal time from inside the probe using the crate's own timing idiom — NOT
`std::time::Instant`, which the workspace lint bans. If no non-`Instant` timing
idiom exists in this crate, say so and time the whole binary externally with
`time`, reporting that the traversal share is therefore an upper bound.

**Decision rule:**
- Traversal under 5% of the terrain build -> proceed with an eager index (Task 5).
- 5% or more -> report it and put the index behind a lazy seam instead.

- [ ] **Step 5: Report**

State explicitly:
- the count per class at each floor;
- which committed figure the river count matches — 115, 123, or **neither**;
- the cost and which branch of Step 4's rule it selects;
- which world constructor you used.

- [ ] **Step 6: Commit the probe only**

```bash
cargo fmt
make gate-commit
git add windows/worldgen/examples/gazetteer_probe.rs
git commit -m "probe(the-gazetteer): measure component counts and traversal cost"
```

---

### Task 2: Component traversal, with `land_component_sizes` as its positive control

**Files:**
- Create: `domains/terrain/src/landscape.rs`
- Modify: `domains/terrain/src/lib.rs` (add `pub mod landscape;`)
- Modify: `domains/terrain/src/shape.rs:156-184` (`land_component_sizes`)
- Test: in-module `#[cfg(test)]` in `landscape.rs`

> **Why `landscape.rs` and not `features.rs`:** `domains/terrain/src/features.rs`
> already exists and is a different thing. Confirm with
> `ls domains/terrain/src/features*` before starting.

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `pub fn components(geo: &Geosphere, member: impl Fn(CellId) -> bool) -> Vec<BTreeSet<CellId>>`,
  returned in ascending order of each component's lowest cell id. Tasks 3-5
  call this.

- [ ] **Step 1: Write the failing tests**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    /// On a geosphere every cell neighbours another, so an all-true predicate
    /// must yield exactly one component holding every cell.
    #[test]
    fn an_all_true_predicate_yields_one_component_of_every_cell() {
        let geo = Geosphere::new(3);
        let comps = components(&geo, |_| true);
        assert_eq!(comps.len(), 1, "the whole sphere is connected");
        assert_eq!(comps[0].len(), geo.cell_count());
    }

    /// The empty case is a legitimate answer, not a panic.
    #[test]
    fn an_all_false_predicate_yields_no_components() {
        let geo = Geosphere::new(3);
        assert!(components(&geo, |_| false).is_empty());
    }

    /// Every member appears in exactly one component. THIS is what makes
    /// "lowest cell id" a valid identity — if a cell could appear twice, two
    /// features would claim it.
    #[test]
    fn components_partition_their_members() {
        let geo = Geosphere::new(3);
        let member = |c: CellId| c.0 % 3 != 0;
        let comps = components(&geo, member);
        let mut seen = BTreeSet::new();
        for comp in &comps {
            for cell in comp {
                assert!(seen.insert(*cell), "cell {cell:?} appeared in two components");
            }
        }
        let expected: BTreeSet<CellId> = geo.cells().filter(|c| member(*c)).collect();
        assert_eq!(seen, expected, "every member is in exactly one component");
    }

    /// Components arrive in identity order, so no call site has to sort.
    #[test]
    fn components_are_ordered_by_their_lowest_cell_id() {
        let geo = Geosphere::new(3);
        let comps = components(&geo, |c| c.0 % 5 < 3);
        let firsts: Vec<u32> = comps.iter().map(|c| c.first().expect("nonempty").0).collect();
        let mut sorted = firsts.clone();
        sorted.sort_unstable();
        assert_eq!(firsts, sorted, "components must arrive in identity order");
    }
}
```

- [ ] **Step 2: Run to verify they fail**

```bash
cargo test -p hornvale-terrain --lib landscape:: > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^test result|error\[|not defined|cannot find" /tmp/hv.log
```

Expected: compile error — `components` cannot be found.

- [ ] **Step 3: Implement**

```rust
//! Landscape features: individuated regions of the world with draw-free
//! identities. See `docs/superpowers/specs/2026-08-18-the-gazetteer-design.md`.

use hornvale_kernel::{CellId, Geosphere};
use std::collections::{BTreeSet, VecDeque};

/// Connected components of the cells satisfying `member`, under
/// [`Geosphere::neighbors`].
///
/// Returned in ascending order of each component's lowest cell id — the
/// identity the caller assigns — so no call site has to sort. Traversal only:
/// no draws, no transcendentals, integer comparisons, so cross-platform
/// byte-identical.
///
/// The components **partition** the members: every satisfying cell appears in
/// exactly one. That is what makes "lowest cell id" a valid identity, and it
/// is asserted rather than assumed.
/// type-audit: bare-ok(count: return)
pub fn components(geo: &Geosphere, member: impl Fn(CellId) -> bool) -> Vec<BTreeSet<CellId>> {
    let mut visited = vec![false; geo.cell_count()];
    let mut out = Vec::new();
    for start in geo.cells() {
        if visited[start.0 as usize] || !member(start) {
            continue;
        }
        visited[start.0 as usize] = true;
        let mut queue = VecDeque::from([start]);
        let mut set = BTreeSet::new();
        while let Some(cell) = queue.pop_front() {
            set.insert(cell);
            for &nb in geo.neighbors(cell) {
                if !visited[nb.0 as usize] && member(nb) {
                    visited[nb.0 as usize] = true;
                    queue.push_back(nb);
                }
            }
        }
        out.push(set);
    }
    out
}
```

Outer iteration is ascending `geo.cells()`, so components emerge in lowest-id
order for free. **Do not add a sort.** If Step 1's ordering test passes without
one, that IS the property; adding a sort would mask a future regression.

- [ ] **Step 4: Run to verify they pass**

```bash
cargo test -p hornvale-terrain --lib landscape:: > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv.log
```

Expected: `test result: ok. 4 passed`.

- [ ] **Step 5: Refactor `land_component_sizes` onto it**

This is spec F2's positive control. `land_component_sizes` feeds the
`landmass-count` and `continent-count` census metrics; its output must not
move. Rewrite it as a thin caller:

```rust
pub fn land_component_sizes(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
) -> Vec<usize> {
    let mut sizes: Vec<usize> =
        crate::landscape::components(geo, |c| *elevation.get(c) >= sea_level)
            .iter()
            .map(std::collections::BTreeSet::len)
            .collect();
    sizes.sort_unstable_by(|a, b| b.cmp(a));
    sizes
}
```

- [ ] **Step 6: Prove the refactor moved nothing**

```bash
cargo test -p hornvale-terrain > /tmp/hv-terrain.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv-terrain.log
grep -E "FAILED|panicked" /tmp/hv-terrain.log
```

**Decision rule:**
- All green -> the refactor is transparent; proceed.
- Any `shape::` or census-metric test red -> **STOP.** The new components and
  the old traversal disagree. Report which test with its output. Do not adjust
  the test to match.

- [ ] **Step 7: Commit**

```bash
cargo fmt
make gate-commit
git add domains/terrain/src/landscape.rs domains/terrain/src/lib.rs domains/terrain/src/shape.rs
git commit -m "feat(terrain): connected components as cell sets, not just sizes"
```

---

### Task 3: The component classes

**Files:**
- Modify: `domains/terrain/src/landscape.rs`
- Test: in-module `#[cfg(test)]`

**Interfaces:**
- Consumes: `components` (Task 2).
- Produces:
  - `pub enum FeatureClass { Volcano = 0, Landmass = 1, Sea = 2, SaltLake = 3, River = 4 }`
    — **discriminants are load-bearing**, see Task 6.
  - `pub struct FeatureId { pub class: FeatureClass, pub cell: CellId }`
  - `pub struct Feature { pub id: FeatureId, pub extent: BTreeSet<CellId>, pub anchor: CellId, pub magnitude: u32 }`
  - `pub fn classify(geo, class, member, floor) -> Vec<Feature>`

- [ ] **Step 1: Write the failing tests**

**Apply the non-degenerate-fixture rule from Global Constraints to every test
below.** The predicates shown (`c.0 % 7 < 4`, `c.0 % 4 == 0`) are inherited
from the same family that proved vacuous in Task 2 — measure what each yields
on `Geosphere::new(3)` before trusting it, and change the predicate if it
yields too few. `the_floor_excludes_components_below_it` in particular needs
components on BOTH sides of floor 3, or its `floored.len() < all.len()`
assertion is not testing a floor.

```rust
    /// A feature's identity is the lowest cell in its extent — canonical,
    /// integer, needing no tie-break.
    #[test]
    fn a_features_identity_is_the_lowest_cell_of_its_extent() {
        let geo = Geosphere::new(3);
        for f in classify(&geo, FeatureClass::Landmass, |c| c.0 % 7 < 4, 1) {
            assert_eq!(f.id.cell, *f.extent.first().expect("nonempty extent"));
        }
    }

    /// The floor EXCLUDES small components rather than shrinking them: a rock
    /// is not a small continent, it is not a continent.
    #[test]
    fn the_floor_excludes_components_below_it() {
        let geo = Geosphere::new(3);
        let all = classify(&geo, FeatureClass::Landmass, |c| c.0 % 7 < 4, 1);
        let floored = classify(&geo, FeatureClass::Landmass, |c| c.0 % 7 < 4, 3);
        assert!(floored.len() < all.len(), "a floor of 3 must exclude something");
        assert!(floored.iter().all(|f| f.magnitude >= 3));
        for f in &floored {
            assert!(all.iter().any(|a| a.id == f.id), "a floor must not invent a feature");
        }
    }

    /// Magnitude is extent size, so the Task 5 ordering is over an integer.
    #[test]
    fn magnitude_is_the_extent_size() {
        let geo = Geosphere::new(3);
        for f in classify(&geo, FeatureClass::Sea, |c| c.0 % 4 == 0, 1) {
            assert_eq!(f.magnitude as usize, f.extent.len());
        }
    }
```

- [ ] **Step 2: Run to verify they fail**

```bash
cargo test -p hornvale-terrain --lib landscape:: > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^test result|cannot find" /tmp/hv.log
```

- [ ] **Step 3: Implement**

```rust
/// What kind of thing a feature is.
///
/// **The discriminants are load-bearing and must not be reordered.** They form
/// the high half of the naming salt (`windows/worldgen`), so changing one
/// renames every feature of that class in every world. `Volcano = 0`
/// deliberately: it makes a volcano's salt equal its bare cell id, which is
/// exactly what `volcano_name` already draws with, so adopting the scheme
/// moves no volcano name.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum FeatureClass {
    /// A volcanic edifice, individuated by `volcano_at` rather than here.
    Volcano = 0,
    /// A connected component of land.
    Landmass = 1,
    /// A connected component of ocean.
    Sea = 2,
    /// A connected component of endorheic salt-sink cells.
    SaltLake = 3,
    /// A maximal subtree of the flow forest.
    River = 4,
}

/// A feature's stable identity: its class and its canonical cell.
/// type-audit: bare-ok(index: cell)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FeatureId {
    /// Which kind of feature.
    pub class: FeatureClass,
    /// The canonical cell — a component's lowest, or a river's terminal.
    pub cell: CellId,
}

/// An individuated region of the world.
/// type-audit: bare-ok(count: magnitude)
#[derive(Clone, Debug)]
pub struct Feature {
    /// The stable identity.
    pub id: FeatureId,
    /// Every cell the feature occupies.
    pub extent: BTreeSet<CellId>,
    /// The cell a label is drawn at.
    pub anchor: CellId,
    /// The integer scalar ranking this feature within its class.
    pub magnitude: u32,
}

/// Every component of `member` at or above `floor` cells, as features of
/// `class`. Below the floor a component is not a small feature — it is not a
/// feature, and stays anonymous.
/// type-audit: bare-ok(count: floor), bare-ok(count: return)
pub fn classify(
    geo: &Geosphere,
    class: FeatureClass,
    member: impl Fn(CellId) -> bool,
    floor: usize,
) -> Vec<Feature> {
    components(geo, member)
        .into_iter()
        .filter(|extent| extent.len() >= floor)
        .map(|extent| {
            let cell = *extent.first().expect("components are nonempty");
            Feature {
                id: FeatureId { class, cell },
                anchor: cell,
                magnitude: extent.len() as u32,
                extent,
            }
        })
        .collect()
}
```

**On `anchor`:** it is the identity cell this campaign. A better anchor (a
centroid, a pole of inaccessibility) is a *map* concern and belongs to the
campaign that has labels to place. Do not invent one here.

- [ ] **Step 4: Run to verify they pass**

```bash
cargo test -p hornvale-terrain --lib landscape:: > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv.log
```

Expected: `7 passed`.

- [ ] **Step 5: Commit**

```bash
cargo fmt
make gate-commit
git add domains/terrain/src/landscape.rs
git commit -m "feat(terrain): FeatureClass, FeatureId and component classification"
```

---

### Task 4: Rivers off the flow forest

**Files:**
- Modify: `domains/terrain/src/landscape.rs`
- Test: in-module `#[cfg(test)]`

**Interfaces:**
- Consumes: `Feature`, `FeatureId`, `FeatureClass` (Task 3);
  `crate::drainage::downhill_targets` (existing).
- Produces: `pub fn rivers(geo, elevation, sea_level, floor: usize) -> Vec<Feature>`.

- [ ] **Step 1: Build the fixture, and make it non-vacuous**

`sloped_test_globe() -> (Geosphere, CellMap<ReferenceElevation>, ReferenceElevation)`
does not exist. `domains/terrain/src/render.rs` and `shape.rs` test modules
both build `CellMap`s; copy the closest idiom.

The fixture **must yield at least two distinct terminals**, or the partition
test below passes vacuously with one river holding everything. Assert that
inside the helper:

```rust
assert!(
    rivers(&geo, &elev, sea, 1).len() >= 2,
    "fixture is vacuous: it must produce more than one catchment"
);
```

- [ ] **Step 2: Write the failing tests**

```rust
    /// Every land cell belongs to exactly one catchment — the river analogue
    /// of `components_partition_their_members`, and what makes the terminal a
    /// valid identity.
    #[test]
    fn every_land_cell_belongs_to_exactly_one_catchment() {
        let (geo, elev, sea) = sloped_test_globe();
        let mut seen = BTreeSet::new();
        for r in rivers(&geo, &elev, sea, 1) {
            for cell in &r.extent {
                assert!(seen.insert(*cell), "cell {cell:?} is in two catchments");
            }
        }
        let land: BTreeSet<CellId> = geo.cells().filter(|c| *elev.get(*c) >= sea).collect();
        assert_eq!(seen, land, "every land cell drains somewhere");
    }

    /// A river's identity is a fixed point of the downhill map.
    #[test]
    fn a_rivers_identity_is_a_terminal_of_the_flow_forest() {
        let (geo, elev, sea) = sloped_test_globe();
        let downhill = crate::drainage::downhill_targets(&geo, &elev, sea);
        for r in rivers(&geo, &elev, sea, 1) {
            assert!(
                downhill[r.id.cell.0 as usize].is_none(),
                "terminal {:?} still points downhill",
                r.id.cell
            );
        }
    }

    /// The floor cuts on CATCHMENT, not mouth drainage (spec 3.1).
    #[test]
    fn the_floor_cuts_on_catchment_size() {
        let (geo, elev, sea) = sloped_test_globe();
        let all = rivers(&geo, &elev, sea, 1);
        let big = rivers(&geo, &elev, sea, 4);
        assert!(big.iter().all(|r| r.magnitude >= 4));
        assert!(big.iter().all(|r| r.extent.len() == r.magnitude as usize));
        assert!(big.len() <= all.len());
    }
```

- [ ] **Step 3: Run to verify they fail**

```bash
cargo test -p hornvale-terrain --lib landscape:: > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^test result|cannot find" /tmp/hv.log
```

- [ ] **Step 4: Implement**

```rust
/// Every river catchment at or above `floor` cells.
///
/// A river is a maximal subtree of the flow forest [`crate::drainage::downhill_targets`]
/// gives, and its identity is that subtree's **terminal** — the ocean cell it
/// empties into, or the interior minimum it dies in. Magnitude is catchment
/// size, so the naming tier is how much land a river drains rather than how
/// much water crosses its mouth.
///
/// The extent holds the **land** that drains to the terminal; when the
/// terminal is an ocean cell it is not itself a member. The partition property
/// in this module's tests pins that.
///
/// The walk is bounded by `cell_count` because the flow forest is acyclic by
/// construction — every hop strictly decreases elevation. The bound is
/// belt-and-braces, not a real termination condition.
/// type-audit: bare-ok(count: floor), bare-ok(count: return)
pub fn rivers(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    floor: usize,
) -> Vec<Feature> {
    let downhill = crate::drainage::downhill_targets(geo, elevation, sea_level);
    let mut catchments: BTreeMap<CellId, BTreeSet<CellId>> = BTreeMap::new();
    for c in geo.cells() {
        if *elevation.get(c) < sea_level {
            continue;
        }
        let mut at = c;
        for _ in 0..geo.cell_count() {
            match downhill[at.0 as usize] {
                Some(next) => at = next,
                None => break,
            }
        }
        catchments.entry(at).or_default().insert(c);
    }
    catchments
        .into_iter()
        .filter(|(_, extent)| extent.len() >= floor)
        .map(|(terminal, extent)| Feature {
            id: FeatureId { class: FeatureClass::River, cell: terminal },
            anchor: terminal,
            magnitude: extent.len() as u32,
            extent,
        })
        .collect()
}
```

- [ ] **Step 5: Run to verify they pass**

```bash
cargo test -p hornvale-terrain --lib landscape:: > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv.log
```

Expected: `10 passed`.

- [ ] **Step 6: Commit**

```bash
cargo fmt
make gate-commit
git add domains/terrain/src/landscape.rs
git commit -m "feat(terrain): rivers as maximal subtrees of the flow forest"
```

---

### Task 5: The index on the provider, and the total ordering

**Files:**
- Modify: `domains/terrain/src/landscape.rs`
- Modify: `domains/terrain/src/provider.rs:14-19` and its builder
- Test: in-module `#[cfg(test)]`

**Interfaces:**
- Consumes: `classify` (Task 3), `rivers` (Task 4).
- Produces:
  - `pub struct FeatureIndex`, `pub fn of(&self, class: FeatureClass) -> &[Feature]`,
    `pub fn all(&self) -> impl Iterator<Item = &Feature>`
  - `GeneratedTerrain::features(&self) -> &FeatureIndex`
- **Floors:** use the values Task 1 selected. Declare them as documented
  `pub const`s — they are shapes of the world, not tuning knobs, and the doc
  comment cites Task 1's measured counts.

- [ ] **Step 1: Write the failing tests**

**Apply the non-degenerate-fixture rule from Global Constraints.** These
ordering tests are the ones most damaged by a thin fixture: with two features
a shuffled order passes half the time, and if magnitude order happens to match
identity order the test cannot tell the two apart — which is precisely the bug
it exists to catch. Use a predicate yielding many features with sizes that are
NOT monotonic in identity, and assert both facts in the test.

```rust
    /// Within a class, features order by magnitude descending, ties broken by
    /// identity ascending. Total, deterministic, integer-only.
    #[test]
    fn features_order_by_magnitude_then_identity() {
        let geo = Geosphere::new(3);
        let feats = classify(&geo, FeatureClass::Landmass, |c| c.0 % 7 < 4, 1);
        let index = FeatureIndex::from_parts(vec![(FeatureClass::Landmass, feats)]);
        for pair in index.of(FeatureClass::Landmass).windows(2) {
            let (a, b) = (&pair[0], &pair[1]);
            assert!(
                a.magnitude > b.magnitude || (a.magnitude == b.magnitude && a.id.cell < b.id.cell),
                "ordering is not total: {:?} before {:?}",
                a.id,
                b.id
            );
        }
    }

    /// The ordering does not depend on input order.
    #[test]
    fn ordering_is_independent_of_input_order() {
        let geo = Geosphere::new(3);
        let feats = classify(&geo, FeatureClass::Landmass, |c| c.0 % 7 < 4, 1);
        let mut reversed = feats.clone();
        reversed.reverse();
        let a = FeatureIndex::from_parts(vec![(FeatureClass::Landmass, feats)]);
        let b = FeatureIndex::from_parts(vec![(FeatureClass::Landmass, reversed)]);
        let ids = |i: &FeatureIndex| -> Vec<FeatureId> {
            i.of(FeatureClass::Landmass).iter().map(|f| f.id).collect()
        };
        assert_eq!(ids(&a), ids(&b));
    }
```

- [ ] **Step 2: Run to verify they fail**

```bash
cargo test -p hornvale-terrain --lib landscape:: > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^test result|cannot find" /tmp/hv.log
```

- [ ] **Step 3: Implement `FeatureIndex`**

Sort key `(Reverse(magnitude), id.cell)` — integer-only, so no `total_cmp`
appears. `from_parts` is the constructor these tests and the real builder
share; mark it `pub(crate)`.

Document on the type: this ordering is the **placement channel**, the same
question `surrounds_ascii::box_rank` answers one rung down. It is not one of
decision 0142's three measurement axes — ordering a feature set is a claim
about what fits on the page, not about the world. **This campaign ships the
ordering and deliberately ships no salience banding**; the map campaign sets
bands against a rendered picture rather than inventing a number here.

- [ ] **Step 4: Populate the volcano class**

`classify` cannot build volcanoes — they are individuated by `volcano_at`, not
by a component walk. The index builder gathers them separately:

- walk the cells `volcano_at` reports an edifice for;
- group by the returned `Volcano`'s `source` cell, which **is** the identity
  (its two halves must not become two features — that is the reason
  `volcano_name` takes the volcano and not the query cell);
- extent is the edifice's cells, anchor and `id.cell` are `source`, magnitude
  is the edifice cell count.

Add the test that pins the grouping. **It lives in `windows/worldgen`, not
`domains/terrain`** — it calls `volcano_at`, and a `domains/` -> `windows/`
dependency is backwards and forbidden by `cli/tests/architecture.rs`. Put it
beside the code that builds the class, wherever Step 4's structural choice
lands that:

```rust
    /// A volcano's two halves are ONE feature. `volcano_at` answers per cell,
    /// so grouping by cell rather than by `source` would split every cone —
    /// the exact mistake `volcano_name`'s signature exists to prevent.
    #[test]
    fn a_volcanos_cells_group_into_one_feature_per_source() {
        let terrain = test_terrain();
        let index = terrain.features();
        for f in index.of(FeatureClass::Volcano) {
            for cell in &f.extent {
                let v = hornvale_worldgen::volcano_at(Seed(42), &terrain, *cell)
                    .expect("an edifice cell has a volcano");
                assert_eq!(v.source, f.id.cell, "cell {cell:?} grouped under the wrong source");
            }
        }
    }
```

**Layering note:** `volcano_at` lives in `windows/worldgen`, which
`domains/terrain` may not depend on — that edge is backwards and
`cli/tests/architecture.rs` forbids it. So the volcano class is populated
**where the other four are consumed**, not inside `domains/terrain`. Two
options; pick one and say which:

- `FeatureIndex` gains a `with_volcanoes(...)` builder called from
  `windows/worldgen`, leaving `domains/terrain` volcano-free; or
- the volcano class lives only in `windows/worldgen`'s gazetteer view over the
  terrain index.

**Decision rule:** if the map campaign will need volcanoes from the terrain
index alone, take the first; if only naming needs them, take the second. State
your reasoning in the report — this is the one structural choice the plan
deliberately leaves open, because it depends on code you will have read and
the plan author has not.

- [ ] **Step 5: Wire it onto the provider**

Add `features: FeatureIndex` to `GeneratedTerrain` (`provider.rs:14-19`), built
where `channels` is built, with an accessor mirroring `channels()` at
`provider.rs:565`. Follow that field exactly — it is the established discipline
for a derived structure held on the provider.

- [ ] **Step 6: Run the terrain suite**

```bash
cargo test -p hornvale-terrain > /tmp/hv-terrain.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv-terrain.log
grep -E "FAILED|panicked" /tmp/hv-terrain.log
```

**Decision rule:**
- Green -> proceed.
- Red on a timing or build-cost assertion -> Task 1 Step 4's second branch
  arriving late. Report the measurement and move the index behind a lazy seam.

- [ ] **Step 7: Commit**

```bash
cargo fmt
make gate-commit
git add domains/terrain/src/landscape.rs domains/terrain/src/provider.rs
git commit -m "feat(terrain): FeatureIndex on GeneratedTerrain, ordered by magnitude"
```

---

### Task 6: The injective salt, and the naming join

**Files:**
- Create: `windows/worldgen/src/gazetteer.rs`
- Modify: `windows/worldgen/src/lib.rs` (`pub mod gazetteer;` + re-exports)
- Test: in-module `#[cfg(test)]`

**Interfaces:**
- Consumes: `FeatureId`, `FeatureClass`, `FeatureIndex` (Tasks 3-5); `Namer`,
  `NameKind::Landform`, `GeneratedName`, `MorphOptions`, `Phonology` (existing).
- Produces:
  - `pub fn feature_salt(id: FeatureId) -> u64`
  - `pub fn feature_name(seed: Seed, id: FeatureId, species: &str, ph: &Phonology, morph: &MorphOptions) -> GeneratedName`

- [ ] **Step 1: Write the failing tests**

`test_phonology()` does not exist — `windows/worldgen/src/volcano.rs`'s test
module (around lines 480-525) already builds a `Phonology` and `MorphOptions`
for exactly this. Reuse that idiom; do not invent a second one.

```rust
    /// THE COLLISION THIS SCHEME EXISTS TO PREVENT. Two features of different
    /// classes sharing an identity cell must not draw the same name from the
    /// same people. Before the class entered the salt they did — silently,
    /// because two features sharing a name is a thing real toponymy does.
    #[test]
    fn two_classes_at_one_cell_do_not_share_a_name() {
        let (ph, morph) = test_phonology();
        let cell = CellId(1234);
        let a = feature_name(
            Seed(42), FeatureId { class: FeatureClass::Landmass, cell }, "aeldrin", &ph, &morph);
        let b = feature_name(
            Seed(42), FeatureId { class: FeatureClass::River, cell }, "aeldrin", &ph, &morph);
        assert_ne!(a.roman, b.roman, "a landmass and a river at cell 1234 share a name");
    }

    /// The salt is injective over (class, cell) — the property the test above
    /// only samples. Exhaustive over a range wide enough to cross the stride.
    #[test]
    fn the_salt_is_injective_over_class_and_cell() {
        let mut seen: BTreeMap<u64, FeatureId> = BTreeMap::new();
        for class in [
            FeatureClass::Volcano,
            FeatureClass::Landmass,
            FeatureClass::Sea,
            FeatureClass::SaltLake,
            FeatureClass::River,
        ] {
            for cell in 0u32..5000 {
                let id = FeatureId { class, cell: CellId(cell) };
                if let Some(prev) = seen.insert(feature_salt(id), id) {
                    panic!("salt collision between {prev:?} and {id:?}");
                }
            }
        }
    }

    /// A volcano's salt is its bare cell id — exactly what `volcano_name`
    /// already draws with, so this scheme moves no volcano name.
    #[test]
    fn a_volcanos_salt_is_its_bare_cell_id() {
        for cell in [0u32, 1, 4095, 99999] {
            let id = FeatureId { class: FeatureClass::Volcano, cell: CellId(cell) };
            assert_eq!(feature_salt(id), u64::from(cell));
        }
    }

    /// One landform, many names (spec H2), at unit scale. Task 7 measures it
    /// at world scale; this is the control that says the species salt reaches
    /// the draw at all.
    #[test]
    fn two_peoples_name_one_feature_differently() {
        let (ph, morph) = test_phonology();
        let id = FeatureId { class: FeatureClass::Landmass, cell: CellId(77) };
        let a = feature_name(Seed(42), id, "aeldrin", &ph, &morph);
        let b = feature_name(Seed(42), id, "khorrun", &ph, &morph);
        assert_ne!(a.roman, b.roman, "the species salt is not reaching the draw");
    }
```

- [ ] **Step 2: Run to verify they fail**

```bash
cargo test -p hornvale-worldgen --lib gazetteer:: > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^test result|cannot find" /tmp/hv.log
```

- [ ] **Step 3: Implement**

`feature_salt` is `((id.class as u64) << 32) | u64::from(id.cell.0)`.
`feature_name` mirrors `volcano_name`'s body exactly:
`Namer::new(&seed, species, ph).name(NameKind::Landform, feature_salt(id), morph)`.

Doc comment on `feature_salt` must state: injective over `(class, cell)` by
construction since `CellId` is a `u32`; without the class term a landmass and
a river sharing an identity cell draw the same name from the same people;
`Volcano = 0` makes a volcano's salt its bare cell id, byte-identical to the
existing draw.

- [ ] **Step 4: Run to verify they pass**

```bash
cargo test -p hornvale-worldgen --lib gazetteer:: > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv.log
```

Expected: `4 passed`. **If `a_volcanos_salt_is_its_bare_cell_id` fails**, a
discriminant moved — fix the enum, not the test.

- [ ] **Step 5: Extend the existing stream-isolation property**

`domains/language/src/naming.rs:2318` already holds that a landform draw moves
no existing kind's draw for any `(seed, species, salt)`. **Extend it to the new
salts** rather than writing a second copy — a duplicate property is two answers
to one question.

```bash
cargo test -p hornvale-language --lib naming:: > /tmp/hv.log 2>&1; echo "exit=$?"
grep -E "^test result" /tmp/hv.log
```

- [ ] **Step 6: Commit**

```bash
cargo fmt
make gate-commit
git add windows/worldgen/src/gazetteer.rs windows/worldgen/src/lib.rs
git commit -m "feat(worldgen): per-culture feature names on an injective salt"
```

---

### Task 7: H2 — measure per-culture divergence

**Files:**
- Create: `windows/worldgen/tests/gazetteer_naming.rs`

**Interfaces:**
- Consumes: `feature_name`, `FeatureIndex` (Tasks 5-6).
- Produces: the H2 readout. No code later tasks call.

- [ ] **Step 0: H1 — assert the counts land in Task 1's band**

Spec H1 says every class yields a total deterministic ordering (Task 5's tests
already hold that) **and** that the named-feature count at seed 42 falls inside
a band set from Task 1's measurement. Assert the second half here, in the same
file, with the band's provenance in the doc comment:

`test_terrain()` and `H1_BANDS` do not exist — build both. `H1_BANDS` is a
`[(FeatureClass, usize, usize)]` const whose values come from **Task 1's
reported counts**, with the probe output quoted in its doc comment so the
provenance is auditable. Do not take bands from the Watershed spec's figures;
they contradict its own chronicle, which is why Task 1 exists.

```rust
/// H1: the landscape is individuated at a useful granularity. The bands come
/// from Task 1's probe (quoted below), NOT from the Watershed spec's figures,
/// which contradict its own chronicle.
#[test]
fn seed_42_names_a_useful_number_of_features() {
    let terrain = test_terrain();
    let index = terrain.features();
    for (class, lo, hi) in H1_BANDS {
        let n = index.of(class).len();
        assert!(
            (lo..=hi).contains(&n),
            "{class:?}: {n} features, outside the preregistered band {lo}..={hi}"
        );
    }
}
```

**Decision rule:** a class yielding ONE feature covering nearly everything
means the floor is too low to discriminate; hundreds of near-identical ones
means too high to name. Either is H1 falsified — report it rather than
adjusting the floor to make the band fit, which would be preregistration
theatre.

- [ ] **Step 1: Write the H2 measurement**

Over seed 42's full feature index, for every pair of the world's peoples,
compute the fraction of features whose two names differ. Report per pair and
overall, with `--nocapture`.

- [ ] **Step 2: Run it and record the number**

```bash
cargo test -p hornvale-worldgen --test gazetteer_naming -- --nocapture > /tmp/hv-h2.log 2>&1
echo "exit=$?"
cat /tmp/hv-h2.log
```

- [ ] **Step 3: Apply the decision rule — do NOT retune**

Preregistered prediction (spec §8, Nathan's): **they diverge.**

- **Divergence high** (most pairs differ on most features) -> H2 confirmed.
  Assert a floor comfortably below the measured value.
- **Divergence near zero** -> H2 falsified. This is a **shippable result** and
  the campaign headline becomes "landform names are effectively universal".
  Do not adjust a constant to rescue it. Before concluding, check the one
  thing that would make it an artefact: Task 6's
  `two_peoples_name_one_feature_differently` is the control — if it passes and
  the world-scale number is still ~0, the null is real.
- **Anything between** -> report the distribution, propose the floor, and state
  what would distinguish the two readings.

**A single-world reading is an anecdote.** The Watershed had a single-world
result reverse at census scale three times in one campaign. If seed 42 says
"confirmed", say so as *seed 42 says*, and name what a multi-seed run costs.

Note also that the null would *simplify* the multi-name rendering of Task 8 —
which is exactly why it must not be allowed to feel convenient.

- [ ] **Step 4: Commit**

```bash
cargo fmt
make gate-commit
git add windows/worldgen/tests/gazetteer_naming.rs
git commit -m "test(worldgen): H2 — per-culture name divergence, measured"
```

---

### Task 8: Surfaces — explain, the almanac, and the gazetteer artifact

**Files:**
- Modify: `windows/explain/src/lib.rs`
- Modify: `windows/almanac/src/lib.rs`
- Modify: `scripts/regenerate-artifacts.sh`
- Modify: `docs/generated-paths.txt` (only if `book/src/gallery/` is not already listed — check first)
- Create: `book/src/gallery/gazetteer-seed-42.md` (generated)
- Modify: `book/src/SUMMARY.md`

**Interfaces:**
- Consumes: `FeatureIndex`, `feature_name` (Tasks 5-6).
- Produces: the committed artifact.

- [ ] **Step 1: Implement the multi-name rendering (spec §3.4)**

A feature surfaces **every** name it has, joined: `Mount McKinley, Denali`.
There is **no primary-name field**. These surfaces have no observer, so the
order is **species-label ascending with no lead** — they have no standing to
elect a canonical name.

- [ ] **Step 2: Wire the artifact into regeneration**

`docs/generated-paths.txt` is the single source of truth for what is
drift-checked. Check whether `book/src/gallery/` is already listed; if it is,
the new page inherits the check — **but only once it is tracked.**
`git diff --exit-code` against a path with no index entry is silently vacuous,
so the first commit introducing it MUST `git add` it.

- [ ] **Step 3: Regenerate**

```bash
make rebaseline
git status --short
```

- [ ] **Step 4: Read the drift against the decision table**

```bash
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

| what moved | reading | action |
|---|---|---|
| `docs/audits/type-audit-report.md` only | expected — new `pub` items | regenerate and commit in the **same** commit |
| `docs/digest/` | expected — a decision record lands | same commit |
| `book/src/gallery/gazetteer-seed-42.md` | expected — this campaign's new artifact | `git add` it here |
| any **other** `book/src/gallery/` page | **STOP** — a rendered world moved, so naming consumed a draw | do not rebaseline; diagnose and report |
| any volcano name anywhere | **STOP** — the salt constraint broke | do not rebaseline; diagnose |
| any census CSV | **STOP** — epoch event | escalate to Nathan |
| `book/src/reference/*-generated.md` | expected **only if** a concept or predicate was registered | if none was, STOP |

- [ ] **Step 5: Commit**

```bash
cargo fmt
make gate-commit
git add -A
git commit -m "feat(explain,almanac): name the landscape; the gazetteer artifact"
```

---

### Task 9: Stage gate

- [ ] **Step 1: Push and submit**

```bash
git push -u origin campaign/the-gazetteer
make sluice-stage BRANCH=campaign/the-gazetteer REF=$(git rev-parse HEAD)
```

`REF` must be a **full SHA**, never a branch name. A conflict is refused at the
mouth in milliseconds — that is the signal to absorb main locally and resubmit.

- [ ] **Step 2: Regenerate after ANY absorption**

A conflict-free merge runs **no hook at all**, and generated files merge wrong
without conflicting. Always `make rebaseline` after absorbing; never infer
freshness from a clean merge.

---

### Task 10: Close

- [ ] **Step 1: Delete the probe**

```bash
git rm windows/worldgen/examples/gazetteer_probe.rs
```

- [ ] **Step 2: Chronicle, retrospective, registry rows, decision**

- `book/src/chronicle/the-gazetteer.md` + `book/src/SUMMARY.md`
- `docs/retrospectives/the-gazetteer.md` (process lessons, not product) and its
  line in `docs/retrospectives/README.md`
- Idea-registry rows for spec §9's carried-forward items
- **Correct `RENDER-fogged-world-map-rung`**: its cost claim ("an ASCII
  equirectangular renderer does not exist") is false — five exist — and its
  "same three epistemic states" is unbuildable, since a planet needs a fourth
  covering nearly all its surface.
- Decision record for spec §10, numbered **contiguously from main** at the time
  of writing. Check `make board` first; a duplicate reddens
  `docs_consistency::decision_log_has_no_gaps`.

- [ ] **Step 3: Freshness sweep**

The book may never lag merged reality. A campaign that moves a Confidence
Gradient bet re-scores that chapter (decision 0030).

- [ ] **Step 4: Merge**

```bash
make sluice BRANCH=campaign/the-gazetteer REF=$(git rev-parse HEAD)
```

Requires an authored `Sluice-Headline:` trailer sharing the `Claude-Session`
trailer block with **no blank line between them** — a merge refuses without it.
