# The Contour Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give Hornvale's deep-history conflict a second contest axis — a cell's defensibility — and measure whether it holds peoples-diversity open.

**Architecture:** One pure function of `(cell, ConnectionGraph)` becomes a multiplier on the *holder's* side of the two dominance tests in `windows/worldgen/src/history_bake.rs`. No new seeded draw, no new authored data, no new crate. A separate derived classification names each people's contour for legibility. The measurement instrument is built and the baseline captured **before** the behaviour changes.

**Tech Stack:** Rust 2024, `hornvale-worldgen` (composition root), `hornvale-topology` (the connection graph), `hornvale-lab` (the measurement instrument), `hornvale_kernel::math::tanh` (libm-backed).

## Global Constraints

- **Dependencies:** `serde`, `serde_json`, `libm` only. No new crates. Randomness comes from the kernel's `Seed`/`Stream`.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only. Float sorting uses `total_cmp`.
- **No wall-clock time.** Time is `WorldTime { day: f64 }`.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field and variant gets a one-line doc comment.
- **Type-audit:** every primitive at a `pub` boundary carries a verdict tag. The grammar is exactly `/// type-audit: bare-ok(<class>)` or `bare-ok(<class>: <field>)` — **`bare-ok(class: field)` with a space after the colon and the class first**; a malformed tag is a recurring plan-sourced failure.
- **`cargo fmt` is the final step before every commit.** Skipped fmt is the single most common review finding.
- **The gate is `make gate`** (~4 min). Iterate with `cargo test -p <crate>`; `--workspace` belongs at the pre-commit gate only.
- **No constant introduced by this plan may be re-tuned after Task 5's baseline is captured.** See spec §4.4. If a readout disappoints, the finding is reported; the constants do not move.

---

## File Structure

| File | Responsibility |
|---|---|
| `windows/worldgen/src/history_bake.rs` | `approach_ease`, `defensibility`, the two call sites, their unit + behavioural tests |
| `windows/worldgen/tests/defensibility_field.rs` | Property tests for the field (monotonicity, asymptotes, determinism) |
| `windows/lab/src/metrics.rs` | M2/M3/M4 extractors |
| `windows/lab/studies/the-contour.study.json` | The preregistered study |
| `domains/species/src/contour.rs` | Contour as a derived `is-a` classification |
| `docs/superpowers/plans/2026-07-29-the-contour.md` | This plan; status updated as tasks complete |

---

### Task 1: `approach_ease` — the raw geographic signal

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs` (add near `traversable_neighbors`, ~line 46)
- Test: `windows/worldgen/src/history_bake.rs` (the existing `#[cfg(test)] mod tests`)

**Interfaces:**
- Consumes: `hornvale_topology::{ConnectionGraph, Edge}`, `hornvale_kernel::CellId`
- Produces: `fn approach_ease(graph: &ConnectionGraph, cell: CellId) -> f64` — the summed conductance of every traversable edge into `cell`. Task 3 consumes this.

- [ ] **Step 1: Write the failing test**

Add to the test module in `history_bake.rs`:

```rust
#[test]
fn approach_ease_sums_traversable_conductance_only() {
    use hornvale_topology::{ConnectionGraph, Edge, EdgeKind};
    let mut g = ConnectionGraph::new(4);
    g.add_edge(
        CellId(0),
        Edge { to: CellId(1), kind: EdgeKind::Adjacency, conductance: 0.25 },
    );
    g.add_edge(
        CellId(0),
        Edge { to: CellId(2), kind: EdgeKind::LandRoute, conductance: 0.75 },
    );
    // Ocean-touching adjacency is stored at exactly 0.0 and must not count.
    g.add_edge(
        CellId(0),
        Edge { to: CellId(3), kind: EdgeKind::Adjacency, conductance: 0.0 },
    );
    assert_eq!(approach_ease(&g, CellId(0)), 1.0);
}

#[test]
fn approach_ease_is_zero_for_an_isolated_cell() {
    use hornvale_topology::ConnectionGraph;
    let g = ConnectionGraph::new(2);
    assert_eq!(approach_ease(&g, CellId(0)), 0.0);
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-worldgen approach_ease`
Expected: FAIL — `cannot find function 'approach_ease' in this scope`

- [ ] **Step 3: Write minimal implementation**

Add immediately after `traversable_neighbors` in `history_bake.rs`:

```rust
/// The aggregate ease of reaching `cell`: the summed `conductance` of every
/// traversable edge into it (`conductance > 0.0` — ocean-touching adjacency
/// edges are stored at exactly 0.0 and are not routes). Higher means more,
/// and easier, ways in.
///
/// A pure function of the graph, with no time, seed, or bake state in it,
/// which is what makes [`defensibility`] recomputable and testable. The
/// graph is per-era, so this is too: a glacial low-stand that exposes a land
/// bridge raises the ease of every cell it reaches.
fn approach_ease(graph: &ConnectionGraph, cell: CellId) -> f64 {
    graph
        .edges(cell)
        .iter()
        .filter(|e| e.conductance > 0.0)
        .map(|e| e.conductance)
        .sum()
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-worldgen approach_ease`
Expected: PASS, 2 tests

- [ ] **Step 5: fmt, clippy, commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
git add windows/worldgen/src/history_bake.rs
git commit -m "feat(the-contour): approach_ease, the raw geographic signal

The summed traversable conductance into a cell — a pure function of the
per-era connection graph, no seed and no bake state. Task 1."
```

---

### Task 2: Measure the `approach_ease` distribution and freeze the constants

**This task must complete before Task 3 writes any constant, and before Task 6 changes any behaviour.** Its whole purpose is that the constants are chosen from the geography rather than from a readout (spec §4.4).

**Files:**
- Create: `windows/worldgen/tests/approach_ease_calibration.rs`
- Modify: `docs/superpowers/plans/2026-07-29-the-contour.md` (record the measured values in this task's Step 4)

**Interfaces:**
- Consumes: `approach_ease` from Task 1.
- Produces: three frozen numbers, written into `history_bake.rs` in Task 3.

- [ ] **Step 1: Write the calibration harness as an `#[ignore]`d test**

Create `windows/worldgen/tests/approach_ease_calibration.rs`:

```rust
//! A one-off calibration: the distribution of `approach_ease` over habitable
//! cells, pooled across seeds. Run once, by hand, to set `DEF_SCALE` so the
//! median cell's defensibility is ~1.0 — i.e. the median world is unchanged
//! and only the extremes of the terrain move. `#[ignore]`d because it is a
//! measurement, not a gate; it asserts nothing about the result.

#[test]
#[ignore = "calibration: run by hand, prints the approach_ease quantiles"]
fn print_approach_ease_quantiles() {
    let mut all: Vec<f64> = Vec::new();
    for seed in 1u64..=30 {
        // Build to Settlements depth: the connection graph exists there and
        // the full stack is not needed.
        let world = /* build via hornvale_worldgen at BuildDepth::Settlements */
            unimplemented!("wire to the worldgen builder — see Step 2");
        let _ = (&world, &mut all, seed);
    }
    all.sort_by(f64::total_cmp);
    for q in [0.05, 0.25, 0.50, 0.75, 0.95] {
        let i = ((all.len() as f64 - 1.0) * q).round() as usize;
        println!("q{:.2} = {:.6}", q, all[i]);
    }
}
```

- [ ] **Step 2: Wire the harness to the real builder**

Find the builder and the graph accessor, then replace the `unimplemented!`:

```bash
grep -n "BuildDepth::Settlements" windows/lab/src/*.rs windows/worldgen/src/lib.rs | head
grep -n "pub fn .*graph\|graphs" windows/worldgen/src/lib.rs | head
```

Collect `approach_ease(graph, cell)` for every cell with non-zero capacity, for each seed, into `all`.

- [ ] **Step 3: Run the calibration**

Run: `cargo test -p hornvale-worldgen --test approach_ease_calibration -- --ignored --nocapture`
Expected: five quantile lines printed.

- [ ] **Step 4: Record the measured values in this plan**

Write the five printed quantiles into this task, in the plan file, as a permanent record of what the constants were chosen from. Then set:

- `DEF_SCALE` = the **median** (q0.50). This makes `tanh(ease/DEF_SCALE) ≈ 0.762` at the median.
- `DEF_FLOOR = 0.75`, `DEF_CEIL = 1.40` — authored, chosen so the median cell lands near 1.0 and the spread is roughly ±30%. These are **authored priors, not fits**; say so in the doc comment.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/worldgen/tests/approach_ease_calibration.rs docs/superpowers/plans/2026-07-29-the-contour.md
git commit -m "test(the-contour): calibrate DEF_SCALE from the approach_ease distribution

Run once, by hand, before any behaviour changes — so the constants come from
the geography rather than from a readout (spec 4.4). Quantiles recorded in
the plan. Task 2."
```

---

### Task 3: `defensibility` — the asymptotic field

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs`
- Test: `windows/worldgen/tests/defensibility_field.rs` (create)

**Interfaces:**
- Consumes: `approach_ease` (Task 1), the constants frozen in Task 2.
- Produces: `fn defensibility(graph: &ConnectionGraph, cell: CellId) -> f64`, returning a value strictly inside `(DEF_FLOOR, DEF_CEIL)`. Task 6 consumes it.

- [ ] **Step 1: Write the failing property tests**

Create `windows/worldgen/tests/defensibility_field.rs`:

```rust
//! The defensibility field's three load-bearing properties (spec §2.3):
//! strictly monotone decreasing in approach ease, ASYMPTOTIC rather than
//! clamped (decision 0086 clause 3), and a pure function of the graph.

use hornvale_kernel::CellId;
use hornvale_topology::{ConnectionGraph, Edge, EdgeKind};
use hornvale_worldgen::defensibility_for_test as defensibility;

fn cell_with_ease(ease: f64) -> ConnectionGraph {
    let mut g = ConnectionGraph::new(2);
    g.add_edge(
        CellId(0),
        Edge { to: CellId(1), kind: EdgeKind::LandRoute, conductance: ease },
    );
    g
}

#[test]
fn defensibility_falls_strictly_as_approach_gets_easier() {
    let mut prev = f64::INFINITY;
    for step in 0..40 {
        let g = cell_with_ease(step as f64 * 0.25);
        let d = defensibility(&g, CellId(0));
        assert!(d < prev, "must be strictly decreasing at step {step}");
        prev = d;
    }
}

#[test]
fn defensibility_is_an_asymptote_not_a_clamp() {
    // Decision 0086 clause 3: the probability of exceeding a clamp is exactly
    // zero at any input, which forecloses the rare tails the sigmoid wager
    // needs. No input may reach either bound exactly.
    let wide_open = cell_with_ease(1.0e6);
    let d_min = defensibility(&wide_open, CellId(0));
    assert!(d_min > 0.75, "never reaches DEF_FLOOR: got {d_min}");

    let isolated = ConnectionGraph::new(2);
    let d_max = defensibility(&isolated, CellId(0));
    assert!(d_max < 1.40, "never reaches DEF_CEIL: got {d_max}");
}

#[test]
fn defensibility_is_deterministic_across_recomputation() {
    let g = cell_with_ease(1.75);
    let first = defensibility(&g, CellId(0));
    for _ in 0..8 {
        assert_eq!(defensibility(&g, CellId(0)), first);
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-worldgen --test defensibility_field`
Expected: FAIL — `defensibility_for_test` not found in `hornvale_worldgen`

- [ ] **Step 3: Implement, substituting Task 2's measured `DEF_SCALE`**

In `history_bake.rs`, beside the other bake constants (~line 72–136):

```rust
/// AUTHORED prior: the least defensible ground the world admits — the value
/// `defensibility` approaches, and never reaches, as approach ease grows
/// without bound. An ASYMPTOTE, not a clamp: decision 0086 clause 3 records
/// that a clamp has exactly zero probability of being exceeded at any input,
/// which forecloses the rare tails the sigmoid wager needs.
/// type-audit: bare-ok(ratio: DEF_FLOOR)
const DEF_FLOOR: f64 = 0.75;
/// AUTHORED prior: the most defensible ground the world admits, approached
/// as approach ease falls to zero. Never reached, for the same reason.
/// type-audit: bare-ok(ratio: DEF_CEIL)
const DEF_CEIL: f64 = 1.40;
/// CALIBRATED (Task 2): the median `approach_ease` over habitable cells,
/// pooled over seeds 1..=30, so the MEDIAN cell's defensibility sits near
/// 1.0 and only the extremes of the terrain move the outcome. Chosen from the
/// geography before any behavioural readout existed, and frozen thereafter
/// (spec §4.4). A save-format constant from here on.
/// type-audit: bare-ok(ratio: DEF_SCALE)
const DEF_SCALE: f64 = 0.0; // <- REPLACE with Task 2's measured q0.50
```

And the function, beside `approach_ease`:

```rust
/// How hard `cell` is to come at, as a multiplier on its HOLDER's side of the
/// dominance test. Strictly decreasing in [`approach_ease`], bounded by
/// `(DEF_FLOOR, DEF_CEIL)` asymptotically, and a pure function of the per-era
/// graph — no seed, no time, no bake state, so it consumes no draw and cannot
/// move stream consumption order.
///
/// The second contest axis (decision 0086 clause 1). It is indifferent to who
/// holds the cell, which is what makes it a legal mechanism rather than an
/// authored handicap: it is a term keyed on ground, and that the weak benefit
/// from defensible ground is a byproduct.
fn defensibility(graph: &ConnectionGraph, cell: CellId) -> f64 {
    let eased = hornvale_kernel::math::tanh(approach_ease(graph, cell) / DEF_SCALE);
    DEF_FLOOR + (DEF_CEIL - DEF_FLOOR) * (1.0 - eased)
}

/// Test-only re-export of [`defensibility`] so the property battery in
/// `tests/defensibility_field.rs` can reach it without making the field part
/// of this crate's real public surface.
#[doc(hidden)]
pub fn defensibility_for_test(graph: &ConnectionGraph, cell: CellId) -> f64 {
    defensibility(graph, cell)
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-worldgen --test defensibility_field`
Expected: PASS, 3 tests

- [ ] **Step 5: fmt, clippy, type-audit, commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/worldgen/
git commit -m "feat(the-contour): defensibility as an asymptotic field

A tanh of approach ease, bounded by (DEF_FLOOR, DEF_CEIL) asymptotically
rather than clamped — decision 0086 clause 3. Pure in the graph, so no draw
moves. Not yet wired to anything. Task 3."
```

---

### Task 4: The measurement instrument

Built **before** the behaviour changes, so the baseline in Task 5 is honest.

**Files:**
- Modify: `windows/lab/src/metrics.rs`

**Interfaces:**
- Produces: three metrics named `peoples-alive-at-bake-end`, `largest-holding-share`, `defensibility-capacity-rank-corr`. Task 5's study names them.

- [ ] **Step 1: Find what `FullView` exposes**

```bash
grep -n "pub struct FullView" -A 30 windows/lab/src/metrics.rs
grep -rn "communities\|occupation" windows/lab/src/metrics.rs | head
```

Record the exact accessor names; the closures below use them.

- [ ] **Step 2: Write the failing test**

Add to `metrics.rs`'s test module:

```rust
#[test]
fn the_contour_metrics_are_registered_and_full_rung() {
    let reg = registry();
    for name in [
        "peoples-alive-at-bake-end",
        "largest-holding-share",
        "defensibility-capacity-rank-corr",
    ] {
        let m = reg
            .iter()
            .find(|m| m.name == name)
            .unwrap_or_else(|| panic!("metric {name} is not registered"));
        assert_eq!(m.rung(), BuildDepth::Full, "{name} must read the full stack");
        assert!(!m.doc.is_empty(), "{name} needs a doc");
    }
}
```

- [ ] **Step 3: Run to verify it fails**

Run: `cargo test -p hornvale-lab the_contour_metrics`
Expected: FAIL — `metric peoples-alive-at-bake-end is not registered`

- [ ] **Step 4: Add the three metrics to `registry()`**

```rust
Metric {
    name: "peoples-alive-at-bake-end",
    doc: "M3: how many distinct peoples still hold a live community when the \
          bake ends — the decision-0086 compliance reading",
    summary: SummaryKind::Numeric { bucket_edges: &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0] },
    extract: Extractor::Full(|v: &FullView| {
        let mut peoples = std::collections::BTreeSet::new();
        // Replace with the accessor found in Step 1.
        for c in v.live_communities() {
            peoples.insert(c.people);
        }
        MetricValue::Number(peoples.len() as f64)
    }),
},
Metric {
    name: "largest-holding-share",
    doc: "M2: the largest single community's population as a share of all live \
          population at bake end — the entity-size reading the criticality \
          campaigns never took",
    summary: SummaryKind::Numeric { bucket_edges: &[0.05, 0.1, 0.2, 0.3, 0.5, 0.7] },
    extract: Extractor::Full(|v: &FullView| {
        let pops: Vec<f64> = v.live_communities().map(|c| c.population).collect();
        let total: f64 = pops.iter().sum();
        if total <= 0.0 {
            return MetricValue::Absent;
        }
        let max = pops.iter().copied().fold(f64::NEG_INFINITY, f64::max);
        MetricValue::Number(max / total)
    }),
},
Metric {
    name: "defensibility-capacity-rank-corr",
    doc: "M4: Spearman rank correlation between a cell's defensibility and its \
          effective capacity over habitable cells — checks spec 2.2's frontier \
          hypothesis, NOT the campaign's",
    summary: SummaryKind::Numeric { bucket_edges: &[-0.6, -0.3, 0.0, 0.3, 0.6] },
    extract: Extractor::Full(|v: &FullView| {
        // Spearman = Pearson on ranks. Ties get average ranks; sort with
        // total_cmp so the ordering is deterministic.
        MetricValue::Number(spearman_defensibility_capacity(v))
    }),
},
```

Write `spearman_defensibility_capacity` as a private helper beside the other metric helpers, sorting with `f64::total_cmp` and assigning average ranks to ties.

- [ ] **Step 5: Run to verify it passes**

Run: `cargo test -p hornvale-lab the_contour_metrics`
Expected: PASS

- [ ] **Step 6: fmt, clippy, commit**

```bash
cargo fmt
cargo clippy -p hornvale-lab --all-targets -- -D warnings
git add windows/lab/src/metrics.rs
git commit -m "feat(the-contour): the M2/M3/M4 metrics, before the mechanism

Built ahead of the behaviour change so Task 5's baseline is a real before.
M2 (largest-holding-share) is the entity-size reading Hornvale has never
taken. Task 4."
```

---

### Task 5: The preregistered study, and the baseline on the UNMODIFIED bake

**Files:**
- Create: `windows/lab/studies/the-contour.study.json`
- Create: `docs/superpowers/plans/the-contour-baseline.md`

- [ ] **Step 1: Write the study**

```json
{
  "name": "the-contour",
  "hypothesis": "A second contest axis (cell defensibility), uncorrelated with strength and entering at the dominance test, raises the number of peoples surviving to bake end. The entity-size distribution is the open question and both branches are informative: a heavier tail supports the sigmoid wager; a geometric tail alongside a diversity rise localises the missing term in per-community deviation rather than leaving it unlocated.",
  "seeds": { "from": 1, "to": 30 },
  "metrics": [
    "peoples-alive-at-bake-end",
    "largest-holding-share",
    "defensibility-capacity-rank-corr"
  ]
}
```

Confirm the schema against a shipped study before running:

```bash
cat windows/lab/studies/the-census.study.json | head -30
```

- [ ] **Step 2: Run the study on the unmodified bake**

Run: `cargo run -p hornvale -- lab run windows/lab/studies/the-contour.study.json`
Expected: a CSV with 30 rows and the three columns.

- [ ] **Step 3: Record the baseline**

Write `docs/superpowers/plans/the-contour-baseline.md` with the three metrics' distributions **as measured before any behaviour change**, and the commit SHA it was taken at. This file is the thing Task 7 compares against and must not be edited afterwards.

- [ ] **Step 4: Commit**

```bash
git add windows/lab/studies/the-contour.study.json docs/superpowers/plans/the-contour-baseline.md
git commit -m "test(the-contour): preregister the study and capture the baseline

Hypothesis and both informative branches frozen BEFORE the mechanism exists
(decision 0016, preregistration_guard). Baseline measured on the unmodified
bake at the recorded SHA. Task 5."
```

---

### Task 6: Wire defensibility into the two dominance tests

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs:2544` (`maybe_raid`) and `:1127` (`best_home`)

**Interfaces:**
- Consumes: `defensibility` (Task 3).

- [ ] **Step 1: Write the failing behavioural test**

In `history_bake.rs`'s test module:

```rust
#[test]
fn an_exposed_holder_is_raided_and_a_sheltered_one_is_not() {
    // Two holders identical in every respect except the approach structure of
    // their cell. Only the exposed one may be taken. This is the test that
    // fails if the term is wired to the ATTACKER's side by mistake.
    let mut bake = /* fixture: raider + two holders, equal population and tech */
        unimplemented!("build via the existing test fixture helpers in this module");
    let _ = &mut bake;
    todo!("assert the exposed holder is raided and the sheltered one is skipped");
}
```

Replace the `unimplemented!`/`todo!` using the fixture helpers already in that module — `stores_raise_strength_but_never_pressure` (~line 3337) shows the established shape.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-worldgen an_exposed_holder`
Expected: FAIL — both holders are raided, because defensibility is not yet read.

- [ ] **Step 3: Change the two call sites**

`maybe_raid`, line 2544:

```rust
            if raider_str <= t_str * defensibility(self.cur(), n) * RAID_MARGIN {
                continue; // dominance: only a fight it can win, on this ground
            }
```

`best_home`, line 1127:

```rust
                        if !may_take_held_land
                            || strength <= hs * defensibility(self.cur(), n) * RAID_MARGIN
                        {
                            continue; // not a fight this people can win here, or survive winning
                        }
```

**These are the only two sites.** Defensibility must not enter `strength`, `pressure_of`, `eff_capacity`, or tribute assessment — the same discipline that kept `stores` out of `pressure_of`.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-worldgen an_exposed_holder`
Expected: PASS

- [ ] **Step 5: Mutation-verify the test**

Temporarily change `defensibility` to `fn defensibility(_g: &ConnectionGraph, _c: CellId) -> f64 { 1.0 }` and re-run.

Run: `cargo test -p hornvale-worldgen an_exposed_holder`
Expected: **FAIL.** If it passes, the test asserts nothing and must be rewritten before proceeding. Revert the stub afterwards.

- [ ] **Step 6: Confirm no draw moved**

Run: `cargo test -p hornvale-terrain --test tectonic_properties && cargo test -p hornvale-astronomy --test genesis_properties`
Expected: PASS, untouched. These are the pin-isolation batteries; a failure here means stream consumption order moved, which contradicts spec §5 and must be understood before continuing.

- [ ] **Step 7: fmt, clippy, commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
git add windows/worldgen/src/history_bake.rs
git commit -m "feat(the-contour): position enters the two dominance tests

The second contest axis, live. Mutation-verified, and the pin-isolation
batteries confirm no draw moved. Task 6."
```

---

### Task 7: The readout and adjudication

**Files:**
- Create: `docs/superpowers/plans/the-contour-readout.md`

- [ ] **Step 1: Re-run the study on the changed bake**

Run: `cargo run -p hornvale -- lab run windows/lab/studies/the-contour.study.json`

- [ ] **Step 2: Replicate at 100 seeds**

Edit the study's seed range to `{"from": 1, "to": 100}` in a scratch copy — **not** in the committed study — and run again, matching the two prior campaigns' replication.

- [ ] **Step 3: Adjudicate each prediction explicitly**

Write `the-contour-readout.md` stating, for each of spec §4.2's three predictions, **confirmed** or **falsified**, with the numbers. If M3 did not move, that is the headline and §4.3's null statement is the conclusion. Do not add a mechanism to rescue a number; do not re-tune `DEF_*`.

- [ ] **Step 4: Commit**

```bash
git add docs/superpowers/plans/the-contour-readout.md
git commit -m "test(the-contour): the readout, adjudicated against the frozen predictions"
```

---

### Task 8: Contour as a derived classification

Spec §3. Separable — if Task 7's readout is a null, this still ships, because it is the legibility half and costs no authored data.

**Files:**
- Create: `domains/species/src/contour.rs`
- Modify: `domains/species/src/lib.rs` (add `mod contour;` and re-export)

**Interfaces:**
- Produces: `pub fn contour_of(mind: MindVector, society: SocietyVector, perception: PerceptionVector) -> Contour` and `pub enum Contour`.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn the_goblin_baseline_classifies_as_unspecialized() {
    // Goblin is the ORIGIN of the species vector space — 0.5 on every scalar,
    // default enum variants. A shape with no peak has no archetype.
    let c = contour_of(goblin_mind(), goblin_society(), goblin_perception());
    assert_eq!(c, Contour::Unspecialized);
}

#[test]
fn a_single_peak_names_that_peak() {
    let mut m = goblin_mind();
    m.time_horizon = 0.95;
    let c = contour_of(m, goblin_society(), goblin_perception());
    assert_eq!(c, Contour::LongSighted);
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-species contour`
Expected: FAIL — `contour_of` not found

- [ ] **Step 3: Implement as a pure classification over the existing vectors**

No new authored field. A dimension is a "peak" if it exceeds the goblin baseline of 0.5 by a margin; the contour is the pattern of peaks, per decisions 0060 and 0062.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-species contour`
Expected: PASS

- [ ] **Step 5: Regenerate the concept dump — this is the recurring miss**

New registered concepts mean `make gate` alone is **not** sufficient freshness.

```bash
cargo run -p hornvale -- concepts > book/src/reference/concepts.md
git diff --stat book/src/reference/
```

- [ ] **Step 6: fmt, clippy, type-audit, commit**

```bash
cargo fmt
cargo clippy -p hornvale-species --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add domains/species/ book/src/reference/
git commit -m "feat(the-contour): contour as a derived is-a classification

The pattern of peaks over the vectors species already carries — no new
authored data, per decisions 0060/0062. Task 8."
```

---

### Task 9: Measure the artifact and epoch consequences

Spec §5. **Measure, then declare — never the reverse** (decision 0084).

**Files:**
- Modify: `domains/history/src/streams.rs` (only if the measurement says so)

- [ ] **Step 1: Regenerate everything except censuses**

Run: `make rebaseline`

- [ ] **Step 2: Read the diff**

```bash
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
```

Record which committed derivations actually moved. **This output, not reasoning, decides the epoch.**

- [ ] **Step 3: Declare the epoch if and only if a derivation moved**

If `history/bake`'s output moved, bump it with an epoch suffix (`history/bake/v2`) — never a rename — and regenerate the stream manifest:

```bash
cargo run -p hornvale -- streams > book/src/reference/streams.md
```

If nothing moved, **declare nothing** and record why, as 0084 did.

- [ ] **Step 4: Run the full gate**

Run: `make gate`
Expected: green. Capture to a file and grep it rather than re-running:
`cargo nextest run --workspace 2>&1 | tee /tmp/hv-contour.txt`

- [ ] **Step 5: Run the checks the gate never runs**

```bash
make shellcheck
make census-check
```

- [ ] **Step 6: STOP — the census refresh is a carve-out**

If the diff shows census goldens moved, **do not run the census.** `scripts/census-run.sh` on host `lefford` (decisions 0079/0081) needs Nathan's explicit authorization at this point. Report what moved and wait.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add -A
git commit -m "chore(the-contour): regenerate artifacts and settle the epoch question

Measured before declared, per decision 0084. Task 9."
```

---

## Self-Review

**Spec coverage.** §1 → Tasks 1/3/6. §2.1–2.4 → Tasks 1, 3, 6. §2.2's frontier hypothesis → M4 in Task 4, adjudicated in Task 7. §3 → Task 8. §4.1 M1 → **gap, now closed:** M1 (the inherited cascade histogram) is the *existing* metric from *The Tumult*/*The Tithe*; Task 5's study must name it alongside the three new ones so the prior nulls stay comparable — the implementer adds the existing cascade metric's registered name to the study's `metrics` array in Task 5 Step 1, found via `cargo run -p hornvale -- lab list-metrics`. §4.2 → Task 7 Step 3. §4.3 → Task 7 Step 3. §4.4 → Task 2 and the Global Constraints. §5 → Task 6 Step 6 and Task 9. §6 → Tasks 1, 3, 6. §7's non-goals → nothing in any task touches d′, prototype inheritance, depth, or trade. §8's three open questions → carried, unresolved, and each is decided in the task that meets it (Task 1 defaults to the unweighted sum; Task 3 computes per-era; Task 8 lands in `domains/species`).

**Placeholder scan.** Two intentional `unimplemented!`/`todo!` markers remain, in Task 2 Step 1 and Task 6 Step 1, each immediately followed by a step that names the exact grep or fixture to replace it with. `DEF_SCALE = 0.0` in Task 3 is deliberately a tripwire — it is replaced by Task 2's measured value and would produce a division by zero if skipped, which is the intended forcing function.

**Type consistency.** `approach_ease(&ConnectionGraph, CellId) -> f64` and `defensibility(&ConnectionGraph, CellId) -> f64` are used with those exact signatures in Tasks 1, 3, and 6. `defensibility_for_test` is the `#[doc(hidden)]` public wrapper the Task 3 battery imports. Metric names are identical in Task 4's registration, Task 4's test, and Task 5's study.
