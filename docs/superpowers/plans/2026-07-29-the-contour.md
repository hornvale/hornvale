# The Contour Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give Hornvale's deep-history conflict a second contest axis — a cell's defensibility — and measure whether it holds peoples-diversity open.

**Architecture:** One pure function of `(ConnectionGraph, from, to)` becomes a multiplier on the *holder's* side of the two dominance tests in `windows/worldgen/src/history_bake.rs`. No new seeded draw, no new authored data, no new crate. A separate derived classification names each people's contour for legibility. The measurement instrument is built and the baseline captured **before** the behaviour changes.

> **Spec amendment 1 (2026-07-30), pre-readout.** Tasks 2/2b/2c measured the
> per-cell aggregate and found it is *two disjoint regimes* split on
> `WaterRoute` vs `Adjacency` — an empty gap between conductance 0.047 and
> 0.5 across 142,595 cells — which no single transform can grade. The
> mechanism therefore reads **the approach's own conductance**, not a per-cell
> aggregate. Task 2d recalibrates against the quantity the amended mechanism
> actually reads. See spec §2.3/§2.3a. No behavioural readout existed when
> this was taken, so nothing was being chased.

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
| `windows/worldgen/src/history_bake.rs` | `approach_ease` (calibration-only), `defensibility`, the two call sites, their unit + behavioural tests |
| `windows/worldgen/tests/approach_ease_calibration.rs` | The calibration harness: the aggregate series (Tasks 2/2b/2c) and the cost-exponent series (Task 2d) |
| `windows/worldgen/tests/defensibility_field.rs` | Property tests for the field (monotonicity, inclusive bounds, parallel-edge max, determinism) |
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

- [x] **Step 4: Record the measured values in this plan**

**Measured**, 30 seeds (`1..=30`), pooled over 142,595 habitable (non-zero-capacity) cells, present-day connection graph (`connection_graph_of`; see the harness's doc comment for why present stands in for the bake's per-era array). Command:

```bash
cargo test -p hornvale-worldgen --test approach_ease_calibration -- --ignored --nocapture
```

Output:

```
q0.05 = 0.004881
q0.25 = 0.008302
q0.50 = 0.012622
q0.75 = 0.028620
q0.95 = 1.003891
```

So `DEF_SCALE = 0.012622` (q0.50). `DEF_FLOOR = 0.75`, `DEF_CEIL = 1.40` — authored, chosen so the median cell lands near 1.0 and the spread is roughly ±30%. These are **authored priors, not fits**; say so in the doc comment.

**Distribution shape — a finding, not smoothed over.** The pooled distribution is heavily right-skewed: the q0.75→q0.95 jump is over 30x (0.0286 → 1.0039), versus a q0.05→q0.75 span of well under one order of magnitude. An unrecorded follow-up pass (same run, diagnostic prints only, not part of the committed harness) put `n = 142,595`, `min ≈ 0` (isolated, single-cell landmasses whose only edges are ocean-touching adjacency, stored at `conductance == 0.0` and filtered out — so a habitable cell can have `approach_ease == 0`), `max ≈ 28.96`, `mean ≈ 0.185`, and `q0.99 ≈ 3.40`. Two consequences for Task 3:

1. Because `DEF_SCALE` is the median and the distribution's upper half is 1-3 orders of magnitude above it, `tanh(ease/DEF_SCALE)` is already within ~2% of its asymptote by q0.75 (ratio ≈ 2.27, `tanh ≈ 0.979`) — differentiation among the *more* defensible half of the map is compressed into a narrow output band near `DEF_FLOOR`, while nearly all of the formula's dynamic range is spent distinguishing cells *below* the median. This is expected of `tanh` on a right-skewed input and is not itself a bug, but it means "defensibility" as this formula defines it is really "how much *worse* than typical is this cell's approach", not a linear rescaling of `approach_ease`.
2. **Confirmed isolated cells (`approach_ease == 0`) exist in the measured population.** At `ease = 0`, `tanh(0 / DEF_SCALE) = 0`, so `defensibility = DEF_FLOOR + (DEF_CEIL - DEF_FLOOR) * (1 - 0) = DEF_CEIL` **exactly** — not strictly inside `(DEF_FLOOR, DEF_CEIL)`. Task 3 Step 4's test asserts `d_max < 1.40` (strict); on a seed where a habitable, isolated cell is sampled, that assertion will fail as written. Flagging for Task 3/6, not fixing here — this task's deliverable is the measurement, not the formula.

**Task 2b addendum (approved follow-up, measurement only): `sum` conflates "how good the best approach is" with "how many approaches there are".** An ideonomy pass on the right-skew finding above raised a real objection: an attacker uses ONE approach, so a sum over all approaches (Thermopylae's many bad ones and one good one, added together) is not obviously the right statistic for "defensibility". The harness was extended with two companion per-cell measurements — `max_approach` (the single largest traversable conductance) and `approach_count` (how many traversable edges) — over the **same** 30 seeds, same present-day era, same habitable-cell set. Command (unchanged):

```bash
cargo test -p hornvale-worldgen --test approach_ease_calibration -- --ignored --nocapture
```

Output (all three series in one run; `sum` reproduced byte-identical to the Step 4 record above):

```
q0.05 = 0.004881
q0.25 = 0.008302
q0.50 = 0.012622
q0.75 = 0.028620
q0.95 = 1.003891
max_conductance q0.05 = 0.001079
max_conductance q0.25 = 0.001616
max_conductance q0.50 = 0.002574
max_conductance q0.75 = 0.005602
max_conductance q0.95 = 0.998020
max_conductance min = 0.000000
max_conductance mean = 0.103236
max_conductance max = 1.000000
edge_count q0.05 = 4.000000
edge_count q0.25 = 6.000000
edge_count q0.50 = 6.000000
edge_count q0.75 = 6.000000
edge_count q0.95 = 6.000000
edge_count min = 0.000000
edge_count mean = 5.829377
edge_count max = 45.000000
```

**The 28.96-max question, resolved as fact, not guess:**

- **No individual `conductance` can exceed 1.0.** Measured `max_conductance max = 1.000000` exactly, over 142,595 cells and 30 seeds — never observed above 1. This matches all three producers in `windows/worldgen/src/graph_derive.rs`: `cost_conductance` (line 224, `1.0 / ((a + b) / 2.0)`, floored at cost 1 per side so bounded ≤ 1) for `Adjacency` edges; the `LandRoute` conductance (line 371, `1.0 / total.max(1)`) is likewise ≤ 1; and the `WaterRoute` conductance (line 326, `vector_magnitude(*current.get(launch))`, `domains/climate/src/currents.rs`) is provably ≤ 1 because `wind_east_tangent` (`domains/climate/src/circulation.rs:75-83`) explicitly unit-normalizes the wind, the 45° Ekman rotation (`currents.rs`, `ocean_current`) preserves that unit norm, and the subsequent coastal-land projection only *subtracts* a component (never adds one) — so a single edge cannot be the source of a 28.96 sum.
- **A habitable cell CAN carry far more traversable edges than its ~5-6 mesh neighbours.** Measured `edge_count`: median 6 (matching bare adjacency) and `q0.95` still 6, but `max = 45` — a small minority of cells accumulate dramatically more edges. Cause: `add_land_routes` (`graph_derive.rs:349-378`) adds one `LandRoute` edge per *settlement pair* within `land_route_radius` (12 hops) whose corridor clears `corridor_max_cost` — not restricted to mesh adjacency at all, so a well-connected settlement in a dense cluster accumulates one edge per nearby settlement, easily exceeding 6. `add_water_routes` (`graph_derive.rs:304-333`) compounds this with fan-in: it launches one route per *coastal source* cell and adds the mirrored edge at whatever cell that route's current-trace lands on, so a single convergent destination cell can receive edges from many distinct, non-adjacent source cells. A cell with 45 traversable edges each ≤ 1.0 easily sums past 28 — no single outsized edge is needed, or observed.
- **A genuine, separate double-counting defect also exists, confirmed empirically.** `traversable_neighbors` (`windows/worldgen/src/history_bake.rs`, ~line 40) sorts-and-dedups its neighbor list specifically because a cell's raw edge list can hold two edges to the *same* neighbor (e.g. `Adjacency` + `LandRoute` for a settlement pair that happens to also be mesh-adjacent) — `add_land_routes` never checks whether an `Adjacency` edge for that pair already exists before adding a `LandRoute` one. A one-off diagnostic (not part of the committed harness) confirmed this is real, not merely theoretical: of the 142,595 habitable cells measured, **9,608 (≈6.7%) have at least one duplicate `to` value among their traversable edges, totaling 15,968 extra double-counted edge entries.** `approach_ease` (both the private original and this harness's mirror) sums the raw edge list without deduping by neighbor, so for those cells the sum genuinely double-counts one physical neighbor's contribution. This is real but secondary to the fan-in effect above (avg ≈1.66 duplicate edges per affected cell, vs. a 45-edge outlier) — it inflates the sum statistic somewhat but is not, by itself, what produces a value near 29.

None of this changes the frozen `DEF_SCALE`/`DEF_FLOOR`/`DEF_CEIL` above (still a measurement task, not a redesign) — it is handed forward to whichever task next touches the `approach_ease`/`defensibility` formula, since "sum vs. max vs. count" is a design choice this task does not make.

**Task 2c addendum (approved follow-up, measurement only): does the bimodality split by `EdgeKind`?** Task 2b's bimodal `max_conductance` (a low cluster near 0.001-0.006 and a high cluster near 0.998-1.0) survived the switch from `sum` to `max`, ruling out a summing artifact. The successor hypothesis: the two populations are water-connected vs. land-only cells, since `WaterRoute` conductance is a current magnitude (bounded ≤ 1 but often near it) while `Adjacency`/`LandRoute` conductance is `1/cost` over terrain costs that are typically much larger than 1. Measured (same 30 seeds, same present-day era, same 142,595-habitable-cell pool):

```
adjacency_max q0.05 = 0.000977
adjacency_max q0.25 = 0.001446
adjacency_max q0.50 = 0.002099
adjacency_max q0.75 = 0.003690
adjacency_max q0.95 = 0.008511
adjacency_max min = 0.000000
adjacency_max mean = 0.003141
adjacency_max max = 0.046512
adjacency_present = 142196 / 142595

water_route_max q0.05 = 0.000000
water_route_max q0.25 = 0.000000
water_route_max q0.50 = 0.000000
water_route_max q0.75 = 0.000000
water_route_max q0.95 = 0.998020
water_route_max min = 0.000000
water_route_max mean = 0.100356
water_route_max max = 1.000000
water_route_present = 18257 / 142595

land_route_max q0.05 = 0.000000
land_route_max q0.25 = 0.000000
land_route_max q0.50 = 0.000000
land_route_max q0.75 = 0.000000
land_route_max q0.95 = 0.000000
land_route_max min = 0.000000
land_route_max mean = 0.000062
land_route_max max = 0.020000
land_route_present = 2696 / 142595
```

**The cross-tab that answers the question** (thresholds: high ≥ 0.5, low ≤ 0.01 — used as the coordinator suggested, since Task 2b's `max_conductance` quantiles already show a clean gap there: q0.75 = 0.005602 well under 0.01, q0.95 = 0.998020 well over 0.5; 164 habitable cells with zero traversable edges at all are excluded rather than folded into "low"):

```
cross_tab high (max >= 0.5): n = 14702, adjacency = 0.0000, water_route = 1.0000, land_route = 0.0000
cross_tab low  (max <= 0.01): n = 119797, adjacency = 0.9881, water_route = 0.0003, land_route = 0.0117
```

**Verdict: the water/land hypothesis holds, cleanly.** `adjacency_max`'s own global maximum (0.046512) never reaches the high-population threshold at all — every habitable cell's own bare-terrain adjacency conductance measured across all 30 seeds tops out below 0.05, so the high population being 100.00% `WaterRoute` is not a close call, it is structurally guaranteed by the cost floor in `cost_conductance` versus a current magnitude that can sit near its own ceiling of 1. Reciprocally, only 18,257 of 142,595 habitable cells (≈12.8%) have any `WaterRoute` edge at all, and the low population is 98.81% `Adjacency`-supplied (`LandRoute` contributes a small 1.17% — land corridors exist but their conductance stays tiny, `max = 0.020000`, because `corridor_max_cost` keeps their total cost well above 1). The bimodality is therefore not a nuisance artifact of the statistic — it is water-connectivity itself, the single most meaningful signal in this field. This changes the design question for whichever task next builds `defensibility`: whether a cell has *any* sea approach may be more load-bearing than a smooth transform of a scalar.

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

### Task 2d: Calibrate `DEF_SCALE` from the cost-exponent distribution

> Added 2026-07-30 by spec amendment 1. Tasks 2/2b/2c measured the *aggregate*
> and, in doing so, showed the aggregate was the wrong quantity. This measures
> the quantity the amended mechanism actually reads. Same discipline, same
> ordering: **before any behavioural constant is written.**

**Files:**
- Modify: `windows/worldgen/tests/approach_ease_calibration.rs`
- Modify: `docs/superpowers/plans/2026-07-29-the-contour.md` (record the result here)

**Interfaces:**
- Produces: one frozen number, `DEF_SCALE`, written into `history_bake.rs` in Task 3.

- [x] **Step 1: Extend the harness with the cost-exponent series**

Over the same 30 seeds, same era, same habitable cells, collect one value per
**traversable edge** (not per cell): `-ln(conductance)`, deduplicating parallel
edges to the same `to` by taking the maximum conductance first, exactly as the
mechanism will. Report the five quantiles plus min/mean/max, and report them
**split by `EdgeKind`** as well as pooled.

**Implemented** as one value per ordered `(from, to)` pair with a traversable
edge, both `from` and `to` restricted to the same habitable-cell population
every prior run (2/2b/2c) used — matching how the mechanism is actually
invoked (raids and resettlement both originate from and land on settled
ground), deduplicated to the MAXIMUM parallel conductance first via
`best_conductance_with_kind`, mirroring the amended `defensibility`'s own
`best` computation in `history_bake.rs` exactly. Used
`hornvale_kernel::math::ln`/`tanh` throughout (never `f64::ln`/`f64::tanh`),
including for the `atanh` identity in Step 3 below (decision 0041).

- [x] **Step 2: Run it**

Run: `cargo test -p hornvale-worldgen --test approach_ease_calibration -- --ignored --nocapture`

Full cost_exponent output (pooled and split by `EdgeKind`):

```
cost_exponent_all n = 756510
cost_exponent_all q0.05 = 4.363099
cost_exponent_all q0.25 = 5.640132
cost_exponent_all q0.50 = 6.256709
cost_exponent_all q0.75 = 6.598509
cost_exponent_all q0.95 = 6.975881
cost_exponent_all min = -0.000000
cost_exponent_all mean = 5.919244
cost_exponent_all max = 8.470102

cost_exponent_adjacency n = 718404
cost_exponent_adjacency q0.05 = 4.871373
cost_exponent_adjacency q0.25 = 5.731722
cost_exponent_adjacency q0.50 = 6.295266
cost_exponent_adjacency q0.75 = 6.612713
cost_exponent_adjacency q0.95 = 6.985179
cost_exponent_adjacency min = 3.068053
cost_exponent_adjacency mean = 6.137051
cost_exponent_adjacency max = 8.470102

cost_exponent_water_route n = 28730
cost_exponent_water_route q0.05 = -0.000000
cost_exponent_water_route q0.25 = 0.000000
cost_exponent_water_route q0.50 = 0.085073
cost_exponent_water_route q0.75 = 0.553922
cost_exponent_water_route q0.95 = 1.790276
cost_exponent_water_route min = -0.000000
cost_exponent_water_route mean = 0.447921
cost_exponent_water_route max = 6.463058

cost_exponent_land_route n = 9376
cost_exponent_land_route q0.05 = 5.236442
cost_exponent_land_route q0.25 = 5.817111
cost_exponent_land_route q0.50 = 6.111467
cost_exponent_land_route q0.75 = 6.276643
cost_exponent_land_route q0.95 = 6.376727
cost_exponent_land_route min = 3.912023
cost_exponent_land_route mean = 5.995846
cost_exponent_land_route max = 6.395262
```

(The `sum`/`max_conductance`/`edge_count`/per-kind-max/cross-tab series from
Tasks 2a-2c reproduced byte-identical on this run, on the post-absorption
tree at `b58b025c` — confirms both the harness's determinism and that the
51-commit merge and amendment 1 did not perturb it.)

- [x] **Step 3: Compute and record `DEF_SCALE`**

`DEF_SCALE = median(cost_exponent) / atanh((1.0 - DEF_MIN) / (DEF_MAX - DEF_MIN))`

With `DEF_MIN = 0.75` and `DEF_MAX = 1.40` the divisor is `atanh(0.3846…) =
0.40546…`. This is the value that places the **median traversable approach** at
defensibility exactly 1.0, so the median world is unchanged and only the
extremes of the terrain move. Write the measured median, the divisor, and the
resulting `DEF_SCALE` into this step as a permanent record.

**Measured and computed** (printed by the harness itself, using
`hornvale_kernel::math::ln` for the `atanh` identity `atanh(x) = 0.5 *
ln((1+x)/(1-x))` — not `f64::ln`):

```
median(cost_exponent) = 6.256709   (cost_exponent_all's q0.50, pooled over all EdgeKinds)
x = (1.0 - 0.75) / (1.40 - 0.75) = 0.3846153846
atanh(x) = 0.5 * ln((1+x)/(1-x)) = 0.4054651081
DEF_SCALE = 6.256709 / 0.4054651081 = 15.430944
```

**`DEF_SCALE = 15.430944`.**

- [x] **Step 4: Check the fallback trigger (spec §4.4)**

Compute the defensibility the chosen `DEF_SCALE` yields at the land
population's q0.05 and q0.95 (`Adjacency`-supplied approaches only). If that
range spans **less than 0.10**, the single-scale form cannot grade the 87% of
the world that is land-only, and spec §4.4's pre-specified fallback applies:
normalize `cost_exponent` within the approach's `EdgeKind` before the `tanh`.

Record the computed range and which branch obtains. **Taking the fallback is
executing the spec, not amending it** — it was specified with its trigger
before this measurement ran. Report the number either way.

**Computed** (using `DEF_MIN = 0.75`, `DEF_MAX = 1.40`, `DEF_SCALE = 15.430944`,
and `hornvale_kernel::math::tanh` — the same libm-backed fn the production
formula uses):

```
adjacency cost_exponent q0.05 = 4.871373 -> defensibility = 0.948642
adjacency cost_exponent q0.95 = 6.985179 -> defensibility = 1.025661
spread = 1.025661 - 0.948642 = 0.077019
```

**`0.077019 < 0.10` — the fallback trigger obtains.** Spec §4.4's
pre-specified fallback applies: `cost_exponent` must be normalized within the
approach's `EdgeKind` before the `tanh`. A single `DEF_SCALE` calibrated to
put the pooled median at defensibility 1.0 grades the land-only 87% of the
world across a span of only ~0.077 — under a tenth of the `[0.75, 1.40)`
range — which is not enough discrimination to be a meaningful second contest
axis for the large majority of habitable cells that are never water-connected.
This is not a surprise given §2.3a's own numbers (land `cost_exponent` "roughly
3.1 to 6.9", closely matching this run's adjacency q0.05=4.87/q0.95=6.99/
min=3.068/max=8.47): the land population's spread in *cost_exponent* is real,
but it sits almost entirely in the flat, saturated part of `tanh` once the
scale is set by a pooled median that a small, near-`0`-cost-exponent
water-connected minority pulls down. Per spec, **taking the fallback is
executing the spec, not amending it**; implementing it is Task 3's job, not
this measurement task's.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/worldgen/tests/approach_ease_calibration.rs docs/superpowers/plans/2026-07-29-the-contour.md
git commit -m "test(the-contour): calibrate DEF_SCALE from the cost-exponent distribution

The quantity the amended mechanism reads, measured before any behavioural
constant exists. Records which branch of spec 4.4's pre-specified fallback
trigger obtains. Task 2d."
```

---

### Task 3: `defensibility` — the per-approach field

> **Amended 2026-07-30 (spec amendment 1, pre-readout).** Task 2's calibration
> found approach ease is two disjoint regimes split on `WaterRoute` vs
> `Adjacency`, not one distribution — so defensibility now reads **the
> approach's own conductance**, not a per-cell aggregate. See spec §2.3/§2.3a.
> `approach_ease` from Task 1 is retained: it is what the calibration measures
> and what Task 2's record is written against, but **nothing in the shipped
> mechanism calls it**, so its `#[allow(dead_code)]` stays.

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs`
- Test: `windows/worldgen/tests/defensibility_field.rs` (create)

**Interfaces:**
- Consumes: `hornvale_topology::{ConnectionGraph, Edge}`, `hornvale_kernel::math::{ln, tanh}`, the constant frozen in Task 2d.
- Produces: `fn defensibility(graph: &ConnectionGraph, from: CellId, to: CellId) -> f64`. Task 6 consumes it with the exact argument order `(graph, from, to)` — `from` is the ATTACKER, `to` is the HOLDER. Getting these backwards is the defect the behavioural test exists to catch.

- [ ] **Step 1: Write the failing property tests**

Create `windows/worldgen/tests/defensibility_field.rs`:

```rust
//! The per-approach defensibility field (spec §2.3): strictly monotone in
//! route cost, bounded in `[DEF_MIN, DEF_MAX)` with `DEF_MIN` ATTAINED at a
//! free route, parallel edges resolved by MAXIMUM conductance, and a pure
//! function of the graph.

use hornvale_kernel::CellId;
use hornvale_topology::{ConnectionGraph, Edge, EdgeKind};
use hornvale_worldgen::defensibility_for_test as defensibility;

fn link(conductances: &[(EdgeKind, f64)]) -> ConnectionGraph {
    let mut g = ConnectionGraph::new(2);
    for &(kind, c) in conductances {
        g.add_edge(CellId(0), Edge { to: CellId(1), kind, conductance: c });
    }
    g
}

#[test]
fn defensibility_rises_strictly_as_the_route_gets_dearer() {
    let mut prev = f64::NEG_INFINITY;
    for step in 1..40 {
        let c = 1.0 / (step as f64);
        let d = defensibility(&link(&[(EdgeKind::Adjacency, c)]), CellId(0), CellId(1));
        assert!(d > prev, "must rise strictly as conductance falls, at step {step}");
        prev = d;
    }
}

#[test]
fn a_free_route_attains_the_floor_and_nothing_reaches_the_ceiling() {
    // conductance == 1.0 is real (the calibration measured it) and SHOULD sit
    // exactly at DEF_MIN: an unobstructed sea lane is the most exposed ground
    // there is. Spec §2.3 corrects an earlier over-reading of decision 0089
    // clause 3, which governs world OUTCOMES, not intermediate fields.
    let free = defensibility(&link(&[(EdgeKind::WaterRoute, 1.0)]), CellId(0), CellId(1));
    assert!((free - 0.75).abs() < 1e-12, "a free route sits at DEF_MIN: got {free}");

    let dear = defensibility(&link(&[(EdgeKind::Adjacency, 1.0e-9)]), CellId(0), CellId(1));
    assert!(dear < 1.40, "nothing reaches DEF_MAX: got {dear}");
}

#[test]
fn parallel_edges_resolve_by_maximum_conductance() {
    // 6.7% of real cells carry an Adjacency AND a LandRoute to the same
    // neighbour (Task 2b). An attacker uses the EASIEST road, so the max wins.
    // A `min` would over-defend and a `sum` would double-count.
    let both = link(&[(EdgeKind::Adjacency, 0.001), (EdgeKind::LandRoute, 0.02)]);
    let only_easy = link(&[(EdgeKind::LandRoute, 0.02)]);
    assert_eq!(
        defensibility(&both, CellId(0), CellId(1)),
        defensibility(&only_easy, CellId(0), CellId(1)),
        "the easiest parallel route must decide"
    );
}

#[test]
fn defensibility_is_deterministic_across_recomputation() {
    let g = link(&[(EdgeKind::Adjacency, 0.0031)]);
    let first = defensibility(&g, CellId(0), CellId(1));
    for _ in 0..8 {
        assert_eq!(defensibility(&g, CellId(0), CellId(1)), first);
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-worldgen --test defensibility_field`
Expected: FAIL — `defensibility_for_test` not found in `hornvale_worldgen`

- [ ] **Step 3: Implement, substituting Task 2d's measured `DEF_SCALE`**

In `history_bake.rs`, beside the other bake constants:

```rust
/// AUTHORED prior: the defensibility of a free route — the value a wholly
/// unobstructed approach yields. ATTAINED, not approached: a cell reached by
/// an open sea lane is the most exposed ground there is (spec §2.3).
/// type-audit: bare-ok(ratio: DEF_MIN)
const DEF_MIN: f64 = 0.75;
/// AUTHORED prior: the defensibility an infinitely dear approach tends to.
/// Approached and never reached, since `tanh` is asymptotic.
/// type-audit: bare-ok(ratio: DEF_MAX)
const DEF_MAX: f64 = 1.40;
/// CALIBRATED (Task 2d): scales `-ln(conductance)` so the MEDIAN traversable
/// approach sits at defensibility 1.0. Measured over seeds 1..=30 before any
/// behavioural readout existed, and frozen thereafter (spec §4.4). A
/// save-format constant from here on.
/// type-audit: bare-ok(ratio: DEF_SCALE)
const DEF_SCALE: f64 = 0.0; // <- REPLACE with Task 2d's measured value
```

And the function, beside `approach_ease`:

```rust
/// How well `to` is defended against an approach from `from`: a strictly
/// monotone, saturating function of the log traversal cost of the cheapest
/// route between them. A multiplier on the HOLDER's side of the dominance
/// test — the second contest axis (decision 0089 clause 1).
///
/// Reads the approach rather than the cell because the calibration found
/// approach ease is two disjoint regimes — water-connected and land-only —
/// which no single transform over an aggregate can grade (spec §2.3a). A raid
/// arrives along one route, and what shelters the defender is the resistance
/// of that route.
///
/// Parallel edges resolve by MAXIMUM conductance: an attacker takes the
/// easiest road, which is also why this cannot double-count the 6.7% of cells
/// carrying duplicate `to` values.
///
/// Pure in `(graph, from, to)` — no seed, no time, no bake state — so it
/// consumes no draw and cannot move stream consumption order. Returns
/// `DEF_MAX` for a nonexistent or wholly impassable link, which no caller
/// reaches: both call sites walk edges that exist.
fn defensibility(graph: &ConnectionGraph, from: CellId, to: CellId) -> f64 {
    let best = graph
        .edges(from)
        .iter()
        .filter(|e| e.to == to && e.conductance > 0.0)
        .map(|e| e.conductance)
        .fold(0.0_f64, f64::max);
    if best <= 0.0 {
        return DEF_MAX;
    }
    let cost_exponent = -hornvale_kernel::math::ln(best);
    DEF_MIN + (DEF_MAX - DEF_MIN) * hornvale_kernel::math::tanh(cost_exponent / DEF_SCALE)
}

/// Test-only re-export of [`defensibility`] so the property battery in
/// `tests/defensibility_field.rs` can reach it without making the field part
/// of this crate's real public surface.
#[doc(hidden)]
pub fn defensibility_for_test(graph: &ConnectionGraph, from: CellId, to: CellId) -> f64 {
    defensibility(graph, from, to)
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-worldgen --test defensibility_field`
Expected: PASS, 4 tests

- [ ] **Step 5: Mutation-verify the parallel-edge test**

Temporarily change the `fold(0.0_f64, f64::max)` to a `sum()`, re-run, and
confirm `parallel_edges_resolve_by_maximum_conductance` FAILS. Then change it
to a `min` over the filtered set and confirm it FAILS again. Revert. If either
mutant passes, the test asserts nothing and must be rewritten before
proceeding — this is the test that pins the double-counting fix.

- [ ] **Step 6: fmt, clippy, type-audit, commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/worldgen/
git commit -m "feat(the-contour): defensibility as a per-approach field

Reads the approach's own conductance, not a per-cell aggregate: the
calibration found approach ease is two disjoint regimes (water-connected
vs land-only) that no single transform can grade. Parallel edges resolve
by max, which fixes the measured 6.7% duplicate-edge double-count by
construction. Spec amendment 1, pre-readout. Task 3."
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
          bake ends — the decision-0089 compliance reading",
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
fn a_cheaply_reached_holder_is_raided_and_a_dearly_reached_one_is_not() {
    // Two holders identical in every respect except the CONDUCTANCE of the
    // route reaching them from the raider. Only the cheaply-reached one may
    // be taken. Two defects this catches: the term wired to the ATTACKER's
    // side, and `from`/`to` transposed (the graph is mirrored, so a
    // transposition compiles and mostly works — it fails exactly when the
    // two cells' parallel-edge sets differ).
    let mut bake = /* fixture: raider + two holders, equal population and tech */
        unimplemented!("build via the existing test fixture helpers in this module");
    let _ = &mut bake;
    todo!("assert the cheaply-reached holder is raided and the dear one is skipped");
}
```

Replace the `unimplemented!`/`todo!` using the fixture helpers already in that module — `stores_raise_strength_but_never_pressure` (~line 3337) shows the established shape. The fixture must give the two holders' approach edges **different conductances**, since that is now the only thing the mechanism reads.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-worldgen a_cheaply_reached_holder`
Expected: FAIL — both holders are raided, because defensibility is not yet read.

- [ ] **Step 3: Change the two call sites**

Note the argument order: **`from` is the attacker's cell, `to` is the holder's.**

`maybe_raid`, line 2544 — the raider sits at `raider_site` and the candidate is `n`:

```rust
            if raider_str <= t_str * defensibility(self.cur(), raider_site, n) * RAID_MARGIN {
                continue; // dominance: only a fight it can win, by this road
            }
```

`best_home`, line 1127 — the approach comes from the ring-walk's origin `from`:

```rust
                        if !may_take_held_land
                            || strength <= hs * defensibility(self.cur(), from, n) * RAID_MARGIN
                        {
                            continue; // not a fight this people can win here, or survive winning
                        }
```

**These are the only two sites.** Defensibility must not enter `strength`, `pressure_of`, `eff_capacity`, or tribute assessment — the same discipline that kept `stores` out of `pressure_of`.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-worldgen a_cheaply_reached_holder`
Expected: PASS

- [ ] **Step 5: Mutation-verify the test**

Temporarily change `defensibility` to `fn defensibility(_g: &ConnectionGraph, _f: CellId, _t: CellId) -> f64 { 1.0 }` and re-run. Then, separately, TRANSPOSE the two call sites' arguments (`defensibility(self.cur(), n, raider_site)`) and re-run again — the graph is mirrored, so a transposition compiles and mostly agrees, and a test that cannot see it is not pinning the direction.

Run: `cargo test -p hornvale-worldgen a_cheaply_reached_holder`
Expected: **FAIL on both mutants.** If it passes, the test asserts nothing and must be rewritten before proceeding. Revert the stub afterwards.

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
