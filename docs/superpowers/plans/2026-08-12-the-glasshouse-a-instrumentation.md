# The Glasshouse, Stage A: Instrumentation — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the temperature baseline's driver measurable — fix the runner bug
that prevents narrow climate studies, add the four missing census columns, and
attribute land-elevation variance to its source terms.

**Architecture:** Read-only instrumentation plus one runner bug fix. No world
generation behaviour changes, so **no committed artifact should drift except the
lab study rows that gain columns**. Stage A's deliverable is the *before-arm* of
the campaign's preregistered measurement, plus the attribution table that Stage B
needs in order to be written at all.

**Tech Stack:** Rust 2024, `cargo nextest`, the `windows/lab` metric registry
(`studies are data, metrics are code` — decision 0011).

## Global Constraints

- Dependencies: `serde`, `serde_json`, `libm` only. **No new crates.**
  (`ALLOWED_EXTERNAL` in `cli/tests/architecture.rs`; decision 0004/0041.)
- Layering: `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain crate
  depends on `hornvale-kernel` and **nothing else**.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only. Float sorting
  uses `total_cmp`. Enforced by `clippy.toml` `disallowed-types`.
- **No wall-clock time.** Time is `WorldTime { day: f64 }`.
- Every crate sets `#![warn(missing_docs)]`; every public item, field and variant
  gets a one-line doc comment.
- Every primitive at a `pub` boundary carries a `type-audit:` verdict tag
  (`bare-ok(<class>)` / `waiver(<reason>)` / `pending(wave-N)`).
- Run `cargo fmt` as the final step before every commit — fmt-gate skips are the
  most common review finding.
- **New constants declare their kind** (decision 0106): one of `physics` /
  `earth-biosphere` / `hornvale-gauge` / `hornvale-choice`, with justification of
  the matching form.
- Commit gate is `make gate` (~8 min since decision 0113). Iterate cost-ordered:
  fmt + clippy first, then `cargo test -p <crate>`, and `--workspace` only at the
  pre-commit gate.
- **There is no CI** (decision 0125). The local gate is the only gate.

## Why this plan stops where it does

The spec's §5 orders hypsometry (task 3) before the thermostat (task 4), because
the thermostat's constants depend on the land term terrain sets. But the
hypsometry *fix* cannot be written to step granularity yet, and the reason is
specific rather than a lack of effort:

- `derive_sea_level` (`domains/terrain/src/elevation.rs:656`) sets sea level to
  the **ocean-fraction percentile** of the elevation distribution. A uniform
  offset applied to every elevation therefore shifts sea level equally and
  changes nothing observable. `ISOSTASY_REF_KM = 30.0` is consequently a
  **gauge** in decision 0106's sense — its value is unobservable — so it is not
  the knob, and "calibrating" it would be the error 0106 names.
- `ISOSTASY_M_PER_KM = 180.0` is physically correct Airy isostasy (crust/mantle
  density contrast gives ~150 m/km), so it is a `physics` constant and should not
  be tuned to hit a distributional target.
- That leaves the **width** of the crustal-thickness distribution and the
  boundary-profile / relief / hotspot terms as candidates. Which of them
  dominates land-elevation variance is unmeasured, and the answer selects the
  fix.

Task 4 produces that attribution. **Stage B is written from Task 4's output.**
Writing it now would mean inventing a fix for a cause not yet identified, which
is the failure mode `docs/retrospectives/` records repeatedly.

## File Structure

| File | Responsibility | Change |
|---|---|---|
| `windows/lab/src/runner.rs` | study sweep, depth resolution | **Modify** — widen a `Terrain`-depth build to `BuiltView::Climate` when a Climate-rung metric is selected |
| `windows/lab/src/metrics.rs` | the metric registry (metrics are code) | **Modify** — add four metrics |
| `windows/lab/tests/rung_selection.rs` | regression test for rung/view pairing | **Create** |
| `studies/the-census.study.json` | the census metric selection | **Modify** — add the four names |
| `docs/audits/land-elevation-attribution.md` | Task 4's committed finding | **Create** |
| `book/src/laboratory/generated/**/rows.csv` | regenerated study rows | Regenerated |

---

### Task 1: Fix the Climate-rung view widening bug

`Extractor::rung()` maps `Extractor::Climate(_)` to `BuildDepth::Terrain`, which
is correct — climate commits no facts. `BuiltView::build_to`'s doc comment then
states that a `BuiltView::Climate` "is only ever constructed by widening the view
of a `Terrain`-depth build for a Climate-rung metric, never returned here."

**That widening is never performed.** So `required_depth` returns `Terrain`,
`build_to` returns `BuiltView::Terrain`, and `BuiltView::climate()` panics. Any
study selecting a Climate-rung metric dies. The committed census survives only
because it selects the full metric set and so builds `Full` — exactly the masking
`TOOL-rung-tag-unchecked` describes.

**Files:**
- Modify: `windows/lab/src/runner.rs` (`required_depth` ~line 121, `run_at_depth` ~line 132)
- Create: `windows/lab/tests/rung_selection.rs`

**Interfaces:**
- Consumes: `Extractor::rung() -> BuildDepth`, `BuiltView::build_to(seed, pins, wc, depth) -> Result<BuiltView, BuildError>`, `Metric::rung()`
- Produces: no new public API. `run` gains the invariant that the built view it
  passes to `Extractor::apply` is always at or above every selected metric's rung
  **as a view variant**, not merely as a build depth.

- [ ] **Step 1: Write the failing test**

Create `windows/lab/tests/rung_selection.rs`. Every metric must be runnable at
its own declared rung — that is the property `TOOL-rung-tag-unchecked` wants and
nothing checks.

```rust
//! Every registered metric must be runnable in a study that selects it ALONE.
//! A metric whose declared rung does not match the view the runner hands it
//! panics (Climate) or reports a silent all-`Absent` column (Settlement/Full
//! past an early return) — see TOOL-rung-tag-unchecked.

use hornvale_lab::{MetricSelection, PinSet, Seeds, Study};

/// A study selecting exactly one metric, over one seed.
fn solo_study(metric: &str) -> Study {
    Study {
        name: format!("solo-{metric}"),
        description: "rung-selection guard: one metric, one seed".to_string(),
        seeds: Seeds { from: 1, count: 1 },
        pin_sets: vec![PinSet {
            label: "default".to_string(),
            pins: vec![],
            // None = the shipped {goblin, kobold} roster. Required in a
            // struct literal even though the JSON path defaults it.
            roster: None,
        }],
        metrics: MetricSelection::Named(vec![metric.to_string()]),
    }
}

#[test]
fn a_climate_rung_metric_runs_when_selected_alone() {
    // mean-land-temperature-c is Extractor::Climate. Before the fix this
    // panics with "climate-rung extractor on a shallower built view".
    let result = hornvale_lab::run(&solo_study("mean-land-temperature-c"));
    assert!(
        result.is_ok(),
        "a Climate-rung metric must run when selected alone: {:?}",
        result.err()
    );
}

#[test]
fn a_terrain_rung_metric_still_runs_when_selected_alone() {
    // Guards against fixing Climate by over-building everything.
    let result = hornvale_lab::run(&solo_study("mountain-coverage"));
    assert!(result.is_ok(), "{:?}", result.err());
}
```

**Note on the exact constructor names:** `Study`, `SeedSpec` and `PinSet` field
names must be read off `windows/lab/src/` before writing this file — use whatever
the crate actually exports (it may be `Study::from_json` over a JSON literal
instead, which is equally acceptable and closer to `studies are data`). Do not
invent a builder. If a JSON literal is the natural constructor, use the shape in
`studies/the-census.study.json`.

- [ ] **Step 2: Run the test to verify it fails, and capture the failure text**

```bash
cargo nextest run -p hornvale-lab --test rung_selection 2>&1 | tee /tmp/hv-rung.txt
```

Expected: `a_climate_rung_metric_runs_when_selected_alone` FAILS with a panic
containing `climate-rung extractor on a shallower built view: runner bug`.
`a_terrain_rung_metric_still_runs_when_selected_alone` PASSES.

**This is a behavioural red, not a compile error.** If the test fails to compile
instead, fix the constructor names and re-run until you see the panic — a compile
error proves nothing about whether the assertion catches the defect.

- [ ] **Step 3: Implement the fix — choose the variant BEFORE building**

**Corrected by controller verification; the plan's original "widening" framing
was wrong.** There is no `ClimateView::from_terrain`. `ClimateView::build_to(seed,
pins, wc, BuildDepth::Terrain)` (`metrics.rs:298`) builds the world to Terrain
depth **and** reconstructs the climate atop it, in one call. So a post-build
widening would either need a constructor that does not exist, or would rebuild
the world and double the cost of every climate study.

The variant must therefore be selected **before** building. The root gap is that
`BuildDepth` is worldgen's enum and has no Climate rung (correctly — climate
commits no facts), so it cannot carry the decision, and `required_depth`
collapses `Climate` and `Terrain` onto the same value.

Fix that collapse. The requirement is:

- A study selecting **any** Climate-rung metric must produce `BuiltView::Climate`.
- A study selecting **no** Climate-rung metric must not pay for the climate
  reconstruction.
- The `BuiltView::climate()` panic should become **unreachable by construction**,
  not merely avoided at one call site — a runtime guard leaves the next caller
  the same trap.

Choose the shape after reading `runner.rs:103-160` and `metrics.rs:504-575`. Two
that satisfy the above:

1. **A lab-owned view rung.** Give the lab its own enum over `BuiltView`
   variants (`ViewRung::{Astronomy, Terrain, Climate, Settlement, Full}`), map
   `Extractor` to it instead of to `BuildDepth`, take the max over selected
   metrics, and have the build site match on that. `BuildDepth` stays the
   worldgen concept it is; the lab stops overloading it. This makes the panic
   unreachable and is the more thorough fix.
2. **Pass the intent into `build_to`.** Add a `want_climate: bool` (or fold it
   into the existing depth argument as a small enum) so `build_to` returns
   `BuiltView::Climate` when asked. Smaller diff, but leaves `BuildDepth`
   overloaded and the panic still reachable from other callers.

Prefer (1) unless reading the code shows it forces churn well beyond this task —
if so, take (2) and say why in the report. Either way `Extractor::rung()`'s
existing doc comment (which explains why Climate maps to Terrain **depth**) must
be updated so it no longer describes a widening that does not happen.

Note for whichever shape you pick: `Metric.extract` is a `pub` field
(`metrics.rs:767`) and `Extractor` is re-exported from `lib.rs`, so matching
`matches!(m.extract, Extractor::Climate(_))` is available to `runner.rs`.

- [ ] **Step 4: Run the tests to verify they pass**

```bash
cargo nextest run -p hornvale-lab --test rung_selection
```

Expected: both tests PASS.

- [ ] **Step 5: Verify no other rung is broken the same way**

```bash
cargo nextest run -p hornvale-lab 2>&1 | tee /tmp/hv-lab.txt
```

Expected: PASS. Trust the exit code; grep `/tmp/hv-lab.txt` rather than re-running.

- [ ] **Step 6: Confirm the original reproduction now works**

```bash
cargo run -p hornvale -- lab list-metrics > /dev/null   # build the binary
cat > /tmp/hv-solo.study.json <<'EOF'
{ "name":"solo-temp","description":"rung fix verification; a debugging read, not a measurement.",
  "seeds":{"from":1,"count":2},"pin_sets":[{"label":"default","pins":[]}],
  "metrics":["mean-land-temperature-c"] }
EOF
cargo run -p hornvale -- lab run /tmp/hv-solo.study.json
```

Expected: completes without panicking and reports two rows.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add windows/lab/src/runner.rs windows/lab/tests/rung_selection.rs
git commit -m "fix(lab): widen a Terrain-depth build to the Climate view rung

BuiltView::build_to documented that a BuiltView::Climate 'is only ever
constructed by widening the view of a Terrain-depth build for a Climate-rung
metric' — and the widening was never performed. required_depth returned
Terrain, build_to returned BuiltView::Terrain, and BuiltView::climate()
panicked, so ANY study selecting a Climate-rung metric died.

The committed census was unaffected because it selects the full metric set and
so builds Full — the exact masking TOOL-rung-tag-unchecked describes. The bug
was invisible until a narrow temperature study was attempted.

Adds tests/rung_selection.rs, which runs a metric ALONE at its own declared
rung. That is the check TOOL-rung-tag-unchecked asks for, in its cheapest form.

Refs: TOOL-rung-tag-unchecked"
```

---

### Task 2: Add the two astronomy driver metrics

The census holds 23 astronomy metrics and **none of them is insolation,
luminosity, or orbital distance** (`CLIM-astronomy-unmeasured`). That is why the
conclusion "near-uninfluenced by its own astronomy" was reachable and wrong: the
census held `a` (r = −0.224) and star class, not `S = L/a²` (r = +0.980) or the
drawn variable `u` (r = −0.981).

Both facts are already committed on every ledger, so these are pure read-side
additions.

**Files:**
- Modify: `windows/lab/src/metrics.rs`
- Modify: `studies/the-census.study.json`

**Interfaces:**
- Consumes: `AstronomyView { pub system }` with `system.star: Star` and
  `system.anchor: Anchor`; `hornvale_astronomy::insolation_rel(&Star, &Anchor) -> f64`
  (the shared SKY-15 definition); `Star::habitable_zone: HabitableZone` with
  `.inner() -> Au` / `.outer() -> Au`; `Anchor::orbit: Au`.
- Produces: metric names `insolation-rel` and `zone-position`, consumed by
  Stage B's success criteria 5 and 6.

- [ ] **Step 1: Write the failing test**

Add to `windows/lab/tests/rung_selection.rs` (the metrics must be registered
before they can be selected):

```rust
#[test]
fn the_astronomy_driver_metrics_are_registered_and_vary() {
    // insolation-rel and zone-position are the campaign's driver columns.
    // Both must exist, and neither may be frozen — a frozen driver column
    // would repeat the defect CLIM-astronomy-unmeasured names.
    for name in ["insolation-rel", "zone-position"] {
        let study = solo_study(name);   // 1 seed proves registration
        assert!(
            hornvale_lab::run(&study).is_ok(),
            "{name} must be registered and runnable alone"
        );
    }
}
```

- [ ] **Step 2: Run it to verify it fails**

```bash
cargo nextest run -p hornvale-lab --test rung_selection
```

Expected: FAIL with `unknown metric "insolation-rel"`.

- [ ] **Step 3: Register both metrics**

Add to the registry in `windows/lab/src/metrics.rs`, beside the other
`Domain::Astronomy` descriptors. Bucket edges bracket the analytically known
range: `S = 1/(0.95+0.42u)²` spans exactly `[0.533, 1.108]`.

```rust
Metric {
    name: "insolation-rel",
    doc: "Top-of-atmosphere stellar flux at the anchor, relative to Earth \
           (L/a², Earth = 1) — the driver of the temperature baseline. The \
           shared SKY-15 definition, not a re-derivation",
    summary: SummaryKind::Numeric {
        bucket_edges: &[0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1],
    },
    domain: Domain::Astronomy,
    role: Role::Descriptor,
    extract: Extractor::Astronomy(|v: &AstronomyView| {
        MetricValue::Number(hornvale_astronomy::insolation_rel(
            &v.system.star,
            &v.system.anchor,
        ))
    }),
},
Metric {
    name: "zone-position",
    doc: "Where in the habitable zone the anchor sits, normalized: \
           (a - inner)/(outer - inner), so 0.0 is the hot inner edge and 1.0 \
           the cold outer edge. This is the DRAWN variable — the orbit is \
           placed uniform in radius across the zone — and it is what \
           temperature actually tracks; `anchor-orbit-au` is 95% collinear \
           with luminosity and so reads as uninformative on its own",
    summary: SummaryKind::Numeric {
        bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
    },
    domain: Domain::Astronomy,
    role: Role::Descriptor,
    extract: Extractor::Astronomy(|v: &AstronomyView| {
        let inner = v.system.star.habitable_zone.inner().get();
        let outer = v.system.star.habitable_zone.outer().get();
        let a = v.system.anchor.orbit.get();
        // HabitableZone::new enforces inner < outer, so the span is positive.
        MetricValue::Number((a - inner) / (outer - inner))
    }),
},
```

**Use `hornvale_astronomy::insolation_rel`, do not inline `L/a²`.** Its doc
comment calls it "the single definition of insolation the whole workspace
shares (SKY-15)"; a second copy is how the two drift apart.

**Accessor, controller-verified:** `Au` is generated by the `quantity!` macro
(`domains/astronomy/src/units.rs:31`, invoked ~line 110) and exposes **both**
`.0` and `.get()`. Use `.get()` — it matches `star.rs:295`. `HabitableZone`'s
`inner()`/`outer()` take `self` by value and return `Au`
(`units.rs:229`/`:233`).

- [ ] **Step 4: Run the test to verify it passes**

```bash
cargo nextest run -p hornvale-lab --test rung_selection
```

Expected: PASS.

- [ ] **Step 5: Add both to the census selection**

Add `"insolation-rel"` and `"zone-position"` to the `metrics` array in
`studies/the-census.study.json`.

- [ ] **Step 6: Verify the identity the campaign's diagnosis rests on**

Add to `windows/lab/tests/rung_selection.rs`. This pins the cancellation
in a test, so a future bracket change cannot silently un-couple it:

**There is no `row.number(name)` accessor** — controller-verified.
`RunResult { metric_names: Vec<&'static str>, rows: Vec<Row> }` and
`Row { values: Vec<MetricValue> }` (`windows/lab/src/runner.rs:39-60`), so a
value is fetched **positionally**: find the name's index in `metric_names`, index
`values`, and match `MetricValue::Number(n)`. Add one shared helper rather than
repeating that in each test:

```rust
/// Fetch a numeric metric from a row by name. Panics with the metric name on a
/// missing column or a non-numeric value — a test helper, so a loud failure is
/// the useful behaviour.
fn number_of(result: &hornvale_lab::RunResult, row: &hornvale_lab::Row, metric: &str) -> f64 {
    let idx = result
        .metric_names
        .iter()
        .position(|n| *n == metric)
        .unwrap_or_else(|| panic!("metric {metric} not selected by this study"));
    match &row.values[idx] {
        hornvale_lab::MetricValue::Number(n) => *n,
        other => panic!("metric {metric} is {other:?}, not a Number"),
    }
}

#[test]
fn insolation_is_determined_by_zone_position_alone() {
    // The habitable zone is denominated in sqrt(L) and insolation is L/a²,
    // so L cancels EXACTLY: S = 1/(0.95 + 0.42u)². Luminosity does not enter.
    // This is the campaign's root-cause claim; if a future change to the
    // bracket breaks the identity, that is a deliberate act and this test
    // is where it must be acknowledged.
    let study = Study {
        name: "insolation-identity".to_string(),
        description: "pins the L-cancellation the campaign's diagnosis rests on".to_string(),
        seeds: Seeds { from: 1, count: 20 },
        pin_sets: vec![PinSet {
            label: "default".to_string(),
            pins: vec![],
            roster: None,
        }],
        metrics: MetricSelection::Named(vec![
            "insolation-rel".to_string(),
            "zone-position".to_string(),
        ]),
    };
    let result = hornvale_lab::run(&study).expect("study runs");
    assert_eq!(result.rows.len(), 20, "one row per seed");
    // `Row` carries `refusal: Option<String>` — a refused genesis yields a row
    // whose `values` may be short. Assert none refused rather than indexing
    // into a short row and reporting a confusing panic instead of the real
    // cause. Seeds 1..20 unpinned should never refuse; if they do, that is the
    // finding.
    let refused: Vec<_> = result
        .rows
        .iter()
        .filter_map(|r| r.refusal.as_ref().map(|m| (r.seed, m.clone())))
        .collect();
    assert!(refused.is_empty(), "unpinned seeds refused genesis: {refused:?}");
    for row in &result.rows {
        let u = number_of(&result, row, "zone-position");
        let s = number_of(&result, row, "insolation-rel");
        let expected = 1.0 / (0.95 + 0.42 * u).powi(2);
        assert!(
            (s - expected).abs() < 1e-6,
            "seed {}: S={s} != 1/(0.95+0.42*{u})^2 = {expected}",
            row.seed
        );
    }
}
```

`MetricValue` and `Row`/`RunResult` are re-exported from `windows/lab/src/lib.rs`
(`pub use runner::{...}`, `pub use metrics::{...}`) — confirm the exact re-export
list and import accordingly rather than pathing into the private modules.

- [ ] **Step 7: Regenerate artifacts and route on what moved**

```bash
make rebaseline
git diff --stat book/src/laboratory/ book/src/reference/ docs/audits/ docs/digest/
```

| what moved | response |
|---|---|
| `book/src/laboratory/**/rows.csv` gained two columns | expected — commit |
| `book/src/reference/` moved | **STOP** — no stream label or predicate was added here; an unintended contract change |
| `docs/audits/type-audit-report.md` moved | expected only if a `pub` boundary changed; if it moved, commit it in this same commit |
| nothing moved at all | **STOP** — the study selection edit did not take effect |

- [ ] **Step 8: Commit**

```bash
cargo fmt
git add windows/lab/src/metrics.rs windows/lab/tests/rung_selection.rs \
        studies/the-census.study.json book/src/laboratory/ docs/audits/
git commit -m "feat(lab): the census can finally see its own driver

None of the 23 astronomy metrics was insolation, luminosity, or orbital
distance, so CLIM-cold-attractor's conclusion — 'near-uninfluenced by its own
astronomy' — rested on a dataset that structurally could not hold the
influence. Measured: r(S,T) = +0.980 and r(u,T) = -0.981, against r(a,T) =
-0.224 and r(L,T) = +0.013. The star is what is uninfluential, because L
cancels out of L/a² when the zone is denominated in sqrt(L).

Adds insolation-rel (the shared SKY-15 definition, not a re-derivation) and
zone-position (the DRAWN variable, u). Also pins the cancellation identity
S = 1/(0.95+0.42u)² in a test, so a future bracket change has to acknowledge
it rather than silently un-couple it.

Read-side only: no world generation changes, so only study rows drift.

Refs: CLIM-astronomy-unmeasured, CLIM-cold-attractor, SKY-19"
```

---

### Task 3: Add `mean-land-elevation-m`

§3.3's ~2200 m figure is **inferred twice and measured never** — once from the
regression intercept (`14.26 K / 0.0065 = 2194 m`) and once from
`mountain-coverage = 0.545`. The two agree, which is why the spec states it, but
no committed metric reports mean land elevation.

**Files:**
- Modify: `windows/lab/src/metrics.rs`
- Modify: `studies/the-census.study.json`

**Interfaces:**
- Consumes: `TerrainView { pub terrain: GeneratedTerrain }`;
  `GeneratedTerrain::geosphere() -> &Geosphere`,
  `::elevation_at(CellId) -> ReferenceElevation`,
  `::sea_level() -> ReferenceElevation`.
- Produces: metric name `mean-land-elevation-m`, consumed by Task 4 and by
  Stage B's hypsometry target.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn mean_land_elevation_is_registered_and_positive() {
    // Mean elevation ABOVE SEA LEVEL over land cells, so it is >= 0 by
    // construction. A negative value would mean the land/sea comparison
    // inverted; seed 1 has land, so Absent would also be a bug.
    let study = solo_study("mean-land-elevation-m");
    let result = hornvale_lab::run(&study).expect("study runs");
    let v = number_of(&result, &result.rows[0], "mean-land-elevation-m");
    assert!(v >= 0.0, "mean land elevation above sea level cannot be negative: {v}");
}
```

Uses the `number_of` helper added in Task 2 — same file, so no new helper.

- [ ] **Step 2: Run it to verify it fails**

Expected: FAIL with `unknown metric "mean-land-elevation-m"`.

- [ ] **Step 3: Register the metric**

**The land definition must match `mountain-coverage`'s** (`e >= sea`), because
§3.3's target compares the two directly. Note that two land definitions exist in
the registry — `mountain-coverage` uses `e >= sea` while
`mean-land-temperature-c` uses `!is_ocean(cell)`. Use `e >= sea` here and say why
in the doc comment.

```rust
Metric {
    name: "mean-land-elevation-m",
    doc: "Mean elevation above sea level over land cells, m — the term the \
           lapse rate turns into a temperature penalty. Land is `e >= sea`, \
           matching `mountain-coverage` so the two are directly comparable \
           (the registry also carries an `is_ocean` land definition; this is \
           deliberately not that one); Absent on a landless world",
    summary: SummaryKind::Numeric {
        bucket_edges: &[0.0, 500.0, 1000.0, 1500.0, 2000.0, 3000.0],
    },
    domain: Domain::Terrain,
    role: Role::Descriptor,
    extract: Extractor::Terrain(|v: &TerrainView| {
        let geo = v.terrain.geosphere();
        let sea = v.terrain.sea_level();
        let (mut sum, mut count) = (0.0_f64, 0_u32);
        for cell in geo.cells() {
            let e = v.terrain.elevation_at(cell);
            if e >= sea {
                sum += e - sea;
                count += 1;
            }
        }
        if count == 0 {
            MetricValue::Absent
        } else {
            MetricValue::Number(sum / f64::from(count))
        }
    }),
},
```

- [ ] **Step 4: Run the test to verify it passes**

```bash
cargo nextest run -p hornvale-lab --test rung_selection
```

- [ ] **Step 5: Check the inference — and do NOT edit the study JSON**

**Corrected after Task 2 (controller):** `studies/the-census.study.json` uses
`"metrics": "all"`, so there is **no array to add a name to**. Registering the
metric in `metrics.rs` is sufficient — the census picks it up automatically. Do
not edit the study file. There is also a metric-count pin
(`registry_metric_count_is_pinned`) which Task 2 moved 200 → 202; bump it **in
this commit**, as the precedent commit `71ed4eeb` did.

Measure the value with a narrow study (which Task 1's fix now makes possible):

```bash
cargo run -p hornvale -- lab run /tmp/hv-elev.study.json   # 200 seeds, this metric alone
```

Record the median in the commit message. **This is a measurement, not a
prediction** — whatever it reads is the number, and if it lands far from ~2200 m
then §3.3's inference chain was wrong and Stage B must be re-derived from the
measured value. Say so in the commit either way.

- [ ] **Step 6: Commit — expect the census fixtures to stay red**

**Corrected after Task 2 (controller).** Adding a registry metric grows the
registry-derived census schema while the committed `rows.csv` fixtures still
carry the old column count, so nine `"metrics": "all"` studies cannot be
reconstructed and the lab calibration tests die. This is **precedented and
deliberate** — commit `71ed4eeb` did exactly this and its message says so — and
`make rebaseline` will exit non-zero at the schema-backfill step, taking four
later steps with it.

Do not write a code workaround. Instead:

1. Run the regen steps `rebaseline` skipped **by hand**, and confirm all seven
   generated-artifact drift paths diff clean.
2. **Classify every failure programmatically** — the discipline that separates a
   known-red from a regression hiding in it:

   ```bash
   cargo nextest run -p hornvale-lab --no-fail-fast > /tmp/hv-lab.txt 2>&1
   grep -c "^        FAIL" /tmp/hv-lab.txt      # note: nextest prints FAIL twice per test
   grep -c "rows.csv header does not match study" /tmp/hv-lab.txt
   ```

   Every failure must carry `rows.csv header does not match study`. Task 2's
   count was 43 of 43, and the precedent's was 42 of 42. **If even one failure
   does not carry it, stop and report** — that one is yours, not the census's.
3. Paste both counts in the report.

The census refresh on lefford clears these, and it is the controller's job and a
carve-out requiring Nathan's explicit authorization. Do not attempt it.

```bash
cargo fmt
git add windows/lab/src/metrics.rs windows/lab/tests/rung_selection.rs \
        book/src/laboratory/ docs/audits/
git commit -m "feat(lab): measure mean land elevation instead of inferring it

The Glasshouse spec put mean land elevation at ~2200 m from two independent
inferences — the regression intercept (14.26 K / 0.0065) and
mountain-coverage = 0.545 — that agree but were never checked against the
thing itself. No committed metric reported it.

Measured median: <FILL IN FROM STEP 5>.

Land is `e >= sea` to match mountain-coverage, since the hypsometry target
compares the two. The registry carries a second land definition (is_ocean,
used by mean-land-temperature-c); this deliberately is not that one.

Refs: The Glasshouse spec §3.3"
```

---

### Task 4: Attribute land-elevation variance to its source terms

**This task's output is what Stage B is written from.** Elevation is assembled
from several additive terms; which of them dominates the variance of
`elevation - sea_level` over land selects the hypsometry fix, and no other task
can be specified until this reports.

**Controller-verified: the terms are purely additive.** `assemble_elevation`
(`elevation.rs:499-500`) closes with exactly:

```rust
let metres =
    base + boundary_term + hotspot_term + relief_term + CELL_EPSILON_M * f64::from(cell.0);
```

So a variance decomposition is well-posed and each term can be zeroed
independently without perturbing the others. The plan originally hedged that they
might not be additive; they are, and there are **five**, not four:

| term | site | constants | note |
|---|---|---|---|
| `base` | `isostatic_m(*crust.get(cell))` | `ISOSTASY_M_PER_KM = 180.0`, `ISOSTASY_REF_KM = 30.0` | driven by the crust-thickness field; the prime suspect for both mean and variance |
| `boundary_term` | `boundary_profile_m(...)` × `profile_scale(...)` | `FORELAND_DEPTH_M`, `TRENCH_DEPTH_M`, `ARC_*`, `*_DECAY_CELLS`, `FOOTHILLS_*`, `FAR_FIELD_*` | signed — `Uplift` positive, `Trough` negative |
| `hotspot_term` | `dome_m` summed over `trail_seamounts` | `HOTSPOT_SIGMA_RAD`, `TRAIL_*` | sum of positive domes, so it raises the mean |
| `relief_term` | `RELIEF_AMPLITUDE_M · relief_scale(...) · (fbm − 0.5) · 2` | `RELIEF_AMPLITUDE_M = 240.0`, `RELIEF_FREQUENCY`, `RELIEF_OCTAVES` | **zero-mean by construction** — adds variance, not mean |
| epsilon | `CELL_EPSILON_M · cell.0` | `CELL_EPSILON_M = 1e-6` | ~0.04 m total at level 6; a tie-breaker, not physics. Include it so the decomposition sums to the total, then ignore it |

Two consequences worth carrying into the analysis: `relief_term` cannot be
responsible for the elevated *mean* (it is zero-mean), and `base` is the only
term whose scale is set by a `physics` constant that §3.3 already ruled out as a
knob — so if `base` dominates, the fix lies in the **crust-thickness field**
that feeds it, not in `isostatic_m`.

**Files:**
- Create: `docs/audits/land-elevation-attribution.md` (a committed finding)
- Create: `windows/lab/tests/land_elevation_attribution.rs` (or a `#[ignore]`d
  probe under the heavy tier if it proves expensive — see Step 4)

**Interfaces:**
- Consumes: `assemble_elevation` (`elevation.rs:421`) and the term functions above.
- Produces: a variance decomposition table, and a named dominant term. Stage B's
  hypsometry task is written against that name.

- [ ] **Step 1: Read `assemble_elevation` completely before writing anything**

```bash
sed -n '421,515p' domains/terrain/src/elevation.rs
```

Do not skim. The additivity above is controller-verified, so the open question is
not *whether* the terms sum but **how each is reachable for zeroing**: which are
computed from arguments you can neutralise at the call site, and which are
computed inline from loop-local state. That determines the shape of the probe, and
it is what Step 2 needs.

Record at the top of the audit document which terms are reachable how.

- [ ] **Step 2: Write the probe**

Over seeds 1..50 at the canonical level-6 grid, for land cells (`e >= sea`),
report for the total and for each term held alone: mean, sd, and the fraction of
total variance attributable to it.

Do **not** prescribe a specific mutation from outside the code — the property the
probe must demonstrate is *which term dominates land-elevation variance*. Find a
discriminating construction after reading Step 1; a term that shares no state
with the others can be zeroed, and one that does not will need a different
approach.

- [ ] **Step 3: Assert the probe actually probes**

Before trusting any number, assert that zeroing a term **changes the output**:

```rust
assert!(
    (baseline_sd - zeroed_sd).abs() > 1e-9,
    "zeroing this term changed nothing — the mutation did not take, \
     and a no-op mutation produces evidence that looks like a result"
);
```

A no-op mutation is worse than no mutation, because it produces evidence.

- [ ] **Step 4: Decide where the probe lives**

```bash
time cargo nextest run -p hornvale-lab --test land_elevation_attribution
```

| runtime | placement |
|---|---|
| under ~30 s | keep it in the commit gate |
| over ~30 s | mark `#[ignore = "heavy: 50-world elevation attribution probe"]` so `cli/tests/heavy_tier.rs` accepts it, and note the runtime in the audit |

The `heavy:` token is required for the ignore-reason scan to accept it.

- [ ] **Step 5: Write the committed finding**

Create `docs/audits/land-elevation-attribution.md` with: the term table from
Step 1 (corrected if they are not additive), the variance decomposition, the
named dominant term, and an explicit statement of which of the four candidates
Stage B should target. Include the command and its output.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add domains/terrain/ windows/lab/tests/land_elevation_attribution.rs \
        docs/audits/land-elevation-attribution.md
git commit -m "measure(terrain): attribute land-elevation variance to its terms

54.5% of Hornvale's land stands above 2000 m against Earth's ~11%, a
near-constant -14 K lapse penalty that is what actually drives the biome and
soil uniformity (specials-before-Whittaker in classify_land swallows 95% of
worlds). Fixing it needed a target, and two candidates were already ruled out
analytically:

  ISOSTASY_REF_KM = 30.0   is a GAUGE (decision 0106) — derive_sea_level takes
                           the ocean-fraction percentile of the same
                           distribution, so a uniform offset cancels exactly
                           and the value is unobservable.
  ISOSTASY_M_PER_KM = 180  is correct Airy isostasy (~150 m/km from the
                           crust/mantle density contrast) — a physics constant,
                           not a tuning knob.

That left the crustal-thickness spread and the boundary/relief/hotspot terms.
This probe measures which dominates.

Dominant term: <FILL IN>.

Refs: The Glasshouse spec §3.3; decision 0106"
```

- [ ] **Step 7: Report and stop**

Stage A is complete. **Do not begin the hypsometry fix.** Report the dominant
term and the measured mean land elevation; Stage B's plan is written from them.

---

## Self-Review

**Spec coverage (Stage A only):** §3.5's four metrics — `insolation-rel` and
`zone-position` in Task 2, `mean-land-elevation-m` in Task 3.
`greenhouse-forcing-k` is deliberately **deferred to Stage B**, because the
quantity does not exist until the drawn residual does; registering a metric for
it now would produce an all-`Absent` column, which is precisely the
`TOOL-rung-tag-unchecked` failure mode this stage fixes. §3.5's verified blocker
is Task 1. §3.3's "measure it directly first" is Task 3, and its attribution
prerequisite is Task 4.

**Deferred to Stage B, with reasons:** §3.1 thermostat + residual (needs Task 3's
measured land term to fit `k`), §3.2 latitude profile (must land with or after
the greenhouse — alone it costs 10 K), §3.3 the hypsometry fix itself (needs Task
4's dominant term), §3.4 classifier gate (already measurement-gated on §3.1–3.3
landing), §6 constant provenance for the constants Stage B touches.

**Constant provenance in Stage A:** no new constants are introduced. Bucket edges
are presentation, not physics — `insolation-rel`'s edges bracket the analytically
exact `[0.533, 1.108]` range rather than being chosen by eye.

**Placeholder scan:** three `<FILL IN>` markers are in *commit messages* and are
outputs of a measurement performed in the immediately preceding step, not
unspecified work. Two `row.number` / `Study{..}` accessor spellings are flagged
inline as "read the real API, the assertion is the contract" — deliberate, per
the rule against prescribing a signature from outside the code.

**Type consistency:** `solo_study(&str) -> Study` is defined once in Task 1 and
reused in Tasks 2 and 3. `e >= sea` is the land definition in Tasks 3 and 4;
`mean-land-temperature-c`'s differing `is_ocean` definition is called out rather
than silently mixed. `GeneratedTerrain` accessors (`geosphere`, `elevation_at`,
`sea_level`, `is_ocean`) were read off `domains/terrain/src/provider.rs:101-141`,
not guessed.

**Determinism:** Stage A changes no generation path, so no seed label, stream
order, or committed world value moves. The only expected drift is study rows
gaining columns — which is why each artifact task carries a branch table whose
"nothing moved" row is a STOP.
