# Campaign L0: The Hornvale Lab — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The batch experiment harness: `hornvale lab run <study.json>` sweeps seeds × pin-sets over full worlds, extracts registry metrics, and publishes drift-checked summaries and SVG charts into the book's new Laboratory section — Study 001 (the Census of Skies) included.

**Architecture:** The composition root extracts from the cli into a new `windows/worldgen` library (behavior-preserving, refereed by the committed artifacts — this also pays the WASM-reuse debt). A new `windows/lab` crate holds the metrics registry (code), study format (JSON data), deterministic runner, summarizer, and hand-rolled SVG charts. The cli gains the `lab` subcommands. CI reruns the 500-seed drift study; the 10k census runs at author time.

**Tech Stack:** Rust edition 2024. serde + serde_json only (already in budget). No plotting libraries — charts are deterministic hand-rolled SVG per the dataviz pass: single accent `#5b8dd9` (validated on light and dark surfaces), `currentColor` text/grid so inlined SVGs inherit mdbook's theme, identity carried by axis labels (single-series charts need no legend), every chart paired with its summary table.

## Global Constraints

- Spec: `docs/superpowers/specs/2026-07-06-campaign-l0-the-lab-design.md`. Studies are data, metrics are code (§2.1); measure via full `build_world` (§2.3); loud validation failures; the seed is identity.
- **Task 1 is behavior-preserving:** all three committed almanacs, concepts, and streams artifacts must regenerate byte-identically after the extraction; 209 tests stay green.
- Determinism: same study file → byte-identical CSV, summary, and SVGs. `BTreeMap` for anything aggregated; fixed float formatting (`{:.1}` for percentages and numeric labels).
- Chart contract (dataviz-locked): bars fill `#5b8dd9`, rx=2, ≥2px gaps; text/axis/gridlines `currentColor` (grid at opacity 0.15); no background rect; viewBox 640×300; value labels above bars; x-labels rotated −30° when any label exceeds 8 chars; titles are chart-local `<text>`, not markdown.
- Every commit passes the full gate (`cargo test --workspace`, `cargo fmt --check` — run `cargo fmt` as the FINAL pre-commit step and paste raw output — `cargo clippy --workspace --all-targets -- -D warnings`); `#![warn(missing_docs)]`; docs on all public items including fields/variants.

## File Structure

```
windows/worldgen/Cargo.toml, src/lib.rs   — T1: extracted composition root (former cli/src/world_builder.rs)
cli/src/{main,repl,concepts}.rs           — T1: import updates; T7: lab subcommands
windows/lab/Cargo.toml                    — T2 (deps: kernel, worldgen, astronomy, religion, serde, serde_json)
windows/lab/src/lib.rs                    — module decls + re-exports (T2–T6)
windows/lab/src/metrics.rs                — T2: MetricValue, SummaryKind, Metric, WorldView, registry()
windows/lab/src/study.rs                  — T3: Study/Seeds/PinSet/MetricSelection, load_study, validation
windows/lab/src/runner.rs                 — T4: Row, RunResult, run(), write_csv()
windows/lab/src/summary.rs                — T5: render_summary()
windows/lab/src/chart.rs                  — T6: bar_chart_svg(), charts_for()
windows/lab/src/publish.rs                — T6: publish() → summary + charts into the book
studies/census-of-skies.study.json        — T7 (10,000 seeds)
studies/census-drift.study.json           — T7 (500 seeds)
book/src/laboratory/{overview.md,study-001.md} — T8 prose shells
book/src/laboratory/generated/*           — T7/T8: committed generated outputs
book/src/SUMMARY.md, .github/workflows/ci.yml, .gitignore (lab-out/) — T7/T8
```

---

### Task 1: Extract `windows/worldgen` (behavior-preserving)

**Files:**
- Create: `windows/worldgen/Cargo.toml`, `windows/worldgen/src/lib.rs`
- Modify: `cli/Cargo.toml`, `cli/src/main.rs`, `cli/src/repl.rs`, `cli/src/concepts.rs` (delete `cli/src/world_builder.rs`)

**Interfaces:**
- Produces: crate `hornvale-worldgen` exporting everything `cli/src/world_builder.rs` exports today, unchanged signatures: `SkyChoice`, `Sky`, `BuildError`, `register_all`, `build_world(Seed, &SkyPins, SkyChoice) -> Result<World, BuildError>`, `sky_of`, `observed_phenomena`, `sky_report`, `climate_report`, `almanac_context`, `calendar_lines`, `night_sky_line`, `genesis_notes` — plus all its unit tests, moved.
- The cli depends on `hornvale-worldgen` and its modules refer to `hornvale_worldgen::` where they said `crate::world_builder::` (or `world_builder::`). No logic changes anywhere.

- [ ] **Step 1: Create the crate**

`windows/worldgen/Cargo.toml`:

```toml
[package]
name = "hornvale-worldgen"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale composition root: where all domains meet to build worlds."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
hornvale-astronomy = { path = "../../domains/astronomy" }
hornvale-climate = { path = "../../domains/climate" }
hornvale-terrain = { path = "../../domains/terrain" }
hornvale-settlement = { path = "../../domains/settlement" }
hornvale-culture = { path = "../../domains/culture" }
hornvale-religion = { path = "../../domains/religion" }
hornvale-almanac = { path = "../../windows/almanac" }
```

Move `cli/src/world_builder.rs` verbatim to `windows/worldgen/src/lib.rs`, with only these mechanical edits: prepend the crate docs + lint (`//! Hornvale composition root — the only place all domains meet.` and `#![warn(missing_docs)]`); the file's former `//!` module comment folds into that; `use crate::...` paths that referred to cli modules do not exist (world_builder had none — verify); tests stay inline.

- [ ] **Step 2: Rewire the cli**

`cli/Cargo.toml`: add `hornvale-worldgen = { path = "../windows/worldgen" }`.
`cli/src/main.rs`: delete `mod world_builder;`, add `use hornvale_worldgen as world_builder;` — every existing `world_builder::` reference keeps compiling unchanged. Same one-line aliasing in `repl.rs`/`concepts.rs` if they name `crate::world_builder` (change to `use hornvale_worldgen as world_builder;` and drop the `crate::` prefixes).

- [ ] **Step 3: The referee**

Run: `cargo test --workspace` (expect 209 — tests moved, none lost; paste the total), then regenerate WITHOUT committing and diff all five artifacts:

```bash
cargo run -q -p hornvale -- new --seed 42 --sky constant --out /tmp/l0-c.json
cargo run -q -p hornvale -- almanac --world /tmp/l0-c.json | diff - book/src/gallery/almanac-seed-42.md
cargo run -q -p hornvale -- new --seed 42 --out /tmp/l0-s.json
cargo run -q -p hornvale -- almanac --world /tmp/l0-s.json | diff - book/src/gallery/almanac-seed-42-sky.md
cargo run -q -p hornvale -- new --seed 42 --rotation locked --out /tmp/l0-l.json
cargo run -q -p hornvale -- almanac --world /tmp/l0-l.json | diff - book/src/gallery/almanac-seed-42-locked.md
cargo run -q -p hornvale -- concepts | diff - book/src/reference/concept-registry-generated.md
cargo run -q -p hornvale -- streams | diff - book/src/reference/stream-manifest-generated.md
```

Expected: every diff empty.

- [ ] **Step 4: Gate, commit**

```bash
git add windows/worldgen/ cli/ Cargo.lock
git commit -m "refactor(worldgen): composition root extracted to a library — artifact-refereed"
```

---

### Task 2: `windows/lab` — metrics registry

**Files:**
- Create: `windows/lab/Cargo.toml`, `windows/lab/src/lib.rs`, `windows/lab/src/metrics.rs` (tests inline)

**Interfaces:**
- Produces:

```rust
pub enum MetricValue { Number(f64), Text(String), Flag(bool), Absent }   // Clone, Debug, PartialEq
pub enum SummaryKind { Categorical, Flag, Numeric { bucket_edges: &'static [f64] } }
pub struct Metric {
    pub name: &'static str,
    pub doc: &'static str,
    pub summary: SummaryKind,
    pub extract: fn(&WorldView) -> MetricValue,
}
pub struct WorldView { pub world: World, pub system: StarSystem, pub calendar: Calendar, pub notes: Vec<String> }
impl WorldView { pub fn build(seed: Seed, pins: &SkyPins) -> Result<WorldView, BuildError> }  // build_world + sky_of; Generated only
pub fn registry() -> Vec<Metric>
pub fn render_metric_list() -> String   // markdown table for `lab list-metrics`
```

- The fourteen tier-1 metrics, with exact names, kinds, and semantics:

| name | kind | value |
|---|---|---|
| `star-class` | Categorical | `system.star.class_name` |
| `tidally-locked` | Flag | rotation is Locked |
| `day-length-hours` | Numeric `[16,20,24,28,32,36,40]` | day×24; Absent if locked |
| `year-std-days` | Numeric `[0,200,400,600,800,1000,1200,1400]` | anchor year |
| `year-local-days` | Numeric `[0,200,400,600,800,1000,1200,1400]` | year in local days; Absent if locked |
| `obliquity-degrees` | Numeric `[0,5,10,15,20,25,30,35]` | obliquity |
| `moons-admitted` | Categorical | moons.len() as text |
| `refused-a-moon` | Flag | `!notes.is_empty()` |
| `total-tide` | Numeric `[0,1,2,3,4,5,6,7,8]` | Σ tide_rel |
| `months-per-year-innermost` | Numeric `[0,5,10,25,50,100,250,700]` | year/period of moons[0]; Absent if no moons |
| `neighbor-count` | Categorical | neighbors.len() as text |
| `brightest-neighbor-class` | Categorical | Debug-name of neighbors[0].class in kebab (map explicitly: RedDwarf→"red-dwarf", SunLike→"sun-like", WhiteDwarf→"white-dwarf", OrangeGiant→"orange-giant", RedGiant→"red-giant", BlueGiant→"blue-giant") |
| `belief-kind` | Categorical | first belief's tenet contains "never" → "eternal", else "cyclic"; Absent if no beliefs. **Deliberately classified from tenet TEXT, independent of rotation, so the calibration against rotation is non-circular.** |
| `genesis-note-count` | Categorical | notes.len() as text |

- [ ] **Step 1: Manifest + failing tests**

`windows/lab/Cargo.toml`:

```toml
[package]
name = "hornvale-lab"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale laboratory: batch studies over generated worlds."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
hornvale-worldgen = { path = "../worldgen" }
hornvale-astronomy = { path = "../../domains/astronomy" }
hornvale-religion = { path = "../../domains/religion" }
serde = { workspace = true }
serde_json = { workspace = true }
```

Tests (in `metrics.rs`): build a `WorldView` for seed 42 default pins and assert `star-class`/`moons-admitted`/`belief-kind` extract non-Absent values of the right variants; a `--rotation locked` view yields `tidally-locked`=Flag(true), `day-length-hours`=Absent, `year-local-days`=Absent, `belief-kind`=Text("eternal"); a spinning view yields Text("cyclic"); seed 23 default yields `refused-a-moon`=Flag(true) and `genesis-note-count`=Text("1"); `registry()` has 14 uniquely-named entries; `render_metric_list()` contains every name.

- [ ] **Step 2–4: Implement per the table, gate, commit**

```bash
git add Cargo.lock windows/lab/
git commit -m "feat(lab): metrics registry — fourteen tier-1 extractors over full worlds"
```

---

### Task 3: Study format and validation

**Files:**
- Create: `windows/lab/src/study.rs` (tests inline); wire into lib.rs

**Interfaces:**
- Produces:

```rust
#[derive(Debug, Clone, PartialEq, serde::Deserialize)] pub struct Seeds { pub from: u64, pub count: u64 }
#[derive(...)] pub struct PinSet { pub label: String, pub pins: Vec<String> }
#[derive(...)] #[serde(untagged)] pub enum MetricSelection { All(String), Named(Vec<String>) }  // "all" | ["a","b"]
#[derive(...)] pub struct Study { pub name: String, pub description: String, pub seeds: Seeds, pub pin_sets: Vec<PinSet>, pub metrics: MetricSelection }
pub struct StudyError { pub message: String }   // Display + Error
pub fn load_study(path: &Path) -> Result<Study, StudyError>   // read + parse + validate
impl Study { pub fn selected_metrics(&self) -> Result<Vec<Metric>, StudyError>; pub fn pin_sets_parsed(&self) -> Result<Vec<(String, SkyPins)>, StudyError> }
```

- Validation (each failure loudly named in `StudyError.message`): name matches `^[a-z0-9-]+$` (hand-rolled check, no regex crate: all chars ascii-lowercase/digit/hyphen, non-empty); `seeds.count > 0`; at least one pin set; duplicate pin-set labels rejected; every pin string parses via `hornvale_astronomy::parse_pin`; `MetricSelection::All` string must equal `"all"`; every named metric exists in `registry()`.

- [ ] **Steps: failing tests (valid file parses; each invalid variant errs with its reason substring — "unknown metric", "duplicate pin-set label", "invalid pin", "name must be", "count must be"), implement, gate, commit**

```bash
git commit -m "feat(lab): studies as JSON data — loud validation, one pin parser"
```

---

### Task 4: The runner

**Files:**
- Create: `windows/lab/src/runner.rs` (tests inline); wire into lib.rs; add `lab-out/` to `.gitignore`

**Interfaces:**
- Produces:

```rust
pub struct Row { pub seed: u64, pub pin_set: String, pub values: Vec<MetricValue>, pub refusal: Option<String> }
pub struct RunResult { pub study: Study, pub metric_names: Vec<&'static str>, pub rows: Vec<Row> }
pub fn run(study: &Study) -> Result<RunResult, StudyError>
pub fn write_csv(result: &RunResult, out_root: &Path) -> std::io::Result<PathBuf>  // lab-out/<study>/rows.csv
```

- Semantics: for each pin set (file order) × each seed (ascending from `seeds.from`): `WorldView::build`; on `Ok`, extract every selected metric in registry order; on `Err(BuildError::Genesis(e))`, emit a refusal row (all values Absent, `refusal: Some(e.to_string())`) — **refusals are data, not crashes**; any other BuildError aborts loudly. CSV: header `seed,pin_set,<metric names...>,refusal`; `Number` as `{}` (Rust shortest round-trip), `Text` verbatim, `Flag` as `true`/`false`, `Absent` empty; text fields never contain commas today (assert in a test) so no quoting layer.

- [ ] **Steps: failing tests (tiny 5-seed study runs; run twice → identical CSV bytes; a pin set of `["moons=3"]` over seeds 6..8 includes seed 7 as a refusal row with "no stable orbit" in the reason; row count = seeds × pin_sets), implement, gate, commit**

```bash
git commit -m "feat(lab): deterministic runner — refusals are data"
```

---

### Task 5: Summaries

**Files:**
- Create: `windows/lab/src/summary.rs` (tests inline); wire into lib.rs

**Interfaces:**
- Produces: `pub fn render_summary(result: &RunResult) -> String` — generated markdown, deterministic:
  - Header: `<!-- GENERATED FILE — do not edit. Regenerate with \`hornvale lab run\`. -->`, then `## Study: {name}`, the description, and a line `Seeds {from}..{from+count} × {n} pin set(s); {rows} rows; {refusals} refusals.`
  - Per pin set, per metric (registry order): a `### {metric} — {pin set label}` block with a table. Categorical/Flag: `| value | count | share |` rows in BTreeMap order, share as `{:.1}%`. Numeric: `| bucket | count | share |` with rows `< first`, `[a, b)` per consecutive edge pair, `≥ last`, and `absent`; bucket labels formatted `{}` from the edge values.
  - Refusals section when any: `| pin set | count | example reason |`.

- [ ] **Steps: failing tests (hand-built RunResult with known rows → exact expected table fragments; bucket boundary value lands in `[a,b)` not `< a`; determinism), implement, gate, commit**

```bash
git commit -m "feat(lab): summaries — deterministic tables from runs"
```

---

### Task 6: Charts and publish

**Files:**
- Create: `windows/lab/src/chart.rs`, `windows/lab/src/publish.rs` (tests inline); wire into lib.rs

**Interfaces:**
- Produces:

```rust
pub fn bar_chart_svg(title: &str, x_labels: &[String], counts: &[u64]) -> String
pub fn charts_for(result: &RunResult) -> Vec<(String, String)>   // (file stem "{study}-{pinset}-{metric}", svg)
pub fn publish(result: &RunResult, generated_dir: &Path) -> std::io::Result<Vec<PathBuf>>
    // writes {study}-summary.md + every chart SVG into book/src/laboratory/generated/
```

- **Chart contract (dataviz-locked, normative):** viewBox `0 0 640 300`, `width="100%"`, `font-family="ui-sans-serif, system-ui, sans-serif"`; margins left 56 / right 16 / top 36 / bottom 64; title at (56, 20) `font-size="13" font-weight="600" fill="currentColor"`; 4 horizontal gridlines `stroke="currentColor" stroke-opacity="0.15" stroke-width="1"` with y-axis labels `font-size="11" fill="currentColor" opacity="0.75"` (max rounded up to a 1-2-5 nice number); bars `fill="#5b8dd9" rx="2"`, slot gap = max(2px, 15% of slot); count label centered above each bar `font-size="11" fill="currentColor"`; x labels `font-size="11" fill="currentColor" opacity="0.85"`, rotated `transform="rotate(-30 x y)"` with `text-anchor="end"` when any label length > 8, else centered horizontal; no background rect. Charts derive from the SAME aggregation code path as summaries (factor the per-metric distribution into a shared helper in summary.rs: `pub(crate) fn distribution(kind, values) -> Vec<(String, u64)>`) so table and chart can never disagree.
- publish paths: `generated_dir.join(format!("{stem}.svg"))` and `{study}-summary.md`.

- [ ] **Steps: failing tests (svg contains `#5b8dd9`, `currentColor`, the title, one `<rect` per non-empty label, rotated labels iff a long label exists; publish writes summary + N charts; determinism), implement, gate, commit**

```bash
git commit -m "feat(lab): charts — single-accent SVG sharing the summary's aggregation"
```

---

### Task 7: CLI subcommands, study files, and the author-time census

**Files:**
- Modify: `cli/Cargo.toml` (add `hornvale-lab`), `cli/src/main.rs` (dispatch + USAGE)
- Create: `studies/census-of-skies.study.json`, `studies/census-drift.study.json`
- Create (generated + committed): `book/src/laboratory/generated/*` for BOTH studies

**Interfaces:**
- `hornvale lab run <path>`: load_study → run → `write_csv(&result, Path::new("lab-out"))` → `publish(&result, Path::new("book/src/laboratory/generated"))` → print `study {name}: {rows} rows, {refusals} refusals; summary + {n} charts published.` Errors are user-facing (`error: {e}`, exit 1). `hornvale lab list-metrics`: print `render_metric_list()`. USAGE updated.
- Study files:

```json
{ "name": "census-of-skies",
  "description": "Distributions of everything the sky generator produces, plus the belief split as instrument calibration.",
  "seeds": { "from": 0, "count": 10000 },
  "pin_sets": [ { "label": "default", "pins": [] } ],
  "metrics": "all" }
```

`census-drift.study.json`: identical but `"name": "census-drift"`, `"count": 500`.

- [ ] **Steps: dispatch tests (unknown path → error; list-metrics contains "belief-kind"); run BOTH studies at author time (census ~minutes — report the wall time); commit the generated outputs; full gate**

```bash
git add cli/ studies/ book/src/laboratory/generated/ Cargo.lock
git commit -m "feat(cli): hornvale lab — census of skies run and published"
```

---

### Task 8: The Laboratory in the book, CI, and calibration

**Files:**
- Create: `book/src/laboratory/overview.md`, `book/src/laboratory/study-001.md`
- Modify: `book/src/SUMMARY.md`, `.github/workflows/ci.yml`
- Create: `windows/lab/tests/calibration.rs`

**Steps:**

1. `overview.md` (shell — final prose is post-plan, comprehension-gated): what a study is (data selecting code-side metrics), how to author one (the JSON shape, `lab list-metrics`), how the drift-check keeps the instrument honest; ends with `### The instrument's self-check` including `{{#include generated/census-drift-summary.md}}`.
2. `study-001.md` (shell): title, one-paragraph framing, `{{#include generated/census-of-skies-summary.md}}`, then a `### Selected charts` section including at least: `moons-admitted`, `refused-a-moon`, `belief-kind`, `day-length-hours` SVGs via `{{#include ...}}`.
3. SUMMARY gains, after The Gallery section:

```markdown
# The Laboratory

- [The Laboratory](./laboratory/overview.md)
- [Study 001: The Census of Skies](./laboratory/study-001.md)
```

4. ci.yml artifact step gains (before the `git diff --exit-code` line):

```yaml
          cargo run -p hornvale -- lab run studies/census-drift.study.json
```

and the diff line extends to `git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/`. NOTE: the drift run rewrites only census-drift outputs; census-of-skies outputs sit in the same dir and are covered by the diff (they can only drift if someone reruns the 10k study with changed behavior — exactly the author-time referee we want).
5. `windows/lab/tests/calibration.rs` — the spec §10 assertion:

```rust
//! Calibration: at tier 0, belief kind is a pure function of rotation.
//! The instrument must reproduce known ground truth exactly (spec §2.5).
use hornvale_lab::{load_study, run, MetricValue};
use std::path::Path;

#[test]
fn eternal_beliefs_coincide_exactly_with_tidal_locking() {
    let study = load_study(Path::new("../../studies/census-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (locked_i, belief_i) = (idx("tidally-locked"), idx("belief-kind"));
    for row in &result.rows {
        let locked = matches!(row.values[locked_i], MetricValue::Flag(true));
        let eternal = matches!(&row.values[belief_i], MetricValue::Text(t) if t == "eternal");
        assert_eq!(locked, eternal, "seed {}: calibration violated", row.seed);
    }
}
```

6. `mdbook build book`; rerun the drift study → `git status` clean (no-op proof); full gate; commit:

```bash
git add book/ .github/ windows/lab/tests/
git commit -m "feat(book): the Laboratory opens — drift-checked census, calibration proven"
```

---

## Post-plan (not subagent work)

Study 001's analysis prose (overview + chapter narrative — the moon-refusal
rate headline, calibration discussion) written in-session, comprehension-
gated; L0 chronicle entry + freshness sweep per the DoD; frontier/memory
updates.

## Self-Review Notes

- **Spec coverage:** §3 format (T3), §4 runner (T4), §5 registry — all 14
  metrics with the non-circular belief classifier (T2), §6 outputs (T4–T6),
  §7 book (T7–T8), §8 CLI + worldgen extraction (T1, T7), §9 CI drift study
  (T8), §10 tests incl. calibration (T8), §11 exit criteria: (1) author-time
  rerun no-op (T7/T8 proofs), (2) refusal rate = `refused-a-moon` share in
  the census summary, (3) calibration test, (4) Laboratory live (T8).
- **Type consistency:** `MetricValue`/`SummaryKind`/`Metric`/`WorldView`
  names match across T2–T8; `run`/`write_csv`/`render_summary`/`publish`
  signatures consistent; the shared `distribution` helper is declared in T6
  and lives in summary.rs.
- **Determinism guards:** BTreeMap aggregation, fixed formatting, registry
  order for columns, file-order pin sets, ascending seeds.
- **Known risks:** census wall-time (spec §13 — shrink to 5k before adding
  complexity); chart legibility is Nathan's taste gate at the chapter read.
