# The Domesday Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Generate a human-readable survey of the thousand census worlds, with preregistered detectors that make world-generation weaknesses impossible to miss.

**Architecture:** A new module in `windows/lab` reads the **committed** `rows.csv` + `schema.json` (never re-running the census, per decisions 0032/0110), computes per-metric statistics, applies seven preregistered weakness detectors, and renders one Book page per domain. Metrics gain two required fields — `domain` (the chapter axis) and `role` (which keeps the detectors from drowning in deliberate invariants).

**Tech Stack:** Rust 2024, `serde`/`serde_json`, `hornvale-kernel` (quantize). No new dependencies.

## Global Constraints

- **Spec:** `docs/superpowers/specs/2026-08-08-the-domesday-design.md`. Where this plan and the spec disagree, the spec governs.
- **Never re-run the census.** Read `book/src/laboratory/generated/the-census/{rows.csv,schema.json}` only. The gate must never pay for 1,000 worlds. A test asserts this.
- **Never set `HV_CENSUS=1`.** `scripts/regenerate-artifacts.sh` skips censuses by default and must stay that way.
- **Determinism:** all float output goes through `hornvale_kernel::quantize` at the emit boundary. Sorting uses `total_cmp`. Ties break lexicographically. No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only (enforced workspace-wide by `clippy.toml`).
- **Preregistered thresholds are frozen** (decision 0016): D1 ≥ 80 %, D3 IQR < 5 % of range, D6 per-comparator band. **D5 has no threshold** — an expectation declares a strength *class* and the detector compares it to the observed class against conventional effect-size bands (spec §4.4). Retuning is a named commit that says so, never a silent edit.
- **`book/src/domesday/` must be `git add`ed in the commit that creates it.** `git diff --exit-code` is silently vacuous against an untracked path (The Digest, Task 7).
- Rust edition 2024; `#![warn(missing_docs)]` — every public item, field, and variant gets a one-line doc comment; primitives at `pub` boundaries need a `type-audit:` tag.
- Run `cargo fmt` as the final step before every commit.
- Commit messages end with: `Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU`

## File Structure

| File | Responsibility |
|---|---|
| `windows/lab/src/metrics.rs` | **modified** — `Metric` gains `domain` + `role`; 191 literals annotated |
| `windows/lab/src/schema.rs` | **modified** — emit `domain` and `role` into `schema.json` |
| `windows/lab/src/domesday/mod.rs` | module root, public entry `render_survey` |
| `windows/lab/src/domesday/census.rs` | load committed `rows.csv` + `schema.json` into a typed table |
| `windows/lab/src/domesday/stats.rs` | per-kind summary statistics |
| `windows/lab/src/domesday/comparators.rs` | load + validate `studies/comparators.json` |
| `windows/lab/src/domesday/detect.rs` | the seven detectors |
| `windows/lab/src/domesday/render.rs` | markdown for one domain page + the index |
| `studies/comparators.json` | real-world reference values (data) |
| `studies/expectations.json` | declared correlations for D5 (data) |
| `book/src/domesday/*.md` | generated output |

---

### Task 1: `domain` and `role` on every metric

**Files:**
- Modify: `windows/lab/src/metrics.rs` (the `Metric` struct and all 191 literals)
- Modify: `windows/lab/src/schema.rs` (emit both fields)

**Interfaces:**
- Produces: `pub enum Domain { Astronomy, Terrain, Climate, Hydrology, Biology, Settlement, Society, Religion, Language, Naming, History }` with `pub fn as_str(&self) -> &'static str`; `pub enum Role { Descriptor, Invariant }` with `as_str`; `Metric { name, doc, summary, extract, domain: Domain, role: Role }`

**The compiler is the enforcement.** Both fields are required (not `Option`), so all 191 literals fail to compile until annotated. That is stronger than a lint and it is why this is one task rather than several — the crate does not build until it is finished.

**Annotation guidance.** ~86 metrics inherit their domain from `rung` (`astronomy` → `Astronomy`, `terrain` → `Terrain`, `settlements` → `Settlement`). The 105 in `full` cluster by name: `pantheon-*`/`cult-*`/`belief-*`/`head-deity-*` → `Religion`; `lexicon-*`/`homophony-*`/`inventory-*`/`confusable-*`/`divergence-*`/`phonotactic-*`/`monophyly-*` → `Language`; `name-*` → `Naming`; `vestige-*`/`forgotten-*` → `History`.

**`role` guidance.** A metric is `Invariant` if it asserts a property that should hold on every world — `phonotactic-validity-*`, `monophyly-*`, `lexicon-regular-*`, `inventory-closure-*`, `name-gloss-true` are the clear cases (all are `true` on all 1,000 worlds today). Everything else is `Descriptor`. **When unsure, choose `Descriptor`** — a false `Invariant` silences D1/D2 on a real weakness, which is the failure that matters; a false `Descriptor` merely produces a D1 hit you can reclassify.

- [ ] **Step 1: Write the failing test**

Add to `windows/lab/src/schema.rs`'s test module:

```rust
    #[test]
    fn schema_carries_domain_and_role() {
        let json = render_schema(&two_seed_result(), &two_seed_csv(), false);
        let v: serde_json::Value = serde_json::from_str(&json).expect("schema parses");
        let cols = v["columns"].as_array().expect("columns");
        let star = cols
            .iter()
            .find(|c| c["name"] == "star-class")
            .expect("star-class present");
        assert_eq!(star["domain"], "astronomy");
        assert_eq!(star["role"], "descriptor");
    }
```

(`two_seed_result()` / `two_seed_csv()` are the existing helpers this module's tests already use; reuse them exactly as `schema_types_documents_and_binds_the_csv` does.)

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-lab --lib schema`
Expected: FAIL — `domain` is `Null`.

- [ ] **Step 3: Add the two enums and the fields**

In `windows/lab/src/metrics.rs`, above `pub struct Metric`:

```rust
/// The subject a metric belongs to — the Domesday's chapter axis, and the
/// taxonomy the Book's Science part will inherit. Deliberately a subject, not
/// a build depth: `rung` puts 105 of 194 metrics in one bucket.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Domain {
    /// Stars, orbits, moons, the sky.
    Astronomy,
    /// Plates, elevation, landforms.
    Terrain,
    /// Temperature, precipitation, biomes.
    Climate,
    /// Rivers, lakes, aquifers, coasts.
    Hydrology,
    /// Species, ecology, life history.
    Biology,
    /// Villages, placement, population.
    Settlement,
    /// Social structure, disposition, conflict.
    Society,
    /// Pantheons, cults, belief.
    Religion,
    /// Phonology, lexicon, grammar.
    Language,
    /// Naming schemes and their products.
    Naming,
    /// Deep history, vestiges, what is forgotten.
    History,
}

impl Domain {
    /// The lowercase token used in `schema.json` and in page filenames.
    /// type-audit: bare-ok(identifier-text)
    pub fn as_str(&self) -> &'static str {
        match self {
            Domain::Astronomy => "astronomy",
            Domain::Terrain => "terrain",
            Domain::Climate => "climate",
            Domain::Hydrology => "hydrology",
            Domain::Biology => "biology",
            Domain::Settlement => "settlement",
            Domain::Society => "society",
            Domain::Religion => "religion",
            Domain::Language => "language",
            Domain::Naming => "naming",
            Domain::History => "history",
        }
    }

    /// Every domain, in rendering order.
    pub fn all() -> &'static [Domain] {
        &[
            Domain::Astronomy, Domain::Terrain, Domain::Climate,
            Domain::Hydrology, Domain::Biology, Domain::Settlement,
            Domain::Society, Domain::Religion, Domain::Language,
            Domain::Naming, Domain::History,
        ]
    }
}

/// Whether a metric is expected to vary. Detectors D1/D2 fire only on
/// `Descriptor`; D7 fires on an `Invariant` that does NOT hold. Without this
/// split D1 fires on 40 of 57 categorical metrics, because 33 of them are
/// deliberate invariants (spec §4.1a).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Role {
    /// A measured property expected to vary across worlds.
    Descriptor,
    /// A property asserted to hold on every world.
    Invariant,
}

impl Role {
    /// The lowercase token used in `schema.json`.
    /// type-audit: bare-ok(identifier-text)
    pub fn as_str(&self) -> &'static str {
        match self {
            Role::Descriptor => "descriptor",
            Role::Invariant => "invariant",
        }
    }
}
```

Then add to `pub struct Metric`:

```rust
    /// The subject this metric belongs to (the Domesday's chapter axis).
    pub domain: Domain,
    /// Whether this metric is expected to vary across worlds.
    pub role: Role,
```

- [ ] **Step 4: Annotate all 191 literals**

`cargo check -p hornvale-lab` lists every unannotated literal. Work the list to zero. Use the guidance above. Example:

```rust
        Metric {
            name: "star-class",
            doc: "Spectral class of the host star",
            summary: SummaryKind::Categorical,
            domain: Domain::Astronomy,
            role: Role::Descriptor,
            extract: /* unchanged */
        },
```

- [ ] **Step 5: Emit both into the schema**

In `windows/lab/src/schema.rs`, in the per-metric `json!` block that already sets `"doc"` and `"rung"`, add:

```rust
            "domain": metric.domain.as_str(),
            "role": metric.role.as_str(),
```

- [ ] **Step 6: Run to verify it passes**

Run: `cargo test -p hornvale-lab --lib schema`
Expected: PASS.

- [ ] **Step 7: Prove the compiler is the enforcement (S1)**

Delete `domain: Domain::Astronomy,` from the `star-class` literal, run `cargo check -p hornvale-lab`, and confirm it fails with `missing field \`domain\``. Paste the error. Restore the line and re-run to green. **Assert the field is present before deleting it** — a no-op deletion proves nothing.

- [ ] **Step 8: Regenerate the schema artifact and commit**

```bash
bash scripts/regenerate-artifacts.sh
git add windows/lab/src/metrics.rs windows/lab/src/schema.rs book/src/laboratory/generated/
cargo fmt
git commit -m "feat(lab): every metric declares its domain and role

The chapter axis for the Domesday, and the taxonomy the Book's Science
part will inherit. Both fields are required, so the compiler enumerates
the 191 metrics that need them.

role exists because 33 of 57 categorical metrics are >=95% single-valued
BY DESIGN (phonotactic-validity, monophyly, lexicon-regular are true on
all 1000 worlds). Without the split, the degeneracy detector fires 40
times and the survey's weakness section is unreadable.

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 2: Read the committed census

**Files:**
- Create: `windows/lab/src/domesday/mod.rs`, `windows/lab/src/domesday/census.rs`
- Modify: `windows/lab/src/lib.rs` (add `pub mod domesday;`)

**Interfaces:**
- Produces: `pub struct Column { pub name: String, pub kind: String, pub doc: String, pub domain: String, pub role: String }`; `pub struct Census { pub columns: Vec<Column>, pub rows: Vec<BTreeMap<String, String>> }`; `pub fn load(dir: &Path) -> Result<Census, String>`; `Census::values(&self, metric: &str) -> Vec<&str>` (excluding empty/absent); `Census::absent_count(&self, metric: &str) -> usize`

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    fn committed() -> Census {
        load(&repo_root().join("book/src/laboratory/generated/the-census"))
            .expect("the committed census loads")
    }

    #[test]
    fn loads_the_committed_census() {
        let c = committed();
        assert_eq!(c.rows.len(), 1000, "the census is 1000 worlds");
        assert!(c.columns.len() >= 194, "194 columns at time of writing");
    }

    #[test]
    fn every_column_carries_a_domain_and_role() {
        for col in &committed().columns {
            if col.name == "seed" || col.name == "pin_set" || col.name == "refusal" {
                continue; // structural columns, not metrics
            }
            assert!(!col.domain.is_empty(), "{} has no domain", col.name);
            assert!(!col.role.is_empty(), "{} has no role", col.name);
        }
    }

    #[test]
    fn absent_values_are_counted_not_skipped() {
        let c = committed();
        let name = &c.columns.iter().find(|c| c.kind == "numeric").expect("a numeric column").name.clone();
        assert_eq!(
            c.values(name).len() + c.absent_count(name),
            1000,
            "present + absent must account for every world"
        );
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-lab --lib domesday::census`
Expected: FAIL — `load` not defined.

- [ ] **Step 3: Implement**

`windows/lab/src/domesday/mod.rs`:

```rust
//! The Domesday — a generated survey of the thousand census worlds.
//!
//! Reads the COMMITTED census (`rows.csv` + `schema.json`) and never re-runs
//! it: decision 0032 established that the gate loads the committed fixture,
//! and 0110 that the census is the suite's shared world-building pass.

pub mod census;
```

`windows/lab/src/domesday/census.rs`:

```rust
//! Loading the committed census table.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// The repository root, resolved from this crate's manifest directory.
pub fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .expect("windows/lab sits two levels below the repo root")
        .to_path_buf()
}

/// One column of the census, as `schema.json` describes it.
pub struct Column {
    /// Metric name (the CSV header).
    pub name: String,
    /// `numeric` | `categorical` | `flag` | `integer`.
    pub kind: String,
    /// Human-readable description.
    pub doc: String,
    /// Subject the metric belongs to.
    pub domain: String,
    /// `descriptor` | `invariant`.
    pub role: String,
}

/// The committed census table.
pub struct Census {
    /// Column descriptors, in schema order.
    pub columns: Vec<Column>,
    /// One map per world, keyed by column name.
    pub rows: Vec<BTreeMap<String, String>>,
}

impl Census {
    /// Present (non-empty) values for a metric, in row order.
    pub fn values(&self, metric: &str) -> Vec<&str> {
        self.rows
            .iter()
            .filter_map(|r| r.get(metric).map(String::as_str))
            .filter(|v| !v.is_empty())
            .collect()
    }

    /// How many worlds have no value for this metric.
    pub fn absent_count(&self, metric: &str) -> usize {
        self.rows.len() - self.values(metric).len()
    }
}

/// Load `rows.csv` and `schema.json` from a study directory.
pub fn load(dir: &Path) -> Result<Census, String> {
    let schema_text = std::fs::read_to_string(dir.join("schema.json"))
        .map_err(|e| format!("schema.json: {e}"))?;
    let schema: serde_json::Value =
        serde_json::from_str(&schema_text).map_err(|e| format!("schema.json parse: {e}"))?;
    let columns = schema["columns"]
        .as_array()
        .ok_or("schema.json has no columns array")?
        .iter()
        .map(|c| Column {
            name: c["name"].as_str().unwrap_or_default().to_string(),
            kind: c["kind"].as_str().unwrap_or_default().to_string(),
            doc: c["doc"].as_str().unwrap_or_default().to_string(),
            domain: c["domain"].as_str().unwrap_or_default().to_string(),
            role: c["role"].as_str().unwrap_or_default().to_string(),
        })
        .collect();

    let csv = std::fs::read_to_string(dir.join("rows.csv")).map_err(|e| format!("rows.csv: {e}"))?;
    let mut lines = csv.lines();
    let header: Vec<&str> = lines.next().ok_or("rows.csv is empty")?.split(',').collect();
    let rows = lines
        .filter(|l| !l.trim().is_empty())
        .map(|line| {
            header
                .iter()
                .zip(line.split(','))
                .map(|(h, v)| ((*h).to_string(), v.to_string()))
                .collect()
        })
        .collect();

    Ok(Census { columns, rows })
}
```

Add `pub mod domesday;` to `windows/lab/src/lib.rs`.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-lab --lib domesday::census`
Expected: PASS, 3 tests.

- [ ] **Step 5: Prove it does not re-run the census**

```rust
    #[test]
    fn loading_never_builds_a_world() {
        let src = include_str!("census.rs");
        for forbidden in ["build_world", "BuildDepth", "build_to", "RunResult"] {
            assert!(
                !src.contains(forbidden),
                "the census reader must not construct worlds; found {forbidden}"
            );
        }
    }
```

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add windows/lab/src/domesday windows/lab/src/lib.rs
git commit -m "feat(lab): read the committed census, never rebuild it

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 3: Summary statistics

**Files:**
- Create: `windows/lab/src/domesday/stats.rs`
- Modify: `windows/lab/src/domesday/mod.rs` (add `pub mod stats;`)

**Interfaces:**
- Consumes: `census::Census`
- Produces: `pub struct NumericStats { pub n: usize, pub absent: usize, pub min: f64, pub p25: f64, pub median: f64, pub p75: f64, pub max: f64, pub mean: f64 }`; `pub fn numeric(c: &Census, metric: &str) -> Option<NumericStats>`; `pub fn categorical(c: &Census, metric: &str) -> Vec<(String, usize)>`

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::domesday::census::{Census, Column};
    use std::collections::BTreeMap;

    fn table(kind: &str, name: &str, vals: &[&str]) -> Census {
        Census {
            columns: vec![Column {
                name: name.into(), kind: kind.into(), doc: String::new(),
                domain: "climate".into(), role: "descriptor".into(),
            }],
            rows: vals
                .iter()
                .map(|v| BTreeMap::from([(name.to_string(), (*v).to_string())]))
                .collect(),
        }
    }

    #[test]
    fn numeric_quartiles_on_a_known_set() {
        let c = table("numeric", "m", &["1", "2", "3", "4", "5"]);
        let s = numeric(&c, "m").expect("stats");
        assert_eq!(s.n, 5);
        assert_eq!(s.min, 1.0);
        assert_eq!(s.median, 3.0);
        assert_eq!(s.max, 5.0);
        assert_eq!(s.mean, 3.0);
    }

    #[test]
    fn absent_values_are_reported_not_dropped() {
        let c = table("numeric", "m", &["1", "", "3"]);
        let s = numeric(&c, "m").expect("stats");
        assert_eq!(s.n, 2, "two present");
        assert_eq!(s.absent, 1, "one absent, and it is REPORTED");
    }

    #[test]
    fn categorical_orders_by_count_then_lexicographically() {
        let c = table("categorical", "m", &["b", "a", "a", "c", "c"]);
        let got = categorical(&c, "m");
        assert_eq!(
            got,
            vec![("a".to_string(), 2), ("c".to_string(), 2), ("b".to_string(), 1)],
            "count descending, ties lexicographic — deterministic"
        );
    }

    #[test]
    fn a_metric_with_no_present_values_yields_none() {
        let c = table("numeric", "m", &["", ""]);
        assert!(numeric(&c, "m").is_none());
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-lab --lib domesday::stats`
Expected: FAIL — `numeric` not defined.

- [ ] **Step 3: Implement**

```rust
//! Per-metric summary statistics over the committed census.

use crate::domesday::census::Census;
use std::collections::BTreeMap;

/// Summary statistics for one numeric metric.
pub struct NumericStats {
    /// Worlds with a present value.
    pub n: usize,
    /// Worlds with no value for this metric.
    pub absent: usize,
    /// Smallest present value.
    pub min: f64,
    /// 25th percentile (nearest-rank).
    pub p25: f64,
    /// 50th percentile (nearest-rank).
    pub median: f64,
    /// 75th percentile (nearest-rank).
    pub p75: f64,
    /// Largest present value.
    pub max: f64,
    /// Arithmetic mean of present values.
    pub mean: f64,
}

/// Nearest-rank percentile over a sorted slice. Deterministic and index-based;
/// no interpolation, so the result is always a value that actually occurs.
fn percentile(sorted: &[f64], q: f64) -> f64 {
    let idx = ((q * sorted.len() as f64).ceil() as usize).saturating_sub(1);
    sorted[idx.min(sorted.len() - 1)]
}

/// Statistics for a numeric metric, or `None` if no world has a value.
pub fn numeric(c: &Census, metric: &str) -> Option<NumericStats> {
    let mut vals: Vec<f64> = c
        .values(metric)
        .iter()
        .filter_map(|v| v.parse::<f64>().ok())
        .filter(|v| v.is_finite())
        .collect();
    if vals.is_empty() {
        return None;
    }
    vals.sort_by(|a, b| a.total_cmp(b));
    let n = vals.len();
    Some(NumericStats {
        n,
        absent: c.absent_count(metric),
        min: vals[0],
        p25: percentile(&vals, 0.25),
        median: percentile(&vals, 0.50),
        p75: percentile(&vals, 0.75),
        max: vals[n - 1],
        mean: vals.iter().sum::<f64>() / n as f64,
    })
}

/// Value counts for a categorical or flag metric, count descending with
/// lexicographic tie-breaks.
pub fn categorical(c: &Census, metric: &str) -> Vec<(String, usize)> {
    let mut counts: BTreeMap<String, usize> = BTreeMap::new();
    for v in c.values(metric) {
        *counts.entry(v.to_string()).or_insert(0) += 1;
    }
    let mut out: Vec<(String, usize)> = counts.into_iter().collect();
    out.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
    out
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-lab --lib domesday::stats`
Expected: PASS, 4 tests.

- [ ] **Step 5: Verify against a known live value**

```rust
    #[test]
    fn matches_the_known_live_median() {
        let c = crate::domesday::census::load(
            &crate::domesday::census::repo_root()
                .join("book/src/laboratory/generated/the-census"),
        )
        .expect("census");
        let s = numeric(&c, "mean-land-temperature-c").expect("stats");
        // Measured by hand at 6e469717 before this code existed (spec §4.4a).
        assert!((s.median - (-11.90)).abs() < 0.01, "median was {}", s.median);
        assert!((s.min - (-47.15)).abs() < 0.01, "min was {}", s.min);
        assert!((s.max - 23.14).abs() < 0.01, "max was {}", s.max);
    }
```

Run it. If the numbers disagree, **report the discrepancy — do not adjust the assertion.** The hand measurement is the oracle.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add windows/lab/src/domesday
git commit -m "feat(lab): per-metric census statistics, pinned to a hand oracle

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 4: Comparators and expectations

**Files:**
- Create: `studies/comparators.json`, `studies/expectations.json`, `windows/lab/src/domesday/comparators.rs`
- Modify: `windows/lab/src/domesday/mod.rs`

**Interfaces:**
- Produces: `pub struct Comparator { pub name: String, pub values: BTreeMap<String, f64>, pub band: BTreeMap<String, f64> }`; `pub fn load_comparators(path: &Path) -> Result<Vec<Comparator>, String>`; `pub struct Expectation { pub metric: String, pub tracks: String, pub why: String, pub declared: String }`; `pub fn load_expectations(path: &Path) -> Result<Vec<Expectation>, String>`

**Real worlds only for v1** (spec §8, ratified at G3). An invented number rendered beside a measured one is the confusion this programme exists to remove.

- [ ] **Step 1: Write the data files**

`studies/comparators.json`:

```json
{
  "worlds": [
    {
      "name": "Earth",
      "kind": "real",
      "values": { "mean-land-temperature-c": 14.0 },
      "band": { "mean-land-temperature-c": 10.0 }
    }
  ]
}
```

`band` is the declared tolerance: D6 fires when `|median − value| > band`.

`studies/expectations.json`:

```json
{
  "expect": [
    {
      "metric": "mean-land-temperature-c",
      "tracks": "year-std-days",
      "why": "orbital period is the only available proxy for orbital distance, and thus for insolation, which is the dominant term in a radiative balance: a longer year means a wider orbit and less energy received. The census measures no insolation directly (see the note below), so this is a proxy and a weak one — which is itself part of the finding.",
      "declared": "dominant"
    }
  ]
}
```

**Two disclosures about this file, because both affect whether you can trust what it produces.**

**1. The census cannot measure insolation.** Its 24 astronomy columns include `star-class`, `obliquity-degrees`, `year-std-days`, `total-tide` — and no luminosity, no orbital distance, no habitable-zone position. The primary energy input to a planetary climate is not a column. `year-std-days` is used as a proxy by Kepler's third law. **Record this as a follow-on finding in Task 8**; it may matter more than any correlation, because SKY-19 concludes the climate is "near-uninfluenced by its own astronomy" using a dataset that never measured the influence.

**2. D5 carries no threshold of ours — SUPERSEDED BY IDEONOMY (2026-08-08).** An earlier draft froze `min_abs_r = 0.50` while already knowing SKY-19's measured r = −0.245, i.e. the value was measured before the judgement was frozen. No justification repairs that phase-order violation. The fix is a different *source*, not a better number: the expectation declares a strength **class** (`dominant`), and the detector reports the observed class against conventional effect-size bands (`|r| ≥ 0.7` dominant, `0.5–0.7` strong, `0.3–0.5` moderate, `0.1–0.3` weak, `< 0.1` none). Observed |r| = 0.245 is **weak**, so D5 reports *"declared dominant, measured weak"*. Nothing we knew about the data can shape either the claim or the bands, and the tension with D1 dissolves — D1 is a global definition of degenerate, D5 now has no threshold at all.

- [ ] **Step 2: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::domesday::census::repo_root;

    #[test]
    fn comparators_load() {
        let cs = load_comparators(&repo_root().join("studies/comparators.json")).expect("load");
        let earth = cs.iter().find(|c| c.name == "Earth").expect("Earth present");
        assert!((earth.values["mean-land-temperature-c"] - 14.0).abs() < 1e-9);
    }

    #[test]
    fn a_comparator_naming_an_unknown_metric_is_rejected() {
        let c = crate::domesday::census::load(
            &repo_root().join("book/src/laboratory/generated/the-census"),
        )
        .expect("census");
        let known: Vec<&str> = c.columns.iter().map(|x| x.name.as_str()).collect();
        for cmp in load_comparators(&repo_root().join("studies/comparators.json")).expect("load") {
            for m in cmp.values.keys() {
                assert!(known.contains(&m.as_str()), "{} names unknown metric {m}", cmp.name);
            }
        }
    }

    #[test]
    fn expectations_name_known_metrics() {
        let c = crate::domesday::census::load(
            &repo_root().join("book/src/laboratory/generated/the-census"),
        )
        .expect("census");
        let known: Vec<&str> = c.columns.iter().map(|x| x.name.as_str()).collect();
        for e in load_expectations(&repo_root().join("studies/expectations.json")).expect("load") {
            assert!(known.contains(&e.metric.as_str()), "unknown metric {}", e.metric);
            assert!(known.contains(&e.tracks.as_str()), "unknown driver {}", e.tracks);
        }
    }
}
```

- [ ] **Step 3: Run to verify it fails, then implement the two loaders**

Both are straightforward `serde_json` reads into the structs above, returning `Result<_, String>` with the file name in the error. Follow `census::load`'s error style exactly.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-lab --lib domesday::comparators`
Expected: PASS, 3 tests. The expectation names `year-std-days`, which IS a census column — verified before this plan was written, after an earlier draft named `insolation-rel`, which is not. If any name in either data file fails this test, **report it and pick the nearest existing column rather than deleting the entry** — a deleted expectation is a silently un-asked question.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add studies/comparators.json studies/expectations.json windows/lab/src/domesday
git commit -m "feat(lab): comparators and expectations as data, validated against the schema

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 5: The seven detectors

**Files:**
- Create: `windows/lab/src/domesday/detect.rs`
- Modify: `windows/lab/src/domesday/mod.rs`

**Interfaces:**
- Consumes: `census::Census`, `stats::{numeric, categorical}`, `comparators::{Comparator, Expectation}`
- Produces: `pub struct Finding { pub detector: &'static str, pub metric: String, pub detail: String }`; `pub fn detect(c: &Census, cmps: &[Comparator], exps: &[Expectation]) -> Vec<Finding>`

**Thresholds are preregistered and frozen** (spec §4.4). D1 ≥ 0.80; D3 IQR < 5 % of range; D6 per-comparator `band`. **D5 has no threshold of ours** — it maps observed |r| to a conventional band (`≥0.7` dominant, `0.5–0.7` strong, `0.3–0.5` moderate, `0.1–0.3` weak, `<0.1` none) and fires when that differs from the expectation's `declared` class.

- [ ] **Step 1: Write the failing tests — each detector must fire AND not fire**

```rust
    #[test]
    fn d1_fires_at_the_threshold_and_not_below_it() {
        // 80 of 100 share a value -> fires. 79 -> does not.
        let at = table_cat("descriptor", &vec!["a"; 80].into_iter().chain(vec!["b"; 20]).collect::<Vec<_>>());
        let below = table_cat("descriptor", &vec!["a"; 79].into_iter().chain(vec!["b"; 21]).collect::<Vec<_>>());
        assert!(fires(&detect_only_d1(&at)), "80% must fire");
        assert!(!fires(&detect_only_d1(&below)), "79% must not");
    }

    #[test]
    fn d1_never_fires_on_an_invariant() {
        let inv = table_cat("invariant", &vec!["true"; 100]);
        assert!(!fires(&detect_only_d1(&inv)), "invariants are not degeneracies");
    }

    #[test]
    fn d7_fires_on_an_invariant_that_varies() {
        let broken = table_cat("invariant", &vec!["true"; 99].into_iter().chain(vec!["false"; 1]).collect::<Vec<_>>());
        let f = detect_only_d7(&broken);
        assert!(fires(&f), "a broken invariant is worth more than a degeneracy");
    }
```

(Write the small `table_cat` / `fires` / `detect_only_*` helpers alongside; `table_cat(role, vals)` builds a one-column `Census` with the given role, mirroring Task 3's `table` helper.)

- [ ] **Step 2: Run to verify they fail, then implement all seven**

D1 degenerate (descriptor categorical/flag, top share ≥ 0.80). D2 frozen (descriptor numeric, `min == max`). D3 narrow (`(p75-p25) < 0.05*(max-min)`, and `max > min`). D4 at-rail (`median == min || median == max`). D5 mis-declared strength (Pearson r over paired present values; map |r| to a band; fire when the observed band differs from `declared`). D6 off-comparator (`|median - value| > band`). D7 broken invariant (role `invariant` with more than one distinct value). D8 unmeasured domain (a crate under `domains/` with no metric in any domain — `alchemy` and `paleoclimate` are the expected hits; they are WORLD gaps, so they render, per spec §4.6a).

- [ ] **Step 3: Run to verify they pass**

Run: `cargo test -p hornvale-lab --lib domesday::detect`

- [ ] **Step 4: Assert the live acceptance cases (S2, S2b, S2c)**

```rust
    #[test]
    fn the_live_census_reproduces_the_preregistered_findings() {
        let c = census(); let cmps = comparators(); let exps = expectations();
        let f = detect(&c, &cmps, &exps);
        let hit = |d: &str, m: &str| f.iter().any(|x| x.detector == d && x.metric == m);

        // S2 — SKY-19's climate defect, by the routes the spec names.
        assert!(hit("D6", "mean-land-temperature-c"), "median -11.90 vs Earth 14.0");
        assert!(hit("D5", "mean-land-temperature-c"), "r = -0.245 vs year-std-days, under the 0.50 dominant-driver floor");
        assert!(!hit("D1", "dominant-land-biome"), "65.1% is a skew, NOT a degeneracy — must not fire");

        // S2b — found while testing, not sought.
        assert!(hit("D2", "reproductive-tempo-goblin"), "min=median=max=0.42");

        // S2c — the falsification clause as a number.
        let d1 = f.iter().filter(|x| x.detector == "D1").count();
        assert!(d1 <= 10, "D1 fired {d1} times; unsplit it would fire 40 and be unreadable");
    }
```

**If any assertion fails, report it — do not weaken the assertion or move a threshold.** A failure here is the campaign's headline finding, exactly as the falsification clause anticipates.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/lab/src/domesday
git commit -m "feat(lab): seven preregistered weakness detectors

D1's 80% threshold is frozen: 65.1% ice-dominance is a skew, not a
degeneracy, and D6 owns that finding. A test asserts D1 does NOT fire on
it so a later reader does not 'fix' correct behaviour.

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 6: Render the survey and wire it into the Book

**Files:**
- Create: `windows/lab/src/domesday/render.rs`, `book/src/domesday/` (generated)
- Modify: `windows/lab/src/domesday/mod.rs`, `book/src/SUMMARY.md`

**Interfaces:**
- Produces: `pub fn render_domain(c: &Census, domain: &str, findings: &[Finding]) -> String`; `pub fn render_index(c: &Census, findings: &[Finding]) -> String`

- [ ] **Step 1: Write the failing test**

```rust
    #[test]
    fn a_domain_page_carries_the_generated_header_and_only_computed_numbers() {
        let page = render_domain(&census(), "climate", &[]);
        assert!(page.starts_with("<!-- GENERATED FILE — do not edit."), "header required");
        assert!(page.contains("mean-land-temperature-c"), "climate metrics appear");
        assert!(page.contains("-11.9"), "the median is read, not restated");
    }

    #[test]
    fn every_domain_renders_and_none_is_empty() {
        let c = census();
        for d in ["astronomy", "terrain", "climate", "hydrology", "biology",
                  "settlement", "society", "religion", "language", "naming", "history"] {
            let page = render_domain(&c, d, &[]);
            assert!(page.len() > 200, "{d} rendered nothing at all");
            // A domain with NO metrics must still render, announcing the gap.
            // Hydrology is exactly this today: rivers, lakes, aquifers and
            // coasts are unmeasured. An absence that announces itself is a
            // finding; a missing chapter is silence. Do not "fix" the gap by
            // assigning it metrics — render it (campaign principle, Nathan).
            if c.columns.iter().all(|col| col.domain != d) {
                assert!(
                    page.contains("no metrics"),
                    "{d} has no metrics and must SAY SO on its page"
                );
            }
        }
    }
```

The second test is deliberately sharp: a domain with no metrics means Task 1's annotation left a hole, and an empty chapter in a survey is worse than no chapter.

- [ ] **Step 2: Run to verify it fails, then implement**

Per domain: the `<!-- GENERATED FILE — do not edit. Regenerate with `make rebaseline`. -->` header, one authored framing line, then per metric a table of statistics appropriate to its kind, then that domain's findings. All floats through `hornvale_kernel::quantize`.

- [ ] **Step 3: Generate the pages and wire SUMMARY.md**

**The subcommand does not exist yet — add it.** `hornvale lab` currently dispatches `run`, `list-metrics`, `diff`, and `backfill-schema`. Add a `domesday` arm in `cli/src/main.rs` alongside `backfill-schema`, following that arm's exact shape (argument parsing is std-only; there is no clap). It takes no arguments: it reads the committed census from its fixed path and writes `book/src/domesday/`.

```bash
mkdir -p book/src/domesday
cargo run -p hornvale -- lab domesday
```

Add a `# The Domesday` part to `book/src/SUMMARY.md` with the index and eleven domain pages.

- [ ] **Step 4: Run to verify it passes, then commit — staging the new directory**

```bash
cargo fmt
git add windows/lab/src/domesday book/src/domesday book/src/SUMMARY.md
git commit -m "feat(lab): render the Domesday, one page per domain

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

**`git add book/src/domesday` is load-bearing** — `git diff --exit-code` is silently vacuous against an untracked path, and Task 7's drift proof depends on it.

---

### Task 7: Regeneration, drift check, and the mutation proof

**Files:**
- Modify: `scripts/regenerate-artifacts.sh`, `CLAUDE.md`, `.github/workflows/ci.yml`

- [ ] **Step 1: Add the render to the regeneration script**, alongside the existing lab study lines.

- [ ] **Step 2: Add `book/src/domesday/` to every drift-check path list** — `CLAUDE.md`'s documented command and `ci.yml`'s. Both, or CI checks something the docs don't.

- [ ] **Step 3: Prove the drift check goes GREEN when clean**

```bash
bash scripts/regenerate-artifacts.sh && git diff --exit-code book/src/domesday/; echo "rc=$?"
```
Expected: `rc=0`.

- [ ] **Step 4: Prove it goes RED on a mutated census value (S3)**

```bash
CSV=book/src/laboratory/generated/the-census/rows.csv
grep -q "mean-land-temperature-c" "$CSV" || { echo "TARGET NOT FOUND — mutation would be a no-op"; exit 1; }
cp "$CSV" /tmp/rows.bak
python3 - <<'PY'
import csv,io
p='book/src/laboratory/generated/the-census/rows.csv'
rows=list(csv.reader(open(p))); h=rows[0]; i=h.index('mean-land-temperature-c')
rows[1][i]='999.0'
w=csv.writer(open(p,'w',newline='')); w.writerows(rows)
PY
bash scripts/regenerate-artifacts.sh
if git diff --exit-code book/src/domesday/ > /dev/null; then echo "S3 FAILED: drift check did not go red"; exit 1; else echo "S3 PASSED"; fi
cp /tmp/rows.bak "$CSV"
bash scripts/regenerate-artifacts.sh
git diff --exit-code book/src/domesday/ && echo "restored clean"
```

- [ ] **Step 5: Prove no number is restated rather than read (S6)**

The mutation above changed one world's temperature. Confirm the rendered climate page's `mean` changed in the S3 diff. If the page is byte-identical except for a table cell you did not expect, the number was hard-coded somewhere.

- [ ] **Step 6: Commit**

---

### Task 8: Close

- [ ] **Step 1:** `make gate` (foreground, `timeout: 3600000`). Report `rc`.
- [ ] **Step 2:** `make rebaseline && git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/ docs/digest/ book/src/domesday/` → 0.
- [ ] **Step 3:** Evaluate the falsification clause. Read the rendered survey end to end. Is the weakness section readable, or drowned? Record the verdict and the D1 count either way.
- [ ] **Step 4:** Chronicle (`book/src/chronicle/the-domesday.md` + SUMMARY), retrospective (`docs/retrospectives/the-domesday.md`), registry flips.
- [ ] **Step 5:** Record every weakness the survey found as follow-on candidates, **RANKED, not merely listed** — by whether the metric is load-bearing downstream, then by severity. A flat list of 60 findings is the falsification clause arriving by the back door. Include the missing-insolation gap (spec §4.4a note) and every unlooked-for hit — **including the ones nobody was looking for.** That list is the campaign's product, not a byproduct.

---

## Self-Review

**Spec coverage.** §4.1 domain → Task 1. §4.1a role → Task 1. §4.2 statistics → Task 3. §4.3 comparators → Task 4. §4.4 detectors → Task 5. §4.4a diagnostic → recorded in the spec, no task needed. §4.5 placement + never-rerun → Tasks 2, 6, 7. §4.6 prose budget → Task 6 + S6 in Task 7. §6 S1→Task 1 Step 7, S2/S2b/S2c→Task 5 Step 4, S3→Task 7 Step 4, S4→Task 4, S5→Task 8, S6→Task 7 Step 5.

**Placeholder scan.** No TBD/TODO. Task 4 Step 3 and Task 5 Step 2 describe implementations in prose rather than full code — deliberate, because both are mechanical given the struct definitions and test cases above them, and the tests fully specify behaviour. Flagged rather than hidden.

**Type consistency.** `Census`/`Column` defined in Task 2, used unchanged in 3–6. `NumericStats` fields defined in Task 3, referenced in 5–6. `Finding { detector, metric, detail }` defined in Task 5, consumed in 6. `repo_root()` defined once in Task 2 and reused.

**Known risk:** Task 1 is large — 191 annotations. It cannot be split, because the crate does not compile until every literal is done. Expect the implementer to work `cargo check`'s error list to zero rather than reading the file top to bottom.
