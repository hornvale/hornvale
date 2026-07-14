# Census as Data — Implementation Plan

> **STATUS: executed, 2026-07-13.** All 10 tasks complete. Task 4's
> execution reorder (owner directive, see the note inline at that task)
> shifted the AWS regeneration, the byte-identity equivalence check, and
> the golden-pin re-pins to merge prep, executed there alongside this
> plan's close; branch calibration-family tests were expected-red for the
> intervening tasks by design, not by drift. See
> `docs/retrospectives/census-as-data.md` for process lessons and
> `docs/superpowers/specs/2026-07-13-census-as-data-design.md` for the
> design this plan implements (now marked implemented).

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking. Every subagent dispatch MUST prepend `.claude/skills/dispatching-hornvale-subagents/dispatch-preamble.md` verbatim (worktree cd + branch check first).

**Goal:** One canonical, self-describing, versioned census (`the-census`: all metrics × 1000 seeds) plus a DuckDB analysis harness (`tools/census/`), per the spec `docs/superpowers/specs/2026-07-13-census-as-data-design.md`.

**Architecture:** The lab window gains a `schema.json` emitter (co-generated with `rows.csv` in `publish()`); `census-lands-drift` is promoted/renamed to `the-census` at 1000 seeds and `branches-family` freezes; a new out-of-workspace tool `tools/census/` materializes a throwaway DuckDB from the committed artifacts with mount-time validation, a unified long view, git-history longitudinal extraction, and canned queries.

**Tech Stack:** Rust (workspace: serde + serde_json ONLY), bash + python3 + duckdb (harness only, outside the workspace).

**Branch/worktree:** branch `census-as-data`, worktree `~/.config/superpowers/worktrees/hornvale/census-as-data` (create via superpowers:using-git-worktrees). Run `make preflight` and absorb main at every task boundary marked **[STAGE]**.

## Global Constraints

- Dependencies: `serde` + `serde_json` only, workspace-wide. No new crates. The harness's `duckdb`/`python3` live OUTSIDE the workspace (`tools/census/`), like `tools/type-audit`.
- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only. No wall-clock time in any committed artifact.
- Every serialized float passes `hornvale_kernel::quantize` (8 significant digits) at the emit boundary.
- Every crate sets `#![warn(missing_docs)]`; every public item gets a one-line doc comment. Every primitive at a `pub` boundary carries a `type-audit:` verdict tag.
- Golden pins re-pin **in the same commit** as the fixture that drifted them — never deferred.
- Commit gate: `make gate` (fmt + clippy + nextest + doctests). Iterate cost-ordered: fmt/clippy first, then `cargo test -p hornvale-lab`, full gate last. Run once, capture output (`2>&1 | tee /tmp/hv-test.txt`), never re-run to grep a second line.
- `cargo fmt` as the final step before every commit.
- **Census regeneration is AWS-ONLY** (owner directive 2026-07-13, mechanized: `regenerate-artifacts.sh` skips censuses unless `HV_CENSUS=1`, which only the AWS path sets): the CONTROLLER runs `make regen-remote` / `scripts/aws-gate/regen-git.sh` — never an implementer subagent, never locally, unless Nathan explicitly says so. Local runs are fast-tier only (8-seed probe, scoped tests, `make gate`).

---

### Task 1: `schema.rs` — FNV-1a 64 + `render_schema`

**Files:**
- Create: `windows/lab/src/schema.rs`
- Modify: `windows/lab/src/lib.rs` (add `pub mod schema;` after `pub mod runner;` region and `pub use schema::{fnv1a64, render_schema};` after the `runner` re-export)
- Test: inline `#[cfg(test)]` in `windows/lab/src/schema.rs`

**Interfaces:**
- Consumes: `crate::{RunResult, SummaryKind, Metric}` (existing), `hornvale_worldgen::BuildDepth` (existing), `hornvale_kernel::quantize` (existing), `crate::runner::render_csv` (existing, `pub(crate)`)
- Produces: `pub fn fnv1a64(bytes: &[u8]) -> u64` and `pub fn render_schema(result: &RunResult, csv: &str, backfilled: bool) -> String` — Tasks 2, 3 call these exact signatures.

- [ ] **Step 1: Write the failing tests**

Create `windows/lab/src/schema.rs` with the module doc, a stub, and the tests (stub first so the test file compiles and FAILS on assertions, not on missing symbols):

```rust
//! The self-describing census manifest: `schema.json` (census-as-data spec
//! §2). Emitted next to `rows.csv` from the same `RunResult`, so the
//! manifest can never disagree with the CSV it sits beside. Deterministic:
//! serde_json's default map is a BTreeMap (alphabetical keys); arrays keep
//! insertion order, so `columns` preserves the CSV's exact header order.

use crate::{RunResult, SummaryKind};
use hornvale_kernel::quantize;
use hornvale_worldgen::BuildDepth;

/// FNV-1a 64-bit hash of `bytes` — the manifest ↔ `rows.csv` integrity
/// binding (spec §2): mismatch detection, not security.
/// type-audit: bare-ok(artifact: bytes), bare-ok(hash: return)
pub fn fnv1a64(bytes: &[u8]) -> u64 {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    hash
}

/// Render `schema.json` for a run result whose committed CSV bytes are
/// `csv`. `backfilled` marks a manifest generated after the fact for a
/// frozen study (spec §2, "Backfill for the frozen tier").
/// type-audit: bare-ok(artifact: csv), bare-ok(flag: backfilled), bare-ok(artifact: return)
pub fn render_schema(result: &RunResult, csv: &str, backfilled: bool) -> String {
    let _ = (result, csv, backfilled);
    todo!("Task 1 Step 3")
}
```

Append the tests:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{MetricSelection, MetricValue, PinSet, Row, Seeds, Study};

    /// A two-seed study over one metric of each kind, mirroring the publish
    /// tests' builder. `star-class` is Categorical, `ocean-fraction` is
    /// Numeric, `tidally-locked` is Flag in the registry.
    fn build_result() -> RunResult {
        let study = Study {
            name: "schema-study".to_string(),
            description: "for schema tests".to_string(),
            seeds: Seeds { from: 0, count: 2 },
            pin_sets: vec![PinSet {
                label: "default".to_string(),
                pins: vec![],
                roster: None,
            }],
            metrics: MetricSelection::Named(vec![
                "star-class".to_string(),
                "ocean-fraction".to_string(),
                "tidally-locked".to_string(),
            ]),
        };
        RunResult {
            study,
            metric_names: vec!["star-class", "ocean-fraction", "tidally-locked"],
            rows: (0..2u64)
                .map(|seed| Row {
                    seed,
                    pin_set: "default".to_string(),
                    values: vec![
                        MetricValue::Text("G".to_string()),
                        MetricValue::Number(0.5),
                        MetricValue::Flag(false),
                    ],
                    refusal: None,
                })
                .collect(),
        }
    }

    #[test]
    fn fnv1a64_matches_the_published_vectors() {
        assert_eq!(fnv1a64(b""), 0xcbf2_9ce4_8422_2325);
        assert_eq!(fnv1a64(b"a"), 0xaf63_dc4c_8601_ec8c);
        assert_eq!(fnv1a64(b"foobar"), 0x85944171f73967e8);
    }

    #[test]
    fn schema_columns_agree_with_the_csv_header_in_order() {
        let result = build_result();
        let csv = crate::runner::render_csv(&result);
        let schema = render_schema(&result, &csv, false);
        let parsed: serde_json::Value = serde_json::from_str(&schema).unwrap();
        let names: Vec<&str> = parsed["columns"]
            .as_array()
            .unwrap()
            .iter()
            .map(|c| c["name"].as_str().unwrap())
            .collect();
        let header = csv.lines().next().unwrap();
        assert_eq!(names.join(","), header);
    }

    #[test]
    fn schema_types_documents_and_binds_the_csv() {
        let result = build_result();
        let csv = crate::runner::render_csv(&result);
        let schema = render_schema(&result, &csv, false);
        let parsed: serde_json::Value = serde_json::from_str(&schema).unwrap();

        assert_eq!(parsed["schema_version"], 1);
        assert_eq!(parsed["study"]["name"], "schema-study");
        assert_eq!(parsed["study"]["seeds"]["count"], 2);
        assert_eq!(
            parsed["conventions"]["float_quantization"],
            "8-significant-digits"
        );
        assert_eq!(parsed["rows"]["count"], 2);
        assert_eq!(
            parsed["rows"]["fnv1a64"],
            format!("0x{:016x}", fnv1a64(csv.as_bytes()))
        );
        // Backfilled flag absent on live manifests.
        assert!(parsed.get("backfilled").is_none());

        let cols = parsed["columns"].as_array().unwrap();
        assert_eq!(cols[0]["name"], "seed");
        assert_eq!(cols[0]["kind"], "integer");
        assert_eq!(cols[1]["name"], "pin_set");
        assert_eq!(cols[1]["kind"], "categorical");
        let star = &cols[2];
        assert_eq!(star["kind"], "categorical");
        assert!(star["doc"].as_str().unwrap().len() > 1);
        assert!(star["rung"].as_str().is_some());
        let ocean = &cols[3];
        assert_eq!(ocean["kind"], "numeric");
        assert!(ocean["buckets"].as_array().unwrap().len() > 1);
        let locked = &cols[4];
        assert_eq!(locked["kind"], "flag");
        assert_eq!(cols.last().unwrap()["name"], "refusal");
        assert_eq!(cols.last().unwrap()["kind"], "categorical");
    }

    #[test]
    fn schema_is_deterministic_and_backfill_flag_appears_when_set() {
        let result = build_result();
        let csv = crate::runner::render_csv(&result);
        assert_eq!(
            render_schema(&result, &csv, false),
            render_schema(&result, &csv, false)
        );
        let back: serde_json::Value =
            serde_json::from_str(&render_schema(&result, &csv, true)).unwrap();
        assert_eq!(back["backfilled"], true);
    }
}
```

Wire the module into `windows/lab/src/lib.rs`:

```rust
pub mod schema;
```

and

```rust
pub use schema::{fnv1a64, render_schema};
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-lab schema 2>&1 | tee /tmp/hv-t1.txt`
Expected: `fnv1a64_matches_the_published_vectors` PASSES (it's fully implemented); the other three FAIL/panic on the `todo!`.

- [ ] **Step 3: Implement `render_schema`**

Replace the `todo!` body:

```rust
pub fn render_schema(result: &RunResult, csv: &str, backfilled: bool) -> String {
    let metrics = result
        .study
        .selected_metrics()
        .expect("the study validated at load time");

    let mut columns: Vec<serde_json::Value> = Vec::new();
    columns.push(serde_json::json!({ "name": "seed", "kind": "integer" }));
    columns.push(serde_json::json!({ "name": "pin_set", "kind": "categorical" }));
    for name in &result.metric_names {
        let metric = metrics
            .iter()
            .find(|m| m.name == *name)
            .expect("metric_names come from the registry");
        let rung = match metric.rung() {
            BuildDepth::Astronomy => "astronomy",
            BuildDepth::Terrain => "terrain",
            BuildDepth::Settlements => "settlements",
            BuildDepth::Full => "full",
        };
        let mut column = serde_json::json!({
            "name": metric.name,
            "doc": metric.doc,
            "rung": rung,
        });
        match &metric.summary {
            SummaryKind::Categorical => {
                column["kind"] = serde_json::json!("categorical");
            }
            SummaryKind::Flag => {
                column["kind"] = serde_json::json!("flag");
            }
            SummaryKind::Numeric { bucket_edges } => {
                column["kind"] = serde_json::json!("numeric");
                let edges: Vec<f64> = bucket_edges.iter().map(|e| quantize(*e)).collect();
                column["buckets"] = serde_json::json!(edges);
            }
        }
        columns.push(column);
    }
    columns.push(serde_json::json!({ "name": "refusal", "kind": "categorical" }));

    let pin_sets: Vec<serde_json::Value> = result
        .study
        .pin_sets
        .iter()
        .map(|ps| {
            serde_json::json!({
                "label": ps.label,
                "pins": ps.pins,
                "roster": ps.roster,
            })
        })
        .collect();

    let mut manifest = serde_json::json!({
        "schema_version": 1,
        "study": {
            "name": result.study.name,
            "description": result.study.description,
            "seeds": { "from": result.study.seeds.from, "count": result.study.seeds.count },
            "pin_sets": pin_sets,
        },
        "conventions": { "float_quantization": "8-significant-digits" },
        "rows": {
            "count": result.rows.len(),
            "fnv1a64": format!("0x{:016x}", fnv1a64(csv.as_bytes())),
        },
        "columns": columns,
    });
    if backfilled {
        manifest["backfilled"] = serde_json::json!(true);
    }

    let mut out = serde_json::to_string_pretty(&manifest).expect("manifest serializes");
    out.push('\n');
    out
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-lab schema 2>&1 | tee /tmp/hv-t1b.txt`
Expected: all 4 PASS.

- [ ] **Step 5: fmt + clippy + commit**

```bash
cargo fmt && cargo clippy -p hornvale-lab --all-targets -- -D warnings
git add windows/lab/src/schema.rs windows/lab/src/lib.rs
git commit -m "feat(lab): schema.json renderer — FNV-1a64 + self-describing census manifest (spec §2)"
```

---

### Task 2: `publish()` emits `schema.json`

**Files:**
- Modify: `windows/lab/src/publish.rs` (the `publish` fn, lines ~55–61, and its tests)

**Interfaces:**
- Consumes: `crate::schema::render_schema(result, &csv, false)` from Task 1.
- Produces: every published study dir contains `schema.json` — Tasks 4, 6 rely on `book/src/laboratory/generated/<study>/schema.json` existing after `lab run`.

- [ ] **Step 1: Extend the existing publish tests (failing first)**

In `publish_writes_summary_and_all_charts_sorted`, change the count assertion and add schema assertions:

```rust
        // 1 summary + 1 rows.csv + 1 schema.json + 2 metrics * 1 pin set = 5 files.
        assert_eq!(written.len(), 5);
```

and after the `rows_path` assertions:

```rust
        let schema_path = study_dir.join("schema.json");
        assert!(written.contains(&schema_path));
        let schema: serde_json::Value =
            serde_json::from_str(&fs::read_to_string(&schema_path).unwrap()).unwrap();
        assert_eq!(schema["study"]["name"], "publish-study");
        // The manifest binds the CSV it sits next to.
        let csv_bytes = fs::read(study_dir.join("rows.csv")).unwrap();
        assert_eq!(
            schema["rows"]["fnv1a64"],
            format!("0x{:016x}", crate::schema::fnv1a64(&csv_bytes))
        );
```

Note: `build_result()` in publish tests selects `star-class` and `moons-admitted` but stores `MetricValue::Text` for both — that's fine; `render_schema` reads kinds from the registry, not from row values.

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-lab publish 2>&1 | tee /tmp/hv-t2.txt`
Expected: FAIL — `written.len()` is 4, not 5.

- [ ] **Step 3: Implement**

In `publish()`, replace the rows.csv block with:

```rust
    // The full per-seed table, committed alongside the summary so downstream
    // consumers (the calibration suite) can load the census instead of
    // recomputing it. Regenerated and drift-checked in CI like every other
    // artifact here.
    let csv_content = crate::runner::render_csv(result);
    let rows_path = study_dir.join("rows.csv");
    fs::write(&rows_path, &csv_content)?;
    written.push(rows_path);

    // The self-describing manifest, co-generated from the same RunResult so
    // it can never disagree with the CSV it sits next to (spec §2).
    let schema_path = study_dir.join("schema.json");
    fs::write(
        &schema_path,
        crate::schema::render_schema(result, &csv_content, false),
    )?;
    written.push(schema_path);
```

- [ ] **Step 4: Run the full lab tests**

Run: `cargo test -p hornvale-lab 2>&1 | tee /tmp/hv-t2b.txt`
Expected: PASS (if other publish tests assert file counts, update them the same way — the stale-artifact and collision tests count nothing and need no change).

- [ ] **Step 5: fmt + commit**

```bash
cargo fmt && cargo clippy -p hornvale-lab --all-targets -- -D warnings
git add windows/lab/src/publish.rs
git commit -m "feat(lab): publish() co-emits schema.json beside rows.csv (spec §2)"
```

---

### Task 3: `lab backfill-schema` CLI subcommand

**Files:**
- Modify: `cli/src/main.rs` — `cmd_lab` dispatch (~line 628), the `usage()` help text (the line near line 56 listing `lab` subcommands), and a new `cmd_lab_backfill_schema` beside `cmd_lab_diff`.

**Interfaces:**
- Consumes: `hornvale_lab::{load_study, load_rows, render_schema}`.
- Produces: `hornvale lab backfill-schema <STUDY_JSON> <ROWS_CSV>` printing a backfilled manifest to stdout — Task 5 runs it for the three frozen studies.

- [ ] **Step 1: Add the dispatch arm and function**

In `cmd_lab`:

```rust
        Some("backfill-schema") => cmd_lab_backfill_schema(args),
```

and update the `None` arm's string to `(run <PATH>|diff <STUDY> <OLD_CSV> <NEW_CSV>|list-metrics|backfill-schema <STUDY_JSON> <ROWS_CSV>)`. Add beside `cmd_lab_diff`:

```rust
/// Generate a backfilled `schema.json` for a frozen study (census-as-data
/// spec §2): load the study and its committed `rows.csv`, reconstruct the
/// run, and print the manifest (marked `"backfilled": true`) on stdout —
/// the caller redirects it into the study's generated directory, once.
fn cmd_lab_backfill_schema(args: &[String]) -> Result<(), String> {
    let (Some(study_path), Some(csv_path)) = (args.get(2), args.get(3)) else {
        return Err(format!(
            "lab backfill-schema requires <STUDY_JSON> <ROWS_CSV>\n{}",
            usage()
        ));
    };
    let study =
        hornvale_lab::load_study(std::path::Path::new(study_path)).map_err(|e| e.to_string())?;
    let csv = std::fs::read_to_string(csv_path).map_err(|e| format!("read {csv_path}: {e}"))?;
    let result = hornvale_lab::load_rows(&study, &csv).map_err(|e| e.to_string())?;
    print!("{}", hornvale_lab::render_schema(&result, &csv, true));
    Ok(())
}
```

Also add one help line to `usage()` next to the existing `lab` lines:

```text
  hornvale lab backfill-schema <STUDY> <CSV>  print a backfilled schema.json for a frozen study
```

- [ ] **Step 2: Verify against a real frozen study**

Run: `cargo run -q -p hornvale -- lab backfill-schema studies/census-of-coasts-tuning.study.json book/src/laboratory/generated/census-of-coasts-tuning/rows.csv | python3 -c "import json,sys; d=json.load(sys.stdin); print(d['backfilled'], d['rows']['count'], len(d['columns']))"`
Expected: `True 2000 10` (2000 rows; seed + pin_set + 7 coast metrics + refusal = 10 columns).

- [ ] **Step 3: fmt + clippy + commit**

```bash
cargo fmt && cargo clippy -p hornvale --all-targets -- -D warnings
git add cli/src/main.rs
git commit -m "feat(cli): lab backfill-schema — one-time manifests for frozen studies (spec §2)"
```

**[STAGE] boundary: run `make preflight`; absorb main if told to.**

---

### Task 4: Promote `the-census`, freeze `branches-family` (the migration commit)

> **EXECUTION REORDER (owner directive 2026-07-13):** no census regeneration until merge prep. Steps 1–2 landed as the wip commit; Steps 3–6 (AWS regen, equivalence check, re-pins, squash) execute during merge prep (Task 10), together with Task 8's golden-pins content (which needs the re-pinned values). Until then the branch's calibration-family tests are expectedly red — Tasks 5–9 verify with scoped commands that avoid the missing fixture.

One atomic commit: rename + 1000 seeds + regeneration + re-pins must land together (golden-pin same-commit rule). This task runs the ~20–40 min release regeneration — dispatch with background/long-timeout discipline.

**Files:**
- Rename: `studies/census-lands-drift.study.json` → `studies/the-census.study.json` (contents edited)
- Delete: `book/src/laboratory/generated/census-lands-drift/` (whole dir; replaced by `the-census/`)
- Modify: `windows/lab/tests/calibration.rs`, `windows/lab/tests/fixture_staleness.rs`, `windows/lab/tests/depth_ladder.rs`, `windows/lab/tests/branches_family_calibration.rs`, `scripts/ci-census-probe.sh`, `scripts/regenerate-artifacts.sh`, `Makefile` (lab-diff help text), `CLAUDE.md` (lines ~43, ~66, ~83), `book/src/laboratory/overview.md` (the `{{#include}}` at line ~88 minimally; prose rewrite is Task 9)
- Create (by regeneration): `book/src/laboratory/generated/the-census/` and `schema.json` files for both live studies

**Interfaces:**
- Consumes: Tasks 1–2 (regeneration emits schema.json).
- Produces: `studies/the-census.study.json` and `book/src/laboratory/generated/the-census/{rows.csv,schema.json}` at 1000 seeds — Tasks 5–9 reference these exact paths.

- [ ] **Step 1: Rename and edit the study**

```bash
git mv studies/census-lands-drift.study.json studies/the-census.study.json
```

New contents:

```json
{ "name": "the-census",
  "description": "THE canonical census: every registered metric over 1,000 unselected worlds (seeds 0-999, default pins) — the project's queryable dataset (census-as-data spec §1) and the CI determinism guard. Subsumes the retired branches-family run column-for-column.",
  "seeds": { "from": 0, "count": 1000 },
  "pin_sets": [ { "label": "default", "pins": [] } ],
  "metrics": "all" }
```

- [ ] **Step 2: Re-point every consumer**

- `windows/lab/tests/calibration.rs`: both path pairs (`DRIFT` static ~lines 26–27, and inside `census_fixture_matches_live_run` ~lines 52–53) become `"../../studies/the-census.study.json"` / `"../../book/src/laboratory/generated/the-census/rows.csv"`. Update the `DRIFT` doc comment from "500-seed drift census" to "1,000-seed canonical census".
- `windows/lab/tests/fixture_staleness.rs`: `CENSUSES` shrinks from 3 entries to 2 — `the-census` (replacing census-lands-drift) and `census-of-the-meeting`. Delete the `branches-family` entry and update the const's doc comment: branches-family is frozen (census-as-data spec §1) — a frozen fixture is intentionally stale and must not be staleness-checked.
- `windows/lab/tests/depth_ladder.rs`: both `"census-lands-drift"` strings (~lines 116, 127) become `"the-census"`.
- `windows/lab/tests/branches_family_calibration.rs`: both load sites (the fixture loader ~lines 21–26 and the live-run guard ~lines 37–55) point at `"../../studies/the-census.study.json"` + `"../../book/src/laboratory/generated/the-census/rows.csv"`. The metric-index helper (~line 65) already looks up by name and works unchanged over the all-metrics study. Update the file's header comment: the battery now reads its 32 columns from the canonical census fixture; ADR 0016 pins unchanged (same seeds, same values). If the live-run guard's message names branches-family regeneration, update it to name `the-census`.
- `scripts/ci-census-probe.sh`: `STUDIES=(the-census census-of-the-meeting)`.
- `scripts/regenerate-artifacts.sh`: the census block becomes exactly two runs:

```bash
    run_release -p hornvale -- lab run studies/the-census.study.json
    run_release -p hornvale -- lab run studies/census-of-the-meeting.study.json
```

- `Makefile`: both `lab-diff` help strings `STUDY=census-lands-drift` → `STUDY=the-census`.
- `CLAUDE.md`: line ~43 comment "(… the 1000-world branches-family census)" → "(… the 1000-world canonical census)"; lines ~66 and ~83 `lab run studies/census-lands-drift.study.json` → `lab run studies/the-census.study.json`.
- `book/src/laboratory/overview.md` line ~88: `{{#include generated/census-lands-drift/census-lands-drift-summary.md}}` → `{{#include generated/the-census/the-census-summary.md}}` (prose around it is Task 9's job, but the include must not dangle).

```bash
git rm -r book/src/laboratory/generated/census-lands-drift
```

- [ ] **Step 3: Regenerate everything — ON AWS (owner directive 2026-07-13: censuses never regenerate locally)**

First commit the edits so the branch carries them (artifacts pending — this commit is squashed away in Step 6):

```bash
cargo fmt && git add -A && git commit -m "wip(census): migration edits, artifacts pending (squashed in the migration commit)"
```

Then the CONTROLLER (not an implementer subagent) runs the regen on the spot box:

```bash
bash scripts/aws-gate/regen-git.sh ~/.config/superpowers/worktrees/hornvale/census-as-data 2>&1 | tee /tmp/hv-regen-aws.txt
```

This pushes the branch to a bare repo on a fresh c7a.16xlarge, runs `scripts/regenerate-artifacts.sh` there, commits the artifacts ON the branch, and fetches the commit home (box always terminated on exit; artifacts fetched before teardown). Review the fetched diff, then:

```bash
git merge --ff-only FETCH_HEAD
git diff --stat HEAD~1 HEAD | tail -20
```

Expected diff surface: new `book/src/laboratory/generated/the-census/*`, `census-of-the-meeting/` gains schema.json ONLY (rows.csv byte-identical), nothing else unexpected. If any OTHER artifact drifted, STOP and investigate before proceeding (that's a determinism bug, not noise). No `docs/timings.md` row — timings are machine-specific and the box row would mislead; skip it.

- [ ] **Step 4: One-shot equivalence check — the frozen branches-family fixture is a projection of the canonical fixture**

```bash
python3 - <<'PY'
import csv
def rows(path):
    with open(path) as f:
        return list(csv.DictReader(f))
old = rows("book/src/laboratory/generated/branches-family/rows.csv")
new = {r["seed"]: r for r in rows("book/src/laboratory/generated/the-census/rows.csv")}
cols = [c for c in old[0].keys() if c not in ("refusal",)]
bad = 0
for o in old:
    n = new[o["seed"]]
    for c in cols:
        if o[c] != n[c]:
            print(f"MISMATCH seed {o['seed']} col {c}: {o[c]!r} != {n[c]!r}")
            bad += 1
            if bad > 10: raise SystemExit(1)
print("OK" if bad == 0 else f"{bad} mismatches")
raise SystemExit(1 if bad else 0)
PY
```

Expected: `OK`. A mismatch means metric extraction is NOT read-only observation — a determinism bug that must be diagnosed (superpowers:systematic-debugging) before this task proceeds. Do not paper over it.

- [ ] **Step 5: Run the lab tests; re-pin drifted golden means (same commit)**

Run: `cargo nextest run -p hornvale-lab 2>&1 | tee /tmp/hv-t4.txt`
Expected: calibration tests that pin 500-seed means/dstributions FAIL with observed-vs-pinned messages (the sample doubled). For each failure in `windows/lab/tests/calibration.rs`, replace the pinned literal with the printed observed value. Re-run once: `cargo nextest run -p hornvale-lab 2>&1 | tee /tmp/hv-t4b.txt` → PASS. The `branches_family_calibration.rs` ADR 0016 pins must NOT need changing (Step 4 proved the columns identical) — if one fails, that's the same stop-and-debug signal.

- [ ] **Step 6: Probe + gate + squash into the single migration commit**

The branch now carries: wip-edits commit → box artifact commit → (uncommitted re-pins). Squash all of it into ONE migration commit so the golden-pin same-commit rule holds in history (branch is private and unpushed; rewriting it is safe):

```bash
bash scripts/ci-census-probe.sh          # expected: "all 2 studies match on the first 8 seeds"
                                         # (8-seed release probe — fast tier, allowed locally)
make gate 2>&1 | tail -5                 # expected: green
cargo fmt
git add -A
BASE=$(git log --format=%H --grep='wip(census): migration edits' -n1)
git reset --soft "$BASE~1"
git commit -m "feat(census)!: promote the-census (all metrics x 1000 seeds); freeze branches-family

census-lands-drift renamed/doubled per census-as-data spec §1; branches-family
leaves regeneration + probe + staleness (frozen preregistered evidence, its
calibrations now read the canonical fixture); calibration means re-pinned for
the 1000-seed sample in this same commit; schema.json now published for both
live studies."
```

**[STAGE] boundary: run `make preflight`; absorb main if told to.**

---

### Task 5: Backfilled manifests for the frozen tier

**Files:**
- Create: `book/src/laboratory/generated/branches-family/schema.json`, `book/src/laboratory/generated/census-of-coasts/schema.json`, `book/src/laboratory/generated/census-of-coasts-tuning/schema.json`

**Interfaces:**
- Consumes: Task 3's `lab backfill-schema`.
- Produces: `schema.json` in each frozen study dir with `"backfilled": true` — Task 6's harness types frozen studies from these.

- [ ] **Step 1: Generate all three**

```bash
for s in branches-family census-of-coasts census-of-coasts-tuning; do
  cargo run -q -p hornvale -- lab backfill-schema \
    "studies/$s.study.json" \
    "book/src/laboratory/generated/$s/rows.csv" \
    > "book/src/laboratory/generated/$s/schema.json"
done
```

- [ ] **Step 2: Verify each binds its CSV**

```bash
for s in branches-family census-of-coasts census-of-coasts-tuning; do
  python3 - "$s" <<'PY'
import json, sys
s = sys.argv[1]
d = json.load(open(f"book/src/laboratory/generated/{s}/schema.json"))
n = sum(1 for _ in open(f"book/src/laboratory/generated/{s}/rows.csv")) - 1
assert d["backfilled"] is True and d["rows"]["count"] == n, (s, d["rows"], n)
print(f"ok: {s} ({n} rows, {len(d['columns'])} columns)")
PY
done
```

Expected: three `ok:` lines (1000, 10000, 2000 rows).

- [ ] **Step 3: Confirm the drift check ignores them (frozen dirs aren't regenerated)**

`scripts/regenerate-artifacts.sh` no longer touches these dirs (Task 4), so CI's `git diff --exit-code` can't flag them. No action — this step is the reasoning check.

- [ ] **Step 4: Commit**

```bash
git add book/src/laboratory/generated/*/schema.json
git commit -m "feat(census): backfilled schema.json for the frozen tier (spec §2)"
```

---

### Task 6: `tools/census/` — manifest, builder, validation, `make census` / `census-query`

**Files:**
- Create: `tools/census/manifest.json`, `tools/census/build.sh`, `tools/census/build_ddl.py`
- Modify: `.gitignore` (add `tools/census/.build/`), `Makefile` (targets `census`, `census-query`; extend `shellcheck` target with `tools/census/*.sh`), `docs/superpowers/specs/2026-07-13-census-as-data-design.md` (§3: "duckdb and jq" → "duckdb and python3" — jq cannot compute FNV over bytes; python3 is already project-blessed via ci-census-probe.sh)

**Interfaces:**
- Consumes: schema.json files from Tasks 4–5; `docs/timings.md`.
- Produces: `tools/census/.build/census.duckdb` with per-study typed views named by study (`"the-census"`, quoted, hyphens and all), `census_long`, and `timings`; `bash tools/census/build.sh` as the idempotent builder — Tasks 7–8 extend this database.

- [ ] **Step 1: Write `tools/census/manifest.json`**

Fill `frozen_at` with real hashes: for each frozen study run `git log -1 --format=%H -- book/src/laboratory/generated/<study>` (branches-family's is Task 4's migration commit; the coasts pair predate it).

```json
{
  "live": [
    { "study": "the-census",
      "dir": "book/src/laboratory/generated/the-census",
      "study_json": "studies/the-census.study.json" },
    { "study": "census-of-the-meeting",
      "dir": "book/src/laboratory/generated/census-of-the-meeting",
      "study_json": "studies/census-of-the-meeting.study.json" }
  ],
  "frozen": [
    { "study": "branches-family",
      "dir": "book/src/laboratory/generated/branches-family",
      "epoch": "pre-consolidation",
      "frozen_at": "<FILL: git log -1 --format=%H -- book/src/laboratory/generated/branches-family>",
      "note": "1,000-seed family battery; subsumed column-for-column by the-census (spec §1); ADR 0016 evidence" },
    { "study": "census-of-coasts",
      "dir": "book/src/laboratory/generated/census-of-coasts",
      "epoch": "pre-crust",
      "frozen_at": "<FILL>",
      "note": "10,000-seed preregistered terrain-overhaul baseline; regenerating under current physics would falsify the record" },
    { "study": "census-of-coasts-tuning",
      "dir": "book/src/laboratory/generated/census-of-coasts-tuning",
      "epoch": "crust-tuning",
      "frozen_at": "<FILL>",
      "note": "2,000-seed tuning-loop variant; measurement record of the Task 9 physics loop" }
  ],
  "sidecars": [
    { "table": "timings", "path": "docs/timings.md", "format": "markdown-table" }
  ]
}
```

- [ ] **Step 2: Write `tools/census/build_ddl.py`**

```python
#!/usr/bin/env python3
"""Validate committed census artifacts and emit DuckDB DDL (census-as-data spec §3).

Reads tools/census/manifest.json; for every study entry validates the
schema.json <-> rows.csv pair (row count, FNV-1a64 content hash, header
order), then prints CREATE VIEW DDL: one typed wide view per study plus the
unified long view `census_long` and the `timings` sidecar. Any validation
failure aborts with the file and the failed check — a dataset that cannot
mount must never silently vanish from query results.
"""
import csv, json, os, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
BUILD = os.path.join(ROOT, "tools", "census", ".build")
SQL_TYPES = {"integer": "BIGINT", "numeric": "DOUBLE", "flag": "BOOLEAN", "categorical": "VARCHAR"}

def die(msg):
    sys.stderr.write(f"census build: {msg}\n")
    sys.exit(1)

def fnv1a64(data: bytes) -> int:
    h = 0xCBF29CE484222325
    for b in data:
        h = ((h ^ b) * 0x100000001B3) & 0xFFFFFFFFFFFFFFFF
    return h

def validate(entry):
    d = os.path.join(ROOT, entry["dir"])
    schema_path, rows_path = os.path.join(d, "schema.json"), os.path.join(d, "rows.csv")
    for p in (schema_path, rows_path):
        if not os.path.exists(p):
            die(f"{entry['study']}: missing {p}")
    schema = json.load(open(schema_path))
    raw = open(rows_path, "rb").read()
    got = f"0x{fnv1a64(raw):016x}"
    if got != schema["rows"]["fnv1a64"]:
        die(f"{entry['study']}: rows.csv hash {got} != manifest {schema['rows']['fnv1a64']} (stale pair)")
    lines = raw.decode("utf-8").splitlines()
    if len(lines) - 1 != schema["rows"]["count"]:
        die(f"{entry['study']}: {len(lines)-1} data rows != manifest count {schema['rows']['count']}")
    names = [c["name"] for c in schema["columns"]]
    if ",".join(names) != lines[0]:
        die(f"{entry['study']}: schema columns disagree with the CSV header")
    return schema, rows_path

def wide_view(entry, schema, rows_path):
    cols = ", ".join(
        f"'{c['name']}': '{SQL_TYPES[c['kind']]}'" for c in schema["columns"]
    )
    return (
        f'CREATE OR REPLACE VIEW "{entry["study"]}" AS '
        f"SELECT * FROM read_csv('{rows_path}', header=true, nullstr='', columns={{{cols}}});"
    )

def long_selects(entry, schema, tier):
    epoch = entry.get("epoch", "live")
    for c in schema["columns"]:
        if c["name"] in ("seed", "pin_set", "refusal"):
            continue
        k = c["kind"]
        vals = {
            "numeric": (f'"{c["name"]}"', "NULL", "NULL"),
            "flag": ("NULL", "NULL", f'"{c["name"]}"'),
            "categorical": ("NULL", f'"{c["name"]}"', "NULL"),
        }[k]
        yield (
            f"SELECT '{entry['study']}' AS study, '{tier}' AS tier, '{epoch}' AS epoch, "
            f"seed, pin_set, '{c['name']}' AS metric, '{k}' AS kind, "
            f"CAST({vals[0]} AS DOUBLE) AS value_num, CAST({vals[1]} AS VARCHAR) AS value_text, "
            f'CAST({vals[2]} AS BOOLEAN) AS value_flag FROM "{entry["study"]}"'
        )

def timings_csv(path):
    """docs/timings.md markdown table -> .build/timings.csv."""
    out = os.path.join(BUILD, "timings.csv")
    with open(path) as f, open(out, "w", newline="") as o:
        w = csv.writer(o)
        rows = [l.strip() for l in f if l.strip().startswith("|")]
        for i, line in enumerate(rows):
            cells = [c.strip() for c in line.strip("|").split("|")]
            if i == 1 and set("".join(cells)) <= set("-: "):
                continue
            w.writerow(cells)
    return out

def main():
    os.makedirs(BUILD, exist_ok=True)
    manifest = json.load(open(os.path.join(ROOT, "tools", "census", "manifest.json")))
    parts = []
    for tier in ("live", "frozen"):
        for entry in manifest[tier]:
            schema, rows_path = validate(entry)
            print(wide_view(entry, schema, rows_path))
            parts.extend(long_selects(entry, schema, tier))
    print("CREATE OR REPLACE VIEW census_long AS\n" + "\nUNION ALL\n".join(parts) + ";")
    for side in manifest.get("sidecars", []):
        if side["format"] == "markdown-table":
            p = timings_csv(os.path.join(ROOT, side["path"]))
            print(
                f'CREATE OR REPLACE VIEW "{side["table"]}" AS '
                f"SELECT * FROM read_csv('{p}', header=true);"
            )

if __name__ == "__main__":
    main()
```

- [ ] **Step 3: Write `tools/census/build.sh`**

```bash
#!/usr/bin/env bash
# tools/census/build.sh — materialize the throwaway analysis database
# (census-as-data spec §3). Validates every mounted dataset (row count,
# FNV-1a64, header order) then builds .build/census.duckdb. Fails loudly at
# mount time; a dataset that can't mount aborts the whole build.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

for tool in duckdb python3; do
    command -v "$tool" >/dev/null 2>&1 || {
        echo "census: '$tool' not found — install with: brew install $tool" >&2
        exit 1
    }
done

mkdir -p tools/census/.build
python3 tools/census/build_ddl.py > tools/census/.build/ddl.sql
duckdb tools/census/.build/census.duckdb < tools/census/.build/ddl.sql > /dev/null
echo "census: built tools/census/.build/census.duckdb" >&2
```

`chmod +x tools/census/build.sh`.

- [ ] **Step 4: Makefile + .gitignore + spec amendment**

Makefile (beside `lab-diff`; add both to `.PHONY`):

```makefile
census: ## Build the analysis DB from committed censuses and open DuckDB on it
	@bash tools/census/build.sh
	@duckdb tools/census/.build/census.duckdb

census-query: ## One-shot census query (usage: make census-query Q="SELECT ...")
	@test -n "$(Q)" || { echo "usage: make census-query Q=\"SELECT ...\""; exit 2; }
	@bash tools/census/build.sh
	@duckdb tools/census/.build/census.duckdb -c "$(Q)"
```

`.gitignore`: append `tools/census/.build/`. `shellcheck` target: append `tools/census/*.sh` to the file list. Spec §3: replace the sentence naming `jq` with `python3` (one-word amendment; python3 is the established scripting tool per `ci-census-probe.sh`, and jq cannot hash bytes).

- [ ] **Step 5: Verify**

```bash
shellcheck tools/census/build.sh
make census-query Q="SELECT study, tier, count(*) AS rows FROM census_long GROUP BY ALL ORDER BY study"
```

Expected: five rows — branches-family/frozen, census-of-coasts/frozen, census-of-coasts-tuning/frozen, census-of-the-meeting/live, the-census/live — with plausible counts (the-census: 1000 seeds × ~110 metrics ≈ 110k). Also spot a typed query: `make census-query Q="SELECT count(*) FROM \"the-census\" WHERE \"tidally-locked\" AND \"moons-admitted\" >= 2"` returns a number without cast errors.

- [ ] **Step 6: Commit**

```bash
git add tools/census .gitignore Makefile docs/superpowers/specs/2026-07-13-census-as-data-design.md
git commit -m "feat(tools): tools/census — validated DuckDB harness over the committed censuses (spec §3)"
```

**[STAGE] boundary: run `make preflight`; absorb main if told to.**

---

### Task 7: `history.sh` + `census-history` + explore queries

**Files:**
- Create: `tools/census/history.sh`, `tools/census/history_load.py`, `tools/census/queries/explore/seed-biography.sql`, `tools/census/queries/explore/interesting-worlds.sql`, `tools/census/queries/explore/metric-drift.sql`
- Modify: `Makefile` (target `census-history`)

**Interfaces:**
- Consumes: Task 6's build (`census.duckdb` must exist; `history.sh` calls `build.sh` first).
- Produces: table `census_history` (columns: `study, seed, pin_set, metric, kind, value_num, value_text, value_flag, commit, commit_date, epoch_label`) inside the same database — Task 8's check and the explore queries read it.

- [ ] **Step 1: Write `tools/census/history_load.py`**

```python
#!/usr/bin/env python3
"""Wide->long conversion for historical rows.csv snapshots (spec §3).

stdin: one wide rows.csv (any column set — history absorbs schema drift).
argv: study, commit, commit_date, epoch_label, current-schema.json path.
stdout: long CSV rows (no header) matching census_history's column order.
Typing falls back: current schema for columns that still exist, VARCHAR
(value_text) for retired ones.
"""
import csv, json, sys

study, commit, cdate, label, schema_path = sys.argv[1:6]
kinds = {}
try:
    for c in json.load(open(schema_path))["columns"]:
        kinds[c["name"]] = c["kind"]
except FileNotFoundError:
    pass

reader = csv.DictReader(sys.stdin)
w = csv.writer(sys.stdout)
for row in reader:
    for col, val in row.items():
        if col in ("seed", "pin_set", "refusal") or val is None:
            continue
        kind = kinds.get(col, "categorical")
        num = val if (kind == "numeric" and val != "") else ""
        flg = val if (kind == "flag" and val != "") else ""
        txt = val if (kind == "categorical" and val != "") else ""
        w.writerow([study, row["seed"], row["pin_set"], col, kind,
                    num, txt, flg, commit, cdate, label])
```

- [ ] **Step 2: Write `tools/census/history.sh`**

```bash
#!/usr/bin/env bash
# tools/census/history.sh <study> — load every committed snapshot of a
# study's rows.csv into census_history, tagged with commit/date/epoch label
# (the --first-parent subject; spec §3). Usage: bash tools/census/history.sh the-census
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"
study="${1:?usage: history.sh <study>}"

dir="$(python3 -c "
import json,sys
m=json.load(open('tools/census/manifest.json'))
for tier in ('live','frozen'):
    for e in m[tier]:
        if e['study']=='$study': print(e['dir']); sys.exit(0)
sys.exit('unknown study: $study')")"
path="$dir/rows.csv"
schema="$dir/schema.json"

bash tools/census/build.sh
hist="tools/census/.build/history-$study.csv"
: > "$hist"

# The path may have lived at an older location (the-census was
# census-lands-drift before consolidation); --follow tracks the rename.
git log --follow --first-parent --format='%H%x09%cI%x09%s' main -- "$path" |
while IFS=$'\t' read -r commit cdate subject; do
    file="$(git log -1 --format= --name-only --follow "$commit" -- "$path" 2>/dev/null | head -1)"
    git show "$commit:${file:-$path}" 2>/dev/null |
        python3 tools/census/history_load.py "$study" "$commit" "$cdate" "$subject" "$schema" >> "$hist" || true
done

duckdb tools/census/.build/census.duckdb -c "
CREATE OR REPLACE TABLE census_history AS
SELECT * FROM read_csv('$hist', header=false, nullstr='', columns={
  'study':'VARCHAR','seed':'BIGINT','pin_set':'VARCHAR','metric':'VARCHAR',
  'kind':'VARCHAR','value_num':'DOUBLE','value_text':'VARCHAR','value_flag':'BOOLEAN',
  'commit':'VARCHAR','commit_date':'TIMESTAMP','epoch_label':'VARCHAR'});
SELECT count(*) AS history_rows, count(DISTINCT commit) AS snapshots FROM census_history;"
echo "census: census_history loaded for $study" >&2
```

`chmod +x tools/census/history.sh`.

- [ ] **Step 3: The three explore queries**

`tools/census/queries/explore/seed-biography.sql`:

```sql
-- The biography of one world: every metric of one seed, across every
-- committed snapshot of the canonical census (spec §3; run history.sh first).
-- Change the seed to follow a different world.
SELECT epoch_label, commit_date, metric,
       coalesce(CAST(value_num AS VARCHAR), value_text, CAST(value_flag AS VARCHAR)) AS value
FROM census_history
WHERE seed = 42 AND study = 'the-census'
ORDER BY metric, commit_date;
```

`tools/census/queries/explore/interesting-worlds.sql`:

```sql
-- Filter template: "interesting" is a query, not a generation stage
-- (spec: the census is an unselected population sample). Edit freely.
SELECT seed, "star-class", "moons-admitted", "tidally-locked",
       "pantheon-size", "ocean-fraction", "settlement-count"
FROM "the-census"
WHERE "moons-admitted" >= 2 AND "pantheon-size" >= 6
ORDER BY "settlement-count" DESC
LIMIT 25;
```

`tools/census/queries/explore/metric-drift.sql`:

```sql
-- How one numeric metric's distribution moved across history epochs
-- (run history.sh first). Swap the metric name to track another.
SELECT epoch_label, min(commit_date) AS at,
       count(*) AS n, avg(value_num) AS mean,
       quantile_cont(value_num, 0.5) AS median
FROM census_history
WHERE metric = 'hypsometric-bimodality' AND study = 'the-census'
GROUP BY epoch_label
ORDER BY at;
```

- [ ] **Step 4: Makefile target**

```makefile
census-history: ## Load a study's git history into census_history (usage: make census-history STUDY=the-census)
	@test -n "$(STUDY)" || { echo "usage: make census-history STUDY=<study-name>"; exit 2; }
	@bash tools/census/history.sh "$(STUDY)"
```

- [ ] **Step 5: Verify**

```bash
shellcheck tools/census/history.sh
make census-history STUDY=the-census
duckdb tools/census/.build/census.duckdb < tools/census/queries/explore/seed-biography.sql | head -20
```

Expected: history load reports ≥1 snapshot (more once main's history includes pre-rename census-lands-drift commits via `--follow`); the biography prints seed-42 metric rows.

- [ ] **Step 6: Commit**

```bash
git add tools/census Makefile
git commit -m "feat(tools): census history extraction + explore queries (spec §3)"
```

---

### Task 8: `golden-pins.sql` + `make census-check`

**Files:**
- Create: `tools/census/queries/calibrate/golden-pins.sql`, `tools/census/check.sh`
- Modify: `Makefile` (target `census-check`), `scripts/gate-full-heavy.sh` OR the `gate-full` help text (add a printed reminder line: "harness: make census-check (local, brew tools)") — reminder only, NOT a gate step (CI runners lack duckdb).

**Interfaces:**
- Consumes: Tasks 6–7.
- Produces: `make census-check` exits nonzero on any mount failure, smoke failure, or golden-pin mismatch.

- [ ] **Step 1: Write `golden-pins.sql`**

At execution time, open `windows/lab/tests/calibration.rs` and copy EVERY pinned constant that is a mean/share over the `the-census` fixture (the values just re-pinned in Task 4 Step 5), one row each. The query shape (values below are ILLUSTRATIVE — copy the real ones from the test file, with the test name in the comment):

```sql
-- The pin-provenance report (spec §3): every pinned calibration constant,
-- recomputed from the committed fixture and compared to the value pinned in
-- windows/lab/tests/calibration.rs. Duplication is deliberate — an
-- independent second path from fixture to pin. When a pin re-pins in Rust,
-- update the literal here IN THE SAME COMMIT.
WITH checks AS (
  SELECT 'mean ocean-fraction (calibration.rs::<test name>)' AS pin,
         (SELECT avg("ocean-fraction") FROM "the-census") AS computed,
         0.0 AS pinned      -- <- copy the pinned literal
  UNION ALL
  SELECT 'mean habitable-fraction (calibration.rs::<test name>)',
         (SELECT avg("habitable-fraction") FROM "the-census"),
         0.0                 -- <- copy the pinned literal
  -- ... one row per pinned constant found in calibration.rs ...
)
SELECT pin, computed, pinned, abs(computed - pinned) < 1e-6 AS ok
FROM checks ORDER BY pin;
```

Rounding note: pinned Rust literals passed `quantize` (8 significant digits); `abs(computed - pinned) < 1e-6` absorbs that. If a pin is a count or share, compare exactly (`computed = pinned AS ok`).

- [ ] **Step 2: Write `tools/census/check.sh`**

```bash
#!/usr/bin/env bash
# tools/census/check.sh — the harness's own gate (spec §3): mount-validate,
# smoke every mounted dataset, and fail on any golden-pin mismatch.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

bash tools/census/build.sh

db="tools/census/.build/census.duckdb"
echo "census-check: smoke (one count per mounted dataset)" >&2
duckdb "$db" -csv -c "SELECT study, count(*) FROM census_long GROUP BY study ORDER BY study" | sed 's/^/  /'
duckdb "$db" -csv -c "SELECT count(*) FROM timings" > /dev/null

echo "census-check: golden pins" >&2
pins="$(duckdb "$db" -csv -c ".read tools/census/queries/calibrate/golden-pins.sql")"
echo "$pins" | sed 's/^/  /'
if echo "$pins" | grep -q ",false$"; then
    echo "census-check: GOLDEN PIN MISMATCH — fixture and calibration.rs disagree" >&2
    exit 1
fi
echo "census-check: ok" >&2
```

`chmod +x tools/census/check.sh`.

- [ ] **Step 3: Makefile target + gate-full reminder**

```makefile
census-check: ## Harness gate: mount-validate + smoke + golden-pins (local; needs duckdb+python3)
	@bash tools/census/check.sh
```

In the `gate-full` target (or `scripts/gate-full-heavy.sh` tail), add the one-line echo reminder: `@echo "reminder: 'make census-check' verifies the analysis harness (local-only, brew tools)"`.

- [ ] **Step 4: Verify — including one deliberate failure**

```bash
shellcheck tools/census/check.sh
make census-check          # expected: smoke counts + all pins ok, exit 0
```

Then corrupt one pinned literal in `golden-pins.sql` (change a digit), re-run `make census-check` → expected: exit 1 with "GOLDEN PIN MISMATCH". Revert the corruption.

- [ ] **Step 5: Commit**

```bash
git add tools/census Makefile scripts/gate-full-heavy.sh
git commit -m "feat(tools): census-check — harness gate + golden-pins provenance report (spec §3)"
```

**[STAGE] boundary: run `make preflight`; absorb main if told to.**

---

### Task 9: Docs sweep — book, decision log, docs map

**Files:**
- Modify: `book/src/laboratory/overview.md` (the study-roster prose, lines ~45–90), `docs/README.md` (one line in the tooling/docs map section)
- Create: `docs/decisions/0044-one-canonical-census.md` (confirm 0044 is still the next free number at execution: `ls docs/decisions | tail -3`; renumber if a parallel campaign landed one)

**Interfaces:**
- Consumes: everything prior (describes merged reality).
- Produces: the book no longer lags the change; the decision is grep-able.

- [ ] **Step 1: Rewrite the laboratory overview roster + add "The Census as Data"**

In `book/src/laboratory/overview.md`, rewrite the study-roster prose (~lines 45–66) to describe the two tiers, and add a new section before the generated include. Content to convey (write at the book's altitude — technical prose, not bullets):

- **Live tier:** `the-census` — every registered metric over 1,000 unselected worlds (seeds 0–999, default pins), the canonical dataset and CI determinism guard; and `census-of-the-meeting` — the solo-roster null control (a genuinely different population). Both regenerated by `make rebaseline`, drift-checked, CI-probed.
- **Frozen tier:** `branches-family` (frozen at the consolidation, its data subsumed column-for-column by `the-census`; still the ADR 0016 evidence), `census-of-coasts` and `census-of-coasts-tuning` (preregistered terrain baselines). Frozen studies are evidence, not instruments: never regenerated — but each records its producing commit in `tools/census/manifest.json`, so any frozen census is a reproducible citation (`git checkout <commit> && lab run`).
- **The Census as Data** (new section): every study dir carries a `schema.json` (typed, documented columns; quantization convention; row count + FNV-1a64 content hash binding manifest to data). `make census` opens a DuckDB session over all of it (typed wide views + the `census_long` long view); `make census-query Q="…"` is the one-shot form; `make census-history STUDY=…` loads the git-history longitudinal table. The graduation doctrine: an ad-hoc query that finds something graduates to a committed canned query under `tools/census/queries/explore/`, and, if it keeps earning its keep, to a metric or preregistered study; queries you *cannot* write mark missing metrics. `golden-pins.sql` is the pin-provenance report: every pinned calibration constant is reproducible as a query against the committed fixture.

Verify the include at the bottom points at `generated/the-census/the-census-summary.md` (done in Task 4).

Run: `mdbook build book 2>&1 | tail -3` → expected: success, no missing-include warnings.

- [ ] **Step 2: Decision record**

`docs/decisions/0044-one-canonical-census.md`, following the existing records' format (check a neighbor like 0040 for the exact header shape):

```markdown
# 0044 — One canonical census; frozen studies are evidence

**Status:** ratified 2026-07-13 (census-as-data campaign)

The live census record is ONE all-metrics study (`the-census`, 1,000 seeds,
default pins) plus the solo-roster null control (`census-of-the-meeting`).
`branches-family` froze at the consolidation: its run retired (its data is a
strict column projection of the canonical run — verified byte-identical at
migration), its committed artifacts kept as preregistered evidence, its
calibrations re-pointed at the canonical fixture. Frozen studies are never
regenerated (regenerating under moved physics would falsify the record) but
each records its producing commit, so any of them is reproducible via
checkout. Every published study carries a co-generated `schema.json`
(spec §2); the analysis harness lives OUTSIDE the workspace
(`tools/census/`, duckdb + python3), like the type audit (0027/0028
pattern). Considered and refused: append-only epoch directories in the
working tree (git IS the append-only store), a committed long-format CSV
(derived, not stored), Rust-native query surfaces (std-only SQL
reinvention). Spec: docs/superpowers/specs/2026-07-13-census-as-data-design.md.
```

Add the 0044 line to `docs/decisions/README.md`'s index if one exists (check; the log had a renumbering pass in 0043 — follow its listing convention).

- [ ] **Step 3: docs map line**

In `docs/README.md`, in the section mapping tooling (where `tools/type-audit` is described), add:

```markdown
- `tools/census/` — the census analysis harness (outside the workspace):
  DuckDB views over every committed `rows.csv`+`schema.json` pair, git-history
  longitudinal extraction, canned explore/calibrate queries. `make census`,
  `make census-query`, `make census-history`, `make census-check`.
```

- [ ] **Step 4: Gate + commit**

```bash
cargo test -p hornvale --test docs_consistency 2>&1 | tail -3   # registry/docs links still resolve
make gate 2>&1 | tail -5
git add book/src/laboratory/overview.md docs/decisions docs/README.md
git commit -m "docs(census): laboratory tiers + The Census as Data; decision 0044; docs map"
```

---

### Task 10: Campaign close

**Files:** per the closing-a-campaign skill (chronicle entry, retrospective, Confidence Gradient re-score, merge).

- [ ] **Step 1: Invoke the `closing-a-campaign` skill** and follow it: chronicle entry in `book/src/chronicle/`, one-page retrospective in `docs/retrospectives/` (decision 0020), Confidence Gradient re-score of `book/src/open-questions.md` if a bet moved (the "can we do data science on the census" bet, if listed), freshness sweep of stale chapters.
- [ ] **Step 2: `make preflight`** from the branch; on NO-GO merge main INTO the branch, re-run `make gate`.
- [ ] **Step 3: Final heavy-tier evidence via `make gate-remote` (the heavy tier's live 1000-world batteries do not run locally) + `make census-check`.
- [ ] **Step 4: Merge to main** per the skill; remove the worktree.

---

## Self-Review (performed at plan-writing time)

- **Spec coverage:** §1 → Task 4; §2 → Tasks 1–3, 5; §3 → Tasks 6–8 (including census-query, graduation doctrine text in Task 9, golden-pins, error doctrine in build.sh/check.sh); §4 → decision record + manifest `frozen_at` fields; §5 → Tasks 1–2 (unit), 4 (migration one-shots), 8 (harness gate); §6 → Tasks 9–10 (idea-registry rows already committed with the spec); §7 → no tasks (refusals), recorded in 0044; §8 → Task 4.
- **Known deviation:** spec §3 named `jq`; the plan uses `python3` (jq cannot hash bytes; python3 is precedented in `ci-census-probe.sh`). Task 6 Step 4 amends the spec line in the same commit.
- **Data-dependent values** (re-pinned means in Task 4 Step 5, pinned literals in golden-pins.sql, `frozen_at` hashes): unknowable at plan time; each has an exact discovery command in its step.
- **Type consistency:** `render_schema(result: &RunResult, csv: &str, backfilled: bool) -> String` and `fnv1a64(bytes: &[u8]) -> u64` used identically in Tasks 1, 2, 3; `census_long` column list identical in Tasks 6, 7, 8.
