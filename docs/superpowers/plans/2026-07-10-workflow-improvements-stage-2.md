# Workflow Improvements Stage 2 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land the next three workflow-tooling keystones from `WORKFLOW_IMPROVEMENTS_PLAN.md` — TOOL-16 (fixture-staleness probe), TOOL-20 (golden-master accept harness), TOOL-19 (human-readable lab diff) — plus the registry catch-up Stage 1 owed.

**Architecture:** TOOL-16 is a new always-on integration test in `windows/lab` that regenerates the first 3 seeds of each committed census live and compares them (quantize-canonicalized) against the committed `rows.csv` fixture, failing with the regeneration instruction. TOOL-20 is a new std-only `hornvale_kernel::golden` module (`assert_golden` / `check_golden` with a `REBASELINE=1` accept path) that replaces the three ad-hoc byte-golden comparisons in `cli`, `windows/scene`, and `windows/worldgen`. TOOL-19 is a new `windows/lab/src/diff.rs` renderer plus a `hornvale lab diff` CLI subcommand and a `make lab-diff` wrapper that summarize which metric moved between two census snapshots.

**Tech Stack:** Rust edition 2024, std + serde/serde_json only (workspace allowlist), `make`, mdBook prose.

## Global Constraints

Every task's requirements implicitly include all of these (from CLAUDE.md and the decision log):

- Dependencies: `serde` + `serde_json` only, workspace-wide. No new crates.
- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only (clippy `disallowed-types` enforces).
- No wall-clock time anywhere.
- Every crate sets `#![warn(missing_docs)]`; every public item, field, and variant gets a one-line doc comment.
- Every primitive at a `pub` boundary carries a `type-audit:` verdict tag; after adding pub items run `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` and regenerate the report (`-- report > docs/audits/type-audit-report.md`) in the same commit.
- Cost-ordered gate per commit: `make quick` (fmt-check + clippy) first, then scoped `cargo test -p <crate>`; the full `make gate` is the final pre-merge step, not every intermediate run. Run test suites ONCE and capture output (`2>&1 | tee /tmp/hv-test.txt`); never re-run to grep a second line.
- `cargo fmt` as the final step before every commit.
- The book may never lag merged reality: a CLI-surface or self-check change updates `book/src/laboratory/overview.md` in the same task.
- Registry discipline (`book/src/frontier/CLAUDE.md`): a shipped idea's row flips status to `shipped` in the shipping commit; row text/IDs are never rewritten. `cargo test -p hornvale --test docs_consistency` checks the registry/book invariants.
- Determinism: all new output (diff reports, probe comparisons) must be deterministic — floats displayed through `hornvale_kernel::quantize`, iteration in study/`BTreeMap` order.
- Commit messages end with:
  `Claude-Session: https://claude.ai/code/session_01STboWuJ5sz26RTjVwDkGQu`
- **Parallel-work caution:** the Lexicon-homophony campaign also touches `windows/lab` (`metrics.rs`, studies). Keep edits to `windows/lab/src/lib.rs` minimal (additive lines only) and do not touch `metrics.rs` or `IMPLEMENTATION_PLAN.md`.

---

### Task 1: Registry catch-up + plan-file Stage 2 section

Stage 1 shipped TOOL-14/TOOL-15/PROC-6 but commit 3519af0 only *added* registry rows; the three shipped rows still read `raw`. Flip them, and record Stage 2 in the tracking plan.

**Files:**
- Modify: `book/src/frontier/idea-registry.md` (rows at lines ~247, ~248, ~417)
- Modify: `WORKFLOW_IMPROVEMENTS_PLAN.md`

**Interfaces:** none (docs only).

- [ ] **Step 1: Flip the three Stage-1 rows to `shipped`**

In `book/src/frontier/idea-registry.md`, change only the Status and Where cells (never the idea text):

- TOOL-14 row: `| raw | high | ideonomy session (workflow/tooling) |` → `| shipped | high | ideonomy session (workflow/tooling); \`Makefile\` + \`scripts/hooks/pre-commit\` |`
- TOOL-15 row: `| raw | high | ideonomy session (workflow/tooling) |` → `| shipped | high | ideonomy session (workflow/tooling); \`scripts/regenerate-artifacts.sh\` (exclude-glob relocation deferred) |`
- PROC-6 row: `| raw | high | ideonomy session (workflow/tooling) |` → `| shipped | high | ideonomy session (workflow/tooling); \`windows/lab/tests/preregistration_guard.rs\` |`

(Each row's Status is the 3rd-from-last cell; TOOL-14/15 sit around lines 247–248, PROC-6 around line 417. Match on the row's `| TOOL-14 |` prefix, not line numbers.)

- [ ] **Step 2: Add the Stage 2 section to `WORKFLOW_IMPROVEMENTS_PLAN.md`**

Replace the current `## Stage 2+ (later passes)` section with:

```markdown
## Stage 2: The fixture spine + review surface (this pass)

**Goal**: the two rows that build on TOOL-15's regeneration spine (TOOL-16,
TOOL-20) plus the review-surface companion (TOOL-19). Plan:
`docs/superpowers/plans/2026-07-10-workflow-improvements-stage-2.md`.

### 2a. TOOL-16 — the fixture-staleness probe
**Deliverable**: `windows/lab/tests/fixture_staleness.rs` — regenerate the
first 3 seeds of each committed census live and compare (quantize-
canonicalized) against the committed `rows.csv`; fail with the `make
rebaseline` instruction. Also repairs `census_fixture_matches_live_run`,
which compared full-precision live rows to quantized fixture rows (broken
silently by the quantization epoch; CI never runs it).
**Status**: In Progress

### 2b. TOOL-20 — the golden-master accept harness
**Deliverable**: `hornvale_kernel::golden` (`assert_golden`/`check_golden`,
`REBASELINE=1` to accept) replacing the ad-hoc comparisons in
`cli/tests/lens_purity.rs`, `windows/scene/tests/golden.rs`, and
`windows/worldgen/tests/proto_goblinoid_golden.rs`; `make
rebaseline-goldens`. `scripts/freeze-fixture.sh` narrows to historical pins
(the `pre-*` fixtures are frozen history, never rebaselined) — a scoped
deviation from the registry row's "replace freeze-fixture.sh".
**Status**: Not Started

### 2c. TOOL-19 — the human-readable lab regression view
**Deliverable**: `hornvale lab diff <STUDY> <OLD_CSV> <NEW_CSV>` rendering
which metric moved and by how much (distribution deltas + numeric mean
shift); `make lab-diff STUDY=<name>` diffs the working tree against HEAD.
**Status**: Not Started

## Stage 3+ (later passes)

TOOL-17/22/23 are CI-topology changes. PROC-7/8/9 are new drift-checks
(host: `docs_consistency`). TOOL-18 and PROC-10 are standalone. TOOL-15's
exclude-glob relocation remains a follow-up.
**Status**: Not Started
```

- [ ] **Step 3: Verify the docs invariants**

Run: `cargo test -p hornvale --test docs_consistency 2>&1 | tee /tmp/hv-t1.txt`
Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add book/src/frontier/idea-registry.md WORKFLOW_IMPROVEMENTS_PLAN.md
git commit -m "docs(registry): flip the shipped Stage-1 workflow rows; open Stage 2

TOOL-14/15 and PROC-6 shipped in Stage 1 but their registry rows still
read raw; flip them and point Where at the shipped artifact. Record the
Stage-2 scope (TOOL-16/20/19) in the tracking plan.

Claude-Session: https://claude.ai/code/session_01STboWuJ5sz26RTjVwDkGQu"
```

---

### Task 2: TOOL-16 — the fixture-staleness probe

A developer who changes worldgen and runs only `cargo test` currently sees calibration pass against a stale census fixture until CI catches it (the one cost decision 0032 accepted knowingly). Close most of that gap with a ~seconds-cost always-on probe: regenerate the first 3 seeds live, compare against the fixture, fail with the regeneration instruction.

**Known latent bug this task also fixes:** `run()` returns full-precision `MetricValue::Number`s; quantization happens only at the `render_csv` emit boundary; `load_rows` returns quantized values. So the ignored guard `census_fixture_matches_live_run` (`windows/lab/tests/calibration.rs:46-68`), which asserts `loaded == live` and which CI does **not** run, has been silently broken since the quantization epoch. Both the new probe and the repaired guard must canonicalize live rows (quantize their Numbers) before comparing.

**Files:**
- Create: `windows/lab/tests/fixture_staleness.rs`
- Modify: `windows/lab/tests/calibration.rs:46-68` (the ignored guard)
- Modify: `book/src/laboratory/overview.md` (end of "The instrument's self-check" section)
- Modify: `book/src/frontier/idea-registry.md` (TOOL-16 row → shipped)

**Interfaces:**
- Consumes: `hornvale_lab::{MetricValue, Row, RunResult, load_rows, load_study, run}`, `hornvale_kernel::quantize` (all existing pub API; `hornvale-kernel` and `serde_json` are already dependencies of `hornvale-lab`, so integration tests may use them).
- Produces: nothing later tasks rely on.

- [ ] **Step 1: Write the probe with a plain (un-canonicalized) comparison first**

Create `windows/lab/tests/fixture_staleness.rs`:

```rust
//! Always-on staleness probe for the committed census fixtures (TOOL-16).
//!
//! Decision 0032 accepted one cost
//! knowingly: a developer who changes worldgen and runs only `cargo test`
//! sees calibration pass against the stale fixture until CI's artifact
//! drift check catches it. This probe closes most of that gap for a few
//! seconds' cost: it regenerates each census's first [`PROBE_SEEDS`] seeds
//! live and compares them, canonicalized, against the committed rows — so
//! a worldgen change that moves the census fails HERE, with the
//! regeneration instruction, not in CI an hour later.
//!
//! Not a replacement for the full ignored guard
//! (`census_fixture_matches_live_run`) or CI's regenerate-and-diff: a
//! change that only moves seeds ≥ [`PROBE_SEEDS`] slips past this probe
//! and is caught there.

use hornvale_kernel::quantize;
use hornvale_lab::{MetricValue, Row, RunResult, Study, load_rows, load_study, run};
use std::path::Path;

/// Seeds probed per census: 3 seeds × (1 + 2) pin sets ≈ 9 worlds ≈ a
/// couple of seconds — cheap enough for every workspace test run.
const PROBE_SEEDS: u64 = 3;

/// The two committed, CI-drift-checked censuses (decision
/// 0029).
const CENSUSES: [(&str, &str); 2] = [
    (
        "../../studies/census-lands-drift.study.json",
        "../../book/src/laboratory/generated/census-lands-drift/rows.csv",
    ),
    (
        "../../studies/census-of-the-meeting.study.json",
        "../../book/src/laboratory/generated/census-of-the-meeting/rows.csv",
    ),
];

/// Canonicalize a live row for comparison with a fixture row: fixture
/// floats passed through the serialization boundary (`render_csv`
/// quantizes — decision
/// 0033), so a
/// live full-precision Number must be quantized before equality holds.
fn canonical(row: &Row) -> Row {
    Row {
        seed: row.seed,
        pin_set: row.pin_set.clone(),
        values: row
            .values
            .iter()
            .map(|v| match v {
                MetricValue::Number(n) => MetricValue::Number(quantize(*n)),
                other => other.clone(),
            })
            .collect(),
        refusal: row.refusal.clone(),
    }
}

/// Compare every live row against its fixture counterpart, failing with
/// the actionable regeneration instruction on any mismatch.
fn assert_fixture_fresh(
    live: &RunResult,
    fixture: &RunResult,
    study_path: &str,
    rows_path: &str,
) {
    for row in &live.rows {
        let pinned = fixture
            .rows
            .iter()
            .find(|r| r.seed == row.seed && r.pin_set == row.pin_set)
            .unwrap_or_else(|| {
                panic!(
                    "census fixture {rows_path} has no row for seed {} / pin set '{}' — the \
                     fixture is stale or truncated; run `make rebaseline` (or `cargo run \
                     --release -p hornvale -- lab run {study_path}`) and commit the diff",
                    row.seed, row.pin_set
                )
            });
        assert_eq!(
            *pinned,
            canonical(row),
            "worldgen changed but the census fixture {rows_path} was not regenerated (seed {} \
             / pin set '{}' differs). Run `make rebaseline` (or `cargo run --release -p \
             hornvale -- lab run {study_path}`), review the diff, and commit it WITH the \
             change that moved it (decision 0032).",
            row.seed,
            row.pin_set
        );
    }
}

#[test]
fn census_fixtures_match_a_probe_of_live_seeds() {
    for (study_path, rows_path) in CENSUSES {
        let study = load_study(Path::new(study_path)).expect("load study");
        let csv = std::fs::read_to_string(rows_path).expect("read census fixture");
        let fixture = load_rows(&study, &csv).expect("reconstruct census from fixture");
        let mut mini = study.clone();
        mini.seeds.count = mini.seeds.count.min(PROBE_SEEDS);
        let live = run(&mini).expect("probe census");
        assert_fixture_fresh(&live, &fixture, study_path, rows_path);
    }
}

/// A tiny synthetic study/result pair for the probe's own rejection tests
/// (the PROC-6 pattern: the guard proves it rejects, not only that the
/// current tree passes). `ocean-fraction` is a real registry metric, so
/// the study validates.
fn synthetic(value: f64) -> RunResult {
    let study: Study = serde_json::from_str(
        r#"{
            "name": "probe-self-test",
            "description": "synthetic rows for the staleness probe's own tests",
            "seeds": { "from": 0, "count": 1 },
            "pin_sets": [ { "label": "default", "pins": [] } ],
            "metrics": ["ocean-fraction"]
        }"#,
    )
    .expect("valid study json");
    study.validate().expect("study validates");
    RunResult {
        study,
        metric_names: vec!["ocean-fraction"],
        rows: vec![Row {
            seed: 0,
            pin_set: "default".to_string(),
            values: vec![MetricValue::Number(value)],
            refusal: None,
        }],
    }
}

#[test]
fn a_stale_fixture_fails_with_the_regeneration_instruction() {
    let live = synthetic(0.42);
    let fixture = synthetic(0.43);
    let err = std::panic::catch_unwind(|| {
        assert_fixture_fresh(&live, &fixture, "studies/example.study.json", "rows.csv")
    })
    .expect_err("a diverging fixture must fail the probe");
    let msg = err
        .downcast_ref::<String>()
        .expect("panic payload is a String");
    assert!(
        msg.contains("make rebaseline"),
        "message must name the fix: {msg}"
    );
    assert!(
        msg.contains("was not regenerated"),
        "message must name the cause: {msg}"
    );
}

#[test]
fn a_full_precision_live_value_matches_its_quantized_fixture() {
    // What `run()` produces vs what `load_rows()` reconstructs: the probe
    // must canonicalize, or every numeric metric is a false staleness.
    let raw = 0.123_456_789_012_345;
    let live = synthetic(raw);
    let fixture = synthetic(quantize(raw));
    assert_fixture_fresh(&live, &fixture, "s.json", "rows.csv");
}

#[test]
fn a_missing_fixture_row_is_reported_as_truncated() {
    let live = synthetic(0.42);
    let mut fixture = synthetic(0.42);
    fixture.rows.clear();
    let err = std::panic::catch_unwind(|| {
        assert_fixture_fresh(&live, &fixture, "s.json", "rows.csv")
    })
    .expect_err("a missing row must fail the probe");
    let msg = err
        .downcast_ref::<String>()
        .expect("panic payload is a String");
    assert!(msg.contains("stale or truncated"), "got: {msg}");
}
```

- [ ] **Step 2: Prove the quantization gap is real (red), then confirm green**

First run only the canonicalization self-test with `canonical()` temporarily bypassed — simplest honest red: in `assert_fixture_fresh`, compare `*pinned == row.clone()` instead of `canonical(row)`, and run:

`cargo test -p hornvale-lab --test fixture_staleness 2>&1 | tee /tmp/hv-t2-red.txt`

Expected: `a_full_precision_live_value_matches_its_quantized_fixture` FAILS (and likely `census_fixtures_match_a_probe_of_live_seeds` fails on the first numeric metric) — this is the evidence the quantization gap exists. Restore `canonical(row)` and re-run:

`cargo test -p hornvale-lab --test fixture_staleness 2>&1 | tee /tmp/hv-t2-green.txt`

Expected: all 4 tests PASS. Note the wall-clock of the probe test in the output; it must be seconds, not minutes (it builds ~9 worlds).

- [ ] **Step 3: Repair the ignored full guard the same way**

In `windows/lab/tests/calibration.rs`, the guard at lines 46–68 compares `loaded == live` directly. Replace the body's comparison (keep the `#[ignore = "runs the full ~145s census; fixtures are drift-checked in CI"]` attribute — the PROC-6 guard sanctions that exact reason string):

```rust
#[test]
#[ignore = "runs the full ~145s census; fixtures are drift-checked in CI"]
fn census_fixture_matches_live_run() {
    for (study_path, rows_path) in [
        (
            "../../studies/census-lands-drift.study.json",
            "../../book/src/laboratory/generated/census-lands-drift/rows.csv",
        ),
        (
            "../../studies/census-of-the-meeting.study.json",
            "../../book/src/laboratory/generated/census-of-the-meeting/rows.csv",
        ),
    ] {
        let study = load_study(Path::new(study_path)).expect("load study");
        let live = run(&study).expect("run study");
        // Canonicalize live Numbers before comparing: the fixture's floats
        // passed the quantizing serialization boundary (`render_csv`), the
        // live run's have not. Without this the guard fails on every
        // numeric metric's last ULPs — a false alarm the quantization
        // epoch introduced.
        let live = RunResult {
            study: live.study.clone(),
            metric_names: live.metric_names.clone(),
            rows: live
                .rows
                .iter()
                .map(|row| Row {
                    seed: row.seed,
                    pin_set: row.pin_set.clone(),
                    values: row
                        .values
                        .iter()
                        .map(|v| match v {
                            MetricValue::Number(n) => {
                                MetricValue::Number(hornvale_kernel::quantize(*n))
                            }
                            other => other.clone(),
                        })
                        .collect(),
                    refusal: row.refusal.clone(),
                })
                .collect(),
        };
        let csv = std::fs::read_to_string(rows_path).expect("read census fixture");
        let loaded = load_rows(&study, &csv).expect("reconstruct census from fixture");
        assert_eq!(
            loaded, live,
            "fixture {rows_path} diverged from a live run — regenerate it with `lab run`"
        );
    }
}
```

Add `Row` to the existing `use hornvale_lab::{...}` import in that file.

- [ ] **Step 4: Run the repaired full guard once to verify (the ~145s run)**

Run: `cargo test -p hornvale-lab --test calibration -- --ignored 2>&1 | tee /tmp/hv-t2-guard.txt`
Expected: PASS (~145s — run it exactly once; inspect `/tmp/hv-t2-guard.txt` for anything else).
Also run the non-ignored calibrations once: `cargo test -p hornvale-lab 2>&1 | tee /tmp/hv-t2-lab.txt` — expected PASS (this also re-runs the preregistration guard over the edited calibration file).

- [ ] **Step 5: Book + registry**

In `book/src/laboratory/overview.md`, append to the end of "The instrument's self-check" section (after the census-of-the-meeting paragraph, before the `{{#include ...}}` line, keeping a blank line on each side):

```markdown
Between commits, a cheap always-on probe (`windows/lab/tests/fixture_staleness.rs`)
regenerates each census's first three seeds live on every `cargo test` and
compares them against the committed rows — so a worldgen change that moves
the census fails locally with the regeneration instruction (`make
rebaseline`) instead of surfacing an hour later in CI's full
regenerate-and-diff.
```

In `book/src/frontier/idea-registry.md`, flip the TOOL-16 row: `| raw | med | ideonomy session (workflow/tooling) |` → `| shipped | med | ideonomy session (workflow/tooling); \`windows/lab/tests/fixture_staleness.rs\` |`.

Update `WORKFLOW_IMPROVEMENTS_PLAN.md` 2a status: `In Progress` → `Complete`, and 2b to `In Progress`.

- [ ] **Step 6: Gate and commit**

```bash
make quick
cargo test -p hornvale --test docs_consistency
cargo fmt
git add windows/lab/tests/fixture_staleness.rs windows/lab/tests/calibration.rs \
    book/src/laboratory/overview.md book/src/frontier/idea-registry.md \
    WORKFLOW_IMPROVEMENTS_PLAN.md
git commit -m "test(lab): probe census fixtures for staleness with three live seeds (TOOL-16)

Encode the 'cheap unless you changed worldgen' rule as a gate: regenerate
each committed census's first three seeds on every test run and compare
quantize-canonicalized rows against the fixture, failing with the make
rebaseline instruction. Also repair census_fixture_matches_live_run, which
compared full-precision live Numbers to quantized fixture Numbers and has
failed silently since the quantization epoch (CI never runs it).

Claude-Session: https://claude.ai/code/session_01STboWuJ5sz26RTjVwDkGQu"
```

---

### Task 3: TOOL-20 — the golden-master accept harness

Three test sites hand-roll byte-golden comparison (`cli/tests/lens_purity.rs`, `windows/scene/tests/golden.rs`, `windows/worldgen/tests/proto_goblinoid_golden.rs`), each with its own regeneration ritual. Centralize behind one std-only helper in the kernel with an explicit `REBASELINE=1` accept path.

**Scope boundary (deliberate deviation from the registry row):** the `pre-*` fixtures in `cli/tests/fixtures/` (used by `branches_identity.rs`) are *frozen historical pins* — their bytes must never track current code, so they get no REBASELINE path and stay out of this harness. `scripts/freeze-fixture.sh` therefore survives, narrowed to that historical-pin role, rather than being deleted.

**Files:**
- Create: `kernel/src/golden.rs`
- Create: `kernel/tests/golden_harness.rs`
- Modify: `kernel/src/lib.rs` (add `pub mod golden;` after line 6's mod block start, alphabetical: between `pub mod geosphere;` and `pub mod ledger;`)
- Modify: `cli/tests/lens_purity.rs`
- Modify: `windows/scene/tests/golden.rs`
- Modify: `windows/worldgen/tests/proto_goblinoid_golden.rs`
- Modify: `Makefile` (new `rebaseline-goldens` target)
- Modify: `scripts/freeze-fixture.sh` (header note narrowing its role)
- Modify: `docs/audits/type-audit-report.md` (regenerated)
- Modify: `book/src/frontier/idea-registry.md` (TOOL-20 row → shipped)

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `hornvale_kernel::golden::assert_golden(path: &Path, actual: &str, context: &str)` (panics on mismatch; `REBASELINE` env accepts), `hornvale_kernel::golden::check_golden(path: &Path, actual: &str, rebaseline: bool) -> Result<GoldenOutcome, GoldenMismatch>` (the testable core), `enum GoldenOutcome { Match, Rewritten, Created }`, `struct GoldenMismatch { message: String }`.

- [ ] **Step 1: Write the failing integration tests**

Create `kernel/tests/golden_harness.rs`:

```rust
//! Integration tests for the golden-master accept harness (TOOL-20).
//! These exercise `check_golden` (the env-free core) directly so no test
//! ever mutates process-global environment variables.

use hornvale_kernel::golden::{GoldenOutcome, check_golden};
use std::path::PathBuf;

/// A scratch path under cargo's per-target test tmpdir.
fn scratch(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(name)
}

#[test]
fn matching_bytes_pass() {
    let path = scratch("match.txt");
    std::fs::write(&path, "alpha\n").unwrap();
    assert_eq!(
        check_golden(&path, "alpha\n", false),
        Ok(GoldenOutcome::Match)
    );
}

#[test]
fn a_mismatch_reports_the_first_divergence_and_the_accept_instruction() {
    let path = scratch("mismatch.txt");
    std::fs::write(&path, "alpha\nbeta\n").unwrap();
    let err = check_golden(&path, "alpha\ngamma\n", false)
        .expect_err("differing bytes must be a mismatch");
    assert!(err.message.contains("line 2"), "got: {}", err.message);
    assert!(err.message.contains("beta"), "got: {}", err.message);
    assert!(err.message.contains("gamma"), "got: {}", err.message);
    assert!(
        err.message.contains("REBASELINE=1"),
        "the accept instruction must be in the message: {}",
        err.message
    );
    assert!(
        err.message.contains("mismatch.txt"),
        "the fixture path must be in the message: {}",
        err.message
    );
    // A rejected mismatch must never modify the fixture.
    assert_eq!(std::fs::read_to_string(&path).unwrap(), "alpha\nbeta\n");
}

#[test]
fn rebaseline_rewrites_a_drifted_fixture() {
    let path = scratch("rewrite.txt");
    std::fs::write(&path, "old\n").unwrap();
    assert_eq!(
        check_golden(&path, "new\n", true),
        Ok(GoldenOutcome::Rewritten)
    );
    assert_eq!(std::fs::read_to_string(&path).unwrap(), "new\n");
}

#[test]
fn rebaseline_on_matching_bytes_is_a_no_op_match() {
    let path = scratch("noop.txt");
    std::fs::write(&path, "same\n").unwrap();
    assert_eq!(
        check_golden(&path, "same\n", true),
        Ok(GoldenOutcome::Match)
    );
}

#[test]
fn rebaseline_creates_a_missing_fixture_and_its_parents() {
    let path = scratch("created/nested/fixture.txt");
    let _ = std::fs::remove_file(&path);
    assert_eq!(
        check_golden(&path, "fresh\n", true),
        Ok(GoldenOutcome::Created)
    );
    assert_eq!(std::fs::read_to_string(&path).unwrap(), "fresh\n");
}

#[test]
fn a_missing_fixture_without_rebaseline_says_how_to_create_it() {
    let path = scratch("never-written.txt");
    let _ = std::fs::remove_file(&path);
    let err = check_golden(&path, "x", false).expect_err("missing fixture must fail");
    assert!(
        err.message.contains("REBASELINE=1"),
        "got: {}",
        err.message
    );
}

#[test]
fn a_trailing_length_difference_is_still_a_mismatch() {
    let path = scratch("prefix.txt");
    std::fs::write(&path, "alpha\n").unwrap();
    let err = check_golden(&path, "alpha\nextra\n", false)
        .expect_err("extra trailing content must be a mismatch");
    assert!(err.message.contains("line 2"), "got: {}", err.message);
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-kernel --test golden_harness 2>&1 | tee /tmp/hv-t3-red.txt`
Expected: COMPILE ERROR — `hornvale_kernel::golden` does not exist.

- [ ] **Step 3: Implement the module**

Create `kernel/src/golden.rs`:

```rust
//! Byte-golden comparison with an explicit accept path (TOOL-20).
//!
//! Every committed golden fixture in the workspace is compared through
//! [`assert_golden`]; a drift is accepted deliberately by re-running the
//! failing test with `REBASELINE=1` (or `make rebaseline-goldens` for all
//! of them) and then reviewing the resulting `git diff` — a snapshot is a
//! migration, and accepting it is a reviewed migration. Dev/test support
//! only: nothing in any sim or generation path may call this module (the
//! `REBASELINE` environment read is deliberate non-determinism that must
//! never reach world output).
//!
//! Frozen *historical* pins (the `pre-<campaign>` fixtures under
//! `cli/tests/fixtures/`) are NOT goldens: their bytes must never track
//! current code, so they are compared directly and have no accept path.

use std::fmt;
use std::fs;
use std::path::Path;

/// A golden mismatch: what differed, where, and how to accept it.
/// type-audit: bare-ok(prose: message)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GoldenMismatch {
    /// Human-readable report: fixture path, first divergence, accept instruction.
    pub message: String,
}

impl fmt::Display for GoldenMismatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for GoldenMismatch {}

/// What a golden check did.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GoldenOutcome {
    /// `actual` matched the committed fixture byte-for-byte.
    Match,
    /// Rebaseline mode rewrote a drifted fixture; review the diff.
    Rewritten,
    /// Rebaseline mode created a missing fixture; review before committing.
    Created,
}

/// The testable, env-free core of [`assert_golden`]: compare `actual`
/// against the fixture at `path`; with `rebaseline` set, accept drift by
/// rewriting (or creating) the fixture instead of failing.
/// type-audit: bare-ok(artifact: actual), bare-ok(flag: rebaseline)
pub fn check_golden(
    path: &Path,
    actual: &str,
    rebaseline: bool,
) -> Result<GoldenOutcome, GoldenMismatch> {
    match fs::read_to_string(path) {
        Ok(expected) if expected == actual => Ok(GoldenOutcome::Match),
        Ok(expected) => {
            if rebaseline {
                write_fixture(path, actual)?;
                Ok(GoldenOutcome::Rewritten)
            } else {
                Err(GoldenMismatch {
                    message: mismatch_report(path, &expected, actual),
                })
            }
        }
        Err(_) => {
            if rebaseline {
                write_fixture(path, actual)?;
                Ok(GoldenOutcome::Created)
            } else {
                Err(GoldenMismatch {
                    message: format!(
                        "golden: no fixture at {} — create it deliberately by re-running \
                         this test with REBASELINE=1, then review and commit the new file",
                        path.display()
                    ),
                })
            }
        }
    }
}

/// Assert `actual` matches the committed golden at `path` byte-for-byte.
///
/// On mismatch, panics with the first diverging line, the caller's
/// `context` (the domain-specific "what drifting means here" guidance),
/// and the accept instruction. Setting the `REBASELINE` environment
/// variable (any value but empty or `0`) accepts instead: the fixture is
/// rewritten and the test passes, leaving the diff for review.
/// type-audit: bare-ok(artifact: actual), bare-ok(prose: context)
pub fn assert_golden(path: &Path, actual: &str, context: &str) {
    let rebaseline = std::env::var_os("REBASELINE").is_some_and(|v| !v.is_empty() && v != "0");
    match check_golden(path, actual, rebaseline) {
        Ok(GoldenOutcome::Match) => {}
        Ok(GoldenOutcome::Rewritten) => eprintln!(
            "golden: REBASELINE rewrote {} — review the diff before committing",
            path.display()
        ),
        Ok(GoldenOutcome::Created) => eprintln!(
            "golden: REBASELINE created {} — review it before committing",
            path.display()
        ),
        Err(mismatch) => panic!("{mismatch}\n{context}"),
    }
}

/// Write an accepted fixture, creating parent directories as needed.
fn write_fixture(path: &Path, actual: &str) -> Result<(), GoldenMismatch> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|e| GoldenMismatch {
            message: format!("golden: cannot create {}: {e}", parent.display()),
        })?;
    }
    fs::write(path, actual).map_err(|e| GoldenMismatch {
        message: format!("golden: cannot write {}: {e}", path.display()),
    })
}

/// Render a compact mismatch report: the fixture path, the first diverging
/// line pair (truncated to keep multi-megabyte fixtures out of the panic),
/// byte lengths, and the accept instruction.
fn mismatch_report(path: &Path, expected: &str, actual: &str) -> String {
    let mut line = 1usize;
    let mut expected_lines = expected.lines();
    let mut actual_lines = actual.lines();
    let (exp_line, act_line) = loop {
        match (expected_lines.next(), actual_lines.next()) {
            (Some(e), Some(a)) if e == a => line += 1,
            (e, a) => break (e, a),
        }
    };
    let show = |l: Option<&str>| match l {
        Some(text) => truncate(text),
        None => "<end of file>".to_string(),
    };
    format!(
        "golden mismatch: {}\nfirst divergence at line {line}:\n  committed: {}\n  actual:    {}\n({} vs {} bytes) — accept deliberately: re-run with REBASELINE=1 (or `make rebaseline-goldens`), then review the diff before committing",
        path.display(),
        show(exp_line),
        show(act_line),
        expected.len(),
        actual.len()
    )
}

/// Truncate a line to 120 chars for the report (fixtures can be one
/// enormous line of JSON).
fn truncate(s: &str) -> String {
    if s.chars().count() > 120 {
        let head: String = s.chars().take(120).collect();
        format!("{head}…")
    } else {
        s.to_string()
    }
}
```

In `kernel/src/lib.rs`, add between `pub mod geosphere;` and `pub mod ledger;`:

```rust
pub mod golden;
```

(No `pub use` re-export: call sites read better fully qualified as `golden::assert_golden`, and the module is dev-support, not sim API.)

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-kernel --test golden_harness 2>&1 | tee /tmp/hv-t3-green.txt`
Expected: 7 tests PASS. Also run kernel's unit suite once: `cargo test -p hornvale-kernel 2>&1 | tee /tmp/hv-t3-kernel.txt` — PASS.

- [ ] **Step 5: Convert the three call sites**

`cli/tests/lens_purity.rs` — replace lines 25–30 (the `include_str!` + `assert_eq!`) with:

```rust
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/world-seed-42.json"
        )),
        &world.to_json(),
        "world identity drifted from the committed fixture — see this file's module doc",
    );
```

Also update that file's module doc sentence "regenerate the fixture in the same commit" to name the new mechanism: "regenerate the fixture in the same commit (`REBASELINE=1 cargo test -p hornvale --test lens_purity`, or `make rebaseline-goldens`) and record why in the chronicle."

`windows/scene/tests/golden.rs` — replace the whole file's test section (keep the `world()`/`seed_1_json()` helpers unchanged) with:

```rust
#[test]
fn v1_bytes_are_pinned() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/tiles-seed-1-w16.json"
        )),
        &seed_1_json(),
        "scene/tiles/v1 bytes moved — this is the epoch decision point (scene-protocol \
         spec §2); accept deliberately and review the diff as a contract change",
    );
}
```

Delete the `regenerate_golden` ignored test entirely (REBASELINE replaces it) and update the module doc's regeneration instruction from `cargo test -p hornvale-scene --test golden -- --ignored` to `REBASELINE=1 cargo test -p hornvale-scene --test golden` (or `make rebaseline-goldens`).

`windows/worldgen/tests/proto_goblinoid_golden.rs` — replace the two `assert_eq!(render_..., include_str!(...), "...")` bodies with (keep each test's existing message string verbatim as the `context` argument):

```rust
#[test]
fn proto_goblinoid_inventory_matches_the_committed_snapshot() {
    let world = reference_world();
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/proto-goblinoid-inventory-seed-42.txt"
        )),
        &render_inventory_snapshot(&world),
        "proto-goblinoid's inventory drifted from the committed snapshot — see this file's \
         module doc: every daughter (goblin/hobgoblin/bugbear) nativizes from this exact \
         inventory, so this is the shared-ancestor diamond dependency moving. If deliberate, \
         regenerate the fixture in this commit and record why in the chronicle.",
    );
}

#[test]
fn proto_goblinoid_root_table_matches_the_committed_snapshot() {
    let world = reference_world();
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/proto-goblinoid-root-table-seed-42.txt"
        )),
        &render_root_table_snapshot(&world),
        "proto-goblinoid's proto-root table drifted from the committed snapshot — see this \
         file's module doc: every daughter's cognate forms are evolved from these exact \
         proto-roots, so this is the shared-ancestor diamond dependency moving. If deliberate, \
         regenerate the fixture in this commit and record why in the chronicle.",
    );
}
```

(`hornvale_kernel` is already a dependency of all three crates; the scene test file already uses it.)

- [ ] **Step 6: Run the three converted suites once each**

```bash
cargo test -p hornvale --test lens_purity 2>&1 | tee /tmp/hv-t3-lens.txt
cargo test -p hornvale-scene --test golden 2>&1 | tee /tmp/hv-t3-scene.txt
cargo test -p hornvale-worldgen --test proto_goblinoid_golden 2>&1 | tee /tmp/hv-t3-proto.txt
```

Expected: all PASS against the existing committed fixtures (byte-identical behavior; nothing regenerated).

- [ ] **Step 7: Makefile target + freeze-fixture note**

In `Makefile`, add `rebaseline-goldens` to the `.PHONY` line and this target after `rebaseline artifacts`:

```make
rebaseline-goldens: ## Accept drifted byte-golden test fixtures (REBASELINE=1), then review the diff
	REBASELINE=1 cargo test -q -p hornvale --test lens_purity
	REBASELINE=1 cargo test -q -p hornvale-scene --test golden
	REBASELINE=1 cargo test -q -p hornvale-worldgen --test proto_goblinoid_golden
```

Run `make rebaseline-goldens` once; expected: all pass AND `git status --short` shows no fixture modified (a clean tree proves accept-mode is a no-op when nothing drifted).

In `scripts/freeze-fixture.sh`, extend the header comment (after the "Usage" examples, before `set -euo pipefail`):

```bash
# Scope (TOOL-20): living byte-goldens (world-seed-42.json, the scene tiles
# pin, the proto-goblinoid snapshots) are accepted via REBASELINE=1 — see
# `make rebaseline-goldens` and kernel/src/golden.rs. This script remains
# for freezing *historical* pins (the pre-<campaign> fixtures), whose bytes
# must never track current code and so must never grow an accept path.
```

Run `shellcheck scripts/freeze-fixture.sh`; expected: clean.

- [ ] **Step 8: Type audit + registry**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
```

Expected: PASS (the new pub items carry `type-audit:` tags). If it flags an untagged boundary, add the missing verdict tag per `docs/decisions/0028-the-bare-ok-rubric.md` — do not waive without reason. Then regenerate the committed report:

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
```

In `book/src/frontier/idea-registry.md`, flip the TOOL-20 row: `| raw | med | ideonomy session (workflow/tooling) |` → `| shipped | med | ideonomy session (workflow/tooling); \`kernel/src/golden.rs\` + \`make rebaseline-goldens\` (freeze-fixture.sh retained for historical pins) |`.

Update `WORKFLOW_IMPROVEMENTS_PLAN.md` 2b status: → `Complete`, 2c → `In Progress`.

- [ ] **Step 9: Gate and commit**

```bash
make quick
cargo test -p hornvale --test docs_consistency
cargo fmt
git add kernel/src/golden.rs kernel/src/lib.rs kernel/tests/golden_harness.rs \
    cli/tests/lens_purity.rs windows/scene/tests/golden.rs \
    windows/worldgen/tests/proto_goblinoid_golden.rs Makefile \
    scripts/freeze-fixture.sh docs/audits/type-audit-report.md \
    book/src/frontier/idea-registry.md WORKFLOW_IMPROVEMENTS_PLAN.md
git commit -m "feat(kernel): golden-master accept harness behind REBASELINE=1 (TOOL-20)

One std-only helper (hornvale_kernel::golden) now backs every living
byte-golden: lens_purity's world pin, scene/tiles/v1, and the two
proto-goblinoid snapshots. Accepting a drift is an explicit act —
REBASELINE=1 (or make rebaseline-goldens) rewrites the fixture and the
git diff is the review surface. The pre-<campaign> fixtures are frozen
history, deliberately excluded; freeze-fixture.sh narrows to that role.

Claude-Session: https://claude.ai/code/session_01STboWuJ5sz26RTjVwDkGQu"
```

---

### Task 4: TOOL-19 — the human-readable lab regression view

When a census `rows.csv` moves, the reviewable surface today is a wall of quantized CSV. Add `hornvale lab diff <STUDY> <OLD_CSV> <NEW_CSV>`: a markdown report of which metric × pin-set distributions moved and by how much, plus `make lab-diff STUDY=<name>` to diff the working tree against HEAD.

**Files:**
- Create: `windows/lab/src/diff.rs`
- Modify: `windows/lab/src/lib.rs` (add `pub mod diff;` after `pub mod chart;`, and `pub use diff::{render_diff, render_diff_results};` after the `chart` re-export)
- Modify: `windows/lab/src/summary.rs:13` (`pub(crate) fn distribution` stays `pub(crate)` — `diff.rs` is in-crate; no change needed, listed for awareness)
- Modify: `cli/src/main.rs` (usage string ~line 51, `cmd_lab` dispatch ~line 617, new `cmd_lab_diff`, usage test ~line 860)
- Modify: `Makefile` (new `lab-diff` target)
- Modify: `book/src/laboratory/overview.md` (one sentence after the Task-2 probe paragraph)
- Modify: `book/src/frontier/idea-registry.md` (TOOL-19 row → shipped)

**Interfaces:**
- Consumes: `crate::summary::distribution` (`pub(crate) fn distribution(kind: &SummaryKind, values: &[&MetricValue]) -> Vec<(String, u64)>`), `crate::{MetricValue, RunResult, Study, StudyError, SummaryKind, load_rows}`, `hornvale_kernel::quantize`.
- Produces: `hornvale_lab::render_diff(study: &Study, old_csv: &str, new_csv: &str) -> Result<String, StudyError>` and `hornvale_lab::render_diff_results(old: &RunResult, new: &RunResult) -> String`.

- [ ] **Step 1: Write the failing unit tests**

Create `windows/lab/src/diff.rs` with the module doc, an empty stub, and the tests (TDD: tests first, stub fails):

```rust
//! Human-readable diff of two census snapshots: which metric moved, and by
//! how much (TOOL-19).
//!
//! The review-surface companion to the CI drift check: when a study's
//! committed `rows.csv` changes, `hornvale lab diff` (and `make lab-diff
//! STUDY=<name>`) renders per-metric distribution deltas and numeric mean
//! shifts instead of a wall of quantized CSV. Deterministic: pin sets in
//! study order, metrics in registry order, floats displayed through the
//! kernel's `quantize`.

use crate::summary::distribution;
use crate::{MetricValue, RunResult, Study, StudyError, SummaryKind, load_rows};
use hornvale_kernel::quantize;
use std::collections::BTreeMap;

/// Render a markdown report of which metrics moved between two `rows.csv`
/// snapshots of the same study, and by how much.
/// type-audit: bare-ok(artifact: old_csv), bare-ok(artifact: new_csv), bare-ok(artifact: return)
pub fn render_diff(study: &Study, old_csv: &str, new_csv: &str) -> Result<String, StudyError> {
    Ok(render_diff_results(
        &load_rows(study, old_csv)?,
        &load_rows(study, new_csv)?,
    ))
}

/// The [`render_diff`] core over already-loaded results (separated so tests
/// and future callers can diff without re-parsing CSV).
/// type-audit: bare-ok(artifact: return)
pub fn render_diff_results(old: &RunResult, new: &RunResult) -> String {
    todo!()
}
```

Then the tests, in the same file:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::Row;

    /// Two-seed study over one Categorical and one Numeric metric, with
    /// per-test value overrides. `star-class` is Categorical and
    /// `ocean-fraction` is Numeric in the registry.
    fn result_with(star: [&str; 2], ocean: [f64; 2]) -> RunResult {
        let study: Study = serde_json::from_str(
            r#"{
                "name": "diff-test",
                "description": "diff unit-test study",
                "seeds": { "from": 0, "count": 2 },
                "pin_sets": [ { "label": "default", "pins": [] } ],
                "metrics": ["star-class", "ocean-fraction"]
            }"#,
        )
        .unwrap();
        study.validate().unwrap();
        RunResult {
            study,
            metric_names: vec!["star-class", "ocean-fraction"],
            rows: (0..2usize)
                .map(|i| Row {
                    seed: i as u64,
                    pin_set: "default".to_string(),
                    values: vec![
                        MetricValue::Text(star[i].to_string()),
                        MetricValue::Number(ocean[i]),
                    ],
                    refusal: None,
                })
                .collect(),
        }
    }

    #[test]
    fn identical_results_report_no_movement() {
        let a = result_with(["K", "M"], [0.5, 0.7]);
        let b = result_with(["K", "M"], [0.5, 0.7]);
        let report = render_diff_results(&a, &b);
        assert!(report.contains("No metric moved."), "got:\n{report}");
        assert!(!report.contains("###"), "no per-metric sections:\n{report}");
    }

    #[test]
    fn a_categorical_shift_shows_the_changed_labels_with_deltas() {
        let old = result_with(["K", "M"], [0.5, 0.7]);
        let new = result_with(["K", "K"], [0.5, 0.7]);
        let report = render_diff_results(&old, &new);
        assert!(
            report.contains("### star-class — default"),
            "got:\n{report}"
        );
        assert!(report.contains("| K | 1 | 2 | +1 |"), "got:\n{report}");
        assert!(report.contains("| M | 1 | 0 | -1 |"), "got:\n{report}");
        // The numeric metric did not move — no section for it.
        assert!(
            !report.contains("### ocean-fraction — default"),
            "got:\n{report}"
        );
        assert!(
            report.contains("1 of 2 metric × pin-set distributions moved."),
            "got:\n{report}"
        );
    }

    #[test]
    fn a_numeric_shift_shows_the_mean_movement() {
        let old = result_with(["K", "M"], [0.50, 0.70]);
        let new = result_with(["K", "M"], [0.50, 0.80]);
        let report = render_diff_results(&old, &new);
        assert!(
            report.contains("### ocean-fraction — default"),
            "got:\n{report}"
        );
        // mean 0.6 → 0.65, quantized display, signed delta.
        assert!(report.contains("mean 0.6 → 0.65"), "got:\n{report}");
        assert!(report.contains("Δ +0.05"), "got:\n{report}");
    }

    #[test]
    fn header_reports_row_and_refusal_counts() {
        let old = result_with(["K", "M"], [0.5, 0.7]);
        let mut new = result_with(["K", "M"], [0.5, 0.7]);
        new.rows[1].refusal = Some("pin refused".to_string());
        new.rows[1].values = vec![MetricValue::Absent, MetricValue::Absent];
        let report = render_diff_results(&old, &new);
        assert!(report.contains("Rows 2 → 2; refusals 0 → 1."), "got:\n{report}");
    }

    #[test]
    fn csv_round_trip_diffs_through_render_diff() {
        let old = result_with(["K", "M"], [0.5, 0.7]);
        let new = result_with(["K", "K"], [0.5, 0.7]);
        let old_csv = crate::runner::render_csv(&old);
        let new_csv = crate::runner::render_csv(&new);
        let report = render_diff(&old.study, &old_csv, &new_csv).unwrap();
        assert!(report.contains("| K | 1 | 2 | +1 |"), "got:\n{report}");
    }
}
```

Note: `render_csv` is `pub(crate)` in `runner.rs` — reachable from this in-crate test as `crate::runner::render_csv`.

- [ ] **Step 2: Run tests to verify they fail**

Add `pub mod diff;` and the re-export to `windows/lab/src/lib.rs` first (the module must compile to run), then:

Run: `cargo test -p hornvale-lab diff 2>&1 | tee /tmp/hv-t4-red.txt`
Expected: FAIL — the `todo!()` panics.

- [ ] **Step 3: Implement `render_diff_results`**

Replace the `todo!()` body:

```rust
pub fn render_diff_results(old: &RunResult, new: &RunResult) -> String {
    let mut out = String::new();
    out.push_str(&format!("## Lab diff: {}\n\n", new.study.name));

    let old_refusals = old.rows.iter().filter(|r| r.refusal.is_some()).count();
    let new_refusals = new.rows.iter().filter(|r| r.refusal.is_some()).count();
    out.push_str(&format!(
        "Rows {} → {}; refusals {} → {}.\n\n",
        old.rows.len(),
        new.rows.len(),
        old_refusals,
        new_refusals
    ));

    let metrics = new
        .study
        .selected_metrics()
        .expect("the study validated at load time");
    let mut moved_sections: Vec<String> = Vec::new();
    let mut pair_count = 0usize;

    for pin_set in &new.study.pin_sets {
        let old_rows: Vec<_> = old
            .rows
            .iter()
            .filter(|r| r.pin_set == pin_set.label)
            .collect();
        let new_rows: Vec<_> = new
            .rows
            .iter()
            .filter(|r| r.pin_set == pin_set.label)
            .collect();
        for (idx, name) in new.metric_names.iter().enumerate() {
            pair_count += 1;
            let metric = metrics
                .iter()
                .find(|m| m.name == *name)
                .expect("metric_names come from the registry");
            let old_values: Vec<&MetricValue> =
                old_rows.iter().map(|r| &r.values[idx]).collect();
            let new_values: Vec<&MetricValue> =
                new_rows.iter().map(|r| &r.values[idx]).collect();
            let dist_old = distribution(&metric.summary, &old_values);
            let dist_new = distribution(&metric.summary, &new_values);
            let (old_mean, new_mean) = match metric.summary {
                SummaryKind::Numeric { .. } => (mean_of(&old_values), mean_of(&new_values)),
                _ => (None, None),
            };
            let dist_moved = dist_old != dist_new;
            let mean_moved = old_mean != new_mean;
            if !dist_moved && !mean_moved {
                continue;
            }

            let mut section = format!("### {} — {}\n\n", name, pin_set.label);
            if dist_moved {
                section.push_str("| value | old | new | Δ |\n|---|---|---|---|\n");
                for (label, o, n) in merged_counts(&dist_old, &dist_new) {
                    if o != n {
                        section.push_str(&format!(
                            "| {} | {} | {} | {:+} |\n",
                            label,
                            o,
                            n,
                            n as i64 - o as i64
                        ));
                    }
                }
                section.push('\n');
            }
            if let (Some(o), Some(n)) = (old_mean, new_mean) {
                section.push_str(&format!(
                    "mean {} → {} (Δ {:+})\n\n",
                    quantize(o),
                    quantize(n),
                    quantize(n - o)
                ));
            }
            moved_sections.push(section);
        }
    }

    if moved_sections.is_empty() {
        out.push_str("No metric moved.\n");
    } else {
        out.push_str(&format!(
            "{} of {} metric × pin-set distributions moved.\n\n",
            moved_sections.len(),
            pair_count
        ));
        for section in moved_sections {
            out.push_str(&section);
        }
    }
    out
}

/// Mean of the present numeric values, or `None` when none are present.
fn mean_of(values: &[&MetricValue]) -> Option<f64> {
    let numbers: Vec<f64> = values
        .iter()
        .filter_map(|v| match v {
            MetricValue::Number(n) => Some(*n),
            _ => None,
        })
        .collect();
    if numbers.is_empty() {
        None
    } else {
        Some(numbers.iter().sum::<f64>() / numbers.len() as f64)
    }
}

/// Merge two `(label, count)` distributions into `(label, old, new)`,
/// keeping the new distribution's order and appending old-only labels at
/// the end (both inputs arrive already deterministically ordered from
/// [`distribution`]).
fn merged_counts(old: &[(String, u64)], new: &[(String, u64)]) -> Vec<(String, u64, u64)> {
    let old_map: BTreeMap<&str, u64> = old.iter().map(|(l, c)| (l.as_str(), *c)).collect();
    let mut merged: Vec<(String, u64, u64)> = new
        .iter()
        .map(|(l, c)| (l.clone(), old_map.get(l.as_str()).copied().unwrap_or(0), *c))
        .collect();
    for (label, count) in old {
        if !new.iter().any(|(l, _)| l == label) {
            merged.push((label.clone(), *count, 0));
        }
    }
    merged
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-lab 2>&1 | tee /tmp/hv-t4-green.txt`
Expected: PASS (the diff tests plus the whole lab suite, including Task 2's probe). If the `mean 0.6 → 0.65` assertion fails on float display (e.g. `0.6000000000000001`), the fix is already in the code — `quantize` canonicalizes display — so a failure there means a real bug; investigate, don't loosen the assertion.

- [ ] **Step 5: Wire the CLI**

In `cli/src/main.rs`:

1. Usage string — after the `hornvale lab run <PATH>` line, add:

```text
  hornvale lab diff <STUDY> <OLD_CSV> <NEW_CSV>  report which census metrics moved between two rows.csv snapshots
```

2. `cmd_lab` dispatch — add the arm and update the no-subcommand message:

```rust
/// Dispatch `lab` subcommands: `run <PATH>`, `diff <STUDY> <OLD_CSV> <NEW_CSV>`,
/// and `list-metrics`.
fn cmd_lab(args: &[String]) -> Result<(), String> {
    match args.get(1).map(String::as_str) {
        Some("run") => cmd_lab_run(args),
        Some("diff") => cmd_lab_diff(args),
        Some("list-metrics") => cmd_lab_list_metrics(),
        Some(other) => Err(format!("lab: unknown subcommand '{other}'\n{}", usage())),
        None => Err(format!(
            "lab: requires a subcommand (run <PATH>|diff <STUDY> <OLD_CSV> <NEW_CSV>|list-metrics)\n{}",
            usage()
        )),
    }
}
```

3. New handler after `cmd_lab_run`:

```rust
/// Diff two `rows.csv` snapshots of one study: which metric moved, and by
/// how much. Old/new are paths to CSV files (typically `git show
/// HEAD:<...>/rows.csv` output vs the working tree's copy — `make lab-diff
/// STUDY=<name>` wraps exactly that).
fn cmd_lab_diff(args: &[String]) -> Result<(), String> {
    let (Some(study_path), Some(old_path), Some(new_path)) =
        (args.get(2), args.get(3), args.get(4))
    else {
        return Err(format!(
            "lab diff requires <STUDY> <OLD_CSV> <NEW_CSV>\n{}",
            usage()
        ));
    };
    let study = hornvale_lab::load_study(std::path::Path::new(study_path))
        .map_err(|e| e.to_string())?;
    let old_csv =
        std::fs::read_to_string(old_path).map_err(|e| format!("read {old_path}: {e}"))?;
    let new_csv =
        std::fs::read_to_string(new_path).map_err(|e| format!("read {new_path}: {e}"))?;
    print!(
        "{}",
        hornvale_lab::render_diff(&study, &old_csv, &new_csv).map_err(|e| e.to_string())?
    );
    Ok(())
}
```

4. Usage test at ~line 860 — extend:

```rust
    fn usage_mentions_lab() {
        assert!(USAGE.contains("lab run"));
        assert!(USAGE.contains("lab diff"));
        assert!(USAGE.contains("list-metrics"));
    }
```

- [ ] **Step 6: Smoke the CLI end-to-end against a real census**

```bash
cargo test -p hornvale usage 2>&1 | tee /tmp/hv-t4-usage.txt
git show HEAD:book/src/laboratory/generated/census-lands-drift/rows.csv > /tmp/hv-old-rows.csv
cargo run -q -p hornvale -- lab diff studies/census-lands-drift.study.json \
    /tmp/hv-old-rows.csv book/src/laboratory/generated/census-lands-drift/rows.csv
```

Expected: usage tests PASS; the diff of the unchanged census prints `## Lab diff: census-lands-drift`, the Rows line, and `No metric moved.`

- [ ] **Step 7: Makefile target + book + registry**

`Makefile` — add `lab-diff` to `.PHONY` and after `rebaseline-goldens`:

```make
lab-diff: ## Report which census metrics moved vs HEAD (usage: make lab-diff STUDY=census-lands-drift)
	@test -n "$(STUDY)" || { echo "usage: make lab-diff STUDY=<study-name>"; exit 2; }
	@old="$$(mktemp)"; \
	git show HEAD:book/src/laboratory/generated/$(STUDY)/rows.csv > "$$old"; \
	cargo run -q -p hornvale -- lab diff studies/$(STUDY).study.json "$$old" \
	    book/src/laboratory/generated/$(STUDY)/rows.csv; \
	status=$$?; rm -f "$$old"; exit $$status
```

Run `make lab-diff STUDY=census-lands-drift`; expected: `No metric moved.`

`book/src/laboratory/overview.md` — append to the self-check section, directly after Task 2's probe paragraph:

```markdown
When a census *does* move, the reviewable surface is `make lab-diff
STUDY=<name>` (wrapping `hornvale lab diff`): a per-metric report of which
distribution moved and by how much — count deltas per value bucket and the
numeric mean shift — rather than a raw CSV diff.
```

Registry — flip the TOOL-19 row: `| raw | med | ideonomy session (workflow/tooling) |` → `| shipped | med | ideonomy session (workflow/tooling); \`windows/lab/src/diff.rs\` + \`hornvale lab diff\` |`.

Update `WORKFLOW_IMPROVEMENTS_PLAN.md` 2c status → `Complete`.

- [ ] **Step 8: Gate and commit**

```bash
make quick
cargo test -p hornvale-lab 2>&1 | tee /tmp/hv-t4-lab-final.txt
cargo test -p hornvale --test docs_consistency
cargo fmt
git add windows/lab/src/diff.rs windows/lab/src/lib.rs cli/src/main.rs Makefile \
    book/src/laboratory/overview.md book/src/frontier/idea-registry.md \
    WORKFLOW_IMPROVEMENTS_PLAN.md
git commit -m "feat(lab): render which census metrics moved and by how much (TOOL-19)

hornvale lab diff <STUDY> <OLD_CSV> <NEW_CSV> (and make lab-diff
STUDY=<name>, diffing the working tree against HEAD) summarizes a moved
rows.csv as per-metric distribution deltas plus numeric mean shifts — the
review-surface companion to the CI drift check, replacing the raw wall of
quantized CSV.

Claude-Session: https://claude.ai/code/session_01STboWuJ5sz26RTjVwDkGQu"
```

---

### Task 5: Close the stage

**Files:**
- Modify: `WORKFLOW_IMPROVEMENTS_PLAN.md` (Stage 2 header note: complete)

**Interfaces:** none.

- [ ] **Step 1: Run the full gate once**

Run: `make gate 2>&1 | tee /tmp/hv-t5-gate.txt`
Expected: fmt clean, clippy clean, full workspace suite PASS (this includes the new probe, harness, and diff tests). Inspect the tee'd file; do not re-run.

- [ ] **Step 2: Mark Stage 2 complete and commit**

In `WORKFLOW_IMPROVEMENTS_PLAN.md`, change the Stage 2 `**Goal**` line's section heading from `(this pass)` context by adding at the end of the Stage 2 intro: `All three landed; see the plan file for details.` and ensure all three sub-statuses read `Complete`.

```bash
git add WORKFLOW_IMPROVEMENTS_PLAN.md
git commit -m "docs(plan): close Workflow Improvements Stage 2 (TOOL-16/19/20)

Claude-Session: https://claude.ai/code/session_01STboWuJ5sz26RTjVwDkGQu"
```

Then use superpowers:finishing-a-development-branch to integrate (merge to main after checking for foreign WIP on `windows/lab` — the homophony campaign may have landed commits there; refreeze/re-run the lab suite after any merge conflict resolution).
