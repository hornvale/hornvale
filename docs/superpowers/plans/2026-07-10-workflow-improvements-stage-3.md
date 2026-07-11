# Workflow Improvements Stage 3 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land the drift-checks-and-orientation pass: the five Stage-2 deferred follow-ups, PROC-8's decision-cite linter, PROC-9's enforcer-emitted layering page, and PROC-10's `make doctor` self-map.

**Architecture:** Task 1 resolves the Stage-2 review deferrals (shared `canonical_row` helper in `hornvale_lab`, timing-accuracy sweep of the "~145s" lore, a decision cite, a mean-only diff test, a fail-fast `lab-diff` recipe). PROC-8 adds a `decision NNNN` / `ADR NNNN` / `decision <slug>` citation resolver to `cli/tests/docs_consistency.rs`. PROC-9 renders the dependency graph from `architecture.rs`'s own `cargo metadata` parse and drift-checks it into the book via the Stage-2 golden harness (`assert_golden` + `REBASELINE=1`). PROC-10 is a read-only `scripts/doctor.sh` of pointers to authoritative sources plus an orphaned-decision scan.

**Tech Stack:** Rust edition 2024, std + serde/serde_json only, `make`, bash (shellcheck-clean), mdBook.

## Global Constraints

Every task's requirements implicitly include all of these (from CLAUDE.md and the decision log):

- Dependencies: `serde` + `serde_json` only, workspace-wide. No new crates.
- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only (clippy `disallowed-types`). No wall-clock time.
- Every crate sets `#![warn(missing_docs)]`; every new public item gets a one-line doc comment; primitives at `pub` boundaries carry `type-audit:` verdict tags, and any pub-surface change regenerates `docs/audits/type-audit-report.md` in the same commit (`cargo run --manifest-path tools/type-audit/Cargo.toml -- check`, then `-- report > docs/audits/type-audit-report.md`).
- Cost-ordered gate per commit: `make quick` first, then scoped `cargo test -p <crate>`; the full `make gate` is the final pre-merge step only. Run suites ONCE, tee'd (`2>&1 | tee /tmp/<name>.txt`); never re-run to grep a second line. `cargo fmt` as the last step before each commit.
- **The book's registry-ID ban:** `docs_consistency`'s `the_book_carries_no_registry_ids_or_process_vocabulary` test fails if any registry ID (`PROC-…`, `TOOL-…`, …) appears in `book/src/` outside `book/src/frontier/`. The new layering pages (Task 3) must therefore never mention registry IDs — cite the mechanism, not the row.
- Registry discipline: a shipped idea's row flips Status to `shipped` in the shipping commit; only Status/Where cells change. Verify with `cargo test -p hornvale --test docs_consistency`.
- Commit messages end with:
  `Claude-Session: https://claude.ai/code/session_01STboWuJ5sz26RTjVwDkGQu`
- **Parallel-work caution:** another campaign owns `windows/lab/src/metrics.rs` and `IMPLEMENTATION_PLAN.md` — never touch either. Keep `windows/lab/src/lib.rs` edits additive.
- Do NOT re-run the ~450s ignored calibration guard in this stage; nothing in it changes semantically (Task 1 edits only its doc/reason strings).

---

### Task 1: Stage-2 deferred follow-ups + open Stage 3 in the tracking plan

Five small resolved-by-review items from Stage 2's final review, one commit.

**Files:**
- Modify: `windows/lab/src/runner.rs` (new `canonical_row` + unit test)
- Modify: `windows/lab/src/lib.rs` (re-export)
- Modify: `windows/lab/tests/fixture_staleness.rs` (use helper; add decision cite to the missing-row panic)
- Modify: `windows/lab/tests/calibration.rs` (use helper; timing strings at lines ~12, ~41, ~47)
- Modify: `windows/lab/tests/preregistration_guard.rs` (timing strings at lines ~13, ~146–147)
- Modify: `CLAUDE.md` (line ~28 timing)
- Modify: `windows/lab/src/diff.rs` (one new unit test)
- Modify: `Makefile` (`lab-diff` recipe fail-fast)
- Modify: `WORKFLOW_IMPROVEMENTS_PLAN.md` (Stage 3 section)

**Interfaces:**
- Consumes: `hornvale_kernel::quantize`, existing `Row`/`RunResult`/`MetricValue`.
- Produces: `hornvale_lab::canonical_row(row: &Row) -> Row` (re-exported from `runner`), used by both lab test files.

- [ ] **Step 1: Write the failing unit test for `canonical_row`**

In `windows/lab/src/runner.rs`, inside the existing `#[cfg(test)] mod tests`, add:

```rust
    #[test]
    fn canonical_row_quantizes_numbers_and_preserves_everything_else() {
        let row = Row {
            seed: 7,
            pin_set: "default".to_string(),
            values: vec![
                MetricValue::Number(0.123_456_789_012_345),
                MetricValue::Text("kept".to_string()),
                MetricValue::Flag(true),
                MetricValue::Absent,
            ],
            refusal: Some("kept too".to_string()),
        };
        let canon = canonical_row(&row);
        assert_eq!(
            canon.values[0],
            MetricValue::Number(hornvale_kernel::quantize(0.123_456_789_012_345))
        );
        assert_eq!(canon.values[1..], row.values[1..]);
        assert_eq!((canon.seed, &canon.pin_set, &canon.refusal), (7, &row.pin_set, &row.refusal));
    }
```

(If the tests module lacks `MetricValue` in scope, its existing `use super::*;` / `use crate::…` imports already cover it — check the module head and extend only if the compiler asks.)

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-lab canonical_row 2>&1 | tee /tmp/hv-s3t1-red.txt`
Expected: COMPILE ERROR — `canonical_row` not found.

- [ ] **Step 3: Implement the helper**

In `windows/lab/src/runner.rs`, directly after the `Row` struct definition:

```rust
/// Canonicalize a row for comparison with fixture-loaded rows: quantize
/// `Number` values exactly as [`render_csv`] does at the serialization
/// boundary, so a full-precision live row compares equal to its committed,
/// quantized counterpart (decision
/// `serialized-floats-are-quantized-for-cross-platform-determinism`).
pub fn canonical_row(row: &Row) -> Row {
    Row {
        seed: row.seed,
        pin_set: row.pin_set.clone(),
        values: row
            .values
            .iter()
            .map(|v| match v {
                MetricValue::Number(n) => MetricValue::Number(hornvale_kernel::quantize(*n)),
                other => other.clone(),
            })
            .collect(),
        refusal: row.refusal.clone(),
    }
}
```

In `windows/lab/src/lib.rs`, extend the runner re-export line to:

```rust
pub use runner::{Row, RunResult, canonical_row, load_rows, run, write_csv};
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-lab canonical_row 2>&1 | tee /tmp/hv-s3t1-green.txt`
Expected: PASS.

- [ ] **Step 5: Swap both test files onto the helper**

`windows/lab/tests/fixture_staleness.rs`:
- Delete the local `fn canonical(row: &Row) -> Row { … }` (the whole function and its doc comment).
- Change the import line to `use hornvale_lab::{MetricValue, Row, RunResult, Study, canonical_row, load_rows, load_study, run};` (keep `use hornvale_kernel::quantize;` — the self-test still calls it).
- In `assert_fixture_fresh`, replace `canonical(row)` with `canonical_row(row)`.
- In the missing-row panic (the `unwrap_or_else` whose message ends "and commit the diff"), append the decision cite so it ends: `and commit the diff (decision calibration-loads-the-census-fixture)`.

`windows/lab/tests/calibration.rs`, in `census_fixture_matches_live_run`:
- Replace the inline canonicalizing rebuild of `live` with:

```rust
        // Canonicalize live Numbers before comparing: the fixture's floats
        // passed the quantizing serialization boundary (`render_csv`), the
        // live run's have not (shared helper: `hornvale_lab::canonical_row`).
        let live = RunResult {
            study: live.study.clone(),
            metric_names: live.metric_names.clone(),
            rows: live.rows.iter().map(canonical_row).collect(),
        };
```

- Update the file's `use hornvale_lab::{…}` line: add `canonical_row`; remove `Row` if the compiler now reports it unused.

- [ ] **Step 6: The timing-accuracy sweep (~145s → measured reality)**

The ignored guard measured ~450s under the test (debug) profile in Stage 2; sweep the stale number in the four places that assert it as fact (leave the two generic "a \"~145s\" style timing" pattern examples in `preregistration_guard.rs` lines ~35/~48 alone — they describe the regex, not the census):

1. `windows/lab/tests/calibration.rs` line ~47:
   `#[ignore = "runs the full ~450s (debug) census; fixtures are drift-checked in CI"]`
2. `windows/lab/tests/calibration.rs` line ~41 doc: change `(~145s)` to `(~450s under the test profile; release regeneration via make rebaseline is much faster)`.
3. `windows/lab/tests/calibration.rs` line ~12 doc: `keeps the ~145s census off` → `keeps the ~450s (debug) census off`.
4. `windows/lab/tests/preregistration_guard.rs` line ~146 comment and line ~147 sample: update the quoted sample string to exactly the new reason from (1); line ~13 doc `the ~145s census` → `the ~450s census`.
5. `CLAUDE.md` line ~28: `#   The ~145s calibration census (windows/lab) loads a drift-checked fixture` → `#   The calibration census (windows/lab; ~450s live in debug) loads a drift-checked fixture`.

The new reason string stays sanctioned by construction: `reason_is_sanctioned` accepts a digit-followed-by-`s` timing ("450s") and the word "census".

- [ ] **Step 7: The mean-only diff test**

First check `ocean-fraction`'s `bucket_edges` in `windows/lab/src/metrics.rs` (read only — do not modify that file): the test below assumes 0.50 and 0.55 fall in the same bucket ([0.5, 0.6) with 0.1-wide edges). If the edges differ, pick two values inside one bucket and recompute the means. Then add to `windows/lab/src/diff.rs`'s `mod tests`:

```rust
    #[test]
    fn a_mean_only_shift_renders_the_mean_line_without_a_table() {
        // 0.50 → 0.55 stays inside one histogram bucket, so the distribution
        // is unchanged while the mean moves: the section must render with
        // the mean line only — no distribution table.
        let old = result_with(["K", "M"], [0.50, 0.70]);
        let new = result_with(["K", "M"], [0.55, 0.70]);
        let report = render_diff_results(&old, &new);
        assert!(
            report.contains("### ocean-fraction — default"),
            "got:\n{report}"
        );
        assert!(report.contains("mean 0.6 → 0.625"), "got:\n{report}");
        assert!(report.contains("Δ +0.025"), "got:\n{report}");
        assert!(
            !report.contains("| value | old | new | Δ |"),
            "no distribution table when only the mean moved:\n{report}"
        );
        assert!(
            report.contains("1 of 2 metric × pin-set distributions moved."),
            "got:\n{report}"
        );
    }
```

This is a coverage-gap fill: expected PASS immediately (the code path exists). A FAIL here is a real rendering bug — investigate, don't loosen.

- [ ] **Step 8: Fail-fast `lab-diff` recipe**

Replace the `lab-diff` recipe body in `Makefile` with:

```make
lab-diff: ## Report which census metrics moved vs HEAD (usage: make lab-diff STUDY=census-lands-drift)
	@test -n "$(STUDY)" || { echo "usage: make lab-diff STUDY=<study-name>"; exit 2; }
	@old="$$(mktemp)"; \
	if ! git show HEAD:book/src/laboratory/generated/$(STUDY)/rows.csv > "$$old" 2>/dev/null; then \
	    rm -f "$$old"; \
	    echo "lab-diff: no committed rows.csv for study '$(STUDY)' at HEAD (check the name under book/src/laboratory/generated/)"; \
	    exit 2; \
	fi; \
	cargo run -q -p hornvale -- lab diff studies/$(STUDY).study.json "$$old" \
	    book/src/laboratory/generated/$(STUDY)/rows.csv; \
	status=$$?; rm -f "$$old"; exit $$status
```

Verify both paths, once each:
`make lab-diff STUDY=census-lands-drift` → ends with `No metric moved.`
`make lab-diff STUDY=no-such-study` → prints the `lab-diff: no committed rows.csv…` line, exit code 2 (check with `echo $?`), and no cargo/git stack noise.

- [ ] **Step 9: Open Stage 3 in `WORKFLOW_IMPROVEMENTS_PLAN.md`**

Change the deferred-follow-ups block's intro line `Small, non-blocking; fold into a later pass:` to `All five resolved in Stage 3a:` (list stays as the record of what they were). Then replace the `## Stage 4+`-to-be section — currently:

```markdown
## Stage 3+ (later passes)

TOOL-17/22/23 are CI-topology changes. PROC-7/8/9 are new drift-checks
(host: `docs_consistency`). TOOL-18 and PROC-10 are standalone. TOOL-15's
exclude-glob relocation remains a follow-up.
**Status**: Not Started
```

with:

```markdown
## Stage 3: Drift-checks and orientation (this pass)

**Goal**: the Stage-2 deferred follow-ups plus the three locally-verifiable
process rows. Plan:
`docs/superpowers/plans/2026-07-10-workflow-improvements-stage-3.md`.

### 3a. Stage-2 deferred follow-ups
**Deliverable**: shared `canonical_row` in `hornvale_lab`; timing-accuracy
sweep of the "~145s" claims (measured ~450s debug); decision cite in the
probe's missing-row panic; a mean-only-movement diff test; fail-fast
`make lab-diff`.
**Status**: Complete

### 3b. PROC-8 — the decision-log consistency linter
**Deliverable**: `docs_consistency` resolves every `decision NNNN` /
`ADR NNNN` / hyphenated `decision <slug>` cite in `.rs`/`.sh` sources
against `docs/decisions/`. The book-staleness half is deferred: mtimes do
not survive git checkout; it needs a git-log-based freshness design.
**Status**: Not Started

### 3c. PROC-9 — the layering page emitted from its enforcer
**Deliverable**: `cli/tests/architecture.rs` renders the enforced graph to
`book/src/reference/layering-generated.md`, golden-checked (REBASELINE=1
accepts); wrapper chapter + SUMMARY entry.
**Status**: Not Started

### 3d. PROC-10 — `make doctor`, the repo self-map
**Deliverable**: `scripts/doctor.sh` printing pointers to the authoritative
orientation sources (layering, gate, determinism contracts, artifact
provenance, doc map, live git state) plus an orphaned-decision scan.
**Status**: Not Started

## Stage 4+ (later passes)

TOOL-17/21/22/23 are CI-topology changes needing CI iteration. PROC-7 (the
mechanical DoD gate) is CI-shaped too. TOOL-18 is sim-facing test policy
(fairness batteries) — spec it deliberately. PROC-8's book-staleness scan
needs the git-log design. TOOL-15's exclude-glob relocation remains.
**Status**: Not Started
```

- [ ] **Step 10: Gate and commit**

```bash
make quick
cargo test -p hornvale-lab 2>&1 | tee /tmp/hv-s3t1-lab.txt
cargo test -p hornvale --test docs_consistency 2>&1 | tee /tmp/hv-s3t1-docs.txt
cargo fmt
git add windows/lab/src/runner.rs windows/lab/src/lib.rs windows/lab/src/diff.rs \
    windows/lab/tests/fixture_staleness.rs windows/lab/tests/calibration.rs \
    windows/lab/tests/preregistration_guard.rs CLAUDE.md Makefile \
    WORKFLOW_IMPROVEMENTS_PLAN.md
git commit -m "chore(lab): resolve Stage 2's deferred review follow-ups

Shared canonical_row helper replaces the duplicated quantize-canonicalize
in the staleness probe and the ignored guard; the ~145s census lore is
re-pinned at the measured ~450s (debug); the probe's missing-row panic
cites its decision; a mean-only movement diff test closes the coverage
gap; make lab-diff fails fast with the real cause on an unknown study.

Claude-Session: https://claude.ai/code/session_01STboWuJ5sz26RTjVwDkGQu"
```

(Expected in `/tmp/hv-s3t1-lab.txt`: the whole lab suite green including `fixture_staleness` (4 tests), `preregistration_guard`, and the new diff/runner tests; the ~450s guard stays ignored.)

---

### Task 2: PROC-8 — the decision-log consistency linter

Every decision cite in Rust and shell sources must resolve to a record in `docs/decisions/`. Numeric cites (`decision 0014`, `decisions 0002`, `ADR 0016`) resolve against `NNNN-*.md`; slug cites (`decision calibration-loads-the-census-fixture`) against `<slug>.md` or a numbered record's `NNNN-<slug>.md` tail. Hyphen-poor tokens ("decision log", "decision point") are prose, not cites — the slug form requires ≥2 hyphens.

**Files:**
- Modify: `cli/tests/docs_consistency.rs` (append helpers + tests at end of file)
- Modify: `book/src/frontier/idea-registry.md` (PROC-8 row → shipped)
- Modify: `WORKFLOW_IMPROVEMENTS_PLAN.md` (3b → Complete, 3c → In Progress)

**Interfaces:**
- Consumes: the file's existing `repo_root()` and `read()` helpers, `std::collections::BTreeSet` (already imported).
- Produces: nothing later tasks rely on.

- [ ] **Step 1: Write the resolver unit tests (failing)**

Append to `cli/tests/docs_consistency.rs`:

```rust
#[test]
fn cite_error_resolves_the_known_forms() {
    let numbers: BTreeSet<String> = ["0016".to_string()].into();
    let slugs: BTreeSet<String> =
        ["calibration-loads-the-census-fixture".to_string(), "slugs-not-numbers".to_string()]
            .into();
    // Resolvable numeric and slug cites.
    assert_eq!(cite_error("0016", &numbers, &slugs), None);
    assert_eq!(
        cite_error("calibration-loads-the-census-fixture", &numbers, &slugs),
        None
    );
    // A numbered record's slug tail resolves too.
    assert_eq!(cite_error("slugs-not-numbers", &numbers, &slugs), None);
    // Unresolvable cites are errors.
    assert!(cite_error("0999", &numbers, &slugs).is_some());
    assert!(cite_error("no-such-decision-here", &numbers, &slugs).is_some());
    // Prose, not cites: hyphen-free words and short hyphenations.
    assert_eq!(cite_error("log", &numbers, &slugs), None);
    assert_eq!(cite_error("point", &numbers, &slugs), None);
    assert_eq!(cite_error("4-digit", &numbers, &slugs), None);
    assert_eq!(cite_error("", &numbers, &slugs), None);
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale --test docs_consistency cite_error 2>&1 | tee /tmp/hv-s3t2-red.txt`
Expected: COMPILE ERROR — `cite_error` not found.

- [ ] **Step 3: Implement the resolver, the walker, and the main test**

Append to `cli/tests/docs_consistency.rs` (above the unit test from Step 1):

```rust
/// Recursively collect `.rs` and `.sh` files under `dir`, skipping build
/// output and hidden directories (the source-side companion to [`md_files`]).
fn source_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries {
        let path = entry.expect("dir entry").path();
        let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
        if path.is_dir() {
            if name != "target" && !name.starts_with('.') {
                source_files(&path, out);
            }
        } else if path.extension().is_some_and(|e| e == "rs" || e == "sh") {
            out.push(path);
        }
    }
}

/// Check one cite token against the decision records. `None` means fine —
/// either it resolves, or it is prose rather than a cite: a 4-digit token
/// must match a `NNNN-*.md` record; a lowercase token with ≥ 2 hyphens must
/// match a `<slug>.md` record or a numbered record's slug tail ("decision
/// log" has no hyphen, "4-digit" has one — neither is a cite).
fn cite_error(
    token: &str,
    numbers: &BTreeSet<String>,
    slugs: &BTreeSet<String>,
) -> Option<String> {
    if token.len() == 4 && token.chars().all(|c| c.is_ascii_digit()) {
        return (!numbers.contains(token))
            .then(|| format!("no docs/decisions/{token}-*.md record"));
    }
    let is_slug_shaped = token.matches('-').count() >= 2
        && !token.starts_with('-')
        && !token.ends_with('-')
        && token
            .chars()
            .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-');
    if is_slug_shaped && !slugs.contains(token) {
        return Some(format!("no docs/decisions/{token}.md record"));
    }
    None
}

/// Every decision citation in the Rust and shell sources resolves to a
/// record in `docs/decisions/` — the decision-log half of the knowledge-base
/// drift linters. Forms: `decision 0014` / `decisions 0002` / `ADR 0016`
/// (numeric) and `decision <slug>` (hyphenated slug, optionally backticked).
#[test]
fn decision_cites_in_sources_resolve() {
    let root = repo_root();
    let mut numbers = BTreeSet::new();
    let mut slugs = BTreeSet::new();
    for entry in fs::read_dir(root.join("docs/decisions")).expect("decisions dir") {
        let name = entry.expect("dir entry").file_name();
        let name = name.to_string_lossy();
        let Some(stem) = name.strip_suffix(".md") else {
            continue;
        };
        let numbered = stem.len() > 5
            && stem.as_bytes()[4] == b'-'
            && stem[..4].chars().all(|c| c.is_ascii_digit());
        if numbered {
            numbers.insert(stem[..4].to_string());
            slugs.insert(stem[5..].to_string());
        } else {
            slugs.insert(stem.to_string());
        }
    }

    let mut files = Vec::new();
    for dir in ["kernel", "domains", "windows", "cli", "tools", "scripts"] {
        source_files(&root.join(dir), &mut files);
    }
    files.sort();

    let mut errors = Vec::new();
    for file in &files {
        let content = read(file);
        let rel = file.strip_prefix(&root).unwrap_or(file).display().to_string();
        for (idx, line) in content.lines().enumerate() {
            for keyword in ["decision ", "decisions ", "ADR "] {
                let mut rest = line;
                while let Some(pos) = rest.find(keyword) {
                    let after = &rest[pos + keyword.len()..];
                    let token: String = after
                        .trim_start_matches('`')
                        .chars()
                        .take_while(|c| c.is_ascii_alphanumeric() || *c == '-')
                        .collect();
                    if let Some(err) = cite_error(&token, &numbers, &slugs) {
                        errors.push(format!(
                            "{rel}:{}: cite `{keyword}{token}` — {err}",
                            idx + 1
                        ));
                    }
                    rest = &rest[pos + keyword.len()..];
                }
            }
        }
    }
    assert!(
        errors.is_empty(),
        "decision cites that resolve to no record (fix the cite, or add the \
         missing record to docs/decisions/):\n  {}",
        errors.join("\n  ")
    );
}
```

- [ ] **Step 4: Run to verify both tests pass on the real tree**

Run: `cargo test -p hornvale --test docs_consistency 2>&1 | tee /tmp/hv-s3t2-green.txt`
Expected: all tests PASS (now 7). If `decision_cites_in_sources_resolve` reports a genuinely broken cite in the tree, fix the *cite* (typo → correct record) in the same commit and note it in your report; if it flags prose (a false positive), tighten `cite_error`'s shape rules instead — never weaken the resolving check itself.

- [ ] **Step 5: Registry + plan bookkeeping**

`book/src/frontier/idea-registry.md`, PROC-8 row: `| raw | med | ideonomy session (workflow/tooling) |` → `| shipped | med | ideonomy session (workflow/tooling); \`cli/tests/docs_consistency.rs\` decision-cite check (book-staleness scan deferred: mtimes do not survive checkout; needs a git-log design) |` (literal backticks in the file).

`WORKFLOW_IMPROVEMENTS_PLAN.md`: 3b → `Complete`, 3c → `In Progress`.

- [ ] **Step 6: Gate and commit**

```bash
make quick
cargo fmt
git add cli/tests/docs_consistency.rs book/src/frontier/idea-registry.md WORKFLOW_IMPROVEMENTS_PLAN.md
git commit -m "test(docs): resolve every decision cite in sources against the log (PROC-8)

The decision-log half of the knowledge-base drift linters: decision NNNN /
decisions NNNN / ADR NNNN and hyphenated slug cites in .rs/.sh sources must
name a real record in docs/decisions/. The book-staleness half is deferred
pending a git-log-based freshness design (mtimes do not survive checkout).

Claude-Session: https://claude.ai/code/session_01STboWuJ5sz26RTjVwDkGQu"
```

---

### Task 3: PROC-9 — the layering page emitted from its enforcer

`cli/tests/architecture.rs` already parses the workspace graph via `cargo metadata`; render that same data as the book's layering page and drift-check it with the Stage-2 golden harness, so the picture can never rot out of sync with the enforced graph.

**IMPORTANT — the registry-ID ban:** neither `book/src/reference/layering.md` nor the generated file may contain any registry ID (`PROC-9`, `TOOL-20`, …); `docs_consistency` fails the build if one appears. Cite mechanisms (file paths, decision numbers), never rows.

**Files:**
- Modify: `cli/tests/architecture.rs` (renderer + two tests appended)
- Create: `book/src/reference/layering-generated.md` (via `REBASELINE=1`)
- Create: `book/src/reference/layering.md`
- Modify: `book/src/SUMMARY.md` (one line, after the Proto-goblinoid entry at ~line 102)
- Modify: `Makefile` (one line added to `rebaseline-goldens`)
- Modify: `book/src/frontier/idea-registry.md` (PROC-9 row → shipped)
- Modify: `WORKFLOW_IMPROVEMENTS_PLAN.md` (3c → Complete, 3d → In Progress)

**Interfaces:**
- Consumes: `architecture.rs`'s existing `Package { name, layer, normal_deps, all_deps }` and `workspace()`; `hornvale_kernel::golden::assert_golden(path, actual, context)` from Stage 2.
- Produces: `book/src/reference/layering-generated.md`, regenerated by `REBASELINE=1 cargo test -p hornvale --test architecture` (wired into `make rebaseline-goldens`).

- [ ] **Step 1: Append the renderer and tests to `cli/tests/architecture.rs`**

```rust
/// Sort rank of a layer along the constitutional chain.
fn layer_rank(layer: &str) -> usize {
    match layer {
        "kernel" => 0,
        "domains" => 1,
        "windows" => 2,
        "cli" => 3,
        other => panic!("unknown layer '{other}' — extend the chain deliberately"),
    }
}

/// Render the enforced dependency graph as the book's generated layering
/// page: the layer chain plus one row per crate, from the same
/// `cargo metadata` truth the assertions above enforce. Deterministic:
/// crates ordered by (layer, name), dependency lists sorted.
fn render_layering(packages: &[Package]) -> String {
    let internal: BTreeSet<&str> = packages.iter().map(|p| p.name.as_str()).collect();
    let mut sorted: Vec<&Package> = packages.iter().collect();
    sorted.sort_by(|a, b| {
        layer_rank(&a.layer)
            .cmp(&layer_rank(&b.layer))
            .then_with(|| a.name.cmp(&b.name))
    });
    let fmt_deps = |deps: &[&str]| {
        if deps.is_empty() {
            "—".to_string()
        } else {
            deps.join(", ")
        }
    };
    let mut out = String::from(
        "<!-- GENERATED FILE — do not edit. Emitted and drift-checked by \
         cli/tests/architecture.rs (the layering enforcer); accept a deliberate \
         graph change with REBASELINE=1 (make rebaseline-goldens). -->\n\n",
    );
    out.push_str("```text\nkernel  →  domains/*  →  windows/*  →  cli\n```\n\n");
    out.push_str("| crate | layer | workspace dependencies | dev/build-only extras |\n");
    out.push_str("|---|---|---|---|\n");
    for pkg in sorted {
        let mut normal: Vec<&str> = pkg
            .normal_deps
            .iter()
            .map(String::as_str)
            .filter(|d| internal.contains(*d))
            .collect();
        normal.sort_unstable();
        normal.dedup();
        let mut extras: Vec<&str> = pkg
            .all_deps
            .iter()
            .map(String::as_str)
            .filter(|d| internal.contains(*d) && !normal.contains(d))
            .collect();
        extras.sort_unstable();
        extras.dedup();
        out.push_str(&format!(
            "| {} | {} | {} | {} |\n",
            pkg.name,
            pkg.layer,
            fmt_deps(&normal),
            fmt_deps(&extras)
        ));
    }
    out
}

#[test]
fn the_layering_render_is_deterministic_and_grounded() {
    let packages = workspace();
    let a = render_layering(&packages);
    assert_eq!(a, render_layering(&packages));
    assert!(
        a.contains("| hornvale-kernel | kernel | — | — |"),
        "the kernel row must show no workspace dependencies:\n{a}"
    );
}

#[test]
fn the_layering_page_matches_the_enforced_graph() {
    hornvale_kernel::golden::assert_golden(
        Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../book/src/reference/layering-generated.md"
        )),
        &render_layering(&workspace()),
        "the book's layering page drifted from the enforced dependency graph — if the \
         workspace graph changed deliberately, accept with REBASELINE=1 (or `make \
         rebaseline-goldens`) and review the diff as an architecture change; the picture \
         is authored by the enforcer, never by hand",
    );
}
```

Also add `use std::path::Path;` to the file's imports (it currently imports only collections and process).

- [ ] **Step 2: Run to verify the golden test fails (no fixture yet)**

Run: `cargo test -p hornvale --test architecture 2>&1 | tee /tmp/hv-s3t3-red.txt`
Expected: `the_layering_page_matches_the_enforced_graph` FAILS with "golden: no fixture at … create it deliberately … REBASELINE=1"; the determinism test and the four enforcement tests PASS.

- [ ] **Step 3: Create the fixture deliberately, inspect it**

Run: `REBASELINE=1 cargo test -p hornvale --test architecture 2>&1 | tee /tmp/hv-s3t3-create.txt`
Expected: all PASS, stderr notes the created file. Then read `book/src/reference/layering-generated.md` and verify: the header comment, the chain code block, and one table row per workspace crate ordered kernel → domains (alphabetical) → windows (alphabetical) → cli; `hornvale-lab`'s row should show `hornvale-worldgen` among its dependencies (the window-on-window edge) and the domains rows exactly `hornvale-kernel`. Confirm no registry ID appears anywhere in it.

- [ ] **Step 4: The wrapper chapter and SUMMARY entry**

Create `book/src/reference/layering.md`:

```markdown
# The Layering

The constitutional dependency rule — `kernel` → `domains/*` → `windows/*` →
`cli` — is enforced by `cli/tests/architecture.rs` (decisions 0002 and
0004): a domain crate depends on the kernel and nothing else, never another
domain; windows may depend on domains and other windows because they present
them; the CLI sits on top; and only `serde`/`serde_json` cross the workspace
boundary from outside. The picture below is emitted by that same enforcer
and drift-checked against it on every test run, so what you read here is
exactly what the tests assert — the diagram cannot rot out of sync with the
graph it depicts.

{{#include layering-generated.md}}
```

In `book/src/SUMMARY.md`, after the `- [Proto-goblinoid](./reference/proto-goblinoid-generated.md)` line, add:

```markdown
- [The Layering](./reference/layering.md)
```

- [ ] **Step 5: Wire regeneration into `make rebaseline-goldens`**

Add as the last line of the `rebaseline-goldens` recipe in `Makefile`:

```make
	REBASELINE=1 cargo test -q -p hornvale --test architecture
```

- [ ] **Step 6: Verify the whole surface once**

```bash
cargo test -p hornvale --test architecture 2>&1 | tee /tmp/hv-s3t3-green.txt
cargo test -p hornvale --test docs_consistency 2>&1 | tee /tmp/hv-s3t3-docs.txt
command -v mdbook >/dev/null && mdbook build book 2>&1 | tail -3 || echo "mdbook not installed; book.yml CI builds it"
git status --short book/src/reference/
```

Expected: architecture green (fixture now committed-to-be); docs_consistency green (registry-ID ban satisfied); mdbook build clean if available; git shows exactly the two new reference files.

- [ ] **Step 7: Registry + plan bookkeeping**

`book/src/frontier/idea-registry.md`, PROC-9 row: `| raw | med | ideonomy session (workflow/tooling) |` → `| shipped | med | ideonomy session (workflow/tooling); \`cli/tests/architecture.rs\` → \`book/src/reference/layering.md\` |` (literal backticks).

`WORKFLOW_IMPROVEMENTS_PLAN.md`: 3c → `Complete`, 3d → `In Progress`.

- [ ] **Step 8: Gate and commit**

```bash
make quick
cargo fmt
git add cli/tests/architecture.rs book/src/reference/layering-generated.md \
    book/src/reference/layering.md book/src/SUMMARY.md Makefile \
    book/src/frontier/idea-registry.md WORKFLOW_IMPROVEMENTS_PLAN.md
git commit -m "feat(book): emit the layering page from its enforcer (PROC-9)

cli/tests/architecture.rs now renders the dependency graph it enforces as
book/src/reference/layering-generated.md, golden-checked on every test run
(REBASELINE=1 / make rebaseline-goldens accepts a deliberate graph change).
The book's picture is authored by the checker, so it cannot rot out of
sync with the enforced graph.

Claude-Session: https://claude.ai/code/session_01STboWuJ5sz26RTjVwDkGQu"
```

---

### Task 4: PROC-10 — `make doctor`, the repo self-map

One read-only command that orients a fresh session: pointers to the authoritative sources (never restatements, so it cannot rot far) plus a live-state footer and the orphaned-decision scan PROC-8 deliberately left informational.

**Files:**
- Create: `scripts/doctor.sh`
- Modify: `Makefile` (`.PHONY`, header comment line, `doctor` target)
- Modify: `book/src/frontier/idea-registry.md` (PROC-10 row → shipped)
- Modify: `WORKFLOW_IMPROVEMENTS_PLAN.md` (3d → Complete)

**Interfaces:**
- Consumes: `make -s help` (Task-independent; exists since Stage 1), `book/src/reference/layering.md` (Task 3), `docs/decisions/` naming (`NNNN-slug.md` or `slug.md`).
- Produces: nothing later tasks rely on.

- [ ] **Step 1: Write the script**

Create `scripts/doctor.sh`:

```bash
#!/usr/bin/env bash
# scripts/doctor.sh — the repo self-map: one read to orient a fresh session
# (or a cold-started subagent).
#
# Prints the orientation knowledge otherwise scattered across CLAUDE.md, the
# decision log, and the Makefile. Each line is a POINTER to the
# authoritative source, not a restatement, so this script cannot rot far.
# Read-only: never mutates anything.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

section() { printf '\n== %s\n' "$1"; }

echo "hornvale doctor — the repo self-map"

section "Layering (enforced: cli/tests/architecture.rs; picture: book/src/reference/layering.md)"
echo "  kernel -> domains/* -> windows/* -> cli"
echo "  a domain depends on the kernel and NOTHING else; windows/worldgen is the"
echo "  composition root; external deps allowlist: serde, serde_json"

section "The gate (cost-ordered; Makefile / CLAUDE.md Commands)"
make -s help

section "Determinism contracts (CLAUDE.md Determinism section is authoritative)"
echo "  - same seed + pins => byte-identical worlds and artifacts; the seed is a world's identity"
echo "  - floats quantized to 8 significant digits at serialization boundaries ONLY (kernel/src/quantize.rs)"
echo "  - no wall-clock time; no HashMap/HashSet (clippy.toml disallowed-types)"
echo "  - stream labels and consumption order are save-format contracts (docs/decisions/0006-*)"

section "Committed generated artifacts"
echo "  - regenerate all: make rebaseline (scripts/regenerate-artifacts.sh; CI drift-checks the set)"
echo "  - byte-golden test fixtures: make rebaseline-goldens (REBASELINE=1 accept path; kernel/src/golden.rs)"
echo "  - census review surface: make lab-diff STUDY=<name>"
echo "  - historical pre-<campaign> pins are frozen history: scripts/freeze-fixture.sh, never rebaselined"

section "Documentation map"
decision_count=$(find docs/decisions -name '*.md' ! -name 'README.md' | wc -l | tr -d ' ')
echo "  - docs/README.md — what knowledge lives where and how ideas flow"
echo "  - docs/decisions/ — ${decision_count} ratified records (append-only; grep before relitigating)"
echo "  - book/src/frontier/idea-registry.md — the idea registry (check before proposing anything)"
echo "  - WORKFLOW_IMPROVEMENTS_PLAN.md — the workflow/tooling backlog and its stages"

section "Live state"
echo "  branch: $(git branch --show-current)   dirty files: $(git status --porcelain | wc -l | tr -d ' ')"
git worktree list | sed 's/^/  /'

section "Decisions never cited in sources or docs (informational, not a gate)"
orphans=0
for f in docs/decisions/*.md; do
    stem="$(basename "$f" .md)"
    [ "$stem" = "README" ] && continue
    case "$stem" in
        [0-9][0-9][0-9][0-9]-*) pat="${stem:0:4}|${stem:5}" ;;
        *) pat="$stem" ;;
    esac
    if ! grep -rqE "(decision|decisions|ADR) (\`)?($pat)" \
        --include='*.rs' --include='*.sh' --include='*.md' \
        --exclude-dir=decisions --exclude-dir=target --exclude-dir=.git \
        kernel domains windows cli tools scripts docs book CLAUDE.md 2>/dev/null; then
        echo "  $stem"
        orphans=$((orphans + 1))
    fi
done
[ "$orphans" -eq 0 ] && echo "  (none)"

exit 0
```

- [ ] **Step 2: shellcheck and run it once**

```bash
shellcheck scripts/doctor.sh
bash scripts/doctor.sh 2>&1 | tee /tmp/hv-s3t4-doctor.txt
```

Expected: shellcheck clean; the output shows all seven sections; the orphan section lists only genuinely-uncited decisions (spot-check one listed stem with `grep -rn "<its number>" kernel domains windows cli docs book | head` — if a listed decision is clearly cited somewhere, the `pat` matching has a bug: fix the script, not the expectation). Note the orphan list in your report.

- [ ] **Step 3: Makefile target**

Add `doctor` to the `.PHONY` line; add to the header comment block (after the `make lab-diff` line):

```make
#   make doctor       # print the repo self-map (orientation for a fresh session)
```

and the target (after `lab-diff`):

```make
doctor: ## Print the repo self-map (orientation for a fresh session)
	@bash scripts/doctor.sh
```

Run `make doctor` once — same output as Step 2.

- [ ] **Step 4: Registry + plan bookkeeping**

`book/src/frontier/idea-registry.md`, PROC-10 row: `| raw | med | ideonomy session (workflow/tooling) |` → `| shipped | med | ideonomy session (workflow/tooling); \`scripts/doctor.sh\` (\`make doctor\`) |` (literal backticks). Note the row text says `just doctor` — leave the row text untouched (IDs and idea text are permanent); the Where cell records the `make` reality.

`WORKFLOW_IMPROVEMENTS_PLAN.md`: 3d → `Complete`.

- [ ] **Step 5: Gate and commit**

```bash
make quick
cargo test -p hornvale --test docs_consistency 2>&1 | tee /tmp/hv-s3t4-docs.txt
cargo fmt
git add scripts/doctor.sh Makefile book/src/frontier/idea-registry.md WORKFLOW_IMPROVEMENTS_PLAN.md
git commit -m "tooling(doctor): the repo self-map as one command (PROC-10)

make doctor prints the orientation a fresh session otherwise re-derives:
the layering chain, the cost-ordered gate, the determinism contracts, the
three regeneration stories, the documentation map, live git state, and the
decisions nothing cites (informational — the PROC-8 gate checks the other
direction). Every line points at the authoritative source rather than
restating it.

Claude-Session: https://claude.ai/code/session_01STboWuJ5sz26RTjVwDkGQu"
```

---

### Task 5: Close the stage

**Files:**
- Modify: `WORKFLOW_IMPROVEMENTS_PLAN.md` (Stage 3 intro: append `All four landed.`)

- [ ] **Step 1: Run the full gate once**

Run: `make gate 2>&1 | tee /tmp/hv-s3t5-gate.txt`
Expected: fmt clean, clippy clean, full workspace suite PASS. Inspect the tee'd file; do not re-run. If anything fails, STOP and report — do not fix.

- [ ] **Step 2: Close the plan section and commit**

Append `All four landed.` to the end of the Stage 3 `**Goal**` paragraph in `WORKFLOW_IMPROVEMENTS_PLAN.md`; verify 3a–3d all read `Complete`.

```bash
git add WORKFLOW_IMPROVEMENTS_PLAN.md
git commit -m "docs(plan): close Workflow Improvements Stage 3 (follow-ups, PROC-8/9/10)

Claude-Session: https://claude.ai/code/session_01STboWuJ5sz26RTjVwDkGQu"
```

Then the controller uses superpowers:finishing-a-development-branch (final whole-branch review first).
