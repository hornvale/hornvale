# The Armature Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Declare thirty causal links across the world model, teach D5 to see direction, and measure the frame exactly once.

**Architecture:** `Expectation` gains a `direction` field validated at load. `detect_d5` reports a *direction* mismatch distinctly from, and ranked above, a *strength* mismatch. The thirty expectations frozen in spec §5 are then committed **without measurement**, and a later task measures once and records what fired.

**Tech Stack:** Rust 2024, `serde_json`, existing `windows/lab/src/domesday/` modules. No new dependencies.

## Global Constraints

- **Spec:** `docs/superpowers/specs/2026-08-09-the-armature-design.md`. Where this plan and the spec disagree, the spec governs.
- **THE BLINDING RULE (spec §3).** Tasks 1–3 must not compute any correlation, run `detect_d5` against the live census, or read a value from `rows.csv`. Reading `schema.json` for **column names** is permitted. Task 4 is the single measurement.
- **Expectations are never revised to fit data.** A row that fires is a finding. Deleting or re-classing a row after seeing a result is forbidden; if one seems wrong, record it and leave it.
- **Never re-run a census. Never set `HV_CENSUS=1`.**
- Determinism: `total_cmp`, no `HashMap`/`HashSet`, floats through `hornvale_kernel::quantize` at emit.
- **Never write `CARGO_MANIFEST_DIR`**, including in comments — `cli/tests/build_path_embedding.rs` default-denies it by raw text match.
- Rust edition 2024; `#![warn(missing_docs)]`; `type-audit:` tags on pub-boundary primitives.
- Run `cargo fmt` before every commit. Commit messages end with: `Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU`

## File Structure

| File | Responsibility |
|---|---|
| `windows/lab/src/domesday/comparators.rs` | **modify** — `Expectation.direction`, `DIRECTIONS`, load-time validation |
| `windows/lab/src/domesday/detect.rs` | **modify** — `detect_d5` reports direction vs strength |
| `studies/expectations.json` | **replace** — the thirty frozen rows |
| `book/src/domesday/*.md` | regenerated output |
| `docs/retrospectives/the-armature.md`, `book/src/chronicle/the-armature.md` | close |

---

### Task 1: `direction` on the expectation

**Files:** Modify `windows/lab/src/domesday/comparators.rs`

**Interfaces:**
- Produces: `Expectation { metric, tracks, why, declared, direction }` (all `String`); `pub const DIRECTIONS: &[&str] = &["positive", "negative", "none"]`

**Blinding:** this task computes nothing. Its tests use temp-dir fixtures only.

- [ ] **Step 1: Write the failing tests**

Add to `comparators.rs`'s test module, following the shape of the existing `an_expectation_with_an_unknown_declared_class_is_rejected`:

```rust
    #[test]
    fn an_expectation_with_an_unknown_direction_is_rejected() {
        let dir = std::env::temp_dir().join("armature-dir-test");
        std::fs::create_dir_all(&dir).expect("tmp");
        let p = dir.join("expectations.json");
        std::fs::write(
            &p,
            r#"{"expect":[{"metric":"a","tracks":"b","why":"w","declared":"weak","direction":"sideways"}]}"#,
        )
        .expect("write");
        let err = load_expectations(&p).expect_err("an unknown direction must be rejected");
        assert!(err.contains("sideways"), "error must name the offending value: {err}");
        assert!(err.contains("positive"), "error must name the permitted set: {err}");
    }

    #[test]
    fn declared_none_requires_direction_none() {
        let dir = std::env::temp_dir().join("armature-none-test");
        std::fs::create_dir_all(&dir).expect("tmp");
        let p = dir.join("expectations.json");
        std::fs::write(
            &p,
            r#"{"expect":[{"metric":"a","tracks":"b","why":"w","declared":"none","direction":"positive"}]}"#,
        )
        .expect("write");
        let err = load_expectations(&p).expect_err("declared none with a signed direction must be rejected");
        assert!(err.contains("none"), "error must explain the coupling: {err}");
    }

    #[test]
    fn a_well_formed_signed_expectation_loads() {
        let dir = std::env::temp_dir().join("armature-ok-test");
        std::fs::create_dir_all(&dir).expect("tmp");
        let p = dir.join("expectations.json");
        std::fs::write(
            &p,
            r#"{"expect":[{"metric":"a","tracks":"b","why":"w","declared":"dominant","direction":"negative"}]}"#,
        )
        .expect("write");
        let es = load_expectations(&p).expect("loads");
        assert_eq!(es[0].direction, "negative");
    }
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-lab --lib domesday::comparators`
Expected: FAIL — `Expectation` has no field `direction`.

- [ ] **Step 3: Implement**

Add to `pub struct Expectation`:

```rust
    /// The declared sign of the relationship: `positive`, `negative`, or
    /// `none`. A `declared` of `none` requires this to be `none` too — a claim
    /// of no relationship has no sign.
    pub direction: String,
```

Add beside `DECLARED_CLASSES`:

```rust
/// The permitted values of [`Expectation::direction`]. A backwards coupling —
/// the right strength with the wrong sign — is a defect a strength-only check
/// cannot see, which is why this field exists (spec §2).
/// type-audit: bare-ok(identifier-text)
pub const DIRECTIONS: &[&str] = &["positive", "negative", "none"];
```

In `load_expectations`, after reading `declared` and before constructing the `Expectation`, read `direction` with the same `required_str` helper and validate:

```rust
        if !DIRECTIONS.contains(&direction) {
            return Err(format!(
                "{}: expectation for {metric} declares direction {direction:?}, \
                 which is not one of {DIRECTIONS:?}",
                path.display()
            ));
        }
        if (declared == "none") != (direction == "none") {
            return Err(format!(
                "{}: expectation for {metric} pairs declared {declared:?} with \
                 direction {direction:?}; `none` must appear in both or neither \
                 — a claim of no relationship has no sign",
                path.display()
            ));
        }
```

- [ ] **Step 4: Run to verify they pass**

Run: `cargo test -p hornvale-lab --lib domesday::comparators`
Expected: PASS. The existing single-row `studies/expectations.json` will now **fail to load** because it has no `direction` — that is expected; Task 2 fixes it. If any other test breaks, note which.

- [ ] **Step 5: Add `direction` to the existing single row so the tree is green**

Edit `studies/expectations.json`'s one row to add `"direction": "negative"` (the frozen frame's row #1 declares negative). Do **not** add any other row here — Task 3 writes the frame.

- [ ] **Step 6: Run the module and commit**

```bash
cargo test -p hornvale-lab --lib domesday
cargo fmt
git add windows/lab/src/domesday/comparators.rs studies/expectations.json
git commit -m "feat(lab): expectations declare a direction

A metric coupled BACKWARDS at the declared strength passed silently,
because detect_d5 compares band_of(r.abs()) and never looks at the sign.

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 2: D5 reports direction distinctly from strength

**Files:** Modify `windows/lab/src/domesday/detect.rs`, `windows/lab/src/domesday/render.rs`

**Interfaces:**
- Consumes: `Expectation { …, direction }`, `DIRECTIONS`
- Produces: `Finding` values whose `detector` is `"D5 direction"` or `"D5 strength"`

**Blinding:** synthetic vectors only. Do **not** run `detect` against the live census.

**The rule (spec §4.2):**

| observed vs declared | outcome |
|---|---|
| band matches, sign matches | silent |
| band matches, **sign opposite** | `D5 direction` — more serious |
| band differs | `D5 strength` |
| observed band is `none` | strength only — **never report sign**, because a near-zero `r`'s sign is noise |

- [ ] **Step 1: Write the failing tests**

```rust
    #[test]
    fn d5_fires_direction_when_the_sign_is_backwards() {
        // Perfectly anti-correlated, declared positive at the same strength.
        let c = two_numeric_columns("m", &[1.0, 2.0, 3.0, 4.0], "d", &[4.0, 3.0, 2.0, 1.0]);
        let e = expectation("m", "d", "dominant", "positive");
        let f = detect_d5(&c, &[e]);
        assert_eq!(f.len(), 1, "a backwards coupling must fire");
        assert_eq!(f[0].detector, "D5 direction");
        assert!(f[0].detail.contains("negative"), "must name what was observed: {}", f[0].detail);
    }

    #[test]
    fn d5_is_silent_when_strength_and_sign_both_match() {
        let c = two_numeric_columns("m", &[1.0, 2.0, 3.0, 4.0], "d", &[4.0, 3.0, 2.0, 1.0]);
        let e = expectation("m", "d", "dominant", "negative");
        assert!(detect_d5(&c, &[e]).is_empty(), "a correct claim must be silent");
    }

    #[test]
    fn d5_reports_strength_not_sign_when_the_observed_band_is_none() {
        // Near-zero correlation: the sign is noise and must not be reported.
        let c = two_numeric_columns("m", &[1.0, 2.0, 3.0, 4.0], "d", &[1.0, 1.0, 1.0, 1.0001]);
        let e = expectation("m", "d", "dominant", "positive");
        let f = detect_d5(&c, &[e]);
        assert_eq!(f.len(), 1);
        assert_eq!(f[0].detector, "D5 strength", "a none-band result is a strength finding");
        assert!(
            !f[0].detail.contains("direction"),
            "the sign of a near-zero r is noise and must not be reported: {}",
            f[0].detail
        );
    }

    #[test]
    fn d5_fires_strength_when_the_band_differs() {
        let c = two_numeric_columns("m", &[1.0, 2.0, 3.0, 4.0], "d", &[4.0, 3.0, 2.0, 1.0]);
        let e = expectation("m", "d", "weak", "negative");
        let f = detect_d5(&c, &[e]);
        assert_eq!(f.len(), 1);
        assert_eq!(f[0].detector, "D5 strength");
    }
```

Write the two helpers alongside, in the style already used in this test module:

```rust
    /// A two-column synthetic census, both numeric descriptors.
    fn two_numeric_columns(a: &str, av: &[f64], b: &str, bv: &[f64]) -> Census { /* build Census with two Columns (kind "numeric", role "descriptor", domain "climate") and one row per index */ }

    /// An expectation with the given declared class and direction.
    fn expectation(metric: &str, tracks: &str, declared: &str, direction: &str) -> Expectation { /* construct with why: String::new() */ }
```

Fill both in concretely — mirror the existing synthetic-`Census` helper in `render.rs`'s tests for the `Census`/`Column` construction.

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-lab --lib domesday::detect`
Expected: FAIL — `detector` is `"D5"`, not `"D5 direction"`/`"D5 strength"`.

- [ ] **Step 3: Implement**

Replace `detect_d5`'s reporting block (currently `detect.rs:265-279`) with:

```rust
        if let Some(r) = pearson(&xs, &ys) {
            let observed = band_of(r.abs());
            let observed_dir = if r < 0.0 { "negative" } else { "positive" };
            if observed != e.declared {
                // Strength mismatch. The sign is NOT reported when nothing was
                // measured — a near-zero r's sign is noise (spec §4.2).
                out.push(Finding {
                    detector: "D5 strength",
                    metric: e.metric.clone(),
                    detail: format!(
                        "declared {} tracking {}, but observed |r| = {:.3} ({} pairs) is {}",
                        e.declared,
                        e.tracks,
                        r.abs(),
                        xs.len(),
                        observed
                    ),
                });
            } else if observed != "none" && observed_dir != e.direction {
                // Right strength, WRONG SIGN: the link exists and runs
                // backwards. More serious than an over-claim.
                out.push(Finding {
                    detector: "D5 direction",
                    metric: e.metric.clone(),
                    detail: format!(
                        "declared {} {} tracking {}, but the observed coupling is {} \
                         (r = {:+.3}, {} pairs) — the link runs backwards",
                        e.direction,
                        e.declared,
                        e.tracks,
                        observed_dir,
                        r,
                        xs.len()
                    ),
                });
            }
        }
```

- [ ] **Step 4: Run to verify they pass**

Run: `cargo test -p hornvale-lab --lib domesday::detect`
Expected: PASS. Other tests referencing `"D5"` will need their detector string updated to `"D5 strength"` — update them, and say in your report which you changed.

- [ ] **Step 4b: Fix the renderer's hardcoded detector list — REQUIRED, or the survey silently undercounts**

`render.rs:548` iterates a frozen literal:

```rust
    for detector in ["D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8"] {
```

Renaming D5 makes that row report **0** while findings exist — a published
undercount, in the artifact whose whole claim is that it computes rather than
restates. This is the fourth frozen roster this programme has found; close the
class rather than patching the literal.

**Derive the list from the findings** instead:

```rust
    let mut detectors: Vec<&str> = findings.iter().map(|f| f.detector).collect();
    detectors.sort_unstable();
    detectors.dedup();
    for detector in detectors {
```

A detector that fires nothing then has no row, which is correct — an empty row
asserts a measurement that was never taken. Add a test that a finding with a
novel detector name appears in the rendered index, so a future rename cannot
silently vanish again.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/lab/src/domesday/detect.rs windows/lab/src/domesday/render.rs
git commit -m "feat(lab): D5 separates a backwards link from a weak one

A direction mismatch means the wire is on the wrong terminal; a strength
mismatch may only be an over-claim. Sign is suppressed when the observed
band is none, because a near-zero r's sign is noise.

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 3: Freeze the frame — THE BLIND TASK

**Files:** Replace `studies/expectations.json`

**Blinding — read this twice.** This task **must not measure anything**. Do not run `detect`, do not compute a correlation, do not read a value from `rows.csv`, do not run the full `domesday` test module. You may read `schema.json` for **column names only**.

The thirty rows are already written, in **spec §5**. Your job is transcription and validation, not authorship. Copy each row's `metric`, `tracks`, `declared`, `direction`, and a `why` drawn from the table's reason column.

- [ ] **Step 1: Transcribe all thirty rows** from spec §5 into `studies/expectations.json`, preserving order (1–30). Two rows (#4, #29) carry `"declared": "none"` and **must** carry `"direction": "none"`.

- [ ] **Step 2: Verify every name exists — names only**

```bash
python3 - <<'PY'
import json
known={c['name'] for c in json.load(open('book/src/laboratory/generated/the-census/schema.json'))['columns']}
exp=json.load(open('studies/expectations.json'))['expect']
bad=[(e['metric'],e['tracks']) for e in exp if e['metric'] not in known or e['tracks'] not in known]
print(f"{len(exp)} expectations; unknown names: {bad if bad else 'none'}")
PY
```

Expected: `30 expectations; unknown names: none`. **If a name is unknown, stop and report** — do not substitute a different metric, because that would be authoring a claim the spec did not freeze.

- [ ] **Step 3: Verify the file loads and the none-pairing holds**

Run: `cargo test -p hornvale-lab --lib domesday::comparators`
Expected: PASS. These tests read names and validate structure; they compute no correlation.

- [ ] **Step 4: Commit — the freeze**

```bash
cargo fmt
git add studies/expectations.json
git commit -m "feat(lab): freeze the thirty-row causal frame

Written from physics in the spec against metric names and doc strings
only; no census value was read while authoring. This commit contains no
measurement — that is spec S5, checkable by commit order.

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

**This commit is the preregistration.** Nothing after it may edit these rows.

---

### Task 4: Measure once

**Files:** Modify `windows/lab/src/domesday/detect.rs` (the live-acceptance test's pinned counts only), regenerate `book/src/domesday/`

**This is the single measurement.** Everything before it was blind.

- [ ] **Step 1: Run the frame against the census and record the raw result**

```bash
cargo run -q -p hornvale -- lab domesday
git diff --stat book/src/domesday/
```

Then extract exactly what fired:

```bash
grep -rhoE "^- \*\*D5 (direction|strength)\*\*.*" book/src/domesday/*.md | sort
```

**Paste that list verbatim in your report.** It is the campaign's product.

- [ ] **Step 2: Report the tallies**

Count and report: how many of the thirty are silent, how many fired `D5 strength`, how many fired `D5 direction`. Report the six Biology rows (#16–#21) separately — the spec predicts all six fire.

- [ ] **Step 3: Re-pin the live-acceptance counts**

`the_live_census_reproduces_the_preregistered_findings` pins detector counts that will now have moved. Update them to the measured values, each with a comment saying the number is a measurement to investigate before re-pinning — the pattern already used for `assert_eq!(d1, 27)`.

**Do not** change a threshold, a declared class, or a direction to make a count nicer.

- [ ] **Step 4: Regenerate and verify determinism**

```bash
bash scripts/regenerate-artifacts.sh
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/ docs/digest/ book/src/domesday/
```
Expected: exit 0 on the second consecutive run.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(lab): measure the frame — <N> of 30 links fired

<one line naming the direction findings, which are the serious ones>

Claude-Session: https://claude.ai/code/session_01BMX7dSxg723Kvmn4p2NmKU"
```

---

### Task 5: Close

- [ ] **Step 1:** `make gate` (foreground, `timeout: 3600000`). Report `rc`. If red, **stop and report BLOCKED**.
- [ ] **Step 2:** `mdbook build book` succeeds.
- [ ] **Step 3: Evaluate the falsification clause (spec §7).** If most rows fired, the likely cause is the author's physics rather than the world's. **Give an honest verdict** and publish the frame with its failures rather than pruning rows. Say which rows you believe were wrong *about the physics* versus which revealed a genuine gap — and mark that as your judgement, not a measurement.
- [ ] **Step 4:** Chronicle `book/src/chronicle/the-armature.md` + SUMMARY; retrospective `docs/retrospectives/the-armature.md` (decision 0020); registry rows; Confidence Gradient re-score if a bet moved (decision 0030).
- [ ] **Step 5: Record the ranked consequences.** Every `D5 direction` finding is a candidate defect — a coupling that runs backwards. Every `D5 strength` on a link the physics says should be strong is a candidate severed wire. Rank them and say which are one-line fixes versus campaigns.

---

## Self-Review

**Spec coverage.** §4.1 `direction` → Task 1. §4.2 D5 reporting → Task 2. §4.3 expect-none → Tasks 1 (validation) and 3 (rows #4, #29). §5 the frame → Task 3. §3 blinding → Tasks 1–3 constraints plus S5's commit-order check, satisfied by Task 3 preceding Task 4. §7 S1→Task 3 Step 2, S2→Task 1, S3→Task 2, S4→Task 2, S5→commit order, S6→Task 4 Step 4.

**Placeholder scan.** Task 2 Step 1's two helper functions are described rather than written out — deliberate, because they must mirror an existing helper in `render.rs`'s tests that the implementer will read. Flagged rather than hidden. No other gaps.

**Type consistency.** `Expectation.direction: String` defined in Task 1, consumed in Task 2. `DIRECTIONS` defined once. `Finding.detector` becomes `"D5 direction"` / `"D5 strength"` in Task 2 and is grepped by those exact strings in Task 4 Step 1.

**Known risk.** Task 3's commit will change what the full test suite reports, because the frame is live from that moment. The blinding rule survives because Task 3 does not *look*: it runs only the `comparators` tests, which validate structure. Task 4 looks, once.
