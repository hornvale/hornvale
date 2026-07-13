# Fast Gate Tiers Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Drop the default `cargo test --workspace` commit gate from ~43.5 min to ≤5 min on an M1 Max by deferring the five live-worldgen correctness batteries behind `#[ignore]`, with the full evidence still runnable on demand and on the cloud.

**Architecture:** Standard-library `#[ignore]` tags with a single canonical reason string carrying a `heavy:` token, so the deferred tier stays greppable, counted, and un-drifted. A `make gate-full` target and the cloud script run `-- --include-ignored`. A convention-guard test locks the reason-string format.

**Tech Stack:** Rust (edition 2024), std only, `make`, `git`.

## Global Constraints

- **Edition 2024**; `#![warn(missing_docs)]` per crate (test files exempt).
- **std-only, no new dependencies.** The dependency allowlist is `serde`, `serde_json` only (`cli/tests/architecture.rs`). The guard test may shell out to `git`/`cargo` via `std::process::Command` (the pattern `architecture.rs` already uses) — that is not a dependency.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (`clippy.toml`).
- **The canonical ignore reason string, used verbatim on every gated test:**
  `heavy: live-worldgen battery; runs in make gate-full / cloud nightly`
- **Binding success criterion:** `time cargo test --workspace` ≤ 300s wall. The 12-test roster below is the first pass; if still over, gate the next-heaviest live-worldgen/subprocess tests (Task 1 Step 5) until under. A fast pure-logic test is never gated.
- **No test is deleted or weakened** — deferral only. `cargo test --workspace -- --include-ignored` must stay green.
- Run `cargo fmt` as the final step before every commit. Every commit must pass `cargo fmt --check` and `cargo clippy --workspace --all-targets -- -D warnings`.
- Commit messages end with the trailer:
  `Claude-Session: https://claude.ai/code/session_017rThA5iKFQqFwrAiE1ZHGB`

---

## File Structure

- `windows/lab/tests/fixture_staleness.rs` — gate 1 test (of 10).
- `windows/worldgen/tests/pin_enumeration.rs` — gate 1 test (whole binary).
- `cli/tests/sky_exit_criterion.rs` — gate 1 test (of 6).
- `windows/worldgen/src/lib.rs` — gate 1 test (of 61).
- `windows/lab/src/runner.rs` — gate 5 tests.
- `windows/lab/src/metrics.rs` — gate 3 tests.
- `Makefile` — new `gate-full` target + help line.
- `scripts/gate-remote.sh` — `make gate` → `make gate-full`.
- `cli/tests/heavy_tier.rs` — NEW: convention-guard test.
- `CLAUDE.md` — gate ladder description.

---

## Task 1: Gate the heavy roster; land the default suite ≤5 min

**Files:**
- Modify: `windows/lab/tests/fixture_staleness.rs:112` (after `#[test]`, before `fn census_fixtures_match_a_probe_of_live_seeds`)
- Modify: `windows/worldgen/tests/pin_enumeration.rs:165`
- Modify: `cli/tests/sky_exit_criterion.rs:232`
- Modify: `windows/worldgen/src/lib.rs:2503`
- Modify: `windows/lab/src/runner.rs` (5 sites)
- Modify: `windows/lab/src/metrics.rs` (3 sites)

**Interfaces:**
- Produces: 12 tests tagged `#[ignore = "heavy: live-worldgen battery; runs in make gate-full / cloud nightly"]`. Task 2's guard test asserts this exact string.

- [ ] **Step 1: Add the ignore tag to each of the 12 heavy tests.**

For each site, insert a new line carrying the canonical tag **between** the existing `#[test]` line and the `fn` line, preserving the existing indentation (integration tests at column 0; the `src/` unit tests are inside `mod tests` at 4-space indent). The tag is identical everywhere:

```rust
#[ignore = "heavy: live-worldgen battery; runs in make gate-full / cloud nightly"]
```

The 12 functions (each currently `#[test]\nfn <name>`):
- `fixture_staleness.rs`: `census_fixtures_match_a_probe_of_live_seeds`
- `pin_enumeration.rs`: `full_pin_product_is_enumerated`
- `sky_exit_criterion.rs`: `graded_pins_never_fail_above_min`
- `worldgen/src/lib.rs`: `build_world_with_default_roster_matches_build_world_byte_for_byte` (4-space indent)
- `runner.rs` (4-space indent): `parallel_run_matches_sequential`, `five_seed_study_runs_and_is_deterministic`, `refusals_are_rows_not_errors`, `row_count_is_seeds_times_pin_sets`, `csv_round_trips_comma_containing_text_fields`
- `metrics.rs` (4-space indent): `shape_metrics_are_present_deterministic_and_sane`, `core_homophony_is_zero_for_every_daughter_under_the_merger_aware_assignment`, `family_battery_metrics_are_deterministic_across_two_builds`

Example (integration test, column 0):
```rust
#[test]
#[ignore = "heavy: live-worldgen battery; runs in make gate-full / cloud nightly"]
fn census_fixtures_match_a_probe_of_live_seeds() {
```
Example (unit test inside `mod tests`, 4-space indent):
```rust
    #[test]
    #[ignore = "heavy: live-worldgen battery; runs in make gate-full / cloud nightly"]
    fn parallel_run_matches_sequential() {
```

- [ ] **Step 2: Verify the default run skips exactly these and stays green.**

Run: `cargo test --workspace 2>&1 | tee /tmp/hv-tier1.txt | grep -E 'ignored|test result: FAILED'`
Expected: every `test result:` line shows `0 failed`; the summed `ignored` count across binaries is ≥ 12 (12 unless a binary counts an already-ignored test).

- [ ] **Step 3: Measure the default suite wall time.**

Run: `/usr/bin/time -p cargo test --workspace 2>&1 | tee /tmp/hv-tier1-timed.txt | grep -E '^(real|user)'`
Expected: `real` ≤ 300 (seconds). Run on as quiet a machine as available; if contention is heavy, note the `user/real` ratio.

- [ ] **Step 4: If real > 300s, find the next elephants.**

Run: `grep -B40 'finished in' /tmp/hv-tier1-timed.txt | grep -E 'Running |finished in [0-9]{2,}'`
Identify the slowest remaining binaries/tests.

- [ ] **Step 5: Gate the next-heaviest live-worldgen/subprocess tests and re-measure.**

Add the same canonical tag to the next-heaviest **live-worldgen or subprocess** tests until `real` ≤ 300s. Candidate order (from the spec): remaining `sky_exit_criterion.rs` CLI tests (`rotation_flip_flips_the_religion`, `moons_flip_flips_the_calendar_and_grows_the_pantheon_without_displacing_the_head`, `worlds_survive_reload_byte_identically`, `scout_is_deterministic_and_finds_three_moon_worlds`, `refusals_are_recorded_in_the_world`), then `domains/astronomy/tests/genesis_properties.rs` and `domains/terrain/tests/tectonic_properties.rs` property batteries, then `windows/worldgen/tests/species_worlds.rs`. Never gate a fast pure-logic test to hit the number. Re-run Step 3 after each addition until ≤300s.

- [ ] **Step 6: Verify the full set still passes with the heavy tier included.**

Run: `cargo test --workspace -- --include-ignored 2>&1 | grep -E 'test result: FAILED|error\['`
Expected: no output (all green). This is the ~40-min full run; pass an explicit Bash `timeout: 3600000`.

- [ ] **Step 7: Format and commit.**

```bash
cargo fmt
git add -A
git commit -m "test: defer live-worldgen batteries behind #[ignore] (gate ≤5 min)

<paste the Step 3 real-seconds before/after and the final gated count>

Claude-Session: https://claude.ai/code/session_017rThA5iKFQqFwrAiE1ZHGB"
```

---

## Task 2: `make gate-full` target + the convention-guard test

**Files:**
- Create: `cli/tests/heavy_tier.rs`
- Modify: `Makefile` (add `gate-full` target after `gate-fast`)

**Interfaces:**
- Consumes: the canonical ignore string and `heavy:` token from Task 1.
- Produces: `make gate-full`; a test `heavy_tier_reason_strings_are_canonical`.

- [ ] **Step 1: Write the guard test.**

Create `cli/tests/heavy_tier.rs`:
```rust
//! The heavy-tier convention (fast-gate-tiers spec): every #[ignore]d test
//! that defers a live-worldgen battery carries the exact canonical reason
//! string, so `make gate` (which skips them) and `make gate-full` (which
//! runs them) stay in sync and the tier is greppable, not tribal.

use std::process::Command;

/// The one reason string every heavy-tier test must use verbatim.
const CANONICAL: &str = "heavy: live-worldgen battery; runs in make gate-full / cloud nightly";

/// All `#[ignore = "..."]` reason strings in the workspace's Rust sources,
/// found via `git grep` (std-only; the pattern architecture.rs already uses).
fn ignore_reasons() -> Vec<String> {
    let out = Command::new("git")
        .args(["grep", "-hoE", r#"#\[ignore = "[^"]*"\]"#, "--", "*.rs"])
        .output()
        .expect("git grep should run");
    assert!(out.status.success(), "git grep failed");
    String::from_utf8(out.stdout)
        .expect("git output is utf8")
        .lines()
        .filter_map(|l| l.split_once("= \"").and_then(|(_, r)| r.strip_suffix("\"]")))
        .map(str::to_string)
        .collect()
}

#[test]
fn heavy_tier_reason_strings_are_canonical() {
    let reasons = ignore_reasons();
    let heavy: Vec<&String> = reasons.iter().filter(|r| r.contains("heavy:")).collect();
    assert!(
        !heavy.is_empty(),
        "expected at least one heavy-tier #[ignore] test; found none"
    );
    for r in &heavy {
        assert_eq!(
            *r, CANONICAL,
            "heavy-tier ignore reason must be verbatim canonical; found: {r:?}"
        );
    }
}
```

- [ ] **Step 2: Run the guard — expect PASS (Task 1 tagged the roster canonically).**

Run: `cargo test -p hornvale --test heavy_tier`
Expected: PASS (1 test).

- [ ] **Step 3: Prove the guard catches a violation.**

Temporarily change one gated test's reason string to `"heavy: oops"` (e.g. in `pin_enumeration.rs`). `git grep` searches tracked files in the working tree, so the uncommitted edit is seen. Run `cargo test -p hornvale --test heavy_tier` (Expected: FAIL, message `found: "heavy: oops"`). Revert the edit and re-run (Expected: PASS). Do not commit the violation.

- [ ] **Step 4: Add the `gate-full` target to the Makefile.**

After the `gate-fast` target, insert:
```makefile
gate-full: fmt-check clippy ## The full evidence gate: fmt + clippy + ALL tests incl. the #[ignore]d heavy tier (~40 min)
	cargo test --workspace -- --include-ignored
```
Add `gate-full` to the `.PHONY` line.

- [ ] **Step 5: Verify the target's command (without paying the 40-min run).**

Run: `make -n gate-full`
Expected: the printed recipe ends with `cargo test --workspace -- --include-ignored`.

- [ ] **Step 6: Format and commit.**

```bash
cargo fmt
git add -A
git commit -m "test(gate): make gate-full + heavy-tier convention guard

Claude-Session: https://claude.ai/code/session_017rThA5iKFQqFwrAiE1ZHGB"
```

---

## Task 3: Point the cloud at the full tier; document the ladder

**Files:**
- Modify: `scripts/gate-remote.sh:45`
- Modify: `CLAUDE.md` (the gate command block)

**Interfaces:**
- Consumes: `make gate-full` from Task 2.

- [ ] **Step 1: Cloud runs the full evidence.**

In `scripts/gate-remote.sh`, in the `main()` gate line, change `make gate` to `make gate-full`:
```bash
  $ssh 'cd work && make gate-full && scripts/regenerate-artifacts.sh && git -c core.fileMode=false diff --exit-code -- book/' || rc=$?
```

- [ ] **Step 2: Shellcheck the edited script.**

Run: `shellcheck scripts/gate-remote.sh`
Expected: no new warnings (pre-existing disables unchanged).

- [ ] **Step 3: Document the ladder in CLAUDE.md.**

In the `## Commands` section's gate block, replace the three-line "full gate" comment with the ladder, keeping the exact commands:
```bash
# The tiered gate (cost-ordered; `make help` lists all targets):
#   make quick       # fmt --check + clippy (cheap pre-commit half)
#   make gate-fast   # ITERATION ONLY: fmt/clippy/test scoped to changed crates
#   make gate        # COMMIT GATE: fmt + clippy + workspace tests (heavy tier skipped, ≤5 min)
#   make gate-full   # full evidence: + the #[ignore]d live-worldgen batteries (~40 min)
# The heavy tier (live-worldgen correctness batteries) is #[ignore]d out of
# `make gate` and runs in `make gate-full` and the cloud nightly; it stays
# greppable via the `heavy:` ignore-reason token (see cli/tests/heavy_tier.rs).
cargo test --workspace   # == the test half of `make gate`
cargo fmt --check
cargo clippy --workspace --all-targets -- -D warnings
```

- [ ] **Step 4: Confirm the doc-consistency test still passes.**

Run: `cargo test -p hornvale --test docs_consistency`
Expected: PASS (CLAUDE.md is prose, not asserted verbatim — but confirm nothing keys off the old text).

- [ ] **Step 5: Commit.**

```bash
git add -A
git commit -m "chore(gate): cloud runs gate-full; document the tier ladder

Claude-Session: https://claude.ai/code/session_017rThA5iKFQqFwrAiE1ZHGB"
```

---

## Definition of Done

- `time cargo test --workspace` ≤ 300s; `cargo test --workspace -- --include-ignored` green.
- `grep -rn 'heavy:' --include=*.rs` enumerates exactly the gated roster; the guard test enforces the canonical string.
- `make gate-full` exists and runs `--include-ignored`; `gate-remote.sh` uses it.
- CLAUDE.md and `make help` describe the ladder.
- fmt + clippy clean; no new deps; no test deleted.
