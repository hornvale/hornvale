# The Timekeeper Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give every test a recorded duration, a committed per-host baseline,
and two alarms — per-test and whole-suite — behind one `make ci` entry point
that persists its raw output.

**Architecture:** `.config/nextest.toml` turns on a `ci` profile emitting
`libtest-json-plus`. Pure parsing/baseline/alarm logic lives in
`windows/lab/src/timings.rs` (the Laboratory is the measurement window, and
`cli/` is bin-only so its tests cannot host shared code). `cli/tests/timings_alarm.rs`
is the failing-test surface. `make ci` is a wrapper with no logic.

**Tech Stack:** Rust 2024 (std + `serde`/`serde_json`, already workspace
dependencies), `cargo-nextest` 0.9.140, `make`.

## Global Constraints

- Dependencies: `serde`, `serde_json`, `libm` only, workspace-wide. **No new
  crates** — `libtest-json-plus` is JSON precisely so no XML parser is needed.
- Every crate sets `#![warn(missing_docs)]`; every public item, field, and
  variant gets a one-line doc comment.
- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only.
- Float sorting uses `total_cmp`.
- Every primitive at a `pub` boundary carries a `type-audit:` verdict tag.
  Grammar exactly: `type-audit: bare-ok(<class>)` for a single item, or
  `type-audit: bare-ok(<class>: <field>), bare-ok(<class>: <field>)` for a
  struct — class first, field name after the colon.
- **Wall-clock exemption:** this module measures the *build*, not the world.
  `census_claim.rs` already carries the precedent — a scoped
  `#[allow(clippy::disallowed_types)]` with a comment explaining it never
  reaches a fact or an artifact. Reuse that wording; do not remove the
  workspace ban.
- `cargo fmt` last before every commit. `make gate` must stay green.
- Run the gate on the Mac, not lefford (decision 0086).

---

### Task 1: The `ci` nextest profile

**Files:**
- Create: `.config/nextest.toml`

**Interfaces:**
- Consumes: nothing.
- Produces: a `ci` profile emitting `libtest-json-plus`; the `default` profile
  unchanged so `make gate` behaves exactly as today.

- [ ] **Step 1: Create the config**

```toml
# .config/nextest.toml — the profile surface, unused until The Timekeeper.
#
# `default` is deliberately left at nextest's own defaults: `make gate` must
# behave exactly as it did before this campaign.
#
# `slow-timeout.period` MARKS a test slow; it does not kill it.
# `terminate-after` is deliberately absent (spec N4): observation and
# enforcement are separate knobs, and conflating them means a contention blip
# destroys a run instead of reporting one.

[profile.default]

[profile.ci]
# Report a test that crosses this, but never terminate it.
slow-timeout = { period = "60s" }
# Full output for anything that fails, so the persisted log is self-contained.
failure-output = "immediate-final"
fail-fast = false
```

`fail-fast = false` is load-bearing: the alarm needs durations for **every**
test, and a fail-fast run cancels the rest. The Siding lost a whole heavy-tier
run to exactly this.

- [ ] **Step 2: Verify the default profile is unchanged**

Run: `cargo nextest run -p hornvale-kernel -E 'test(text_of)'`
Expected: PASS, same human output as before — no JSON, no config errors.

- [ ] **Step 3: Verify the ci profile emits durations**

Run:
```bash
NEXTEST_EXPERIMENTAL_LIBTEST_JSON=1 cargo nextest run -p hornvale-kernel \
  --profile ci --message-format libtest-json-plus -E 'test(text_of)' 2>&1 | grep exec_time
```
Expected: a line containing `"event":"ok"` and `"exec_time":`.

- [ ] **Step 4: Commit**

```bash
git add .config/nextest.toml
git commit -m "feat(the-timekeeper): add the ci nextest profile"
```

---

### Task 2: A typed way to ask who holds the box

The alarm enforces only under exclusive access (spec §3d). The only public
accessor today is `status_line() -> String`, which returns prose — parsing it
would repeat this repo's documented "a measurement is only as good as its
parser" failure. `live_holder_at` exists at `census_claim.rs:204` but is
private.

**Files:**
- Modify: `windows/lab/src/census_claim.rs`

**Interfaces:**
- Consumes: `ClaimInfo { pid, host, user, started, goldens, label, reference,
  cmdline }` and the private `fn live_holder_at(path: &Path) -> Option<ClaimInfo>`.
- Produces: `pub fn current_holder() -> Option<ClaimInfo>` — the live holder of
  this machine's claim, or `None`.

- [ ] **Step 1: Write the failing test**

Add to `mod tests` in `windows/lab/src/census_claim.rs`:

```rust
#[test]
fn current_holder_is_none_when_no_claim_file_exists() {
    let dir = std::env::temp_dir().join(format!("hv-tk-{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let missing = dir.join("absent.claim");
    // SAFETY-of-intent: this test owns the env var for its own process.
    unsafe { std::env::set_var("HV_CENSUS_CLAIM_PATH", &missing) };
    assert!(current_holder().is_none());
    unsafe { std::env::remove_var("HV_CENSUS_CLAIM_PATH") };
    std::fs::remove_dir_all(&dir).ok();
}
```

- [ ] **Step 2: Run it and confirm it fails**

Run: `cargo test -p hornvale-lab --lib current_holder_is_none_when_no_claim_file_exists`
Expected: FAIL — `cannot find function 'current_holder' in this scope`.

- [ ] **Step 3: Implement**

Add above `status_line` in `windows/lab/src/census_claim.rs`:

```rust
/// The live holder of this machine's claim, if any — the typed answer that
/// [`status_line`] renders as prose. Callers that must *decide* something
/// (whether to enforce a timing budget, say) need the fields, not the
/// sentence: parsing prose back into data is how a measurement acquires a
/// second, disagreeing model of its own format.
pub fn current_holder() -> Option<ClaimInfo> {
    let path = std::path::PathBuf::from(
        std::env::var("HV_CENSUS_CLAIM_PATH").unwrap_or_else(|_| CLAIM_PATH.to_string()),
    );
    live_holder_at(&path)
}
```

Then make `status_line` call it, so there is one reader:

```rust
    match current_holder() {
```
(replacing its `match live_holder_at(&path)`, and deleting the now-unused
local `path` binding in `status_line`).

- [ ] **Step 4: Run the test and the existing claim tests**

Run: `cargo test -p hornvale-lab --lib census_claim`
Expected: PASS, including `the_status_line_names_the_job_kind_from_the_label`
— `status_line`'s behaviour must not change.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/lab/src/census_claim.rs
git commit -m "feat(the-timekeeper): expose the claim holder as data, not prose"
```

---

### Task 3: Parse `libtest-json-plus` into durations

**Files:**
- Create: `windows/lab/src/timings.rs`
- Modify: `windows/lab/src/lib.rs` (declare the module)

**Interfaces:**
- Consumes: `serde_json` (already a dependency of `hornvale-lab`).
- Produces:
  - `pub struct TestDuration { pub id: String, pub seconds: f64 }`
  - `pub fn parse_run(json_lines: &str) -> Result<Vec<TestDuration>, String>`
    — one entry per `{"type":"test","event":"ok"|"failed", …,"exec_time":…}`
    line, sorted by `id`. Non-test lines are ignored. **Errors** when zero
    test records are found.

- [ ] **Step 1: Write the failing test**

Create `windows/lab/src/timings.rs` containing only:

```rust
//! Recorded durations for the repo's own test suite (The Timekeeper).

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r#"{"type":"suite","event":"started","test_count":2}
{"type":"test","event":"started","name":"crate::bin$mod::alpha"}
{"type":"test","event":"ok","name":"crate::bin$mod::alpha","exec_time":0.25}
{"type":"test","event":"failed","name":"crate::bin$mod::beta","exec_time":1.5}
{"type":"suite","event":"ok","passed":1,"failed":1,"exec_time":1.75}
"#;

    #[test]
    fn parse_run_reads_every_test_record_and_ignores_the_rest() {
        let rows = parse_run(SAMPLE).expect("parses");
        assert_eq!(rows.len(), 2, "two test records, suite lines ignored");
        assert_eq!(rows[0].id, "crate::bin$mod::alpha");
        assert_eq!(rows[0].seconds, 0.25);
        assert_eq!(rows[1].id, "crate::bin$mod::beta");
        assert_eq!(rows[1].seconds, 1.5, "a failed test still has a duration");
    }

    #[test]
    fn parse_run_errors_rather_than_reporting_an_empty_run() {
        // An unrecognised shape must fail LOUDLY: libtest-json-plus is
        // experimental, and silently recording zero durations would make the
        // alarm permanently, invisibly green (spec A2).
        let err = parse_run(r#"{"type":"suite","event":"ok"}"#).unwrap_err();
        assert!(err.contains("no test records"), "got: {err}");
    }
}
```

- [ ] **Step 2: Declare the module and run the test**

Add to `windows/lab/src/lib.rs`, in alphabetical position among the existing
`pub mod` lines:

```rust
pub mod timings;
```

Run: `cargo test -p hornvale-lab --lib timings`
Expected: FAIL — `cannot find function 'parse_run'`.

- [ ] **Step 3: Implement**

Add above `mod tests` in `windows/lab/src/timings.rs`:

```rust
use serde_json::Value;

/// One test's measured wall time from a nextest run.
/// type-audit: bare-ok(identifier-text: id), bare-ok(seconds: seconds)
#[derive(Debug, Clone, PartialEq)]
pub struct TestDuration {
    /// Fully-qualified nextest test id, e.g. `hornvale-kernel::lib$mod::name`.
    pub id: String,
    /// Wall seconds the test took, as nextest reported it.
    pub seconds: f64,
}

/// Parse a `libtest-json-plus` stream into per-test durations, sorted by id.
///
/// Only `{"type":"test"}` records carrying an `exec_time` are kept; `suite`
/// lines and `started` events are ignored. A stream with no test records is an
/// ERROR, not an empty result: the format is experimental (spec A2), and a
/// silent empty parse would leave the alarm green forever.
/// type-audit: bare-ok(prose: json_lines)
pub fn parse_run(json_lines: &str) -> Result<Vec<TestDuration>, String> {
    let mut out = Vec::new();
    for line in json_lines.lines() {
        let line = line.trim();
        if line.is_empty() || !line.starts_with('{') {
            continue;
        }
        let Ok(v) = serde_json::from_str::<Value>(line) else {
            continue; // interleaved human output; not our business
        };
        if v.get("type").and_then(Value::as_str) != Some("test") {
            continue;
        }
        let (Some(id), Some(secs)) = (
            v.get("name").and_then(Value::as_str),
            v.get("exec_time").and_then(Value::as_f64),
        ) else {
            continue; // `started` events carry no exec_time
        };
        out.push(TestDuration { id: id.to_string(), seconds: secs });
    }
    if out.is_empty() {
        return Err(
            "no test records in the nextest stream — libtest-json-plus is \
             experimental and its shape may have changed; refusing to report \
             an empty run as a clean one"
                .to_string(),
        );
    }
    out.sort_by(|a, b| a.id.cmp(&b.id));
    Ok(out)
}
```

- [ ] **Step 4: Run the tests**

Run: `cargo test -p hornvale-lab --lib timings`
Expected: PASS, 2 tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/lab/src/timings.rs windows/lab/src/lib.rs
git commit -m "feat(the-timekeeper): parse nextest durations from libtest-json-plus"
```

---

### Task 4: The per-host baseline file

**Files:**
- Modify: `windows/lab/src/timings.rs`

**Interfaces:**
- Consumes: `TestDuration` and `parse_run` from Task 3.
- Produces:
  - `pub fn baseline_path(repo_root: &Path, host: &str) -> PathBuf` —
    `<root>/docs/timings/test-baseline-<host>.tsv`
  - `pub fn render_baseline(rows: &[TestDuration], sha: &str) -> String`
  - `pub fn parse_baseline(text: &str) -> Result<Vec<TestDuration>, String>`

Format, one row per test, tab-separated, sorted by id:
`<test-id>\t<seconds>\t<sha>`, with a leading `# ` comment header.

- [ ] **Step 1: Write the failing test**

Add to `mod tests` in `windows/lab/src/timings.rs`:

```rust
#[test]
fn a_baseline_round_trips() {
    let rows = vec![
        TestDuration { id: "b::two".into(), seconds: 2.5 },
        TestDuration { id: "a::one".into(), seconds: 0.125 },
    ];
    let text = render_baseline(&rows, "deadbeef");
    assert!(text.starts_with("# "), "carries a header comment");
    let back = parse_baseline(&text).expect("parses");
    assert_eq!(back.len(), 2);
    assert_eq!(back[0].id, "a::one", "rendered sorted by id");
    assert_eq!(back[0].seconds, 0.125);
    assert_eq!(back[1].id, "b::two");
}

#[test]
fn the_baseline_path_is_per_host() {
    let p = baseline_path(std::path::Path::new("/repo"), "lefford");
    assert_eq!(p, std::path::Path::new("/repo/docs/timings/test-baseline-lefford.tsv"));
}
```

- [ ] **Step 2: Run it and confirm it fails**

Run: `cargo test -p hornvale-lab --lib timings`
Expected: FAIL — `cannot find function 'render_baseline'`.

- [ ] **Step 3: Implement**

Add to `windows/lab/src/timings.rs`:

```rust
use std::fmt::Write as _;
use std::path::{Path, PathBuf};

/// Where this machine's baseline lives. One file PER HOST: the Mac and the
/// canonical box differ by roughly 4x, so a shared baseline would alarm on
/// every cross-machine run.
/// type-audit: bare-ok(identifier-text: host)
pub fn baseline_path(repo_root: &Path, host: &str) -> PathBuf {
    repo_root
        .join("docs/timings")
        .join(format!("test-baseline-{host}.tsv"))
}

/// Render a baseline: sorted, tab-separated, one row per test.
///
/// The file holds only the PRESENT; git holds the history, which is what makes
/// `git log -p` the archaeology tool (spec N1). Appending every run instead
/// would grow without bound.
/// type-audit: bare-ok(identifier-text: sha)
pub fn render_baseline(rows: &[TestDuration], sha: &str) -> String {
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| a.id.cmp(&b.id));
    let mut s = String::from(
        "# Hornvale test-duration baseline (The Timekeeper). One row per test:\n\
         # <test-id>\\t<seconds>\\t<sha-recorded-at>. Rewritten by `make ci`;\n\
         # history lives in git, so `git log -p` on this file is the record.\n",
    );
    for r in &sorted {
        let _ = writeln!(s, "{}\t{}\t{}", r.id, r.seconds, sha);
    }
    s
}

/// Parse a baseline back. Comment lines (`#`) and blanks are skipped; a
/// malformed data row is an error rather than a skipped line, so a corrupted
/// baseline cannot quietly disable the alarm.
/// type-audit: bare-ok(prose: text)
pub fn parse_baseline(text: &str) -> Result<Vec<TestDuration>, String> {
    let mut out = Vec::new();
    for (n, line) in text.lines().enumerate() {
        if line.trim().is_empty() || line.starts_with('#') {
            continue;
        }
        let mut parts = line.split('\t');
        let (Some(id), Some(secs)) = (parts.next(), parts.next()) else {
            return Err(format!("baseline line {}: expected <id>\\t<seconds>\\t<sha>", n + 1));
        };
        let seconds: f64 = secs
            .parse()
            .map_err(|_| format!("baseline line {}: '{secs}' is not a number", n + 1))?;
        out.push(TestDuration { id: id.to_string(), seconds });
    }
    out.sort_by(|a, b| a.id.cmp(&b.id));
    Ok(out)
}
```

- [ ] **Step 4: Run the tests**

Run: `cargo test -p hornvale-lab --lib timings`
Expected: PASS, 4 tests.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/lab/src/timings.rs
git commit -m "feat(the-timekeeper): the per-host duration baseline format"
```

---

### Task 5: The two alarms

**Files:**
- Modify: `windows/lab/src/timings.rs`

**Interfaces:**
- Consumes: `TestDuration` from Task 3.
- Produces:
  - `pub const PER_TEST_FLOOR_SECS: f64 = 5.0;`
  - `pub const PER_TEST_MULTIPLE: f64 = 2.0;`
  - `pub const SUITE_TOLERANCE: f64 = 0.25;`
  - `pub struct Shift { pub id: String, pub baseline: f64, pub current: f64 }`
  - `pub fn per_test_shifts(current: &[TestDuration], baseline: &[TestDuration]) -> Vec<Shift>`
  - `pub fn suite_shift(current: &[TestDuration], baseline: &[TestDuration]) -> Option<Shift>`
    (`id` is `"<whole suite>"`)

- [ ] **Step 1: Write the failing tests**

Add to `mod tests`:

```rust
fn d(id: &str, s: f64) -> TestDuration {
    TestDuration { id: id.into(), seconds: s }
}

#[test]
fn a_fast_test_doubling_is_below_the_floor_and_does_not_alarm() {
    // 7ms -> 15ms is scheduler noise across 2548 tests.
    let shifts = per_test_shifts(&[d("a", 0.015)], &[d("a", 0.007)]);
    assert!(shifts.is_empty(), "got {shifts:?}");
}

#[test]
fn a_slow_test_doubling_alarms() {
    let shifts = per_test_shifts(&[d("a", 20.0)], &[d("a", 6.0)]);
    assert_eq!(shifts.len(), 1);
    assert_eq!(shifts[0].id, "a");
}

#[test]
fn a_slow_test_growing_under_the_multiple_does_not_alarm() {
    let shifts = per_test_shifts(&[d("a", 9.0)], &[d("a", 6.0)]);
    assert!(shifts.is_empty(), "1.5x is under the 2x multiple");
}

#[test]
fn a_test_with_no_baseline_never_alarms() {
    let shifts = per_test_shifts(&[d("brand-new", 600.0)], &[]);
    assert!(shifts.is_empty(), "a new test has nothing to regress against");
}

#[test]
fn death_by_a_thousand_cuts_alarms_on_the_suite_total() {
    // No single test doubles; the suite grows 40%. This is the 234s -> 934s
    // shape, and the per-test alarm is structurally blind to it.
    let base: Vec<_> = (0..100).map(|i| d(&format!("t{i:03}"), 1.0)).collect();
    let now: Vec<_> = (0..100).map(|i| d(&format!("t{i:03}"), 1.4)).collect();
    assert!(per_test_shifts(&now, &base).is_empty(), "no single test alarms");
    let s = suite_shift(&now, &base).expect("the suite alarms");
    assert_eq!(s.id, "<whole suite>");
    assert_eq!(s.baseline, 100.0);
}

#[test]
fn a_suite_within_tolerance_does_not_alarm() {
    let base: Vec<_> = (0..100).map(|i| d(&format!("t{i:03}"), 1.0)).collect();
    let now: Vec<_> = (0..100).map(|i| d(&format!("t{i:03}"), 1.2)).collect();
    assert!(suite_shift(&now, &base).is_none(), "20% is under the 25% bound");
}

#[test]
fn an_empty_baseline_never_alarms_the_suite() {
    assert!(suite_shift(&[d("a", 99.0)], &[]).is_none(), "first run records only");
}
```

- [ ] **Step 2: Run and confirm they fail**

Run: `cargo test -p hornvale-lab --lib timings`
Expected: FAIL — `cannot find function 'per_test_shifts'`.

- [ ] **Step 3: Implement**

```rust
/// Below this, a doubling is scheduler noise: most of the suite runs in
/// single-digit milliseconds. CHOSEN, not derived (spec A1) — revisit against
/// the baseline's own spread once several runs exist.
/// type-audit: bare-ok(seconds)
pub const PER_TEST_FLOOR_SECS: f64 = 5.0;

/// A test must exceed this multiple of its baseline to alarm.
/// CHOSEN, not derived (spec A1).
/// type-audit: bare-ok(ratio)
pub const PER_TEST_MULTIPLE: f64 = 2.0;

/// Fractional growth of the whole suite's total that alarms.
/// CHOSEN, not derived (spec A1).
/// type-audit: bare-ok(ratio)
pub const SUITE_TOLERANCE: f64 = 0.25;

/// One duration that moved beyond tolerance.
/// type-audit: bare-ok(identifier-text: id), bare-ok(seconds: baseline), bare-ok(seconds: current)
#[derive(Debug, Clone, PartialEq)]
pub struct Shift {
    /// Test id, or `<whole suite>` for the aggregate.
    pub id: String,
    /// The recorded baseline, in seconds.
    pub baseline: f64,
    /// What this run measured, in seconds.
    pub current: f64,
}

fn lookup(rows: &[TestDuration], id: &str) -> Option<f64> {
    rows.binary_search_by(|r| r.id.as_str().cmp(id))
        .ok()
        .map(|i| rows[i].seconds)
}

/// Tests that crossed BOTH the absolute floor and the multiple. A test absent
/// from the baseline is new and never alarms.
pub fn per_test_shifts(current: &[TestDuration], baseline: &[TestDuration]) -> Vec<Shift> {
    let mut out = Vec::new();
    for c in current {
        let Some(b) = lookup(baseline, &c.id) else {
            continue;
        };
        if c.seconds >= PER_TEST_FLOOR_SECS && c.seconds > b * PER_TEST_MULTIPLE {
            out.push(Shift { id: c.id.clone(), baseline: b, current: c.seconds });
        }
    }
    out.sort_by(|a, b| b.current.total_cmp(&a.current));
    out
}

/// The aggregate alarm. Load-bearing and easy to omit: a suite can grow by
/// half without any single test doubling, which is the 234s -> 934s shape the
/// per-test alarm cannot see. An empty baseline is a first run: record only.
pub fn suite_shift(current: &[TestDuration], baseline: &[TestDuration]) -> Option<Shift> {
    if baseline.is_empty() {
        return None;
    }
    let now: f64 = current.iter().map(|r| r.seconds).sum();
    let was: f64 = baseline.iter().map(|r| r.seconds).sum();
    if was > 0.0 && now > was * (1.0 + SUITE_TOLERANCE) {
        return Some(Shift { id: "<whole suite>".to_string(), baseline: was, current: now });
    }
    None
}
```

- [ ] **Step 4: Run the tests**

Run: `cargo test -p hornvale-lab --lib timings`
Expected: PASS, 11 tests.

- [ ] **Step 5: Prove the alarm can actually fire (mutation check)**

Temporarily change `PER_TEST_FLOOR_SECS` to `100000.0`, re-run, and confirm
`a_slow_test_doubling_alarms` FAILS. Then restore it and confirm green.
Asserting an alarm exists is not asserting it alarms — this repo has shipped
tests that assert nothing.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add windows/lab/src/timings.rs
git commit -m "feat(the-timekeeper): per-test and whole-suite duration alarms"
```

---

### Task 6: The alarm test and the raw-output test

**Files:**
- Create: `cli/tests/timings_alarm.rs`

**Interfaces:**
- Consumes: `hornvale_lab::timings::{parse_run, parse_baseline, baseline_path,
  per_test_shifts, suite_shift}` (Tasks 3–5) and
  `hornvale_lab::census_claim::current_holder` (Task 2).
- Produces: the failing surface. Reads
  `target/nextest/ci/run.json` (written by `make ci`, Task 7) and the
  committed baseline.

- [ ] **Step 1: Write the test file**

```rust
//! The duration alarm (The Timekeeper) and the raw-output guarantee.
//!
//! This reads the JSON that `make ci` wrote for the run that just finished —
//! a test cannot observe its own suite's durations, so the alarm is a separate
//! pass over the previous step's artifact.

use hornvale_lab::census_claim::current_holder;
use hornvale_lab::timings::{
    baseline_path, parse_baseline, parse_run, per_test_shifts, suite_shift,
};
use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ has a parent")
        .to_path_buf()
}

fn run_json() -> PathBuf {
    repo_root().join("target/nextest/ci/run.json")
}

fn host() -> String {
    std::process::Command::new("hostname")
        .arg("-s")
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "unknown".to_string())
}

/// Whether THIS machine currently holds the box claim. Timing budgets are
/// meaningless under contention: The Siding measured `scene_api_cost`'s
/// genesis step at 19,722 ms contended and 3,818 ms quiet, a 5.2x swing that
/// would false-alarm every budget in the suite.
fn box_is_ours() -> bool {
    current_holder().is_some_and(|h| h.host == host())
}

#[test]
#[ignore = "timekeeper: reads the run.json `make ci` writes; not a standalone test"]
fn durations_have_not_regressed() {
    let json = std::fs::read_to_string(run_json()).unwrap_or_else(|e| {
        panic!(
            "no run at {} ({e}). This test reads what `make ci` wrote; run \
             `make ci`, not this test alone.",
            run_json().display()
        )
    });
    let current = parse_run(&json).expect("parse the nextest run");

    let path = baseline_path(&repo_root(), &host());
    let baseline = match std::fs::read_to_string(&path) {
        Ok(t) => parse_baseline(&t).expect("parse the baseline"),
        Err(_) => {
            eprintln!("no baseline at {} — first run on this host, recording only", path.display());
            Vec::new()
        }
    };

    let per_test = per_test_shifts(&current, &baseline);
    let suite = suite_shift(&current, &baseline);

    if !box_is_ours() {
        eprintln!(
            "timekeeper: this machine does not hold the box claim, so timings are \
             contended and NOT enforced. Observed {} per-test shift(s), suite shift: {}.",
            per_test.len(),
            suite.is_some()
        );
        return;
    }

    let mut problems = Vec::new();
    for s in &per_test {
        problems.push(format!(
            "  {} took {:.3}s against a {:.3}s baseline ({:.1}x)",
            s.id, s.current, s.baseline, s.current / s.baseline
        ));
    }
    if let Some(s) = &suite {
        problems.push(format!(
            "  <whole suite> took {:.1}s against a {:.1}s baseline (+{:.0}%)",
            s.current, s.baseline, (s.current / s.baseline - 1.0) * 100.0
        ));
    }
    assert!(
        problems.is_empty(),
        "test durations regressed on {}:\n{}\n\nIf this is intended, re-record \
         the baseline in the SAME commit that caused it — a deferred re-record \
         is how the census went stale for 139 commits.",
        host(),
        problems.join("\n")
    );
}

#[test]
#[ignore = "timekeeper: reads the run.json `make ci` writes; not a standalone test"]
fn the_raw_output_was_persisted() {
    // If we report summaries, the raw output must exist somewhere and be
    // verifiable — otherwise the summary is one more unverified check.
    let p = run_json();
    let text = std::fs::read_to_string(&p)
        .unwrap_or_else(|e| panic!("raw run output missing at {} ({e})", p.display()));
    assert!(!text.trim().is_empty(), "raw run output at {} is empty", p.display());
    let rows = parse_run(&text).expect("raw output parses");
    assert!(
        rows.iter().any(|r| r.id.contains("hornvale")),
        "raw output at {} names no hornvale tests — wrong file?",
        p.display()
    );
}
```

- [ ] **Step 2: Confirm both tests fail without a run**

Run: `cargo test -p hornvale --test timings_alarm -- --ignored`
Expected: both FAIL with "no run at …/run.json" / "raw run output missing".
That is the correct failure — they are useless without `make ci`, and they say
so.

- [ ] **Step 3: Commit**

```bash
cargo fmt
git add cli/tests/timings_alarm.rs
git commit -m "feat(the-timekeeper): the duration alarm and raw-output tests"
```

---

### Task 7: `make ci`, the baseline directory, and the docs

**Files:**
- Modify: `Makefile`
- Create: `docs/timings/.gitkeep`
- Modify: `CLAUDE.md`

**Interfaces:**
- Consumes: Tasks 1–6.
- Produces: `make ci` — runs the suite under the `ci` profile, persists
  `target/nextest/ci/run.json`, records the baseline, runs the alarm, prints a
  summary and the file list.

- [ ] **Step 1: Add the recipe**

Add to `.PHONY` on line 25: `ci`. Then add after `gate-full`:

```makefile
# The CI entry point. A WRAPPER: every decision it makes lives in Rust
# (windows/lab/src/timings.rs, cli/tests/timings_alarm.rs). Raw output is
# persisted before anything summarises it, so a surprise never costs a re-run.
ci: ## Run the suite under the ci profile, record durations, alarm on a shift
	@mkdir -p target/nextest/ci docs/timings
	NEXTEST_EXPERIMENTAL_LIBTEST_JSON=1 cargo nextest run --workspace \
	    --profile ci --message-format libtest-json-plus \
	    > target/nextest/ci/run.json 2> target/nextest/ci/run.log || true
	cargo run --quiet -p hornvale -- ci-record
	cargo test -q -p hornvale --test timings_alarm -- --ignored
	@echo ""
	@echo "== make ci: detail written to =="
	@echo "  target/nextest/ci/run.json   structured per-test durations"
	@echo "  target/nextest/ci/run.log    human output, including failures"
	@echo "  docs/timings/test-baseline-$$(hostname -s).tsv   recorded baseline"
```

`|| true` on the nextest line is deliberate: a failing suite must still leave
its JSON behind for the alarm and for archaeology. `run.log` failing the build
is the alarm test's job, not the redirect's.

- [ ] **Step 2: Add the `ci-record` subcommand**

In `cli/src/main.rs`, add this function beside the existing `cmd_lab_*`
functions:

```rust
/// Record this run's per-test durations as the host's baseline (The
/// Timekeeper). Reads what `make ci` just wrote; writes the rolling baseline
/// that `cli/tests/timings_alarm.rs` compares against.
fn cmd_ci_record() -> Result<(), String> {
    use hornvale_lab::timings::{baseline_path, parse_run, render_baseline};

    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .ok_or("cli/ has a parent")?
        .to_path_buf();
    let run = root.join("target/nextest/ci/run.json");
    let text = std::fs::read_to_string(&run)
        .map_err(|e| format!("ci-record: no run at {} ({e})", run.display()))?;
    let rows = parse_run(&text).map_err(|e| format!("ci-record: {e}"))?;

    let out = |cmd: &str, args: &[&str]| -> String {
        std::process::Command::new(cmd)
            .args(args)
            .output()
            .ok()
            .and_then(|o| String::from_utf8(o.stdout).ok())
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .unwrap_or_else(|| "unknown".to_string())
    };
    let sha = out("git", &["rev-parse", "--short", "HEAD"]);
    let host = out("hostname", &["-s"]);

    let path = baseline_path(&root, &host);
    if let Some(dir) = path.parent() {
        std::fs::create_dir_all(dir).map_err(|e| format!("ci-record: {e}"))?;
    }
    std::fs::write(&path, render_baseline(&rows, &sha))
        .map_err(|e| format!("ci-record: writing {}: {e}", path.display()))?;
    println!("ci-record: {} durations -> {}", rows.len(), path.display());
    Ok(())
}
```

Wire it into the subcommand match beside `Some("lab") => …`:

```rust
        Some("ci-record") => cmd_ci_record(),
```

and add one line to `usage()`, matching the surrounding style:

```
  hornvale ci-record                       record this run's durations as the host baseline
```

- [ ] **Step 3: Run it end to end**

Run: `make ci`
Expected: green; `target/nextest/ci/run.json` non-empty;
`docs/timings/test-baseline-<host>.tsv` created with ~2548 rows; the summary
lists all three files. The first run records only — no baseline exists yet, so
neither alarm can fire.

- [ ] **Step 4: Prove the alarm fires end to end**

```bash
python3 - <<'EOF'
import pathlib, subprocess
h = subprocess.run(["hostname","-s"],capture_output=True,text=True).stdout.strip()
p = pathlib.Path(f"docs/timings/test-baseline-{h}.tsv")
out = []
for line in p.read_text().splitlines():
    if line.startswith("#") or not line.strip():
        out.append(line); continue
    a, b, c = line.split("\t")
    out.append(f"{a}\t{float(b)/10}\t{c}")     # pretend everything was 10x faster
p.write_text("\n".join(out) + "\n")
EOF
make ci
```
Expected: **RED**, naming the suite shift and per-test shifts. Then
`git checkout -- docs/timings/` and re-run `make ci`: green.

This is the step that proves the alarm alarms. Do not skip it.

- [ ] **Step 5: Prove the contention gate suppresses enforcement**

Spec §4 requires that a run without the box claim records but does not
enforce, and nothing above tests it. With the baseline still divided by ten
from Step 4:

```bash
# No claim file -> box_is_ours() is false -> record, do not enforce.
HV_CENSUS_CLAIM_PATH=/tmp/hv-definitely-absent.claim make ci
```

Expected: **green**, with the stderr line `timekeeper: this machine does not
hold the box claim … NOT enforced` naming a non-zero shift count. Same input
that went red in Step 4, so this isolates the gate rather than the tolerance.
Then `git checkout -- docs/timings/`.

Without this step the suppression path could invert — enforcing when
contended, silent when exclusive — and every other check here would still pass.

- [ ] **Step 6: Document it**

In `CLAUDE.md`'s Commands section, beside the gate ladder, add `make ci` with
one line on what it writes and where, and note that the baseline is per host
and committed so `git log -p` is the archaeology.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add Makefile cli/src/main.rs CLAUDE.md docs/timings/
git commit -m "feat(the-timekeeper): make ci, the baseline recorder, and the docs"
```

---

## Definition of Done

Beyond the tasks above, use the `closing-a-campaign` skill: chronicle entry
(`book/src/chronicle/the-timekeeper.md` + `SUMMARY.md`), retrospective
(`docs/retrospectives/the-timekeeper.md`), freshness sweep, a decision record
ratifying the review loop (spec N1) and the observation/enforcement split (N4),
and A1's chosen tolerances carried forward as a follow-up with the data that
would settle them.
