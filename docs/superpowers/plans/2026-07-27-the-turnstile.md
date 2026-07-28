# The Turnstile Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** One heavy writer at a time on the canonical box, enforced at every entry point, with a wait that explains itself.

**Architecture:** A claim file at a fixed `/tmp` path is the single serialization point. Rust acquires it before an expensive run (not at publish — the contention lives in the compute phase) and releases it via `Drop`. The shell layers keep their `flock` but gain a bounded wait and a re-entrancy env var so the nested `census-run.sh` → `regenerate-artifacts.sh` → `lab run` path cannot deadlock against itself.

**Tech Stack:** Rust edition 2024, bash. No new dependencies (`serde`/`serde_json` only, enforced by `cli/tests/architecture.rs`).

## Global Constraints

- **Decision [0081](../../decisions/0081-one-heavy-writer-per-box-claimed-at-the-write-seam.md) governs.** One heavy writer per box; global claim, not per-directory; blocking with a bounded wait; scope drawn by cost.
- **This campaign changes nothing a run computes.** A census regen under the claim must still produce a zero diff. If any golden moves, stop.
- **`std::time::Instant`/`SystemTime` are disallowed types** (`clippy.toml`, decision 0001). The claim module needs them and gets a scoped `#[allow(clippy::disallowed_types)]` **with a comment**, exactly as `hornvale_worldgen::profiled` does. CI runs clippy with `-D warnings`.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only.
- **Every crate sets `#![warn(missing_docs)]`** — every new public item, field, and variant needs a one-line doc comment.
- **Silence is a bug, not a cosmetic gap.** A waiting run that prints nothing is indistinguishable from a hang; tests assert the messages exist.
- **Run `cargo fmt` as the final step before every commit.**
- Worktree `~/.config/superpowers/worktrees/hornvale/the-turnstile`, branch `the-turnstile`.

---

### Task 1: The claim module

**Files:**
- Create: `windows/lab/src/census_claim.rs`
- Modify: `windows/lab/src/lib.rs` (add `mod census_claim;` + re-exports)

**Interfaces:**
- Produces: `pub fn claims_the_box(world_builds: u64, writes_census_goldens: bool) -> bool`; `pub struct Claim` (RAII, releases on `Drop`); `pub fn acquire(label: &str, goldens: &Path, world_builds: u64, writes_census_goldens: bool) -> Result<Option<Claim>, String>` (returns `Ok(None)` when no claim is needed or when already serialized by an ancestor); `pub fn status_line() -> String`. Task 2 consumes all four.

- [ ] **Step 1: Write the failing tests**

Create `windows/lab/src/census_claim.rs` with the test module only for now, so the tests name the API before it exists:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_cost_threshold_draws_the_line_at_200_builds() {
        assert!(!claims_the_box(50, false), "the-chorus (50) must not claim");
        assert!(claims_the_box(200, false), "the perf probe (200) claims");
        assert!(claims_the_box(1000, false), "the-census (1000) claims");
        // Census goldens claim at ANY size: the correctness half is
        // independent of the cost threshold (spec 1.1).
        assert!(claims_the_box(1, true), "a census-goldens write always claims");
    }

    #[test]
    fn a_claim_naming_a_dead_pid_is_stale() {
        // PID 1 is always alive; u32::MAX never is.
        assert!(pid_is_alive(1));
        assert!(!pid_is_alive(u32::MAX));
    }

    #[test]
    fn a_claim_round_trips_through_its_file_format() {
        let rendered = render_claim(&ClaimInfo {
            pid: 4242,
            host: "lefford".to_string(),
            user: "nathan".to_string(),
            started: "2026-07-27T18:53:41Z".to_string(),
            goldens: "/tmp/x/book/src/laboratory/generated".to_string(),
            label: "the-census".to_string(),
            reference: "the-hoist@94bcc07a".to_string(),
            cmdline: "hornvale lab run studies/the-census.study.json".to_string(),
        });
        let parsed = parse_claim(&rendered).expect("a rendered claim parses");
        assert_eq!(parsed.pid, 4242);
        assert_eq!(parsed.host, "lefford");
        assert_eq!(parsed.label, "the-census");
        assert_eq!(parsed.started, "2026-07-27T18:53:41Z");
    }

    #[test]
    fn a_partial_or_corrupt_claim_parses_to_none_rather_than_panicking() {
        assert!(parse_claim("").is_none());
        assert!(parse_claim("pid=notanumber\n").is_none());
        // A truncated write (killed mid-write) must not wedge the box.
        assert!(parse_claim("pid=42\nhost=lefford\n").is_none());
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-lab census_claim`
Expected: FAIL to compile — `cannot find function claims_the_box` etc.

- [ ] **Step 3: Write the module**

Prepend to the same file, above the test module:

```rust
//! One heavy writer per box (decision 0081).
//!
//! `scripts/census-run.sh` has always taken `/tmp/hv-census.lock`, but it is
//! one of three ways to start an expensive run; the other two take no lock.
//! This module is the seam every entry point inherits, the same way
//! [`crate::census_guard`] is for the canonical-host check — and for the same
//! stated reason: a shell wrapper cannot guard `cargo run -p hornvale -- lab
//! run <study>`.
//!
//! The claim is GLOBAL, not per-directory: two runs in different worktrees
//! are independent in their output and not in their clock, and the binding
//! constraint is the machine (decision 0081).

use std::fs;
use std::io::Write as _;
use std::path::Path;
// The claim is scheduling infrastructure, not world state: it measures how
// long a run waited and stamps when a hold began. It never reads `WorldTime`,
// never touches a fact, and never reaches an artifact, so it carries the same
// wall-clock exemption `hornvale_worldgen::profiled` does (clippy.toml /
// decision 0001).
#[allow(clippy::disallowed_types)]
use std::time::{Duration, Instant};

/// Where the claim lives. Distinct from `/tmp/hv-census.lock`, which the
/// shell layer still flocks — the two compose (shell queues, Rust claims).
pub const CLAIM_PATH: &str = "/tmp/hv-census.claim";

/// Projected world-builds at or above which a run claims the box: roughly 40
/// seconds of exclusive time at ~0.2s per build on 38 cores. CHOSEN, not
/// derived — revisit against `docs/timings.md` once it carries census rows
/// (decision 0081, Consequences).
pub const CLAIM_BUILDS_THRESHOLD: u64 = 200;

/// Default bound on the wait, in seconds (45 minutes). Override with
/// `HV_CENSUS_WAIT_TIMEOUT`. Must exceed the longest legitimate hold: a full
/// `HV_CENSUS=1` regen is ~12 min and a queue two deep ~24.
pub const DEFAULT_TIMEOUT_SECS: u64 = 2700;

/// Env var by which an outer shell announces it already holds the lock, so a
/// nested run does not block against its own ancestor. Names the holder PID.
pub const LOCK_HELD_ENV: &str = "HV_CENSUS_LOCK_HELD";

const POLL_SECS: u64 = 3;
const PROGRESS_SECS: u64 = 60;

/// The context a blocked caller needs: who holds the box, since when, doing
/// what to which tree, from which code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClaimInfo {
    /// Holder process id.
    pub pid: u32,
    /// Holder short hostname.
    pub host: String,
    /// Holder unix user.
    pub user: String,
    /// ISO-8601 UTC instant the hold began.
    pub started: String,
    /// Absolute goldens directory being written.
    pub goldens: String,
    /// Study or job label.
    pub label: String,
    /// `branch@commit` the holder is running — "someone else's branch" is a
    /// materially different situation from "main".
    pub reference: String,
    /// The holder's command line.
    pub cmdline: String,
}

/// True when a run must serialize: expensive by projected cost, or writing
/// census goldens at any size (decision 0081).
pub fn claims_the_box(world_builds: u64, writes_census_goldens: bool) -> bool {
    writes_census_goldens || world_builds >= CLAIM_BUILDS_THRESHOLD
}

/// Render a claim as `key=value` lines — `/tmp` scratch, greppable by eye and
/// by script, no serde.
fn render_claim(info: &ClaimInfo) -> String {
    format!(
        "pid={}\nhost={}\nuser={}\nstarted={}\ngoldens={}\nlabel={}\nref={}\ncmdline={}\n",
        info.pid,
        info.host,
        info.user,
        info.started,
        info.goldens,
        info.label,
        info.reference,
        info.cmdline
    )
}

/// Parse a claim file. `None` for anything malformed or truncated — a run
/// killed mid-write must not wedge the box behind an unreadable claim.
fn parse_claim(text: &str) -> Option<ClaimInfo> {
    let mut pid = None;
    let (mut host, mut user, mut started) = (None, None, None);
    let (mut goldens, mut label, mut reference, mut cmdline) = (None, None, None, None);
    for line in text.lines() {
        let (key, value) = line.split_once('=')?;
        let value = value.to_string();
        match key {
            "pid" => pid = value.parse::<u32>().ok(),
            "host" => host = Some(value),
            "user" => user = Some(value),
            "started" => started = Some(value),
            "goldens" => goldens = Some(value),
            "label" => label = Some(value),
            "ref" => reference = Some(value),
            "cmdline" => cmdline = Some(value),
            _ => {}
        }
    }
    Some(ClaimInfo {
        pid: pid?,
        host: host?,
        user: user?,
        started: started?,
        goldens: goldens?,
        label: label?,
        reference: reference?,
        cmdline: cmdline?,
    })
}

/// Is `pid` a live process? Linux `/proc`; decision 0063 makes this box the
/// single golden-authoring platform, so Linux-only is acceptable here.
fn pid_is_alive(pid: u32) -> bool {
    Path::new(&format!("/proc/{pid}")).exists()
}
```

- [ ] **Step 4: Run the tests**

Run: `cargo test -p hornvale-lab census_claim`
Expected: PASS, 4 tests.

- [ ] **Step 5: Register the module**

In `windows/lab/src/lib.rs`, beside the existing `census_guard` declaration:

```rust
pub mod census_claim;
```

Match the existing style — if `census_guard` is declared `mod census_guard;` with selective `pub use`, do the same and re-export `claims_the_box`, `acquire`, `status_line`, `Claim`, `ClaimInfo`.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add windows/lab/src/census_claim.rs windows/lab/src/lib.rs
git commit -m "feat(lab): the census claim's file format and cost threshold"
```

---

### Task 2: Acquiring, waiting, and releasing

**Files:**
- Modify: `windows/lab/src/census_claim.rs`

**Interfaces:**
- Consumes: `ClaimInfo`, `parse_claim`, `render_claim`, `pid_is_alive` from Task 1.
- Produces: `pub struct Claim`, `pub fn acquire(...) -> Result<Option<Claim>, String>`, `pub fn status_line() -> String`. Task 3 consumes `status_line`; Task 4 consumes `Claim::waited_secs`.

- [ ] **Step 1: Write the failing tests**

Append to the test module. These use an overridable path so they never touch the real claim:

```rust
    #[test]
    fn a_live_ancestor_holding_the_lock_makes_a_claim_a_no_op() {
        // The nested path census-run.sh -> regenerate-artifacts.sh -> lab run
        // must NOT block against its own ancestor. This is the deadlock guard.
        let me = std::process::id();
        assert!(
            already_serialized_by(&me.to_string()),
            "our own pid must count as a live ancestor"
        );
        assert!(!already_serialized_by(&u32::MAX.to_string()));
        assert!(!already_serialized_by(""));
    }

    #[test]
    fn a_stale_claim_is_taken_over_rather_than_waited_on() {
        let dir = std::env::temp_dir().join(format!("hv-claim-test-{}", std::process::id()));
        let _ = fs::create_dir_all(&dir);
        let path = dir.join("stale.claim");
        fs::write(
            &path,
            render_claim(&ClaimInfo {
                pid: u32::MAX, // never alive
                host: "lefford".into(),
                user: "nathan".into(),
                started: "2026-07-27T18:53:41Z".into(),
                goldens: "/tmp/x".into(),
                label: "the-census".into(),
                reference: "main@deadbeef".into(),
                cmdline: "hornvale lab run".into(),
            }),
        )
        .unwrap();
        // A stale claim must be reported as absent, so acquisition proceeds.
        assert!(live_holder_at(&path).is_none());
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn a_live_claim_is_reported_with_its_context() {
        let dir = std::env::temp_dir().join(format!("hv-claim-live-{}", std::process::id()));
        let _ = fs::create_dir_all(&dir);
        let path = dir.join("live.claim");
        fs::write(
            &path,
            render_claim(&ClaimInfo {
                pid: std::process::id(), // alive by construction
                host: "lefford".into(),
                user: "nathan".into(),
                started: "2026-07-27T18:53:41Z".into(),
                goldens: "/tmp/x".into(),
                label: "census-of-the-meeting".into(),
                reference: "the-hoist@94bcc07a".into(),
                cmdline: "hornvale lab run".into(),
            }),
        )
        .unwrap();
        let held = live_holder_at(&path).expect("a live claim is reported");
        assert_eq!(held.label, "census-of-the-meeting");
        assert_eq!(held.reference, "the-hoist@94bcc07a");
        let _ = fs::remove_dir_all(&dir);
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-lab census_claim`
Expected: FAIL — `cannot find function already_serialized_by` / `live_holder_at`.

- [ ] **Step 3: Implement re-entrancy and holder inspection**

```rust
/// Walk this process's ancestor chain via `/proc/<pid>/status`'s `PPid`.
fn is_ancestor(candidate: u32) -> bool {
    let mut pid = std::process::id();
    for _ in 0..64 {
        if pid == candidate {
            return true;
        }
        let status = match fs::read_to_string(format!("/proc/{pid}/status")) {
            Ok(text) => text,
            Err(_) => return false,
        };
        let ppid = status
            .lines()
            .find_map(|l| l.strip_prefix("PPid:"))
            .and_then(|v| v.trim().parse::<u32>().ok());
        match ppid {
            Some(0) | None => return false,
            Some(next) => pid = next,
        }
    }
    false
}

/// True when an outer shell already holds the lock on our behalf. Requires
/// the named pid to be BOTH alive and an ancestor — a stale exported variable
/// from an unrelated shell must not silently disable serialization.
fn already_serialized_by(env_value: &str) -> bool {
    match env_value.trim().parse::<u32>() {
        Ok(pid) => pid_is_alive(pid) && is_ancestor(pid),
        Err(_) => false,
    }
}

/// The claim at `path` if one is held by a LIVE process; `None` if absent,
/// malformed, or stale (holder no longer alive).
fn live_holder_at(path: &Path) -> Option<ClaimInfo> {
    let text = fs::read_to_string(path).ok()?;
    let info = parse_claim(&text)?;
    if pid_is_alive(info.pid) { Some(info) } else { None }
}
```

- [ ] **Step 4: Run the tests**

Run: `cargo test -p hornvale-lab census_claim`
Expected: PASS, 7 tests.

- [ ] **Step 5: Implement acquire/release**

```rust
/// A held claim. Releases on drop — including on the error and panic paths,
/// which is why this is RAII rather than an explicit `release()` a caller can
/// forget on an early return.
pub struct Claim {
    path: std::path::PathBuf,
    waited: Duration,
}

impl Claim {
    /// How long this run waited in the queue before starting. Feeds the
    /// timing ledger's `waited_s`, which is what separates "queued" from
    /// "slow" after the fact.
    pub fn waited_secs(&self) -> u64 {
        self.waited.as_secs()
    }
}

impl Drop for Claim {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
    }
}

/// Acquire the box for an expensive run, blocking until it is free.
///
/// Returns `Ok(None)` when no claim is needed — the run is below the cost
/// threshold and writes no census goldens — or when an ancestor already holds
/// the lock. Returns `Err` only on timeout, whose message names the holder
/// and what to do about it.
pub fn acquire(
    label: &str,
    goldens: &Path,
    world_builds: u64,
    writes_census_goldens: bool,
) -> Result<Option<Claim>, String> {
    if !claims_the_box(world_builds, writes_census_goldens) {
        return Ok(None);
    }
    if let Ok(held) = std::env::var(LOCK_HELD_ENV)
        && already_serialized_by(&held)
    {
        return Ok(None);
    }

    let path = std::path::PathBuf::from(
        std::env::var("HV_CENSUS_CLAIM_PATH").unwrap_or_else(|_| CLAIM_PATH.to_string()),
    );
    let timeout = Duration::from_secs(
        std::env::var("HV_CENSUS_WAIT_TIMEOUT")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(DEFAULT_TIMEOUT_SECS),
    );

    let began = Instant::now();
    let mut announced = false;
    let mut last_progress = Instant::now();
    loop {
        match live_holder_at(&path) {
            None => {
                // Free, or stale. Announce a takeover so a crashed run leaves
                // a trace rather than silently vanishing.
                if path.exists() {
                    eprintln!(
                        "census-claim: taking over a STALE claim at {} — its holder is no longer alive.",
                        path.display()
                    );
                    let _ = fs::remove_file(&path);
                }
                match fs::OpenOptions::new().create_new(true).write(true).open(&path) {
                    Ok(mut file) => {
                        let info = ClaimInfo {
                            pid: std::process::id(),
                            host: crate::census_guard::current_hostname(),
                            user: std::env::var("USER").unwrap_or_default(),
                            started: now_iso(),
                            goldens: goldens.display().to_string(),
                            label: label.to_string(),
                            reference: git_reference(),
                            cmdline: std::env::args().collect::<Vec<_>>().join(" "),
                        };
                        file.write_all(render_claim(&info).as_bytes())
                            .map_err(|e| format!("census-claim: cannot write {}: {e}", path.display()))?;
                        let waited = began.elapsed();
                        if announced {
                            eprintln!("census-claim: acquired after {}.", human(waited));
                        }
                        return Ok(Some(Claim { path, waited }));
                    }
                    // Lost a race to another acquirer; fall through and wait.
                    Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {}
                    Err(e) => {
                        return Err(format!("census-claim: cannot create {}: {e}", path.display()));
                    }
                }
            }
            Some(holder) => {
                if !announced {
                    // Immediately, never a silent pause: an unexplained pause
                    // on a 40-core box is exactly what a hang looks like.
                    eprintln!(
                        "census-claim: waiting — held by pid {} ({}@{}) since {}, writing {}, running {} @ {}. Waiting up to {}.",
                        holder.pid,
                        holder.user,
                        holder.host,
                        holder.started,
                        holder.goldens,
                        holder.label,
                        holder.reference,
                        human(timeout)
                    );
                    announced = true;
                    last_progress = Instant::now();
                } else if last_progress.elapsed() >= Duration::from_secs(PROGRESS_SECS) {
                    eprintln!(
                        "census-claim: still waiting ({} elapsed; holder pid {} started {}).",
                        human(began.elapsed()),
                        holder.pid,
                        holder.started
                    );
                    last_progress = Instant::now();
                }
                if began.elapsed() >= timeout {
                    return Err(format!(
                        "census-claim: TIMED OUT after {} waiting for pid {} ({}@{}, started {}, running {}). \
                         Inspect: ps -p {} -o pid,etimes,args. If it is dead, remove {}.",
                        human(timeout),
                        holder.pid,
                        holder.user,
                        holder.host,
                        holder.started,
                        holder.label,
                        holder.pid,
                        path.display()
                    ));
                }
            }
        }
        std::thread::sleep(Duration::from_secs(POLL_SECS));
    }
}

/// `1h02m03s` / `6m30s` / `12s` — compact and unambiguous in a log line.
fn human(d: Duration) -> String {
    let s = d.as_secs();
    if s >= 3600 {
        format!("{}h{:02}m{:02}s", s / 3600, (s % 3600) / 60, s % 60)
    } else if s >= 60 {
        format!("{}m{:02}s", s / 60, s % 60)
    } else {
        format!("{s}s")
    }
}

/// UTC ISO-8601, via `date` for the same reason `current_hostname` shells to
/// `hostname`: no date arithmetic and no dependency.
fn now_iso() -> String {
    std::process::Command::new("date")
        .args(["-u", "+%Y-%m-%dT%H:%M:%SZ"])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .map(|s| s.trim().to_string())
        .unwrap_or_default()
}

/// `branch@short-commit`, or `-` outside a work tree.
fn git_reference() -> String {
    let run = |args: &[&str]| {
        std::process::Command::new("git")
            .args(args)
            .output()
            .ok()
            .filter(|o| o.status.success())
            .and_then(|o| String::from_utf8(o.stdout).ok())
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
    };
    match (
        run(&["branch", "--show-current"]),
        run(&["rev-parse", "--short", "HEAD"]),
    ) {
        (Some(b), Some(c)) => format!("{b}@{c}"),
        _ => "-".to_string(),
    }
}

/// One line describing the current claim, for `census-run.sh status`.
pub fn status_line() -> String {
    let path = std::path::PathBuf::from(
        std::env::var("HV_CENSUS_CLAIM_PATH").unwrap_or_else(|_| CLAIM_PATH.to_string()),
    );
    match live_holder_at(&path) {
        None => "no census running".to_string(),
        Some(h) => format!(
            "census running: pid {} ({}@{}) since {}, writing {}, running {} @ {}",
            h.pid, h.user, h.host, h.started, h.goldens, h.label, h.reference
        ),
    }
}
```

**Note:** `current_hostname` is `pub` in `census_guard`; if it is not, make it `pub(crate)` rather than duplicating it — one hostname reader, per that module's own "one file, two readers" note.

- [ ] **Step 6: Write the timeout test**

```rust
    #[test]
    fn a_timeout_names_the_holder_and_what_to_do() {
        let dir = std::env::temp_dir().join(format!("hv-claim-to-{}", std::process::id()));
        let _ = fs::create_dir_all(&dir);
        let path = dir.join("busy.claim");
        fs::write(&path, render_claim(&ClaimInfo {
            pid: std::process::id(), // alive, so never released
            host: "lefford".into(), user: "nathan".into(),
            started: "2026-07-27T18:53:41Z".into(), goldens: "/tmp/x".into(),
            label: "the-census".into(), reference: "main@deadbeef".into(),
            cmdline: "hornvale lab run".into(),
        })).unwrap();

        // SAFETY: single-threaded test process; these drive the code under test.
        unsafe {
            std::env::set_var("HV_CENSUS_CLAIM_PATH", &path);
            std::env::set_var("HV_CENSUS_WAIT_TIMEOUT", "1");
        }
        let err = acquire("the-census", Path::new("/tmp/x"), 1000, true)
            .expect_err("a held claim must time out, not succeed");
        assert!(err.contains("TIMED OUT"), "message: {err}");
        assert!(err.contains(&std::process::id().to_string()), "names the holder: {err}");
        assert!(err.contains("remove"), "says what to do: {err}");
        unsafe {
            std::env::remove_var("HV_CENSUS_CLAIM_PATH");
            std::env::remove_var("HV_CENSUS_WAIT_TIMEOUT");
        }
        let _ = fs::remove_dir_all(&dir);
    }
```

If env-var mutation in tests proves awkward under `cargo nextest` (process-per-test makes it safe, but the `unsafe` blocks are edition-2024 noise), the alternative is to give `acquire` an internal `acquire_at(path, timeout, ...)` taking both explicitly and have the public `acquire` read the env. Prefer that if the test fights you.

- [ ] **Step 7: Run and commit**

Run: `cargo test -p hornvale-lab census_claim`
Expected: PASS, 8 tests.

```bash
cargo fmt
git add windows/lab/src/census_claim.rs
git commit -m "feat(lab): acquire, wait, and release the census claim"
```

---

### Task 3: Wire the claim into the run path

**Files:**
- Modify: `cli/src/main.rs` (`cmd_lab_run`, ~:983-1010)
- Modify: `windows/lab/src/study.rs` (add a projected-cost accessor if none exists)

**Interfaces:**
- Consumes: `acquire`, `Claim` from Task 2.
- Produces: a claim held across the whole run; `Claim::waited_secs()` available for Task 5.

- [ ] **Step 1: Add the projected-cost accessor**

In `windows/lab/src/study.rs`, on `Study`:

```rust
    /// Projected world-builds for this study: one per seed per pin set. Known
    /// before the run, which is what lets the claim decide by cost rather
    /// than by name (decision 0081).
    pub fn projected_world_builds(&self) -> u64 {
        self.seeds.count * self.pin_sets.len() as u64
    }
```

Check the real field names first — `grep -n "pub struct Study" -A 12 windows/lab/src/study.rs` — and match them.

- [ ] **Step 2: Acquire before the run, beside the host check**

In `cli/src/main.rs::cmd_lab_run`, immediately after the existing `require_canonical_host_for` call (which is already there "to fail fast, before spending any compute"):

```rust
    // Serialize expensive runs on this box (decision 0081). Held across the
    // WHOLE run, not just `publish`: the contention lives in the compute
    // phase, so claiming only around the write would leave the real problem
    // untouched. Released on drop, including on the error paths below.
    let claim = hornvale_lab::census_claim::acquire(
        &study.name,
        goldens_dir,
        study.projected_world_builds(),
        hornvale_lab::is_census_study(&study.name),
    )?;
```

`is_census_study` is currently private to `census_guard`; make it `pub` (it is already the module's own notion of "writes census goldens").

- [ ] **Step 3: Report the wait in the run's tally**

Extend the existing `println!` at the end of `cmd_lab_run` so a queued run says so:

```rust
    let waited = claim
        .as_ref()
        .map(|c| c.waited_secs())
        .filter(|s| *s > 0)
        .map(|s| format!(" (queued {s}s)"))
        .unwrap_or_default();
    println!(
        "study {}: {} rows, {} refusals; summary + {} charts published.{}",
        result.study.name,
        result.rows.len(),
        refusals,
        charts,
        waited
    );
```

Keep the existing field names for rows/refusals/charts — read the current statement and extend it rather than retyping it.

- [ ] **Step 4: Verify the claim covers the run, and does not deadlock**

```bash
# A small study must NOT claim (below threshold, no census goldens):
cargo run --release -p hornvale -- lab run studies/the-chorus.study.json
# Expect: no census-claim lines at all.

# Re-entrancy: a live ancestor pid disables the claim.
HV_CENSUS_LOCK_HELD=$$ cargo run --release -p hornvale -- lab run studies/the-chorus.study.json
# Expect: still no claim lines, and no hang.
```

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add cli/src/main.rs windows/lab/src/study.rs windows/lab/src/census_guard.rs
git commit -m "feat(cli): claim the box for expensive lab runs"
```

---

### Task 4: The shell layer

**Files:**
- Modify: `scripts/census-run.sh` (export the env var; add `status`; bound the flock)
- Modify: `scripts/regenerate-artifacts.sh` (re-entrant flock beside the host guard)
- Modify: `Makefile` (advisory note on `gate`/`gate-full`; fix the stale `rebaseline` help text)

- [ ] **Step 1: Export the re-entrancy variable and bound the wait in `census-run.sh`**

Replace the bare `flock 9` with a bounded wait, and export the holder pid so nested layers do not block against it:

```bash
timeout_s="${HV_CENSUS_WAIT_TIMEOUT:-2700}"
echo "census-run: waiting for the census lock ($LOCK; up to ${timeout_s}s) …" >&2
if ! flock -w "$timeout_s" 9; then
    echo "census-run: TIMED OUT after ${timeout_s}s waiting for the census lock." >&2
    echo "census-run: current claim — $(cargo run --quiet --release -p hornvale -- lab claim-status 2>/dev/null || echo 'unknown')" >&2
    exit 75
fi
echo "census-run: lock acquired at $(date -Is)" >&2
# Announce the hold so the nested regenerate-artifacts.sh -> lab run path does
# not block against its own ancestor (decision 0081).
export HV_CENSUS_LOCK_HELD=$$
```

- [ ] **Step 2: Add the `status` subcommand**

Near the top of `census-run.sh`, before the lock is taken:

```bash
if [ "${1:-}" = "status" ]; then
    cargo run --quiet --release -p hornvale -- lab claim-status
    exit $?
fi
```

Add the matching `lab claim-status` subcommand in `cli/src/main.rs`, printing `hornvale_lab::census_claim::status_line()`, and list it in `lab`'s usage string beside `run`/`diff`/`list-metrics`.

- [ ] **Step 3: The re-entrant flock in `regenerate-artifacts.sh`**

Beside the host guard already hoisted to the top:

```bash
if [ "${HV_CENSUS:-0}" = 1 ] && [ "${SKIP_CENSUS:-0}" != 1 ]; then
    . "$(dirname "$0")/census-canonical-host.sh"
    require_canonical_census_host || exit 1
    # Serialize with any other census on this box (decision 0081) — unless an
    # ancestor already holds the lock, in which case re-flocking a fresh fd
    # would DEADLOCK against our own parent (flock is per open-file-description).
    if [ -z "${HV_CENSUS_LOCK_HELD:-}" ] || ! kill -0 "${HV_CENSUS_LOCK_HELD}" 2>/dev/null; then
        exec 9>"${HV_CENSUS_LOCK:-/tmp/hv-census.lock}"
        timeout_s="${HV_CENSUS_WAIT_TIMEOUT:-2700}"
        echo "regenerate-artifacts: waiting for the census lock (up to ${timeout_s}s) …" >&2
        flock -w "$timeout_s" 9 || {
            echo "regenerate-artifacts: TIMED OUT waiting for the census lock." >&2
            exit 75
        }
        export HV_CENSUS_LOCK_HELD=$$
    fi
fi
```

- [ ] **Step 4: Advisory note on the gates, and the stale help text**

In the `Makefile`, before `gate` and `gate-full` run their work:

```make
	@bash scripts/census-advisory.sh || true
```

Create `scripts/census-advisory.sh` — advisory only, never blocking, never failing the build:

```bash
#!/usr/bin/env bash
# Advisory only (decision 0081): a gate is not a measurement, and a developer
# waiting 12 minutes to START a 4-minute gate is worse than the contention.
# Print the context and get out of the way.
set -uo pipefail
status="$(cargo run --quiet --release -p hornvale -- lab claim-status 2>/dev/null || true)"
case "$status" in
    "census running"*) echo "note: $status — your timings will be contended." >&2 ;;
esac
exit 0
```

Fix `Makefile:77`'s help text, which still asserts the pre-0063 world:

```make
rebaseline artifacts: ## Regenerate committed artifacts EXCEPT censuses (refresh those with scripts/census-run.sh)
```

- [ ] **Step 5: Test the shell layer, including the deadlock case**

```bash
shellcheck scripts/census-run.sh scripts/regenerate-artifacts.sh scripts/census-advisory.sh

# The deadlock guard: an ancestor holding the lock must not block the child.
( exec 9>/tmp/hv-census.lock; flock 9; HV_CENSUS_LOCK_HELD=$$ SKIP_CENSUS=1 \
  timeout 60 bash scripts/regenerate-artifacts.sh >/dev/null && echo "NO DEADLOCK" )
# Expect: "NO DEADLOCK" well inside 60s.

# Status with nothing running:
bash scripts/census-run.sh status
# Expect: "no census running"
```

- [ ] **Step 6: Commit**

```bash
git add scripts/ Makefile cli/src/main.rs
git commit -m "feat(scripts): bounded, re-entrant census lock plus a status query"
```

---

### Task 5: Censuses enter the timing ledger

**Files:**
- Modify: `scripts/timed.sh` (add the `waited_s` column)
- Modify: `scripts/census-run.sh` (wrap the regen in `timed.sh`)
- Modify: `docs/timings.md` (header row + column doc)

- [ ] **Step 1: Add the column**

`timed.sh` gains `waited_s`, read from `HV_CENSUS_WAITED_S` (exported by the claimant) and defaulting to `0`:

```bash
    printf '| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n' \
        "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$label" "$real" "$user" "$sys" "$ratio" \
        "${HV_CENSUS_WAITED_S:-0}" \
        "$(git rev-parse --short HEAD 2>/dev/null || echo '-')" \
        ...
```

Update `init_ledger`'s header row and the prose to document `waited_s`:

> `waited_s` is time spent queued behind another heavy run (decision 0081),
> not work. It is what separates a *queued* run from a *slow* one — a row
> reading `wall=1240 waited_s=620` explains itself; the same row without the
> column is a mystery.

Existing rows have ten columns and the header now has eleven. Markdown tolerates this, and the file is explicitly *not* drift-checked ("a record you read"), so leave history untouched rather than backfilling zeros into rows that predate the column.

- [ ] **Step 2: Wrap the census regen**

In `census-run.sh`, replace the bare invocation:

```bash
    HV_CENSUS=1 bash scripts/timed.sh census -- bash scripts/regenerate-artifacts.sh
```

- [ ] **Step 3: Verify a row appears**

```bash
SKIP_CENSUS=1 bash scripts/timed.sh census-smoke -- true
tail -2 docs/timings.md
```

Expected: a new row whose `waited_s` column is `0`.

- [ ] **Step 4: Commit**

```bash
git add scripts/timed.sh scripts/census-run.sh docs/timings.md
git commit -m "feat(timings): record censuses, and split queued from slow"
```

---

### Task 6: The documentation sweep

**Files (live instructional docs only):**
- `CLAUDE.md` (lines ~26 and ~71)
- `.claude/skills/closing-a-campaign/SKILL.md`
- `.claude/skills/dispatching-hornvale-subagents/dispatch-preamble.md`
- `book/src/laboratory/overview.md`
- `scripts/census-run.sh`, `scripts/regenerate-artifacts.sh` (header comments)
- `docs/decisions/0063-*.md`, `docs/decisions/0079-*.md` (pointer lines only)

- [ ] **Step 1: Name the locked path everywhere it is instructional**

Replace every *instruction* to run `HV_CENSUS=1 bash scripts/regenerate-artifacts.sh` with `scripts/census-run.sh`. In `CLAUDE.md:71` the sentence becomes:

> The sanctioned refresh is therefore local now — `scripts/census-run.sh`, run
> once per campaign at the pre-merge close. It serializes against any other
> heavy run on the box and records the run in `docs/timings.md` (decision
> 0081); `HV_CENSUS=1 bash scripts/regenerate-artifacts.sh` still works and is
> still guarded, but prefer the wrapper.

- [ ] **Step 2: Point the superseded decisions forward**

Append one line each to 0063 and 0079 — **do not rewrite their bodies**:

```markdown
**Refined by [0081](0081-one-heavy-writer-per-box-claimed-at-the-write-seam.md)** (2026-07-27): one heavy writer per box, claimed at the write seam.
```

- [ ] **Step 3: Leave history alone**

Do **not** edit `docs/retrospectives/*`, old plans, or superseded decision 0046. They record what was true when written. Verify with:

```bash
git diff --name-only | grep -E "retrospectives|decisions/0046" | wc -l
```

Expected: `0`.

- [ ] **Step 4: Full gate and commit**

```bash
make gate
cargo fmt
git add -A
git commit -m "docs: name the locked census path; point 0063/0079 at 0081"
```

---

### Task 7: Close

- [ ] **Step 1: Absorb main and re-gate** — `make preflight`; on NO-GO merge main INTO the branch and re-run the gate there.

- [ ] **Step 2: Zero-diff census regen — OWNER-AUTHORIZED CARVE-OUT.** Run it the new way, which is itself the acceptance test for the campaign:

```bash
scripts/census-run.sh
git diff --exit-code
```

Expect a zero diff (this campaign changes nothing a run computes), a `census` row in `docs/timings.md`, and — if anything else is running — a legible wait rather than a silent one.

- [ ] **Step 3: Chronicle, retrospective, book sweep.** Invoke the `closing-a-campaign` skill and follow it. The retrospective should record that the constants (200 builds, 45 minutes) were chosen rather than derived, and what the first real ledger rows say about them.

---

## Self-Review

**Spec coverage.** §1 problem → Tasks 1-4. §1.1 tearing → Task 3 (claim held across the run). §1.2 global key → Task 1 (`CLAIM_PATH`, no per-directory keying). §2 non-goals → Global Constraints. §3 item 1 docs → Task 6. Item 2 flock → Task 4 Step 3. Item 3 claim → Tasks 1-3, including timeout, staleness, re-entrancy, and cost scope. Item 3b context contract → Task 2 Step 5 (claim fields, immediate announce, 60s progress, acquired-after, stale takeover, timeout) and Task 4 Step 2 (`status`). Item 4 ledger → Task 5. §4 verification → Task 2 tests, Task 3 Step 4, Task 4 Step 5, Task 7 Step 2. §5 open items → carried into Task 7's retrospective. §6 acceptance → Tasks 4, 5, 7. No gaps.

**Placeholder scan.** No TBD/TODO. Every code step carries real code. Where a name must be confirmed against the tree (Study's field names, the `println!` tally, `current_hostname`'s visibility), the step says which `grep` answers it rather than hand-waving.

**Type consistency.** `ClaimInfo`'s eight fields are identical across `render_claim`, `parse_claim`, the tests, and `status_line`. `acquire` returns `Result<Option<Claim>, String>` in its definition (Task 2) and at its call site (Task 3). `claims_the_box(world_builds, writes_census_goldens)` has the same argument order everywhere. `waited_secs()` is defined in Task 2 and consumed in Tasks 3 and 5.

**Known risks carried inline.** The `flock`-per-open-file-description deadlock is called out where the code would cause it (Task 4 Step 3), and its guard is tested (Task 4 Step 5). The env-var-in-tests awkwardness has a stated fallback (Task 2 Step 6). The eleventh ledger column against ten-column history is addressed rather than discovered (Task 5 Step 1).
