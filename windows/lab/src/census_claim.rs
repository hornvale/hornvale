//! One heavy writer per box (decision 0081).
//!
//! `scripts/census-run.sh` has always taken `/tmp/hv-census.lock`, but it is
//! one of three ways to start an expensive run; the other two take no lock.
//! This module is the seam every entry point inherits, the same way
//! [`crate::census_guard`] is for the canonical-host check — and for the same
//! stated reason: a shell wrapper cannot guard `cargo run -p hornvale -- lab
//! run <study>`.
//!
//! The claim is GLOBAL, not per-directory: two runs in different worktrees are
//! independent in their output and not in their clock, and the binding
//! constraint is the machine — there is one canonical box (decision 0081).

use std::fs;
use std::path::Path;
// The claim is scheduling infrastructure, not world state: it measures how
// long a run waited and stamps when a hold began. It never reads `WorldTime`,
// never touches a fact, and never reaches an artifact, so it carries the same
// wall-clock exemption `hornvale_worldgen::profiled` does (clippy.toml /
// decision 0001).
#[allow(clippy::disallowed_types)]
use std::time::{Duration, Instant};

const POLL_SECS: u64 = 3;
const PROGRESS_SECS: u64 = 60;

/// Where the claim lives. Distinct from `/tmp/hv-census.lock`, which the shell
/// layer still flocks — the two compose (shell queues, Rust claims).
/// type-audit: bare-ok(identifier-text)
pub const CLAIM_PATH: &str = "/tmp/hv-census.claim";

/// Projected world-builds at or above which a run claims the box: roughly 40
/// seconds of exclusive time at ~0.2s per build on 38 cores. CHOSEN, not
/// derived — revisit against `docs/timings.md` once it carries census rows
/// (decision 0081, Consequences).
/// type-audit: bare-ok(count)
pub const CLAIM_BUILDS_THRESHOLD: u64 = 200;

/// Default bound on the wait, in seconds (45 minutes). Override with
/// `HV_CENSUS_WAIT_TIMEOUT`. Must exceed the longest legitimate hold: a full
/// `HV_CENSUS=1` regen is ~12 min and a queue two deep ~24.
/// type-audit: bare-ok(count)
pub const DEFAULT_TIMEOUT_SECS: u64 = 2700;

/// Env var by which an outer shell announces it already holds the lock, so a
/// nested run does not block against its own ancestor. Names the holder PID.
/// type-audit: bare-ok(identifier-text)
pub const LOCK_HELD_ENV: &str = "HV_CENSUS_LOCK_HELD";

/// The context a blocked caller needs: who holds the box, since when, doing
/// what to which tree, from which code.
/// type-audit: bare-ok(count: pid), bare-ok(identifier-text: host), bare-ok(identifier-text: user), bare-ok(identifier-text: started), bare-ok(identifier-text: goldens), bare-ok(identifier-text: label), bare-ok(identifier-text: reference), bare-ok(prose: cmdline)
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
/// type-audit: bare-ok(count: world_builds), bare-ok(flag: writes_census_goldens), bare-ok(flag: return)
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

/// True when an outer shell already holds the lock on our behalf. Requires the
/// named pid to be BOTH alive and an ancestor: a stale exported variable from
/// an unrelated shell must not silently disable serialization — a guard that
/// reports success while guarding nothing is worse than no guard.
fn already_serialized_by(env_value: &str) -> bool {
    match env_value.trim().parse::<u32>() {
        Ok(pid) => pid_is_alive(pid) && is_ancestor(pid),
        Err(_) => false,
    }
}

/// The claim at `path` if held by a LIVE process; `None` if absent, malformed,
/// or stale (holder no longer alive).
fn live_holder_at(path: &Path) -> Option<ClaimInfo> {
    let text = fs::read_to_string(path).ok()?;
    let info = parse_claim(&text)?;
    if pid_is_alive(info.pid) {
        Some(info)
    } else {
        None
    }
}

/// A held claim. Releases on drop — including on the error and panic paths,
/// which is why this is RAII rather than an explicit `release()` a caller can
/// forget on an early return.
#[derive(Debug)]
pub struct Claim {
    path: std::path::PathBuf,
    waited: Duration,
}

impl Claim {
    /// How long this run waited in the queue before starting. Feeds the timing
    /// ledger's `waited_s`, which is what separates "queued" from "slow" after
    /// the fact.
    /// type-audit: bare-ok(count: return)
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
/// the lock. Returns `Err` only on timeout, whose message names the holder and
/// what to do about it.
/// type-audit: bare-ok(identifier-text: label), bare-ok(count: world_builds), bare-ok(flag: writes_census_goldens), bare-ok(prose: return)
pub fn acquire(
    label: &str,
    goldens: &Path,
    world_builds: u64,
    writes_census_goldens: bool,
) -> Result<Option<Claim>, String> {
    let path = std::path::PathBuf::from(
        std::env::var("HV_CENSUS_CLAIM_PATH").unwrap_or_else(|_| CLAIM_PATH.to_string()),
    );
    let timeout = Duration::from_secs(
        std::env::var("HV_CENSUS_WAIT_TIMEOUT")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(DEFAULT_TIMEOUT_SECS),
    );
    acquire_at(
        &path,
        timeout,
        label,
        goldens,
        world_builds,
        writes_census_goldens,
    )
}

/// The testable core of [`acquire`]: path and timeout explicit, so tests need
/// no environment mutation.
///
/// The wall-clock exemption is scoped to this function, which is the only one
/// that reads a clock: it measures how long a run waited and when to print
/// progress. It never reads `WorldTime`, never touches a fact, and never
/// reaches an artifact — the same exemption `hornvale_worldgen::profiled`
/// carries (clippy.toml / decision 0001).
#[allow(clippy::disallowed_types)]
fn acquire_at(
    path: &Path,
    timeout: Duration,
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

    let began = Instant::now();
    let mut announced = false;
    let mut last_progress = Instant::now();
    loop {
        match live_holder_at(path) {
            None => {
                // Free, or stale. Announce a takeover so a crashed run leaves a
                // trace rather than silently vanishing.
                if path.exists() {
                    eprintln!(
                        "census-claim: taking over a STALE claim at {} — its holder is no longer alive.",
                        path.display()
                    );
                    let _ = fs::remove_file(path);
                }
                match fs::OpenOptions::new()
                    .create_new(true)
                    .write(true)
                    .open(path)
                {
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
                        use std::io::Write as _;
                        file.write_all(render_claim(&info).as_bytes())
                            .map_err(|e| {
                                format!("census-claim: cannot write {}: {e}", path.display())
                            })?;
                        let waited = began.elapsed();
                        if announced {
                            eprintln!("census-claim: acquired after {}.", human(waited));
                        }
                        return Ok(Some(Claim {
                            path: path.to_path_buf(),
                            waited,
                        }));
                    }
                    // Lost a race to another acquirer; fall through and wait.
                    Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {}
                    Err(e) => {
                        return Err(format!(
                            "census-claim: cannot create {}: {e}",
                            path.display()
                        ));
                    }
                }
            }
            Some(holder) => {
                if !announced {
                    // Immediately, never a silent pause: an unexplained pause on
                    // a 40-core box is exactly what a hang looks like, and this
                    // guard exists because a contended run and a normal run were
                    // indistinguishable.
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

/// UTC ISO-8601, via `date` for the same reason [`crate::census_guard::current_hostname`]
/// shells to `hostname`: no date arithmetic and no dependency.
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
/// type-audit: bare-ok(prose: return)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_cost_threshold_draws_the_line_at_200_builds() {
        assert!(!claims_the_box(50, false), "the-chorus (50) must not claim");
        assert!(claims_the_box(200, false), "the perf probe (200) claims");
        assert!(claims_the_box(1000, false), "the-census (1000) claims");
        // Census goldens claim at ANY size: the correctness half is
        // independent of the cost threshold (spec §1.1).
        assert!(
            claims_the_box(1, true),
            "a census-goldens write always claims"
        );
    }

    // Linux-only: this asserts through `pid_is_alive`, which reads `/proc`
    // by deliberate design (decision 0063 makes the canonical box the single
    // golden-authoring platform). On Darwin `/proc` does not exist, so the
    // probe reports every pid dead and these assertions describe a platform
    // they are not running on. The behaviour is Linux-only; so is its test.
    #[cfg(target_os = "linux")]
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

    fn scratch(tag: &str) -> std::path::PathBuf {
        let dir = std::env::temp_dir().join(format!("hv-claim-{tag}-{}", std::process::id()));
        let _ = fs::create_dir_all(&dir);
        dir
    }

    fn sample(pid: u32, label: &str) -> ClaimInfo {
        ClaimInfo {
            pid,
            host: "lefford".to_string(),
            user: "nathan".to_string(),
            started: "2026-07-27T18:53:41Z".to_string(),
            goldens: "/tmp/x".to_string(),
            label: label.to_string(),
            reference: "main@deadbeef".to_string(),
            cmdline: "hornvale lab run".to_string(),
        }
    }

    // Linux-only: this asserts through `pid_is_alive`, which reads `/proc`
    // by deliberate design (decision 0063 makes the canonical box the single
    // golden-authoring platform). On Darwin `/proc` does not exist, so the
    // probe reports every pid dead and these assertions describe a platform
    // they are not running on. The behaviour is Linux-only; so is its test.
    #[cfg(target_os = "linux")]
    #[test]
    fn a_live_ancestor_holding_the_lock_makes_a_claim_a_no_op() {
        // The nested census-run.sh -> regenerate-artifacts.sh -> lab run path
        // must NOT block against its own ancestor. This is the deadlock guard.
        let me = std::process::id();
        assert!(
            already_serialized_by(&me.to_string()),
            "our own pid must count as a live ancestor"
        );
        assert!(!already_serialized_by(&u32::MAX.to_string()));
        assert!(!already_serialized_by(""));
        assert!(!already_serialized_by("not-a-pid"));
    }

    #[test]
    fn a_stale_claim_is_taken_over_rather_than_waited_on() {
        let dir = scratch("stale");
        let path = dir.join("stale.claim");
        fs::write(&path, render_claim(&sample(u32::MAX, "the-census"))).unwrap();
        // Reported as absent, so acquisition proceeds instead of waiting on a
        // holder that will never release.
        assert!(live_holder_at(&path).is_none());
        let claim = acquire_at(
            &path,
            Duration::from_secs(5),
            "the-census",
            Path::new("/tmp/x"),
            1000,
            true,
        )
        .expect("a stale claim is taken over")
        .expect("a census run claims the box");
        assert_eq!(claim.waited_secs(), 0, "takeover must not wait");
        drop(claim);
        assert!(!path.exists(), "drop releases the claim");
        let _ = fs::remove_dir_all(&dir);
    }

    // Linux-only: this asserts through `pid_is_alive`, which reads `/proc`
    // by deliberate design (decision 0063 makes the canonical box the single
    // golden-authoring platform). On Darwin `/proc` does not exist, so the
    // probe reports every pid dead and these assertions describe a platform
    // they are not running on. The behaviour is Linux-only; so is its test.
    #[cfg(target_os = "linux")]
    #[test]
    fn a_live_claim_is_reported_with_its_context() {
        let dir = scratch("live");
        let path = dir.join("live.claim");
        fs::write(
            &path,
            render_claim(&sample(std::process::id(), "census-of-the-meeting")),
        )
        .unwrap();
        let held = live_holder_at(&path).expect("a live claim is reported");
        assert_eq!(held.label, "census-of-the-meeting");
        assert_eq!(held.reference, "main@deadbeef");
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn a_cheap_run_never_claims_and_never_waits() {
        let dir = scratch("cheap");
        let path = dir.join("busy.claim");
        // A live holder is present; a below-threshold run must sail past it.
        fs::write(&path, render_claim(&sample(std::process::id(), "held"))).unwrap();
        let claim = acquire_at(
            &path,
            Duration::from_secs(1),
            "the-chorus",
            Path::new("/tmp/x"),
            50,
            false,
        )
        .expect("a cheap run does not time out");
        assert!(claim.is_none(), "the-chorus (50 builds) must not claim");
        let _ = fs::remove_dir_all(&dir);
    }

    // Linux-only: this asserts through `pid_is_alive`, which reads `/proc`
    // by deliberate design (decision 0063 makes the canonical box the single
    // golden-authoring platform). On Darwin `/proc` does not exist, so the
    // probe reports every pid dead and these assertions describe a platform
    // they are not running on. The behaviour is Linux-only; so is its test.
    #[cfg(target_os = "linux")]
    #[test]
    fn a_timeout_names_the_holder_and_what_to_do() {
        let dir = scratch("timeout");
        let path = dir.join("busy.claim");
        // Our own pid: alive by construction, so the claim is never released.
        fs::write(
            &path,
            render_claim(&sample(std::process::id(), "the-census")),
        )
        .unwrap();
        let err = acquire_at(
            &path,
            Duration::from_secs(1),
            "the-census",
            Path::new("/tmp/x"),
            1000,
            true,
        )
        .expect_err("a held claim must time out, not succeed");
        assert!(err.contains("TIMED OUT"), "message: {err}");
        assert!(
            err.contains(&std::process::id().to_string()),
            "names the holder: {err}"
        );
        assert!(err.contains("remove"), "says what to do: {err}");
        let _ = fs::remove_dir_all(&dir);
    }
}
