//! The duration alarm (The Timekeeper) and the raw-output guarantee.
//!
//! This reads the JSON that `make ci` wrote for the run that just finished —
//! a test cannot observe its own suite's durations, so the alarm is a separate
//! pass over the previous step's artifact.

use hornvale_lab::census_claim::{ClaimInfo, current_holder};
use hornvale_lab::timings::{
    BASELINE_FLOOR_SECS, baseline_path, fold_below_floor, parse_baseline, parse_run,
    per_test_shifts, suite_shift, top_contributors, total_seconds,
};
use std::fmt::Write as _;
use std::path::{Path, PathBuf};

/// How many of the whole-suite alarm's top contributors to name — enough to
/// point at where to profile without dumping the whole intersection.
const TOP_CONTRIBUTORS_SHOWN: usize = 10;

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

/// Whether a live claim means this run's timings are trustworthy enough to
/// enforce.
///
/// `make ci` does not itself acquire the census-claim lock, so the presence
/// of a claim can only mean some OTHER heavy job (a census, the heavy tier)
/// is holding this box RIGHT NOW — timings are contended and meaningless.
/// The claim file is already per-machine (`current_holder` reads a local
/// path), so any claim we can read at all names a job on THIS host; there is
/// no cross-host comparison to make, and an earlier version of this function
/// wrongly compared `h.host` against our own hostname, which had the whole
/// polarity backwards:
///
/// - No claim (`None`) → the box is QUIET → this is the best moment to
///   enforce a budget. Enforce.
/// - A claim present (`Some`) → something else is running here → timings are
///   contended (The Siding measured `scene_api_cost`'s genesis step at
///   19,722 ms contended vs 3,818 ms quiet, a 5.2x swing that would
///   false-alarm every budget in the suite). Suppress.
///
/// Pulled apart from `current_holder()`'s I/O so the decision itself — a
/// pure function of `Option<ClaimInfo>` — is unit-testable without a real
/// claim file; see `mod tests` below.
fn enforcement_is_appropriate(claim: &Option<ClaimInfo>) -> bool {
    claim.is_none()
}

/// claim: structural(seed: none) — reads target/nextest/ci/run.json, not a
/// seed sweep; "seed"-shaped tokens are timing-baseline plumbing, not a
/// world-seed loop
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
    // Fold to the SAME shape the baseline is stored in (`ci-record` folds
    // before writing) — otherwise every sub-floor test id in `current` finds
    // no match in a baseline that only carries the `<below-floor>`
    // aggregate for that range, and silently drops out of `suite_shift`'s
    // intersection instead of contributing through the aggregate row.
    let current_folded = fold_below_floor(&current, BASELINE_FLOOR_SECS);

    let path = baseline_path(&repo_root(), &host());
    let baseline = match std::fs::read_to_string(&path) {
        Ok(t) => parse_baseline(&t).expect("parse the baseline"),
        Err(_) => {
            eprintln!(
                "no baseline at {} — first run on this host, recording only",
                path.display()
            );
            Vec::new()
        }
    };

    // A truncated or shape-changed nextest stream can yield only a handful of
    // records and still parse cleanly (`parse_run` only errors on TOTAL
    // emptiness) — `per_test_shifts` then finds little to compare, and
    // `suite_shift`'s intersection shrinks on BOTH sides, so a 5-row run.json
    // against a 2548-row baseline stays under tolerance and reports green.
    // Worse, that 5-row run would then get recorded as the new baseline,
    // permanently hiding the shrinkage. A first run (no baseline yet) must
    // still pass, so this only fires once a baseline exists to compare
    // against.
    //
    // `baseline.len()` alone understates the original test count now that
    // sub-floor tests fold into one `<below-floor>` row — reconstruct the
    // real total from each row's `folded_count` (1 for an ordinary row) so
    // this check keeps comparing like against like rather than silently
    // weakening once the floor shrank `baseline.len()` from ~2578 to ~540.
    let baseline_total_tests: usize = baseline
        .iter()
        .map(|r| r.folded_count.map(|n| n as usize).unwrap_or(1))
        .sum();
    if !baseline.is_empty() {
        assert!(
            current.len() >= baseline_total_tests / 2,
            "timekeeper: this run recorded only {} test(s) against a {}-test \
             baseline (folded-row count reconstructed) on {} — that looks like \
             a truncated or shape-changed nextest stream, not a clean suite \
             run. Refusing to treat a partial run as green; inspect {} before \
             re-running `make ci`.",
            current.len(),
            baseline_total_tests,
            host(),
            run_json().display()
        );
    }

    let per_test = per_test_shifts(&current_folded, &baseline);
    let suite = suite_shift(&current_folded, &baseline);

    let claim = current_holder();
    if !enforcement_is_appropriate(&claim) {
        let holder = claim.expect("enforcement_is_appropriate(&claim) is false only for Some");
        eprintln!(
            "timekeeper: {} (pid {}) is holding this box, so timings are contended \
             and NOT enforced. Observed {} per-test shift(s), suite shift: {}.",
            holder.label,
            holder.pid,
            per_test.len(),
            suite.is_some()
        );
        return;
    }

    // The two alarms mean different things, so they say so: a per-test shift
    // names a SPECIFIC test that went pathological; a suite shift is usually
    // accumulated feature growth, so it names the ranked profile list
    // instead of pretending one test is at fault.
    let mut problems = Vec::new();
    for s in &per_test {
        problems.push(format!(
            "  PER-TEST: {} took {:.3}s against a {:.3}s baseline ({:.1}x) — something \
             specific went pathological in this test; investigate it directly.",
            s.id,
            s.current,
            s.baseline,
            s.current / s.baseline
        ));
    }
    if let Some(s) = &suite {
        // `s.baseline`/`s.current` are the INTERSECTION sums that actually
        // tripped the alarm (shared-test comparison); `total_seconds` on the
        // full slices is reported alongside so a human can tell "the shared
        // tests got slower" apart from "we added tests" — the intersection
        // total alone cannot distinguish those two stories.
        let mut msg = format!(
            "  WHOLE-SUITE: (shared tests only) took {:.1}s against a {:.1}s baseline (+{:.0}%)\n\
             \x20   full totals for context: {:.1}s current vs {:.1}s baseline\n\
             \x20   this is usually accumulated feature growth, not one pathological test —\n\
             \x20   top {} contributor(s) by absolute seconds gained (profile these first):",
            s.current,
            s.baseline,
            (s.current / s.baseline - 1.0) * 100.0,
            total_seconds(&current_folded),
            total_seconds(&baseline),
            TOP_CONTRIBUTORS_SHOWN,
        );
        for c in top_contributors(&current_folded, &baseline, TOP_CONTRIBUTORS_SHOWN) {
            let _ = write!(
                msg,
                "\n    {}: {:.3}s -> {:.3}s ({:+.3}s)",
                c.id,
                c.baseline,
                c.current,
                c.delta_seconds()
            );
        }
        msg.push_str(
            "\n   we expect many things to grow in duration as complexity is added — the \
             right response is usually NOT \"why did we change an O(n) algorithm to \
             O(n^m)?\" but \"we added features; profile the list above and optimize where \
             it matters.\" Re-recording the baseline here is deliberate: do it in the SAME \
             commit that caused the growth.",
        );
        problems.push(msg);
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
    assert!(
        !text.trim().is_empty(),
        "raw run output at {} is empty",
        p.display()
    );
    let rows = parse_run(&text).expect("raw output parses");
    assert!(
        rows.iter().any(|r| r.id.contains("hornvale")),
        "raw output at {} names no hornvale tests — wrong file?",
        p.display()
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fake_claim() -> ClaimInfo {
        ClaimInfo {
            pid: 1234,
            host: "somebox".to_string(),
            user: "someone".to_string(),
            started: "2026-07-29T00:00:00Z".to_string(),
            goldens: "/tmp/goldens".to_string(),
            label: "the-census".to_string(),
            reference: "main@deadbeef".to_string(),
            cmdline: "hornvale lab run studies/the-census.study.json".to_string(),
        }
    }

    // Fix round 1: the shipped version of this decision had the polarity
    // backwards — it enforced exactly when a claim was present (contended)
    // and suppressed exactly when the box was quiet. Neither
    // `durations_have_not_regressed` nor `the_raw_output_was_persisted`
    // exercises this branch (both are `#[ignore]`d and read a real
    // `run.json`), which is how the inversion survived review. These pin the
    // polarity directly against the pure decision function so a future
    // regression here fails a normal, non-ignored `cargo test`.

    #[test]
    fn no_claim_means_the_box_is_quiet_and_enforcement_is_appropriate() {
        assert!(
            enforcement_is_appropriate(&None),
            "no claim on this host -> nothing else is running -> enforce"
        );
    }

    #[test]
    fn a_live_claim_means_the_box_is_contended_and_enforcement_is_not_appropriate() {
        assert!(
            !enforcement_is_appropriate(&Some(fake_claim())),
            "a live claim -> some other heavy job holds this box -> suppress"
        );
    }
}
