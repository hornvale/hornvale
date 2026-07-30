//! The duration alarm (The Timekeeper) and the raw-output guarantee.
//!
//! This reads the JSON that `make ci` wrote for the run that just finished —
//! a test cannot observe its own suite's durations, so the alarm is a separate
//! pass over the previous step's artifact.

use hornvale_lab::census_claim::current_holder;
use hornvale_lab::timings::{
    baseline_path, parse_baseline, parse_run, per_test_shifts, suite_shift, total_seconds,
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
            eprintln!(
                "no baseline at {} — first run on this host, recording only",
                path.display()
            );
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
        problems.push(format!(
            "  <whole suite> (shared tests only) took {:.1}s against a {:.1}s baseline (+{:.0}%)\n\
             \x20   full totals for context: {:.1}s current vs {:.1}s baseline",
            s.current,
            s.baseline,
            (s.current / s.baseline - 1.0) * 100.0,
            total_seconds(&current),
            total_seconds(&baseline),
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
