//! The census duration tripwire (The Sluice).
//!
//! DIRECTION THIS CHECK ENFORCES: the most recent SUCCESSFUL census run in
//! `docs/timings.md` completed within the budget. It is structurally blind to
//! everything else — it says nothing about whether a census is current, ran on
//! the right host, or produced correct goldens.
//!
//! WHY A FIXED CEILING AND NOT A RATCHET. Nathan's rule is "if it takes longer
//! than ~15 minutes we need to freak out and profile it until it is back under
//! 15 minutes" — a budget, not a trend. A ratchet against recent best would arm
//! at the latest 882.487 s and fire on ordinary run-to-run variance, and a
//! check that is always red is ignored exactly as fast as one that is always
//! green.
//!
//! THE MARGIN IS THIN AND THAT IS THE POINT. Of the last three runs at the time
//! this landed — 949.579, 920.212, 882.487 — two would have tripped this.

use std::path::{Path, PathBuf};

/// Seconds a census may take before this test fails. Nathan's ~15 minutes.
const CENSUS_BUDGET_SECS: f64 = 900.0;

/// The repository root, resolved from this crate's manifest directory.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ always has a parent")
        .to_path_buf()
}

/// Every successful census row as (when, wall_seconds), oldest first.
fn successful_census_rows() -> Vec<(String, f64)> {
    let text = std::fs::read_to_string(repo_root().join("docs/timings.md"))
        .expect("docs/timings.md must exist");
    let mut rows = Vec::new();
    for line in text.lines() {
        let parts: Vec<&str> = line.split('|').map(str::trim).collect();
        // A leading empty field precedes `when`, so the label is parts[2].
        if parts.len() < 8 || parts[2] != "census" || parts[7] != "0" {
            continue;
        }
        if let Ok(wall) = parts[3].parse::<f64>() {
            rows.push((parts[1].to_string(), wall));
        }
    }
    rows
}

#[test]
fn the_census_ledger_has_rows_this_test_can_read() {
    // Guards the vacuous case: a parser that silently matches nothing would
    // make the budget assertion below pass forever.
    assert!(
        !successful_census_rows().is_empty(),
        "no successful `| census |` rows parsed from docs/timings.md — the \
         column layout changed and this test has gone vacuous"
    );
}

#[test]
fn the_latest_census_is_within_budget() {
    let rows = successful_census_rows();
    let (when, wall) = rows.last().expect("guarded by the test above").clone();
    let recent: Vec<String> = rows
        .iter()
        .rev()
        .take(5)
        .map(|(w, s)| format!("  {w}  {s:.3} s"))
        .collect();
    assert!(
        wall <= CENSUS_BUDGET_SECS,
        "the latest census took {wall:.3} s (at {when}), over the \
         {CENSUS_BUDGET_SECS:.0} s budget.\n\
         PROFILE IT — do not raise this number.\n\
         last five successful runs, newest first:\n{}",
        recent.join("\n")
    );
}
