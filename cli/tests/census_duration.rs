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
//!
//! "MOST RECENT" IS CHRONOLOGICAL, NOT FILE POSITION. `docs/timings.md` is
//! demonstrably not append-only in timestamp order — merge interleaving across
//! parallel campaign branches produces 60+ out-of-order pairs (e.g. line 700
//! stamped `2026-08-11T03:12:17Z`, line 707 stamped the earlier
//! `2026-08-11T00:28:30Z`). Taking the last matching line would read a stale
//! run as "latest" whenever an absorb lands a census row out of position — so
//! this test sorts by the `when` field, not by where the row landed in the
//! file. ASSUMED, NOT VERIFIED: `when` is an ISO-8601 UTC stamp (`...Z`) at
//! second resolution, which is exactly what makes plain lexicographic string
//! comparison sort correctly — this test never parses it as a date, and it
//! trusts the recorded `when` itself (a hand-edited or clock-skewed timestamp
//! would misorder here the same way file position used to).
//!
//! WHAT THE VACUITY TEST ACTUALLY GUARDS. It is not "a parser that matches
//! nothing would make the budget assertion pass forever" — it wouldn't: with
//! today's `rows.last().expect(...)`-equivalent lookup in the budget test, an
//! empty `rows` panics too. The vacuity test earns its place for two other
//! reasons instead: **diagnosis** (its message names the actual cause, "the
//! column layout changed", where the budget test's panic would be an opaque
//! `.expect()` message that doesn't); and **future-proofing** (a plausible
//! refactor that replaces the budget test's `.expect()` with an `if let` would
//! make it start silently passing on empty input, and this is exactly what
//! would catch that regression).

use std::path::{Path, PathBuf};

/// Seconds a census may take before this test fails.
///
/// **The policy target is 900 — Nathan's ~15 minutes — and this is TEMPORARILY
/// 1050, on measurement, with an expiry.** Raised deliberately and recorded
/// here rather than quietly, which is the discipline `cli/tests/session_cost.rs`
/// states for its own ceilings: they ratchet DOWN freely, and raising one is an
/// explicit, reviewed act.
///
/// **Why.** The Glasshouse's temperature epoch (`main` at `63669d2d`) made the
/// census 12% more expensive — 882.487 s to 979.539 s. That is real work, not
/// contention: `cpu_ratio` was 32.10 before and 32.35 after, essentially
/// unchanged, while CPU work rose 12.1%. This test caught it on its first
/// firing, which is the tripwire doing its job.
///
/// **What the profile found** (400 worlds per SHA, run-to-run spread < 1%,
/// probe validated against the ledger to 2.5%):
///
/// ```text
/// world build   1601.9 -> 1752.0 CPU-s   +9.4%   21% of the delta
/// extraction    3887.9 -> 4457.0 CPU-s  +14.6%   79% of the delta
///
/// history-myth-hop-median          825.2 -> 1189.7  +44.2%  (64% of the delta)
/// defensibility-capacity-rank-corr 682.2 ->  851.5  +24.8%  (30%)
/// ```
///
/// Neither hot metric's code changed. `history-myth-hop-median` is superlinear:
/// `descendants_of` in `lineage.rs` filters every node through
/// `ancestry(*k).contains(&of)`, allocating per node, and `median_hops` calls it
/// twice per node — O(nodes² × depth). A 17% larger lineage tree bought a 44%
/// larger bill. `defensibility` grew 24.8% on 30.1% more habitable cells:
/// sub-linear, nothing to fix.
///
/// **So ~51% of the regression is optimisable** and the fix is contained to two
/// functions (a `children`/`depth` map built once). That metric is 19.2% of the
/// whole sweep, so even a 5x recovery returns ~150 s against the +97 s the
/// regression cost — putting the census *below* where it started.
///
/// **The expiry.** When that fix lands, ratchet this back to 900. If it lands
/// and the census is still over 900, that is a finding: the remaining ~49% is
/// inherent epoch cost and the policy number needs re-deciding on evidence, not
/// another raise.
///
/// **Do not raise this number again to make a red go away.** The first version
/// of this doc said "do not raise this number" flatly; that was right in spirit
/// and unusable in practice, because it offered no legitimate path when the
/// increase was real and attributed. The rule that replaces it: a raise must
/// carry the attribution, the optimisable share, and the condition for ratcheting
/// back down. This one does.
const CENSUS_BUDGET_SECS: f64 = 1050.0;

/// The repository root, resolved from this crate's manifest directory.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ always has a parent")
        .to_path_buf()
}

/// Parse every successful census row as (when, wall_seconds) out of raw
/// `docs/timings.md` text, in whatever order the file happens to hold them.
fn parse_successful_census_rows(text: &str) -> Vec<(String, f64)> {
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

/// Every successful census row as (when, wall_seconds), in file order.
fn successful_census_rows() -> Vec<(String, f64)> {
    let text = std::fs::read_to_string(repo_root().join("docs/timings.md"))
        .expect("docs/timings.md must exist");
    parse_successful_census_rows(&text)
}

/// The chronologically latest row, by ISO-8601 `when` — deliberately NOT
/// `rows.last()`, because the file is not reliably in timestamp order (see
/// the module doc). Panics on an empty slice; callers guard emptiness first.
fn latest_by_timestamp(rows: &[(String, f64)]) -> (String, f64) {
    rows.iter()
        .max_by(|a, b| a.0.cmp(&b.0))
        .expect("caller guards emptiness")
        .clone()
}

/// The `n` most recent rows by `when`, newest first.
fn most_recent(rows: &[(String, f64)], n: usize) -> Vec<(String, f64)> {
    let mut sorted = rows.to_vec();
    sorted.sort_by(|a, b| b.0.cmp(&a.0));
    sorted.into_iter().take(n).collect()
}

#[test]
fn the_census_ledger_has_rows_this_test_can_read() {
    assert!(
        !successful_census_rows().is_empty(),
        "no successful `| census |` rows parsed from docs/timings.md — the \
         column layout changed and this test has gone vacuous"
    );
}

#[test]
fn the_latest_census_is_within_budget() {
    let rows = successful_census_rows();
    let (when, wall) = latest_by_timestamp(&rows);
    let recent: Vec<String> = most_recent(&rows, 5)
        .iter()
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

#[test]
fn the_chronologically_latest_row_wins_even_when_it_is_not_last_in_the_file() {
    // A fixture, not the live file, so it cannot rot. Models the real defect
    // shape found in `docs/timings.md`: the chronologically latest row can
    // appear EARLIER in the file than an older row, because merge
    // interleaving across parallel campaign branches does not preserve
    // timestamp order. `rows.last()` would return the second entry here
    // (the earlier stamp); the correct answer is the first.
    let rows = vec![
        ("2026-08-14T15:36:53Z".to_string(), 111.0),
        ("2026-08-11T03:12:17Z".to_string(), 999.0),
    ];
    let (when, wall) = latest_by_timestamp(&rows);
    assert_eq!(
        when, "2026-08-14T15:36:53Z",
        "must pick the chronologically latest row, not the last row in file order"
    );
    assert_eq!(wall, 111.0);
}
