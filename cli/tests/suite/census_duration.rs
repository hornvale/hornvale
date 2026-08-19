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

use std::fs;
use std::path::{Path, PathBuf};

/// Seconds a census may take before this test fails.
///
/// **The policy target is 900 — Nathan's ~15 minutes — and this is 900 again.**
/// The temporary 1050 was RATCHETED BACK on 2026-08-16, on the census row its
/// own expiry named. Raised deliberately and lowered deliberately, both
/// recorded here rather than quietly, which is the discipline
/// `cli/tests/session_cost.rs` states for its own ceilings: they ratchet DOWN
/// freely, and raising one is an explicit, reviewed act.
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
/// functions. That metric is 19.2% of the whole sweep, so even a 5x recovery
/// returns ~150 s against the +97 s the regression cost — putting the census
/// *below* where it started.
///
/// **THE FIX HAS LANDED (The Begat), AND THE FORM PROJECTED ABOVE WAS HALF
/// UNSAFE.** This paragraph used to prescribe "a `children`/`depth` map built
/// once". The children map is right; a global DEPTH map is not. `ancestry`
/// carries a cycle guard, and depth is undefined on a cycle, so a depth map
/// would have diverged silently in exactly the case the guard exists for —
/// a faster metric with a different value, which is a save-format-class event
/// rather than a speedup. What shipped instead is a hop-carrying downward walk
/// over the children map, equivalent whether or not the data is acyclic:
/// each node has at most one parent, so `ancestry`'s break-on-repeat makes
/// "`of` is in `k`'s ancestry" exactly "`k` is reachable downward from `of`",
/// and seeding the visited set with `of` blocks the one longer route (around a
/// cycle through `of` itself). `windows/hearsay/tests/lineage.rs` keeps the old
/// implementation as an oracle and asserts the equivalence on cyclic input.
/// So establishing whether cycles can occur was never needed — the equivalence
/// does not depend on it.
///
/// Measured on a 40-world panel, 3 interleaved reps, differenced against a
/// control study building the same worlds: the metric fell from **1.708
/// CPU-s/world to below that panel's noise floor** (hot minus control, −0.09
/// CPU-s over 40 worlds — a bound, not a resolved figure), with the control
/// unmoved at 91.66 → 91.65 CPU-s. That is a Mac reading on a 40-seed panel and
/// is deliberately NOT offered as a prediction of the census number.
///
/// **The expiry, DISCHARGED.** The Begat's fix landed on `main` at `1e92c152`
/// and the next census on the canonical box read **855.533 s** (row stamped
/// 2026-08-16T23:35:30Z, `cpu_ratio` 32.82 on 40 cores) against the pre-fix
/// 979.539 s — a 12.7% fall. That is real work removed rather than a quieter
/// box: `cpu_ratio` was 32.35 before and 32.82 after. **Zero goldens moved** in
/// that regeneration, so the metric-level win cost no census value — the
/// byte-identity claim's strongest confirmation, a full 1000-seed canonical
/// run rather than a probe. The condition written here was met, so the
/// constant below is 900 again.
///
/// **What the ratchet did NOT fix, and it is the live question now.** 900
/// leaves 44.5 s of headroom over the 855.5 s reading — 5.2% — while the
/// observed run-to-run spread with *no code change at all* was 882.5–949.6 s,
/// or 7.6%. The ceiling is therefore still inside the instrument's noise,
/// which is exactly the condition The Sluice's retrospective named and Nathan
/// deferred. The fix moved the number without fixing the instrument, so this
/// may flap. The durable repair is to denominate against `cpu_ratio` so
/// contention and regression separate — the only one of that retrospective's
/// three options that distinguishes them. **A flap here is an instrument
/// defect, not grounds for a raise.**
///
/// **Do not raise this number again to make a red go away.** The first version
/// of this doc said "do not raise this number" flatly; that was right in spirit
/// and unusable in practice, because it offered no legitimate path when the
/// increase was real and attributed. The rule that replaces it: a raise must
/// carry the attribution, the optimisable share, and the condition for ratcheting
/// back down. This one does.
const CENSUS_ALARM_SECS: f64 = 900.0;

/// **The refusal ceiling, and why there are now two numbers instead of one.**
///
/// The doc above diagnosed the defect precisely and then could not act on it:
/// 900 leaves 5.2% of headroom over the best reading while the observed
/// run-to-run spread *with no code change at all* is 7.6%, so the ceiling sits
/// INSIDE the instrument's noise and "may flap". It duly flapped — The Burr's
/// census read 918.457 s (2026-08-19T15:30:40Z, `cpu_ratio` 32.45) and reddened
/// a merge whose own artifacts phase moved a single timings row.
///
/// The flap is not the campaign's. Of the seven post-memoisation runs on the
/// canonical box, FOUR exceed 900 — 949.579, 920.212, 979.539 and now 918.457 —
/// at `cpu_ratio` 28.56 to 33.23, so contention explains none of the spread.
/// Two of them are HIGHER than the reading that reddened. A ceiling that half
/// the population crosses is measuring the population, not a regression.
///
/// **Raising 900 was the wrong repair and the doc above already says so** — "a
/// flap here is an instrument defect, not grounds for a raise" — and the
/// `f7496156` precedent (900 -> 1050, expiring, ratcheted back by `e0a2e988`)
/// is a precedent for a raise that carried an ATTRIBUTION. This one would not.
///
/// So the single number is split by the question each half actually answers:
///
/// ```text
/// over 1200 s   REFUSE. Far outside the noise — 22% above the highest reading
///               ever recorded post-memoisation. Something is genuinely wrong.
/// over  900 s   ALARM, and owe a profiling follow-up. Inside the noise, so it
///               cannot distinguish regression from variance on its own, but it
///               is the number the project actually wants the census to hold.
/// ```
///
/// The alarm keeps the optimisation pressure that a bare raise to 1200 would
/// have thrown away; the ceiling keeps the tripwire that a bare alarm would
/// have made unenforceable. **Neither number is a raise of the other.**
///
/// **THE PAIR IS THE UNIT, AND IT IS EXPECTED TO RATCHET UP.** Nathan's framing,
/// recorded because the first draft of this doc justified 1200 historically
/// ("22% above the highest reading ever recorded") and that argument silently
/// expires the moment a feature legitimately makes the census slower:
///
///   * ALARM is where the census *should* sit. RED is not an absolute ceiling
///     but a statement that **one run jumped far enough that something is
///     wrong** — a ~33% single-run increase, on a run we cannot even be sure
///     will finish, is worth calling foul on regardless of the absolute number.
///   * Both numbers move UP together as features land, holding roughly this
///     ratio (RED ≈ typical × 1.3). Worked example: if the census settles
///     regularly at 16 minutes, ALARM becomes ~17 min and RED ~21 min.
///   * The trigger for re-setting them is **"the new normal is consistently
///     near ALARM"**, never "a run went red". Raising because a run went red is
///     the flap-hiding move this file has already refused once.
///
/// **Why 1200 has real headroom right now, forward-looking rather than
/// historical:** the work in prospect is local-level — rooms, scenes, the
/// game seam — rather than world-scale generation, so it should not move
/// census time much. That argument is the thing to re-examine first if this
/// ceiling ever starts flapping: it stops holding the moment campaigns return
/// to world-scale generation, and a reader should suspect it before suspecting
/// the number.
///
/// The durable repair the doc above names — denominate against `cpu_ratio` so
/// contention and regression separate — is still owed and is now carried by
/// `PROC-census-budget-denominated-by-cpu-ratio` in the idea registry. The
/// alarm below REQUIRES that row to exist, so the follow-up cannot be silently
/// dropped while the census keeps alarming.
const CENSUS_REFUSAL_SECS: f64 = 1200.0;

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

/// The REFUSAL half. A reading above this is outside the instrument's noise by
/// a wide margin, so it means something is wrong rather than that the box had a
/// bad afternoon.
#[test]
fn the_latest_census_is_under_the_refusal_ceiling() {
    let rows = successful_census_rows();
    let (when, wall) = latest_by_timestamp(&rows);
    let recent: Vec<String> = most_recent(&rows, 5)
        .iter()
        .map(|(w, s)| format!("  {w}  {s:.3} s"))
        .collect();
    assert!(
        wall <= CENSUS_REFUSAL_SECS,
        "the latest census took {wall:.3} s (at {when}), over the \
         {CENSUS_REFUSAL_SECS:.0} s REFUSAL ceiling.\n\
         This is not a flap: the ceiling sits 22% above the highest reading \
         ever recorded on the canonical box, so it is outside the instrument's \
         noise entirely.\n\
         PROFILE IT. Raising this number needs the attribution, the optimisable \
         share, and the condition for ratcheting back down — see the doc on \
         CENSUS_REFUSAL_SECS, and the f7496156 / e0a2e988 precedent.\n\
         last five successful runs, newest first:\n{}",
        recent.join("\n")
    );
}

/// The ALARM half, and the reason it is an assertion rather than a `println!`.
///
/// A passing test's output is captured and hidden by nextest, so "green, but
/// printed loudly" would be invisible in exactly the place this has to be read
/// — a chamber log nobody scrolls. An alarm nobody sees is the report-only
/// check this project has already learned to distrust, so the referral is
/// mechanical instead: while the census is over the alarm threshold, the
/// idea-registry row naming the follow-up MUST exist. Delete the row while the
/// census is still slow and this reddens.
///
/// It is three-valued in the house style (`tropes check`, type-audit's
/// `waiver(...)`, seam-guard's `expect(survives: …)`):
/// ```text
/// under the alarm                        -> green, silent
/// over, THIS RUN acknowledged with a
///   non-empty finding                    -> green
/// over, this run unacknowledged          -> RED: profile it and record it
/// ```
/// **Keyed on the RUN, not the condition**, and that is the whole design. The
/// first version asserted an idea-registry row existed — satisfiable once, by
/// anyone, on behalf of every future yellow run. This one cannot be inherited:
/// a new slow run is a new row.
///
/// It still cannot verify that a flamegraph was taken or that anything got
/// faster. It buys a forced look. Naming that limit here is deliberate — a
/// check that does not state its direction reads as a guarantee it never made.
#[test]
fn a_census_over_the_alarm_threshold_owes_a_profiling_followup() {
    let rows = successful_census_rows();
    let (when, wall) = latest_by_timestamp(&rows);
    if wall <= CENSUS_ALARM_SECS {
        return;
    }
    let log_path = repo_root().join("docs/timings/census-yellow-log.tsv");
    let log = fs::read_to_string(&log_path).expect("the yellow log is tracked and readable");
    let row = log
        .lines()
        .filter(|l| !l.starts_with('#'))
        .find(|l| l.split('\t').next() == Some(when.as_str()));
    let finding = row.and_then(|l| l.split('\t').nth(3)).unwrap_or("").trim();
    assert!(
        !finding.is_empty(),
        "the latest census took {wall:.3} s (at {when}), over the \
         {CENSUS_ALARM_SECS:.0} s ALARM threshold.\n\
         \n\
         That is allowed — the alarm sits inside the instrument's own noise, so \
         it cannot tell a regression from variance by itself. What is NOT \
         allowed is letting it pass unlooked-at: O(n^2) work has slipped into \
         this codebase more than once, and it enters as a yellow run long \
         before it becomes an obvious one, when bisecting it is cheap rather \
         than expensive.\n\
         \n\
         PROFILE IT — flamegraph the run — then add a row to \
         docs/timings/census-yellow-log.tsv:\n\
         \n\
         \t{when}\t{wall:.3}\t<cpu_ratio>\t<what the profile showed>\n\
         \n\
         The `finding` column must not be empty. Acknowledging WITHOUT \
         profiling is the failure this replaced: the previous version of this \
         test asked only that an idea-registry row exist, which any single run \
         could satisfy on behalf of every future one."
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
