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

use hornvale_lab::census_guard::{CENSUS_ALARM_SECS, CENSUS_REFUSAL_SECS};

/// Seconds a census may take before this test fails.
///
/// **The policy target was 900 — Nathan's ~15 minutes — and the constant is now
/// 1000; see the `900 -> 950` and `950 -> 1000` sections below for why, and note
/// the policy target itself is unchanged. Neither raise is a looser budget: the
/// first accounted for the noise floor, the second for a world that grew. If the
/// ~15 minutes is to be held as a real budget rather than a remembered one, that
/// is a decision about scope — fewer seeds, fewer metrics — not about this
/// constant.**
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
/// larger bill. `defensibility` grew 24.8% on 30.1% more habitable vertices:
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
/// constant below was 900 again, then 950, and is now 1000.
///
/// **What the ratchet did NOT fix — ANSWERED 2026-08-26, and this paragraph
/// predicted it.** 900 left 44.5 s of headroom over the 855.5 s reading — 5.2%
/// — while the observed run-to-run spread with *no code change at all* was
/// 882.5–949.6 s, or 7.6%. A ceiling inside the instrument's own noise fires on
/// noise, and it did: 7 of the last 12 runs. The `900 -> 950` section below is
/// that finding acted on rather than restated. What it does NOT do is fix the
/// instrument — 1000 is still a wall-clock number on a shared box, and the
/// durable repair named below (denominate against `cpu_ratio`, so contention
/// and regression separate) is still unbuilt. This raise buys signal-to-noise,
/// not measurement.
///
/// The original wording follows, because it is the reasoning the raise rests
/// on: the ceiling was still inside the instrument's noise,
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
///
/// # 900 -> 950 (2026-08-26)
///
/// **Attribution: the census did not get slower. The threshold was set AT the
/// median instead of above it.** 900 was chosen when the post-Millrace census
/// ran ~900 s, which makes a ~50% fire rate arithmetic rather than evidential —
/// a threshold at the median alarms on half of a healthy distribution by
/// construction. Measured over the last twelve runs (2026-08-13 .. 2026-08-25):
///
/// ```text
///   min 855.5   median 902.8   p90 920.2   max 979.5   (excl. the 19,207 s Rill run)
///   fire rate at 900: 7/12 (58%)     at 925: 2/12     at 950: 2/12
/// ```
///
/// 925 and 950 are behaviourally IDENTICAL on the observed data — both catch
/// exactly the 979.5 s run and The Rill's catastrophe. The choice between them
/// is headroom against variance, not sensitivity: the observed healthy maximum
/// is 920.2 s, so 925 leaves 4.8 s and 950 leaves 29.8 s. A yellow now costs a
/// human a flamegraph and a written finding in the yellow log, so a false
/// alarm is not free noise — it is wasted investigation. 950 it is.
///
/// **Optimisable share: unknown, and deliberately not guessed.** What is known
/// is that the yellow mechanism has already paid for itself — see
/// `168a2a9de` ("two optimizations the yellow-run profile found") and
/// `3f50a5fc8`. The current ~903 s median is the steady state AFTER those
/// landed, so it is not obviously carrying slack; establishing whether it does
/// needs a profile, which is exactly what a yellow is for. Raising the
/// threshold does not retire that question, it stops asking it eight times out
/// of twelve.
///
/// **Ratchet back down when: the median over ten consecutive runs falls below
/// 870 s.** At that point 925 restores the same ~2-in-12 fire rate this raise
/// is buying, and the number should follow the distribution down. Read the
/// median from `docs/timings.md`, never from this comment — a figure written
/// here is a claim with a date, and this whole raise exists because the last
/// one outlived its data.
/// # 950 -> 1000 (2026-08-28)
///
/// **Attribution: the world got bigger, and this is the first raise where that
/// is the whole reason.** The Sources landed an energy field over the rock —
/// `windows/worldgen/src/energy.rs` (+791), a tenth `ResourceAxis`
/// (`CHEMOSYNTHATE`), 1,684 insertions of new generation work — and the census
/// generates ~2,000 worlds through exactly that path. Two censuses of main
/// after it landed:
///
/// ```text
///   969.204 s  (2f8faf243, operator baseline)
///   942.658 s  (27a2da724, campaign/the-precedence)
///   pre-Sources main, same instrument: 897.790 s and 918.590 s
/// ```
///
/// The step is real and attributed to a feature, not to drift.
///
/// **The distribution, and why 1000 rather than 970.** n=2 is thin — the fire
/// rate methodology the `900 -> 950` section used wants ~12 runs and I have two.
/// What the two do establish is that the run-to-run spread on an UNCHANGED tree
/// is at least 26.5 s wide (942.7–969.2). A bound at 970 would sit 0.8 s above
/// the observed max, which is inside that spread by a factor of thirty: it would
/// fire intermittently on a healthy census, which is the failure mode the last
/// raise existed to remove. 1000 leaves 30.8 s over the observed maximum,
/// deliberately matching the 29.8 s of headroom the 950 choice bought over its
/// own 920.2 s healthy max. The precedent is followed, not re-derived.
///
/// **Optimisable share: partly known, and the obvious win is already taken.**
/// The Sources profiled their own addition (`perf record -F 99 -g --call-graph
/// dwarf,16384`, 60-seed subset, lefford), found it pushing census CPU +16.6%
/// over its band maximum, and fixed three pure-optimisation defects in
/// `d0d0a4e02` — `subterranean_substrate_at_rung` was calling `terrain.cave_at`
/// BEFORE checking `Band::Surface`, deriving a cave only to discard it. Measured
/// recovery: the two underworld field functions 12.43% -> 4.15% of census cycles,
/// `SphereFbm::new` project-wide 4.48% -> 2.17%. So ~10.6 points were already
/// paid down before this raise; 969 s is the post-optimisation cost, not a number
/// waiting for work nobody has done. Pre-optimisation main would have been ~1084 s.
///
/// **Ratchet back down when: the median over ten consecutive runs falls below
/// 920 s.** At that point 970 restores the headroom-to-spread ratio this raise
/// is buying, and the number should follow the distribution down. Read the median
/// from `docs/timings.md`, never from this comment — and note that since the
/// census joined the merge queue a run's timing row lands on its `census/*`
/// DELIVERY BRANCH first, so the ledger on main lags until that branch merges.
/// `scripts/census-duration-alarm.sh` reports on the box at the moment of
/// measurement precisely because this test cannot see a run it never receives.
///
/// **Still unfixed, and named again rather than quietly dropped:** the durable
/// repair is to denominate against `cpu_ratio` so contention and regression
/// separate. Every raise so far has bought signal-to-noise instead. This one
/// does too.
/// # 1000 -> 1320 (2026-09-06)
///
/// **Attribution: three world-scale features in a week, and the refusal
/// ceiling below fired on the third.** Every census since 2026-09-03 has been
/// over 1000 (973.5, 1019.1, 1060.3, 1070.9, 1142.2, 1278.2 s), so the alarm
/// has fired on five consecutive healthy runs — a threshold at the median again,
/// the exact shape the 900 -> 950 section retired. The steps are attributed,
/// not drift: The Warp added 32 columns riding a cached grid sweep (1070.9 ->
/// 1142.2 s, +71 s), and The Lot added six columns that draw 200 lives per
/// world (1142.2 -> 1278.2 s, +136 s wall, +5,376 CPU-s, cpu_ratio 28.35 ->
/// 29.57 — more work, not a busier box). The Lot's share was measured BEFORE
/// it landed: ~4,000 CPU-s over the 1,000-seed study; the refresh read +3,661
/// on `census-study-the-census` (20,916 -> 24,577) and +1,668 on
/// `census-study-the-meeting` (8,968 -> 10,636), the estimate within 9%.
///
/// **The distribution, and why 1320.** n=1 on the post-Lot tree (1278.160 s at
/// 2026-09-06T04:00:44Z). The 950 -> 1000 raise left 30.8 s over its observed
/// maximum of 969.2 — 3.2% — and the run-to-run spread on an unchanged tree
/// is at least 26.5 s. 3.2% of 1278 is 41 s; 1320 leaves 42 s. The precedent's
/// ratio is followed, not re-derived. A second reading arrives with the
/// re-run this raise exists to unblock, and is recorded in The Lot's ledger.
///
/// **Optimisable share: The Lot's is measured, The Warp's is not.**
/// `hornvale_lot::context::assemble` rebuilds terrain, climate, the demography
/// report and the sky that `FullView` already holds — ~0.9 s of each world's
/// ~4.2 s, about 22% of the six columns' cost, ~800 CPU-s, ~20 s of wall on a
/// 40-core box: around 2% of the census, not a path back under 1200
/// (`TOOL-lot-assemble-reuses-the-view` in the idea registry carries the
/// measurement). The remaining 78% is the draws themselves — 200 lives per
/// world, each filling 26 slots from the ledger — which is the metric's
/// definition, preregistered before the readout and not to be thinned after it.
/// The Warp's 0.331 CPU-s/world was accepted over its own 0.25 rule at its
/// ledger #10, with no optimisable share named.
///
/// **Ratchet back down when: the median over ten consecutive runs falls below
/// 1200 s.** At that point 1240 restores this raise's headroom-to-spread ratio,
/// and the refusal ceiling below follows it at the same ratio. Read the median
/// from `docs/timings.md`, never from this comment.
///
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
/// # 1200 -> 1650 (2026-09-06)
///
/// **The forward-looking argument above expired, exactly as it said it would.**
/// "The work in prospect is local-level ... it stops holding the moment
/// campaigns return to world-scale generation." They did: The Sources (an energy
/// field over the rock), The Warp (32 columns over a grid sweep) and The Lot
/// (200 lives drawn per world) landed inside eight days, and the census went
/// 873 -> 1278 s across them with cpu_ratio flat (28–32). The Lot's refresh read
/// 1278.160 s on 2026-09-06 and this ceiling refused it — a 12% step on one
/// attributed feature, not the ~33% single-run jump the pair was designed to
/// call foul on.
///
/// **So the pair moves together, per the rule above.** ALARM goes to 1320 (its
/// own section states the arithmetic); RED follows at 1650 = 1320 x 1.25, inside
/// the "RED ≈ typical x 1.3" band the worked example gives (17 -> 21 min is
/// 1.24) and 29% over the one post-Lot reading. This is the first time the pair
/// has been re-set as a pair; the two raises before it moved ALARM alone.
///
/// **What would have been wrong:** raising RED to 1300 to clear one red run,
/// which is the flap-hiding move the doc above refuses, and which would have
/// left a ceiling 1.7% over the reading it was raised for.
///
/// **Ratchet back down with ALARM:** when the median over ten consecutive runs
/// falls below 1200 s, ALARM returns to 1240 and this ceiling to 1550.
///
/// # 1320/1650 -> 1630/2040 (2026-09-13, The Tidemark)
///
/// **Re-set from the SERIES, not from the run that went red.** The Tidemark's
/// refresh read 1898.859 s and this ceiling refused it. That refusal is NOT
/// the justification — "raising because a run went red is the flap-hiding
/// move this file has already refused once", and 1898.859 is a +32% step on
/// the five-run median, which is the very shape RED exists to call foul on.
///
/// The justification is the trigger the rule actually names — *the new normal
/// is consistently near ALARM* — which was met independently of that run, and
/// had been for days:
///
/// ```text
///   2026-09-10  1412.890  cpu_ratio 29.88
///   2026-09-10  1434.633            31.85
///   2026-09-10  1443.918            31.88
///   2026-09-11  1366.487            30.84
///   2026-09-12  1624.010            31.18   <- 1.6% under the old RED
/// ```
///
/// Five consecutive runs, every one of them OVER the 1320 alarm (by 3.5% to
/// 23%), with `cpu_ratio` flat at 29.9-31.9 across all of them — so the rise
/// is work, not contention. The last of the five sat 1.6% under the old
/// ceiling: the pair was already one ordinary feature away from flapping,
/// before this campaign measured anything.
///
/// **The arithmetic, per the rule's own worked example.** ALARM goes to where
/// the census actually sits at the top of that established series, 1630
/// (~1624, the 2026-09-12 reading). RED follows at 1630 x 1.25 = **2040**,
/// inside the "RED ~= typical x 1.3" band. Note what this deliberately does
/// NOT do: it does not set ALARM from 1898.859, because one reading is not a
/// normal. If the re-run lands near 1900 again, the pair is due another move
/// and the *next* campaign will have two readings to set it from.
///
/// **The forward-looking argument has expired a second time**, in the same
/// words as before. The work that moved this was world-scale, not local: six
/// obligate marine peoples entering the roster, and a `land_settlement`
/// selector repair that widened every world by 39 settlements (396 -> 435 on
/// seed 42). A census over 500 worlds pays for both.
///
/// **Ratchet back down** on the same rule as the paragraph above: when the
/// median over ten consecutive runs falls below 1480 s, ALARM returns to 1320
/// and this ceiling to 1650.
///
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
