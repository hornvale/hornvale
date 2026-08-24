//! H1 — does the anomaly report's per-world ranking concentrate a planted
//! perturbation? (spec §3.5, preregistered before this file existed.)
//!
//! **This is a CHEAP READ, deliberately not a heavy-tier battery.** The
//! expensive half — perturbing one generative constant, rebuilding, and
//! running twenty worlds per arm — happens in `scripts/gnomon-injection.sh`
//! and is committed as fixture evidence, because a compiled test binary
//! cannot change the constants it was compiled from. This file reads that
//! evidence and does arithmetic over it. The seam is the one
//! `scripts/census-run.sh` already uses: authoring is host-pinned and
//! human-run, reading is free and runs on every commit.
//!
//! ## What recall@10 means here, exactly
//!
//! The unit is an **(injection × seed) pair**, not an injection — five
//! injections alone would make recall a five-point scale on which the 0.60
//! bar is just "three of five".
//!
//! For an injection arm `A` and a seed `s`:
//!
//! - `moved(A, s)` is the set of **evaluable** census columns whose raw
//!   value differs between `baseline-a`'s row for `s` and `A`'s row for `s`.
//! - the pair is **VOID** when that set is empty — it counts in neither the
//!   numerator nor the denominator, and it is printed with its reason. A
//!   silently dropped void pair makes recall look better than it is.
//! - the pair is a **HIT** when any member of `moved(A, s)` appears in the
//!   perturbed world's [`REPORT_SIZE`]-column report.
//!
//! **`moved` is restricted to the evaluable surface, and that is a
//! structural fact rather than a lenient choice.** A column outside spec
//! §3.3's evaluable surface — frozen, all-absent, both rails tied — cannot
//! appear in ANY world's report, because it has no tail to be deep in. A
//! definition that counted those would measure the exclusion roster, not the
//! ranking, and would report a miss for something the ranking was never able
//! to do. The number of pairs whose *only* moved columns were non-evaluable
//! is printed separately, so this restriction can never hide a population.
//!
//! **The label is independent of the census** (spec §3.5): `moved` is a raw
//! value comparison between two committed CSVs. No percentile decides it.
//!
//! ## The positive control comes first, and it is `render_diff`
//!
//! A mutation proves only what it perturbs, and a recall of zero is
//! unreadable without it: it means either "the report missed it" or "the
//! mutation did nothing", which demand opposite responses. The study-level
//! control is [`hornvale_lab::render_diff`] — the library behind `hornvale
//! lab diff`, which already answers "which metrics moved between two
//! `rows.csv` snapshots". It is not reimplemented here; that would be the
//! exact duplication this campaign exists to stop.
//!
//! The per-PAIR movement above is a raw string comparison of two CSV vertices,
//! which `render_diff` cannot express (it summarises a whole arm's
//! distribution, not one seed's row) and which is not a differ in any
//! meaningful sense.

use hornvale_lab::domesday::anomaly::{self, REPORT_SIZE};
use hornvale_lab::domesday::census::{Census, load};
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

/// The preregistered success bar (spec §3.5). Frozen: never retuned to
/// rescue a result. A falsification is the campaign's headline, not a
/// failure to be tuned away.
const RECALL_BAR: f64 = 0.60;

/// The smallest battery that adjudicates H1: five injections, twenty seeds
/// each (≈100 pairs). Derived from the fixtures on disk, never declared —
/// a flag a human could flip is a guard a comment can satisfy.
const MIN_INJECTIONS: usize = 5;
/// Seeds per arm the preregistered battery needs, so that recall is not a
/// coarse fraction of a handful of worlds.
const MIN_SEEDS: usize = 20;

fn fixtures() -> PathBuf {
    Path::new("tests/fixtures/injection").to_path_buf()
}

fn census() -> Census {
    load(Path::new("../../book/src/laboratory/generated/the-census"))
        .expect("the committed census loads")
}

fn arm(name: &str) -> Census {
    load(&fixtures().join(name)).unwrap_or_else(|e| {
        panic!(
            "injection fixture {name} loads: {e} — re-author it with scripts/gnomon-injection.sh"
        )
    })
}

fn manifest() -> serde_json::Value {
    let text = std::fs::read_to_string(fixtures().join("manifest.json"))
        .expect("the injection manifest is committed alongside its fixtures");
    serde_json::from_str(&text).expect("the injection manifest is JSON")
}

fn arm_names(kind: &str) -> Vec<String> {
    manifest()["arms"]
        .as_array()
        .expect("arms is an array")
        .iter()
        .filter(|a| a["kind"] == kind)
        .map(|a| a["name"].as_str().expect("arm name is text").to_string())
        .collect()
}

/// Rows keyed by seed, so two arms can be compared world for world.
fn by_seed(c: &Census) -> BTreeMap<u64, &BTreeMap<String, String>> {
    c.rows
        .iter()
        .filter_map(|r| r.get("seed")?.parse::<u64>().ok().map(|s| (s, r)))
        .collect()
}

/// Was every arm authored on the canonical box? The fixtures are scored
/// against census goldens authored there and the machines disagree by one
/// unit on ~0.1% of discrete-count metrics (decisions 0063/0079), so an
/// off-host arm is a PILOT: it proves the machinery and adjudicates nothing.
fn every_arm_is_canonical() -> bool {
    manifest()["arms"]
        .as_array()
        .expect("arms is an array")
        .iter()
        .all(|a| {
            a["host"].as_str().is_some_and(|h| {
                h.eq_ignore_ascii_case(hornvale_lab::census_guard::CANONICAL_CENSUS_HOST)
            })
        })
}

/// Does the committed battery meet the preregistered shape? Every clause is
/// read off the fixtures themselves.
fn battery_adjudicates_h1(seeds: usize) -> (bool, String) {
    let injections = arm_names("injection").len();
    let baselines = arm_names("baseline").len();
    let canonical = every_arm_is_canonical();
    let ok = injections >= MIN_INJECTIONS && baselines >= 2 && seeds >= MIN_SEEDS && canonical;
    let why = format!(
        "{injections} injections (need >= {MIN_INJECTIONS}), {baselines} baselines (need >= 2), \
         {seeds} seeds/arm (need >= {MIN_SEEDS}), canonical host: {canonical}"
    );
    (ok, why)
}

/// The evaluable columns that differ between two arms for one seed.
fn moved_columns(
    base: &BTreeMap<String, String>,
    perturbed: &BTreeMap<String, String>,
    evaluable: &BTreeSet<String>,
) -> BTreeSet<String> {
    base.iter()
        .filter(|(k, v)| perturbed.get(*k) != Some(*v))
        .map(|(k, _)| k.clone())
        .filter(|k| evaluable.contains(k))
        .collect()
}

/// Every column that differs, evaluable or not — the denominator-side
/// counterpart of [`moved_columns`], so a pair whose only movement is
/// unrankable is reported rather than silently void.
fn moved_columns_any(
    base: &BTreeMap<String, String>,
    perturbed: &BTreeMap<String, String>,
) -> BTreeSet<String> {
    base.iter()
        .filter(|(k, v)| perturbed.get(*k) != Some(*v))
        .map(|(k, _)| k.clone())
        .collect()
}

/// **Fixture staleness, asserted rather than hoped for.** The fixtures are
/// scored against the committed census, so a census refresh that adds or
/// removes columns stales them.
///
/// The branch table for a red here:
/// - the census gained columns and the fixtures did not → re-author the
///   fixtures in the same commit as the refresh (Task 7 carries this);
/// - the fixtures carry columns the census lacks → they were authored
///   against a different metric registry; re-author, never filter the
///   mismatch away.
#[test]
fn the_fixture_columns_match_the_census() {
    let c = census();
    let want: BTreeSet<&str> = c.columns.iter().map(|col| col.name.as_str()).collect();
    for name in arm_names("baseline")
        .into_iter()
        .chain(arm_names("injection"))
    {
        let a = arm(&name);
        let got: BTreeSet<&str> = a.columns.iter().map(|col| col.name.as_str()).collect();
        let missing: Vec<&&str> = want.difference(&got).collect();
        let extra: Vec<&&str> = got.difference(&want).collect();
        assert!(
            missing.is_empty() && extra.is_empty(),
            "injection fixture {name} is STALE against the census: \
             columns the census has and the fixture lacks: {missing:?}; \
             columns the fixture has and the census lacks: {extra:?}. \
             Re-author with scripts/gnomon-injection.sh on the canonical box \
             in the same commit as the census refresh — never filter the \
             mismatch away."
        );
    }
}

/// **H1's mandatory positive control, run before recall is computed at
/// all.** Every injection must be shown to have moved the world, and the
/// unperturbed second baseline must be shown not to have.
///
/// The movement is asserted through `hornvale lab diff`'s own library
/// function, so the control is the instrument the project already uses to
/// answer this question rather than a second one written here.
#[test]
fn every_injection_moved_the_world_and_the_baselines_did_not() {
    let study = hornvale_lab::load_study(Path::new("../../studies/gnomon-injection.study.json"))
        .expect("the injection study loads");
    let base_csv = std::fs::read_to_string(fixtures().join("baseline-a/rows.csv"))
        .expect("baseline-a rows.csv is committed");

    for name in arm_names("injection") {
        let csv = std::fs::read_to_string(fixtures().join(&name).join("rows.csv"))
            .expect("arm rows.csv is committed");
        let report = hornvale_lab::render_diff(&study, &base_csv, &csv).expect("diff renders");
        assert!(
            !report.contains("No metric moved."),
            "injection {name} moved NOTHING — it is VOID and cannot contribute to \
             recall in either direction. Either the substitution did not reach the \
             generated world, or the constant is not load-bearing. Replace the \
             injection; do not leave a void arm in the battery.\n{report}"
        );
        println!(
            "[control] {name}: {}",
            report
                .lines()
                .find(|l| l.contains("distributions moved"))
                .unwrap_or("(movement reported)")
        );
    }

    for name in arm_names("baseline")
        .into_iter()
        .filter(|n| n != "baseline-a")
    {
        let csv = std::fs::read_to_string(fixtures().join(&name).join("rows.csv"))
            .expect("arm rows.csv is committed");
        let report = hornvale_lab::render_diff(&study, &base_csv, &csv).expect("diff renders");
        assert!(
            report.contains("No metric moved."),
            "{name} is an UNPERTURBED rerun of baseline-a and must be identical to \
             it. A difference here is a determinism bug in the generative path — it \
             outranks everything else in this campaign.\n{report}"
        );
        println!("[control] {name}: no metric moved (as required)");
    }
}

/// **H1's false-positive arm.** Two *independently generated* baseline runs
/// must produce identical top-10s.
///
/// Note what this replaces and why. Running `rank` twice over one in-memory
/// census compares a pure function of fixed input against itself: it cannot
/// disagree, so that guard could never fail (`five-vacuous-guards-one-
/// campaign`). The content is in whether two separate world-BUILD runs
/// produce identical rows, which is why the authoring script emits the
/// baseline arm twice, as separate invocations, and commits both.
///
/// claim: invariant(every unperturbed world ranks identically across two
/// independently generated baseline runs) — an identity check over committed
/// rows, not a rate: one differing world is a determinism bug.
#[test]
fn two_independent_baseline_runs_rank_identically() {
    let c = census();
    let a = arm("baseline-a");
    let b = arm("baseline-b");
    let (rows_a, rows_b) = (by_seed(&a), by_seed(&b));
    assert_eq!(
        rows_a.keys().collect::<Vec<_>>(),
        rows_b.keys().collect::<Vec<_>>(),
        "the two baseline arms cover the same seeds"
    );
    let mut differing = 0usize;
    for (seed, row_a) in &rows_a {
        let report_a = anomaly::score_row(&c, *seed, row_a);
        let report_b = anomaly::score_row(&c, *seed, rows_b[seed]);
        if report_a != report_b {
            differing += 1;
            println!("[false-positive] seed {seed}: {report_a:?} vs {report_b:?}");
        }
    }
    assert_eq!(
        differing, 0,
        "{differing} unperturbed worlds ranked differently between two independent \
         baseline runs. The census is deterministic, so this is an identity check, \
         not a statistic: a non-zero result means the generative path or the scorer \
         has a nondeterministic tie-break, which is a determinism bug and outranks \
         everything else in this campaign."
    );
}

/// **H1, the headline.** recall@10 over the (injection × seed) pairs.
///
/// The preregistered bar is `recall@10 >= 0.60`. It is asserted only when
/// the committed battery actually meets the preregistered shape — five
/// injections, two baselines, twenty seeds an arm, every one of them
/// authored on the canonical box. Everything else is a PILOT: it proves the
/// machinery and prints the figure, and the figure adjudicates nothing,
/// because fixtures authored off-host carry ~0.1% discrete-count divergence
/// against the census goldens they are scored against.
///
/// Neither `k`, nor `TAIL_DEPTH_BAR`, nor `REPORT_SIZE`, nor the injection
/// set may be adjusted to rescue a result. A recall below the bar falsifies
/// H1 and is published as the finding.
///
/// claim: readout(preregistered) — recall@10 over the committed (injection x
/// seed) pairs, against the frozen 0.60 bar; the seed loop enumerates the
/// battery's own arms rather than sampling a population.
#[ignore = "PREREGISTERED, cannot adjudicate at n=120: awaits TOOL-anomaly-ranking-concentrates-injection (recall@10 = 0.5833 over 120 pairs, -0.37 SE from the 0.60 bar; five census epochs of one unchanged report read 0.5667, 0.6083, 0.6000, 0.6083 and 0.5833, all inside one SE of the bar, so the battery separates nothing)"]
#[test]
fn h1_recall_at_10() {
    let t = tally_recall();

    assert!(
        t.counted > 0,
        "no (injection × seed) pair contributed to recall — every pair was void, so \
         the figure is undefined rather than low. Re-author the battery."
    );

    let (adjudicates, why) = battery_adjudicates_h1(t.seeds);
    if !adjudicates {
        println!(
            "[recall] PILOT BATTERY — H1 IS NOT ADJUDICATED HERE ({why}). The \
             preregistered bar is deliberately NOT asserted against a battery this \
             shape; author the full one on the canonical box with \
             scripts/gnomon-injection.sh and re-read this test."
        );
        return;
    }

    let recall = t.recall();
    assert!(
        recall >= RECALL_BAR,
        "H1 FALSIFIED: recall@10 = {recall:.4} over {} (injection × seed) \
         pairs, below the preregistered bar of {RECALL_BAR}. This is the campaign's \
         headline finding and is published as such. Do NOT adjust k, TAIL_DEPTH_BAR, \
         REPORT_SIZE or the injection set to rescue it: retune the report and this \
         test measures nothing. Record the figure, mark the row `refuted`, and say so \
         in the chronicle.",
        t.counted
    );
    println!(
        "[recall] H1 MET: recall@10 = {recall:.4} >= {RECALL_BAR} over {} pairs",
        t.counted
    );
}

/// **The witness that keeps the falsification measured while H1 is
/// `#[ignore]`d.**
///
/// [`h1_recall_at_10`] is ignored because its preregistered assertion is
/// *not met* and the failure is the record (the roster entry in
/// `cli/tests/heavy_tier.rs` carries the figure). But an ignored measurement
/// stops being measured: change [`REPORT_SIZE`], `TAIL_DEPTH_BAR` or the
/// scorer and nothing anywhere goes red, while the published 0.5667 quietly
/// becomes fiction. No other test in this file pins it.
///
/// **This pins a witness, not a claim.** The numbers below are not a bar the
/// report must clear — they are the exact integers this battery produced
/// against this census on the canonical box, recorded so that any change to
/// the report *forces a deliberate re-read*. Moving them is therefore not
/// "updating a number": it means the falsification was re-measured, and the
/// chronicle, the registry row and the roster entry must all be re-read and
/// re-stated against the new figure in the same commit. Re-pinning without
/// re-reading is the retuning the campaign's preregistration forbids.
///
/// It asserts integers rather than the float recall on purpose: a rate hides
/// which of its two terms moved, and `hits`/`counted` separate a report that
/// found less from a battery that offered less.
///
/// **RE-READ AT THE GLASSHOUSE'S CLOSE (2026-08-15), and the verdict it
/// witnessed no longer stands.** This block did exactly what it was written
/// to do: it forced a deliberate re-read instead of letting a moved number
/// pass as bookkeeping.
///
/// The Glasshouse warmed the census (median land temperature −11.99 →
/// −3.65 °C) and the injection fixtures were re-authored against it on the
/// canonical box. Nothing about the *report* changed — not `REPORT_SIZE`, not
/// `TAIL_DEPTH_BAR`, not the scorer, not the evaluable surface. Only the
/// worlds moved. Re-measured: **73/120 = 0.6083**, against the same
/// preregistered 0.60 bar the old 68/120 = 0.5667 fell short of.
///
/// **That is NOT a confirmation, and it is recorded as "cannot tell".** The
/// crossing is one hit out of 120. At the bar the standard error is
/// `sqrt(0.6·0.4/120) = 0.0447`, so 0.5667 sat 0.75 SE *below* it and 0.6083
/// sits 0.19 SE *above* it; the move between them is 0.66 SE. A 120-pair
/// battery cannot separate "works" from "does not work" at a 0.60 line, and
/// it never could — the old refutation looked clean only because it happened
/// to land on the low side of the same noise.
///
/// So the registry row is no longer `refuted`, and it is not `shipped`
/// either: the honest state is an open question with an underpowered
/// instrument, and the fix is more pairs, not a moved bar. Nathan's call at
/// the close (see The Gnomon's chronicle postscript). **Do not "resolve" this
/// by widening the bar or by re-reading the battery until it lands where you
/// want it** — that is the retuning the preregistration forbids, and the
/// direction of the error is now known to be smaller than the noise either
/// way.
///
/// **RE-READ AGAIN AT THE UNDERWORLD'S CLOSE (2026-08-17), AND THE VERDICT IS
/// UNCHANGED — WHICH IS ITSELF THE RESULT.** The chamber epoch refreshed the
/// canonical census (223e7d57, goldens 8df714ed on lefford, 0063/0079) and
/// the injection fixtures were re-scored against it. Again nothing about the
/// *report* changed — not `REPORT_SIZE`, not `TAIL_DEPTH_BAR`, not the
/// scorer, not the evaluable surface. Only the worlds moved. Re-measured:
/// **72/120 = 0.6000**, which is *exactly* the preregistered bar.
///
/// Five readings now exist, across five census epochs, of one unchanged
/// report:
///
/// ```text
///     SE at the bar = sqrt(0.6 * 0.4 / 120) = 0.04472
///
///     68/120 = 0.5667   -0.745 SE   The Gnomon      (published as refuted)
///     73/120 = 0.6083   +0.186 SE   The Glasshouse  (verdict withdrawn)
///     72/120 = 0.6000    0.000 SE   The Underworld  (this re-read)
///     73/120 = 0.6083   +0.186 SE   The Burr        (repeats Glasshouse)
///     70/120 = 0.5833   -0.374 SE   The Granary     (this re-read)
/// ```
///
/// All sit inside one standard error of the bar, scattered on both sides of
/// it — exactly the signature of an instrument measuring nothing but noise
/// at this n. **The verdict therefore remains "cannot tell"; the instrument
/// remains underpowered; the fix remains more pairs, not a moved bar.**
///
/// This re-read STRENGTHENS that conclusion rather than disturbing it. The
/// Glasshouse's argument was that a 120-pair battery cannot separate "works"
/// from "does not work" at a 0.60 line, inferred from two readings straddling
/// it. A third world-change has now produced a third reading inside the same
/// one-SE band, landing on the line itself. A witness that has fired twice
/// and corroborated its own prior re-read is worth more than one that never
/// moved: the underpowered diagnosis is no longer an inference from two
/// points but an observation repeated across three independent census epochs.
///
/// The registry row stays neither `refuted` nor `shipped`, and its status is
/// deliberately NOT changed by this re-read. Nothing here licenses widening
/// the bar, and nothing licenses re-reading the battery until it lands
/// somewhere comfortable — landing on the bar is uncomfortable and is the
/// honest reading.
///
/// **RE-READ A THIRD TIME AT THE BURR'S CLOSE (2026-08-18/19), AND THE
/// READING REPEATS EXACTLY.** The Burr refreshed the canonical census with a
/// language epoch (`ROOT_EPOCH v4`, census committed as `635d116d`) that
/// redraws every generated name and reseeds the phonology cascades feeding
/// it — nothing about the *report* changed. Re-measured: **73/120 = 0.6083**,
/// which is not merely close to The Glasshouse's reading, it IS The
/// Glasshouse's reading: the same 73 hits over the same 120 pairs, +0.19 SE
/// from the bar, reproduced by an unrelated mechanism at an unrelated epoch.
/// Four readings of one unchanged report now exist:
///
/// ```text
///     SE at the bar = sqrt(0.6 * 0.4 / 120) = 0.04472
///
///     68/120 = 0.5667   -0.75 SE   The Gnomon      (published as refuted)
///     73/120 = 0.6083   +0.19 SE   The Glasshouse  (verdict withdrawn)
///     72/120 = 0.6000    0.00 SE   The Underworld  (corroborated the withdrawal)
///     73/120 = 0.6083   +0.19 SE   The Burr        (repeats The Glasshouse's reading)
/// ```
///
/// No two consecutive readings land on the same side of the bar — below, on
/// the high side, on the line, on the high side again — which is exactly what
/// an instrument with no real signal at this `n` should produce: noise
/// scattered tightly around the threshold rather than converging toward
/// either side. The registry row's status is unchanged by this re-read, as it
/// was unchanged by the second.
///
/// claim: invariant(the committed battery scores exactly 70 hits over 120
/// evaluable (injection x seed) pairs, with no void pairs) — an identity over
/// committed fixtures and a committed census, not a statistic.
#[test]
fn the_falsified_recall_is_pinned_as_a_witness() {
    let t = tally_recall();
    assert_eq!(
        (
            t.hits,
            t.counted,
            t.void_no_movement,
            t.void_unrankable_only
        ),
        (70, 120, 0, 0),
        "the injection battery's recall tally moved. This is the WITNESS to The \
         Gnomon's reading (recall@10 = 70/120 = 0.5833, -0.37 SE from the \
         preregistered bar of 0.60 — a bar this battery is NOT powered to \
         adjudicate, see the doc comment), and it is pinned so that a change to \
         the report — REPORT_SIZE, TAIL_DEPTH_BAR, the scorer, the evaluable \
         surface, the census, or the fixtures — cannot silently turn the \
         published figure into fiction. Do not simply update these integers: \
         re-read the finding, re-state it in book/src/chronicle/the-gnomon.md, \
         in the TOOL-anomaly-ranking-concentrates-injection registry row and in \
         the `#[ignore]` reason rostered in cli/tests/heavy_tier.rs, and re-pin \
         all four in the same commit. THIS HAS NOW HAPPENED FOUR TIMES (The \
         Glasshouse, 2026-08-15, which overturned the verdict; The Underworld, \
         2026-08-17, which corroborated the withdrawal at a third census \
         epoch; The Burr, 2026-08-18/19, which corroborated it again at a \
         fourth, reproducing The Glasshouse's exact reading). Every time the \
         report was untouched and the number moved because the WORLD moved, \
         which is the strongest argument for keeping this pin. FIRST ASK \
         WHETHER YOUR CHANGE TOUCHED THE REPORT: if it did, the four readings \
         above are no longer comparable and you have a different, larger \
         question than a re-pin."
    );
}

/// The recall tally, computed once and read by both
/// [`h1_recall_at_10`] (which judges it against the preregistered bar) and
/// [`the_falsified_recall_is_pinned_as_a_witness`] (which pins it). One
/// computation, two readings — a second implementation here would be the
/// duplication this campaign exists to name.
struct Tally {
    /// Pairs whose moved evaluable columns reached the world's report.
    hits: usize,
    /// Pairs that contributed to recall at all (moved something evaluable).
    counted: usize,
    /// Pairs where the perturbed row equalled baseline in every column.
    void_no_movement: usize,
    /// Pairs that moved only columns outside the evaluable surface.
    void_unrankable_only: usize,
    /// Seeds per arm, read off the baseline arm rather than declared.
    seeds: usize,
}

impl Tally {
    /// recall@[`REPORT_SIZE`], or NaN when nothing was counted.
    fn recall(&self) -> f64 {
        if self.counted == 0 {
            f64::NAN
        } else {
            self.hits as f64 / self.counted as f64
        }
    }
}

fn tally_recall() -> Tally {
    let c = census();
    let (evaluable_list, _) = anomaly::evaluable_columns(&c);
    let evaluable: BTreeSet<String> = evaluable_list.into_iter().collect();

    let base = arm("baseline-a");
    let base_rows = by_seed(&base);

    let mut hits = 0usize;
    let mut counted = 0usize;
    let mut void_no_movement = 0usize;
    let mut void_unrankable_only = 0usize;

    for name in arm_names("injection") {
        let a = arm(&name);
        let rows = by_seed(&a);
        let (mut arm_hits, mut arm_counted) = (0usize, 0usize);
        let mut arm_moved: BTreeSet<String> = BTreeSet::new();
        let mut best_hit_depth = f64::INFINITY;
        let mut worst_miss_cutoff = 0.0f64;

        for (seed, base_row) in &base_rows {
            let Some(row) = rows.get(seed) else {
                panic!("injection {name} is missing seed {seed} — re-author the battery")
            };
            let moved = moved_columns(base_row, row, &evaluable);
            if moved.is_empty() {
                if moved_columns_any(base_row, row).is_empty() {
                    void_no_movement += 1;
                } else {
                    void_unrankable_only += 1;
                }
                continue;
            }
            arm_moved.extend(moved.iter().cloned());
            arm_counted += 1;

            let report = anomaly::score_row(&c, *seed, row);
            assert!(
                report.flags.len() <= REPORT_SIZE,
                "a world's report is capped at REPORT_SIZE columns"
            );
            let flagged: BTreeMap<&str, f64> = report
                .flags
                .iter()
                .map(|f| (f.metric.as_str(), f.depth))
                .collect();
            let hit_depth = moved
                .iter()
                .filter_map(|m| flagged.get(m.as_str()).copied())
                .fold(f64::INFINITY, f64::min);
            if hit_depth.is_finite() {
                arm_hits += 1;
                best_hit_depth = best_hit_depth.min(hit_depth);
            } else {
                // The report's own cut-off: the depth the moved columns
                // failed to beat. Recorded because a miss on a column just
                // outside the report is a different finding from a miss on
                // one nowhere near it, and a bare recall number hides which.
                let cutoff = report.flags.last().map(|f| f.depth).unwrap_or(f64::NAN);
                if cutoff > worst_miss_cutoff {
                    worst_miss_cutoff = cutoff;
                }
            }
        }

        hits += arm_hits;
        counted += arm_counted;
        let rate = if arm_counted == 0 {
            f64::NAN
        } else {
            arm_hits as f64 / arm_counted as f64
        };
        println!(
            "[recall] {name:14} {arm_hits}/{arm_counted} = {rate:.3}  \
             moved-evaluable: {}  best-hit-depth: {best_hit_depth:.6}  \
             deepest-miss-cutoff: {worst_miss_cutoff:.6}",
            arm_moved.len()
        );
        println!("[recall] {name:14} moved columns: {arm_moved:?}");
    }

    let t = Tally {
        hits,
        counted,
        void_no_movement,
        void_unrankable_only,
        seeds: base_rows.len(),
    };
    println!(
        "[recall] TOTAL {hits}/{counted} = {:.4} (bar {RECALL_BAR})",
        t.recall()
    );
    println!(
        "[recall] void pairs: {void_no_movement} moved nothing at all, \
         {void_unrankable_only} moved only columns outside the evaluable surface"
    );
    t
}
