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
/// - the census gained columns and the fixtures did not → a queued census
///   delivery (`scripts/sluice-census.sh`) re-authors them at the census's
///   ref, under the box lock, in the same commit as the goldens (decision
///   0836); a delivery that could not — a ref predating The Spillway, whose
///   authoring script refuses the staged goldens as dirt — is refused at its
///   pre-flight, and the by-hand path is `scripts/gnomon-injection.sh` on the
///   canonical box;
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
             in the same commit as the census refresh — a queued delivery \
             (make sluice-census) does this itself since The Spillway, \
             decision 0836 — never filter the mismatch away."
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
    let base = fixtures().join("baseline-a");

    for name in arm_names("injection") {
        // AS AUTHORED, through each arm's own committed `schema.json`
        // (`hornvale_lab::authored`) — never through the live registry. These
        // arms are authored evidence: `scripts/gnomon-injection.sh` can only
        // re-author them on the canonical box at a checked-out SHA, so reading
        // them through a registry that keeps growing would make every past
        // injection unreadable the moment anybody registers a metric.
        let (report, age) = hornvale_lab::render_authored_diff(
            &study,
            &base,
            "baseline-a",
            &fixtures().join(&name),
            &name,
        )
        .expect("diff renders");
        age.announce(&format!("injection fixtures ({name} vs baseline-a)"));
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
        let (report, _) = hornvale_lab::render_authored_diff(
            &study,
            &base,
            "baseline-a",
            &fixtures().join(&name),
            &name,
        )
        .expect("diff renders");
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
#[ignore = "PREREGISTERED, cannot adjudicate at n=120: awaits TOOL-anomaly-ranking-concentrates-injection (recall@10 = 0.5333 over 120 pairs; thirteenth canonical reading, incomparable because the Trencher's post-merge census re-seated the world and fixture inputs; the scorer is unchanged and the evaluable surface shrank 181 -> 179 with a measured share of exactly zero, so this re-read is not comparable to the six-epoch series. The six comparable readings remain within one SE of the 0.60 bar; the battery still separates nothing)"]
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
/// `cli/tests/suite/heavy_tier.rs` carries the figure, in its
/// `EXPECTED_UNTOKENISED` roster — NOT in the heavy tier, which this battery
/// is not in). But an ignored measurement
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
/// **RE-READ A FOURTH TIME AT THE GRANARY'S CLOSE (2026-08-24), WITH A HOST
/// CAVEAT THE EARLIER RE-READS DID NOT NEED.** The Granary's sub-year raid
/// timing moved the worlds, and the campaign's first measurement of the
/// refreshed battery read **70/120 = 0.5833** — but that figure came from
/// LOCAL PILOT fixtures (`HV_GNOMON_PILOT=1`, Nathan-authorized), which sit
/// in the known ~0.1% discrete-count host-divergence class against the
/// census goldens. The CANONICAL re-author on lefford (fixtures at
/// `13695c4ae`, census goldens at `c54fb62c9`) restored **73/120 = 0.6083,
/// +0.19 SE** — numerically identical to The Glasshouse/Burr readings.
/// Nothing about the *report* changed. The canonical reading list stays:
/// 0.5667 / 0.6083 / 0.6000 / 0.6083 / 0.6083 across five epochs — all
/// within one SE of the bar. The pilot detour arguably STRENGTHENS the
/// underpower diagnosis: even a world-moving campaign leaves the canonical
/// reading inside one SE of the bar — and an off-host fixture set can move
/// the tally by three hits, which is exactly why only canonical readings
/// enter this list.
///
/// **RE-READ A FIFTH TIME AT THE WINZE'S CLOSE (2026-08-29), AND THIS IS THE
/// FIRST RE-READ WHERE THE RANKED SURFACE ITSELF MOVED.** Every earlier
/// re-read could say "nothing about the report changed" and mean it in the
/// strong sense: same `REPORT_SIZE`, same `TAIL_DEPTH_BAR`, same scorer, and
/// the same set of columns the scorer ranks over. The Winze registered a new
/// census metric, `breached-delving-count`, and it lands EVALUABLE — so the
/// surface [`anomaly::score_row`] ranks over grew from 117 columns to 118,
/// while the report stayed a fixed top-`REPORT_SIZE` cut of it. One more
/// candidate column can displace the tenth flag of any world, and the moved
/// set a hit is scored against is itself intersected with the surface, so
/// there are two live mechanisms by which the INSTRUMENT rather than the
/// world could have moved the tally. Re-measured on the refreshed census and
/// re-authored fixtures: **72/120 = 0.6000**, exactly on the bar.
///
/// **THE CONFOUND WAS MEASURABLE AND IT MEASURES NULL.** Two checks settle
/// it, both run this campaign, neither requiring a line of committed code to
/// change:
///
/// 1. *The surface moved by exactly one column.* Running
///    [`anomaly::evaluable_columns`] over the pre-refresh committed census
///    and over the refreshed one: evaluable 117 → 118, excluded 50 → 50, the
///    difference set `{breached-delving-count}` in the new direction and
///    **empty** in the old. Nothing else crossed the boundary under the world
///    change, so `breached-delving-count` is the whole of the instrument
///    delta rather than merely the visible part of it.
/// 2. *Ablating that one column reproduces the tally exactly.* Dropping
///    `breached-delving-count` from the loaded `Census` restores the
///    117-column surface, and re-scoring the same committed fixtures against
///    it reads **72/120 — and 20/20, 4/20, 20/20, 2/20, 7/20, 19/20 arm for
///    arm**, identical in every arm. The new column contributed zero hits and
///    zero counted pairs.
///
/// The ablation is cheap and needs no change to the scorer, which is the
/// reason it was worth running instead of declaring the reading
/// incomparable: [`anomaly::evaluable_columns`] derives the surface from
/// `Census::columns`, [`anomaly::score_row`] takes the census by reference,
/// and each column's index is built independently of the others — so
/// removing one column from an in-memory census reproduces the older surface
/// against the newer worlds exactly. **A future re-read that finds the
/// surface has moved should run these same two checks before concluding
/// anything**, in either direction: a null here licenses comparison, and a
/// non-null would be the larger question the assertion below warns about.
///
/// So this is a sixth COMPARABLE reading, and it repeats The Underworld's:
///
/// ```text
///     SE at the bar = sqrt(0.6 * 0.4 / 120) = 0.04472
///
///     68/120 = 0.5667   -0.75 SE   The Gnomon      (published as refuted)
///     73/120 = 0.6083   +0.19 SE   The Glasshouse  (verdict withdrawn)
///     72/120 = 0.6000    0.00 SE   The Underworld  (corroborated the withdrawal)
///     73/120 = 0.6083   +0.19 SE   The Burr        (repeats The Glasshouse)
///     73/120 = 0.6083   +0.19 SE   The Granary     (canonical; a pilot first read 70)
///     72/120 = 0.6000    0.00 SE   The Winze       (repeats The Underworld)
/// ```
///
/// Six readings, six census epochs, one report whose *definition* has never
/// moved, every one of them inside ±0.75 SE of the bar. **Landing exactly ON
/// a bar this battery cannot adjudicate is worth one sentence and no more: it
/// is not a result, it is the same null arriving at its least legible
/// coordinate.** The verdict remains "cannot tell"; the instrument remains
/// underpowered; the fix remains more pairs, not a moved bar. The registry
/// row's status is unchanged, as it was unchanged by each of the four before
/// it.
///
/// One arm-level note, recorded because it is the closest thing to
/// reassurance the ablation's null can offer: `breached-delving-count`
/// appears in GEOTHERMAL's own moved-column set, which is physically
/// coherent — a geothermal-gradient perturbation moves cave depth, which
/// moves delve depth, which moves breaches. The new column is not inert. It
/// simply never displaced anything out of a top-10 report in a way that
/// changed whether a pair scored.
///
/// **RE-READ A SIXTH TIME AT THE WEFT'S CLOSE (2026-09-04), AND THE
/// INSTRUMENT ABLATION IS NON-NULL.** The canonical census added 22 numeric
/// Weft metrics. Twenty-one vary and enter the ranked surface; only
/// `weft-legibility-mi-erratic` is frozen and excluded. The surface therefore
/// grew 118 -> 139 while excluded columns grew 50 -> 51. This is not the
/// Winze's one-column perturbation repeated at a larger count: twenty-one new
/// candidates can displace the fixed report's tenth flag.
///
/// Re-authoring all eight fixture arms on lefford at census delivery
/// `b162273b4` produced **69/120 = 0.5750**, with zero void pairs. Arm for arm:
/// geothermal 20/20, unconformity 4/20, aquifer 20/20, karst 2/20, pantheon
/// 6/20, phonology 17/20. The preregistered bar remains 0.60; nothing about
/// this re-read changes it.
///
/// The same ablation test used by The Winze now gives the opposite kind of
/// answer. Removing the Weft family from the in-memory census (the 21 ranked
/// columns plus the one already-excluded column, which is inert here) and
/// scoring the SAME freshly authored fixture rows restores **72/120**, and
/// restores the preceding arm totals exactly: 20/20, 4/20, 20/20, 2/20,
/// 7/20, 19/20. The new ranked surface therefore displaced three hits — one
/// in the pantheon arm and two in phonology. Its share is measured and is not
/// null.
///
/// Consequently 69/120 is a pinned current witness but **not a seventh point
/// in the six-epoch comparable series**. Numerically it still sits within one
/// standard error of the bar, but adding it to that series would erase the
/// very instrument confound the ablation found. The standing verdict remains
/// "cannot tell": the six comparable readings still cluster within one SE,
/// and the remedy remains more pairs rather than a moved bar. The difference
/// from The Winze is the durable finding: an ablation licenses comparison
/// only when it reads null; this one does not.
///
/// **RE-READ A SEVENTH TIME AT THE WARP'S CLOSE (2026-09-05), AND THE
/// ABLATION HAD TO BE RUN FAMILY BY FAMILY TO MEAN ANYTHING.** The Warp
/// registered 32 numeric `warp-*` metrics. Thirty-one vary and enter the
/// ranked surface; only `warp-found-fraction-erratic` is excluded, Absent by
/// construction. The surface therefore grew 139 -> 170 while excluded columns
/// grew 51 -> 52 (the census's metric columns, 249 -> 281). Its goldens were
/// authored on lefford at `4a419e996ef7`, and the eight fixture arms were
/// re-authored there against them in the same delivery (manifest sha
/// `8c33817b9`, 8 arms x 20 rows, zero refusals).
///
/// On the full new surface the witness reads **66/120 = 0.5500**, zero void
/// pairs. Arm for arm: geothermal 20/20, unconformity 2/20, aquifer 20/20,
/// karst 2/20, pantheon 6/20, phonology 16/20. The bar is still 0.60 and this
/// re-read does not move it.
///
/// **The Weft's ablation asked one question; this census needed two.** The
/// obvious ablation — drop the newest family and re-score — does NOT restore
/// the previous witness, and reading only that would have mis-attributed the
/// move. Measured over the same freshly authored rows, dropping columns from
/// the in-memory `Census` by name prefix:
///
/// ```text
///     ablated                      surface (eval/excl)   tally           arms (geo, unc, aqu, kar, pan, pho)
///     none (the committed census)      170 / 52          66/120 0.5500   20, 2, 20, 2, 6, 16
///     warp-*                           139 / 51          67/120 0.5583   20, 2, 20, 2, 6, 17
///     warp-* and weft-*                118 / 50          72/120 0.6000   20, 4, 20, 2, 7, 19
///     warp-* and weft-legibility-mi-*  136 / 50          68/120 0.5667   20, 3, 20, 2, 6, 17
///     weft-legibility-mi-* alone       167 / 51          66/120 0.5500   20, 2, 20, 2, 6, 16
///     weft-* alone                     149 / 51          67/120 0.5583   20, 2, 20, 2, 6, 17
/// ```
///
/// Read the first three rows in order. Ablating `warp-*` reproduces The
/// Weft's surface EXACTLY — 139 evaluable, 51 excluded, the same two integers
/// that campaign recorded — and yet it reads **67/120, not the 69/120 The Weft
/// pinned**. So the Warp family's own instrument share is **one hit**
/// (phonology 17 -> 16); the other two hits of the 69 -> 66 move are NOT the
/// new surface at all. They are the Weft's own columns changing in VALUE:
/// this campaign re-parameterised the spring and overhang kinds, and the
/// census non-regression readout at its close measured every `weft-*` spring
/// and overhang column moving on 998-1,000 of 1,000 rows while every thicket
/// and erratic column moved on zero. A column that moves in value moves both
/// the ranking prior it contributes to and the moved-set a hit is scored
/// against, which is a third mechanism neither The Winze nor The Weft had to
/// separate.
///
/// Ablating both families restores The Winze's 118-column surface and reads
/// **72/120 arm for arm (20, 4, 20, 2, 7, 19)** — identical to The Winze's own
/// reading, and identical to the ablated tally The Weft measured. On the
/// surface all three campaigns share, nothing has moved across two further
/// census epochs. That is the closest thing to a control this witness has,
/// and it is reassuring about the SCORER while saying nothing about the bar.
///
/// So 66/120 is the eighth canonical reading and the second consecutive
/// **incomparable** one: it extends The Weft's case rather than the
/// six-epoch series, which stays 0.5667 / 0.6083 / 0.6000 / 0.6083 / 0.6083 /
/// 0.6000. The standing verdict is unchanged — "cannot tell", an underpowered
/// instrument, and the remedy is more pairs rather than a moved bar.
///
/// **The durable lesson is the family-by-family requirement.** A single
/// ablation of the newest family would have read "the instrument displaced
/// one hit" and quietly attributed the other two to the world. Ablate each
/// family that CHANGED since the pinned reading — the one that was added and
/// the ones whose values moved — and report the ladder, not one rung of it.
///
/// **RE-READ A TENTH TIME AT THE MURRAIN'S CLOSE (2026-09-08).** The Murrain
/// changed committed world facts by absorbing the social-household layer. The
/// census surface grew by five columns — four epidemic metrics and
/// `lot-named-disease-deaths` — and 17 injection fixture files were re-authored
/// (+752/-192) across all six scored arms and both baselines. The scorer itself
/// was unchanged.
///
/// On that changed surface the witness reads **63/120 = 0.5250**, zero void
/// pairs. Arm for arm: geothermal 20/20, unconformity 2/20, aquifer 20/20,
/// karst 2/20, pantheon 6/20, phonology 13/20. The bar is still 0.60 and this
/// re-read does not move it.
///
/// So 63/120 is the eleventh canonical reading and the fifth consecutive
/// **incomparable** one. The six-epoch series stays 0.5667 / 0.6083 / 0.6000 /
/// 0.6083 / 0.6083 / 0.6000, and the verdict stays "cannot tell". This is not
/// a new point in that comparable series: both the census surface and fixture
/// inputs changed along with the committed world facts.
///
/// **RE-READ AT THE UNDERWORLD PEOPLES' CLOSE (2026-09-10).** Four new sentient
/// peoples changed the committed world and the census inputs together; the scorer
/// was unchanged. The witness reads **68/120 = 0.5667**, with no void pairs.
/// Arm for arm: geothermal 20/20, unconformity 1/20, aquifer 20/20, karst 3/20,
/// pantheon 7/20, phonology 17/20. This is the twelfth canonical reading and
/// another incomparable one; the six comparable readings remain within one
/// standard error of the 0.60 bar, so the verdict remains "cannot tell".
///
/// **RE-READ AT THE TRENCHER'S CLOSE (2026-09-13), AND IT IS THE FIRST
/// SHRINKING SURFACE THIS WITNESS HAS SEEN.** The post-merge census
/// (`06055072d639`, goldens `0865b31bb`) rewrote all 1,000 census rows and
/// re-authored all eight injection arms at the same ref. The witness reads
/// **64/120 = 0.5333**, zero void pairs. Arm for arm: geothermal 20/20,
/// unconformity 2/20, aquifer 20/20, karst 3/20, pantheon 6/20, phonology
/// 13/20. The bar is still 0.60 and this re-read does not move it.
///
/// **The instrument did not move, and that was established rather than
/// assumed.** `windows/lab/src/domesday/` — the scorer, `score_row`,
/// `REPORT_SIZE`, `TAIL_DEPTH_BAR` and `evaluable_columns` itself — has ZERO
/// commits between the previously pinned reading's tree (`0865b31bb~1`) and
/// this one. No metric was registered or deregistered: a sorted, line-by-line
/// diff of the two censuses' CSV headers is EMPTY (295 columns less `seed`,
/// `pin_set` and `refusal` on both). The evaluable surface nonetheless moved
/// 181 -> 179, because `evaluable_columns` is a pure function of the
/// committed CSV and two EXISTING columns crossed to the excluded rails on
/// their own moved values: `first-day-occ-cause-famine` (32 at min, 15 at max
/// of 707) and `toponymic-roots-won` (11 at min, 603 at max of 1,000).
/// Nothing crossed the other way.
///
/// **THE PRESCRIBED ABLATION DOES NOT FIT A SHRINK, AND THAT IS THE FINDING
/// WORTH MORE THAN THE NUMBER.** Every worked case above — The Winze, The
/// Weft, The Warp — is a surface that GREW, where the instrument delta is a
/// set of columns present only in the new census, so removing them literally
/// reconstructs the old surface and "restores the previous reading arm for
/// arm" is a reachable null. On a shrink the delta is columns the new census
/// still CONTAINS but now classifies as excluded. They are already outside
/// its evaluable surface, so ablating them is a **no-op by construction** —
/// measured, 64/120 before and after, arm for arm. No operation on the new
/// census restores the old surface, because ablation removes columns and what
/// would be needed is to re-admit two the classifier rejected, i.e. to
/// override `evaluable_columns`' verdict on the data in order to manufacture
/// comparability. The null arm being unreachable means the **non-null arm
/// fires spuriously**: a shrink always reads "non-null", and the standing
/// instruction ("THAT is when to stop and say so") would have refused this
/// re-pin on the epoch carrying the cleanest instrument evidence this witness
/// has ever had. "Ablate family by family" has no unit here either — nothing
/// was registered, and the 97 value-moving columns span 37 name prefixes, 24
/// of them singletons, so prefix-grouping partitions the world's movement
/// rather than the instrument's.
///
/// **The generalisation that does fit: two-sided invariance.** Ablate the
/// symmetric difference from BOTH epochs and ask whether EITHER side's tally
/// moves, instead of whether the two sides meet. If neither moves, the
/// surface delta is inert and its share is zero — strictly stronger than "the
/// numbers restore". Measured over the same rows, dropping columns from the
/// in-memory `Census` by name:
///
/// ```text
///     ablated                        census  surface(eval/excl)  tally    arms (geo, unc, aqu, kar, pan, pho)
///     none                           OLD        181 / 51         68/120   20, 1, 20, 3, 7, 17
///     none                           NEW        179 / 53         64/120   20, 2, 20, 3, 6, 13
///     the 2 departed columns         OLD        179 / 51         68/120   20, 1, 20, 3, 7, 17
///     the 2 departed columns         NEW        179 / 51         64/120   20, 2, 20, 3, 6, 13
///     all 97 value-movers            OLD        106 / 45         51/100   20, 1, 20, 2, -, 8
///     all 97 value-movers            NEW        106 / 45         51/100   20, 1, 20, 2, -, 8
/// ```
///
/// Rows 3 and 4 are the two-sided null: the surface delta changes NEITHER
/// epoch's tally, so its share is exactly zero. Rows 5 and 6 are the control
/// this witness has never had — on the surface where no value moved between
/// the two epochs, the two censuses read **identically, arm for arm**, which
/// says the scorer is stable and the entire -4 lives in the 97 columns whose
/// values moved. (The pantheon arm voids there: all 20 of its pairs move only
/// ablated columns, which is why the denominator is 100.) Row 1 is also the
/// positive control — the harness reproduces the pinned 68/120 and its arm
/// breakdown exactly before anything is ablated.
///
/// **Why the world moved is measured too, and it is not the absorb.** The
/// tally the pin held (68) was taken against main's COMMITTED census, which
/// was stale relative to the code. This branch's own earlier delivery
/// (`bdc59cc18`, pre-merge) and this one differ on exactly THREE metric
/// columns, all astronomy — `figure-count`, `largest-figure-members`,
/// `ecliptic-figure-count` — so the 41 absorbed commits of origin/main moved
/// almost nothing here, and the eleven calibration rows re-pinned alongside
/// this one return to their delivery-era values. Task 10's chorus memoisation
/// is byte-neutral and was proven so before it landed.
///
/// So 64/120 is the thirteenth canonical reading and incomparable to the
/// six-epoch series — but for the ORDINARY reason readings eleven and twelve
/// were (moved world facts arriving with moved census and fixture inputs),
/// NOT for an instrument reason. The series stays 0.5667 / 0.6083 / 0.6000 /
/// 0.6083 / 0.6083 / 0.6000, the verdict stays "cannot tell", and the remedy
/// is still more pairs rather than a moved bar.
///
/// claim: invariant(the committed battery scores exactly 64 hits over 120
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
        (64, 120, 0, 0),
        "the injection battery's recall tally moved. This is the WITNESS to The \
         Gnomon's finding (recall@10 now reads 64/120 = 0.5333 against the \
         preregistered bar of 0.60 — a bar this battery is NOT powered to \
         adjudicate; this thirteenth reading is incomparable to the six-epoch \
         series, as the seventh through twelfth were, but for the ORDINARY \
         reason — moved world facts arriving with moved census and fixture \
         inputs — and NOT for an instrument reason: the surface delta's share \
         was measured at exactly ZERO in both directions, see the doc \
         comment), and it is pinned so \
         that a change to \
         the report — REPORT_SIZE, TAIL_DEPTH_BAR, the scorer, the evaluable \
         surface, the census, or the fixtures — cannot silently turn the \
         published figure into fiction. \
         \
         DO NOT SIMPLY UPDATE THESE INTEGERS. Re-read the finding and re-state \
         it, in the same commit, at all FOUR of the sites below. The list was \
         wrong twice before and each error sent a reader somewhere that no \
         longer existed, so it names the file AND what in it holds the figure: \
         (1) THIS FILE — these integers, this test's doc comment, and the \
         `#[ignore]` reason on h1_recall_at_10 above; \
         (2) book/src/chronicle/the-gnomon.md — the postscript series; \
         (3) book/src/frontier/idea-registry.md — the \
         TOOL-anomaly-ranking-concentrates-injection row (capped at 600 chars, \
         so compact rather than append); \
         (4) cli/tests/suite/heavy_tier.rs — the EXPECTED_UNTOKENISED array, \
         which holds a VERBATIM copy of that `#[ignore]` reason and is an \
         exact-set assertion, so changing the reason without it reds \
         the_untokenised_ignore_reasons_are_exactly_this_roster. \
         Site 4's path and its DESCRIPTION were both stale here until The \
         Winze: the file moved to tests/suite/ with test-binary consolidation, \
         and this message called it `the heavy tier`, which it is not — this \
         battery carries no `heavy:` token and is in no lane set. It is the \
         UNTOKENISED roster that happens to live in the same file, and reading \
         `heavy tier` literally is exactly what makes a reader conclude the \
         site is gone. It is not. \
         \
         THE PIN HAS NOW BEEN RE-STATED SEVEN TIMES, producing eight readings \
         with The Gnomon's original: The Glasshouse (2026-08-15, overturned \
         the verdict); The Underworld (2026-08-17, corroborated the withdrawal \
         at a third census epoch); The Burr (2026-08-18/19, reproduced The \
         Glasshouse's exact reading at a fourth); The Granary (2026-08-24, \
         whose canonical reading repeated it a fifth time after a \
         host-divergent local pilot first said otherwise); The Winze \
         (2026-08-29, the sixth); The Weft (2026-09-04, the seventh and first \
         genuinely incomparable reading); and The Warp (2026-09-05, the \
         eighth, incomparable for a compound reason). (This message previously said FIVE TIMES \
         while naming four campaigns — it was counting readings in one clause \
         and re-statements in the other. Both counts are given above so the \
         next reader does not have to guess which is meant.) \
         \
         FIRST ASK WHETHER YOUR CHANGE TOUCHED THE REPORT. For the first four \
         re-statements the answer was no, and the number moved because the \
         WORLD moved, which was the strongest argument for keeping this pin. \
         The Winze is the first where the answer was YES: registering \
         `breached-delving-count` grew the evaluable surface 117 -> 118, so \
         instrument and world both changed at once. That is the LARGER \
         QUESTION this sentence has always warned about, and the way through \
         it is to MEASURE the instrument's share rather than to declare the \
         reading incomparable: diff evaluable_columns across the two censuses \
         to bound what moved, then ablate the moved columns out of an \
         in-memory Census and re-score. Both are cheap, neither touches the \
         scorer, and for The Winze both read null — the ablated tally is \
         72/120 arm for arm. See the doc comment for the full derivation. If \
         your own ablation does NOT read null, you have a genuinely \
         incomparable reading, and THAT is when to stop and say so. The Weft \
         is that case: its full surface read 69/120 (20, 4, 20, 2, 6, 17 by \
         arm), while ablating the Weft family restored 72/120 (20, 4, 20, 2, \
         7, 19). \
         \
         ABLATE FAMILY BY FAMILY, NOT JUST THE NEWEST ONE — The Warp is why. \
         Its full surface reads 66/120 (20, 2, 20, 2, 6, 16). Ablating only \
         the 32 `warp-*` columns reproduces The Weft's surface exactly (139 \
         evaluable, 51 excluded) but reads 67/120, NOT The Weft's 69/120: the \
         new family displaced ONE hit, and the other two moved because this \
         campaign re-parameterised the spring and overhang kinds, so the \
         Weft's own columns changed in VALUE (998-1,000 rows of 1,000 each; \
         thicket and erratic moved on zero). Ablating `warp-*` AND `weft-*` \
         together restores The Winze's 118-column surface and reads 72/120 \
         arm for arm (20, 4, 20, 2, 7, 19) — unchanged across two further \
         epochs, which is the control. Had only the newest family been \
         ablated, two of the three moved hits would have been silently \
         attributed to the world. \
         \
         AND NOW THE CASE THE THREE PARAGRAPHS ABOVE DO NOT COVER: A SURFACE \
         THAT SHRINKS. Every worked example above is a surface that GREW. The \
         Trencher (2026-09-13) is the first shrink — evaluable 181 -> 179, no \
         column added or removed, two EXISTING columns reclassified to `both \
         rails tied` by their own moved values — and the prescribed test \
         MISFIRES ON IT IN BOTH ARMS. (1) The null arm is unreachable by \
         construction: `restores the previous reading` needs ablation to be \
         able to remove the instrument delta, and on a grow it can, because \
         the delta is columns present only in the new census. On a shrink the \
         delta is columns the new census STILL CONTAINS but now classifies as \
         excluded — already outside its evaluable surface — so ablating them \
         is a NO-OP. Measured: 64/120 before and after, arm for arm. Nothing \
         done to the new census restores the old surface, because ablation \
         REMOVES columns and what would be needed is to RE-ADMIT two the \
         classifier rejected, which is overriding `evaluable_columns` to \
         manufacture comparability. (2) The non-null arm therefore fires \
         spuriously: a shrink always reads non-null, so `THAT is when to stop \
         and say so` would have refused this re-pin on the epoch with the \
         CLEANEST instrument evidence this witness has ever had. (3) `Family \
         by family` also has no unit here — nothing was registered, and the \
         97 value-movers span 37 name prefixes, 24 of them singletons, so \
         prefix-grouping partitions the WORLD's movement, not the \
         instrument's. \
         \
         THE TEST THAT DOES FIT A SHRINK, and run it instead: ablate the \
         symmetric difference from BOTH epochs and ask whether EITHER side's \
         tally moves, rather than whether the two sides meet. Two-sided \
         invariance is the right null — if neither moves, the surface delta \
         is INERT and its share is zero, which is a stronger claim than `the \
         numbers restore`. For The Trencher both sides were exactly \
         invariant: 68/120 -> 68/120 and 64/120 -> 64/120, arm for arm. And \
         the complementary control gives the scorer null this witness never \
         had: ablating every column whose VALUE moved (97 of 292) from both \
         epochs reads 51/100 IDENTICALLY, arm for arm (20, 1, 20, 2, -, 8). \
         On the value-stable surface the two censuses agree exactly, so the \
         whole -4 is world and the instrument's share is measured at zero."
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
