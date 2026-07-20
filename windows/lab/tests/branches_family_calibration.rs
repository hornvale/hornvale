//! Calibration for The Branches (Task 10): the family battery, pinned per
//! ADR 0016 — directions preregistered before the sweep ran (see each
//! test's doc comment), exact measured values pinned after, never tuned to
//! pass. `branches-family` itself is frozen (census-as-data spec §1): the
//! battery now reads its 32 columns from the canonical census fixture
//! (`the-census`, 1,000 seeds, `metrics: "all"`) instead of its own
//! retired study/fixture pair; ADR 0016 pins are unchanged (same seeds,
//! same values — Task 4's equivalence check proved the columns identical).
use hornvale_lab::{
    MetricValue, RunResult, canonical_row, load_rows, load_study, record_failure, run,
};
use std::path::Path;
use std::sync::LazyLock;

/// The 1,000-seed canonical census, loaded ONCE from its committed
/// `rows.csv` fixture and shared by every calibration in this file (mirrors
/// `calibration.rs`'s `DRIFT`/`MEETING` per decision
/// 0032). The fixture is published by
/// `lab run` and regenerated + drift-checked in CI's "Artifacts are current"
/// step; `branches_fixture_matches_live_run` below pins fixture == live.
/// Loading instead of recomputing keeps the full live sweep off every
/// local `cargo test`. Init panics on a load error (a test-setup failure,
/// not a calibration).
static BRANCHES: LazyLock<RunResult> = LazyLock::new(|| {
    let study = load_study(Path::new("../../studies/the-census.study.json"))
        .expect("load the-census study");
    let csv = std::fs::read_to_string("../../book/src/laboratory/generated/the-census/rows.csv")
        .expect("read the-census fixture");
    load_rows(&study, &csv).expect("reconstruct the-census from fixture")
});

/// Guard — ignored by default because it pays the full live sweep: the
/// committed fixture reconstructs *exactly* what a live `run` produces, so
/// every other test in this file may trust the fixture. Run it after
/// regenerating the fixture, or explicitly:
/// `cargo test -p hornvale-lab --test branches_family_calibration -- --ignored`.
#[test]
#[ignore = "runs the full live census sweep; the fixture is drift-checked in CI"]
fn branches_fixture_matches_live_run() {
    let study = load_study(Path::new("../../studies/the-census.study.json"))
        .expect("load the-census study");
    let live = run(&study).expect("run the-census study");
    // Canonicalize live Numbers before comparing: the fixture's floats passed
    // the quantizing serialization boundary (`render_csv`), the live run's
    // have not (shared helper: `hornvale_lab::canonical_row`).
    let live = RunResult {
        study: live.study.clone(),
        metric_names: live.metric_names.clone(),
        rows: live.rows.iter().map(canonical_row).collect(),
    };
    let csv = std::fs::read_to_string("../../book/src/laboratory/generated/the-census/rows.csv")
        .expect("read the-census fixture");
    let loaded = load_rows(&study, &csv).expect("reconstruct census from fixture");
    assert_eq!(
        loaded, live,
        "fixture diverged from a live run — regenerate with \
         `lab run studies/the-census.study.json`"
    );
}

/// The column index of `name` within `BRANCHES`'s rows.
fn col(name: &str) -> usize {
    BRANCHES
        .metric_names
        .iter()
        .position(|n| *n == name)
        .unwrap_or_else(|| panic!("metric {name} not in the branches-family study"))
}

/// Every row's `MetricValue::Flag` at `idx`, panicking on any non-flag,
/// non-absent value (a schema drift this suite would want to fail loudly
/// on) and skipping `Absent` rows (none are expected over the shipped
/// four-people roster, but the study is not scoped to guarantee it).
fn flags(idx: usize) -> Vec<(u64, bool)> {
    BRANCHES
        .rows
        .iter()
        .filter_map(|row| match row.values[idx] {
            MetricValue::Flag(v) => Some((row.seed, v)),
            MetricValue::Absent => None,
            ref other => panic!("seed {}: expected a flag, got {other:?}", row.seed),
        })
        .collect()
}

/// Every row's `MetricValue::Number` at `idx`, panicking on any non-number,
/// non-absent value.
fn numbers(idx: usize) -> Vec<f64> {
    BRANCHES
        .rows
        .iter()
        .filter_map(|row| match row.values[idx] {
            MetricValue::Number(v) => Some(v),
            MetricValue::Absent => None,
            ref other => panic!("seed {}: expected a number, got {other:?}", row.seed),
        })
        .collect()
}

#[test]
fn lexicon_regular_family_holds_on_every_swept_seed() {
    // Preregistered (spec §9.1, generalized family-wide): Neogrammarian
    // regularity is mechanical (`evolve` is a pure per-segment function),
    // so this is expected to hold on every seed with no exceptions.
    let rows = flags(col("lexicon-regular-family"));
    assert_eq!(rows.len(), 1000, "every swept seed must report a flag");
    let failures: Vec<u64> = rows.iter().filter(|(_, v)| !v).map(|(s, _)| *s).collect();
    if let Some(&first_failure) = failures.first() {
        // Best-effort black-box: record the first failing seed's world so a
        // developer gets it on disk, not just a seed number. An io failure
        // here must never mask the real assertion failure below.
        let pin_set = BRANCHES
            .rows
            .iter()
            .find(|r| r.seed == first_failure)
            .map(|r| r.pin_set.as_str())
            .unwrap_or("default");
        let recording_note = match record_failure(&BRANCHES.study, first_failure, pin_set) {
            Ok(path) => format!(" Failing world recorded at {}.", path.display()),
            Err(_) => String::new(),
        };
        panic!("lexicon-regular-family failed on seeds {failures:?}.{recording_note}");
    }
}

#[test]
fn monophyly_goblinoid_holds_on_every_swept_seed() {
    // Preregistered (spec §3): every goblinoid daughter's Root traces to
    // the shared family proto-root by construction (`build_lexicon` draws
    // it once at the family level), so this is expected to hold on every
    // seed with no exceptions.
    let rows = flags(col("monophyly-goblinoid"));
    assert_eq!(rows.len(), 1000);
    let failures: Vec<u64> = rows.iter().filter(|(_, v)| !v).map(|(s, _)| *s).collect();
    assert!(
        failures.is_empty(),
        "monophyly-goblinoid failed on seeds {failures:?}"
    );
}

#[test]
fn inventory_closure_holds_on_every_swept_seed_for_every_daughter() {
    // Preregistered (spec §2.2): `evolve`'s codomain constraint plus its
    // final `nativize` pass together guarantee every modern form lands in
    // the daughter's own inventory, so this is expected to hold on every
    // seed with no exceptions, for all four daughters.
    for species in ["goblin", "hobgoblin", "bugbear", "kobold"] {
        let rows = flags(col(&format!("inventory-closure-{species}")));
        assert_eq!(
            rows.len(),
            1000,
            "{species}: every swept seed must report a flag"
        );
        let failures: Vec<u64> = rows.iter().filter(|(_, v)| !v).map(|(s, _)| *s).collect();
        assert!(
            failures.is_empty(),
            "{species}: inventory-closure failed on seeds {failures:?}"
        );
    }
}

#[test]
fn divergence_real_holds_on_every_swept_seed() {
    // Preregistered (spec §3's stemmatics guard, the seed-swept form of the
    // Task 6 `goblinoid_daughters_actually_diverge` guard): the daughters'
    // inventories differ along the loudness axis, so nativization is
    // expected to diverge at least one shared-root concept on every seed,
    // with no exceptions.
    let rows = flags(col("divergence-real"));
    assert_eq!(rows.len(), 1000);
    let failures: Vec<u64> = rows.iter().filter(|(_, v)| !v).map(|(s, _)| *s).collect();
    assert!(
        failures.is_empty(),
        "divergence-real failed on seeds {failures:?}"
    );
}

/// Honest finding, pinned not forced (ADR 0016): "clean outgroup" is a
/// STATISTICAL near-certainty, not a structural one — kobold's proto-roots
/// and the goblinoid family's are independent draws (different
/// seed-derivation paths, different phonologies), so a coincidental collision
/// on some concept is possible purely by chance. Under The Words' per-concept
/// draw this tail actually surfaced twice over the sweep (seeds 278, 816).
/// The `root/v2` injective assignment SCATTERS colliders through a
/// probe-keyed sub-stream and additionally holds core roots apart by a
/// minimal pair, which pushes kobold's forms further from the goblinoid
/// family's: re-measured under v2, the coincidence rate falls to zero —
/// 1000/1000 clean, no exceptions. Pinned as empty, re-derived not forced.
#[test]
fn clean_outgroup_kobold_holds_on_every_swept_seed() {
    let rows = flags(col("clean-outgroup-kobold"));
    assert_eq!(rows.len(), 1000);
    let failures: Vec<u64> = rows.iter().filter(|(_, v)| !v).map(|(s, _)| *s).collect();
    assert!(
        failures.is_empty(),
        "clean-outgroup-kobold coincided with the goblinoid family on seeds \
         {failures:?} — re-derive and re-pin, don't force back to empty"
    );
}

/// Preregistered (spec §3, ADR 0016 — direction stated BEFORE this sweep
/// ran): a quieter people draws a smaller inventory and so nativizes more
/// proto-contrasts away, giving the loudness ordering **bugbear ≥ goblin
/// ≥ hobgoblin** in divergence magnitude (voice_loudness: bugbear 0.3 <
/// goblin 0.5 < hobgoblin 0.8 — the SPECIES registry values, quietest to
/// loudest). Divergence magnitude is a discrete count driven by which
/// proto segments a given seed's random draw happens to land off-inventory,
/// so the ordering is claimed at the POPULATION level (the mean over the
/// sweep), not as a per-seed total order — three independently noisy counts
/// are not expected to rank identically on every single seed. Measured over
/// 1,000 seeds: mean divergence magnitude goblin 3.059, hobgoblin 2.486,
/// bugbear 4.480 — the aggregate ordering HOLDS decisively (bugbear's mean
/// is roughly 1.8x hobgoblin's). Per-seed, the full strict chain holds on
/// only 588/1000 seeds (58.8%) — NOT a universal invariant, reported
/// honestly rather than forced; the pairwise majorities are directionally
/// consistent: bugbear ≥ goblin on 860/1000 (86.0%), goblin ≥ hobgoblin on
/// 719/1000 (71.9%), bugbear ≥ hobgoblin on 908/1000 (90.8%).
#[test]
fn divergence_magnitude_loudness_ordering_holds_in_aggregate_not_per_seed() {
    let goblin = numbers(col("divergence-magnitude-goblin"));
    let hobgoblin = numbers(col("divergence-magnitude-hobgoblin"));
    let bugbear = numbers(col("divergence-magnitude-bugbear"));
    assert_eq!(goblin.len(), 1000);
    assert_eq!(hobgoblin.len(), 1000);
    assert_eq!(bugbear.len(), 1000);

    let mean = |xs: &[f64]| xs.iter().sum::<f64>() / xs.len() as f64;
    let (mg, mh, mb) = (mean(&goblin), mean(&hobgoblin), mean(&bugbear));

    // The aggregate ordering: pinned exact means.
    //
    // Census regen (2026-07-16, post-sculpting/isotherm/true-name 1000-seed
    // regen, commit 1c954d0): re-measured (goblin 3.058 -> 3.057).
    assert!((mg - 3.057).abs() < 1e-9, "goblin mean drifted: {mg}");
    // Census regen (2026-07-18, the-chorus close, regen commit fe2332c):
    // re-measured (was 2.485) — accumulated lexeme-space drift (the person
    // concept (C2), the grammar streams (C3), The Echo) surfacing at the
    // fixtures' first refresh since; the chorus itself adds zero draws
    // (genesis byte-identical).
    // Local-canonical adoption (2026-07-19, The Local Census, decision 0063):
    // this machine is now the reference platform; re-measured 2.486 -> 2.485
    // (a discrete count flips on ~1 seed between the old AWS goldens and local).
    assert!((mh - 2.485).abs() < 1e-9, "hobgoblin mean drifted: {mh}");
    // Census regen (2026-07-16, post-sculpting/isotherm/true-name 1000-seed
    // regen, commit 1c954d0): re-measured (bugbear 4.482 -> 4.481).
    assert!((mb - 4.481).abs() < 1e-9, "bugbear mean drifted: {mb}");
    assert!(
        mb >= mg && mg >= mh,
        "loudness ordering failed in aggregate: bugbear {mb} >= goblin {mg} >= hobgoblin {mh}"
    );

    // Per-seed rates: pinned, NOT asserted as a universal invariant — the
    // point of this second half of the test is to keep the honest,
    // non-universal rate on record rather than silently forgetting it.
    let n = goblin.len();
    let bg = (0..n).filter(|&i| bugbear[i] >= goblin[i]).count();
    let gh = (0..n).filter(|&i| goblin[i] >= hobgoblin[i]).count();
    let bh = (0..n).filter(|&i| bugbear[i] >= hobgoblin[i]).count();
    let chain = (0..n)
        .filter(|&i| bugbear[i] >= goblin[i] && goblin[i] >= hobgoblin[i])
        .count();
    // Census regen (2026-07-16, post-sculpting/isotherm/true-name 1000-seed
    // regen, commit 1c954d0): per-seed rates re-measured below.
    assert_eq!(bg, 859, "bugbear>=goblin rate drifted: {bg}/{n}");
    assert_eq!(gh, 718, "goblin>=hobgoblin rate drifted: {gh}/{n}");
    assert_eq!(bh, 908, "bugbear>=hobgoblin rate drifted: {bh}/{n}");
    assert_eq!(chain, 586, "full-chain per-seed rate drifted: {chain}/{n}");
}

/// Observation, not a pass/fail invariant (spec §3's merger-induced
/// homophony — the L4 confound banked, per the task brief, NOT asserted as
/// a claim to hold): expected highest among the goblinoid daughters for
/// bugbear (smallest family inventory). Re-measured over 1,000 seeds after the
/// **merger-aware assignment** (epoch root/v3): this is the RAW pair count over
/// the whole vocabulary — mean goblin 2.589, hobgoblin 1.631, bugbear 6.765,
/// kobold 2.454, down again from the phonology-epoch counts (goblin 3.618,
/// bugbear 11.234) because choosing core protos that survive each daughter's
/// cascade distinct also clears many periphery collisions. The functional-load
/// number Nathan targets, `core-homophony-*`, is now exactly ZERO for every
/// daughter on every seed (asserted in the Lab's
/// `core_homophony_is_zero_for_every_daughter_under_the_merger_aware_assignment`);
/// what remains here is periphery-only. Bugbear stays highest among the
/// goblinoid daughters as expected (smallest family inventory nativizes the most
/// proto-contrasts back together). The residual is merger-induced (distinct
/// protos re-merged by the cascade or nativization — see `homophony-merger-share-*`),
/// atonal-tail accounting (`confusable-homophony-*`) now measures.
#[test]
fn homophony_count_is_measured_and_pinned() {
    let goblin = numbers(col("homophony-count-goblin"));
    let hobgoblin = numbers(col("homophony-count-hobgoblin"));
    let bugbear = numbers(col("homophony-count-bugbear"));
    let kobold = numbers(col("homophony-count-kobold"));
    for xs in [&goblin, &hobgoblin, &bugbear, &kobold] {
        assert_eq!(xs.len(), 1000);
        assert!(
            xs.iter().all(|&n| n >= 0.0),
            "a homophony count went negative"
        );
    }

    let mean = |xs: &[f64]| xs.iter().sum::<f64>() / xs.len() as f64;
    let (mg, mh, mb, mk) = (
        mean(&goblin),
        mean(&hobgoblin),
        mean(&bugbear),
        mean(&kobold),
    );
    // merge (2026-07-11, main into campaign-crust): re-pinned on the merged
    // code — the L6 terrain relocates settlements and reshapes each people's
    // naming draws, shifting the family homophony means (was goblin 2.589,
    // hobgoblin 1.631, bugbear 6.765, kobold 2.454).
    // libm (decision 0041, 2026-07-13): kobold re-pinned 2.509 -> 2.501; the
    // other three are unchanged to 1e-9 (Apple libm == crate libm there).
    // Census regen (2026-07-14, the-gathering + night-sky, 1000-seed
    // `the-census`): goblin re-pinned 2.702 -> 2.273, hobgoblin 1.638 ->
    // 1.989, bugbear 6.818 -> 8.047, kobold 2.501 -> 2.384 (the-gathering's
    // field condensation shifts which settlements each seed fields, moving
    // every daughter's periphery homophony draws).
    // Census regen (2026-07-16, post-sculpting/isotherm/true-name 1000-seed
    // regen, commit 1c954d0): goblin re-pinned 2.273 -> 1.852, hobgoblin
    // 1.989 -> 1.755, bugbear 8.047 -> 3.007, kobold 2.384 -> 1.006 (the
    // true-name/KindId work reshapes each daughter's periphery homophony
    // draws).
    // Census regen (2026-07-16 #2, rift-and-fit terrain epoch v4 +
    // the-terminator SKY-24, commit 945f62b): the epoch relocates
    // settlements and shifts each people's naming draws; re-measured
    // (goblin 1.852 -> 1.811, hobgoblin 1.755 -> 1.649, kobold 1.006 ->
    // 0.920; bugbear unchanged at 3.007).
    // Census regen (2026-07-18, the-chorus close, regen commit fe2332c):
    // re-measured (was 1.811) — accumulated lexeme-space drift (the person
    // concept (C2), the grammar streams (C3), The Echo) surfacing at the
    // fixtures' first refresh since; the chorus itself adds zero draws
    // (genesis byte-identical).
    // Local-canonical adoption (2026-07-19, The Local Census, decision 0063):
    // re-measured on this machine (goblin 1.843 -> 2.365; hobgoblin/bugbear
    // below). The large goblin move inherits origin/main's un-pinned physics
    // (the AWS goldens lagged ~26 commits before this first local refresh).
    // The Demesne (BIO-35 Stage 1) local regen, lefford 2026-07-20:
    // spatial supply relocates settlements and shifts each people's naming
    // draws (goblin 2.365 -> 1.939; others below).
    assert!((mg - 1.939).abs() < 1e-9, "goblin mean drifted: {mg}");
    assert!((mh - 1.689).abs() < 1e-9, "hobgoblin mean drifted: {mh}");
    assert!((mb - 3.025).abs() < 1e-9, "bugbear mean drifted: {mb}");
    assert!((mk - 0.904).abs() < 1e-9, "kobold mean drifted: {mk}");
    assert!(
        mb > mg && mb > mh,
        "expected bugbear's homophony mean highest among the goblinoid daughters: {mb} vs goblin {mg}, hobgoblin {mh}"
    );
}
