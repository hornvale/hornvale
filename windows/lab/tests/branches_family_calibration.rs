//! Calibration for The Branches (Task 10): the family battery, run once over
//! the 1,000-seed `branches-family` study and pinned per ADR 0016 —
//! directions preregistered before the sweep ran (see each test's doc
//! comment), exact measured values pinned after, never tuned to pass.
use hornvale_lab::{MetricValue, RunResult, load_study, run};
use std::path::Path;
use std::sync::LazyLock;

/// The 1,000-seed family-battery census, run ONCE and shared by every
/// calibration in this file (mirrors `calibration.rs`'s `DRIFT`). Init
/// panics on a load/run error (a test-setup failure, not a calibration).
static BRANCHES: LazyLock<RunResult> = LazyLock::new(|| {
    let study = load_study(Path::new("../../studies/branches-family.study.json"))
        .expect("load branches-family study");
    run(&study).expect("run branches-family study")
});

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
    assert!(
        failures.is_empty(),
        "lexicon-regular-family failed on seeds {failures:?}"
    );
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

/// Honest finding, pinned not forced (ADR 0016): unlike the mechanically
/// guaranteed flags above, "clean outgroup" is a STATISTICAL near-certainty,
/// not a structural one. Kobold's proto-root and the goblinoid family's
/// proto-root are independent draws (different seed-derivation paths,
/// different phonologies), each a short 1-2 syllable stem filled from a
/// modest phoneme inventory — a large but FINITE space, so a coincidental
/// collision on some concept is expected at some small nonzero rate purely
/// by chance (a birthday-paradox tail over ~dozens of concepts x 1,000
/// seeds), not a pipeline defect. Measured: 998/1000 true (99.8%); the two
/// measured exceptions are pinned by seed and concept below, each a single
/// short CV proto-root landing on the identical segment sequence by chance.
#[test]
fn clean_outgroup_kobold_holds_except_two_measured_coincidental_collisions() {
    let rows = flags(col("clean-outgroup-kobold"));
    assert_eq!(rows.len(), 1000);
    let failures: Vec<u64> = rows.iter().filter(|(_, v)| !v).map(|(s, _)| *s).collect();
    assert_eq!(
        failures,
        vec![278, 816],
        "the measured clean-outgroup exceptions moved — re-derive and re-pin, don't force back to these seeds"
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
/// 1,000 seeds: mean divergence magnitude goblin 3.058, hobgoblin 2.484,
/// bugbear 4.482 — the aggregate ordering HOLDS decisively (bugbear's mean
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
    assert!((mg - 3.058).abs() < 1e-9, "goblin mean drifted: {mg}");
    assert!((mh - 2.484).abs() < 1e-9, "hobgoblin mean drifted: {mh}");
    assert!((mb - 4.482).abs() < 1e-9, "bugbear mean drifted: {mb}");
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
    assert_eq!(bg, 860, "bugbear>=goblin rate drifted: {bg}/{n}");
    assert_eq!(gh, 719, "goblin>=hobgoblin rate drifted: {gh}/{n}");
    assert_eq!(bh, 908, "bugbear>=hobgoblin rate drifted: {bh}/{n}");
    assert_eq!(chain, 588, "full-chain per-seed rate drifted: {chain}/{n}");
}

/// Observation, not a pass/fail invariant (spec §3's merger-induced
/// homophony — the L4 confound banked, per the task brief, NOT asserted as
/// a claim to hold): expected highest among the goblinoid daughters for
/// bugbear (smallest family inventory). Measured over 1,000 seeds: mean
/// homophone-pair count goblin 15.056, hobgoblin 11.481, bugbear 28.606 —
/// bugbear is indeed highest among the three goblinoid daughters, matching
/// the expectation. Kobold (25.180, drawing the lowest voice_loudness of
/// all four peoples, 0.2 vs bugbear's 0.3) lands a close second overall,
/// ahead of goblin and hobgoblin but still behind bugbear — a bonus
/// cross-family data point (kobold isn't a goblinoid daughter, so it has no
/// preregistered ordering claim here) consistent with the general
/// small-inventory -> more-homophony mechanism, not just the family's own
/// nativization channel.
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
    assert!((mg - 15.056).abs() < 1e-9, "goblin mean drifted: {mg}");
    assert!((mh - 11.481).abs() < 1e-9, "hobgoblin mean drifted: {mh}");
    assert!((mb - 28.606).abs() < 1e-9, "bugbear mean drifted: {mb}");
    assert!((mk - 25.180).abs() < 1e-9, "kobold mean drifted: {mk}");
    assert!(
        mb > mg && mb > mh,
        "expected bugbear's homophony mean highest among the goblinoid daughters: {mb} vs goblin {mg}, hobgoblin {mh}"
    );
}
