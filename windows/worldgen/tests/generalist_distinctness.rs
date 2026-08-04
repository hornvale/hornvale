//! The Generalist, Task 5: human must be measurably NOT goblin.
//!
//! The vacuity check the spec §4 gates its readout behind. Two generalists
//! whose per-cell fit vectors are one generalist with two names would make
//! every H1-H3 result in Task 6 an artifact.
//!
//! **Why this file does NOT use Spearman rank correlation** (the plan's
//! original proposal, `.superpowers/sdd/2026-08-03-the-generalist/task-5-brief.md`):
//! human and goblin share the SAME elevation optimum (1500.0 m,
//! deliberately - see `human_condition_niche()`'s doc comment in
//! `domains/species/src/lib.rs`). `ConditionResponse::eval` is
//! `floor + (1 - floor) * devotion * exp(-0.5 * z^2)`
//! (`kernel/src/ecology.rs`): for a fixed optimum, `eval` is a strictly
//! monotone-decreasing function of `|field - optimum|` regardless of
//! `devotion` or `width` (both only rescale the same monotone shape). Rank
//! correlation is invariant under a monotone transform, so on the shared-
//! optimum elevation axis alone, human and goblin are RANK-IDENTICAL by
//! construction, no matter how devotion or width differs - the exact case
//! Spearman cannot see. Composing that with three axes where a rank
//! statistic COULD move made a threshold set anywhere near "expected for two
//! genuine generalists" dangerously close to the trap it was meant to guard
//! against.
//!
//! **The statistic this file uses instead: each kind's coefficient of
//! variation (CV = population stddev / mean) of per-cell fit, pooled over
//! the same settleable-cell population, compared as `cv(human) / cv(goblin)`.**
//! CV is scale-free, so it isolates *relative dispersion* (shape), not
//! overall magnitude - a claim a monotone-invariant rank statistic cannot
//! make at all.
//!
//! **A prediction this file's first draft got backwards, corrected against
//! the measured numbers (see `task-5-report.md`).** The initial hypothesis
//! was that human's uniformly LOWER devotion (0.20-0.30 vs goblin's
//! 0.35-0.45 on every axis) would flatten its response curves enough to
//! produce LOWER dispersion than goblin's. Measured reality is the
//! opposite: `cv(human) ≈ 1.05 × cv(goblin)` (human MORE dispersed), because
//! devotion is not the only axis-by-axis difference - human is also
//! NARROWER than goblin on temperature (22.0 vs 28.0) and elevation (2000.0
//! vs 3000.0), and a narrower Gaussian sigma steepens the fall-off in `z`
//! for a given physical distance from the optimum by more than a lower
//! devotion flattens the peak amplitude. Devotion alone predicts flatter;
//! devotion-and-width together (the niches as actually authored) predict
//! sharper on two of four axes. The statistic still separates the two kinds
//! cleanly - that is what the gate needs - but the DIRECTION only makes
//! sense once width's contribution is admitted alongside devotion's, so the
//! gate below compares `|cv_ratio - 1|` against a floor rather than
//! asserting a signed direction: the claim this file defends is "human's
//! fit-shape dispersion is measurably NOT goblin's", not "human is flatter"
//! (see `domains/species/src/lib.rs`'s own qualifier: "width alone does not
//! separate the two kinds, and is not the claim to test" - true in
//! isolation, but width still contributes to the joint shape this file
//! measures once devotion is also in play).
//!
//! World-building idiom (decision 0092 test-fixture posture) and the
//! `niche_per_species_k` build-local-index contract are reused verbatim from
//! `windows/worldgen/tests/generalist_baseline.rs` (Task 1) - read that
//! file's header first.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{KindId, Seed};
use hornvale_species::{BiosphereTraits, ConditionNiche, biosphere_registry};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, build_world, climate_of, niche_per_species_k, sky_of, terrain_of,
};

/// The viability floor below which a cell's K is ecological noise rather
/// than presence - [`hornvale_demography::FLOOR`], unchanged; reused
/// identical to `generalist_baseline.rs`'s `VIABILITY_FLOOR`.
const VIABILITY_FLOOR: f64 = hornvale_demography::FLOOR;

const SEEDS: std::ops::RangeInclusive<u64> = 1..=30;

/// Floor on `|cv(human)/cv(goblin) - 1|` above which the two kinds' per-cell
/// fit dispersion is judged measurably different (the gate this file
/// exists to be); below it the two shapes are judged indistinguishable and
/// the vacuity check fires. Set from the measured real and mutated values
/// (`task-5-report.md`): the real, authored pair measured a ~4.6% relative
/// gap (`cv_ratio ≈ 1.046`); `0.02` sits with margin below that measured
/// gap and, per the mutation test, above what substituting goblin's niche
/// for human's collapses to.
const CV_RATIO_GAP_FLOOR: f64 = 0.02;

/// Population coefficient of variation (stddev / mean) of `vals`. Callers
/// only ever pass a per-cell fit vector pooled over cells where at least one
/// of the two compared kinds cleared [`VIABILITY_FLOOR`] (see
/// [`measure_fit_pair`]), so `vals` is never empty and its mean is never
/// exactly zero in practice; a zero mean would make this `NaN`, which the
/// caller's finite-ness is implicitly checked by the threshold comparison
/// itself (`NaN` fails every comparison, so a degenerate input fails
/// loudly rather than silently passing).
fn coefficient_of_variation(vals: &[f64]) -> f64 {
    let n = vals.len() as f64;
    let mean = vals.iter().sum::<f64>() / n;
    let variance = vals.iter().map(|v| (v - mean) * (v - mean)).sum::<f64>() / n;
    variance.sqrt() / mean
}

/// Build `seed` to full depth and return `(fits_a, fits_b)`: the per-cell K
/// (`niche_per_species_k`'s raw output) for `bio_a` and `bio_b` respectively,
/// over exactly the cells where at least one of the two clears
/// [`VIABILITY_FLOOR`] - the same "settleable by at least one roster member"
/// filter `generalist_baseline.rs` applies over the five-people roster,
/// narrowed here to the two kinds under comparison. Both vectors are the
/// same length and indexed cell-for-cell, same as `measure_one`'s
/// `per_people_fits` there.
fn measure_fit_pair(
    seed: Seed,
    bio_a: &BiosphereTraits,
    bio_b: &BiosphereTraits,
) -> (Vec<f64>, Vec<f64>) {
    let bios: [&BiosphereTraits; 2] = [bio_a, bio_b];

    let world = build_world(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap_or_else(|e| panic!("{seed:?} failed to build: {e:?}"));

    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let climate = climate_of(&world).expect("climate reconstructs");
    let sky = sky_of(&world).expect("sky reconstructs");
    let geo = terrain.geosphere();
    let system = sky
        .system()
        .unwrap_or_else(|| panic!("{seed:?} has a generated star system"));
    let insolation = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
    let obliquity = system.anchor.obliquity.get();
    let regime = match system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => {
            hornvale_climate::RotationRegime::Spinning { day_std: day.get() }
        }
        hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
    };

    let ks = niche_per_species_k(
        geo, &terrain, &climate, obliquity, insolation, &regime, &bios,
    );
    // Build-local dense index -> slot mapping (niche_per_species_k's doc
    // comment): `bios` above has bio_a at index 0, bio_b at index 1, so the
    // tag IS the position - looked up rather than assumed, matching the
    // rebuild-per-seed discipline `generalist_baseline.rs` documents.
    let k_a = &ks.iter().find(|(tag, _)| *tag == 0).unwrap().1;
    let k_b = &ks.iter().find(|(tag, _)| *tag == 1).unwrap().1;

    let mut fits_a: Vec<f64> = Vec::new();
    let mut fits_b: Vec<f64> = Vec::new();
    for cell in geo.cells() {
        let va = *k_a.get(cell);
        let vb = *k_b.get(cell);
        if va >= VIABILITY_FLOOR || vb >= VIABILITY_FLOOR {
            fits_a.push(va);
            fits_b.push(vb);
        }
    }
    (fits_a, fits_b)
}

/// `goblin`'s own authored `ConditionNiche`, read from the canonical
/// registry rather than re-declared here - the mutation must be goblin's
/// REAL niche, not a hand-copied stand-in that could quietly drift from it.
fn goblin_niche_from_registry() -> ConditionNiche {
    biosphere_registry()
        .get(&KindId("goblin"))
        .expect("goblin is in the canonical registry")
        .condition_niche
}

/// `cv(human fit) / cv(goblin fit)`, pooled over seeds 1..=30, with `human`
/// taken verbatim from the canonical registry unless `override_niche` is
/// supplied, in which case human's `condition_niche` is replaced before the
/// K build (mass, resource niche, and potency stay human's own) - the
/// mutation `substituting_goblins_niche_for_humans_is_detected` performs.
fn cv_ratio_human_vs_goblin(override_human_niche: Option<ConditionNiche>) -> f64 {
    let registry = biosphere_registry();
    let mut human = registry.get(&KindId("human")).unwrap().clone();
    let goblin = registry.get(&KindId("goblin")).unwrap().clone();
    if let Some(niche) = override_human_niche {
        human.condition_niche = niche;
    }

    let mut fits_human: Vec<f64> = Vec::new();
    let mut fits_goblin: Vec<f64> = Vec::new();
    for seed in SEEDS {
        let (h, g) = measure_fit_pair(Seed(seed), &human, &goblin);
        fits_human.extend(h);
        fits_goblin.extend(g);
    }

    let cv_human = coefficient_of_variation(&fits_human);
    let cv_goblin = coefficient_of_variation(&fits_goblin);
    let ratio = cv_human / cv_goblin;
    println!(
        "cv(human) = {cv_human:.4}, cv(goblin) = {cv_goblin:.4}, cv_ratio = {ratio:.4}, n = {} cells",
        fits_human.len()
    );
    ratio
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn human_is_not_goblin_recentred() {
    let cv_ratio = cv_ratio_human_vs_goblin(None);
    let gap = (cv_ratio - 1.0).abs();
    println!("cv_ratio (real) = {cv_ratio:.4}, |cv_ratio - 1| = {gap:.4}");
    assert!(
        gap >= CV_RATIO_GAP_FLOOR,
        "human's fit dispersion is statistically indistinguishable from goblin's \
         (cv_ratio = {cv_ratio:.4}, |cv_ratio - 1| = {gap:.4} < {CV_RATIO_GAP_FLOOR}); \
         the campaign has authored a synonym and the Gause readout is vacuous"
    );
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn substituting_goblins_niche_for_humans_is_detected() {
    let real = cv_ratio_human_vs_goblin(None);
    let mutated = cv_ratio_human_vs_goblin(Some(goblin_niche_from_registry()));
    let real_gap = (real - 1.0).abs();
    let mutated_gap = (mutated - 1.0).abs();
    println!(
        "cv_ratio real = {real:.4} (gap {real_gap:.4}), cv_ratio mutated = {mutated:.4} (gap {mutated_gap:.4})"
    );

    assert!(
        real_gap >= CV_RATIO_GAP_FLOOR,
        "real cv_ratio gap ({real_gap:.4}) is below the floor - re-check \
         human_is_not_goblin_recentred, which should already have failed"
    );
    assert!(
        mutated_gap < CV_RATIO_GAP_FLOOR,
        "the mutation was NOT detected (cv_ratio = {mutated:.4}, gap {mutated_gap:.4} \
         still >= {CV_RATIO_GAP_FLOOR}): this statistic cannot distinguish human from \
         goblin, so the vacuity check above proves nothing"
    );
}
