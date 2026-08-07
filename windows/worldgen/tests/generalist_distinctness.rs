//! The Generalist, Task 5: human must be measurably NOT goblin.
//!
//! The vacuity check spec §4 gates its readout behind
//! (`docs/superpowers/specs/2026-08-03-the-generalist-design.md`). Two
//! generalists whose per-cell fit vectors are one generalist with two names
//! would make every H1-H3 result in Task 6 an artifact.
//!
//! **Why this file does NOT use Spearman rank correlation** (spec §4's
//! original preregistered wording: "their per-cell fit vectors must not be a
//! monotone rescaling of one another"). `ConditionResponse::eval` is
//! `floor + (1 - floor) * devotion * exp(-0.5 * z^2)`
//! (`kernel/src/ecology.rs`), `z = (field - optimum) / width`: for a FIXED
//! optimum, `eval` is a strictly monotone-decreasing function of
//! `|field - optimum|` regardless of `devotion` or `width` (both only
//! rescale the same monotone shape). Rank correlation is invariant under a
//! monotone transform. Human and goblin share not one but **two** axis
//! optima by design: elevation (1500.0 m, both) and moisture (0.50, both) -
//! see `human_condition_niche()`'s doc comment,
//! `domains/species/src/lib.rs`. So on HALF the axes, human and goblin are
//! rank-identical by construction, no matter how devotion or width differ on
//! those axes - the exact case Spearman cannot see. A rank-correlation
//! ceiling anywhere near "expected for two genuine generalists" was
//! therefore not merely risky but algebraically invalid as a gate: it could
//! not fail on the shared-optimum half of the niche no matter what values
//! were authored there. This was discovered by this task, before Task 6
//! unblinded anything, and is a post-hoc amendment to spec §4 (see that
//! section for the amendment note) - not a quiet rewrite.
//!
//! **The statistic this file uses instead: each kind's coefficient of
//! variation (CV = population stddev / mean) of per-cell fit, pooled over
//! the same settleable-cell population, compared as `cv(human) / cv(goblin)`.**
//! CV is scale-free, so it isolates *relative dispersion* (shape), not
//! overall magnitude - a claim a monotone-invariant rank statistic cannot
//! make at all.
//!
//! **A prediction this file's first draft got backwards, corrected against
//! the measured numbers** (kept here rather than only in scratch, per
//! review). The initial hypothesis was that human's uniformly LOWER
//! devotion (0.20-0.30 vs goblin's 0.35-0.45 on every axis) would flatten
//! its response curves enough to produce LOWER dispersion than goblin's.
//! Measured reality, BEFORE the Task 5b re-authoring below, was the
//! opposite: `cv(human) ≈ 1.046 × cv(goblin)` (human MORE dispersed),
//! because devotion was not the only axis-by-axis difference - human was
//! also NARROWER than goblin on temperature (22.0 vs 28.0) and elevation
//! (2000.0 vs 3000.0) at the time, and a narrower Gaussian sigma steepens
//! the fall-off in `z` for a given physical distance from the optimum by
//! more than a lower devotion flattens the peak amplitude. Devotion alone
//! predicted flatter; devotion-and-width together (the niche as authored at
//! Task 2) predicted sharper on two of four axes - the opposite of the
//! design intent stated in `human_condition_niche()`'s doc comment at the
//! time.
//!
//! **A CRITICAL blind spot this statistic has, found at review, proved
//! algebraically, and left in rather than quietly patched over: elevation's
//! `devotion` contributes EXACTLY ZERO to `cv_ratio`, in isolation.**
//! `per_species_suitability` (`windows/worldgen/src/lib.rs`) evaluates elevation
//! with a HARD floor of `0.0` (sovereignty buffers physiology, never
//! geometry): `cn.elevation.eval(s.elevation, 0.0)` reduces the general
//! formula to exactly `devotion_E * bump_E(cell)`, where `bump_E` depends
//! only on `optimum`/`width`. The whole-cell fit is therefore
//! `K(cell) = devotion_E * [supply(cell) * eval_temp(cell) *
//! eval_moisture(cell) * eval_insolation(cell) * bump_E(cell)]` - `devotion_E`
//! multiplies EVERY cell's `K` by the same positive constant, holding
//! everything else fixed. `CV = stddev / mean` is scale-invariant under a
//! positive constant multiplier, so varying elevation's `devotion` alone
//! moves `cv_ratio` by exactly zero: it is algebraically, not just
//! empirically, invisible to this statistic. (The other three axes use a
//! nonzero sovereignty floor and are NOT scale-invariant this way, because a
//! nonzero additive floor breaks the pure-multiplier property - see
//! `sovereignty_floor` and each axis's `eval` call in
//! `per_species_suitability`.) This is a structural fact about the statistic,
//! unaffected by the Task 5b re-authoring below; it stays true of the
//! current niche too.
//!
//! **The width-only attribution reading.** A third measurement - human's
//! devotions and optima kept, all four widths replaced by goblin's -
//! isolates what devotion alone contributes once width's difference is
//! removed. Reported, not gated (a devotion-only-differing human is still a
//! genuinely different niche, so nothing asserts this variant's gap must
//! collapse).
//!
//! **Task 5b re-authoring (2026-08-04) changed the real-case reading.** The
//! width-attribution finding above (real gap width-dominated and pointing
//! the OPPOSITE direction from the devotion-only reading) is what the owner
//! directed be fixed: `human_condition_niche()` was re-authored so every
//! width is at least as wide as the measurement-grounded floor derived in
//! its doc comment, and verified wider than every other people's width on
//! every axis (`domains/species/src/lib.rs`). Measured after that change,
//! seeds 1..=30:
//!
//! | case | cv(variant) | cv(goblin) | cv_ratio | gap | n cells |
//! |---|---|---|---|---|---|
//! | real (human's own niche, post re-authoring) | 0.4641 | 0.4871 | 0.9528 | **0.0472** | 142,587 |
//! | mutated (human's `condition_niche` ← goblin's) | 0.4813 | 0.4871 | 0.9882 | **0.0118** | 142,590 |
//! | width-only (human devotions+optima, goblin widths) | 0.4747 | 0.4871 | 0.9747 | **0.0253** | 142,587 |
//!
//! Devotion and width now push the SAME direction: both the real case and
//! the width-only reading have `cv_ratio < 1` (human less dispersed than
//! goblin), where before re-authoring they disagreed in sign
//! (`cv_ratio` 1.0462 real vs. 0.9766 width-only). The real gap also grew
//! (`0.0462` → `0.0472`), consistent with the two effects reinforcing
//! rather than fighting. `CV_RATIO_GAP_FLOOR` (`0.02`) is still cleared with
//! comfortable margin by both the real case (136% above floor) and the
//! width-only reading (27% above floor); the mutated gap is unchanged
//! (goblin's own niche substituted wholesale, independent of human's
//! widths) and still stays below the floor, so the mutation is still
//! detected. See `substituting_goblins_niche_for_humans_is_detected`'s body
//! for the full reading and `human_condition_niche()`'s doc comment
//! (`domains/species/src/lib.rs`) for the width-authoring rule that produced
//! this.
//!
//! World-building idiom (decision 0092 test-fixture posture) and the
//! `per_species_suitability` build-local-index contract are reused verbatim from
//! `windows/worldgen/tests/generalist_baseline.rs` (Task 1) - read that
//! file's header first.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{ConditionResponse, KindId, Seed};
use hornvale_species::{BiosphereTraits, ConditionNiche, HabitatRealm, biosphere_registry};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, build_world, climate_of, per_species_suitability, sky_of, terrain_of,
};

/// The viability floor below which a cell's K is ecological noise rather
/// than presence - [`hornvale_demography::FLOOR`], unchanged; reused
/// identical to `generalist_baseline.rs`'s `VIABILITY_FLOOR`.
const VIABILITY_FLOOR: f64 = hornvale_demography::FLOOR;

const SEEDS: std::ops::RangeInclusive<u64> = 1..=30;

/// Minimum pooled settleable-cell count [`cv_ratio_human_vs_goblin`] will
/// accept before asserting on its output. `generalist_baseline.rs` guards
/// its own measurement against an EMPTY sample; this guards against a
/// small-but-nonzero one, which is the more dangerous failure here - `NaN`
/// (an exactly-empty or exactly-zero-mean sample) already fails every
/// `>=`/`<` comparison below loudly, but a shrunk-yet-nonempty population
/// (e.g. a future `FLOOR` or land-mask change cutting the cell set to a few
/// hundred) would not: `cv_ratio` on a small sample drifts from `1.0` on
/// noise alone, `gap >= CV_RATIO_GAP_FLOOR` could pass for the wrong reason,
/// and the vacuity check would green-light Task 6 on a population too small
/// to trust. `142_587`/`142_590` were the two measured populations (real and
/// mutated respectively, seeds 1..=30, mesh level 6); `100_000` sits with
/// wide margin below both while still being far above "a few hundred".
const MIN_SETTLEABLE_CELLS: usize = 100_000;

/// Floor on `|cv(human)/cv(goblin) - 1|` above which the two kinds' per-cell
/// fit dispersion is judged measurably different (the gate this file
/// exists to be); below it the two shapes are judged indistinguishable and
/// the vacuity check fires.
///
/// **Measured values this constant was set from** (mesh level 6, the
/// default `Geosphere` subdivision - 40,962 cells/world, ~110 km
/// resolution; seeds 1..=30; cell filter: pooled over cells where EITHER
/// compared kind's `per_species_suitability` output clears
/// [`VIABILITY_FLOOR`]). Superseded by the Task 5b re-authoring
/// (2026-08-04, `domains/species/src/lib.rs`'s `human_condition_niche()`) -
/// current numbers:
///
/// | case | cv(human variant) | cv(goblin) | cv_ratio | \|ratio − 1\| | n cells |
/// |---|---|---|---|---|---|
/// | real (human's own niche) | 0.4641 | 0.4871 | 0.9528 | **0.0472** | 142,587 |
/// | mutated (human's `condition_niche` ← goblin's) | 0.4813 | 0.4871 | 0.9882 | **0.0118** | 142,590 |
///
/// `0.02` sits strictly between the two measured gaps, with margin on both
/// sides (real is 136% above the floor; mutated is 41% below it) - see
/// `substituting_goblins_niche_for_humans_is_detected` for the mutation
/// proof this margin is drawn from, and this file's module doc comment for
/// why elevation's `devotion` specifically cannot be part of what widened
/// the real gap, and for the width-only reading's own numbers.
const CV_RATIO_GAP_FLOOR: f64 = 0.02;

/// Population coefficient of variation (stddev / mean) of `vals`.
///
/// A zero-mean `vals` (which [`cv_ratio_human_vs_goblin`]'s
/// [`MIN_SETTLEABLE_CELLS`] guard makes unreachable in practice, since every
/// pooled cell has at least one kind's `K` above the tiny
/// [`VIABILITY_FLOOR`]) would make this `NaN`; every comparison this file
/// makes against the result (`>=`, `<`) is false for `NaN`, so a degenerate
/// input fails the assertion loudly rather than silently passing it.
fn coefficient_of_variation(vals: &[f64]) -> f64 {
    let n = vals.len() as f64;
    let mean = vals.iter().sum::<f64>() / n;
    let variance = vals.iter().map(|v| (v - mean) * (v - mean)).sum::<f64>() / n;
    variance.sqrt() / mean
}

/// Build `seed` to full depth and return `(fits_a, fits_b)`: the per-cell K
/// (`per_species_suitability`'s raw output) for `bio_a` and `bio_b` respectively,
/// over exactly the cells where at least one of the two clears
/// [`VIABILITY_FLOOR`] - the same "settleable by at least one roster member"
/// filter `generalist_baseline.rs` applies over the five-people roster,
/// narrowed here to the two kinds under comparison. Both vectors are the
/// same length and indexed cell-for-cell, same as `measure_one`'s
/// `per_people_fits` there.
/// Every variant's `(human_fits, goblin_fits)` pair for one seed, from **one**
/// world build and **one** `per_species_suitability` call.
///
/// Scoring every variant alongside goblin in a single call is exact, not an
/// approximation: that function hoists its supply fields out of the
/// per-species loop, so a kind's K does not depend on which other kinds it is
/// scored beside. Verified by this battery's own numbers, which did not move
/// when it changed shape.
fn measure_fits(
    seed: Seed,
    humans: &[BiosphereTraits],
    goblin: &BiosphereTraits,
) -> Vec<(Vec<f64>, Vec<f64>)> {
    let mut bios: Vec<&BiosphereTraits> = humans.iter().collect();
    bios.push(goblin);

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

    // Every kind here is peopled and surface-scored — absent from the sparse
    // habitat-realm store, so all default to `Surface`.
    let realm = vec![HabitatRealm::SURFACE; bios.len()];
    let ks = per_species_suitability(
        geo, &terrain, &climate, obliquity, insolation, &regime, &bios, &realm,
    );
    // Build-local dense index -> slot mapping (per_species_suitability's doc
    // comment): `bios` above holds the human variants in order and goblin
    // last, so the tag IS the position - looked up rather than assumed,
    // matching the rebuild-per-seed discipline `generalist_baseline.rs`
    // documents.
    let at = |tag: u32| &ks.iter().find(|(t, _)| *t == tag).unwrap().1;
    let k_goblin = at(humans.len() as u32);

    // Each variant keeps its OWN viability filter against goblin: the
    // surviving cell set differs per variant, which is why goblin's fits are
    // re-collected per variant rather than shared. Pooling them would quietly
    // change every reported cv(goblin).
    humans
        .iter()
        .enumerate()
        .map(|(i, _)| {
            let k_h = at(i as u32);
            let mut fits_h: Vec<f64> = Vec::new();
            let mut fits_g: Vec<f64> = Vec::new();
            for cell in geo.cells() {
                let vh = *k_h.get(cell);
                let vg = *k_goblin.get(cell);
                if vh >= VIABILITY_FLOOR || vg >= VIABILITY_FLOOR {
                    fits_h.push(vh);
                    fits_g.push(vg);
                }
            }
            (fits_h, fits_g)
        })
        .collect()
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

/// Human's niche with ONLY the four `width` fields replaced by goblin's -
/// `optimum` and `devotion` stay human's own on every axis. The attribution
/// variant that isolates whether the real gap is carried by width rather
/// than by devotion (see the module doc comment's CRITICAL note on
/// elevation's `devotion` being algebraically invisible to `cv_ratio`).
fn human_niche_with_goblins_widths() -> ConditionNiche {
    let registry = biosphere_registry();
    let human = registry
        .get(&KindId("human"))
        .expect("human is in the canonical registry")
        .condition_niche;
    let goblin = goblin_niche_from_registry();
    ConditionNiche {
        temperature: ConditionResponse {
            width: goblin.temperature.width,
            ..human.temperature
        },
        moisture: ConditionResponse {
            width: goblin.moisture.width,
            ..human.moisture
        },
        insolation: ConditionResponse {
            width: goblin.insolation.width,
            ..human.insolation
        },
        elevation: ConditionResponse {
            width: goblin.elevation.width,
            ..human.elevation
        },
    }
}

/// `cv(human fit) / cv(goblin fit)`, pooled over seeds 1..=30, with `human`
/// taken verbatim from the canonical registry unless `override_human_niche`
/// is supplied, in which case human's `condition_niche` is replaced before
/// the K build (mass, resource niche, and potency stay human's own).
/// Asserts [`MIN_SETTLEABLE_CELLS`] before returning - see that constant's
/// doc for why a shrunk-but-nonempty population is dangerous here.
/// Every variant's `cv_ratio` against goblin, over **one** pass of the seed
/// sweep. `None` means human's own authored niche; `Some(n)` substitutes `n`.
///
/// **Why this takes a slice rather than being called once per variant.** The
/// world build does not depend on the niche at all — the niche enters at
/// `per_species_suitability`, long after genesis — so calling a one-variant
/// helper N times rebuilt the same thirty worlds N times.
/// `substituting_goblins_niche_for_humans_is_detected` asks three questions,
/// and so paid for **ninety world builds to look at thirty worlds**. One
/// sweep answers all of them, and the reported numbers are unchanged because
/// nothing about the measurement moved — only how many times the same worlds
/// were generated.
fn cv_ratios(variants: &[Option<ConditionNiche>]) -> Vec<f64> {
    let registry = biosphere_registry();
    let base_human = registry
        .get(&KindId("human"))
        .expect("human is in the canonical registry")
        .clone();
    let goblin = registry
        .get(&KindId("goblin"))
        .expect("goblin is in the canonical registry")
        .clone();

    let humans: Vec<BiosphereTraits> = variants
        .iter()
        .map(|override_niche| {
            let mut h = base_human.clone();
            if let Some(niche) = override_niche {
                h.condition_niche = *niche;
            }
            h
        })
        .collect();

    let mut fits_human: Vec<Vec<f64>> = vec![Vec::new(); variants.len()];
    let mut fits_goblin: Vec<Vec<f64>> = vec![Vec::new(); variants.len()];
    for seed in SEEDS {
        for (i, (h, g)) in measure_fits(Seed(seed), &humans, &goblin)
            .into_iter()
            .enumerate()
        {
            fits_human[i].extend(h);
            fits_goblin[i].extend(g);
        }
    }

    (0..variants.len())
        .map(|i| {
            assert!(
                fits_human[i].len() >= MIN_SETTLEABLE_CELLS,
                "only {} settleable cells pooled over {} seeds (floor {MIN_SETTLEABLE_CELLS}); \
                 cv_ratio on a population this small cannot be trusted - see MIN_SETTLEABLE_CELLS's doc",
                fits_human[i].len(),
                SEEDS.count()
            );
            let cv_human = coefficient_of_variation(&fits_human[i]);
            let cv_goblin = coefficient_of_variation(&fits_goblin[i]);
            let ratio = cv_human / cv_goblin;
            println!(
                "cv(human) = {cv_human:.4}, cv(goblin) = {cv_goblin:.4}, cv_ratio = {ratio:.4}, n = {} cells",
                fits_human[i].len()
            );
            ratio
        })
        .collect()
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn human_is_not_goblin_recentred() {
    let cv_ratio = cv_ratios(&[None])[0];
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
    // All three variants in ONE seed sweep — see `cv_ratios`. The width-only
    // reading below comes out of the same pass rather than a third rebuild.
    let ratios = cv_ratios(&[
        None,
        Some(goblin_niche_from_registry()),
        Some(human_niche_with_goblins_widths()),
    ]);
    let (real, mutated, width_only) = (ratios[0], ratios[1], ratios[2]);
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
    // The mutated gap is not exactly zero even though human's ConditionNiche
    // is now bit-for-bit goblin's: mass still differs (human 70 kg vs
    // goblin 18.1 kg), which feeds hornvale_kernel::sovereignty_floor into a
    // different `floor` (~0.447 vs ~0.335) on the three non-elevation axes,
    // and human's resource-niche ResourceVector (PLANT_FORAGE 0.55 /
    // ANIMAL_PREY 0.45) differs slightly from goblin's (0.50/0.50), changing
    // `axis_supply`'s per-cell weighting a little. Neither difference
    // touches the elevation axis's hard floor(0.0) term. If a later
    // campaign changes human's mass or resource niche, this residual will
    // move and this assertion's message ("this statistic cannot distinguish
    // human from goblin") would be a MISDIAGNOSIS of that instead - the
    // actual cause would be traits this test intentionally holds fixed
    // (only `condition_niche` is mutated), not a broken statistic.
    assert!(
        mutated_gap < CV_RATIO_GAP_FLOOR,
        "the mutation was NOT detected (cv_ratio = {mutated:.4}, gap {mutated_gap:.4} \
         still >= {CV_RATIO_GAP_FLOOR}): this statistic cannot distinguish human from \
         goblin, so the vacuity check above proves nothing (but check human's mass and \
         resource-niche vector first - see the comment above this assertion)"
    );

    // The width-only attribution reading: human's devotions and optima
    // kept, all four widths replaced by goblin's. NOT asserted to collapse -
    // a devotion-only-differing human is still a genuinely different niche
    // from goblin's, so there is no "correct" direction to gate here.
    // Reported so the campaign knows whether the real gap is carried by
    // devotion (the documented, intended contrast) or by width.
    let width_only_gap = (width_only - 1.0).abs();
    println!(
        "cv_ratio width-only (human devotions+optima, goblin widths) = {width_only:.4} (gap {width_only_gap:.4})"
    );
    // Reading, measured post Task 5b re-authoring (2026-08-04,
    // `domains/species/src/lib.rs`'s `human_condition_niche()`): width_only
    // = 0.9747, gap 0.0253 - ABOVE CV_RATIO_GAP_FLOOR (27% margin), so
    // devotion ALONE (widths matched to goblin's, removing width's
    // contribution entirely) still produces a detectable, gate-clearing
    // signal, in the direction devotion's own mechanism predicts (lower
    // devotion -> lower relative dispersion: 0.9747 < 1).
    //
    // BEFORE re-authoring this pointed the OPPOSITE direction from the real
    // case (real cv_ratio 1.0462 > 1; width-only 0.9766 < 1) - width's
    // contribution was larger than devotion's and the two fought, with
    // width winning the sign. That was the finding that triggered the
    // re-authoring: `human_condition_niche()`'s widths were unargued and
    // came out narrower than goblin's on two axes, contradicting the
    // niche's own stated design intent. After re-authoring every width from
    // a measurement-grounded floor (see that function's doc comment) and
    // verifying human is the widest curve of the six peoples on all four
    // axes, the REAL case now ALSO reads cv_ratio 0.9528 < 1 - the SAME direction as
    // the width-only reading, and the real gap grew (0.0462 -> 0.0472)
    // rather than shrank. Devotion and width now reinforce each other
    // instead of opposing: "human is a low-devotion, wide-tolerance
    // generalist" is both the authored intent and what the gate's
    // real-case number measures.
}
