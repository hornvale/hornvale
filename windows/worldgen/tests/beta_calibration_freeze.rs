//! Preregistered calibration test for the FROZEN competition-temperature
//! constant `hornvale_demography::BETA` (task A16c; the controller's chosen
//! value, 2.0, adjudicated from the task-A16b sweep in
//! `windows/worldgen/tests/beta_calibration_sweep.rs`).
//!
//! **Preregistered target**: "realistic coexistence" for the shipped
//! 4-goblinoid roster means a clear local dominant with graded rivals, not a
//! monoculture and not undifferentiated sharing. Operationally: the mean
//! per-CLAIMED-cell effective diversity — `byproducts.strife` (the inverse-
//! Herfindahl evenness of a cell's per-species density shares) averaged over
//! habitable cells where Σ species density > 0 — lands in the physically
//! motivated band `[2.0, 3.0]`: comfortably above winner-take-all
//! monoculture (`strife` → 1) and comfortably below undifferentiated
//! "oatmeal" sharing (`strife` → 4, the species count). This band, and the
//! choice to measure at the frozen β rather than search for one, were fixed
//! before this test's assertion was written, matching the task-A16b sweep's
//! own measured mean of ≈2.4 at β=2.0.
//!
//! **Weak-knob / Stage-B caveat** (carried from `coexist::BETA`'s doc and the
//! A16b sweep's module doc): against the shipped roster's near-tied
//! carrying capacities, β only moves claimed-cell diversity across a narrow
//! 2.14–2.53 band over the ENTIRE swept range β∈[0.1, 6.0] — this test's
//! [2.0, 3.0] band is wide enough to hold for that whole sweep, so passing
//! here confirms the frozen value is *in a physically sane regime*, not that
//! β was surgically tuned. The knob will bite harder once the Stage-B
//! menagerie adds species with disparate K (spec §3); Stage B RE-MEASURES
//! against that richer roster rather than re-tuning this constant.
//!
//! Deliberately light for the commit gate: ~5 seeds (not the sweep's 13),
//! each world built ONCE to [`BuildDepth::Terrain`] (the shallowest rung
//! `demography_report` needs — settlement/culture/religion facts are
//! irrelevant to demography), a single β (the frozen constant, read via
//! `hornvale_demography::report`'s default path). NOT `#[ignore]`d — this is
//! the preregistered freeze check, meant to run in the commit gate.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, build_world_to, default_roster, demography_report,
};

/// A handful of seeds (not a census — `HV_CENSUS`/`make rebaseline` stay
/// untouched), overlapping the task-A16b sweep's seed set so this test's
/// single-β read is directly comparable to that sweep's row for β=2.0.
const SEEDS: [u64; 5] = [1, 2, 3, 4, 42];

/// Mean `byproducts.strife` over habitable cells CLAIMED (Σ species density
/// > 0) by at least one species, for one seed's world at the frozen β.
fn claimed_diversity(seed: u64, roster: &[hornvale_species::SpeciesDef]) -> f64 {
    let world = build_world_to(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        roster,
        BuildDepth::Terrain,
    )
    .expect("seed builds at BuildDepth::Terrain");

    let report = demography_report(&world, roster).expect("demography report reconstructs");

    let mut sum = 0.0_f64;
    let mut n = 0u32;
    for (cell, strife) in report.byproducts.strife.iter() {
        let total_density: f64 = report.stack.density.iter().map(|(_, d)| *d.get(cell)).sum();
        if total_density > 0.0 {
            sum += *strife;
            n += 1;
        }
    }
    assert!(n > 0, "seed {seed} claims at least one cell");
    sum / f64::from(n)
}

/// The preregistered freeze check: at the frozen β, the mean per-claimed-cell
/// effective diversity across a handful of seeds lands in `[2.0, 3.0]` — see
/// the module doc for the preregistered target and the weak-knob caveat.
#[test]
fn beta_yields_realistic_coexistence() {
    let roster = default_roster();

    let per_seed: Vec<(u64, f64)> = SEEDS
        .iter()
        .map(|&seed| (seed, claimed_diversity(seed, &roster)))
        .collect();

    let mean: f64 = per_seed.iter().map(|(_, d)| *d).sum::<f64>() / per_seed.len() as f64;

    assert!(
        (2.0..=3.0).contains(&mean),
        "mean per-claimed-cell diversity at beta={} across seeds {per_seed:?} = {mean}, \
         expected in the preregistered band [2.0, 3.0] (monoculture ~1, oatmeal ~4)",
        hornvale_demography::BETA
    );
}
