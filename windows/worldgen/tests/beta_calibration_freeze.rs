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
//! motivated band `[1.5, 3.0]`: comfortably above winner-take-all
//! monoculture (`strife` → 1) and comfortably below undifferentiated
//! "oatmeal" sharing (`strife` → 4, the species count).
//!
//! **Niche-era re-baseline (task E1b)**: the band above was originally
//! `[2.0, 3.0]`, preregistered against the pre-Niche **flat** K model, where
//! the task-A16b sweep measured a mean of ≈2.4 at β=2.0. The Niche campaign
//! replaced that flat K with niche-differentiated K — each species' carrying
//! capacity now peaks where its traits best fit the world's local
//! conditions, rather than being flat across the map. That is a deliberate
//! model change, not a regression: a world where each cell has a clearer
//! locally-best-suited species *is* the campaign's biogeography, so
//! per-cell diversity legitimately drops as species sort into the terrain
//! that favors them instead of sharing every cell evenly. Re-running this
//! test's five seeds at the frozen β=2.0 under niche-differentiated K
//! measured:
//!
//! | seed | claimed diversity |
//! |------|--------------------|
//! | 1    | 1.9112534308796194 |
//! | 2    | 2.090962659447805  |
//! | 3    | 1.881701865784351  |
//! | 4    | 2.0204691074243333 |
//! | 42   | 2.0017951778530727 |
//!
//! mean ≈ 1.9812364482778360, with the lowest individual seed (seed 3) at
//! ≈1.8817 — comfortably clear of monoculture (1) but below the old flat-
//! model floor of 2.0. The band is re-baselined to `[1.5, 3.0]`: `1.5` sits
//! with margin under every measured seed (the closest, seed 3, is ≈0.38
//! above it) while staying well clear of monoculture, so the band stays
//! physically defensible under further seed variation rather than being
//! fitted to the measured mean by epsilon. The upper bound `3.0` is
//! unchanged (still well below oatmeal=4) — the niche model does not push
//! diversity upward, so it needed no re-justification. The frozen `BETA=2.0`
//! itself is **unchanged** by this re-baseline; only the band that
//! interprets its measured effect was re-measured, exactly as the
//! Weak-knob/Stage-B caveat below anticipated.
//!
//! **Weak-knob / Stage-B caveat** (carried from `coexist::BETA`'s doc and the
//! A16b sweep's module doc): against the shipped roster's near-tied
//! carrying capacities, β only moves claimed-cell diversity across a narrow
//! band over the swept range β∈[0.1, 6.0] — this test's re-baselined
//! [1.5, 3.0] band is wide enough to hold across that sweep's shape, so
//! passing here confirms the frozen value is *in a physically sane regime*,
//! not that β was surgically tuned. The knob will bite harder once the
//! Stage-B menagerie (Stage F) adds species with disparate K (spec §3);
//! that stage RE-MEASURES again against the richer, genuinely
//! differentiated roster rather than re-tuning this constant.
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
/// effective diversity across a handful of seeds lands in `[1.5, 3.0]` — see
/// the module doc for the niche-era re-baseline and the weak-knob caveat.
#[test]
fn beta_yields_realistic_coexistence() {
    // This freeze is preregistered against "the shipped 4-goblinoid roster"
    // (module doc, top). Task 4 (the canonical-5E menagerie) widened
    // `default_roster()` with 12 biosphere-only fauna, but those are not
    // yet folded into the coexistence packer's competition — that cutover
    // is Task 5's (niche-K), which re-measures this band against the
    // richer roster per the module doc's own Stage-B caveat. Scope this
    // read to the peopled species so it keeps measuring what it always
    // measured until that re-measurement lands.
    let roster: Vec<hornvale_species::SpeciesDef> = default_roster()
        .into_iter()
        .filter(|d| d.peopled.is_some())
        .collect();

    let per_seed: Vec<(u64, f64)> = SEEDS
        .iter()
        .map(|&seed| (seed, claimed_diversity(seed, &roster)))
        .collect();

    let mean: f64 = per_seed.iter().map(|(_, d)| *d).sum::<f64>() / per_seed.len() as f64;

    assert!(
        (1.5..=3.0).contains(&mean),
        "mean per-claimed-cell diversity at beta={} across seeds {per_seed:?} = {mean}, \
         expected in the niche-era re-baselined band [1.5, 3.0] (monoculture ~1, oatmeal ~4)",
        hornvale_demography::BETA
    );
}
