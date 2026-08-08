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
//! **Roster-era re-baseline (The Delvers, 2026-08-07)**: the ceiling is no
//! longer a literal. The band above was preregistered against a **four**-people
//! roster, and its upper bound says so in its own words — "comfortably below
//! undifferentiated *oatmeal* sharing (`strife` → 4, **the species count**)".
//! `3.0` was therefore never an absolute quantity; it was **75% of oatmeal**,
//! and oatmeal is the size of the peopled roster. That dependency was invisible
//! because it was compiled into a number.
//!
//! The Delvers shipped **three** dwarves, taking the peopled set — `psyche`,
//! which counts the three dragons alongside the settling peoples — from **9 to
//! 12**. Measured at the frozen β=2.0 on the roster that actually shipped:
//!
//! | seed | claimed diversity |
//! |------|--------------------|
//! | 1    | 3.082600209114541  |
//! | 2    | 3.3263969671285327 |
//! | 3    | 2.88374745112571   |
//! | 4    | 2.918818431327006  |
//! | 42   | 2.8391207889036836 |
//!
//! mean ≈ **3.0101**, against a derived ceiling of `0.75 × 12 = 9.0`.
//!
//! Two things are worth reading off that number rather than one. First, the
//! bound's own quantity: 3.01 of a possible 12 is **25% of oatmeal**, where the
//! original band permitted 75%. Absolute diversity rose, because there are more
//! peoples available to share a cell; diversity *relative to undifferentiated
//! sharing* fell to a third of what the band allows. The new kinds are
//! partitioning space rather than piling onto it, which is precisely what this
//! bound exists to check.
//!
//! Second, and this is the sharper half: **the stale literal would have failed
//! by 0.0101.** A mean of 3.0101 breaches a ceiling of 3.0 — barely, and for a
//! reason that has nothing to do with the world going undifferentiated. That is
//! what a compiled-in dependency looks like when it finally rots: not a dramatic
//! failure that announces its cause, but a hair over the line, exactly the shape
//! most likely to be waved through as noise and re-pinned.
//!
//! **An earlier revision of this doc recorded a five-dwarf roster** (peopled set
//! "six to eleven", mean 3.4238). Those two kinds were cut before merge, so that
//! evidence describes a roster that never shipped; it is replaced rather than
//! kept, because a calibration table is only worth what its population is. It
//! also conflated the settling count with `psyche.len()`, which is the count the
//! assertion below actually derives its ceiling from.
//!
//! So the ceiling is now **derived** — `0.75 × peopled_count` — which preserves
//! the original preregistration exactly at a roster of four and cannot rot
//! again. The floor stays the absolute `1.5`: monoculture drives `strife` to 1
//! whatever the roster size, so that half never scaled. **`BETA = 2.0` is
//! unchanged**; as in the niche-era re-baseline above, only the band that
//! interprets its effect moved.
//!
//! Stated plainly because it is a post-unblinding change to a preregistered
//! bound: this was authorized deliberately (Nathan, 2026-08-07) rather than
//! adjusted to make a suite green, it re-derives the bound's *rule* instead of
//! fitting its *value* to the measurement, and it is recorded in the campaign's
//! chronicle. The honest cost: a ceiling that scales with the roster is a
//! weaker discriminator on a large roster than a literal was on a small one.
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
//! `demography_report_from` needs — settlement/culture/religion facts are
//! irrelevant to demography), a single β (the frozen constant, read via
//! `hornvale_demography::report`'s default path). NOT `#[ignore]`d — this is
//! the preregistered freeze check, meant to run in the commit gate.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, climate_from,
    demography_report_from, terrain_of,
};

/// A handful of seeds (not a census — `HV_CENSUS`/`make rebaseline` stay
/// untouched), overlapping the task-A16b sweep's seed set so this test's
/// single-β read is directly comparable to that sweep's row for β=2.0.
const SEEDS: [u64; 5] = [1, 2, 3, 4, 42];

/// Mean `byproducts.strife` over habitable cells CLAIMED (Σ species density
/// > 0) by at least one species, for one seed's world at the frozen β.
fn claimed_diversity(seed: u64, wc: &WorldComponents) -> f64 {
    let world = build_world_to(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        BuildDepth::Terrain,
    )
    .expect("seed builds at BuildDepth::Terrain");

    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let climate = climate_from(&world, &terrain).expect("climate reconstructs");
    let report = demography_report_from(&world, wc, &terrain, &climate)
        .expect("demography report reconstructs");

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

/// The 4-goblinoid peopled component set: the canonical registries scoped to
/// the peopled kinds (the `psyche` key-set — fauna are biosphere-only, so they
/// carry no psyche row). Byte-identical to the four-goblinoid component set the
/// freeze was originally preregistered against.
fn peopled_components() -> WorldComponents {
    use hornvale_kernel::{ComponentStore, KindId};
    let psyche = hornvale_species::psyche_registry();
    let peopled: std::collections::BTreeSet<KindId> = psyche.ids().copied().collect();
    let biosphere: ComponentStore<KindId, hornvale_species::BiosphereTraits> =
        hornvale_species::biosphere_registry()
            .iter()
            .filter(|(k, _)| peopled.contains(k))
            .map(|(k, v)| (*k, v.clone()))
            .collect();
    let family_of: ComponentStore<KindId, &'static str> = hornvale_species::family_of()
        .iter()
        .filter(|(k, _)| peopled.contains(k))
        .map(|(k, v)| (*k, *v))
        .collect();
    WorldComponents::from_stores(
        biosphere,
        psyche,
        hornvale_species::society_registry(),
        hornvale_species::perception_registry(),
        hornvale_language::articulation_registry(),
        hornvale_language::lexicon_registry(),
        hornvale_language::family_proto(),
        family_of,
        ComponentStore::new(),
        ComponentStore::new(),
        ComponentStore::new(),
        ComponentStore::new(),
    )
    .expect("the peopled-only component set is well-formed")
}

/// The preregistered freeze check: at the frozen β, the mean per-claimed-cell
/// effective diversity across a handful of seeds lands in `[1.5, 3.0]` — see
/// the module doc for the niche-era re-baseline and the weak-knob caveat.
#[test]
fn beta_yields_realistic_coexistence() {
    // This freeze is preregistered against "the shipped 4-goblinoid roster"
    // (module doc, top). Task 4 (the canonical-5E menagerie) widened
    // the roster with 12 biosphere-only fauna, but those are not
    // yet folded into the coexistence packer's competition — that cutover
    // is Task 5's (niche-K), which re-measures this band against the
    // richer roster per the module doc's own Stage-B caveat. Scope this
    // read to the peopled species so it keeps measuring what it always
    // measured until that re-measurement lands.
    let wc = peopled_components();
    // Both halves of the band, named so the assertion states its own
    // direction rather than presenting two magic numbers.
    /// Absolute lower bound: winner-take-all monoculture drives `strife` to
    /// 1 regardless of how many peoples exist, so this does NOT scale.
    const MONOCULTURE_FLOOR: f64 = 1.5;
    /// Upper bound as a fraction of "oatmeal" — undifferentiated sharing,
    /// where `strife` approaches the peopled-species count. `0.75` preserves
    /// the original preregistration exactly: `3.0` against a 4-people roster.
    const OATMEAL_FRACTION: f64 = 0.75;

    let per_seed: Vec<(u64, f64)> = SEEDS
        .iter()
        .map(|&seed| (seed, claimed_diversity(seed, &wc)))
        .collect();

    let mean: f64 = per_seed.iter().map(|(_, d)| *d).sum::<f64>() / per_seed.len() as f64;

    // THE DELVERS: the ceiling is DERIVED from the live peopled count, not
    // written as a literal. See the module doc's roster-era re-baseline for
    // why — a literal `3.0` silently encoded "the roster has four peoples",
    // and rotted the moment one didn't.
    let oatmeal = wc.psyche.len() as f64;
    let ceiling = OATMEAL_FRACTION * oatmeal;

    assert!(
        (MONOCULTURE_FLOOR..=ceiling).contains(&mean),
        "mean per-claimed-cell diversity at beta={} across seeds {per_seed:?} = {mean}, \
         expected in [{MONOCULTURE_FLOOR}, {ceiling}] — the floor is absolute (monoculture \
         is 1 whatever the roster size) and the ceiling is {OATMEAL_FRACTION} x oatmeal, \
         where oatmeal = {oatmeal} peopled species. If this fails ABOVE the ceiling the \
         world has gone undifferentiated; BELOW the floor it has gone monocultural. Do not \
         replace the derived ceiling with a literal.",
        hornvale_demography::BETA
    );
}
