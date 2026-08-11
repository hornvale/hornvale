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
//! **The Muster (2026-08-11): two stores this guard reads were empty, so the
//! affinity rows could not reach the number it asserts on.**
//! `peopled_components` below built its component set with five stores left as
//! `ComponentStore::new()`. Two of those five are read by
//! `demography_report_with_beta_from` on the exact path to `byproducts.strife`
//! — `habitat_realm` and `biome_affinity`
//! (`windows/worldgen/src/lib.rs:1734-1753`) — and both are SPARSE stores read
//! through a default on absence (`HabitatRealm::SURFACE`; `None` = unrestricted
//! at every biome). An empty store therefore raised nothing: it silently
//! supplied the null hypothesis, and every authored affinity row scored
//! identically here. Measured at the frozen β over this test's own five seeds,
//! with the two stores decomposed because they went live in the same change:
//!
//! | component set | realm rows | affinity rows | mean claimed diversity | Δ vs blind |
//! |---|---|---|---|---|
//! | pre-Muster (both stores empty) | 0 | 0 | 2.3734235460211663 | — |
//! | realm only | 1 | 0 | 2.3678005458279160 | −0.0056 (0.24%) |
//! | **affinity only** | 0 | 7 | 2.2024420744011466 | **−0.1710 (7.2%)** |
//! | repaired, as shipped | 1 | 7 | 2.1127601185602627 | −0.2607 (11.0%) |
//! | full canonical (`WorldComponents::assemble()`) | 3 | 8 | 1.5063123260334543 | — |
//!
//! **Attribute that carefully.** The combined 11% is *not* the biome-affinity
//! effect. The two stores are not additive — −0.0056 + −0.1710 = −0.1766
//! against a combined −0.2607, so ≈0.084, a third of the total, is interaction
//! between the single realm row and the affinity rows. Affinity's own
//! contribution is **−0.171, 7.2%**; size anything against that figure, not
//! against 11%. The other three empty stores (`deity`, `culture`, `material`)
//! stay empty deliberately and the code says why; no build or demography path
//! reads them off `wc`.
//!
//! **What this does and does not establish.** It establishes that an affinity
//! change now reaches this assertion's number, which it demonstrably could not
//! before. It does *not* establish that the guard can go red on any affinity
//! change: a uniform rescale of the affinity **level**, holding its per-biome
//! shape fixed, moves the mean the wrong way (up, to ≈2.495) and still passes,
//! while a change to the per-biome **shape** — each kind given a distinct
//! stronghold biome — drives it to ≈1.334 and fails beneath the floor. So what
//! is demonstrated is sensitivity to affinity *structure*. Whether the level
//! alone can be made to redden this band is settled by the positive control in
//! the next section, and the answer is *not at today's reach*. (A trap that
//! section had to steer around: an over-strong affinity crush makes the world
//! claim no cells at all and trips `claimed_diversity`'s `assert!(n > 0)`
//! before the band is ever evaluated — a red for the wrong reason.)
//!
//! **The Muster's positive control (2026-08-11): what reddens this guard, and
//! what does not.** A repaired guard that stays green proves nothing, so the
//! campaign owed a mutation that makes this assertion fail. What it got is a
//! *qualified* confirmation, and the qualification is the finding.
//!
//! Every arm below is test-fixture-only — nothing under `domains/species/` is
//! edited. A row is mutated by inverting it to its authored preference vector
//! and rebuilding it at a new level, so the authored **shape** is carried
//! through unchanged:
//!
//! ```text
//!   pref = (factor - level) / (1 - level)             // invert
//!   row  = BiomeAffinity::from_preferences(l, pref)   // rebuild at level l
//!   l(d) = level - (d - 1) * (1 - level)              // the depth knob
//! ```
//!
//! `d` is the PENALTY DEPTH: `d = 1` is the shipped level exactly, `d = 0` sets
//! every factor to `1.0` (affinity off), and `d > 1` drives the level below its
//! authored value toward the hard-exclusion `0`. Write `l(d)` in the form
//! above and not as the algebraically identical `1 - d * (1 - level)`, which is
//! one ULP off at `d = 1` and costs the bit-exact reproduction of the shipped
//! rows that makes the whole sweep interpretable.
//!
//! **The level alone, at today's reach, cannot redden this guard.** Its WHOLE
//! attainable range over the seven authored rows, five seeds, frozen β, each
//! arm building its own worlds exactly as the test below does:
//!
//! | arm (the 7 authored rows) | mean claimed diversity | verdict |
//! |---|---|---|
//! | `d = 0.00` — level 1, affinity off | 2.3678005458279161 | PASS |
//! | `d = 1.00` — SHIPPED | 2.1127601185602627 | PASS |
//! | `d = 1.20` | 2.0817935086273343 | PASS |
//! | `d = 1.40` — the level's MINIMUM | 2.0691689632978352 | PASS |
//! | level `0` — hard exclusion off-shape | 2.9734723549133930 | PASS |
//!
//! The floor sits 0.569 below that minimum and the ceiling 10.5 above that
//! maximum. Two mechanisms hold it there, and they are different in kind:
//!
//! 1. **The ceiling is unreachable in principle, by any mutation of this
//!    store.** `strife` cannot exceed the number of species PRESENT in a cell,
//!    and the mean claimed cell holds 6.44 of the 18 (pooled over the five
//!    seeds' 177 336 claimed cells). A mean of 13.5 asks for more coexistence
//!    than the world puts in a cell at all. This band can only ever be failed
//!    from below.
//! 2. **The floor asks for dominance, and the level cannot manufacture it at
//!    partial reach.** The level is a biome-INDEPENDENT scalar on a kind's `K`,
//!    so it registers only as a *contrast* between kinds that carry a row and
//!    kinds that do not. That contrast is not thin — the seven carriers hold
//!    61.2% of claimed-cell density — but deepening it SUPPRESSES those seven,
//!    which evens out the survivors instead of concentrating them. Driven to
//!    the extreme (level `0`: hard exclusion everywhere off the authored shape)
//!    the mean *rises* to 2.97, away from the only edge it could cross. The
//!    level's total downward reach from the shipped value is 0.043.
//!
//! **The level DOES redden it once the rows reach the whole roster.** Same
//! knob, same round trip, all seven authored shapes unchanged; the eleven kinds
//! with no authored row are each given one row carrying a single distinct land
//! stronghold (non-marine `hornvale_climate::biome::ALL`, taken in roster
//! order) at their own `sovereignty_floor` as level. Then:
//!
//! | arm (all 18 kinds carry a row) | mean | verdict |
//! |---|---|---|
//! | `d = 1.00` (the 7 authored rows bit-identical to shipped) | 2.5789073591583951 | PASS |
//! | `d = 1.60` | 1.5473196487276366 | PASS |
//! | **`d = 1.70`** | **1.4155085834088321** | **RED, beneath the floor** |
//!
//! Reach and shape are held fixed across those three rows; only the level
//! moves. It fails through the BAND — every seed still claims cells at every
//! arm, so `assert!(n > 0)` is never the thing that fires. And the seven-row
//! `d = 1.00` arm reproduces 2.1127601185602627 exactly, which is this sweep's
//! own check that it interpolates the shipped world rather than a neighbour of
//! it (the control §6 of the campaign spec generalises).
//!
//! **The qualification, stated as the property this guard actually has.** It is
//! not reach on its own: widening the reach to all eighteen kinds while giving
//! them a COMMON shape leaves the level nearly gauge again — 2.3678 / 2.5792 /
//! 2.5754 across the whole depth range, a span of 0.21, every arm PASS. What
//! the level needs in order to be visible here is a roster DIFFERENTIATED
//! ACROSS SPACE; the level then sets how sharply each kind is confined to its
//! own ground.
//!
//! > `beta_yields_realistic_coexistence` detects the biome-affinity level to
//! > the extent that the rows it scales partition the world between kinds. At
//! > today's seven overlapping rows in eighteen kinds it detects the level at
//! > no value whatsoever; with every kind on its own ground it detects it
//! > sharply, crossing the floor between `d = 1.60` and `d = 1.70`.
//!
//! Nor does either row group's level carry that red on its own (measured on a
//! shared-world probe, so read the third digit as approximate): with both
//! groups at `d = 1.70` the mean is 1.414; moving only the eleven added rows
//! gives 1.738 and moving only the seven authored rows gives 2.436, both PASS.
//! Parts of −0.842 and −0.143 against a combined −1.165 — about 15%
//! interaction. The red is a property of the roster, not of a subset of it.
//!
//! **The roster decides the verdict, so the assertion now names its roster.**
//! The band `[1.5, 0.75 × oatmeal]` is scored over the PEOPLED kinds — the
//! `psyche` key-set, 18 today. Scored instead over the 39-row biosphere, the
//! same worlds at the same β read 1.5063123260334543: against the floor of
//! **1.5 then in force** that is a margin of +0.0063, with two of five seeds
//! (1.3483, 1.3784) individually beneath that floor. (Both figures are dated
//! deliberately. The floor is expected to move — `BIO-40` is an open row asking
//! for exactly that recalibration — and a recorded margin is only meaningful
//! against the floor it was recorded against.) Both rosters pass, so the
//! campaign's predicted verdict *flip* did not reproduce on this tree — the
//! spec expected ≈1.42–1.46 and failure over the biosphere, and the measurement
//! says otherwise. Recorded as the falsification it is rather than adjusted to
//! recover the prediction. What survives is the sharper half: a margin of 0.4%
//! of the floor
//! is not a band that means the same thing under both readings, and the
//! published figure for this bound does not state which instrument produced it.
//! Hence the roster in the failure message.
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

/// The peopled component set: the canonical registries scoped to the peopled
/// kinds (the `psyche` key-set — fauna are biosphere-only, so they carry no
/// psyche row). This is the roster the band is scored over, and the assertion
/// below says so in its failure message; see the module doc's Muster section
/// for why that matters and what the biosphere reading is instead.
///
/// The scoping is by KEY-SET, never by a hand-written list: every store here
/// filters a live canonical registry, so a kind added to the registries appears
/// here without anyone editing this file. That is also why `habitat_realm` and
/// `biome_affinity` must be filtered rather than left empty — see below.
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
    // THE MUSTER: these two stores were `ComponentStore::new()` until now, and
    // that made this guard structurally blind to the two quantities it is most
    // supposed to see. `demography_report_with_beta_from` reads exactly three
    // stores off `wc` on the path to `byproducts.strife`
    // (`windows/worldgen/src/lib.rs:1728-1753`): `biosphere`, then
    // `habitat_realm`, then `biome_affinity` — all three iterated in the same
    // `wc.biosphere` order so they stay index-aligned. Both sparse stores read
    // through a default on absence (`HabitatRealm::SURFACE`; `None` =
    // unrestricted at every biome), so an EMPTY store is not an error — it is
    // silently the null hypothesis, and every authored affinity level scored
    // identically here. They now hold the live rows, scoped to the peopled
    // key-set exactly as `biosphere` and `family_of` above are.
    let habitat_realm: ComponentStore<KindId, hornvale_species::HabitatRealm> =
        hornvale_species::habitat_realm_registry()
            .iter()
            .filter(|(k, _)| peopled.contains(k))
            .map(|(k, v)| (*k, *v))
            .collect();
    let biome_affinity: ComponentStore<KindId, hornvale_species::BiomeAffinity> =
        hornvale_species::biome_affinity_registry()
            .iter()
            .filter(|(k, _)| peopled.contains(k))
            .map(|(k, v)| (*k, v.clone()))
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
        // deity / culture / material stay EMPTY deliberately, not by oversight:
        // no build or demography path reads them off `wc`. Their only readers
        // in the whole workspace are `WorldComponents::kinds_with` and
        // `kinds` (`windows/worldgen/src/components.rs:206-228`), pure
        // reflection over the roster, which this guard never calls. Filling
        // them would add non-peopled deity/culture/material kinds to
        // `wc.kinds()` and widen the roster this test names.
        ComponentStore::new(),
        ComponentStore::new(),
        ComponentStore::new(),
        habitat_realm,
        biome_affinity,
    )
    .expect("the peopled-only component set is well-formed")
}

/// The preregistered freeze check: at the frozen β, the mean per-claimed-cell
/// effective diversity across a handful of seeds lands in the band
/// `[MONOCULTURE_FLOOR, OATMEAL_FRACTION × peopled_count]` — `[1.5, 13.5]` at
/// today's 18 peopled kinds. The ceiling is DERIVED (The Delvers) and the
/// literal `3.0` this line used to name is the retired pre-Delvers value; see
/// the module doc for that re-baseline, the niche-era one, and the weak-knob
/// caveat.
/// claim: readout(preregistered) — mean per-claimed-cell diversity across
/// SEEDS over the PEOPLED roster, band [1.5, 0.75 x peopled_count]
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
    /// THE MUSTER, recorded 2026-08-11: the same quantity measured over the
    /// FULL 39-row biosphere (`WorldComponents::assemble()`) instead of the
    /// peopled roster, same seeds, same β. A historical datum, not a live
    /// read — the guard does not pay for a second 39-kind arm on every run.
    const BIOSPHERE_PER_SEED_AT_MUSTER: [f64; 5] = [
        1.5057753776200489,
        1.378426262285174,
        1.7867721889108887,
        1.512329406007136,
        1.3482583953440246,
    ];
    /// The floor in force when [`BIOSPHERE_PER_SEED_AT_MUSTER`] was recorded.
    /// Kept beside the measurement so the recorded margin stays interpretable
    /// after `MONOCULTURE_FLOOR` moves — `BIO-40` is an open row asking for
    /// exactly that recalibration, and a margin quoted against an unnamed
    /// floor goes silently false the moment the floor changes.
    const FLOOR_WHEN_BIOSPHERE_MEASURED: f64 = 1.5;

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

    // THE MUSTER: the band's verdict depends on WHICH POPULATION is counted,
    // not on the physics alone, so the assertion names its own roster. A red
    // gate must identify its instrument as well as its number.
    //
    // Everything the message says about the biosphere arm is either DATED to
    // the floor it was measured against or DERIVED from the floor in force on
    // this run. Nothing is a frozen margin quoted beside a live constant: that
    // pairing is exactly how a failure message starts printing arithmetic
    // nobody measured once someone moves the floor.
    let affinity_rows = wc.biome_affinity.len();
    let biosphere_rows = hornvale_species::biosphere_registry().len();
    let biosphere_mean: f64 = BIOSPHERE_PER_SEED_AT_MUSTER.iter().sum::<f64>()
        / BIOSPHERE_PER_SEED_AT_MUSTER.len() as f64;
    // Recorded margin, against the floor named beside the measurement.
    let biosphere_margin_then = biosphere_mean - FLOOR_WHEN_BIOSPHERE_MEASURED;
    let below_then = BIOSPHERE_PER_SEED_AT_MUSTER
        .iter()
        .filter(|d| **d < FLOOR_WHEN_BIOSPHERE_MEASURED)
        .count();
    // The same recorded reading placed against THIS run's floor. Both of these
    // stay arithmetically true if `MONOCULTURE_FLOOR` moves.
    let biosphere_margin_now = biosphere_mean - MONOCULTURE_FLOOR;
    let below_now = BIOSPHERE_PER_SEED_AT_MUSTER
        .iter()
        .filter(|d| **d < MONOCULTURE_FLOOR)
        .count();
    let n_seeds = BIOSPHERE_PER_SEED_AT_MUSTER.len();
    assert!(
        (MONOCULTURE_FLOOR..=ceiling).contains(&mean),
        "mean per-claimed-cell diversity at beta={} across seeds {per_seed:?} = {mean}, \
         expected in [{MONOCULTURE_FLOOR}, {ceiling}] — the floor is absolute (monoculture \
         is 1 whatever the roster size) and the ceiling is {OATMEAL_FRACTION} x oatmeal, \
         where oatmeal = {oatmeal} peopled species. If this fails ABOVE the ceiling the \
         world has gone undifferentiated; BELOW the floor it has gone monocultural. Do not \
         replace the derived ceiling with a literal.\n\
         ROSTER: this number was measured over the {oatmeal} PEOPLED kinds — the `psyche` \
         key-set, with every other canonical registry filtered to it — of which \
         {affinity_rows} carry a biome-affinity row. It is NOT the {biosphere_rows}-row \
         biosphere, and the distinction changes the reading rather than the world.\n\
         THE OTHER ROSTER, as recorded by The Muster on 2026-08-11 (a dated measurement, \
         not a live read): the same seeds at the same beta over the full biosphere gave \
         mean {biosphere_mean}, which against the floor of \
         {FLOOR_WHEN_BIOSPHERE_MEASURED} then in force was a margin of \
         {biosphere_margin_then:+}, with {below_then} of {n_seeds} seeds beneath that \
         floor. Placed against the floor THIS run used ({MONOCULTURE_FLOOR}), that same \
         recorded mean sits {biosphere_margin_now:+}, with {below_now} of {n_seeds} seeds \
         beneath it. When those two disagree in sign, the two rosters disagree on the \
         verdict — and this band was preregistered about the peopled one. Before deciding \
         the band is wrong, check which population you are counting.",
        hornvale_demography::BETA,
    );
}
