//! The Deep Realm, Task 6: H1 readout — has the xorn's (and rust monster's)
//! condition niche stopped faking cave-dark on the surface?
//!
//! **REPORTED, never asserted** (spec §7, H1; plan Task 6 Step 4): whichever
//! way the numbers land is the finding. This battery does not assert that
//! either species' surface fit collapses — it measures both species' mean
//! niche fit against the real surface substrate and against the derived
//! subterranean substrate, and prints both.
//!
//! **The `unreachable!()` hazard (Task 1, carried forward to Task 6).** This
//! file never constructs a `hornvale_climate::facets::BiomeExpr` and never
//! calls `.biome()` — it only ever builds `hornvale_worldgen::Substrate`
//! values (via `substrate_field` and `subterranean_substrate`), so the
//! `unreachable!()` panics guarding a cave `Formation`/rock `Stratum`
//! reaching that projection (`domains/climate/src/facets.rs:305,316`,
//! `variants.rs:727`) are never in this file's call path. See
//! `subterranean_substrate`'s own docs for the same statement made at its
//! definition.
//!
//! **World identity moves here (ledger #27).** `xorn` and `rust-monster`'s
//! re-authored niches are still scored against the SURFACE substrate by
//! every ordinary consumer (`per_species_suitability`, settlement
//! placement, …) — chambers are not wired into placement in this campaign
//! (spec §6: C2a has nobody underground). So a niche genuinely tuned for
//! subterranean conditions relocates these two kinds in the generated
//! world; `cli/tests/fixtures/world-seed-42.json` and the generated
//! artifacts are re-baselined in the same commit as this file.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture, reused from
//! `windows/worldgen/tests/deep_realm_substrate.rs` and `insolation_probe.rs`.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_species::{ConditionNiche, HabitatRealm};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, Substrate, WorldComponents, build_world_to, climate_of,
    per_species_suitability, substrate_field, subterranean_substrate, terrain_of,
};

/// The probe window — matches Task 0's `deep_realm_substrate.rs` convention
/// (seeds `1..=30`) so this readout's cost and coverage are of a piece with
/// the campaign's other live-worldgen batteries.
const SEEDS: std::ops::RangeInclusive<u64> = 1..=30;

/// The habitat-condition product alone — [`ConditionNiche`]'s four-axis
/// term, EXCLUDING resource supply. This isolates exactly what Task 6
/// changed (the niche curves, and the substrate a subterranean reading
/// scores against) from what it did not (the resource-supply model, whose
/// shape is unmodified by this task). Mirrors the condition-response half of
/// `hornvale_worldgen::per_species_suitability`'s per-cell product.
fn niche_fit(cn: &ConditionNiche, s: &Substrate, floor: f64) -> f64 {
    cn.temperature.eval(s.temperature_c, floor)
        * cn.moisture.eval(s.moisture, floor)
        * cn.insolation.eval(s.insolation, floor)
        * cn.elevation.eval(s.height_asl_m.get(), 0.0)
}

/// One seed's mean surface vs. subterranean niche fit for one species,
/// averaged over every LAND cell (`!terrain.is_ocean`) — the same predicate
/// Task 0's battery uses, and the one `GeneratedTerrain::cave_at` itself
/// already gates on (it returns `None` on every ocean cell), so this
/// introduces no second, independently-chosen population.
struct SeedFit {
    /// Mean niche-fit product over land cells, scored against the real
    /// surface `Substrate`.
    surface_mean: f64,
    /// Mean niche-fit product over the SAME land cells, scored against
    /// `subterranean_substrate` of that same surface reading.
    subterranean_mean: f64,
}

/// Build `seed` to `BuildDepth::Terrain` (the shallowest rung carrying the
/// terrain/climate this readout reads — `climate_of` reconstructs climate
/// from committed facts independent of build depth, so `Terrain` suffices;
/// see `insolation_probe.rs` for the same pattern) and measure `label`'s
/// (`"xorn"` or `"rust-monster"`) mean niche fit over every land cell.
fn measure_one(seed: Seed, wc: &WorldComponents, label: &str) -> SeedFit {
    let world = build_world_to(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        BuildDepth::Terrain,
    )
    .unwrap_or_else(|e| panic!("{seed:?} failed to build: {e:?}"));
    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let climate = climate_of(&world).expect("climate reconstructs");
    let geo = terrain.geosphere();

    let bio = wc
        .biosphere
        .get_by_label(label)
        .unwrap_or_else(|| panic!("{label:?} missing from the biosphere roster"));
    let floor = hornvale_kernel::sovereignty_floor(bio.mass, bio.potency);
    let cn = &bio.condition_niche;

    let surface = substrate_field(
        geo,
        &terrain,
        &climate,
        climate.obliquity_deg(),
        climate.insolation(),
        &climate.regime(),
    );

    let mut surface_total = 0.0;
    let mut subterranean_total = 0.0;
    let mut n = 0usize;
    for cell in geo.cells() {
        if terrain.is_ocean(cell) {
            continue;
        }
        let s = *surface.get(cell);
        let sub = subterranean_substrate(s);
        surface_total += niche_fit(cn, &s, floor);
        subterranean_total += niche_fit(cn, &sub, floor);
        n += 1;
    }
    assert!(n > 0, "{seed:?} has no land cells");
    SeedFit {
        surface_mean: surface_total / n as f64,
        subterranean_mean: subterranean_total / n as f64,
    }
}

/// Spec H1, REPORTED not asserted (plan Task 6 Step 1/4). Report surface vs.
/// subterranean niche fit for the xorn and the rust monster, over seeds
/// `1..=30`, before making any claim about whether either "collapsed" —
/// the module doc comment states why this file's own report is the finding,
/// whichever way it comes out.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn report_the_xorn_before_and_after() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");

    for label in ["xorn", "rust-monster"] {
        let mut surface_sum = 0.0;
        let mut subterranean_sum = 0.0;
        let mut count = 0usize;
        println!("\n=== {label}: per-seed mean niche fit (land cells only) ===");
        println!(
            "{:>6} | {:>16} | {:>16}",
            "seed", "surface_fit", "subterranean_fit"
        );
        for raw_seed in SEEDS {
            let fit = measure_one(Seed(raw_seed), &wc, label);
            println!(
                "{:>6} | {:>16.6} | {:>16.6}",
                raw_seed, fit.surface_mean, fit.subterranean_mean
            );
            assert!(
                fit.surface_mean.is_finite() && fit.surface_mean >= 0.0,
                "{label} seed {raw_seed}: surface fit must be finite and non-negative, got {}",
                fit.surface_mean
            );
            assert!(
                fit.subterranean_mean.is_finite() && fit.subterranean_mean >= 0.0,
                "{label} seed {raw_seed}: subterranean fit must be finite and non-negative, got {}",
                fit.subterranean_mean
            );
            surface_sum += fit.surface_mean;
            subterranean_sum += fit.subterranean_mean;
            count += 1;
        }

        let surface_avg = surface_sum / count as f64;
        let subterranean_avg = subterranean_sum / count as f64;
        let ratio = if subterranean_avg > 0.0 {
            surface_avg / subterranean_avg
        } else {
            f64::INFINITY
        };
        println!(
            "{label}: mean surface fit = {surface_avg:.6}, mean subterranean fit = {subterranean_avg:.6}, \
             surface/subterranean ratio = {ratio:.6} over {count} seeds"
        );
    }
}

// ---------------------------------------------------------------------------
// THE WARREN, Task 3: the hand probe above measured the rehoming by calling
// `subterranean_substrate` directly, because nothing live used it. Task 2
// wired `per_species_suitability` to ask `HabitatRealm` and score a
// `Subterranean` kind against that same function's output. These two tests
// are the wiring check, on seed 42, restricted to cave-bearing land cells
// (where the cave-availability factor is 1.0 for both readings, so the ratio
// isolates the niche-curve difference rather than the gate).
//
// **Both now assert a ratio of exactly 1.000, and that is a falsification
// pinned rather than a test relaxed.** The Tilth's Liebig minimum floors
// temperature, moisture and insolation by the sovereignty floor and leaves
// elevation bare, so the unfloored elevation term is the sole determinant —
// and `subterranean_substrate` inherits `height_asl_m` from the surface cell
// unchanged. Going underground improves exactly the axes it was built to
// improve (moisture .585 -> .787, insolation .467 -> .840) and the minimum
// never sees it; `warren_liebig_probe.rs` prints all four terms. The Warren's
// spec §10.3 records the amendment, and `warren_readout.rs` carries the same
// tripwire: when a tolerance model lands in which a non-lethal preference can
// bind, these reddens on purpose.
// ---------------------------------------------------------------------------

/// The `(names, realm slice)` construction `demography_report_with_beta_from`
/// itself does — copied rather than shared (decision 0092's test-fixture
/// posture, the same one this file's header already invokes).
fn realm_slice(wc: &WorldComponents) -> Vec<HabitatRealm> {
    wc.biosphere
        .iter()
        .map(|(kind, _)| {
            wc.habitat_realm
                .get(kind)
                .copied()
                .unwrap_or(HabitatRealm::SURFACE)
        })
        .collect()
}

/// Seed 42's mean suitability for `label`, over every LAND cell that HOLDS A
/// CAVE, scored two ways through the SAME live `per_species_suitability`
/// call site: once with the real (sparse) realm slice — "live", identical to
/// what `demography_report_with_beta_from` computes — and once with every
/// kind forced to `Surface` — "surface-forced", the pre-campaign scoring.
/// Restricting to cave-bearing cells holds the availability factor at `1.0`
/// on both sides, so the ratio measures only the niche-curve asymmetry Task
/// 6 of The Deep Realm measured by hand.
fn live_vs_surface_forced_on_cave_cells(label: &str) -> (f64, f64, usize) {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let world = build_world_to(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Terrain,
    )
    .expect("seed 42 builds");
    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let climate = climate_of(&world).expect("climate reconstructs");
    let geo = terrain.geosphere();

    let bio: Vec<&hornvale_species::BiosphereTraits> =
        wc.biosphere.iter().map(|(_, b)| b).collect();
    let names: Vec<&'static str> = wc.biosphere.ids().map(|k| k.0).collect();
    let realm_live = realm_slice(&wc);
    let realm_surface_forced: Vec<HabitatRealm> = vec![HabitatRealm::Surface; bio.len()];

    let obliquity_deg = climate.obliquity_deg();
    let insolation_scalar = climate.insolation();
    let regime = climate.regime();

    let k_live = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &bio,
        &realm_live,
    );
    let k_surface_forced = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &bio,
        &realm_surface_forced,
    );

    let tag = names
        .iter()
        .position(|n| *n == label)
        .unwrap_or_else(|| panic!("{label} missing from the biosphere roster"))
        as u32;
    let live_map = &k_live.iter().find(|(t, _)| *t == tag).unwrap().1;
    let surface_map = &k_surface_forced.iter().find(|(t, _)| *t == tag).unwrap().1;

    let mut live_total = 0.0;
    let mut surface_total = 0.0;
    let mut n = 0usize;
    for cell in geo.cells() {
        if terrain.is_ocean(cell) || terrain.cave_at(cell).is_none() {
            continue;
        }
        live_total += *live_map.get(cell);
        surface_total += *surface_map.get(cell);
        n += 1;
    }
    assert!(n > 0, "seed 42 must have cave-bearing land cells");
    (live_total / n as f64, surface_total / n as f64, n)
}

#[test]
fn rust_monster_live_path_reproduces_c2as_subterranean_asymmetry() {
    // C2a measured rust-monster's subterranean fit at ~2.5x its surface fit
    // by hand, and this branch reproduced it at 2.557x through the live path
    // BEFORE absorbing The Tilth. It now reads exactly 1.000 — see this
    // module's header for why, and do not relax this bound to make it pass.
    let (live, surface_forced, n) = live_vs_surface_forced_on_cave_cells("rust-monster");
    let ratio = live / surface_forced;
    println!(
        "rust-monster (live path): live={live:.6} surface-forced={surface_forced:.6} \
         ratio={ratio:.3} over {n} cave-bearing land cells"
    );
    assert!(
        (ratio - 1.0).abs() < 1e-9,
        "rust-monster's live ratio is expected to be EXACTLY 1.000 while the unfloored \
         elevation axis is the sole determinant of `tolerance_liebig` (see this module's \
         header). Got {ratio:.6} ({live:.6} vs {surface_forced:.6} over {n} cells). If this \
         moved, a tolerance model in which a non-lethal preference can bind has landed — that \
         is good news, and The Warren's spec §10.3 and chronicle need re-measuring rather \
         than this assertion needing a nudge."
    );
}

#[test]
fn xorn_live_path_reproduces_c2as_flat_ratio() {
    // C2a measured the xorn's ratio at 1.02, flat within noise: its potency
    // buys a large sovereignty floor and its devotions are near-zero on
    // every axis, so no curve moves it. THIS is the campaign's wiring check
    // (plan Task 3) — reproducing that flatness through a DIFFERENT code
    // path is what proves the right thing got connected, rather than
    // something that merely moves numbers.
    let (live, surface_forced, n) = live_vs_surface_forced_on_cave_cells("xorn");
    let ratio = live / surface_forced;
    println!(
        "xorn (live path): live={live:.6} surface-forced={surface_forced:.6} ratio={ratio:.3} \
         over {n} cave-bearing land cells"
    );
    assert!(
        (0.8..=1.25).contains(&ratio),
        "xorn's ratio must stay essentially flat between subterranean and surface scoring \
         (C2a measured 1.02); got ratio {ratio:.3} ({live:.6} vs {surface_forced:.6} over {n} cells)"
    );
}
