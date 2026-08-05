//! THE TILTH — derivation probe. Measurement only, not a shipped feature.
//!
//! The campaign's own §6 names its central risk: `V_max`, `K_m` and the
//! moisture→precipitation scale are three new authored constants in a campaign
//! whose thesis is that the model already carries too much unexplained
//! arithmetic. This probe exists so all three are **derived from measured
//! distributions and shown**, rather than authored round and defended later.
//!
//! It answers, over the spec's five probe seeds:
//!
//! 1. **The moisture distribution over land** — Lieth's precipitation term needs
//!    mm/yr and Hornvale has a normalised `[0,1]` moisture, so the conversion
//!    scale must be pinned to where moisture actually sits, not guessed.
//! 2. **The `axis_supply` distribution over land, per settling species** — `K_m`
//!    is a half-saturation constant, so it belongs at the median of the supply it
//!    is meant to half-saturate.
//! 3. **Today's capacity on GOOD ground** — `V_max` is calibrated so the case the
//!    model already gets right keeps roughly today's answer. Calibrating on the
//!    well-behaved case and letting the marginal cases fall where the model puts
//!    them is what makes the result falsifiable rather than fitted.
//!
//! Inherits two corrections from `keeping_probe.rs`: ocean is
//! `terrain.is_ocean`, never `elevation < 0` (seed 42's sea level is −2,936 m);
//! and `per_species_suitability` returns a dimensionless suitability, so
//! anything with headcount units must come from the capacity field.

// `terrain_of` is a named derivation entry point (decision 0092): it sculpts a
// full tectonic globe per call, and a probe measuring five worlds is exactly such
// a site — the allowance `waterline_probe.rs` and `keeping_probe.rs` both take.
#![allow(clippy::disallowed_methods)]

use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::{
    SETTLERS_PER_CAPACITY, SettlementPins, SkyChoice, axis_supply, build_world, carrying_inputs_of,
    climate_of, sky_of, terrain_of,
};

/// The settling roster (`SocialForm::Settled`), verified as exactly these six.
const SETTLERS: [&str; 6] = ["kobold", "goblin", "hobgoblin", "bugbear", "gnoll", "human"];

/// `worldgen::MINERAL_SUPPLY_SCALE`, private, so mirrored here (the pattern
/// `keeping_probe.rs` uses for the bake's private constants). A probe should not
/// force an API widening.
const MINERAL_SUPPLY_SCALE: f64 = 1.0;
/// `worldgen::MARINE_SUPPLY_SCALE`, private, so mirrored here.
const MARINE_SUPPLY_SCALE: f64 = 1.0;

/// The spec's probe seeds.
const SEEDS: [u64; 5] = [42, 7, 999_999, 16_244_526_067_196_353_746, 1234];

fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let i = ((sorted.len() - 1) as f64 * q).round() as usize;
    sorted[i]
}

#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn tilth_derivation_probe() {
    println!("\n############ THE TILTH — derivation probe ############");
    println!(
        "deriving: moisture->precip scale, K_m (half-saturation), V_max (headcount ceiling)\n"
    );

    let mut all_moisture: Vec<f64> = Vec::new();
    let mut all_supply: Vec<f64> = Vec::new();
    let mut all_good_capacity: Vec<f64> = Vec::new();
    let mut all_min_cond: Vec<f64> = Vec::new();

    for seed in SEEDS {
        let world = build_world(
            hornvale_kernel::Seed(seed),
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("probe seed builds");

        let terrain = terrain_of(&world).unwrap();
        let climate = climate_of(&world).unwrap();
        let sky = sky_of(&world).unwrap();
        let geo = terrain.geosphere();
        let generated = match &sky {
            hornvale_worldgen::Sky::Generated(g) => g,
            _ => panic!("probe expects a generated sky"),
        };
        let system = generated.system();
        let insolation_scalar = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
        let obliquity_deg = system.anchor.obliquity.get();
        let regime = match system.anchor.rotation {
            hornvale_astronomy::Rotation::Spinning { day, .. } => {
                hornvale_climate::RotationRegime::Spinning { day_std: day.get() }
            }
            hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
        };

        let wc = WorldComponents::assemble().unwrap();
        let land: Vec<_> = geo.cells().filter(|c| !terrain.is_ocean(*c)).collect();

        // (1) moisture over land
        let mut moisture: Vec<f64> = land.iter().map(|c| climate.moisture_at(*c)).collect();
        moisture.sort_by(f64::total_cmp);
        all_moisture.extend(moisture.iter().copied());

        // (3) today's capacity, and "good ground" = the top decile of it
        let capacity = hornvale_demography::carrying_capacity(
            geo,
            &carrying_inputs_of(geo, &terrain, &climate),
        );
        let mut caps: Vec<f64> = land
            .iter()
            .map(|c| capacity.at(*c) * SETTLERS_PER_CAPACITY)
            .filter(|v| *v > 0.0)
            .collect();
        caps.sort_by(f64::total_cmp);
        let good: Vec<f64> = caps
            .iter()
            .copied()
            .filter(|v| *v >= pct(&caps, 0.90))
            .collect();
        all_good_capacity.extend(good.iter().copied());

        // (2) axis_supply over land, per settling species — the quantity K_m
        // half-saturates. Rebuilt from the same per-axis fields
        // `per_species_suitability` uses, so the distribution is the real one.
        let substrate = hornvale_worldgen::substrate_field(
            geo,
            &terrain,
            &climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
        );
        let base = capacity.as_cell_map();
        let forage = hornvale_worldgen::forage_supply_field(geo, base);
        let mineral = hornvale_worldgen::mineral_supply_field(geo, &terrain, MINERAL_SUPPLY_SCALE);
        let detritus = hornvale_worldgen::detritus_supply_field(geo, &terrain);
        let prey = hornvale_worldgen::prey_supply_field(geo, &forage);
        let marine = hornvale_worldgen::marine_forage_supply_field(
            geo,
            &terrain,
            &climate,
            MARINE_SUPPLY_SCALE,
        );

        let mut supply: Vec<f64> = Vec::new();
        for name in SETTLERS {
            let bio = wc.biosphere.iter().find(|(id, _)| id.0 == name).unwrap().1;
            for &c in &land {
                use hornvale_kernel::{
                    ANIMAL_PREY, DETRITUS, MARINE_FORAGE, MINERAL, PHOTOSYNTHATE, PLANT_FORAGE,
                };
                let per_axis = [
                    (PHOTOSYNTHATE, *base.get(c)),
                    (PLANT_FORAGE, *forage.get(c)),
                    (MINERAL, *mineral.get(c)),
                    (DETRITUS, *detritus.get(c)),
                    (ANIMAL_PREY, *prey.get(c)),
                    (MARINE_FORAGE, *marine.get(c)),
                ];
                let s = axis_supply(&bio.niche, &per_axis);
                if s > 0.0 {
                    supply.push(s);
                }
            }
        }
        supply.sort_by(f64::total_cmp);
        all_supply.extend(supply.iter().copied());

        // (4) THE LAST UNMEASURED FACTOR: min-of-conditions (Liebig, per spec
        // §3.3) for the BEST-FIT settler on GOOD ground. V_max cannot be solved
        // without it, and authoring it is exactly what §6 forbids.
        let good_cut = pct(&caps, 0.90);
        for &c in &land {
            if capacity.at(c) * SETTLERS_PER_CAPACITY < good_cut {
                continue;
            }
            let sub = substrate.get(c);
            let best = SETTLERS
                .iter()
                .map(|name| {
                    let bio = wc.biosphere.iter().find(|(id, _)| id.0 == *name).unwrap().1;
                    let fl = hornvale_kernel::sovereignty_floor(bio.mass, bio.potency);
                    let cn = &bio.condition_niche;
                    // Liebig: the binding axis limits. Elevation is hard (floor 0).
                    cn.temperature
                        .eval(sub.temperature_c, fl)
                        .min(cn.moisture.eval(sub.moisture, fl))
                        .min(cn.insolation.eval(sub.insolation, fl))
                        .min(cn.elevation.eval(sub.elevation, 0.0))
                })
                .fold(0.0_f64, f64::max);
            all_min_cond.push(best);
        }

        println!(
            "seed {seed}: land {}  moisture p50 {:.4} p90 {:.4} max {:.4}  |  \
             capacity>0 n={} p50 {:.2} p90 {:.2}  |  supply>0 n={} p50 {:.5}",
            land.len(),
            pct(&moisture, 0.50),
            pct(&moisture, 0.90),
            pct(&moisture, 1.0),
            caps.len(),
            pct(&caps, 0.50),
            pct(&caps, 0.90),
            supply.len(),
            pct(&supply, 0.50),
        );
    }

    all_moisture.sort_by(f64::total_cmp);
    all_supply.sort_by(f64::total_cmp);
    all_good_capacity.sort_by(f64::total_cmp);
    all_min_cond.sort_by(f64::total_cmp);

    println!("\n=== POOLED, all five seeds — the derivation inputs ===");
    println!(
        "moisture over land        n={:8}  p10 {:.4}  p50 {:.4}  p90 {:.4}  max {:.4}",
        all_moisture.len(),
        pct(&all_moisture, 0.10),
        pct(&all_moisture, 0.50),
        pct(&all_moisture, 0.90),
        pct(&all_moisture, 1.0)
    );
    println!(
        "axis_supply (>0) over land n={:8}  p10 {:.5}  p50 {:.5}  p90 {:.5}  max {:.5}",
        all_supply.len(),
        pct(&all_supply, 0.10),
        pct(&all_supply, 0.50),
        pct(&all_supply, 0.90),
        pct(&all_supply, 1.0)
    );
    println!(
        "capacity on GOOD ground    n={:8}  p10 {:.2}  p50 {:.2}  p90 {:.2}  max {:.2}",
        all_good_capacity.len(),
        pct(&all_good_capacity, 0.10),
        pct(&all_good_capacity, 0.50),
        pct(&all_good_capacity, 0.90),
        pct(&all_good_capacity, 1.0)
    );
    // V_max is NOT the target directly: it must be solved THROUGH the
    // Michaelis-Menten fraction, because eff = V_max * S/(K_m+S) * min(conditions).
    // Solve so the MEDIAN new capacity on good ground equals the MEDIAN of
    // today's. Everything below is measured; nothing is authored.
    let k_m = pct(&all_supply, 0.50);
    let target = pct(&all_good_capacity, 0.50);
    let s_good = pct(&all_supply, 0.90); // good ground carries high supply
    let mm_frac = s_good / (k_m + s_good);
    println!(
        "\nDERIVATIONS — measured, then solved. Do NOT author round numbers.\n\
         \x20 K_m   := median axis_supply over land            = {k_m:.5}\n\
         \x20 target: median capacity on good ground today     = {target:.2} headcount\n\
         \x20 MM fraction at good-ground supply (p90={s_good:.5})   = {mm_frac:.4}\n\
         \x20 => V_max = target / (MM_frac * min_conditions_on_good_ground)\n\
         \x20 min-of-conditions (Liebig) for the BEST-FIT settler on good ground:\n\
         \x20    p10 {:.4}  p50 {:.4}  p90 {:.4}   (n={})\n\
         \x20 => V_max = {:.2} / ({:.4} * {:.4}) = {:.1}\n\
         \x20 moisture->precip: P_FULL = 2000 mm/yr, anchored EXTERNALLY so the\n\
         \x20   pooled median land moisture ({:.4}) maps to {:.0} mm/yr against\n\
         \x20   Earth's land-mean ~750. Median land is then moisture-limited\n\
         \x20   (Lieth precip 0.389 < temp 0.786 at 22 C), as Earth's is.",
        pct(&all_min_cond, 0.10),
        pct(&all_min_cond, 0.50),
        pct(&all_min_cond, 0.90),
        all_min_cond.len(),
        target,
        mm_frac,
        pct(&all_min_cond, 0.50),
        target / (mm_frac * pct(&all_min_cond, 0.50)),
        pct(&all_moisture, 0.50),
        pct(&all_moisture, 0.50) * 2000.0,
    );
}
