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

/// Stage 5's dimensional Michaelis-Menten ceiling, DERIVED (spec §5a, re-derived
/// on stage-1+4 physics): target 68.87 (pre-campaign good ground, a gauge choice
/// per 0105) / (MM frac 0.8138 x Liebig tolerance 0.6035).
const V_MAX: f64 = 140.2;
/// Half-saturation supply, DERIVED: median axis_supply over land on stage-1+4
/// physics (n = 401,148).
const K_M: f64 = 0.03004;

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
    // Per-seed (product-wins, liebig-wins) counts, indexed by SETTLERS position.
    let mut h1: Vec<(Vec<usize>, Vec<usize>)> = Vec::new();

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

        // H1 per seed: who wins each land cell under each combination rule?
        let mut wins_p = vec![0usize; SETTLERS.len()];
        let mut wins_l = vec![0usize; SETTLERS.len()];
        for &c in &land {
            use hornvale_kernel::{
                ANIMAL_PREY, DETRITUS, MARINE_FORAGE, MINERAL, PHOTOSYNTHATE, PLANT_FORAGE,
            };
            let mut best = (-1.0_f64, usize::MAX, -1.0_f64, usize::MAX);
            for (i, name) in SETTLERS.iter().enumerate() {
                let bio = wc.biosphere.iter().find(|(id, _)| id.0 == *name).unwrap().1;
                let fl = hornvale_kernel::sovereignty_floor(bio.mass, bio.potency);
                let cn = &bio.condition_niche;
                let sub = substrate.get(c);
                let per_axis = [
                    (PHOTOSYNTHATE, *base.get(c)),
                    (PLANT_FORAGE, *forage.get(c)),
                    (MINERAL, *mineral.get(c)),
                    (DETRITUS, *detritus.get(c)),
                    (ANIMAL_PREY, *prey.get(c)),
                    (MARINE_FORAGE, *marine.get(c)),
                ];
                let sup = axis_supply(&bio.niche, &per_axis);
                let (t, m, i2, e) = (
                    cn.temperature.eval(sub.temperature_c, fl),
                    cn.moisture.eval(sub.moisture, fl),
                    cn.insolation.eval(sub.insolation, fl),
                    cn.elevation.eval(sub.elevation, 0.0),
                );
                // today: saturate dimensionlessly, then MULTIPLY four tolerances
                let p = (sup / (1.0 + sup)) * t * m * i2 * e;
                // stage 5: dimensional Michaelis-Menten, then LIEBIG minimum
                let l = (V_MAX * sup / (K_M + sup)) * t.min(m).min(i2).min(e);
                if p > best.0 {
                    best.0 = p;
                    best.1 = i;
                }
                if l > best.2 {
                    best.2 = l;
                    best.3 = i;
                }
            }
            if best.0 > 0.0 {
                wins_p[best.1] += 1;
            }
            if best.2 > 0.0 {
                wins_l[best.3] += 1;
            }
        }
        h1.push((wins_p, wins_l));

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

    // ---- H1, tested as a pure measurement before any production change ----
    // best-fit territory is argmax_sp of eff(c,sp), and the species-blind capacity
    // CANCELS from that argmax (spec §5d), so best-fit depends ONLY on how the
    // per-species term combines. Compute it both ways over the same cells and the
    // attribution is exact: PRODUCT (today) versus LIEBIG MINIMUM (stage 5).
    println!("\n=== H1: does the combination rule redistribute best-fit territory? ===");
    println!(
        "    {:<10} {:>12} {:>12}",
        "species", "PRODUCT", "LIEBIG min"
    );
    let mut tot_prod = vec![0usize; SETTLERS.len()];
    let mut tot_lieb = vec![0usize; SETTLERS.len()];
    for (pw, lw) in &h1 {
        for i in 0..SETTLERS.len() {
            tot_prod[i] += pw[i];
            tot_lieb[i] += lw[i];
        }
    }
    for (i, name) in SETTLERS.iter().enumerate() {
        println!("    {:<10} {:>12} {:>12}", name, tot_prod[i], tot_lieb[i]);
    }
    let won_p = tot_prod.iter().filter(|n| **n > 0).count();
    let won_l = tot_lieb.iter().filter(|n| **n > 0).count();
    println!("    species winning ANY territory: product {won_p}/6  ->  Liebig {won_l}/6");

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
    // THE GAUGE IS FROZEN, and this line is why it has to be stated rather than
    // measured. `target = pct(&all_good_capacity, 0.50)` re-measured the anchor
    // on every run, so the "derivation" was really "reproduce whatever good
    // ground reads today" — and any drift in the base field would be silently
    // absorbed into V_max, gauging the ruler against the thing it measures.
    // Decision 0105 fixes the target at the PRE-CAMPAIGN level; that is what
    // makes it a gauge choice rather than a fit.
    const FROZEN_TARGET: f64 = 68.87;
    let target = FROZEN_TARGET;
    let measured_today = pct(&all_good_capacity, 0.50);
    let drift = (measured_today / target - 1.0) * 100.0;
    let s_good = pct(&all_supply, 0.90); // good ground carries high supply
    let mm_frac = s_good / (k_m + s_good);
    println!(
        "\nDERIVATIONS — measured, then solved. Do NOT author round numbers.\n\
         \x20 K_m   := median axis_supply over land            = {k_m:.5}\n\
         \x20 target: PRE-CAMPAIGN good ground (frozen gauge)  = {target:.2} headcount\n\
         \x20 measured on good ground TODAY                    = {measured_today:.2} \
({drift:+.1}% vs the gauge)\n\
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
