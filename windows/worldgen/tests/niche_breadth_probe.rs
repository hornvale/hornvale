//! THE TILTH — niche-breadth diagnostic. Measurement only, not a shipped feature.
//!
//! Answers one question Nathan raised that the siting algorithm cannot: **if a
//! people is meant to be a generalist — "like rats or humans, pretty much
//! anywhere they haven't been actively exterminated" — does the model let it be
//! one?**
//!
//! The siting bug that prompted this (kobold seeded onto ground worth zero, then
//! starved out of the world) is downstream of niche breadth. A shortlist can only
//! shortlist ground the species can actually live on, so if the breadth is wrong
//! no siting rule can rescue it.
//!
//! ## What it prints, and why each column
//!
//! Per settling species, over LAND only (`terrain.is_ocean`, never `elevation <
//! 0` — seed 42's sea level is −2,936 m):
//!
//! - **`K>0`** — where the species has any capacity at all. This is what
//!   `vacant_for` filters on, so it bounds where the species may ever be placed.
//! - **`K>5`** — where a genesis community can *survive*. `GENESIS_POP` is 10 and
//!   `pressure = pop × NEED / eff`, so `eff > 10/COLLAPSE_PRESSURE = 5.0` is the
//!   real floor for a founding. **A cell that passes `K>0` but fails `K>5` is a
//!   trap**: the species may be sited there and will starve.
//! - **`K>14.3`** — where it can throw a daughter (`DAUGHTER_MAX_PRESSURE` 0.7),
//!   i.e. where the species can actually *spread* rather than merely cling.
//! - **binding axis** — which of the four condition responses is the Liebig
//!   minimum. `tolerance_liebig` buffers temperature/moisture/insolation by the
//!   species' `sovereignty_floor` but passes **`0.0` for elevation**, so elevation
//!   is the only axis that can decay to zero. If it binds nearly everywhere, the
//!   other three axes are decoration and every species is an elevation specialist
//!   whatever its author intended.

// `terrain_of` is a named derivation entry point (decision 0092); a probe
// measuring one world is exactly the site the allowance is for.
#![allow(clippy::disallowed_methods)]

use hornvale_kernel::sovereignty_floor;
use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, build_world, climate_of, per_species_capacity, sky_of,
    substrate_field, terrain_of,
};

/// The settling roster, in registry order.
const SETTLERS: [&str; 6] = ["kobold", "goblin", "hobgoblin", "bugbear", "gnoll", "human"];

/// The seed the campaign's preregistrations are stated on.
const SEED: u64 = 42;

/// `GENESIS_POP / COLLAPSE_PRESSURE` — the capacity a founding needs to survive
/// its first epoch. Mirrored from `history_bake`'s private constants, the pattern
/// `keeping_probe.rs` and `tilth_probe.rs` both use.
const SURVIVE_K: f64 = 10.0 / 2.0;

/// `GENESIS_POP / DAUGHTER_MAX_PRESSURE` — the capacity needed to ever spread.
const SPREAD_K: f64 = 10.0 / 0.7;

fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let i = ((sorted.len() - 1) as f64 * q).round() as usize;
    sorted[i]
}

#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn niche_breadth_over_land_at_seed_42() {
    report(SEED);
}

/// The same tables for seed 1234, which bakes a world with **zero surviving
/// settlements** — 27 occupations opened, 25 starved, none alive at `now`. It did
/// so before this campaign too, so it is a standing defect rather than a
/// regression. Its almanac looks unremarkable beside seed 42's (11% habitable
/// against 10%, 72% ocean against 73%), so whatever kills it is not visible at
/// that altitude; this prints the field the bake actually reasons over.
#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn niche_breadth_over_land_at_seed_1234() {
    report(1234);
}

fn report(seed_value: u64) {
    let wc = WorldComponents::assemble().expect("components assemble");
    let seed = hornvale_kernel::Seed(seed_value);
    let world = build_world(
        seed,
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("probe seed builds");
    let terrain = terrain_of(&world).expect("terrain");
    let climate = climate_of(&world).expect("climate");
    let geo = terrain.geosphere();
    let sky = sky_of(&world).expect("sky");
    // The stellar-input triple, resolved exactly as `tilth_probe.rs` does
    // (`stellar_inputs` itself is private to worldgen).
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

    let land: Vec<hornvale_kernel::CellId> =
        geo.cells().filter(|&c| !terrain.is_ocean(c)).collect();
    println!("== seed {seed_value} ==");
    println!("land cells: {} of {}", land.len(), geo.cell_count());

    let biosphere: Vec<&hornvale_species::BiosphereTraits> = SETTLERS
        .iter()
        .map(|n| {
            wc.biosphere
                .get(&hornvale_kernel::KindId(n))
                .expect("settler has biosphere traits")
        })
        .collect();

    let caps = per_species_capacity(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &biosphere,
    );
    let substrate = substrate_field(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
    );

    println!();
    println!(
        "{:<10} {:>6} {:>8} {:>8} {:>8}   {:>8} {:>8} {:>8}  {:>8}",
        "species", "floor", "K>0", "K>5", "K>14.3", "p50", "p90", "max", "cold"
    );
    println!("  (last column: % of SUB-SNOWLINE land the species calls survivable)");
    for (i, name) in SETTLERS.iter().enumerate() {
        let bio = biosphere[i];
        let floor = sovereignty_floor(bio.mass, bio.potency);
        let cap = &caps[i].1;
        let mut vals: Vec<f64> = land.iter().map(|&c| cap.at(c)).collect();
        let n = vals.len() as f64;
        let gt = |t: f64| vals.iter().filter(|v| **v > t).count() as f64 / n * 100.0;
        let (p0, p5, p14) = (gt(0.0), gt(SURVIVE_K), gt(SPREAD_K));
        // The column that says whether a species is indifferent to LETHAL cold:
        // how much of the land below the bake's own -10C snowline it would still
        // call survivable. The era mask calls every one of these cells
        // uninhabitable; capacity never consults the snowline at all.
        let cold_survivable = land
            .iter()
            .filter(|&&c| substrate.get(c).temperature_c < -10.0)
            .filter(|&&c| cap.at(c) > SURVIVE_K)
            .count();
        let cold_total = land
            .iter()
            .filter(|&&c| substrate.get(c).temperature_c < -10.0)
            .count()
            .max(1);
        let cold_pct = cold_survivable as f64 / cold_total as f64 * 100.0;
        vals.sort_by(f64::total_cmp);
        println!(
            "{:<10} {:>6.3} {:>7.1}% {:>7.1}% {:>7.1}%   {:>8.2} {:>8.2} {:>8.2}  {:>7.1}%",
            name,
            floor,
            p0,
            p5,
            p14,
            pct(&vals, 0.50),
            pct(&vals, 0.90),
            pct(&vals, 1.0),
            cold_pct
        );
    }

    // Which condition axis is the Liebig minimum, over land. Printed under BOTH
    // elevation-floor conventions, because the first version of this probe
    // hardcoded `0.0` and so reported an identical table while the library under
    // test had been changed to buffer elevation — a measurement that could not
    // see the thing it was measuring. Printing both makes the comparison the
    // point rather than an assumption.
    for (label, elev_floored) in [("elevation floor 0.0", false), ("elevation buffered", true)] {
        println!();
        println!("-- binding axis, {label} --");
        println!(
            "{:<10} {:>10} {:>10} {:>10} {:>10}",
            "species", "temp%", "moist%", "insol%", "ELEV%"
        );
        for (i, name) in SETTLERS.iter().enumerate() {
            let bio = biosphere[i];
            let floor = sovereignty_floor(bio.mass, bio.potency);
            let elev_floor = if elev_floored { floor } else { 0.0 };
            let cn = &bio.condition_niche;
            let mut counts = [0u32; 4];
            for &c in &land {
                let s = substrate.get(c);
                let terms = [
                    cn.temperature.eval(s.temperature_c, floor),
                    cn.moisture.eval(s.moisture, floor),
                    cn.insolation.eval(s.insolation, floor),
                    cn.elevation.eval(s.elevation, elev_floor),
                ];
                let mut best = 0;
                for (j, t) in terms.iter().enumerate() {
                    if *t < terms[best] {
                        best = j;
                    }
                }
                counts[best] += 1;
            }
            let n = land.len() as f64;
            println!(
                "{:<10} {:>9.1}% {:>9.1}% {:>9.1}% {:>9.1}%",
                name,
                f64::from(counts[0]) / n * 100.0,
                f64::from(counts[1]) / n * 100.0,
                f64::from(counts[2]) / n * 100.0,
                f64::from(counts[3]) / n * 100.0
            );
        }
    }

    // The climate the niches are being evaluated against. Temperature is the
    // dominant binding axis once every axis is floored, so its real range over
    // land is the context every number above sits in — and it is what tells a
    // genuinely frozen world apart from a temperate one that merely has nobody
    // left on it.
    println!();
    let mut temps: Vec<f64> = land
        .iter()
        .map(|&c| substrate.get(c).temperature_c)
        .collect();
    temps.sort_by(f64::total_cmp);
    let qs = [0.0, 0.05, 0.25, 0.50, 0.75, 0.95, 1.0];
    print!("{:<10}", "land degC:");
    for q in qs {
        print!(" {:>7.1}", pct(&temps, q));
    }
    println!();
    print!("{:<10}", "quantile:");
    for q in qs {
        print!(" {:>7.2}", q);
    }
    println!();
    // The bake's era mask admits a cell only at or above this line (worldgen's
    // private FREEZE_C, mirrored), before any glacial cooling offset is added.
    const FREEZE_C: f64 = -10.0;
    let frozen = temps.iter().filter(|t| **t < FREEZE_C).count();
    println!(
        "below the -10C snowline TODAY: {frozen} of {} land cells ({:.1}%)",
        temps.len(),
        frozen as f64 / temps.len() as f64 * 100.0
    );

    // What the elevation curve alone yields across the real land range — the
    // curve, not the parameters (the constant-vs-curve rule).
    println!();
    let mut elevs: Vec<f64> = land.iter().map(|&c| substrate.get(c).elevation).collect();
    elevs.sort_by(f64::total_cmp);
    let bands = [0.05, 0.25, 0.50, 0.75, 0.95];
    print!("{:<10}", "elev m:");
    for q in bands {
        print!(" {:>9.0}", pct(&elevs, q));
    }
    println!();
    for (i, name) in SETTLERS.iter().enumerate() {
        let cn = &biosphere[i].condition_niche;
        print!("{name:<10}");
        for q in bands {
            print!(" {:>9.3}", cn.elevation.eval(pct(&elevs, q), 0.0));
        }
        println!();
    }
}

/// Documents the mirrored thresholds' derivation so a reader can check them
/// against `history_bake`'s constants by eye. They are not importable: the bake's
/// `GENESIS_POP` and `DAUGHTER_MAX_PRESSURE` are private, and a probe should not
/// force an API widening (`fallow_feasibility.rs` mirrors the same way).
#[test]
fn mirrored_viability_thresholds_are_self_consistent() {
    assert_eq!(SURVIVE_K, 5.0, "GENESIS_POP 10 / COLLAPSE_PRESSURE 2.0");
    assert!(
        (SPREAD_K - 14.2857).abs() < 1e-3,
        "GENESIS_POP 10 / DAUGHTER_MAX_PRESSURE 0.7"
    );
}
