//! THE TENSE §3.3 — the temperature gate in **shadow mode**. Measurement only.
//!
//! Step 4 of the plan deletes the era mask's thermal half. Before that, the
//! replacement has to be characterised, because the failure mode is silent: if
//! the gate excludes far *less* than the mask did, the cutover makes every world
//! more permissive and nothing turns red; if far *more*, worlds collapse and the
//! cause is buried under a rebaseline.
//!
//! The move is borrowed from systems that swap load-bearing parts for a living.
//! Aviation runs a new system in parallel — computing, logged, compared, not
//! acted on — until its disagreement with the incumbent is known. Double-entry
//! bookkeeping refuses to delete an account with a balance: the balance must
//! land somewhere, and a balance that vanishes is an error by construction.
//! **Exclusion is that balance here.** This measures it on both sides.
//!
//! Two questions, and they are not the same:
//!
//! 1. **Extent** — how much land does each rule exclude?
//! 2. **Agreement** — do they exclude the *same* land? Two rules can exclude 40%
//!    each and disagree about all of it.

// `terrain_of` is a named derivation entry point (decision 0092); a probe
// measuring a handful of worlds is the site the allowance is for.
#![allow(clippy::disallowed_methods)]

use hornvale_kernel::sovereignty_floor;
use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::{
    EraAdjust, EraInvariantSupply, SettlementPins, SkyChoice, build_world, climate_of,
    per_species_capacity_at, sky_of, substrate_field_at, terrain_of, tolerance_tiered,
};

const SETTLERS: [&str; 6] = ["kobold", "goblin", "hobgoblin", "bugbear", "gnoll", "human"];

/// `bake_eras`'s snowline, mirrored: the era mask admits a cell iff its
/// era-adjusted mean temperature is at or above this.
const FREEZE_C: f64 = -10.0;

/// `GENESIS_POP / COLLAPSE_PRESSURE` — the capacity a founding needs to survive.
/// "Excluded" for a continuous field means "cannot seat anyone", not "exactly
/// zero": an unfloored Gaussian underflows to zero only far past the point where
/// it has stopped supporting life.
const SURVIVE_K: f64 = 5.0;

/// The era offsets to sweep. `bake_eras` derives these from orbital forcing; a
/// fixed sweep is used instead so the readout is about the RULES rather than
/// about one world's particular glacial history.
const OFFSETS_C: [f64; 4] = [0.0, -3.0, -6.0, -9.0];

#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn temperature_gate_versus_era_mask() {
    let wc = WorldComponents::assemble().expect("components assemble");

    println!(
        "{:<8} {:>6} {:>10} {:>10} {:>10} {:>10} {:>10}",
        "seed", "dT", "mask-excl", "gate-excl", "both", "mask-only", "gate-only"
    );
    for seed in [42, 7, 1234] {
        let world = build_world(
            hornvale_kernel::Seed(seed),
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed builds");
        let terrain = terrain_of(&world).expect("terrain");
        let climate = climate_of(&world).expect("climate");
        let geo = terrain.geosphere();
        let sky = sky_of(&world).expect("sky");
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
        let biosphere: Vec<&hornvale_species::BiosphereTraits> = SETTLERS
            .iter()
            .map(|n| {
                wc.biosphere
                    .get(&hornvale_kernel::KindId(n))
                    .expect("settler has biosphere traits")
            })
            .collect();
        let hoisted = EraInvariantSupply::build(
            geo,
            &terrain,
            &climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
        );

        for dt in OFFSETS_C {
            let adjust = EraAdjust {
                temp_offset: hornvale_kernel::TempAnomaly::from_offset_c(dt),
                sea_level: terrain.sea_level(),
            };
            let substrate =
                substrate_field_at(geo, &terrain, &climate, &hoisted.insolation, &adjust);
            let caps =
                per_species_capacity_at(geo, &terrain, &climate, &hoisted, &adjust, &biosphere);

            // Land at this era. Ocean is excluded by capacity already (proven in
            // `era_substrate.rs`), so counting it here would drown the signal.
            let land: Vec<hornvale_kernel::CellId> = geo
                .cells()
                .filter(|&c| terrain.elevation_at(c) >= adjust.sea_level)
                .collect();

            // MASK: the incumbent. Excludes a cell for everyone, on temperature.
            // GATE: the successor. A cell is "excluded" when NO settling species
            // can seat a founding on it -- the same question the mask answers,
            // asked of capacity instead.
            let (mut mask_only, mut gate_only, mut both, mut mask_n, mut gate_n) =
                (0usize, 0usize, 0usize, 0usize, 0usize);
            for &c in &land {
                let masked = substrate.get(c).temperature_c < FREEZE_C;
                let gated = caps.iter().all(|(_, cap)| cap.at(c) < SURVIVE_K);
                match (masked, gated) {
                    (true, true) => {
                        both += 1;
                        mask_n += 1;
                        gate_n += 1;
                    }
                    (true, false) => {
                        mask_only += 1;
                        mask_n += 1;
                    }
                    (false, true) => {
                        gate_only += 1;
                        gate_n += 1;
                    }
                    (false, false) => {}
                }
            }
            let pc = |n: usize| n as f64 / land.len() as f64 * 100.0;
            println!(
                "{:<8} {:>6.1} {:>9.1}% {:>9.1}% {:>9.1}% {:>9.1}% {:>9.1}%",
                seed,
                dt,
                pc(mask_n),
                pc(gate_n),
                pc(both),
                pc(mask_only),
                pc(gate_only)
            );
        }
    }
    println!();
    println!("mask-only = the mask kills it, capacity would not  -> cutover makes these LIVE");
    println!("gate-only = capacity kills it, the mask would not   -> already dead by capacity");
}

/// Would promoting moisture to a gate change the picture? The spec hedges
/// ("moisture is probably a gate"), so it is measured rather than decided.
///
/// Reported as the share of land whose tolerance the moisture promotion would
/// drive under the survivable bar — i.e. the extra exclusion it buys.
#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn would_moisture_as_a_gate_add_exclusion() {
    let wc = WorldComponents::assemble().expect("components assemble");
    let world = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let terrain = terrain_of(&world).expect("terrain");
    let climate = climate_of(&world).expect("climate");
    let geo = terrain.geosphere();
    let sky = sky_of(&world).expect("sky");
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
    let hoisted = EraInvariantSupply::build(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
    );
    let adjust = EraAdjust {
        temp_offset: hornvale_kernel::TempAnomaly::from_offset_c(0.0),
        sea_level: terrain.sea_level(),
    };
    let substrate = substrate_field_at(geo, &terrain, &climate, &hoisted.insolation, &adjust);
    let land: Vec<hornvale_kernel::CellId> =
        geo.cells().filter(|&c| !terrain.is_ocean(c)).collect();

    println!(
        "{:<10} {:>14} {:>16} {:>14}",
        "species", "temp gate only", "temp+moist gate", "extra excluded"
    );
    for name in SETTLERS {
        let bio = wc
            .biosphere
            .get(&hornvale_kernel::KindId(name))
            .expect("settler has biosphere traits");
        let floor = sovereignty_floor(bio.mass, bio.potency);
        let cn = &bio.condition_niche;
        // Tolerance below this leaves no cell able to seat a founding even at
        // the best supply on the map, so it is a fair proxy for "gated out".
        let bar = 0.05;
        let (mut t_only, mut t_moist) = (0usize, 0usize);
        for &c in &land {
            let s = substrate.get(c);
            if tolerance_tiered(cn, s, floor) < bar {
                t_only += 1;
            }
            let gate =
                cn.temperature.eval(s.temperature_c, 0.0) * cn.moisture.eval(s.moisture, 0.0);
            let modifier = cn
                .insolation
                .eval(s.insolation, floor)
                .min(cn.elevation.eval(s.height_asl_m.get(), floor));
            if gate * modifier < bar {
                t_moist += 1;
            }
        }
        let pc = |n: usize| n as f64 / land.len() as f64 * 100.0;
        println!(
            "{:<10} {:>13.1}% {:>15.1}% {:>13.1}%",
            name,
            pc(t_only),
            pc(t_moist),
            pc(t_moist) - pc(t_only)
        );
    }
}
