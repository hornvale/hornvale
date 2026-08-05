//! Cost probe for the era-varying capacity design. Measurement only.
//!
//! The proposed change makes capacity a function of `(species, cell, era)`
//! rather than `(species, cell)`. `CLIMATE_ERAS` is 25, so the naive reading is
//! "25x the work", and the design's central risk is whether that is affordable
//! inside a ~2000-world census.
//!
//! This times the pieces rather than assuming. Wall-clock is banned
//! workspace-wide (`clippy.toml` `disallowed-types`, decision 0001) and the lint
//! caught this file on its first commit; the scoped `allow`s below are the
//! sanctioned exception, matching `hornvale_worldgen::profiled` and
//! `cli/tests/graph_cost.rs`. Nothing here enters world state.
//!
//! **Read the output as CORE-seconds.** `windows/lab/src/runner.rs` parallelises
//! a study over seeds with `available_parallelism()`, so a census divides this by
//! the box's core count — and censuses run on lefford, not on whatever machine
//! this probe was last run on. A per-world number here is not a census number.

#![allow(clippy::disallowed_methods)]

use hornvale_worldgen::components::WorldComponents;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, build_world, climate_of, per_species_capacity, sky_of,
    substrate_field, terrain_of,
};
// A benchmark harness measuring the cost of a derivation, not sim logic: it
// never reads `WorldTime`, never touches a fact, and never reaches an artifact,
// so it carries the same wall-clock exemption `hornvale_worldgen::profiled` and
// `cli/tests/graph_cost.rs` do (clippy.toml / decision 0001).
#[allow(clippy::disallowed_types)]
use std::time::Instant;

const SETTLERS: [&str; 6] = ["kobold", "goblin", "hobgoblin", "bugbear", "gnoll", "human"];

/// `worldgen::CLIMATE_ERAS`, private, mirrored (probe convention).
const CLIMATE_ERAS: usize = 25;

#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn cost_of_making_capacity_era_varying() {
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
    let biosphere: Vec<&hornvale_species::BiosphereTraits> = SETTLERS
        .iter()
        .map(|n| {
            wc.biosphere
                .get(&hornvale_kernel::KindId(n))
                .expect("settler has biosphere traits")
        })
        .collect();

    #[allow(clippy::disallowed_types)] // benchmark harness, not sim logic
    let t0 = Instant::now();
    let substrate = substrate_field(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
    );
    let substrate_ms = t0.elapsed().as_secs_f64() * 1000.0;
    std::hint::black_box(&substrate);

    #[allow(clippy::disallowed_types)] // benchmark harness, not sim logic
    let t1 = Instant::now();
    let caps = per_species_capacity(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &biosphere,
    );
    let caps_ms = t1.elapsed().as_secs_f64() * 1000.0;
    std::hint::black_box(&caps);

    // Memory, if every era's field were held at once rather than streamed.
    let cells = geo.cell_count();
    let bytes_one = cells * SETTLERS.len() * std::mem::size_of::<f64>();
    let bytes_all = bytes_one * CLIMATE_ERAS;

    println!("cells                    {cells}");
    println!("species                  {}", SETTLERS.len());
    println!("CLIMATE_ERAS             {CLIMATE_ERAS}");
    println!("substrate_field          {substrate_ms:.1} ms");
    println!(
        "per_species_capacity     {caps_ms:.1} ms   (includes its own substrate + 5 supply fields)"
    );
    println!(
        "  x{CLIMATE_ERAS} eras            {:.1} ms  = {:.2} s per world",
        caps_ms * CLIMATE_ERAS as f64,
        caps_ms * CLIMATE_ERAS as f64 / 1000.0
    );
    println!("memory, one era          {:.1} MB", bytes_one as f64 / 1e6);
    println!(
        "memory, all eras resident {:.1} MB  (streaming one era at a time avoids this)",
        bytes_all as f64 / 1e6
    );
    println!(
        "at 300 settling species, one era: {:.1} MB",
        (cells * 300 * std::mem::size_of::<f64>()) as f64 / 1e6
    );
}
