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

/// The shared world + stellar-input + biosphere setup both timings need.
#[allow(clippy::type_complexity)]
fn setup() -> (
    &'static hornvale_kernel::Geosphere,
    hornvale_terrain::GeneratedTerrain,
    hornvale_climate::GeneratedClimate,
    f64,
    f64,
    hornvale_climate::RotationRegime,
    Vec<&'static hornvale_species::BiosphereTraits>,
    &'static WorldComponents,
) {
    // Leaked deliberately: a probe process is short-lived, and leaking lets the
    // borrows outlive the setup frame without threading lifetimes through a
    // measurement harness.
    let wc: &'static WorldComponents = Box::leak(Box::new(
        WorldComponents::assemble().expect("components assemble"),
    ));
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
    let biosphere: Vec<&'static hornvale_species::BiosphereTraits> = SETTLERS
        .iter()
        .map(|n| {
            wc.biosphere
                .get(&hornvale_kernel::KindId(n))
                .expect("settler has biosphere traits")
        })
        .collect();
    let geo: &'static hornvale_kernel::Geosphere = Box::leak(Box::new(terrain.geosphere().clone()));
    (
        geo,
        terrain,
        climate,
        obliquity_deg,
        insolation_scalar,
        regime,
        biosphere,
        wc,
    )
}

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

/// **Plan task 1 for The Tense: is hoisting the era-invariant work worth it?**
///
/// §4 of the spec observes that `substrate_field` is 89% of
/// `per_species_capacity`'s cost and reasons that the era-invariant parts could
/// be hoisted out of a per-era loop. That was reasoning, not measurement. This
/// times each component so the *actual* era-invariant fraction is known before
/// anything is built — the go/no-go, because if the invariant share is small
/// then giving capacity an era axis costs the full 25x and the campaign has to
/// fall back to The Fallow's stock.
///
/// Which components move with the era, from what each function reads:
///
/// - ERA-VARYING: `substrate_field` (temperature/moisture/insolation all shift),
///   `carrying_capacity` (NPP is a climate function), `forage` (rides carrying),
///   `prey` (rides forage), `marine` (reads climate).
/// - ERA-INVARIANT: `mineral` and `detritus` (terrain only).
///
/// **This granularity is too coarse and its "0% invariant" answer is
/// misleading** — see `where_substrate_cost_lives_and_whether_latitudes_repeat`,
/// which decomposes `substrate_field` itself and finds the opposite. Kept because
/// the whole-function numbers are still the right denominator, and because the
/// coarse answer is what a reasonable person would have assumed.
#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn era_invariant_fraction_of_capacity_cost() {
    let (geo, terrain, climate, obliquity_deg, insolation_scalar, regime, _biosphere, _wc) =
        setup();

    macro_rules! timed {
        ($label:expr, $e:expr) => {{
            #[allow(clippy::disallowed_types)] // benchmark harness, not sim logic
            let t = Instant::now();
            let v = $e;
            let ms = t.elapsed().as_secs_f64() * 1000.0;
            std::hint::black_box(&v);
            ($label, ms, v)
        }};
    }

    let (_, t_substrate, substrate) = timed!(
        "substrate_field",
        substrate_field(
            geo,
            &terrain,
            &climate,
            obliquity_deg,
            insolation_scalar,
            &regime
        )
    );
    let (_, t_carrying, carrying) = timed!(
        "carrying_capacity",
        hornvale_demography::carrying_capacity(
            geo,
            &hornvale_worldgen::carrying_inputs_of(geo, &terrain, &climate)
        )
    );
    let base = carrying.as_cell_map();
    let (_, t_forage, forage) = timed!(
        "forage_supply_field",
        hornvale_worldgen::forage_supply_field(geo, base)
    );
    let (_, t_prey, _prey) = timed!(
        "prey_supply_field",
        hornvale_worldgen::prey_supply_field(geo, &forage)
    );
    let (_, t_marine, _marine) = timed!(
        "marine_forage_supply_field",
        hornvale_worldgen::marine_forage_supply_field(geo, &terrain, &climate, 1.0)
    );
    let (_, t_mineral, _mineral) = timed!(
        "mineral_supply_field",
        hornvale_worldgen::mineral_supply_field(geo, &terrain, 1.0)
    );
    let (_, t_detritus, _detritus) = timed!(
        "detritus_supply_field",
        hornvale_worldgen::detritus_supply_field(geo, &terrain)
    );
    std::hint::black_box(&substrate);

    let varying = t_substrate + t_carrying + t_forage + t_prey + t_marine;
    let invariant = t_mineral + t_detritus;
    let total = varying + invariant;

    println!("component                     ms      era?");
    println!("substrate_field         {t_substrate:>8.1}   VARYING");
    println!("carrying_capacity       {t_carrying:>8.1}   VARYING");
    println!("forage_supply_field     {t_forage:>8.1}   VARYING");
    println!("prey_supply_field       {t_prey:>8.1}   VARYING");
    println!("marine_forage_supply    {t_marine:>8.1}   VARYING");
    println!("mineral_supply_field    {t_mineral:>8.1}   invariant");
    println!("detritus_supply_field   {t_detritus:>8.1}   invariant");
    println!("--");
    println!(
        "era-VARYING total       {varying:>8.1} ms  ({:.0}%)",
        varying / total * 100.0
    );
    println!(
        "era-invariant total     {invariant:>8.1} ms  ({:.0}%)",
        invariant / total * 100.0
    );
    println!(
        "naive x{CLIMATE_ERAS}:              {:>8.1} ms",
        total * CLIMATE_ERAS as f64
    );
    println!(
        "hoisted (invariant once): {:>8.1} ms   -> saves {:.0}%",
        invariant + varying * CLIMATE_ERAS as f64,
        (1.0 - (invariant + varying * CLIMATE_ERAS as f64) / (total * CLIMATE_ERAS as f64)) * 100.0
    );
}

/// Where `substrate_field`'s cost actually is, and whether it is avoidable
/// WITHOUT changing a single output byte.
///
/// `annual_mean_insolation` integrates 48 orbital samples with ~9 libm
/// transcendentals each -- ~430 per cell, ~17.6M per field. It is a pure
/// function of `(latitude, obliquity, insolation_scalar)`, and the latter two are
/// constant within a world. So if cells SHARE latitudes exactly, the integration
/// can be memoised on the exact bit pattern: same inputs, same libm calls, same
/// result, byte-identical output and no epoch.
#[test]
#[ignore = "probe: measurement only, run explicitly"]
fn where_substrate_cost_lives_and_whether_latitudes_repeat() {
    let (geo, terrain, climate, obliquity_deg, insolation_scalar, _regime, _bio, _wc) = setup();

    macro_rules! ms {
        ($e:expr) => {{
            #[allow(clippy::disallowed_types)] // benchmark harness, not sim logic
            let t = Instant::now();
            let v = $e;
            std::hint::black_box(&v);
            t.elapsed().as_secs_f64() * 1000.0
        }};
    }

    let t_temp = ms!(hornvale_kernel::CellMap::from_fn(geo, |c| climate
        .mean_temperature_at(c)
        .get()));
    let t_moist = ms!(hornvale_kernel::CellMap::from_fn(geo, |c| climate.moisture_at(c)));
    let t_insol = ms!(hornvale_kernel::CellMap::from_fn(geo, |c| {
        hornvale_worldgen::annual_mean_insolation(
            geo.coord(c).latitude,
            obliquity_deg,
            insolation_scalar,
        )
    }));
    let sea = terrain.sea_level();
    let t_elev = ms!(hornvale_kernel::CellMap::from_fn(geo, |c| terrain
        .elevation_at(c)
        - sea));

    println!("substrate component      ms");
    println!("  temperature      {t_temp:>9.1}");
    println!("  moisture         {t_moist:>9.1}");
    println!("  INSOLATION       {t_insol:>9.1}");
    println!("  elevation        {t_elev:>9.1}");

    // Do latitudes repeat exactly? Keyed on the bit pattern, so "exactly" means
    // bit-for-bit -- the only kind of sharing a byte-identity guarantee allows.
    let mut lats: Vec<u64> = geo
        .cells()
        .map(|c| geo.coord(c).latitude.to_bits())
        .collect();
    let total = lats.len();
    lats.sort_unstable();
    lats.dedup();
    println!("--");
    println!("cells                  {total}");
    println!("distinct latitudes     {}", lats.len());
    println!(
        "memoisation ratio      {:.1}x   (exact, byte-identical)",
        total as f64 / lats.len() as f64
    );
}
