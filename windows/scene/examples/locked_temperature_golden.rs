//! Throwaway: regenerate the orrery's locked-world temperature golden from
//! the current producer. Seed 8 is tidally locked (obliquity ~21.8°); its
//! substellar hot spot librates in latitude with the year phase (Task 3,
//! The Wandering Sun). Emits full-precision `temperature_grid` values on a
//! width-64 tiles lattice, sampled at one node per latitude row (node
//! indices `32, 96, 160, …` — the substellar-meridian longitude column,
//! where the libration signal is visible, every latitude band) x days
//! spanning the year. Mirrors the provenance recorded
//! in `region_temperature_golden.rs`. Run:
//!   cargo run -p hornvale-scene --example locked_temperature_golden
use hornvale_kernel::Seed;
use hornvale_scene::temperature_grid;
use hornvale_worldgen::{SkyChoice, build_world, climate_of};

/// Lattice width in tiles (height is `width / 2` = 32 latitude rows).
const WIDTH: u32 = 64;

fn main() {
    let world = build_world(
        Seed(8),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 8 builds");
    let climate = climate_of(&world).expect("seed 8 climate builds");
    assert!(climate.is_locked(), "seed 8 must be tidally locked");
    let year = climate.year_length_std();
    let days = [0.0, year / 4.0, year / 2.0, 3.0 * year / 4.0];
    // One node per latitude row (stride 64 = WIDTH), offset to px = 32 so
    // the column sits on the substellar meridian (longitude ~0, the +x
    // axis `SUBSTELLAR` convention in `substellar.rs`) rather than the
    // antistellar one at px = 0 — the column where the hot spot actually
    // appears as it librates in latitude.
    const PX: usize = 32;
    let nodes: Vec<usize> = (0..(WIDTH / 2) as usize)
        .map(|py| py * WIDTH as usize + PX)
        .collect();

    println!("# Producer-sourced golden for the locked-world (scene/tiles/v1 `locked: true`)");
    println!("# librating-substellar temperature reconstruction test.");
    println!("# Provenance: hornvale `temperature_grid` (`temperature_at` sampled on the");
    println!("#   scene/tiles/v1 lattice) — world: seed 8, generated sky, tidally locked");
    println!("#   (`hornvale new --seed 8`); lattice: width=64 (height=32); node set: px=32");
    println!("#   (longitude ~0, the substellar meridian) across every latitude row — node");
    println!("#   indices 32, 96, 160, … — top row (lat ~+90) first.");
    println!("# days: {{0, year/4, year/2, 3*year/4}} where year = {year} std days");
    println!("#   (climate.year_length_std() for this world).");
    println!("# These are the sim's ground-truth temperatures (full precision); the orrery's");
    println!("# TS evaluator reconstructs the locked seasonal term from the librating");
    println!("# substellar point and must agree to ~1e-6 relative (quantization precision,");
    println!("# decision 0033). This golden is producer-sourced, NEVER a client");
    println!("# reconstruction of itself (The Isotherm rule).");
    println!("# Regenerate: cargo run -p hornvale-scene --example locked_temperature_golden");
    println!("# columns: node_index,day,temperature_c");
    for &day in &days {
        let grid = temperature_grid(&world, WIDTH, day).expect("temperature grid builds");
        for &node in &nodes {
            println!("{},{},{}", node, day, grid[node]);
        }
    }
}
