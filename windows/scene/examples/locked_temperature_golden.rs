//! Throwaway: regenerate the orrery's locked-world temperature golden from
//! the current producer. Seed 8 is tidally locked (obliquity ~21.8°); its
//! substellar hot spot librates in latitude with the year phase (Task 3,
//! The Wandering Sun). Emits full-precision temperatures on a width-64
//! tiles lattice, sampled at one node per latitude row (node indices
//! `32, 96, 160, …` — the substellar-meridian longitude column, where the
//! libration signal is visible, every latitude band) x days spanning the
//! year. Values are evaluated at the equirect tile-CENTER position (the
//! same position the client reconstructs from width/height/i), NOT snapped
//! to the nearest climate cell the way `temperature_grid` samples — the
//! snap is exactly what the client's tile-center reconstruction cannot
//! reproduce (Task 7, ~1.1°C divergence). `locked_temperature_at_position`
//! is the shared, position-based evaluator both this golden and the client
//! now agree on. Mirrors the provenance recorded in
//! `region_temperature_golden.rs`. Run:
//!   cargo run -p hornvale-scene --example locked_temperature_golden
use hornvale_climate::locked_temperature_at_position;
use hornvale_kernel::{Seed, math};
use hornvale_scene::tiles_scene;
use hornvale_worldgen::{SkyChoice, build_world, climate_of};

/// Lattice width in tiles (height is `width / 2` = 32 latitude rows).
const WIDTH: u32 = 64;

/// Dry-adiabatic-ish lapse rate, °C lost per meter of elevation above sea
/// level — mirrors `LAPSE_C_PER_M` in `domains/climate/src/temperature.rs`
/// (a private constant there; this throwaway generator re-states its value
/// rather than widening that crate's public surface for it).
const LAPSE_C_PER_M: f64 = 6.5 / 1000.0;

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
    // The elevation/sea-level layers (for the per-tile lapse) — same
    // lattice, built once and reused across every day/node below.
    let scene = tiles_scene(&world, WIDTH).expect("tiles scene builds");
    let height = WIDTH / 2;
    let year = climate.year_length_std();
    let days = [0.0, year / 4.0, year / 2.0, 3.0 * year / 4.0];
    // One node per latitude row (stride 64 = WIDTH), offset to px = 32 so
    // the column sits on the substellar meridian (longitude ~0, the +x
    // axis `SUBSTELLAR` convention in `substellar.rs`) rather than the
    // antistellar one at px = 0 — the column where the hot spot actually
    // appears as it librates in latitude.
    const PX: usize = 32;
    let nodes: Vec<usize> = (0..height as usize)
        .map(|py| py * WIDTH as usize + PX)
        .collect();

    println!("# Producer-sourced golden for the locked-world (scene/tiles/v1 `locked: true`)");
    println!("# librating-substellar temperature reconstruction test.");
    println!("# Provenance: hornvale `locked_temperature_at_position` evaluated at equirect");
    println!("#   tile-CENTER positions — NOT snapped to the nearest climate cell — matching");
    println!("#   the client reconstruction exactly (Task 7, The Wandering Sun); world: seed");
    println!("#   8, generated sky, tidally locked (`hornvale new --seed 8`); lattice:");
    println!("#   width=64 (height=32); node set: px=32 (longitude ~0, the substellar");
    println!("#   meridian) across every latitude row — node indices 32, 96, 160, … — top row");
    println!("#   (lat ~+90) first.");
    println!("# days: {{0, year/4, year/2, 3*year/4}} where year = {year} std days");
    println!("#   (climate.year_length_std() for this world).");
    println!("# These are the sim's ground-truth temperatures (full precision); the orrery's");
    println!("# TS evaluator reconstructs the locked seasonal term from the librating");
    println!("# substellar point at the SAME tile-center position and must agree to ~1e-6");
    println!("# relative (quantization precision, decision 0033). This golden is");
    println!("# producer-sourced, NEVER a client reconstruction of itself (The Isotherm rule).");
    println!("# Regenerate: cargo run -p hornvale-scene --example locked_temperature_golden");
    println!("# columns: node_index,day,temperature_c");
    for &day in &days {
        for &node in &nodes {
            let py = node / WIDTH as usize;
            let px = node % WIDTH as usize;
            let latitude = 90.0 - (py as f64 + 0.5) / f64::from(height) * 180.0;
            let longitude = (px as f64 + 0.5) / f64::from(WIDTH) * 360.0 - 180.0;
            let lat_rad = latitude.to_radians();
            let lon_rad = longitude.to_radians();
            let p = [
                math::cos(lat_rad) * math::cos(lon_rad),
                math::cos(lat_rad) * math::sin(lon_rad),
                math::sin(lat_rad),
            ];
            let elevation_m = scene.elevation_m[node];
            let lapse = LAPSE_C_PER_M * (elevation_m - scene.sea_level_m).max(0.0);
            let t = locked_temperature_at_position(
                p,
                climate.insolation(),
                lapse,
                day,
                climate.obliquity_deg(),
                climate.year_phase_offset(),
                year,
            );
            println!("{node},{day},{t}");
        }
    }
}
