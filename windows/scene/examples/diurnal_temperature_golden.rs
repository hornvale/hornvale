//! Throwaway: regenerate the orrery's diurnal-temperature golden from the
//! current producer. Seed 42's built system supplies the real obliquity,
//! year length, rotation-day, and year-phase inputs `diurnal_waveform`
//! needs; this golden pins the orrery's upcoming TypeScript port of that
//! pure function (Task 5, The Turning) against the Rust ground truth,
//! evaluated at the SAME point the client uses: equirect tile-CENTER
//! coordinates (the Wandering Sun lesson — same formula AND same point;
//! see `locked_temperature_golden.rs`, which this mirrors). Emits
//! `{lat_deg, lon_deg, day_fraction, amplitude_c, diurnal_c}` rows for 8
//! tile-center coordinates (7 land tiles spanning the driest interiors at
//! several latitudes, plus one ocean tile with amplitude_c ~= 0) x 6
//! day-fraction samples spanning a rotation, all at a fixed integer day
//! (day = 0, so `year_phase == climate.year_phase_offset()` exactly). This
//! golden is producer-sourced, NEVER a client reconstruction of itself (The
//! Isotherm rule). Run:
//!   cargo run -p hornvale-scene --example diurnal_temperature_golden
use hornvale_climate::{RotationRegime, diurnal_waveform};
use hornvale_kernel::{NearestCellIndex, Seed};
use hornvale_worldgen::{SkyChoice, build_world, climate_of, terrain_of};
use serde::Serialize;

/// Lattice width (tiles across) used only to derive realistic tile-CENTER
/// coordinates — the same equirect formula `scene/tiles/v1` uses (height =
/// width / 2).
const WIDTH: u32 = 64;

/// The `day_fraction` samples printed for every coordinate: spans a full
/// rotation, including pre-dawn, the afternoon peak (`PEAK_FRAC = 0.60`),
/// and back toward pre-dawn.
const DAY_FRACTIONS: [f64; 6] = [0.0, 0.15, 0.30, 0.45, 0.60, 0.75];

/// The fixed integer day this golden is evaluated at.
const DAY: f64 = 0.0;

/// How many driest-interior land tiles to sample (spread across the sorted
/// list so latitudes vary, not just the single hottest desert).
const LAND_PICKS: usize = 7;

/// One golden row: the client's TS `diurnalWaveform` port must reproduce
/// `diurnal_c` from `amplitude_c * diurnalWaveform(lat_deg, lon_deg,
/// obliquity_deg, year_phase, day_fraction, day_length_std)` (obliquity/
/// year_phase/day_length_std are fixed per-run system constants documented
/// in the header comment above, not repeated per row).
#[derive(Serialize)]
struct Row {
    lat_deg: f64,
    lon_deg: f64,
    day_fraction: f64,
    amplitude_c: f64,
    diurnal_c: f64,
}

/// A tile-center sample: its coordinate, the climate's diurnal half-range
/// amplitude there, and whether the underlying terrain cell is ocean.
struct Coord {
    latitude: f64,
    longitude: f64,
    amplitude_c: f64,
    is_ocean: bool,
}

fn main() {
    let world = build_world(
        Seed(42),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds");
    let climate = climate_of(&world).expect("seed 42 climate builds");
    let RotationRegime::Spinning { day_std } = climate.regime() else {
        panic!("seed 42 (generated sky, default rotation pin) must be spinning");
    };
    let terrain = terrain_of(&world).expect("seed 42 terrain builds");
    let obliquity_deg = climate.obliquity_deg();
    let year_phase =
        (DAY / climate.year_length_std() + climate.year_phase_offset()).rem_euclid(1.0);

    let height = WIDTH / 2;
    let terrain_index = NearestCellIndex::new(terrain.geosphere());
    let climate_index = NearestCellIndex::new(climate.geosphere());

    // Scan the full lattice once, at tile-CENTER coordinates (matching
    // `scene/tiles/v1`'s own formula exactly), collecting each tile's
    // diurnal amplitude and ocean status.
    let mut coords = Vec::with_capacity((WIDTH * height) as usize);
    for py in 0..height {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
        for px in 0..WIDTH {
            let longitude = (f64::from(px) + 0.5) / f64::from(WIDTH) * 360.0 - 180.0;
            let t_cell = terrain_index.nearest(terrain.geosphere(), latitude, longitude);
            let c_cell = climate_index.nearest(climate.geosphere(), latitude, longitude);
            coords.push(Coord {
                latitude,
                longitude,
                amplitude_c: climate.diurnal_amp_at(c_cell),
                is_ocean: terrain.is_ocean(t_cell),
            });
        }
    }

    // The driest-interior land tiles, descending by amplitude.
    let mut land: Vec<&Coord> = coords.iter().filter(|c| !c.is_ocean).collect();
    land.sort_by(|a, b| b.amplitude_c.total_cmp(&a.amplitude_c));
    // The open ocean: the ocean tile with the SMALLEST amplitude (farthest
    // from any coast, so continentality ~= 0) — not merely the first ocean
    // tile the scan happens to hit, which may sit near a shoreline and
    // carry a non-trivial amplitude.
    let ocean = coords
        .iter()
        .filter(|c| c.is_ocean)
        .min_by(|a, b| a.amplitude_c.total_cmp(&b.amplitude_c))
        .expect("seed 42 has an ocean tile");

    let mut picks: Vec<&Coord> = Vec::new();
    let stride = (land.len() / LAND_PICKS).max(1);
    for i in 0..LAND_PICKS {
        if let Some(c) = land.get(i * stride) {
            picks.push(c);
        }
    }
    picks.push(ocean);

    let mut rows = Vec::new();
    for coord in &picks {
        for &day_fraction in &DAY_FRACTIONS {
            let d = diurnal_waveform(
                coord.latitude,
                coord.longitude,
                obliquity_deg,
                year_phase,
                day_fraction,
                day_std,
            );
            rows.push(Row {
                lat_deg: coord.latitude,
                lon_deg: coord.longitude,
                day_fraction,
                amplitude_c: coord.amplitude_c,
                diurnal_c: coord.amplitude_c * d,
            });
        }
    }

    println!(
        "{}",
        serde_json::to_string_pretty(&rows).expect("rows serialize")
    );
}
