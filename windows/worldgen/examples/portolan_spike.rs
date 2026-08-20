//! THROWAWAY SPIKE (The Portolan). Answers one question before anyone
//! designs a salience gate for the Gazetteer's map labels: how bad is the
//! label-legibility problem if every one of seed 42's 405 features is drawn
//! on an ASCII Mercator map with **no gate at all**? This file is not a
//! design and ships no interface anyone should call -- see
//! `.superpowers/sdd/the-portolan-spike-report.md` for the write-up.
//!
//! Run: `cargo run -p hornvale-worldgen --example portolan_spike`

use hornvale_kernel::{CellId, Geosphere, Seed, math};
use hornvale_language::{Envelope, ExoticSeg, MorphOptions, draw_phonology};
use hornvale_terrain::landscape::{Feature, FeatureClass};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::{feature_name, gazetteer_features};
use std::collections::BTreeMap;

/// Mercator's standard pole clamp: the projection diverges at +/-90 deg, so
/// latitude is clamped to +/-85 deg before projecting (stated here and in
/// the report, per the spike's brief).
const LAT_CLAMP_DEG: f64 = 85.0;

/// Terminal glyphs read roughly twice as tall as they are wide; used only
/// to pick a height that does not look vertically stretched, not for
/// anything geodetic.
const GLYPH_ASPECT: f64 = 2.0;

/// One people's name, chosen arbitrarily (this spike draws exactly one, not
/// all 15 -- drawing every people's name for every feature would multiply
/// the label count and answer a different question). `"aeldrin"` is the
/// species this crate's own gazetteer/volcano test fixtures already use.
const PEOPLE: &str = "aeldrin";

/// One named, positioned feature: its class, drawn label text, and lat/lon
/// (degrees) at its anchor cell.
struct Named {
    class: FeatureClass,
    text: String,
    lat: f64,
    lon: f64,
}

fn morph() -> MorphOptions {
    MorphOptions {
        honorifics: false,
        shape_weights: [1.0, 1.0, 1.0],
        shape_beta: 1.0,
    }
}

/// Mercator y (unnormalized) at a clamped latitude, degrees.
fn mercator_y(lat_deg: f64) -> f64 {
    let lat = lat_deg.clamp(-LAT_CLAMP_DEG, LAT_CLAMP_DEG);
    let lat_rad = lat.to_radians();
    math::ln(math::tan(std::f64::consts::FRAC_PI_4 + lat_rad / 2.0))
}

/// `mercator_y` at the clamp latitude -- the half-height of the projected
/// range, used to normalize.
fn mercator_y_max() -> f64 {
    mercator_y(LAT_CLAMP_DEG)
}

/// Height (rows) for a given width (columns) under the Mercator aspect,
/// corrected for terminal glyph aspect. Derivation:
///
/// Mercator projects longitude linearly in radians (x range `2*pi` over
/// 360 deg) and latitude as `y = ln(tan(pi/4 + lat/2))` (y range
/// `2*mercator_y_max()` over the clamped band). For `width` columns to span
/// the x range without on-screen distortion, given a glyph that reads
/// `GLYPH_ASPECT` times taller than wide:
///
/// `height = width * mercator_y_max() / (pi * GLYPH_ASPECT)`
fn height_for_width(width: u32) -> u32 {
    let y_max = mercator_y_max();
    let h = f64::from(width) * y_max / (std::f64::consts::PI * GLYPH_ASPECT);
    h.round() as u32
}

/// Forward-project a lat/lon (degrees) to a (row, col) cell on a
/// `width`x`height` Mercator grid. Longitude wraps; latitude is clamped
/// before projecting (so a feature beyond the clamp band still lands at the
/// map's top/bottom edge rather than being dropped).
fn project(lat_deg: f64, lon_deg: f64, width: u32, height: u32) -> (usize, usize) {
    let lon = lon_deg.rem_euclid(360.0);
    let col = ((lon / 360.0) * f64::from(width)).floor() as i64;
    let col = col.rem_euclid(i64::from(width)) as usize;

    let y = mercator_y(lat_deg);
    let y_max = mercator_y_max();
    let row = ((y_max - y) / (2.0 * y_max) * f64::from(height)).floor();
    let row = row.clamp(0.0, f64::from(height) - 1.0) as usize;
    (row, col)
}

/// Inverse-project a (row, col) grid cell back to a lat/lon sample point --
/// used only to paint the terrain backdrop (land vs water), never for
/// feature placement (which uses the feature's own anchor coordinate).
fn unproject(row: usize, col: usize, width: u32, height: u32) -> (f64, f64) {
    let lon = (f64::from(col as u32) + 0.5) / f64::from(width) * 360.0 - 180.0;
    let y_max = mercator_y_max();
    let y = y_max - (f64::from(row as u32) + 0.5) / f64::from(height) * (2.0 * y_max);
    let lat_rad = 2.0 * math::atan(math::exp(y)) - std::f64::consts::FRAC_PI_2;
    (lat_rad.to_degrees(), lon)
}

fn main() {
    let level = hornvale_terrain::GLOBE_LEVEL;
    let seed = Seed(42);
    let geo = Geosphere::new(level);
    let outcome = hornvale_terrain::generate(seed, &geo, &TerrainPins::default())
        .expect("default pins generate seed 42");
    let terrain = GeneratedTerrain::new(geo.clone(), outcome);

    let features: Vec<Feature> = gazetteer_features(seed, &geo, &terrain);
    println!("The Portolan spike -- seed 42, GLOBE_LEVEL {level}, people {PEOPLE:?}");
    println!("total features: {}", features.len());
    let mut by_class: BTreeMap<FeatureClass, usize> = BTreeMap::new();
    for f in &features {
        *by_class.entry(f.id.class).or_default() += 1;
    }
    for (class, n) in &by_class {
        println!("  {class:?}: {n}");
    }
    println!(
        "mercator_y_max = {:.6}, so height = round(width * {:.6} / pi / {GLYPH_ASPECT})",
        mercator_y_max(),
        mercator_y_max()
    );

    let ph = draw_phonology(
        &Seed(7),
        PEOPLE,
        &Envelope {
            labiality: 1.0,
            vowel_space: 1.0,
            voicing: 1.0,
            sibilance: 1.0,
            voice_loudness: 1.0,
            tonality: 0.0,
            exotic: ExoticSeg::None,
        },
        &hornvale_language::typology::concatenative(),
    );
    let morph = morph();

    // Draw order: `gazetteer_features`'s own order -- volcanoes (grouped by
    // source cell id ascending, not sorted by magnitude), then Landmass,
    // Sea, SaltLake, River (each already magnitude-descending /
    // identity-ascending from `FeatureIndex`). "Who wins a collision" below
    // depends on this order.
    let named: Vec<Named> = features
        .iter()
        .map(|f| {
            let text = feature_name(seed, f.id, PEOPLE, &ph, &morph).roman;
            let coord = geo.coord(f.anchor);
            Named {
                class: f.id.class,
                text,
                lat: coord.latitude,
                lon: coord.longitude,
            }
        })
        .collect();

    for width in [72u32, 144, 288] {
        let height = height_for_width(width);
        render_zoom(&terrain, width, height, &named);
    }
}

fn glyph_for(terrain: &GeneratedTerrain, lat: f64, lon: f64) -> char {
    let cell: CellId = terrain.nearest_cell(lat, lon);
    if terrain.is_ocean(cell) { '~' } else { '.' }
}

fn render_zoom(terrain: &GeneratedTerrain, width: u32, height: u32, named: &[Named]) {
    println!();
    println!("=== zoom {width}x{height} (w={width} h={height}) ===");

    let w = width as usize;
    let h = height as usize;

    // Terrain backdrop: one glyph per cell, sampled at each grid cell's
    // inverse-projected lat/lon (the same "sample the pixel's lat/lon
    // center" idiom `domains/terrain/src/render.rs::rasterize` uses, just
    // under the Mercator inverse instead of the equirectangular one).
    let mut grid: Vec<Vec<char>> = (0..h)
        .map(|row| {
            (0..w)
                .map(|col| {
                    let (lat, lon) = unproject(row, col, width, height);
                    glyph_for(terrain, lat, lon)
                })
                .collect()
        })
        .collect();

    // Demand map: for every character cell, how many DISTINCT features'
    // labels claim it -- computed from each feature's own anchor position,
    // independent of draw order. This is the "how many labels want the
    // same cell" measurement.
    let mut demand: BTreeMap<(usize, usize), u32> = BTreeMap::new();
    // Per-feature cell spans, kept alongside so the placement pass below
    // does not recompute projection twice.
    let mut spans: Vec<(usize, Vec<(usize, usize)>)> = Vec::with_capacity(named.len());
    for (i, f) in named.iter().enumerate() {
        let (row, col) = project(f.lat, f.lon, width, height);
        let cells: Vec<(usize, usize)> = (0..f.text.chars().count())
            .map(|k| (row, (col + k) % w))
            .collect();
        for &c in &cells {
            *demand.entry(c).or_default() += 1;
        }
        spans.push((i, cells));
    }

    // Placement pass: draw order = `named`'s own order (see main()'s
    // comment). A label is "clean" if none of its cells were already
    // occupied by an earlier label; occupied or not, it is drawn anyway
    // (no gate, no collision avoidance) and overwrites whatever was there.
    let mut occupied: Vec<Vec<bool>> = vec![vec![false; w]; h];
    let mut clean_by_class: BTreeMap<FeatureClass, (u32, u32)> = BTreeMap::new(); // (clean, total)
    let mut clean_volcanoes = 0u32;
    let mut total_volcanoes = 0u32;

    for (i, cells) in &spans {
        let f = &named[*i];
        let entry = clean_by_class.entry(f.class).or_insert((0, 0));
        entry.1 += 1;
        if f.class == FeatureClass::Volcano {
            total_volcanoes += 1;
        }
        let clean = cells.iter().all(|&(r, c)| !occupied[r][c]);
        if clean {
            entry.0 += 1;
            if f.class == FeatureClass::Volcano {
                clean_volcanoes += 1;
            }
        }
        for &(r, c) in cells {
            occupied[r][c] = true;
        }
        for (k, ch) in f.text.chars().enumerate() {
            let (r, c) = cells[k];
            grid[r][c] = ch;
        }
    }

    // Report: labels-per-cell distribution.
    let mut histogram: BTreeMap<u32, u32> = BTreeMap::new();
    let mut worst_cell = ((0usize, 0usize), 0u32);
    for (&cell, &count) in &demand {
        *histogram.entry(count).or_default() += 1;
        if count > worst_cell.1 {
            worst_cell = (cell, count);
        }
    }
    let consumed_cells = demand.len();
    let total_cells = w * h;
    let consumed_fraction = consumed_cells as f64 / total_cells as f64;

    println!(
        "consumed cells: {consumed_cells}/{total_cells} = {:.1}% of the map's character cells carry at least one label character",
        consumed_fraction * 100.0
    );
    println!("labels-per-cell histogram (count -> #cells at that demand):");
    for (count, cells) in &histogram {
        println!("  {count} label(s) wanting a cell: {cells} cells");
    }
    println!(
        "worst cell: row {} col {}, {} labels want it",
        worst_cell.0.0, worst_cell.0.1, worst_cell.1
    );
    println!("per-class placeable-without-collision (draw order = gazetteer_features order):");
    for (class, (clean, total)) in &clean_by_class {
        println!("  {class:?}: {clean}/{total} clean");
    }
    println!("  Volcano specifically: {clean_volcanoes}/{total_volcanoes} clean");

    println!("--- rendered map ---");
    for row in &grid {
        let line: String = row.iter().collect();
        println!("{line}");
    }
}
