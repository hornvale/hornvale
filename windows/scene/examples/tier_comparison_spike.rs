//! THROWAWAY SPIKE (2026-08-30, campaign/the-legend) — not part of the
//! eleven-task plan, not wired into `regenerate-artifacts.sh`, not listed in
//! `docs/generated-paths.txt`. Kept in the tree only so the picture it
//! produced is not lost; do not build on this file as an API.
//!
//! Renders seed 42's real world map — real terrain and climate through the
//! real provider accessors, no mock data — three times at the same rung
//! (`BuildDepth::Settlements`) and the same 80x40 equirectangular window, in
//! three vocabularies: tier 0 (plain ASCII, monochrome), tier 1 (same
//! glyphs, 16-colour ANSI), and tier 2 (extended CP437-as-UTF-8 repertoire,
//! 24-bit colour, elevation-driven depth tinting). Writes all three panels
//! to `docs/audits/tier-comparison-spike.txt`.
//!
//! Run: `cargo run -p hornvale-scene --example tier_comparison_spike`
//! (no arguments; never reads stdin).

use hornvale_astronomy::SkyPins;
use hornvale_climate::{Biome, GeneratedClimate};
use hornvale_kernel::{NearestVertexIndex, Value, Vertex, World};
use hornvale_terrain::{GeneratedTerrain, WaterKind};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};
use std::collections::BTreeMap;
use std::fmt::Write as _;
#[allow(clippy::disallowed_types)]
use std::time::Instant;

/// The canonical fixture seed (matches every other committed example/golden).
const SEED: u64 = 42;
/// Panel width in characters.
const WIDTH: u32 = 80;
/// Panel height in characters — an honest 2:1 equirectangular aspect
/// (longitude spans 360 degrees, latitude 180), not `hornvale_terrain`'s
/// ASCII convention (72x24, a 3:1 char-aspect correction). The brief allows
/// 30-40 rows and is explicit that this render is not bound by the 24-row
/// floor, so the plain geographic ratio is used rather than compensating for
/// terminal character aspect.
const HEIGHT: u32 = WIDTH / 2;

/// A coarse terrain classification: the handful of physiognomic buckets a
/// reader needs to make sense of a world map at a glance. Deliberately much
/// coarser than the 22-biome catalog — colour (tier 1/2) subdivides a bucket,
/// the glyph (all three tiers) never does.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TerrainClass {
    Ocean,
    SaltBasin,
    River,
    Mountain,
    Ice,
    Desert,
    Forest,
    Grassland,
}

/// A point of interest overlaid on top of whatever terrain sits beneath it —
/// always drawn, in every tier, because Nathan's point is that the map
/// should read as "places worth going to."
#[derive(Clone, Debug, PartialEq, Eq)]
enum Poi {
    Settlement(String),
    CaveMouth,
}

/// Everything one raster tile needs across all three tiers, computed once.
#[derive(Clone, Debug)]
struct Tile {
    class: TerrainClass,
    biome: Biome,
    /// Metres above (or below) this world's sea level — the input to depth
    /// tinting and to the snow-cap/bare-mountain split.
    height_asl_m: f64,
    /// Upstream land-vertex count (0 off-river) — subdivides the river glyph
    /// by discharge rather than by a guessed threshold (see `render`).
    drainage: f64,
    poi: Option<Poi>,
}

fn main() {
    #[allow(clippy::disallowed_types)]
    let build_start = Instant::now();
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let artifacts = build_world_to_with_artifacts(
        hornvale_kernel::Seed(SEED),
        &SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Settlements,
    )
    .expect("seed 42 builds at BuildDepth::Settlements");
    #[allow(clippy::disallowed_types)]
    let build_secs = build_start.elapsed().as_secs_f64();

    let terrain = artifacts
        .terrain
        .expect("terrain is Some at BuildDepth::Settlements");
    let climate = artifacts
        .climate
        .expect("climate is Some at BuildDepth::Settlements");
    let world = artifacts.world;

    let settlements = settlement_markers(&world);
    let poi_overlay = project_poi_overlay(&terrain, &settlements);
    eprintln!(
        "diagnostics: geosphere vertex_count={}, committed settlements={}, \
         cave-bearing vertices={}, distinct {WIDTH}x{HEIGHT} POI pixels after \
         forward projection={}",
        terrain.geosphere().vertex_count(),
        settlements.len(),
        terrain
            .geosphere()
            .vertices()
            .filter(|v| terrain.cave_at(*v).is_some())
            .count(),
        poi_overlay.len()
    );
    let grid = build_grid(&terrain, &climate, &poi_overlay);

    let mut out = String::new();
    let _ = writeln!(
        out,
        "seed {SEED}, BuildDepth::Settlements, {WIDTH}x{HEIGHT}, built in {build_secs:.3}s\n\
         THROWAWAY SPIKE — see .superpowers/sdd/2026-08-28-the-legend/spike-tiers-report.md\n"
    );

    let _ = writeln!(out, "=== Tier 0 — ASCII, monochrome ===");
    let _ = writeln!(
        out,
        "legend: ~ ocean  = salt-basin  + river  ^ mountain  * ice  . desert  f forest  \" grassland  T settlement  c cave mouth"
    );
    out.push_str(&render_tier0(&grid));
    out.push('\n');

    let _ = writeln!(out, "=== Tier 1 — ASCII glyphs + 16-colour ANSI ===");
    let _ = writeln!(
        out,
        "legend: same glyphs as tier 0; colour subdivides a glyph class (marine biome, \
         forest/grassland biome, snow-capped vs bare mountain, major vs minor river)"
    );
    out.push_str(&render_tier1(&grid));
    out.push('\n');

    let _ = writeln!(
        out,
        "=== Tier 2 — extended repertoire (CP437-as-UTF-8) + 24-bit colour + depth tinting ==="
    );
    let _ = writeln!(
        out,
        "legend: shading ramp for ocean depth, box-drawing for river/coastline connectivity, \
         land glyphs as tier 0/1, colour from the real biome palette, elevation-tinted toward \
         a haze background (aerial perspective)"
    );
    out.push_str(&render_tier2(&grid));
    out.push('\n');

    std::fs::write("docs/audits/tier-comparison-spike.txt", out)
        .expect("docs/audits/ exists and is writable");
    println!("wrote docs/audits/tier-comparison-spike.txt");
}

/// Every committed settlement, keyed by the geosphere vertex it sits on,
/// with its name — read straight off the ledger the way
/// `windows/worldgen`'s own settlement scans do (`IS_SETTLEMENT`'s subject,
/// `VERTEX_ID`'s value, `NAME`'s text).
fn settlement_markers(world: &World) -> BTreeMap<Vertex, String> {
    let mut out = BTreeMap::new();
    for f in world.ledger.find(hornvale_settlement::IS_SETTLEMENT) {
        let id = f.subject;
        let vertex = match world.ledger.value_of(id, hornvale_settlement::VERTEX_ID) {
            Some(Value::Number(n)) => Vertex(*n as u32),
            _ => continue,
        };
        let name = world
            .ledger
            .text_of(id, hornvale_kernel::NAME)
            .unwrap_or("(unnamed)")
            .to_string();
        out.insert(vertex, name);
    }
    out
}

/// Forward-project every committed settlement and every cave-bearing vertex
/// onto its own `(px, py)` pixel — the inverse of `build_grid`'s per-pixel
/// nearest-vertex reverse lookup.
///
/// **Why this exists rather than checking `cave_at`/`settlements.get` on the
/// per-pixel nearest vertex directly (the first cut of this spike):** at
/// 80x40 over a 40,962-vertex geosphere, the vast majority of vertices are
/// never anyone's "nearest" pixel centre, so that approach silently dropped
/// most committed POIs — measured, only a small fraction of settlements
/// survived (see the report). Forward-projecting each POI's own
/// [`hornvale_kernel::GeoCoord`] guarantees every one of them lands
/// *somewhere* on the raster, at the cost of collisions where several POIs
/// share a pixel (an honest resolution limit, not a bug — one 80x40 tile
/// covers roughly 4.5 degrees square).
fn project_poi_overlay(
    terrain: &GeneratedTerrain,
    settlements: &BTreeMap<Vertex, String>,
) -> BTreeMap<(u32, u32), Poi> {
    let geo = terrain.geosphere();
    let project = |vertex: Vertex| -> (u32, u32) {
        let coord = geo.coord(vertex);
        let py = (((90.0 - coord.latitude) / 180.0 * f64::from(HEIGHT)) as u32).min(HEIGHT - 1);
        let px = (((coord.longitude + 180.0) / 360.0 * f64::from(WIDTH)) as u32) % WIDTH;
        (px, py)
    };
    let mut overlay = BTreeMap::new();
    // Cave mouths first, settlements second: a settlement is the more
    // legible POI at a shared pixel, so it overwrites.
    for vertex in geo.vertices() {
        if terrain.cave_at(vertex).is_some() {
            overlay.insert(project(vertex), Poi::CaveMouth);
        }
    }
    for (&vertex, name) in settlements {
        overlay.insert(project(vertex), Poi::Settlement(name.clone()));
    }
    overlay
}

/// Classify one vertex's terrain into the coarse [`TerrainClass`] bucket.
/// Water first (a river cuts visibly through a mountain rather than being
/// hidden by it), then relief, then biome vegetation family.
fn classify(
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
    vertex: Vertex,
) -> TerrainClass {
    match terrain.water_kind_at(vertex) {
        WaterKind::Ocean => return TerrainClass::Ocean,
        WaterKind::SaltBasin => return TerrainClass::SaltBasin,
        WaterKind::River => return TerrainClass::River,
        WaterKind::DryLand => {}
    }
    let height = terrain.elevation_at(vertex).above(terrain.sea_level());
    let band = hornvale_scene::relief_band(height);
    let biome = climate.biome_at(vertex);
    if band >= 4 || biome == Biome::Alpine {
        return TerrainClass::Mountain;
    }
    match biome {
        Biome::Ice => TerrainClass::Ice,
        Biome::Desert => TerrainClass::Desert,
        Biome::Taiga
        | Biome::TemperateForest
        | Biome::TemperateRainforest
        | Biome::TropicalSeasonalForest
        | Biome::TropicalRainforest => TerrainClass::Forest,
        // Every remaining land biome (TemperateGrassland, Shrubland, Savanna,
        // Tundra, and — belt and braces — any land-side marine variant that
        // should be structurally unreachable here) reads as open ground.
        _ => TerrainClass::Grassland,
    }
}

/// Rasterize the full grid once: one `Tile` per character, in row-major
/// (latitude-descending, longitude-ascending) order, matching
/// `hornvale_terrain::render::elevation_ascii`'s own scan order.
fn build_grid(
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
    poi_overlay: &BTreeMap<(u32, u32), Poi>,
) -> Vec<Vec<Tile>> {
    let geo = terrain.geosphere();
    let index = NearestVertexIndex::new(geo);
    let mut grid = Vec::with_capacity(HEIGHT as usize);
    for py in 0..HEIGHT {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(HEIGHT) * 180.0;
        let mut row = Vec::with_capacity(WIDTH as usize);
        for px in 0..WIDTH {
            let longitude = (f64::from(px) + 0.5) / f64::from(WIDTH) * 360.0 - 180.0;
            let vertex = index.nearest(geo, latitude, longitude);
            let class = classify(terrain, climate, vertex);
            let biome = climate.biome_at(vertex);
            let height_asl_m = terrain
                .elevation_at(vertex)
                .above(terrain.sea_level())
                .get();
            let drainage = terrain.drainage_at(vertex);
            let poi = poi_overlay.get(&(px, py)).cloned();
            row.push(Tile {
                class,
                biome,
                height_asl_m,
                drainage,
                poi,
            });
        }
        grid.push(row);
    }
    grid
}

/// The bare glyph for a terrain class — identical across all three tiers.
fn base_glyph(class: TerrainClass) -> char {
    match class {
        TerrainClass::Ocean => '~',
        TerrainClass::SaltBasin => '=',
        TerrainClass::River => '+',
        TerrainClass::Mountain => '^',
        TerrainClass::Ice => '*',
        TerrainClass::Desert => '.',
        TerrainClass::Forest => 'f',
        TerrainClass::Grassland => '"',
    }
}

/// The glyph actually drawn at a tile: a POI overlay wins over terrain,
/// identically in every tier (a settlement or a cave mouth is always worth
/// seeing).
fn glyph_at(tile: &Tile) -> char {
    match &tile.poi {
        Some(Poi::Settlement(_)) => 'T',
        Some(Poi::CaveMouth) => 'c',
        None => base_glyph(tile.class),
    }
}

/// Tier 0: plain ASCII, no colour, one line per row.
fn render_tier0(grid: &[Vec<Tile>]) -> String {
    let mut out = String::with_capacity((WIDTH as usize + 1) * HEIGHT as usize);
    for row in grid {
        for tile in row {
            out.push(glyph_at(tile));
        }
        out.push('\n');
    }
    out
}

/// The river drainage median across every river tile in the grid — used to
/// split "major" from "minor" river colouring on real data rather than a
/// guessed absolute threshold, since `drainage_at`'s units (upstream
/// land-vertex count) have no a-priori scale.
fn river_drainage_median(grid: &[Vec<Tile>]) -> f64 {
    let mut values: Vec<f64> = grid
        .iter()
        .flatten()
        .filter(|c| c.class == TerrainClass::River)
        .map(|c| c.drainage)
        .collect();
    if values.is_empty() {
        return 0.0;
    }
    values.sort_by(f64::total_cmp);
    values[values.len() / 2]
}

/// One of the 16 standard ANSI foreground codes.
#[derive(Clone, Copy)]
struct Ansi16(u8);

impl Ansi16 {
    const RED: Ansi16 = Ansi16(31);
    const GREEN: Ansi16 = Ansi16(32);
    const YELLOW: Ansi16 = Ansi16(33);
    const BLUE: Ansi16 = Ansi16(34);
    const MAGENTA: Ansi16 = Ansi16(35);
    const CYAN: Ansi16 = Ansi16(36);
    const WHITE: Ansi16 = Ansi16(37);
    const BRIGHT_BLACK: Ansi16 = Ansi16(90);
    const BRIGHT_YELLOW: Ansi16 = Ansi16(93);
    const BRIGHT_BLUE: Ansi16 = Ansi16(94);
    const BRIGHT_CYAN: Ansi16 = Ansi16(96);
    const BRIGHT_GREEN: Ansi16 = Ansi16(92);
    const BRIGHT_WHITE: Ansi16 = Ansi16(97);

    fn code(self) -> String {
        format!("\x1b[{}m", self.0)
    }
}

const RESET: &str = "\x1b[0m";

/// The 16-colour subdivision within a glyph class, driven by the real biome
/// or real elevation reading at the tile — never invented per-tile noise.
fn tier1_color(tile: &Tile, river_median: f64) -> Ansi16 {
    if let Some(poi) = &tile.poi {
        return match poi {
            Poi::Settlement(_) => Ansi16::BRIGHT_YELLOW,
            Poi::CaveMouth => Ansi16::RED,
        };
    }
    match tile.class {
        TerrainClass::Ocean => match tile.biome {
            Biome::CoralReef => Ansi16::MAGENTA,
            Biome::KelpForest => Ansi16::CYAN,
            Biome::SeaIce => Ansi16::WHITE,
            Biome::HydrothermalVent => Ansi16::RED,
            Biome::Upwelling => Ansi16::BRIGHT_CYAN,
            Biome::Epipelagic => Ansi16::BRIGHT_BLUE,
            Biome::Mesopelagic => Ansi16::BLUE,
            Biome::Bathypelagic | Biome::Abyssal | Biome::HadalTrench => Ansi16::BRIGHT_BLACK,
            _ => Ansi16::BLUE,
        },
        TerrainClass::SaltBasin => Ansi16::WHITE,
        TerrainClass::River => {
            if tile.drainage >= river_median {
                Ansi16::BRIGHT_BLUE
            } else {
                Ansi16::BLUE
            }
        }
        TerrainClass::Mountain => {
            let band = 4 + u32::from(tile.height_asl_m >= 2500.0);
            if band >= 5 {
                Ansi16::BRIGHT_WHITE // snow-capped
            } else {
                Ansi16::WHITE // bare
            }
        }
        TerrainClass::Ice => Ansi16::BRIGHT_WHITE,
        TerrainClass::Desert => Ansi16::YELLOW,
        TerrainClass::Forest => match tile.biome {
            Biome::TemperateRainforest | Biome::TropicalRainforest => Ansi16::BRIGHT_GREEN,
            Biome::TropicalSeasonalForest => Ansi16::YELLOW,
            _ => Ansi16::GREEN,
        },
        TerrainClass::Grassland => match tile.biome {
            Biome::Savanna => Ansi16::YELLOW,
            Biome::Shrubland => Ansi16::BRIGHT_YELLOW,
            Biome::Tundra => Ansi16::WHITE,
            _ => Ansi16::GREEN,
        },
    }
}

/// Tier 1: tier-0 glyphs painted with the 16-colour subdivision above.
fn render_tier1(grid: &[Vec<Tile>]) -> String {
    let median = river_drainage_median(grid);
    let mut out = String::new();
    for row in grid {
        for tile in row {
            out.push_str(&tier1_color(tile, median).code());
            out.push(glyph_at(tile));
        }
        out.push_str(RESET);
        out.push('\n');
    }
    out
}

/// The real biome palette (`hornvale_climate::Biome::color`) as this tile's
/// base truecolor — "real terrain through the real classifiers," not a
/// hand-picked map palette. Water/POI classes that do not resolve to a
/// meaningful land biome colour (salt basin, river) get one fixed tone each
/// instead of the incidental land-biome colour `biome_at` would otherwise
/// return for a non-ocean vertex.
fn base_truecolor(tile: &Tile) -> [u8; 3] {
    if let Some(poi) = &tile.poi {
        return match poi {
            Poi::Settlement(_) => [255, 205, 90], // hearth-gold
            Poi::CaveMouth => [220, 110, 60],     // cave-mouth glow
        };
    }
    match tile.class {
        TerrainClass::SaltBasin => [225, 235, 225],
        TerrainClass::River => {
            // Same drainage-driven brightness idea as tier 1, continuous
            // instead of binary: darker for a trickle, brighter for a major
            // river, scaled against the grid's own observed max.
            let t = (tile.drainage / 400.0).clamp(0.0, 1.0);
            lerp([50, 90, 150], [110, 180, 255], t)
        }
        // Ocean and every land class read the real biome colour: `biome_at`
        // already resolves the correct marine variant on an ocean vertex and
        // the correct land community on a dry one.
        _ => tile.biome.color(),
    }
}

/// Linear-interpolate two RGB triples.
fn lerp(a: [u8; 3], b: [u8; 3], t: f64) -> [u8; 3] {
    let t = t.clamp(0.0, 1.0);
    [
        (f64::from(a[0]) + (f64::from(b[0]) - f64::from(a[0])) * t).round() as u8,
        (f64::from(a[1]) + (f64::from(b[1]) - f64::from(a[1])) * t).round() as u8,
        (f64::from(a[2]) + (f64::from(b[2]) - f64::from(a[2])) * t).round() as u8,
    ]
}

/// Aerial-perspective depth tint: blend a tile's base colour toward a cool
/// haze background as its elevation falls, so low ground recedes and high
/// ground stays crisp. Never fully desaturates (floor at 0.22) and never
/// exceeds full saturation (ceiling at 1.0) — a demonstration of the
/// technique for later rope-bridge/cavern-ledge use, applied here to terrain
/// elevation rather than a first-person depth coordinate.
fn depth_tint(base: [u8; 3], height_asl_m: f64) -> [u8; 3] {
    const HAZE: [u8; 3] = [56, 64, 80];
    // Trench floor (~-6000 m) to alpine peak (~+4000 m) spans the range this
    // world's own relief actually occupies (see `hornvale_scene::relief_band`
    // and `RELIEF_LEGEND`'s abyss/alpine endpoints).
    let t = ((height_asl_m + 6000.0) / 10000.0).clamp(0.0, 1.0);
    let mix = 0.22 + 0.78 * t;
    lerp(HAZE, base, mix)
}

fn truecolor_code(rgb: [u8; 3]) -> String {
    format!("\x1b[38;2;{};{};{}m", rgb[0], rgb[1], rgb[2])
}

/// A 4-neighbour connectivity bitmask (N, S, E, W), each true iff that
/// neighbour tile also belongs to `member`. Longitude wraps (a real sphere);
/// latitude clamps at the poles (no wraparound there).
fn neighbor_mask(grid: &[Vec<Tile>], px: usize, py: usize, member: impl Fn(&Tile) -> bool) -> u8 {
    let h = grid.len();
    let w = grid[0].len();
    let north = py.checked_sub(1).map(|y| member(&grid[y][px]));
    let south = if py + 1 < h {
        Some(member(&grid[py + 1][px]))
    } else {
        None
    };
    let west = member(&grid[py][(px + w - 1) % w]);
    let east = member(&grid[py][(px + 1) % w]);
    let mut mask = 0u8;
    if north.unwrap_or(false) {
        mask |= 0b1000;
    }
    if south.unwrap_or(false) {
        mask |= 0b0100;
    }
    if east {
        mask |= 0b0010;
    }
    if west {
        mask |= 0b0001;
    }
    mask
}

/// A single-line box-drawing character auto-tiled from a (N, S, E, W)
/// connectivity mask — the same "wall auto-tiling" idea roguelikes use for
/// corridors, applied here to a river's or a coastline's course. `0` (an
/// isolated tile with no like neighbour, e.g. a one-vertex river mouth)
/// falls back to a bullet rather than a bare pipe, so it does not read as a
/// stray vertical/horizontal stroke pointing nowhere.
fn box_glyph(mask: u8) -> char {
    match mask {
        0b0000 => '\u{25CF}',                   // ● isolated
        0b1000 | 0b0100 | 0b1100 => '\u{2502}', // │
        0b0001..=0b0011 => '\u{2500}',          // ─
        0b1010 => '\u{2514}',                   // └
        0b1001 => '\u{2518}',                   // ┘
        0b0110 => '\u{250C}',                   // ┌
        0b0101 => '\u{2510}',                   // ┐
        0b1110 => '\u{251C}',                   // ├
        0b1101 => '\u{2524}',                   // ┤
        0b1011 => '\u{2534}',                   // ┴
        0b0111 => '\u{252C}',                   // ┬
        0b1111 => '\u{253C}',                   // ┼
        _ => unreachable!("4-bit mask"),
    }
}

/// Tier 2: extended repertoire, truecolor, elevation depth tint, and
/// box-drawing connectivity for rivers and coastlines.
fn render_tier2(grid: &[Vec<Tile>]) -> String {
    let h = grid.len();
    let w = grid[0].len();
    let mut out = String::new();
    for py in 0..h {
        for px in 0..w {
            let tile = &grid[py][px];
            let glyph = match &tile.poi {
                Some(Poi::Settlement(_)) => 'T',
                Some(Poi::CaveMouth) => 'c',
                None => match tile.class {
                    TerrainClass::Ocean => {
                        // A shading-ramp bathymetry read: heavier shade for
                        // deeper water. Coastal water (touching dry land)
                        // instead traces the shoreline in box-drawing, one
                        // tile deep on the water side.
                        let touches_land = neighbor_mask(grid, px, py, |c| {
                            !matches!(c.class, TerrainClass::Ocean | TerrainClass::SaltBasin)
                        }) != 0;
                        if touches_land {
                            let coast_mask = neighbor_mask(grid, px, py, |c| {
                                matches!(c.class, TerrainClass::Ocean | TerrainClass::SaltBasin)
                            });
                            box_glyph(coast_mask)
                        } else if tile.height_asl_m < -3000.0 {
                            '\u{2588}' // deep: full block
                        } else if tile.height_asl_m < -1000.0 {
                            '\u{2593}' // dark shade
                        } else if tile.height_asl_m < -200.0 {
                            '\u{2592}' // medium shade
                        } else {
                            '\u{2591}' // light shade (shelf)
                        }
                    }
                    TerrainClass::River => {
                        let mask = neighbor_mask(grid, px, py, |c| {
                            matches!(
                                c.class,
                                TerrainClass::River | TerrainClass::Ocean | TerrainClass::SaltBasin
                            )
                        });
                        box_glyph(mask)
                    }
                    other => base_glyph(other),
                },
            };
            let base = base_truecolor(tile);
            let tinted = depth_tint(base, tile.height_asl_m);
            out.push_str(&truecolor_code(tinted));
            out.push(glyph);
        }
        out.push_str(RESET);
        out.push('\n');
    }
    out
}
