//! THROWAWAY DIAGNOSTIC (delete me) — the CONSERVATION test decision 0124
//! demands, for the two candidate refinements of the world plate's terrain
//! layer:
//!
//! 1. relief from a bilinear BLEND at the tile's own facet depth (ordinal —
//!    permitted to band a blend, decision 0121), vs today's nearest-corner
//!    snap at the grid-level ancestor;
//! 2. water from the FLOW GRAPH (`GeneratedTerrain::transverse_at`, The
//!    Ford's polyline+width carrier), vs today's nominal `water_kind_at`
//!    point sample.
//!
//! The variation criterion is "does the picture gain detail". The
//! CONSERVATION criterion is "does the water area survive" — 0121 measured a
//! rejected refinement deleting 29% of fresh water, and no local hypothesis
//! would have caught it. So this reports areas at the MATCHED rung (6), where
//! today's answer is the calibrated reference, as well as the pictures at
//! fine rungs.
//!
//! Run: `cargo run --manifest-path clients/game/bin/Cargo.toml --release
//! --example rung_detail_probe`

use hornvale_game::mercator;
use hornvale_game::plate::{self, Window};
use hornvale_kernel::{Facet, Geosphere, NearestVertexIndex, RoomMeshMemo, Seed};
use hornvale_terrain::{GeneratedTerrain, TerrainPins, Transverse, WaterKind};
use std::collections::BTreeMap;

const RELIEF: [char; 6] = [' ', '`', ',', ';', '^', '%'];

fn relief_glyph(band: u32) -> char {
    RELIEF[(band as usize).min(5)]
}

/// Today's answer for one tile.
// A diagnostic that mirrors `plate::terrain_at_tile`'s own parameter list
// so the two can be read side by side; bundling them into a struct here
// would make the comparison harder, not easier.
#[allow(clippy::too_many_arguments)]
fn shipped(
    terrain: &GeneratedTerrain,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    memo: &mut RoomMeshMemo,
    f: &mercator::Frame,
    win: &Window,
    vw: u32,
    vh: u32,
    row: u32,
    col: u32,
) -> char {
    let t = plate::terrain_at_tile(
        terrain,
        geo,
        index,
        memo,
        f,
        win,
        vw,
        vh,
        row,
        col,
        None,
        hornvale_kernel::WorldTime::GENESIS,
    );
    match t.water {
        0 => '~',
        1 => '=',
        2 => '"',
        _ => relief_glyph(t.band),
    }
}

/// The candidate: blended relief (ordinal) + flow-graph water (partition).
// A diagnostic that mirrors `plate::terrain_at_tile`'s own parameter list
// so the two can be read side by side; bundling them into a struct here
// would make the comparison harder, not easier.
#[allow(clippy::too_many_arguments)]
fn candidate(
    terrain: &GeneratedTerrain,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    f: &mercator::Frame,
    win: &Window,
    vw: u32,
    vh: u32,
    row: u32,
    col: u32,
) -> char {
    let (lat, lon) = mercator::unproject(f, win.origin_row + row, win.origin_col + col, vw, vh);
    let pos = hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon);
    let facet = Facet::containing(pos, win.depth.max(geo.depth()));

    let Some(wts) = facet.corner_weights(geo, index) else {
        return '?';
    };
    let denom: u64 = wts.iter().map(|&(_, w)| w).sum();
    let blended: f64 = wts
        .iter()
        .map(|&(c, w)| w as f64 * terrain.elevation_at(c).get())
        .sum::<f64>()
        / denom as f64;
    let asl = blended - terrain.sea_level().get();

    // Nominal fields still PARTITION (0121): the dominant corner decides
    // ocean vs land and salt basin, never a threshold on the blend.
    let best = wts
        .iter()
        .max_by_key(|&&(v, w)| (w, std::cmp::Reverse(v)))
        .unwrap()
        .0;
    match terrain.water_kind_at(best) {
        WaterKind::Ocean => return '~',
        WaterKind::SaltBasin => return '=',
        _ => {}
    }

    // A river is a LINE with a width, asked at this tile's own position —
    // The Ford's carrier, not the vertex's nominal class.
    //
    // SCALE-AWARE, and that is the whole correction: `Transverse::Channel`
    // answers "is this POINT in the channel", but a chart tile wider than the
    // channel is asking "does a channel CROSS this tile". The two are the
    // same question at different scales, so the test is the signed distance
    // against whichever is larger — the channel's own half-width, or half
    // this tile's footprint. At the matched rung the tile dominates and the
    // coarse answer is reproduced; at band B the channel dominates and the
    // river is drawn at its true width.
    let (transverse, d) = terrain.transverse_at(pos);
    let tile_half_rad = (std::f64::consts::PI
        / (2.0 * hornvale_kernel::math::powf(2.0, f64::from(win.depth))))
        * hornvale_kernel::math::cos(lat.to_radians()).abs().max(1e-6)
        / 2.0;
    if matches!(transverse, Transverse::Channel) || d.abs() <= tile_half_rad {
        return '"';
    }

    relief_glyph(hornvale_scene::relief_band(
        hornvale_kernel::SeaLevelHeight::from_metres(asl),
    ))
}

fn main() {
    let geo = Geosphere::new(hornvale_terrain::GLOBE_LEVEL);
    let outcome = hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default())
        .expect("default pins generate seed 42");
    let terrain = GeneratedTerrain::new(geo.clone(), outcome);
    let index = NearestVertexIndex::new(&geo);
    let f = mercator::frame_for(false);

    let mut river = None;
    for v in geo.vertices() {
        if terrain.water_kind_at(v) == WaterKind::River {
            river = Some(v);
            break;
        }
    }
    let river = river.expect("seed 42 has rivers");
    let p = geo.position(river);
    let lat = hornvale_kernel::math::asin(p[2]).to_degrees();
    let lon = hornvale_kernel::math::atan2(p[1], p[0]).to_degrees();

    // ---- CONSERVATION: whole-chart areas at the MATCHED rung ----
    {
        let rung = plate::GLOBE_RUNG;
        let (vw, vh) = plate::virtual_dims(rung);
        let win = Window {
            depth: rung,
            origin_col: 0,
            origin_row: 0,
        };
        let mut memo = RoomMeshMemo::default();
        let (mut a, mut b): (BTreeMap<char, usize>, BTreeMap<char, usize>) =
            (BTreeMap::new(), BTreeMap::new());
        // Sample the whole chart on a stride so this stays seconds, not minutes.
        let stride = 2;
        let mut n = 0usize;
        for row in (0..vh).step_by(stride) {
            for col in (0..vw).step_by(stride) {
                *a.entry(shipped(
                    &terrain, &geo, &index, &mut memo, &f, &win, vw, vh, row, col,
                ))
                .or_default() += 1;
                *b.entry(candidate(
                    &terrain, &geo, &index, &f, &win, vw, vh, row, col,
                ))
                .or_default() += 1;
                n += 1;
            }
        }
        println!(
            "=== CONSERVATION at the matched rung {rung} ({n} samples over the whole chart) ==="
        );
        println!(
            "{:>8}  {:>10}  {:>10}  {:>8}",
            "glyph", "today", "candidate", "delta"
        );
        let mut keys: Vec<char> = a.keys().chain(b.keys()).copied().collect();
        keys.sort();
        keys.dedup();
        for k in keys {
            let (x, y) = (*a.get(&k).unwrap_or(&0), *b.get(&k).unwrap_or(&0));
            let d = if x == 0 {
                f64::INFINITY
            } else {
                (y as f64 - x as f64) / x as f64 * 100.0
            };
            println!("{k:>8?}  {x:>10}  {y:>10}  {d:>7.1}%");
        }
    }

    // ---- VARIATION: the pictures ----
    let (w, h) = (108u32, 22u32);
    for rung in [plate::GLOBE_RUNG, 10, plate::BAND_B_RUNG] {
        let (vw, vh) = plate::virtual_dims(rung);
        let (crow, ccol) = mercator::project(&f, lat, lon, vw, vh).expect("in frame");
        let win = Window {
            depth: rung,
            origin_col: ccol.saturating_sub(w / 2),
            origin_row: crow.saturating_sub(h / 2),
        };
        let mut memo = RoomMeshMemo::default();
        println!("\n=== rung {rung}: today (left) vs candidate (right) ===");
        for row in 0..h {
            let mut la = String::new();
            let mut lb = String::new();
            for col in 0..w / 2 {
                la.push(shipped(
                    &terrain, &geo, &index, &mut memo, &f, &win, vw, vh, row, col,
                ));
                lb.push(candidate(
                    &terrain, &geo, &index, &f, &win, vw, vh, row, col,
                ));
            }
            println!("{la}   {lb}");
        }
    }
}
