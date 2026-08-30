//! The Legend, Task 6's own behavioural tests for the world map's terrain
//! vocabulary.
//!
//! Before this task, `terrain_at_tile`/`draw_terrain_layer` painted exactly
//! two glyphs — `~` ocean, `.` land — regardless of elevation or water
//! kind. Nathan's own report started this campaign: "the ocean is `~` and
//! the land is `.` -- nothing more detailed than that."
//! `seed_42_draws_more_than_two_distinct_terrain_glyphs` is that sentence
//! as an assertion.

use hornvale_game::mercator;
use hornvale_game::plate::{self, Window};
use hornvale_game_core::Grid;
use hornvale_game_core::register::binding_of;
use hornvale_kernel::{Geosphere, NearestVertexIndex, RoomMeshMemo, Seed};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use std::collections::BTreeSet;

/// Seed 42's terrain, built the same way `plate.rs`'s own private
/// `test_world()` does — no shared helper crosses the integration-test
/// boundary (that module's own doc records there is no such helper
/// anywhere in `clients/game`), so this is the one this file owns.
fn test_world() -> (GeneratedTerrain, Geosphere) {
    let geo = Geosphere::new(hornvale_terrain::GLOBE_LEVEL);
    let outcome = hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default())
        .expect("default pins generate seed 42");
    let terrain = GeneratedTerrain::new(geo.clone(), outcome);
    (terrain, geo)
}

/// The window this whole file draws through: [`plate::GLOBE_RUNG`]
/// (the world map's own coarsest, always-shipped rung), centred on the
/// equator at an 80x24-terminal-sized plate — the same origin construction
/// `plate.rs`'s own `mesh_addressing_agrees_with_the_spatial_search` uses
/// (equatorial, where Mercator's stretch is least), sized to what a player
/// actually sees at the client's floor. At seed 42 this window reaches a
/// real coastline, a mountain range, a river and a salt basin all at once —
/// verified by inspection, not asserted here (the tests below assert the
/// PROPERTIES that follow from that, not the specific terrain).
fn seed_42_window(
    h: u16,
) -> (
    GeneratedTerrain,
    Geosphere,
    NearestVertexIndex,
    mercator::Frame,
    Window,
    u16,
) {
    let (terrain, geo) = test_world();
    let index = NearestVertexIndex::new(&geo);
    let f = mercator::frame_for(false);
    let (_, vh) = plate::virtual_dims(plate::GLOBE_RUNG);
    let w = 80u16;
    let win = Window {
        depth: plate::GLOBE_RUNG,
        origin_col: 0,
        origin_row: vh / 2 - u32::from(h) / 2,
    };
    (terrain, geo, index, f, win, w)
}

/// The terrain layer only (no settlements/caves/discovery — this file's
/// subject is the raster, not the point-site overlay), colour off.
fn seed_42_terrain_layer() -> Grid {
    let (terrain, geo, index, f, win, w) = seed_42_window(24);
    plate::draw_with(
        &terrain,
        &geo,
        &index,
        &f,
        &win,
        w,
        24,
        false,
        &BTreeSet::new(),
        &BTreeSet::new(),
        &Default::default(),
    )
}

/// Every glyph actually painted onto `grid`.
fn drawn_glyphs(grid: &Grid) -> BTreeSet<char> {
    (0..grid.height())
        .flat_map(|y| (0..grid.width()).map(move |x| (x, y)))
        .filter_map(|(x, y)| grid.get(x, y).and_then(|c| c.glyph))
        .collect()
}

/// Nathan's actual report: "the ocean is `~` and the land is `.` --
/// nothing more detailed than that". This is that sentence as an
/// assertion.
#[test]
fn seed_42_draws_more_than_two_distinct_terrain_glyphs() {
    let grid = seed_42_terrain_layer();
    let glyphs = drawn_glyphs(&grid);
    assert!(
        glyphs.len() > 2,
        "the whole terrain vocabulary is still {glyphs:?}"
    );
}

/// Decision 0389 enforced against a REAL render rather than the table
/// alone: a glyph nobody claimed is exactly how `+` came to mean three
/// things.
#[test]
fn every_drawn_glyph_is_claimed_by_the_register() {
    let grid = seed_42_terrain_layer();
    for g in drawn_glyphs(&grid) {
        assert!(
            binding_of(g).is_some() || g.is_ascii_alphabetic() || g == ' ',
            "{g:?} is drawn but unclaimed"
        );
    }
}

/// The Portolan fix round 1, Finding 2 bought this with a 49-point vote:
/// the strip must never name a land feature on a terminal grid square drawn
/// as water — an AREA, the sense `hornvale_game_core`'s own grid square type
/// already carries, never the mesh-vertex sense the lexicon guard exists to
/// keep out. (lexicon: terminal-grid-square sense, not mesh-vertex sense)
/// [`plate::TileTerrain`]'s own doc says this holds BY CONSTRUCTION (the
/// vertex is chosen from the corners the class was decided on) — this test
/// pins that construction against a real render rather than trusting the
/// doc comment alone.
#[test]
fn the_ocean_land_boundary_still_agrees_with_the_terrain() {
    let (terrain, geo, index, f, win, w) = seed_42_window(24);
    let (virtual_w, virtual_h) = plate::virtual_dims(win.depth);
    let mut memo = RoomMeshMemo::default();
    for row in 0..u32::from(24u16) {
        for col in 0..u32::from(w) {
            let tile = plate::terrain_at_tile(
                &terrain, &geo, &index, &mut memo, &f, &win, virtual_w, virtual_h, row, col,
            );
            assert_eq!(
                terrain.is_ocean(tile.vertex),
                tile.ocean,
                "tile at (row={row}, col={col}) disagreed with its own vertex's ocean status"
            );
        }
    }
}
