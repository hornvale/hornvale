//! The Legend, Task 6's own behavioural tests for the world map's terrain
//! vocabulary.
//!
//! Before this task, `terrain_at_tile`/`draw_terrain_layer` painted exactly
//! two glyphs — `~` ocean, `.` land — regardless of elevation or water
//! kind. Nathan's own report started this campaign: "the ocean is `~` and
//! the land is `.` -- nothing more detailed than that."
//! `seed_42_draws_more_than_two_distinct_terrain_glyphs` is that sentence
//! as an assertion.

use hornvale_game::discovery::{Discovered, FeatureId};
use hornvale_game::mercator;
use hornvale_game::plate::{self, Window};
use hornvale_game_core::Grid;
use hornvale_game_core::register::binding_of;
use hornvale_kernel::{Geosphere, NearestVertexIndex, RoomMeshMemo, Seed, Vertex};
use hornvale_terrain::landscape::FeatureClass;
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

/// The terrain layer only (no sites and no discovery — this file's
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
        &[],
        &BTreeSet::new(),
        &[],
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

/// Whether `site`'s own projected position draws `glyph` at SOME rung of
/// the shipped ladder (`GLOBE_RUNG..=BAND_B_RUNG`), when `draw` renders a
/// small `w`x`h` window centred there. `draw` is handed the window and the
/// screen column/row to check, and supplies its own rosters/discovery
/// gate — kept generic over a closure because the two landform kinds
/// below take different roster TYPES (a volcano is discovery-gated through
/// `BTreeSet<Vertex>`; a waterfall draws unconditionally from a bare
/// slice), so one shared roster shape would not fit both.
///
/// A vertex above the projection's polar clamp at a given rung is SKIPPED
/// at that rung, not treated as absent — the loop still tries every other
/// rung, since a real vertex can be inside the clamp at one rung's frame
/// geometry and not another's only in principle (the clamp is a fixed
/// latitude band, so this is defensive rather than observed on seed 42).
fn drawn_at_some_shipped_rung(
    geo: &Geosphere,
    f: &mercator::Frame,
    w: u16,
    h: u16,
    site: Vertex,
    glyph: char,
    mut draw: impl FnMut(&Window, u16, u16) -> Grid,
) -> bool {
    for depth in plate::GLOBE_RUNG..=plate::BAND_B_RUNG {
        let (vw, vh) = plate::virtual_dims(depth);
        let g = geo.coord(site);
        let Some((row, col)) = mercator::project(f, g.latitude, g.longitude, vw, vh) else {
            continue;
        };
        let origin_row = row.saturating_sub(u32::from(h) / 2).min(vh - u32::from(h));
        let origin_col = (col + vw - u32::from(w) / 2) % vw;
        let win = Window {
            depth,
            origin_col,
            origin_row,
        };
        let sx = ((col + vw - origin_col) % vw) as u16;
        let sy = (row - origin_row) as u16;
        let grid = draw(&win, sx, sy);
        if grid.get(sx, sy).and_then(|c| c.glyph) == Some(glyph) {
            return true;
        }
    }
    false
}

/// Task 7's own guard, and it exists because of a defect The Quadrat found
/// once already: a glyph that is defined and reachable in principle but
/// UNDRAWABLE in practice (seed 42's flagship settlement, undrawable at
/// every shipped rung under the old area-majority sampling). For each
/// landform the terrain reports as PRESENT on seed 42, this asserts its
/// glyph appears somewhere across the shipped rungs. A landform seed 42
/// does NOT have is SKIPPED, not failed — this is a reachability guard on
/// what exists, not an assertion that every world has every landform.
///
/// **Fix round 1 dropped the third landform (river delta) outright** —
/// its glyph collided with the sim's own impedance ladder, and unlike
/// highland/impedance-4 a delta has no shared concept to merge with; see
/// `clients/game/core/src/register.rs`'s own `REGISTER` doc. Two landforms
/// remain: volcano and waterfall. `checked` guards the SKIP-not-fail
/// discipline from decaying into a vacuous pass — a future world (or a
/// pin change) that happened to have neither would otherwise let this
/// test exit green having asserted nothing at all, which is exactly the
/// silent-pass shape the reachability guard exists to rule out.
#[test]
fn seed_42_draws_at_least_one_of_each_landform_it_actually_has() {
    let (terrain, geo) = test_world();
    let index = NearestVertexIndex::new(&geo);
    let f = mercator::frame_for(false);
    let (w, h) = (32u16, 16u16);
    let mut checked = 0u32;

    // Volcanoes are assembled at the composition root
    // (`hornvale_worldgen::gazetteer_features`), not by `domains/terrain`
    // alone — see that crate's own doc on why `FeatureClass::Volcano`
    // cannot be built there.
    let features = hornvale_worldgen::gazetteer_features(Seed(42), &geo, &terrain);
    let volcano_anchor = features
        .iter()
        .find(|feat| feat.id.class == FeatureClass::Volcano)
        .map(|feat| feat.anchor);

    if let Some(site) = volcano_anchor {
        checked += 1;
        let volcanoes: BTreeSet<Vertex> = std::iter::once(site).collect();
        let mut discovered = Discovered::default();
        discovered.record(FeatureId::Extent(hornvale_terrain::landscape::FeatureId {
            class: FeatureClass::Volcano,
            vertex: site,
        }));
        let found =
            drawn_at_some_shipped_rung(&geo, &f, w, h, site, plate::VOLCANO_GLYPH, |win, _, _| {
                plate::draw_with(
                    &terrain,
                    &geo,
                    &index,
                    &f,
                    win,
                    w,
                    h,
                    false,
                    &[],
                    &volcanoes,
                    &[],
                    &discovered,
                )
            });
        assert!(
            found,
            "seed 42 has a volcano at {site:?} but its glyph never appeared at any shipped rung"
        );
    }

    if let Some(&site) = terrain.waterfalls().first() {
        checked += 1;
        let waterfalls = [site];
        let found = drawn_at_some_shipped_rung(
            &geo,
            &f,
            w,
            h,
            site,
            plate::WATERFALL_GLYPH,
            |win, _, _| {
                plate::draw_with(
                    &terrain,
                    &geo,
                    &index,
                    &f,
                    win,
                    w,
                    h,
                    false,
                    &[],
                    &BTreeSet::new(),
                    &waterfalls,
                    &Discovered::default(),
                )
            },
        );
        assert!(
            found,
            "seed 42 has a waterfall at {site:?} but its glyph never appeared at any shipped rung"
        );
    }

    assert!(
        checked > 0,
        "neither landform this test knows about (volcano, waterfall) is present on seed 42 — \
         a SKIP-not-fail guard with nothing left to skip is a vacuous pass, not a green result"
    );
}
