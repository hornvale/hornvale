//! The world plate: the whole-planet Mercator backdrop, drawn in `bin`
//! (never `core`, which carries no hornvale crate — see
//! `hornvale-game-core`'s `Cargo.toml`) and handed to `core` as an
//! already-rendered [`hornvale_game_core::Grid`].
//!
//! [`draw`]/[`draw_with`] paint exactly one thing: land vs ocean, one
//! glyph per grid cell, sampled by inverse-projecting the cell's screen
//! position ([`crate::mercator::unproject`]) to a (lat, lon) and asking
//! [`hornvale_terrain::GeneratedTerrain`] whether the nearest terrain cell
//! is ocean. This is the spike's own `glyph_for`
//! (`windows/worldgen/examples/portolan_spike.rs`, line 171) — reused
//! rather than a second glyph vocabulary invented for this task — except
//! that the cell lookup itself goes through
//! [`hornvale_kernel::NearestCellIndex`] rather than
//! [`hornvale_terrain::GeneratedTerrain::nearest_cell`]'s O(cell count)
//! brute-force scan. The spike didn't care (it renders three static
//! frames and exits); `bin/src/driver.rs` already established the indexed
//! lookup as the real-code idiom for exactly this "screen position to
//! nearest terrain cell" query (`Driver::resolve_walk_band`), and
//! `NearestCellIndex::nearest`'s own doc guarantees it returns the
//! bit-identical cell the brute-force scan would — same max dot product,
//! same first-in-CellId-order tie-break — so this is a performance
//! choice, not a behavioural one.

use hornvale_game_core::{Cell, Grid, Ink, Source, Weight};
use hornvale_kernel::{Geosphere, NearestCellIndex};
use hornvale_terrain::GeneratedTerrain;

use crate::mercator::{self, Frame};

/// A view onto the world plate: which zoom level, and which cell the
/// window's origin sits at.
///
/// The Portolan part II's Task 2 (this task) only ever constructs
/// `Window { zoom: 0, origin_col: 0, origin_row: 0 }` — the whole planet,
/// filling the requested `w`x`h` exactly — and [`draw_with`] treats `w`
/// and `h` as the FULL Mercator projection's own dimensions, so a zero
/// origin is a no-op addition. Task 3 (zoom/scroll, a paused follow-on)
/// is the first consumer that gives a nonzero origin or a `w`x`h` smaller
/// than the virtual map meaning, and defines what that means; this task
/// stores the fields and threads them into the projection call so the
/// signature does not have to change again to start reading them.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Window {
    /// The zoom level. `0` is the whole-planet view drawn today; what a
    /// higher level means is Task 3's to define.
    pub zoom: u8,
    /// The window's origin column, in plate cells, added to every drawn
    /// column before inverse-projecting. Always `0` in this task.
    pub origin_col: u32,
    /// The window's origin row, in plate cells, added to every drawn row
    /// before inverse-projecting. Always `0` in this task.
    pub origin_row: u32,
}

/// The glyph for a cell whose nearest terrain cell is ocean — the spike's
/// own vocabulary (see the module doc).
const OCEAN_GLYPH: char = '~';
/// The glyph for a cell whose nearest terrain cell is land — see
/// [`OCEAN_GLYPH`].
const LAND_GLYPH: char = '.';

/// The colour claim for an ocean cell, when colour is allowed. An
/// invented client-side palette, not a wire value: the world plate has no
/// snapshot channel to carry a colour off (see
/// [`hornvale_game_core::Source::World`]'s own doc), so this and
/// [`LAND_COLOR`] are the only two colours this module will ever draw.
const OCEAN_COLOR: [u8; 3] = [20, 60, 160];
/// The colour claim for a land cell, when colour is allowed. See
/// [`OCEAN_COLOR`].
const LAND_COLOR: [u8; 3] = [40, 120, 40];

/// Draw the whole-world Mercator plate at `w`x`h`, resolving whether
/// colour is allowed from `NO_COLOR` exactly once
/// ([`hornvale_game_core::Ink::from_wire`]'s own probe). Delegates to
/// [`draw_with`], which is the hermetically testable seam — see that
/// function's doc.
pub fn draw(
    terrain: &GeneratedTerrain,
    geo: &Geosphere,
    f: &Frame,
    win: &Window,
    w: u16,
    h: u16,
) -> Grid {
    let colour_allowed = Ink::from_wire(Some([0, 0, 0])) != Ink::Plain;
    draw_with(terrain, geo, f, win, w, h, colour_allowed)
}

/// [`draw`]'s pure seam: `colour_allowed` is taken as an argument rather
/// than read from the environment, so a test can drive both colour
/// regimes without touching `NO_COLOR` (the discipline
/// `clients/game/core/tests/monochrome_floor.rs` already keeps, and that
/// The Chroma's fix round `389d498de` established the need for — threaded
/// tests mutating `NO_COLOR` under each other).
///
/// For every cell of the returned `w`x`h` grid, [`mercator::unproject`]
/// (at `win`'s origin, added before projecting — see [`Window`]'s doc)
/// samples a (lat, lon), and the nearest terrain cell decides ocean vs
/// land.
///
/// **No polar fabrication (spec §6), and it is automatic rather than an
/// extra check here.** `unproject`'s own `mercator_y` formula maps
/// `row in [0, h)` to `y` strictly inside `(-y_max, y_max)` — never equal
/// to either bound, because `(row + 0.5)/h` is strictly inside `(0, 1)`
/// for every valid row — so the FRAME latitude it recovers is always
/// strictly inside `(-LAT_CLAMP_DEG, LAT_CLAMP_DEG)`. There is no row/col
/// in this loop for which `unproject` can hand back an out-of-clamp
/// point, so there is no cell here that needs to be left blank; a runtime
/// check on the GEOGRAPHIC latitude this function receives back would
/// additionally be the wrong test to make — `mercator::to_frame`'s own
/// doc is explicit that a locked frame's rotation is not a coordinate
/// permutation, so a point's frame latitude and its geographic latitude
/// are different numbers in that frame, and the clamp is stated on the
/// former ([`mercator::project`]'s own `None` branch checks the frame
/// latitude, never the geographic one).
pub fn draw_with(
    terrain: &GeneratedTerrain,
    geo: &Geosphere,
    f: &Frame,
    win: &Window,
    w: u16,
    h: u16,
    colour_allowed: bool,
) -> Grid {
    let mut grid = Grid::new(w, h);
    let index = NearestCellIndex::new(geo);
    let width = u32::from(w);
    let height = u32::from(h);

    for row in 0..height {
        for col in 0..width {
            let plate_row = win.origin_row + row;
            let plate_col = win.origin_col + col;
            let (lat, lon) = mercator::unproject(f, plate_row, plate_col, width, height);
            let cell = index.nearest(geo, lat, lon);
            let ocean = terrain.is_ocean(cell);
            let (glyph, color) = if ocean {
                (OCEAN_GLYPH, OCEAN_COLOR)
            } else {
                (LAND_GLYPH, LAND_COLOR)
            };
            grid.set(
                col as u16,
                row as u16,
                Cell {
                    glyph: Some(glyph),
                    weight: Weight::Normal,
                    ink: Ink::resolve(Some(color), colour_allowed),
                    source: Source::World,
                },
            );
        }
    }

    grid
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;
    use hornvale_terrain::TerrainPins;

    /// A committed-seed world, built the same way the spike builds one
    /// (`windows/worldgen/examples/portolan_spike.rs`'s own `main`) — no
    /// `test_world()` helper exists anywhere in `clients/game` (T2-d), so
    /// this is the one this module owns.
    fn test_world() -> (GeneratedTerrain, Geosphere) {
        let geo = Geosphere::new(hornvale_terrain::GLOBE_LEVEL);
        let outcome = hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default())
            .expect("default pins generate seed 42");
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        (terrain, geo)
    }

    /// F4: at the 80x24 floor, the plate is 40 columns wide; its content
    /// height is what `spread::content_height(24)` leaves after the strip
    /// row (20).
    #[test]
    fn the_plate_fits_the_eighty_by_twenty_four_floor_exactly() {
        let (terrain, geo) = test_world();
        let f = crate::mercator::frame_for(false);
        let win = Window {
            zoom: 0,
            origin_col: 0,
            origin_row: 0,
        };
        let g = draw(&terrain, &geo, &f, &win, 40, 20);
        assert_eq!(g.width(), 40);
        assert_eq!(g.height(), 20);
        assert!(
            g.provenance().get(&Source::World).copied().unwrap_or(0) > 0,
            "the plate must actually draw cells, not return an empty grid"
        );
    }

    /// Monochrome is the FLOOR: the coloured and monochrome renders draw
    /// the identical glyphs, and only the ink differs. The vacuity guard
    /// (at least one non-`Plain` ink in the coloured render) is what
    /// keeps the equality check above from passing on two identical
    /// all-Plain grids.
    #[test]
    fn a_land_cell_tints_when_colour_is_allowed_and_is_plain_when_it_is_not() {
        let (terrain, geo) = test_world();
        let f = crate::mercator::frame_for(false);
        let win = Window {
            zoom: 0,
            origin_col: 0,
            origin_row: 0,
        };

        let lit = draw_with(&terrain, &geo, &f, &win, 40, 20, true);
        let mono = draw_with(&terrain, &geo, &f, &win, 40, 20, false);

        assert_eq!(
            lit.to_plain_text(),
            mono.to_plain_text(),
            "colour may not change which glyph is drawn"
        );

        let inks = |g: &Grid| -> Vec<Ink> {
            (0..g.height())
                .flat_map(|y| (0..g.width()).filter_map(move |x| g.get(x, y).map(|c| c.ink)))
                .collect()
        };
        assert!(inks(&mono).iter().all(|i| *i == Ink::Plain));
        assert!(
            inks(&lit).iter().any(|i| matches!(i, Ink::Rgb(_))),
            "with colour allowed at least one cell must claim one — this is the VACUITY \
             guard: two identical all-Plain grids would otherwise pass the equality above"
        );
    }
}
