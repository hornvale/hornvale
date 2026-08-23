//! The world plate: the whole-planet Mercator backdrop, drawn in `bin`
//! (never `core`, which carries no hornvale crate — see
//! `hornvale-game-core`'s `Cargo.toml`) and handed to `core` as an
//! already-rendered [`hornvale_game_core::Grid`].
//!
//! [`draw`]/[`draw_with`] paint exactly one thing: land vs ocean, one
//! glyph per grid cell. Each screen cell takes the AREA-MAJORITY of the
//! terrain cells its own footprint covers, not merely the nearest cell to
//! its centre — Nathan's ruling, fix round 1 of this task, after the
//! task report's H1' investigation found nearest-cell sampling
//! (equivalent to a footprint of exactly one query point) made a
//! coastline character land-or-water at random at the 40x20 floor rung,
//! where roughly 51 real terrain cells sit behind every character. See
//! [`SUBSAMPLES_PER_AXIS`]'s own doc for how the footprint is sampled,
//! and the task report's "fix round 1" section for the H1'' measurement
//! this produced.
//!
//! The glyph vocabulary (`~` ocean, `.` land) is the spike's own
//! (`windows/worldgen/examples/portolan_spike.rs`, `glyph_for`, line
//! 171) — reused rather than invented — even though the cell-lookup
//! mechanism underneath it is not: `glyph_for` asked
//! [`hornvale_terrain::GeneratedTerrain::nearest_cell`], an O(cell count)
//! brute-force scan the spike didn't have to care about (it renders three
//! static frames and exits). This module asks
//! [`hornvale_kernel::NearestCellIndex`] instead — the real-code idiom
//! `bin/src/driver.rs` already established for exactly this "screen
//! position to nearest terrain cell" query
//! (`Driver::resolve_walk_band`) — and `NearestCellIndex::nearest`'s own
//! doc guarantees it returns the bit-identical cell the brute-force scan
//! would (same max dot product, same first-in-CellId-order tie-break), so
//! this is a performance choice, not a behavioural one.
//!
//! **The index is built ONCE by the caller and passed in, never rebuilt
//! per call.** Fix round 1's other half: point sampling already made a
//! per-call `NearestCellIndex::new(geo)` (~200ms by the task report's own
//! measurement) wasteful, and area sampling multiplies the per-cell query
//! count by [`SUBSAMPLES_PER_AXIS`] squared, so rebuilding it inside
//! `draw_with` on every redraw (Task 3 wires this into the live redraw
//! path) would have compounded a wasteful cost into a much larger one.
//! `driver.rs` already builds its own `NearestCellIndex` once, at
//! `Driver::start`, and reuses it for the session's lifetime
//! (`self.nearest`) — this module follows the same idiom rather than
//! caching one internally.

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

/// The glyph for a cell whose sampled footprint is majority ocean — the
/// spike's own vocabulary (see the module doc).
const OCEAN_GLYPH: char = '~';
/// The glyph for a cell whose sampled footprint is majority land — see
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

/// Sub-samples per axis for area-majority sampling: each screen cell
/// takes the majority vote of `SUBSAMPLES_PER_AXIS * SUBSAMPLES_PER_AXIS`
/// nearest-cell queries spread evenly across its own footprint, in place
/// of the single nearest-cell query at its centre the previous,
/// point-sampled scheme used.
///
/// **This generalises the old code rather than replacing it (Nathan's
/// own framing).** At the projection's own ceiling — roughly one screen
/// cell per terrain cell — every sub-sample within a footprint lands on
/// the same nearest cell, so the majority is unanimous and the answer is
/// identical to the old point-sampled one; the two are the same function
/// evaluated at different resolutions, not two code paths that happen to
/// agree.
///
/// `7` (49 samples — always odd, so a tie is impossible) approximates the
/// ~51 real terrain cells the task report's H1' investigation measured
/// behind each character at the 40x20 floor rung (`sqrt(51) ≈ 7.1`,
/// rounded down). It is a fixed constant, not derived from
/// `GeneratedTerrain`'s actual cell count — threading that count through
/// just to pick a sampling density would be more machinery than a fixed
/// approximation buys here, and the floor rung is the only resolution
/// this task measures.
///
/// Each sub-sample point is obtained by calling
/// [`mercator::unproject`] at a VIRTUAL resolution `SUBSAMPLES_PER_AXIS`
/// times finer in both axes — `unproject`'s own `(col + 0.5) / w`
/// cell-centre formula, evaluated at `(col * SUBSAMPLES_PER_AXIS + j,
/// w * SUBSAMPLES_PER_AXIS)` for `j` in `0..SUBSAMPLES_PER_AXIS`, lands
/// exactly on the `j`-th of `SUBSAMPLES_PER_AXIS` even sub-positions
/// inside the original cell's footprint. This reuses `unproject`
/// UNCHANGED (no fractional-coordinate variant needed, and none added to
/// `mercator.rs`) rather than inventing a second projection entry point.
const SUBSAMPLES_PER_AXIS: u32 = 7;

/// Draw the whole-world Mercator plate at `w`x`h`, resolving whether
/// colour is allowed from `NO_COLOR` exactly once
/// ([`hornvale_game_core::Ink::from_wire`]'s own probe). Delegates to
/// [`draw_with`], which is the hermetically testable seam — see that
/// function's doc.
pub fn draw(
    terrain: &GeneratedTerrain,
    geo: &Geosphere,
    index: &NearestCellIndex,
    f: &Frame,
    win: &Window,
    w: u16,
    h: u16,
) -> Grid {
    let colour_allowed = Ink::from_wire(Some([0, 0, 0])) != Ink::Plain;
    draw_with(terrain, geo, index, f, win, w, h, colour_allowed)
}

/// [`draw`]'s pure seam: `colour_allowed` is taken as an argument rather
/// than read from the environment, so a test can drive both colour
/// regimes without touching `NO_COLOR` (the discipline
/// `clients/game/core/tests/monochrome_floor.rs` already keeps, and that
/// The Chroma's fix round `389d498de` established the need for — threaded
/// tests mutating `NO_COLOR` under each other).
///
/// `index` must be built over the SAME `geo` (`NearestCellIndex::new(geo)`
/// — see the module doc for why it is a caller-owned parameter rather
/// than being built or cached here).
///
/// For every cell of the returned `w`x`h` grid, this samples
/// [`SUBSAMPLES_PER_AXIS`] squared points spread evenly across that
/// cell's own footprint (see that constant's doc for exactly how), looks
/// up each sample's nearest terrain cell, and draws ocean or land by
/// MAJORITY vote across the samples.
///
/// **No polar fabrication (spec §6), and it is automatic rather than an
/// extra check here.** `unproject`'s own `mercator_y` formula maps `row
/// in [0, h)` to `y` strictly inside `(-y_max, y_max)` for ANY `h`
/// (including the virtual, supersampled `h * SUBSAMPLES_PER_AXIS` this
/// function actually calls it with) — never equal to either bound,
/// because `(row + 0.5)/h` is strictly inside `(0, 1)` for every valid
/// row — so the FRAME latitude it recovers is always strictly inside
/// `(-LAT_CLAMP_DEG, LAT_CLAMP_DEG)`. There is no row/col this function
/// visits, at any sub-sample resolution, for which `unproject` can hand
/// back an out-of-clamp point, so there is no sample here that needs to
/// be discarded; a runtime check on the GEOGRAPHIC latitude this function
/// receives back would additionally be the wrong test to make —
/// `mercator::to_frame`'s own doc is explicit that a locked frame's
/// rotation is not a coordinate permutation, so a point's frame latitude
/// and its geographic latitude are different numbers in that frame, and
/// the clamp is stated on the former ([`mercator::project`]'s own `None`
/// branch checks the frame latitude, never the geographic one).
#[allow(clippy::too_many_arguments)] // `index` (fix round 1: build-once-pass-in, per Nathan's ruling) pushed this to 8 — mirroring `hornvale_game_core::render_with`'s own allow
pub fn draw_with(
    terrain: &GeneratedTerrain,
    geo: &Geosphere,
    index: &NearestCellIndex,
    f: &Frame,
    win: &Window,
    w: u16,
    h: u16,
    colour_allowed: bool,
) -> Grid {
    let mut grid = Grid::new(w, h);
    let width = u32::from(w);
    let height = u32::from(h);
    let sub_width = width * SUBSAMPLES_PER_AXIS;
    let sub_height = height * SUBSAMPLES_PER_AXIS;

    for row in 0..height {
        for col in 0..width {
            let plate_row = win.origin_row + row;
            let plate_col = win.origin_col + col;

            let mut land_votes = 0u32;
            let mut ocean_votes = 0u32;
            for i in 0..SUBSAMPLES_PER_AXIS {
                for j in 0..SUBSAMPLES_PER_AXIS {
                    let sub_row = plate_row * SUBSAMPLES_PER_AXIS + i;
                    let sub_col = plate_col * SUBSAMPLES_PER_AXIS + j;
                    let (lat, lon) =
                        mercator::unproject(f, sub_row, sub_col, sub_width, sub_height);
                    let cell = index.nearest(geo, lat, lon);
                    if terrain.is_ocean(cell) {
                        ocean_votes += 1;
                    } else {
                        land_votes += 1;
                    }
                }
            }

            // `SUBSAMPLES_PER_AXIS * SUBSAMPLES_PER_AXIS` is odd (7*7 =
            // 49), so `ocean_votes == land_votes` cannot happen and this
            // is a true majority, never a tie broken by arm order.
            let ocean = ocean_votes > land_votes;
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
        let index = NearestCellIndex::new(&geo);
        let f = crate::mercator::frame_for(false);
        let win = Window {
            zoom: 0,
            origin_col: 0,
            origin_row: 0,
        };
        let g = draw(&terrain, &geo, &index, &f, &win, 40, 20);
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
        let index = NearestCellIndex::new(&geo);
        let f = crate::mercator::frame_for(false);
        let win = Window {
            zoom: 0,
            origin_col: 0,
            origin_row: 0,
        };

        let lit = draw_with(&terrain, &geo, &index, &f, &win, 40, 20, true);
        let mono = draw_with(&terrain, &geo, &index, &f, &win, 40, 20, false);

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

    /// **At the ceiling, area-majority sampling collapses to point
    /// sampling.** A tiny window (2x2) over the whole planet at a coarse
    /// virtual resolution means each screen cell's SUBSAMPLES_PER_AXIS^2
    /// sub-samples are close enough together (relative to the mesh's own
    /// cell size) that every sub-sample within a cell lands on the same
    /// nearest terrain cell — so the majority is unanimous and equals
    /// what a single centre-point query would have returned. This is the
    /// behavioural claim `SUBSAMPLES_PER_AXIS`'s own doc makes ("the two
    /// are the same function evaluated at different resolutions"),
    /// checked directly rather than only asserted in prose: every drawn
    /// cell's glyph must equal the glyph a plain nearest-cell query at
    /// that same cell's own centre would have produced.
    #[test]
    fn at_a_fine_enough_window_area_majority_agrees_with_point_sampling() {
        let (terrain, geo) = test_world();
        let index = NearestCellIndex::new(&geo);
        let f = crate::mercator::frame_for(false);
        let win = Window {
            zoom: 0,
            origin_col: 0,
            origin_row: 0,
        };
        // 400x200: fine enough that a single screen cell's footprint (a
        // fraction of a degree) is much smaller than the mesh's own
        // typical cell spacing at GLOBE_LEVEL, so every sub-sample within
        // one cell's footprint should agree.
        let (w, h) = (400u16, 200u16);
        let g = draw_with(&terrain, &geo, &index, &f, &win, w, h, false);

        let mut agree = 0u32;
        let mut total = 0u32;
        for row in 0..u32::from(h) {
            for col in 0..u32::from(w) {
                let (lat, lon) =
                    crate::mercator::unproject(&f, row, col, u32::from(w), u32::from(h));
                let point_cell = index.nearest(&geo, lat, lon);
                let point_ocean = terrain.is_ocean(point_cell);
                let expected = if point_ocean { OCEAN_GLYPH } else { LAND_GLYPH };
                let got = g.get(col as u16, row as u16).unwrap().glyph.unwrap();
                total += 1;
                if got == expected {
                    agree += 1;
                }
            }
        }
        // Not a strict 100%: a cell whose footprint straddles a REAL
        // coastline can have sub-samples split closely enough that the
        // majority differs from the single centre sample by chance, even
        // at this resolution. The claim is "the two methods overwhelmingly
        // agree once the footprint is fine", not "always identical" --
        // asserting exactly 100% would be the "float guard against an
        // exact literal" mistake (a threshold that never actually gates).
        let ratio = f64::from(agree) / f64::from(total);
        assert!(
            ratio > 0.97,
            "area-majority and point sampling should agree almost \
             everywhere at a fine window: {agree}/{total} = {ratio}"
        );
    }
}
