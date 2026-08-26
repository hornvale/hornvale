//! The world plate: the whole-planet Mercator backdrop, drawn in `bin`
//! (never `core`, which carries no hornvale crate — see
//! `hornvale-game-core`'s `Cargo.toml`) and handed to `core` as an
//! already-rendered [`hornvale_game_core::Grid`].
//!
//! [`draw`]/[`draw_with`] paint exactly one thing: land vs ocean, one
//! glyph per grid cell. Each screen cell takes the AREA-MAJORITY of the
//! terrain vertices its own footprint covers, not merely the nearest vertex to
//! its centre — Nathan's ruling, fix round 1 of this task, after the
//! task report's H1' investigation found nearest-vertex sampling
//! (equivalent to a footprint of exactly one query point) made a
//! coastline character land-or-water at random at the 40x20 floor rung,
//! where roughly 51 real terrain vertices sit behind every character. See
//! [`SUBSAMPLES_PER_AXIS`]'s own doc for how the footprint is sampled,
//! and the task report's "fix round 1" section for the H1'' measurement
//! this produced.
//!
//! The glyph vocabulary (`~` ocean, `.` land) is the spike's own
//! (`windows/worldgen/examples/portolan_spike.rs`, `glyph_for`, line
//! 171 -- deleted at this campaign's close, git history at `0292de87f^`)
//! — reused rather than invented — even though the vertex-lookup
//! mechanism underneath it is not: `glyph_for` asked
//! [`hornvale_terrain::GeneratedTerrain::nearest_vertex`], an O(vertex count)
//! brute-force scan the spike didn't have to care about (it renders three
//! static frames and exits). This module asks
//! [`hornvale_kernel::NearestVertexIndex`] instead — the real-code idiom
//! `bin/src/driver.rs` already established for exactly this "screen
//! position to nearest terrain vertex" query
//! (`Driver::resolve_walk_band`) — and `NearestVertexIndex::nearest`'s own
//! doc guarantees it returns the bit-identical vertex the brute-force scan
//! would (same max dot product, same first-in-Vertex-order tie-break), so
//! this is a performance choice, not a behavioural one.
//!
//! **The index is built ONCE by the caller and passed in, never rebuilt
//! per call.** Fix round 1's other half: point sampling already made a
//! per-call `NearestVertexIndex::new(geo)` (~200ms by the task report's own
//! measurement) wasteful, and area sampling multiplies the per-cell query
//! count by [`SUBSAMPLES_PER_AXIS`] squared, so rebuilding it inside
//! `draw_with` on every redraw (Task 3 wires this into the live redraw
//! path) would have compounded a wasteful cost into a much larger one.
//! `driver.rs` already builds its own `NearestVertexIndex` once, at
//! `Driver::start`, and reuses it for the session's lifetime
//! (`self.nearest`) — this module follows the same idiom rather than
//! caching one internally.

use hornvale_game_core::{Cell, Grid, Ink, Source, Weight};
use hornvale_kernel::{Geosphere, NearestVertexIndex, Vertex};
use hornvale_terrain::GeneratedTerrain;
use std::collections::BTreeSet;

use crate::discovery::{Discovered, FeatureId};
use crate::mercator::{self, Frame};

/// A view onto the world plate: which mesh RUNG the virtual chart is drawn
/// at, and which tile of that chart the window's own origin sits at.
///
/// **The rung is a mesh depth now, not a zoom step** (The Quadrat, Task 1).
/// The old `zoom: u8` — an index into a ladder of doublings that started
/// from the drawn plate's own width — is REPLACED, not supplemented: it made
/// the chart's size a function of the plate, which is precisely why no
/// caller could render PART of a chart
/// (`CLIENT-draw-with-cannot-render-a-subrect`). `depth` names a resolution
/// the world itself has, so two callers drawing different-sized plates at
/// the same rung are looking at the same chart.
///
/// `w`/`h` (passed to [`draw`]/[`draw_with`]) are always the DRAWN plate's
/// own size — the screen window — while `depth` picks the virtual chart that
/// window is a subrect OF, and `origin_col`/`origin_row` are that chart's
/// own coordinates, never the screen's.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Window {
    /// The mesh rung the virtual chart is drawn at —
    /// [`GLOBE_RUNG`]`..=`[`BAND_B_RUNG`] on the shipped ladder. Higher is
    /// FINER: each rung halves the facet edge, so it doubles the chart.
    /// See [`virtual_dims`] for the chart that follows from it.
    pub depth: u32,
    /// The window's origin column, in VIRTUAL chart cells (see
    /// [`virtual_dims`]), added to every drawn column before
    /// inverse-projecting. Longitude wraps, so any value names a real
    /// column.
    pub origin_col: u32,
    /// The window's origin row, in VIRTUAL chart cells (see
    /// [`virtual_dims`]), added to every drawn row before
    /// inverse-projecting. Latitude does NOT wrap: the caller keeps
    /// `origin_row + h` inside the chart's own height — see [`draw_with`]'s
    /// own polar-fabrication note.
    pub origin_row: u32,
}

/// The mesh depth band B is drawn at: the walk band, one tile per facet.
/// Matches `hornvale_vessel::agent::walk_depth` (`globe_level + 6`) for the
/// canonical globe level of 6; a world pinned to another globe level moves
/// both together.
/// type-audit: bare-ok(count)
pub const BAND_B_RUNG: u32 = 12;

/// The coarsest rung: the canonical grid level, below which the terrain
/// fields have no resolution to disclose (decision 0196).
/// type-audit: bare-ok(count)
pub const GLOBE_RUNG: u32 = 6;

/// The central angle an icosahedron's own base-face edge subtends at the
/// centre of its circumsphere, in radians — `acos(1/sqrt(5))`, about
/// 63.4349°. Every refinement level halves it, which is the whole content of
/// [`tiles_around_a_great_circle`].
///
/// Derived, never tabulated: `hornvale_kernel::math` (the `libm` route
/// `mercator.rs` already uses for every transcendental in this client) is
/// asked for the `acos` rather than a rounded literal being pasted here.
fn base_edge_rad() -> f64 {
    hornvale_kernel::math::acos(1.0 / 5.0f64.sqrt())
}

/// How many facet edges fit around a great circle at mesh depth `depth`.
///
/// **Derived from the icosahedron's own geometry, not a hardcoded ladder.**
/// A table becomes a tuned number the first time the globe level moves —
/// the mistake `hornvale_vessel::course::step_length_rad`'s own doc records
/// avoiding. The base-face edge subtends [`base_edge_rad`]; each of the
/// `depth` refinement levels halves it (the facet count is `20 << (2 *
/// depth)`, i.e. four facets per facet per level, so the edge halves), so
/// the count around a great circle is `2*pi` divided by that angle.
///
/// `2.0^depth` is computed through `math::powf` rather than a shift, so a
/// depth at or past 32 saturates instead of overflowing.
fn tiles_around_a_great_circle(depth: u32) -> u32 {
    let edge = base_edge_rad() / hornvale_kernel::math::powf(2.0, f64::from(depth));
    ((std::f64::consts::TAU / edge).round() as u32).max(1)
}

/// The virtual chart's size in TILES at `depth`, derived from the mesh alone.
///
/// **The plate's own width is deliberately not a parameter.** It used to be,
/// and that is precisely why no caller could render part of a chart
/// (`CLIENT-draw-with-cannot-render-a-subrect`): `draw_with(w = 1, ..)` drew a
/// one-column-wide whole planet rather than one column of a wide one. The
/// chart is a property of the rung; the plate is a window onto it.
///
/// Width is how many facet edges fit around a great circle at `depth`
/// ([`tiles_around_a_great_circle`]). Height follows from the CLAMPED
/// MERCATOR's own aspect — `2*mercator_y_max()/2pi`, about 0.9967, i.e.
/// nearly square — and NOT from
/// [`hornvale_game_core::spread::GLYPH_ASPECT`]. Conflating the two is
/// `MAP-vertical-axis-undersamples-the-mesh`: the glyph aspect says a tile is
/// two columns wide on a terminal, which is a fact about terminals.
///
/// **This is the ONE function both [`draw_with`] (painting) and `bin`'s
/// cursor resolver (`driver.rs`) call to turn a window position into a
/// Mercator tile** — never a second copy of this arithmetic.
///
/// **`depth` is clamped at [`BAND_B_RUNG`], and that clamp is load-bearing,
/// not tidiness.** The old body clamped `zoom` at `MAX_ZOOM` for the same
/// reason and the field is still `pub` with no validation: [`area_majority`]
/// computes `virtual_w * SUBSAMPLES_PER_AXIS` in `u32`, which overflows once
/// the chart passes ~613 million tiles — depth 27 and up. `apply_zoom` never
/// gets there, but Task 5's tile cache constructs [`Window`]s directly, so
/// "unreachable" stops being true. A COARSER-than-`GLOBE_RUNG` depth is
/// deliberately still honoured: nothing overflows downward, and
/// `driver.rs`'s own disclosure test needs a rung coarser than the mesh to
/// exercise at all.
/// type-audit: bare-ok(count: depth), bare-ok(count: return)
pub fn virtual_dims(depth: u32) -> (u32, u32) {
    let w = tiles_around_a_great_circle(depth.min(BAND_B_RUNG));
    let aspect = (2.0 * mercator::mercator_y_max()) / (2.0 * std::f64::consts::PI);
    let h = ((f64::from(w) * aspect).round() as u32).max(1);
    (w, h)
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

/// The glyph for a DISCOVERED settlement (Task 5, §A3's "point sites").
/// Never drawn undiscovered — see [`draw_point_sites`]'s own doc for why
/// "not yet drawn" is the only state an undiscovered site is ever in.
pub(crate) const SETTLEMENT_GLYPH: char = '#';
/// The glyph for a DISCOVERED cave mouth. See [`SETTLEMENT_GLYPH`].
pub(crate) const CAVE_GLYPH: char = 'o';

/// The colour claim for a discovered settlement — a warm tint distinct from
/// both terrain colours, so a settlement reads as a different SUBSTANCE
/// (§A5: colour carries substance, never the epistemic channel — an
/// undiscovered site simply is not drawn at all, so there is no
/// discovered/undiscovered pair of colours to confuse with one another).
const SETTLEMENT_COLOR: [u8; 3] = [220, 180, 60];
/// The colour claim for a discovered cave mouth. See [`SETTLEMENT_COLOR`].
const CAVE_COLOR: [u8; 3] = [130, 120, 110];

/// Sub-samples per axis for area-majority sampling: each screen cell
/// takes the majority vote of `SUBSAMPLES_PER_AXIS * SUBSAMPLES_PER_AXIS`
/// nearest-vertex queries spread evenly across its own footprint, in place
/// of the single nearest-vertex query at its centre the previous,
/// point-sampled scheme used.
///
/// **This generalises the old code rather than replacing it (Nathan's
/// own framing).** At the projection's own ceiling — roughly one screen
/// cell per terrain vertex — every sub-sample within a footprint lands on
/// the same nearest vertex, so the majority is unanimous and the answer is
/// identical to the old point-sampled one; the two are the same function
/// evaluated at different resolutions, not two code paths that happen to
/// agree.
///
/// `7` (49 samples — always odd, so a tie is impossible) approximates the
/// ~51 real terrain vertices the task report's H1' investigation measured
/// behind each character at the 40x20 floor rung (`sqrt(51) ≈ 7.1`,
/// rounded down). It is a fixed constant, not derived from
/// `GeneratedTerrain`'s actual vertex count — threading that count through
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
#[allow(clippy::too_many_arguments)] // mirrors `draw_with`'s own allow, one level up
pub fn draw(
    terrain: &GeneratedTerrain,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    f: &Frame,
    win: &Window,
    w: u16,
    h: u16,
    settlements: &BTreeSet<Vertex>,
    caves: &BTreeSet<Vertex>,
    discovered: &Discovered,
) -> Grid {
    let colour_allowed = Ink::from_wire(Some([0, 0, 0])) != Ink::Plain;
    draw_with(
        terrain,
        geo,
        index,
        f,
        win,
        w,
        h,
        colour_allowed,
        settlements,
        caves,
        discovered,
    )
}

/// [`draw`]'s pure seam: `colour_allowed` is taken as an argument rather
/// than read from the environment, so a test can drive both colour
/// regimes without touching `NO_COLOR` (the discipline
/// `clients/game/core/tests/monochrome_floor.rs` already keeps, and that
/// The Chroma's fix round `389d498de` established the need for — threaded
/// tests mutating `NO_COLOR` under each other).
///
/// `index` must be built over the SAME `geo` (`NearestVertexIndex::new(geo)`
/// — see the module doc for why it is a caller-owned parameter rather
/// than being built or cached here).
///
/// For every cell of the returned `w`x`h` grid, this samples
/// [`SUBSAMPLES_PER_AXIS`] squared points spread evenly across that
/// cell's own footprint (see that constant's doc for exactly how), looks
/// up each sample's nearest terrain vertex, and draws ocean or land by
/// MAJORITY vote across the samples.
///
/// **`w`/`h` are the drawn plate's own size — the screen window —
/// never the virtual chart's.** [`virtual_dims`]`(win.depth)` gives the
/// latter; `win.origin_row`/`origin_col` are offsets into IT, not into
/// `w`x`h`. The two used to COINCIDE at the coarsest rung, because the
/// chart was derived from `w` itself; since The Quadrat they never do, and
/// `w`x`h` is always a genuine subrect of the chart.
///
/// **No polar fabrication (spec §6) — automatic, PROVIDED the caller keeps
/// `win.origin_row` inside its own valid range.** `unproject`'s own
/// `mercator_y` formula maps a row inside `[0, virtual_h)` to a `y` value
/// strictly inside `(-y_max, y_max)` — never equal to either bound — so the
/// FRAME latitude it recovers is always strictly inside `(-LAT_CLAMP_DEG,
/// LAT_CLAMP_DEG)`, PROVIDED every visited row (the origin plus up to `h -
/// 1`) stays below `virtual_h`. `bin`'s `driver.rs` is the one place
/// `origin_row` is ever set or moved, and every site there re-clamps it via
/// [`virtual_dims`] for exactly this reason (see that module's
/// `reclamp_window`).
///
/// `origin_col` carries no such obligation: longitude WRAPS (the module
/// doc on `wrap_deg_signed`'s periodicity), so a column past the virtual
/// width still lands on a valid, correctly wrapped longitude rather than
/// an out-of-clamp one.
///
/// A runtime check on the GEOGRAPHIC latitude this function receives back
/// would additionally be the wrong test to make: `mercator::to_frame`'s
/// own doc is explicit that a locked frame's rotation is not a coordinate
/// permutation, so a point's frame latitude and its geographic latitude
/// are different numbers in that frame, and the clamp is stated on the
/// former ([`mercator::project`]'s own `None` branch checks the frame
/// latitude, never the geographic one).
///
/// **Task 5: point sites are gated HERE, not filtered afterward** — spec
/// Amendment 1 §A7's "nothing is drawn and then hidden" refusal:
/// `settlements`/`discovered` are consulted per screen cell,
/// alongside the ocean/land vote, and an undiscovered site's glyph is
/// simply never chosen — there is no suppression pass over an already-
/// painted grid, because a site never drawn cannot leak. A discovered
/// site's glyph OVERRIDES the terrain glyph at its own cell (§A3: a point
/// site "is not in the terrain render at all," unlike a terrain-borne
/// landmark, which draws regardless of discovery).
#[allow(clippy::too_many_arguments)] // `index` (fix round 1: build-once-pass-in, per Nathan's ruling) pushed this to 8; Task 5's `settlements`/`discovered` push it to 10 — mirroring `hornvale_game_core::render_with`'s own allow
pub fn draw_with(
    terrain: &GeneratedTerrain,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    f: &Frame,
    win: &Window,
    w: u16,
    h: u16,
    colour_allowed: bool,
    settlements: &BTreeSet<Vertex>,
    caves: &BTreeSet<Vertex>,
    discovered: &Discovered,
) -> Grid {
    let mut grid = Grid::new(w, h);
    let width = u32::from(w);
    let height = u32::from(h);
    let (virtual_w, virtual_h) = virtual_dims(win.depth);

    for row in 0..height {
        for col in 0..width {
            let (ocean, vertex) =
                area_majority(terrain, geo, index, f, win, virtual_w, virtual_h, row, col);
            // TERRAIN ONLY. Sites are PROJECTED in a second pass below —
            // see `draw_point_sites` for why asking each screen cell "is
            // your representative a site?" dropped 37.8% of caves.
            let _ = vertex;
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

    draw_point_sites(
        geo,
        f,
        win,
        virtual_w,
        virtual_h,
        w,
        h,
        colour_allowed,
        settlements,
        caves,
        discovered,
        &mut grid,
    );
    grid
}

/// Draw every DISCOVERED point site by PROJECTING it, rather than by asking
/// each screen cell whether its area-majority representative happens to be
/// one.
///
/// **Why the direction matters.** The sampled scheme drew a site only when
/// its exact vertex won the majority vote for some character. Measured on seed
/// 42: only 70.5% of the planet's 40,962 vertices are ever a representative at
/// any zoom or scroll position, so **37.8% of cave vertices were undrawable by
/// construction** — a player could walk into a cave the map could never show,
/// at any rung. Terrain is a texture and belongs to sampling; a settlement or
/// a cave mouth is a landmark and belongs to projection, the same way a real
/// chart draws a coastline but pins a town.
///
/// It is also far cheaper: one projection per site in the roster against
/// `width * height * 49` samples for the terrain pass.
///
/// **Undiscovered sites are never drawn**, so §A7's "nothing is drawn and
/// then hidden" still holds by construction — an undiscovered site is not
/// suppressed here, it is never reached.
///
/// Caves are drawn first and settlements second, so a settlement wins a cell
/// they share — the same precedence the sampled scheme's `if/else` gave.
#[allow(clippy::too_many_arguments)] // mirrors `draw_with`'s own parameter list, which this is lifted out of
fn draw_point_sites(
    geo: &Geosphere,
    f: &Frame,
    win: &Window,
    virtual_w: u32,
    virtual_h: u32,
    w: u16,
    h: u16,
    colour_allowed: bool,
    settlements: &BTreeSet<Vertex>,
    caves: &BTreeSet<Vertex>,
    discovered: &Discovered,
    grid: &mut Grid,
) {
    let width = u32::from(w);
    let height = u32::from(h);

    let place = |vertex: Vertex, id: FeatureId, glyph: char, color: [u8; 3], grid: &mut Grid| {
        if !discovered.contains(id) {
            return;
        }
        let g = geo.coord(vertex);
        let Some((plate_row, plate_col)) =
            mercator::project(f, g.latitude, g.longitude, virtual_w, virtual_h)
        else {
            return; // above the clamp: not on this map at all
        };
        // LONGITUDE WRAPS, LATITUDE DOES NOT — the same asymmetry
        // `move_cursor`'s scroll obeys, so a site just past the seam is
        // still on screen when the window straddles it.
        let dcol = (plate_col + virtual_w - (win.origin_col % virtual_w)) % virtual_w;
        let Some(drow) = plate_row.checked_sub(win.origin_row) else {
            return;
        };
        if dcol >= width || drow >= height {
            return;
        }
        grid.set(
            dcol as u16,
            drow as u16,
            Cell {
                glyph: Some(glyph),
                weight: Weight::Normal,
                ink: Ink::resolve(Some(color), colour_allowed),
                source: Source::World,
            },
        );
    };

    for &vertex in caves {
        place(
            vertex,
            FeatureId::Cave(vertex),
            CAVE_GLYPH,
            CAVE_COLOR,
            grid,
        );
    }
    for &vertex in settlements {
        place(
            vertex,
            FeatureId::Settlement(vertex),
            SETTLEMENT_GLYPH,
            SETTLEMENT_COLOR,
            grid,
        );
    }
}

/// The 49-point area-majority vote for ONE screen cell at `(row, col)`
/// (screen-relative, before `win.origin_row`/`origin_col` are added) —
/// SHARED by [`draw_with`] (which paints the winning class as a glyph)
/// and `bin`'s world-view resolver (Task 3b fix round 1, Finding 2: the
/// resolver must never name a vertex whose class contradicts the glyph the
/// player is looking at).
///
/// Returns whether the majority is ocean, and the [`hornvale_kernel::
/// Vertex`] of the MAJORITY-class sample nearest the screen cell's own
/// true centre — the `(3, 3)` sub-sample, since [`SUBSAMPLES_PER_AXIS`]
/// is odd (`7`) and index `3` of `0..7` is the exact centre. That
/// sub-sample's own `unproject` call is provably identical to a direct
/// single-point query at the screen cell's centre (`(row + 0.5, col +
/// 0.5)` scaled by the virtual chart, not the sub-sampled grid — the
/// `3.5 / 7 == 0.5` identity `bin`'s `driver.rs` `resolve_world_view` doc
/// works out), so this is never coarser than what the previous,
/// single-point resolver asked for — only additionally constrained to
/// agree with what got drawn. Distance-to-centre ties break on the
/// smaller `(i, j)` in row-major order: deterministic, and never reached
/// by [`draw_with`] itself (which only reads the vote tally, never which
/// sample cast it), so this matters only to the resolver.
///
/// `virtual_w`/`virtual_h` are `virtual_dims(win.depth)`'s own
/// output — passed in rather than recomputed per cell, since [`draw_with`]
/// already computes it once for the whole plate and a caller resolving a
/// single cursor position computes it once per keypress; neither needs a
/// second copy of that arithmetic per cell.
#[allow(clippy::too_many_arguments)] // mirrors `draw_with`'s own allow, one level down
pub(crate) fn area_majority(
    terrain: &GeneratedTerrain,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    f: &Frame,
    win: &Window,
    virtual_w: u32,
    virtual_h: u32,
    row: u32,
    col: u32,
) -> (bool, hornvale_kernel::Vertex) {
    let plate_row = win.origin_row + row;
    let plate_col = win.origin_col + col;
    let sub_width = virtual_w * SUBSAMPLES_PER_AXIS;
    let sub_height = virtual_h * SUBSAMPLES_PER_AXIS;

    let mut land_votes = 0u32;
    let mut ocean_votes = 0u32;
    let mut samples: Vec<(u32, u32, bool, hornvale_kernel::Vertex)> =
        Vec::with_capacity((SUBSAMPLES_PER_AXIS * SUBSAMPLES_PER_AXIS) as usize);
    for i in 0..SUBSAMPLES_PER_AXIS {
        for j in 0..SUBSAMPLES_PER_AXIS {
            let sub_row = plate_row * SUBSAMPLES_PER_AXIS + i;
            let sub_col = plate_col * SUBSAMPLES_PER_AXIS + j;
            let (lat, lon) = mercator::unproject(f, sub_row, sub_col, sub_width, sub_height);
            let vertex = index.nearest(geo, lat, lon);
            let ocean = terrain.is_ocean(vertex);
            if ocean {
                ocean_votes += 1;
            } else {
                land_votes += 1;
            }
            samples.push((i, j, ocean, vertex));
        }
    }

    // `SUBSAMPLES_PER_AXIS * SUBSAMPLES_PER_AXIS` is odd (7*7 = 49), so
    // `ocean_votes == land_votes` cannot happen and this is a true
    // majority, never a tie broken by arm order.
    let ocean = ocean_votes > land_votes;

    const CENTRE: i64 = (SUBSAMPLES_PER_AXIS / 2) as i64; // 3, the exact centre of 0..7
    let nearest = samples
        .into_iter()
        .filter(|&(_, _, sample_ocean, _)| sample_ocean == ocean)
        .map(|(i, j, _, vertex)| {
            let di = i64::from(i) - CENTRE;
            let dj = i64::from(j) - CENTRE;
            (di * di + dj * dj, i, j, vertex)
        })
        .min()
        .expect("the majority class always has at least one matching sample, by definition")
        .3;

    (ocean, nearest)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;
    use hornvale_terrain::TerrainPins;

    /// A committed-seed world, built the same way the spike builds one
    /// (`windows/worldgen/examples/portolan_spike.rs`'s own `main`,
    /// deleted at this campaign's close -- git history at `0292de87f^`)
    /// — no shared `test_world()` helper exists anywhere in `clients/game`
    /// (grepped, not merely assumed), so this is the one this module
    /// owns.
    fn test_world() -> (GeneratedTerrain, Geosphere) {
        let geo = Geosphere::new(hornvale_terrain::GLOBE_LEVEL);
        let outcome = hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default())
            .expect("default pins generate seed 42");
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        (terrain, geo)
    }

    /// A window at `depth` positioned so the virtual chart's tile
    /// `(row, col)` falls inside a `w`x`h` plate, plus the screen
    /// `(x, y)` it falls at.
    ///
    /// **This helper could not have existed before Task 1.** The chart used
    /// to be derived from the plate's own width, so the only way to get a
    /// distant chart tile on screen was to enlarge the plate until the
    /// chart grew to reach it — which is why several tests below used to
    /// draw 400x200 grids to see one cave. A plate is a SUBRECT now, so a
    /// test moves the window instead.
    fn window_showing(depth: u32, row: u32, col: u32, w: u16, h: u16) -> (Window, u16, u16) {
        let (vw, vh) = virtual_dims(depth);
        assert!(
            u32::from(w) < vw && u32::from(h) < vh,
            "window_showing wants a plate strictly smaller than the chart"
        );
        let origin_row = row.saturating_sub(u32::from(h) / 2).min(vh - u32::from(h));
        let origin_col = (col + vw - u32::from(w) / 2) % vw;
        let win = Window {
            depth,
            origin_col,
            origin_row,
        };
        (
            (win),
            ((col + vw - origin_col) % vw) as u16,
            (row - origin_row) as u16,
        )
    }

    #[test]
    fn virtual_dims_come_from_the_mesh_not_the_plate() {
        // The virtual chart's size is a property of the RUNG alone. Two callers
        // drawing different-sized plates at the same rung must agree about the
        // chart they are windows onto -- that agreement is what makes a subrect
        // meaningful, and deriving it from the plate width is what made it
        // impossible before this task.
        let (w_a, h_a) = virtual_dims(BAND_B_RUNG);
        let (w_b, h_b) = virtual_dims(BAND_B_RUNG);
        assert_eq!((w_a, h_a), (w_b, h_b));

        // Coarser rung => half the tiles. Each mesh level halves the edge length.
        // Tolerance is +/-1 IN EITHER DIRECTION because each rung rounds
        // independently: at the real numbers, rung 11 gives 11,623 and rung 12
        // gives 23,245, so doubling the coarse rung OVERSHOOTS by one. A
        // one-sided tolerance fails here, which is what the first draft of this
        // assertion did.
        let (w_coarse, _) = virtual_dims(BAND_B_RUNG - 1);
        assert!(
            (w_coarse * 2).abs_diff(w_a) <= 1,
            "rung {} gave {w_coarse} and rung {} gave {w_a}; expected a halving within 1",
            BAND_B_RUNG - 1,
            BAND_B_RUNG
        );

        // The ceiling clamp `depth` no longer validates for itself: past
        // BAND_B_RUNG the chart stops growing, so `area_majority`'s
        // `virtual_w * SUBSAMPLES_PER_AXIS` can never leave `u32`. A caller
        // building a `Window` by hand -- Task 5's tile cache -- is the one
        // that can reach here.
        assert_eq!(
            virtual_dims(BAND_B_RUNG + 20),
            (w_a, h_a),
            "the chart must stop growing at BAND_B_RUNG"
        );
    }

    #[test]
    fn the_clamped_mercator_is_nearly_square_in_tiles() {
        // MAP-vertical-axis-undersamples-the-mesh: the old code set
        // `virtual_h = virtual_w / GLYPH_ASPECT`, conflating the GLYPH aspect (a
        // screen property, 2 columns per row) with the PROJECTION aspect (a
        // geometry property). A +/-85-clamped Mercator is nearly square in
        // projected coordinates, so the vertical axis was sampled ~2x too
        // coarsely. Pin the geometry, not the glyph.
        let (w, h) = virtual_dims(BAND_B_RUNG);
        let ratio = f64::from(h) / f64::from(w);
        assert!(
            (ratio - 0.9967).abs() < 0.01,
            "clamped-Mercator aspect came out {ratio}, expected ~0.9967"
        );
    }

    /// THE TASK'S OWN POINT: a narrow plate draws a SUBRECT of a wide one,
    /// tile for tile. Before Task 1 the chart's width was the plate's width,
    /// so `draw_with(w = 8, ..)` drew an 8-column-wide whole planet rather
    /// than eight columns of a wide one, and this assertion could not have
    /// been written at all.
    ///
    /// **The origin is a mixed patch on purpose, and the vacuity guard below
    /// is what keeps it honest.** The first draft of this test sat at
    /// `(40, 20)`, which at [`GLOBE_RUNG`] is open arctic ocean: both plates
    /// came back `land = 0`, so all 64 comparisons were `Some('~')` against
    /// `Some('~')` and the test passed against a `draw_with` that ignored
    /// `w`, ignored `origin_col`, or painted a constant. Nine later tasks
    /// build on the guarantee this test is supposed to pin, so it is pinned
    /// where the two plates could actually disagree — the same guard
    /// `a_land_cell_tints_when_colour_is_allowed_and_is_plain_when_it_is_not`
    /// keeps against two identical all-`Plain` grids.
    #[test]
    fn a_narrow_plate_draws_a_subrect_of_the_wide_one() {
        let (terrain, geo) = test_world();
        let index = NearestVertexIndex::new(&geo);
        let f = mercator::frame_for(false);
        let win = Window {
            depth: GLOBE_RUNG,
            origin_col: 289,
            origin_row: 68,
        };
        let wide = draw_with(
            &terrain,
            &geo,
            &index,
            &f,
            &win,
            32,
            8,
            false,
            &BTreeSet::new(),
            &BTreeSet::new(),
            &Discovered::default(),
        );
        let narrow = draw_with(
            &terrain,
            &geo,
            &index,
            &f,
            &win,
            8,
            8,
            false,
            &BTreeSet::new(),
            &BTreeSet::new(),
            &Discovered::default(),
        );
        // THE VACUITY GUARD: the compared region must straddle a real
        // coastline. A monochrome patch makes every assertion below
        // trivially true no matter what `draw_with` does with `w`.
        let glyphs: Vec<Option<char>> = (0..8u16)
            .flat_map(|y| (0..8u16).map(move |x| (x, y)))
            .map(|(x, y)| narrow.get(x, y).and_then(|c| c.glyph))
            .collect();
        let land = glyphs.iter().filter(|g| **g == Some(LAND_GLYPH)).count();
        let ocean = glyphs.iter().filter(|g| **g == Some(OCEAN_GLYPH)).count();
        assert!(
            land > 0 && ocean > 0,
            "the compared 8x8 region must straddle a coastline or this test proves \
             nothing: land={land} ocean={ocean}"
        );

        for y in 0..8u16 {
            for x in 0..8u16 {
                assert_eq!(
                    narrow.get(x, y).map(|c| c.glyph),
                    wide.get(x, y).map(|c| c.glyph),
                    "column {x} row {y} disagreed between an 8-wide and a 32-wide plate"
                );
            }
        }
    }

    /// A settlement and a cave sharing one screen cell: the settlement
    /// wins. Precedence used to be the `if/else` order inside
    /// `point_site_at`; it is now the DRAW ORDER in `draw_point_sites`,
    /// which is easy to invert by accident and was covered by nothing
    /// after that function was removed.
    #[test]
    fn a_settlement_outranks_a_cave_on_the_same_cell() {
        let (terrain, geo) = test_world();
        let index = NearestVertexIndex::new(&geo);
        let f = crate::mercator::frame_for(false);
        let (w, h) = (32u16, 16u16);
        let (vw, vh) = virtual_dims(GLOBE_RUNG);

        // Any vertex inside the clamp will do; the WINDOW is then moved to
        // show it, rather than the plate being grown until it reaches.
        let shared = geo
            .vertices()
            .find(|&c| {
                let g = geo.coord(c);
                crate::mercator::project(&f, g.latitude, g.longitude, vw, vh).is_some()
            })
            .expect("some vertex projects inside the clamp");
        let g = geo.coord(shared);
        let (row, col) = crate::mercator::project(&f, g.latitude, g.longitude, vw, vh).unwrap();
        let (win, sx, sy) = window_showing(GLOBE_RUNG, row, col, w, h);

        let both: BTreeSet<Vertex> = std::iter::once(shared).collect();
        let mut discovered = Discovered::default();
        discovered.record(FeatureId::Settlement(shared));
        discovered.record(FeatureId::Cave(shared));

        let grid = draw_with(
            &terrain,
            &geo,
            &index,
            &f,
            &win,
            w,
            h,
            false,
            &both,
            &both,
            &discovered,
        );
        assert_eq!(
            grid.get(sx, sy).and_then(|c| c.glyph),
            Some(SETTLEMENT_GLYPH),
            "a cave overwrote a settlement on a shared cell — draw order inverted"
        );
    }

    /// THE INVERSION, AS A TEST. Sites used to be drawn by asking each
    /// SCREEN CELL "is your area-majority representative a site?" — so a
    /// site whose vertex never won a vote was never drawn at any zoom.
    /// Measured before this change: only 70.5% of vertices are ever a
    /// representative, so 37.8% of cave vertices were undrawable by
    /// construction.
    ///
    /// This test finds a cave the OLD scheme could not draw — one whose
    /// vertex is not the representative of the screen cell it falls in — and
    /// asserts it draws now. It searches rather than hardcoding a vertex, so
    /// it cannot rot into a tautology if the sampling changes.
    #[test]
    fn a_cave_that_wins_no_vote_is_still_drawn() {
        let (terrain, geo) = test_world();
        let index = NearestVertexIndex::new(&geo);
        let f = crate::mercator::frame_for(false);
        let (w, h) = (32u16, 16u16);
        let (vw, vh) = virtual_dims(GLOBE_RUNG);
        // The chart tile a vertex falls in is now plate-independent, so the
        // search below covers the WHOLE planet rather than whichever corner
        // of it a 104x52 plate happened to reach.
        let anywhere = Window {
            depth: GLOBE_RUNG,
            origin_col: 0,
            origin_row: 0,
        };

        // Every cave vertex, and the chart tile each projects into.
        let caves: BTreeSet<Vertex> = (0..geo.vertex_count())
            .map(|i| Vertex(i as u32))
            .filter(|&c| terrain.cave_at(c).is_some())
            .collect();
        assert!(!caves.is_empty(), "seed 42 must have caves to test with");

        // A cave that is NOT its own chart tile's representative: exactly
        // the case the old scheme dropped.
        let orphan = caves.iter().copied().find_map(|c| {
            let g = geo.coord(c);
            let (row, col) = crate::mercator::project(&f, g.latitude, g.longitude, vw, vh)?;
            let (_, rep) = area_majority(&terrain, &geo, &index, &f, &anywhere, vw, vh, row, col);
            (rep != c).then_some((c, row, col))
        });
        let (orphan, row, col) = orphan.expect("seed 42 must have a cave that wins no vote");
        let (win, sx, sy) = window_showing(GLOBE_RUNG, row, col, w, h);

        let mut discovered = Discovered::default();
        discovered.record(FeatureId::Cave(orphan));
        let grid = draw_with(
            &terrain,
            &geo,
            &index,
            &f,
            &win,
            w,
            h,
            false,
            &BTreeSet::new(),
            &caves,
            &discovered,
        );

        assert_eq!(
            grid.get(sx, sy).and_then(|c| c.glyph),
            Some(CAVE_GLYPH),
            "a discovered cave that wins no area-majority vote was not drawn"
        );
    }

    /// F4: at the 80x24 floor, the plate is 40 columns wide; its content
    /// height is what `spread::content_height(24)` leaves after the strip
    /// row (20).
    #[test]
    fn the_plate_fits_the_eighty_by_twenty_four_floor_exactly() {
        let (terrain, geo) = test_world();
        let index = NearestVertexIndex::new(&geo);
        let f = crate::mercator::frame_for(false);
        let win = Window {
            depth: GLOBE_RUNG,
            origin_col: 0,
            origin_row: 0,
        };
        let empty_settlements = BTreeSet::new();
        let empty_discovered = Discovered::default();
        let g = draw(
            &terrain,
            &geo,
            &index,
            &f,
            &win,
            40,
            20,
            &empty_settlements,
            &BTreeSet::new(), // no caves: this test's subject is terrain/colour
            &empty_discovered,
        );
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
        let index = NearestVertexIndex::new(&geo);
        let f = crate::mercator::frame_for(false);
        let win = Window {
            depth: GLOBE_RUNG,
            origin_col: 0,
            origin_row: 0,
        };

        let empty_settlements = BTreeSet::new();
        let empty_discovered = Discovered::default();
        let lit = draw_with(
            &terrain,
            &geo,
            &index,
            &f,
            &win,
            40,
            20,
            true,
            &empty_settlements,
            &BTreeSet::new(), // no caves: this test's subject is terrain/colour
            &empty_discovered,
        );
        let mono = draw_with(
            &terrain,
            &geo,
            &index,
            &f,
            &win,
            40,
            20,
            false,
            &empty_settlements,
            &BTreeSet::new(), // no caves: this test's subject is terrain/colour
            &empty_discovered,
        );

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
    /// vertex spacing) that every sub-sample within a cell lands on the same
    /// nearest terrain vertex — so the majority is unanimous and equals
    /// what a single centre-point query would have returned. This is the
    /// behavioural claim `SUBSAMPLES_PER_AXIS`'s own doc makes ("the two
    /// are the same function evaluated at different resolutions"),
    /// checked directly rather than only asserted in prose: every drawn
    /// cell's glyph must equal the glyph a plain nearest-vertex query at
    /// that same cell's own centre would have produced.
    #[test]
    fn at_a_fine_enough_window_area_majority_agrees_with_point_sampling() {
        let (terrain, geo) = test_world();
        let index = NearestVertexIndex::new(&geo);
        let f = crate::mercator::frame_for(false);
        // GLOBE_RUNG's chart is 363x362 -- ~3.2 tiles per terrain vertex,
        // finer than the 2 tiles per vertex the old 400x200 whole-chart
        // draw achieved, so the claim is tested at least as sharply. The
        // PLATE is 100x50 and the window is parked near the equator, where
        // Mercator's own stretch is least and the chart is closest to the
        // mesh's real spacing -- the hardest place for the two methods to
        // agree, and (since Task 1) reachable without drawing the whole
        // chart to get there.
        let (vw, vh) = virtual_dims(GLOBE_RUNG);
        let (w, h) = (100u16, 50u16);
        let win = Window {
            depth: GLOBE_RUNG,
            origin_col: 0,
            origin_row: vh / 2 - u32::from(h) / 2,
        };
        let empty_settlements = BTreeSet::new();
        let empty_discovered = Discovered::default();
        let g = draw_with(
            &terrain,
            &geo,
            &index,
            &f,
            &win,
            w,
            h,
            false,
            &empty_settlements,
            &BTreeSet::new(), // no caves: this test's subject is terrain/colour
            &empty_discovered,
        );

        let mut agree = 0u32;
        let mut total = 0u32;
        for row in 0..u32::from(h) {
            for col in 0..u32::from(w) {
                let (lat, lon) = crate::mercator::unproject(
                    &f,
                    win.origin_row + row,
                    win.origin_col + col,
                    vw,
                    vh,
                );
                let point_vertex = index.nearest(&geo, lat, lon);
                let point_ocean = terrain.is_ocean(point_vertex);
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

    // -- Task 5: point sites, gated inside `draw_with` --------------------

    /// **`draw_with` never draws an undiscovered point site, and always
    /// draws a discovered one** — the real test the discovery gate exists
    /// for (spec Amendment 1 §A3/§A7, "nothing is drawn and then hidden"):
    /// the gate lives inside the paint loop, not a filter pass afterward.
    /// Uses a real cave vertex (real terrain, not a fixture), PROJECTED to
    /// its own chart tile and then shown by moving the window there.
    ///
    /// It used to draw a 400x200 plate and SEARCH it for the screen tile
    /// whose `area_majority` vote landed on `cave_vertex` — the only way to
    /// reach a given cave when the chart was the plate. Since Task 1 the
    /// tile a site falls in is a property of the rung, so the site's
    /// position is computed, not hunted for, and the plate is 32x16.
    #[test]
    fn draw_with_gates_a_point_site_on_discovery() {
        let (terrain, geo) = test_world();
        let index = NearestVertexIndex::new(&geo);
        let f = crate::mercator::frame_for(false);
        let cave_vertex = geo
            .vertices()
            .find(|&c| terrain.cave_at(c).is_some())
            .expect("seed 42 at GLOBE_LEVEL has at least one cave vertex");
        let settlements = BTreeSet::new();
        // The cave roster this test's subject must be IN — sites are
        // PROJECTED from the roster now, not sampled for, so an empty
        // roster would make this test vacuous rather than failing.
        let caves: BTreeSet<Vertex> = std::iter::once(cave_vertex).collect();
        let (w, h) = (32u16, 16u16);
        let (virtual_w, virtual_h) = virtual_dims(GLOBE_RUNG);
        let cg = geo.coord(cave_vertex);
        let (crow, ccol) =
            crate::mercator::project(&f, cg.latitude, cg.longitude, virtual_w, virtual_h)
                .expect("the cave vertex is inside the projection's clamp");
        let (win, col, row) = window_showing(GLOBE_RUNG, crow, ccol, w, h);

        let undiscovered = Discovered::default();
        let g_before = draw_with(
            &terrain,
            &geo,
            &index,
            &f,
            &win,
            w,
            h,
            false,
            &settlements,
            &caves,
            &undiscovered,
        );
        assert_ne!(
            g_before.get(col, row).unwrap().glyph,
            Some(CAVE_GLYPH),
            "an undiscovered cave must never be drawn"
        );

        let mut discovered = Discovered::default();
        discovered.record(FeatureId::Cave(cave_vertex));
        let g_after = draw_with(
            &terrain,
            &geo,
            &index,
            &f,
            &win,
            w,
            h,
            false,
            &settlements,
            &caves,
            &discovered,
        );
        assert_eq!(
            g_after.get(col, row).unwrap().glyph,
            Some(CAVE_GLYPH),
            "a discovered cave must be drawn at its own resolved screen position"
        );
    }
}
