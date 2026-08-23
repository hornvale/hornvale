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
//! 171 -- deleted at this campaign's close, git history at `0292de87f^`)
//! — reused rather than invented — even though the cell-lookup
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
use hornvale_kernel::{CellId, Geosphere, NearestCellIndex};
use hornvale_terrain::GeneratedTerrain;
use std::collections::BTreeSet;

use crate::discovery::{Discovered, FeatureId};
use crate::mercator::{self, Frame};

/// A view onto the world plate: which zoom level, and which cell the
/// window's origin sits at.
///
/// The Portolan part II's Task 2 only ever constructed
/// `Window { zoom: 0, origin_col: 0, origin_row: 0 }` — the whole planet,
/// filling the requested `w`x`h` exactly — and [`draw_with`] treated `w`
/// and `h` as the FULL Mercator projection's own dimensions, so a zero
/// origin was a no-op addition. **Task 3b is the first consumer that gives
/// a nonzero origin or a zoom above `0`, and [`virtual_dims`] is where
/// that meaning is defined**: `w`/`h` (passed to [`draw`]/[`draw_with`])
/// are always the DRAWN plate's own size — the screen window — while
/// `zoom` picks how much larger a virtual chart that window scrolls
/// inside of, and `origin_col`/`origin_row` are that virtual chart's own
/// coordinates, not the screen's.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Window {
    /// The zoom level, `0..=`[`MAX_ZOOM`]. `0` is the whole planet (the
    /// virtual chart is exactly the drawn plate's own size, so there is
    /// nothing to scroll); [`MAX_ZOOM`] is one character per terrain cell
    /// (see that constant's doc). See [`virtual_dims`] for the formula.
    pub zoom: u8,
    /// The window's origin column, in VIRTUAL chart cells (see
    /// [`virtual_dims`]), added to every drawn column before
    /// inverse-projecting. `0` at `zoom == 0`, where it is the only valid
    /// value (the whole virtual chart is on screen).
    pub origin_col: u32,
    /// The window's origin row, in VIRTUAL chart cells (see
    /// [`virtual_dims`]), added to every drawn row before
    /// inverse-projecting. `0` at `zoom == 0`, where it is the only valid
    /// value (the whole virtual chart is on screen).
    pub origin_row: u32,
}

/// Task 1's own measurement: walking the equator at `GLOBE_LEVEL` (seed
/// 42) crosses 385 distinct terrain cells — not spec §4.3's ~364 estimate.
/// This is the zoom ladder's ceiling: beyond a virtual chart this wide, a
/// character would be drawing detail the mesh does not have (decision
/// 0123: disclose a resolution, never invent detail below it). Believed
/// to be a level-6-TOPOLOGY constant (the icosphere subdivision, not the
/// terrain outcome) rather than a seed-42 fact, but unconfirmed against a
/// second seed.
///
/// **This is true of the HORIZONTAL axis only, and the doc used to claim
/// it for both.** `virtual_h = virtual_w / GLYPH_ASPECT`, and Mercator
/// clamped at ±85° is nearly SQUARE in projected coordinates — so the
/// vertical axis samples roughly twice as coarsely as this ceiling
/// implies and never reaches the mesh's real detail on that axis at all.
/// Measured on seed 42, best case (finest zoom, scrolled across the WHOLE
/// virtual chart, no plate size limiting what is on screen): only 70.5%
/// of the planet's 40,962 terrain cells are EVER an `area_majority`
/// representative at all -- 29.5% are undrawable by construction on this
/// axis alone, no matter how the plate scrolls. Of the planet's 874 cave
/// cells specifically, under that same best case, 330 (37.8%) are among
/// the undrawable ones. **A real plate is worse than the best case**,
/// because it shows one screen's worth at a time rather than scrolling
/// everywhere at once: only **5.7% of all cells are ever drawable at the
/// design plate's coarsest rung** (zoom 0, no scrolling), and only **1.1%
/// at the 80x24 floor's coarsest rung** -- i.e. 94.3% and 98.9%
/// undrawable respectively, at those two sizes, at that one rung.
/// Recorded, not fixed here (registry row
/// `MAP-vertical-axis-undersamples-the-mesh`): a genuine fix widens
/// `virtual_h` independently of `GLYPH_ASPECT`'s horizontal role, which is
/// a real signature change to every function in this module that takes
/// `virtual_h`.
pub const MAX_VIRTUAL_WIDTH: u16 = 385;

/// How many zoom steps [`Window::zoom`] carries. Each step DOUBLES the
/// virtual chart's width (see [`virtual_dims`]), starting from the 80x24
/// floor's own plate width ([`hornvale_game_core::spread::PLATE_WIDTH`],
/// 40 — the narrowest plate this client ever draws) and clamped at
/// [`MAX_VIRTUAL_WIDTH`] (385): `40 << 3 == 320 < 385`, `40 << 4 == 640 >=
/// 385`, so four doublings is the smallest ladder that reaches the
/// ceiling from the floor. A wider terminal's plate (Task 3a's dynamic
/// [`hornvale_game_core::spread::world_plate_width`]) reaches the ceiling
/// in fewer of these four steps — [`virtual_dims`]'s own clamp catches
/// that — so the last one or two zoom-in presses there are no-ops rather
/// than the finest rung being unreachable; the ladder is sized for the
/// floor, not wasted on it.
pub const MAX_ZOOM: u8 = 4;

/// The full virtual chart's width and height, in Mercator grid cells, at
/// `win`'s zoom level, for a plate whose OWN drawn width is
/// `plate_width` (its height following
/// [`hornvale_game_core::spread::GLYPH_ASPECT`] — the same ratio
/// `bin`'s `Driver::world_plate` derives the drawn plate's height from).
///
/// At `zoom == 0` this returns `(plate_width, plate_width / GLYPH_ASPECT)`
/// exactly — the whole planet fits with no scrolling, by construction, no
/// matter how wide `plate_width` itself is (a test probing a resolution no
/// live terminal uses must still see this invariant hold). Each zoom step
/// doubles the width, clamped at [`MAX_VIRTUAL_WIDTH`] — or at
/// `plate_width`, whichever is larger, so the clamp only ever limits
/// GROWTH and never forces the virtual chart narrower than what is
/// already on screen.
///
/// **This is the ONE function both [`draw_with`] (painting) and `bin`'s
/// cursor resolver (`driver.rs`) call to turn a window position into a
/// Mercator cell — never a second copy of this doubling-and-clamp
/// arithmetic.** This campaign has already fixed the "two computations of
/// the same geometry disagree" defect twice (a stale cursor, then a
/// hardcoded plate height); H3 is exactly this defect a third time, and
/// this function is the fix.
pub fn virtual_dims(win: &Window, plate_width: u16) -> (u32, u32) {
    let plate_width = u32::from(plate_width);
    let zoom = win.zoom.min(MAX_ZOOM);
    let scaled = plate_width << zoom;
    // The ceiling is `MAX_VIRTUAL_WIDTH`, EXCEPT when the plate itself is
    // already wider than that (an oversized screen, or a test probing a
    // resolution no live terminal uses) — `zoom == 0` must always yield
    // `virtual_w == plate_width` exactly (the "whole planet, no scroll"
    // invariant), even past 385 columns, so the effective ceiling can
    // never be smaller than `plate_width` itself. `scaled` is always
    // `>= plate_width` (zoom only grows it), so this only ever clamps
    // GROWTH, never forces `virtual_w` below the plate's own width.
    let ceiling = u32::from(MAX_VIRTUAL_WIDTH).max(plate_width);
    let virtual_w = scaled.min(ceiling);
    let virtual_h = virtual_w / u32::from(hornvale_game_core::spread::GLYPH_ASPECT);
    (virtual_w, virtual_h)
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
/// Never drawn undiscovered — see [`point_site_at`]'s own doc for why
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

/// Which point site, if any, stands at `cell` — and whether the possession
/// has discovered it (§A4b: encountering the THING, never the ground it
/// stands on). Settlement locations are precomputed once by the caller
/// (`Driver::start`, the same "derive once, never per-turn" discipline
/// `CellFeatureIndex` itself follows); cave presence is asked directly of
/// `terrain.cave_at`, which is already an O(1) pure function of the cell —
/// no second precomputed roster needed for it.
///
/// **Settlement checked before cave**, deterministically: the two rosters
/// are drawn from disjoint sources (the ledger's settlement facts vs. the
/// terrain's own cave process) and could in principle name the same cell:
/// this ordering is arbitrary but fixed, so two calls with the same inputs
/// always agree.
///
/// Returns `None` for a cell with no point site at all, `Some((id, false))`
/// for one not yet discovered (never drawn — see [`draw_with`]'s own
/// doc), and `Some((id, true))` for one to actually paint.
fn point_site_at(
    terrain: &GeneratedTerrain,
    settlements: &BTreeSet<CellId>,
    discovered: &Discovered,
    cell: CellId,
) -> Option<(FeatureId, bool)> {
    let id = if settlements.contains(&cell) {
        FeatureId::Settlement(cell)
    } else if terrain.cave_at(cell).is_some() {
        FeatureId::Cave(cell)
    } else {
        return None;
    };
    Some((id, discovered.contains(id)))
}

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
#[allow(clippy::too_many_arguments)] // mirrors `draw_with`'s own allow, one level up
pub fn draw(
    terrain: &GeneratedTerrain,
    geo: &Geosphere,
    index: &NearestCellIndex,
    f: &Frame,
    win: &Window,
    w: u16,
    h: u16,
    settlements: &BTreeSet<CellId>,
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
/// **`w`/`h` are the drawn plate's own size — the screen window —
/// never the virtual chart's.** [`virtual_dims`]`(win, w)` gives the
/// latter; `win.origin_row`/`origin_col` are offsets into IT, not into
/// `w`x`h`. Task 2 (before Task 3b) only ever drew at `win.zoom == 0`,
/// where the two coincide, so this distinction was invisible until zoom
/// and scroll existed to tell them apart.
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
    index: &NearestCellIndex,
    f: &Frame,
    win: &Window,
    w: u16,
    h: u16,
    colour_allowed: bool,
    settlements: &BTreeSet<CellId>,
    discovered: &Discovered,
) -> Grid {
    let mut grid = Grid::new(w, h);
    let width = u32::from(w);
    let height = u32::from(h);
    let (virtual_w, virtual_h) = virtual_dims(win, w);

    for row in 0..height {
        for col in 0..width {
            let (ocean, cell) =
                area_majority(terrain, geo, index, f, win, virtual_w, virtual_h, row, col);
            let (glyph, color) = match point_site_at(terrain, settlements, discovered, cell) {
                Some((FeatureId::Settlement(_), true)) => (SETTLEMENT_GLYPH, SETTLEMENT_COLOR),
                Some((FeatureId::Cave(_), true)) => (CAVE_GLYPH, CAVE_COLOR),
                // Undiscovered (or no site at all): terrain, exactly as
                // before Task 5 — never suppressed, never overridden.
                _ if ocean => (OCEAN_GLYPH, OCEAN_COLOR),
                _ => (LAND_GLYPH, LAND_COLOR),
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

/// The 49-point area-majority vote for ONE screen cell at `(row, col)`
/// (screen-relative, before `win.origin_row`/`origin_col` are added) —
/// SHARED by [`draw_with`] (which paints the winning class as a glyph)
/// and `bin`'s world-view resolver (Task 3b fix round 1, Finding 2: the
/// resolver must never name a cell whose class contradicts the glyph the
/// player is looking at).
///
/// Returns whether the majority is ocean, and the [`hornvale_kernel::
/// CellId`] of the MAJORITY-class sample nearest the screen cell's own
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
/// `virtual_w`/`virtual_h` are `virtual_dims(win, plate_width)`'s own
/// output — passed in rather than recomputed per cell, since [`draw_with`]
/// already computes it once for the whole plate and a caller resolving a
/// single cursor position computes it once per keypress; neither needs a
/// second copy of that arithmetic per cell.
#[allow(clippy::too_many_arguments)] // mirrors `draw_with`'s own allow, one level down
pub(crate) fn area_majority(
    terrain: &GeneratedTerrain,
    geo: &Geosphere,
    index: &NearestCellIndex,
    f: &Frame,
    win: &Window,
    virtual_w: u32,
    virtual_h: u32,
    row: u32,
    col: u32,
) -> (bool, hornvale_kernel::CellId) {
    let plate_row = win.origin_row + row;
    let plate_col = win.origin_col + col;
    let sub_width = virtual_w * SUBSAMPLES_PER_AXIS;
    let sub_height = virtual_h * SUBSAMPLES_PER_AXIS;

    let mut land_votes = 0u32;
    let mut ocean_votes = 0u32;
    let mut samples: Vec<(u32, u32, bool, hornvale_kernel::CellId)> =
        Vec::with_capacity((SUBSAMPLES_PER_AXIS * SUBSAMPLES_PER_AXIS) as usize);
    for i in 0..SUBSAMPLES_PER_AXIS {
        for j in 0..SUBSAMPLES_PER_AXIS {
            let sub_row = plate_row * SUBSAMPLES_PER_AXIS + i;
            let sub_col = plate_col * SUBSAMPLES_PER_AXIS + j;
            let (lat, lon) = mercator::unproject(f, sub_row, sub_col, sub_width, sub_height);
            let cell = index.nearest(geo, lat, lon);
            let ocean = terrain.is_ocean(cell);
            if ocean {
                ocean_votes += 1;
            } else {
                land_votes += 1;
            }
            samples.push((i, j, ocean, cell));
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
        .map(|(i, j, _, cell)| {
            let di = i64::from(i) - CENTRE;
            let dj = i64::from(j) - CENTRE;
            (di * di + dj * dj, i, j, cell)
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
        let index = NearestCellIndex::new(&geo);
        let f = crate::mercator::frame_for(false);
        let win = Window {
            zoom: 0,
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
            &empty_discovered,
        );

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

    // -- Task 5: point sites, gated inside `draw_with` --------------------

    /// [`point_site_at`] on a cell with no settlement and no cave: `None`,
    /// regardless of discovery state (there is nothing to discover).
    #[test]
    fn point_site_at_a_bare_cell_is_none() {
        let (terrain, _geo) = test_world();
        let settlements = BTreeSet::new();
        let discovered = Discovered::default();
        // A cell measured, against this module's own fixture world above, to
        // carry neither a settlement (empty `settlements`) nor a cave.
        let bare = CellId(0);
        // Sanity: pick a cell `cave_at` genuinely refuses, so this test
        // does not accidentally exercise the cave arm.
        let bare = (0..)
            .map(CellId)
            .find(|&c| terrain.cave_at(c).is_none())
            .unwrap_or(bare);
        assert_eq!(
            point_site_at(&terrain, &settlements, &discovered, bare),
            None
        );
    }

    /// A settlement cell reads `Some((Settlement, false))` before
    /// discovery and `Some((Settlement, true))` after — [`point_site_at`]
    /// never invents or withholds the SITE's existence, only whether it is
    /// drawn (that gate is `draw_with`'s own job, tested below).
    #[test]
    fn point_site_at_a_settlement_reflects_discovery() {
        let (terrain, _geo) = test_world();
        let cell = CellId(5);
        let mut settlements = BTreeSet::new();
        settlements.insert(cell);
        let mut discovered = Discovered::default();

        assert_eq!(
            point_site_at(&terrain, &settlements, &discovered, cell),
            Some((FeatureId::Settlement(cell), false))
        );
        discovered.record(FeatureId::Settlement(cell));
        assert_eq!(
            point_site_at(&terrain, &settlements, &discovered, cell),
            Some((FeatureId::Settlement(cell), true))
        );
    }

    /// A cave cell (found in real seed-42 terrain, since `cave_at` is a
    /// real derivation, not a fixture) reads `Some((Cave, _))`, tracking
    /// discovery the same way a settlement does.
    #[test]
    fn point_site_at_a_cave_reflects_discovery() {
        let (terrain, geo) = test_world();
        let cave_cell = geo
            .cells()
            .find(|&c| terrain.cave_at(c).is_some())
            .expect("seed 42 at GLOBE_LEVEL has at least one cave cell");
        let settlements = BTreeSet::new();
        let mut discovered = Discovered::default();

        assert_eq!(
            point_site_at(&terrain, &settlements, &discovered, cave_cell),
            Some((FeatureId::Cave(cave_cell), false))
        );
        discovered.record(FeatureId::Cave(cave_cell));
        assert_eq!(
            point_site_at(&terrain, &settlements, &discovered, cave_cell),
            Some((FeatureId::Cave(cave_cell), true))
        );
    }

    /// When a cell is BOTH a settlement and carries a cave (an edge case
    /// the two independent sources could in principle collide on),
    /// settlement wins deterministically — see [`point_site_at`]'s own
    /// doc for why the ordering is arbitrary but fixed.
    #[test]
    fn point_site_at_prefers_settlement_over_cave_on_collision() {
        let (terrain, geo) = test_world();
        let cave_cell = geo
            .cells()
            .find(|&c| terrain.cave_at(c).is_some())
            .expect("seed 42 at GLOBE_LEVEL has at least one cave cell");
        let mut settlements = BTreeSet::new();
        settlements.insert(cave_cell);
        let discovered = Discovered::default();

        assert_eq!(
            point_site_at(&terrain, &settlements, &discovered, cave_cell),
            Some((FeatureId::Settlement(cave_cell), false)),
            "a cell that is both a settlement and a cave resolves to the settlement"
        );
    }

    /// **`draw_with` never draws an undiscovered point site, and always
    /// draws a discovered one** — the real test the discovery gate exists
    /// for (spec Amendment 1 §A3/§A7, "nothing is drawn and then hidden"):
    /// the gate lives inside the paint loop, not a filter pass afterward.
    /// Uses a real cave cell (real terrain, not a
    /// fixture) at a window fine enough that `area_majority`'s vote
    /// collapses to point sampling (the same technique
    /// `at_a_fine_enough_window_area_majority_agrees_with_point_sampling`
    /// already establishes), so the screen position resolving to
    /// `cave_cell` is found by direct search rather than assumed.
    #[test]
    fn draw_with_gates_a_point_site_on_discovery() {
        let (terrain, geo) = test_world();
        let index = NearestCellIndex::new(&geo);
        let f = crate::mercator::frame_for(false);
        let win = Window {
            zoom: 0,
            origin_col: 0,
            origin_row: 0,
        };
        let cave_cell = geo
            .cells()
            .find(|&c| terrain.cave_at(c).is_some())
            .expect("seed 42 at GLOBE_LEVEL has at least one cave cell");
        let settlements = BTreeSet::new();
        let (w, h) = (400u16, 200u16);
        let (virtual_w, virtual_h) = virtual_dims(&win, w);

        // Find the screen position area_majority resolves to `cave_cell`.
        let mut found = None;
        'search: for row in 0..u32::from(h) {
            for col in 0..u32::from(w) {
                let (_ocean, cell) = area_majority(
                    &terrain, &geo, &index, &f, &win, virtual_w, virtual_h, row, col,
                );
                if cell == cave_cell {
                    found = Some((row, col));
                    break 'search;
                }
            }
        }
        let (row, col) = found.expect("cave_cell's own screen position is on this fine a plate");

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
            &undiscovered,
        );
        assert_ne!(
            g_before.get(col as u16, row as u16).unwrap().glyph,
            Some(CAVE_GLYPH),
            "an undiscovered cave must never be drawn"
        );

        let mut discovered = Discovered::default();
        discovered.record(FeatureId::Cave(cave_cell));
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
            &discovered,
        );
        assert_eq!(
            g_after.get(col as u16, row as u16).unwrap().glyph,
            Some(CAVE_GLYPH),
            "a discovered cave must be drawn at its own resolved screen position"
        );
    }
}
