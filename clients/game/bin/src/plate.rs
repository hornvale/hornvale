//! The world plate: the whole-planet Mercator backdrop, drawn in `bin`
//! (never `core`, which carries no hornvale crate — see
//! `hornvale-game-core`'s `Cargo.toml`) and handed to `core` as an
//! already-rendered [`hornvale_game_core::Grid`].
//!
//! [`draw`]/[`draw_with`] paint exactly one thing: land vs ocean, one
//! glyph per grid cell. Since The Quadrat a tile is a MESH FACET and its
//! terrain is read from that facet's own grid-level triangle by direct
//! addressing — [`terrain_at_tile`] — never by resampling the screen.
//!
//! **This replaces 49-point area-majority sampling, and the premise that
//! justified that sampling is the thing that expired.** Area majority was
//! Nathan's ruling in The Portolan's fix round 1, because nearest-vertex
//! sampling made a coastline character land-or-water at random at the
//! 40x20 floor rung, where roughly 51 real terrain vertices sat behind
//! every character. Task 1 made the chart a property of the RUNG rather
//! than of the drawn plate, and the coarsest shipped rung
//! ([`GLOBE_RUNG`]) is now a 363x362 chart against 40,962 vertices —
//! about 3.2 tiles per vertex. There is no longer any shipped rung at
//! which one character stands for many vertices, which is the same fact
//! `Driver::resolution_disclosure` already reports by staying silent at
//! every rung the raster draws. A footprint vote over a footprint smaller
//! than a mesh cell buys nothing and costs 49 spatial searches.
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
//! per call.** Fix round 1's other half, and still true even though the
//! index is barely consulted now: a per-call `NearestVertexIndex::new(geo)`
//! costs ~200 ms by The Portolan's own measurement, which would dwarf the
//! whole draw. `driver.rs` builds its own once, at `Driver::start`, and
//! reuses it for the session's lifetime (`self.nearest`) — this module
//! follows the same idiom rather than caching one internally.
//!
//! **What the index is still FOR is one question, asked a few dozen times
//! per plate instead of a few million: which three `Vertex` ids are the
//! corners of a grid-level triangle.** That is
//! [`hornvale_kernel::Facet::corner_weights`]'s three
//! `nearest_to_position` scans, and it is memoized per grid-level facet
//! through a [`hornvale_kernel::RoomMeshMemo`] [`draw_with`] owns for the
//! length of one draw. Thousands of band-B tiles share one grid-level
//! ancestor, so the scan count collapses from `w * h * 49` to roughly the
//! number of grid-level facets the plate covers.

use hornvale_game_core::{Cell, Grid, Ink, Source, Weight};
use hornvale_kernel::{
    Facet, FacetId, Geosphere, NearestVertexIndex, RoomMeshMemo, Value, Vertex, World,
};
use hornvale_terrain::GeneratedTerrain;
use std::collections::{BTreeMap, BTreeSet};

use crate::discovery::{Discovered, FeatureId};
use crate::mercator::{self, Frame};

/// Every placed settlement's nearest terrain vertex — every subject
/// carrying [`hornvale_settlement::IS_SETTLEMENT`], resolved through its own
/// committed `LATITUDE`/`LONGITUDE` to the [`Vertex`] [`NearestVertexIndex::nearest`]
/// says is closest. A settlement missing either coordinate fact is skipped
/// rather than guessed at (genesis always commits the pair together; a
/// hand-built world might not, and this must not panic on one).
///
/// **Extracted here (fix round 1, R10) from two near-identical copies**:
/// `driver.rs`'s `Driver::start_from_world` inlined this exact sequence
/// (Task 5, The Portolan part II) before `crate::overture::atlas` needed the
/// same read for a second caller and grew its own copy. Two copies of a
/// ledger-reading idiom drift silently — the skip-on-missing rule above is
/// exactly the kind of thing one side would later "fix" without the other —
/// so this is the one copy both callers share. Lives in `plate.rs` because
/// this is where the roster is CONSUMED ([`draw_with`]'s own `settlements`
/// parameter), matching [`draw_feature_layer`]'s existing convention of
/// taking a resolved `BTreeSet<Vertex>` rather than a ledger to read.
pub fn settlements_of(
    world: &World,
    geo: &Geosphere,
    nearest: &NearestVertexIndex,
) -> BTreeSet<Vertex> {
    world
        .ledger
        .find(hornvale_settlement::IS_SETTLEMENT)
        .filter_map(|fact| {
            let lat = match world
                .ledger
                .value_of(fact.subject, hornvale_settlement::LATITUDE)
            {
                Some(Value::Number(n)) => *n,
                _ => return None,
            };
            let lon = match world
                .ledger
                .value_of(fact.subject, hornvale_settlement::LONGITUDE)
            {
                Some(Value::Number(n)) => *n,
                _ => return None,
            };
            Some(nearest.nearest(geo, lat, lon))
        })
        .collect()
}

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
    /// The mesh rung the virtual chart is drawn at. [`GLOBE_RUNG`]`..=`
    /// [`BAND_B_RUNG`] is every rung `Driver` itself ever picks, but it is
    /// not the full range a caller may construct: [`virtual_dims`]'s own doc
    /// states that a COARSER-than-`GLOBE_RUNG` depth is deliberately still
    /// honoured (`terrain_at_tile` resolves it at the grid's own level,
    /// the only level terrain exists on), and `overture::atlas`'s
    /// `fit_depth` ships depths below `GLOBE_RUNG` for exactly this reason —
    /// a small startup plate needs a coarser-than-any-`Driver`-rung chart to
    /// show the whole globe at once. Higher is FINER: each rung halves the
    /// facet edge, so it doubles the chart. See [`virtual_dims`] for the
    /// chart that follows from any given value.
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
/// not tidiness.** The field is `pub` with no validation, and Task 5's tile
/// cache constructs [`Window`]s directly, so "`apply_zoom` never gets
/// there" is not a guarantee. [`BAND_B_RUNG`] is the finest band the client
/// draws at all — past it the mesh discloses nothing new and the chart's
/// own width leaves `u32` around depth 31. A COARSER-than-`GLOBE_RUNG`
/// depth is deliberately still honoured: nothing overflows downward, and
/// `driver.rs`'s own disclosure test needs a rung coarser than the mesh to
/// exercise at all — [`terrain_at_tile`] resolves such a tile at the grid's
/// own level, which is the only level terrain exists on.
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
/// Never drawn undiscovered — see [`draw_feature_layer`]'s own doc for why
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

/// The observer's own position, on the band-B perception layer
/// ([`draw_perception_layer`]). The SAME character
/// `clients/game/core/src/chart.rs` paints for the same fact, deliberately:
/// a reader who has seen the walk-band chart must not have to learn a second
/// symbol for "you are here" when a raster appears under it.
const HERE_GLYPH: char = '@';

/// A marked facet whose mark kind this client has no world-map counterpart
/// for — an `"agent"`, and any future kind (see [`mark_glyph`] on why an
/// unrecognised kind still draws rather than vanishing).
const AGENT_GLYPH: char = '&';

/// The colour claim for the observer's own facet. Like [`SETTLEMENT_COLOR`]
/// this is an invented client-side palette entry, not a wire value: the
/// packet's own `color` field is a facet's SURFACE cover, which is the
/// terrain the raster already draws underneath, so tinting the marker with
/// it would say the marker was ground.
const HERE_COLOR: [u8; 3] = [240, 240, 240];

/// The colour claim for a marked facet. See [`HERE_COLOR`] for why the
/// wire's own per-facet colour is not used here.
const MARK_COLOR: [u8; 3] = [230, 120, 120];

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
    draw_with(
        terrain,
        geo,
        index,
        f,
        win,
        w,
        h,
        colour_allowed(),
        settlements,
        caves,
        discovered,
    )
}

/// Whether this terminal accepts colour — [`hornvale_game_core::Ink`]'s own
/// `NO_COLOR` probe, asked in exactly one place.
///
/// **Named because two callers now need the same answer**, and a second
/// spelling of it is the shape `content_height`'s own doc warns about
/// elsewhere in this crate: [`draw`] resolves it for a whole plate, and
/// `driver.rs`'s layered redraw path resolves it for the feature layer it
/// composes over a cached terrain layer. Both must agree, so neither
/// computes it. It is deliberately NOT a cache-key input anywhere:
/// `NO_COLOR` cannot change under a running process (see `PlateKey`'s
/// counterpart doc in `driver.rs`).
pub(crate) fn colour_allowed() -> bool {
    Ink::from_wire(Some([0, 0, 0])) != Ink::Plain
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
/// For every cell of the returned `w`x`h` grid, this asks
/// [`terrain_at_tile`] which mesh facet the cell's own centre falls in and
/// reads that facet's grid-level triangle by direct addressing — no
/// footprint resampling, and (past the first tile of each grid-level
/// facet) no spatial search. The [`hornvale_kernel::RoomMeshMemo`] that
/// makes the second half true is owned HERE, for the length of one draw:
/// it caches a pure function of `(Facet, Geosphere::level())`, so its
/// lifetime is a performance choice and never a correctness one, and a
/// per-draw memo keeps `draw`/`draw_with`'s public signatures unchanged
/// while still collapsing the plate's scan count.
///
/// **How MUCH it collapses is rung-conditional, and stating one figure
/// unqualified is the mistake this sentence used to make.** The memo saves
/// exactly the reuse the rung offers, which is how many tiles share a
/// grid-level facet. Measured on a 200x200 plate (`examples/rung_bench.rs`,
/// 2026-08-26), against the 1,960,000 scans the 49-point vote cost at every
/// rung: **90 at [`BAND_B_RUNG`]** (three orders of magnitude, because
/// thousands of tiles share one grid-level facet) but **88,986 at
/// [`GLOBE_RUNG`]** (about 1.4 orders, because there a chart tile is already
/// about the size of a facet and there is almost nothing to share). The
/// mechanism holds at every rung — the coarse end is still 22x — but the
/// magnitude does not, and `GLOBE_RUNG` is a shipped rung a player reaches
/// by holding `-`. The module doc states the rule this is an instance of.
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
/// **Since The Quadrat's Task 4 this function OWNS no drawing of its own:
/// it is the composition of [`draw_terrain_layer`] and
/// [`draw_feature_layer`]**, in that order, over one `Grid` and one memo.
/// The split is not tidiness — it is the invalidation argument
/// (`CLIENT-tiles-need-the-overlay-split`, spec §3): terrain and point
/// sites drew in ONE pass, so a tile keyed on the discovery version was
/// invalidated by every discovery, *the whole pyramid, for one settlement*.
/// The terrain layer's parameter list is the statement of the fix — it does
/// not take a `Discovered` at all, so no cache keyed on it can depend on
/// one.
///
/// **Task 5: point sites are gated in [`draw_feature_layer`], not filtered
/// afterward** — spec Amendment 1 §A7's "nothing is drawn and then hidden"
/// refusal: `settlements`/`discovered` are consulted before a glyph is ever
/// chosen, and an undiscovered site's glyph is simply never chosen — there
/// is no suppression pass over an already-painted grid, because a site
/// never drawn cannot leak. A discovered site's glyph OVERRIDES the terrain
/// glyph at its own cell (§A3: a point site "is not in the terrain render
/// at all," unlike a terrain-borne landmark, which draws regardless of
/// discovery).
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
    // One memo for the whole plate — see this function's own doc for why
    // its lifetime is free to be exactly this long.
    let mut memo = RoomMeshMemo::default();
    let mut grid = draw_terrain_layer(terrain, geo, index, &mut memo, f, win, w, h, colour_allowed);
    draw_feature_layer(
        &mut grid,
        geo,
        f,
        win,
        colour_allowed,
        settlements,
        caves,
        discovered,
    );
    grid
}

/// LAYER ONE: the land/ocean raster, and NOTHING else (The Quadrat, Task 4).
///
/// **The parameter list IS the interface** — this function's whole point is
/// what it cannot see. It reads only `(frame, win.depth, win.origin_col,
/// win.origin_row, w, h, colour_allowed)` plus the world's own fixed
/// terrain, so its output is a pure function of the rung and the window.
/// There is no `Discovered` parameter, no settlement roster and no cave
/// roster, which is what makes a tile keyed on `(frame, rung, tile)` sound:
/// `CLIENT-tiles-need-the-overlay-split` recorded the measured reason the
/// single pass could not be cached — a tile keyed on the discovery version
/// is invalidated by every discovery, *the whole pyramid, for one
/// settlement*. Adding a discovery-shaped argument here would silently undo
/// that; `a_discovery_does_not_change_the_terrain_layer` is the assertion
/// that says so, and it guards its own non-vacuity by first proving the
/// window it chose really does hold a site the feature layer would draw.
///
/// `memo` is the caller's, not this function's, and that is deliberate:
/// [`hornvale_kernel::RoomMeshMemo`] caches a pure function of `(Facet,
/// Geosphere::level())`, so its lifetime is a performance choice and never a
/// correctness one — Task 5's tile cache draws many tiles of one chart and
/// wants one memo across all of them, where [`draw_with`] wants one per
/// plate. See [`terrain_at_tile`] for what it saves and at which rungs.
///
/// Everything [`draw_with`]'s own doc says about `w`/`h` being the DRAWN
/// plate's size, about the polar-fabrication obligation on `win.origin_row`,
/// and about `origin_col` wrapping freely, is stated of this function: it is
/// the one that paints those cells.
#[allow(clippy::too_many_arguments)] // `index` and the caller-owned `memo` push this to 9 — mirrors `draw_with`'s own allow, one level down
pub(crate) fn draw_terrain_layer(
    terrain: &GeneratedTerrain,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    memo: &mut RoomMeshMemo,
    f: &Frame,
    win: &Window,
    w: u16,
    h: u16,
    colour_allowed: bool,
) -> Grid {
    let mut grid = Grid::new(w, h);
    let width = u32::from(w);
    let height = u32::from(h);
    let (virtual_w, virtual_h) = virtual_dims(win.depth);

    for row in 0..height {
        for col in 0..width {
            let tile = terrain_at_tile(
                terrain, geo, index, memo, f, win, virtual_w, virtual_h, row, col,
            );
            // TERRAIN ONLY. Sites are PROJECTED by `draw_feature_layer` —
            // see its doc for why asking each screen cell "is your
            // representative a site?" dropped 37.8% of caves.
            let ocean = tile.ocean;
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

/// Where a VIRTUAL chart tile lands on a `win`-scrolled plate, as
/// `(screen row, screen col)` — or `None` when it is above or left of the
/// window.
///
/// **The ONE copy of this arithmetic** (Task 6, fix round 1's M1). It was
/// written twice — [`draw_feature_layer`]'s own `place` closure and
/// [`perception_tile_on_screen`] — line for line, while the second one's doc
/// invoked the one-copy discipline. The duplication was not merely untidy:
/// the reviewer found it by accident because a single mutation hit both
/// sites at once, so the copies were actively weakening the mutation
/// evidence for each other.
///
/// LONGITUDE WRAPS, LATITUDE DOES NOT — spec §4.2's own asymmetry, the same
/// one `Driver::move_cursor`'s scroll spill obeys. A site just past the seam
/// is still on screen when the window straddles it; a row above the window
/// is simply off the plate. The RIGHT and BOTTOM bounds are deliberately
/// NOT checked here: they need the drawn plate's size, which each caller
/// reads off its own `dst` (see [`draw_feature_layer`]'s doc on why that
/// bound is never a parameter).
fn tile_on_screen(
    win: &Window,
    virtual_w: u32,
    plate_row: u32,
    plate_col: u32,
) -> Option<(u32, u32)> {
    let dcol = (plate_col + virtual_w - (win.origin_col % virtual_w)) % virtual_w;
    let drow = plate_row.checked_sub(win.origin_row)?;
    Some((drow, dcol))
}

/// LAYER TWO: every DISCOVERED point site, drawn onto `dst` by PROJECTING
/// it, rather than by asking each screen cell whether its area-majority
/// representative happens to be one.
///
/// **Why the direction matters.** The sampled scheme drew a site only when
/// its exact vertex won the majority vote for some character. Measured on seed
/// 42 **before this change**: only 70.5% of the planet's 40,962 vertices were
/// ever a representative at any zoom or scroll position, so **37.8% of cave
/// vertices were undrawable by construction** — a player could walk into a cave
/// the map could never show, at any rung. Those two figures are history, not a
/// live reading: re-measured at The Quadrat's close, after this campaign's own
/// rebuild of the rung ladder, rung 7 and every finer shipped rung reach 40,848
/// of 40,962 vertices (99.7%) and all 874 of 874 cave vertices; the coarsest
/// rung, the worst case, reaches 39,500 (96.4%) and 823 of 874 caves (94.2%).
/// The argument stands on
/// its own either way — terrain is a texture and belongs to sampling; a
/// settlement or a cave mouth is a landmark and belongs to projection, the same
/// way a real chart draws a coastline but pins a town.
///
/// It is also far cheaper: one projection per site in the roster against
/// one [`terrain_at_tile`] per screen cell (and, before The Quadrat, 49
/// nearest-vertex searches per screen cell on top of that). That cheapness
/// is why this layer is REDRAWN every frame while
/// [`draw_terrain_layer`]'s output is cached — a handful of projections is
/// not worth an invalidation key, and giving it one is exactly the defect
/// `CLIENT-tiles-need-the-overlay-split` records.
///
/// **`dst` is drawn ONTO, never replaced**, so this composes over whatever
/// the terrain layer painted (or, at Task 5, over a tile served from a
/// cache). It writes only the cells its own discovered sites project into.
///
/// **The window's SIZE is read from `dst` itself, and there is deliberately
/// no `w`/`h` parameter to disagree with it** (fix round 1, Minor 1).
/// [`hornvale_game_core::Grid::set`] silently DROPS an out-of-range write,
/// so a `dst` smaller than a passed-in `w`/`h` would lose exactly the sites
/// nearest the edge — invisibly, with a green suite. No caller could
/// disagree today, because both driver paths size through
/// `Driver::world_plate_dims`; Task 5 composes this layer over a tile
/// served from a CACHE, which is precisely where a `dst` of one size and a
/// bound of another become plausible. Deriving the bound removes the
/// disagreement rather than asserting its absence.
///
/// **Undiscovered sites are never drawn**, so §A7's "nothing is drawn and
/// then hidden" still holds by construction — an undiscovered site is not
/// suppressed here, it is never reached.
///
/// Caves are drawn first and settlements second, so a settlement wins a cell
/// they share — the same precedence the sampled scheme's `if/else` gave.
/// **`pub` rather than `pub(crate)` for the same reason
/// [`terrain_at_tile`] is** (Task 3): `examples/rung_bench.rs` is a separate
/// crate and prices this layer on its own, which is the only way to answer
/// whether the per-redraw roster scan needs a window pre-filter. A cost
/// attributed by differencing two composed plates is a subtraction of two
/// noisy numbers, not a measurement.
#[allow(clippy::too_many_arguments)] // mirrors `draw_with`'s own parameter list, which this is lifted out of
pub fn draw_feature_layer(
    dst: &mut Grid,
    geo: &Geosphere,
    f: &Frame,
    win: &Window,
    colour_allowed: bool,
    settlements: &BTreeSet<Vertex>,
    caves: &BTreeSet<Vertex>,
    discovered: &Discovered,
) {
    let width = u32::from(dst.width());
    let height = u32::from(dst.height());
    let (virtual_w, virtual_h) = virtual_dims(win.depth);

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
        // Longitude wraps, latitude does not — see [`tile_on_screen`], which
        // is now the only place that arithmetic is written.
        let Some((drow, dcol)) = tile_on_screen(win, virtual_w, plate_row, plate_col) else {
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
        place(vertex, FeatureId::Cave(vertex), CAVE_GLYPH, CAVE_COLOR, dst);
    }
    for &vertex in settlements {
        place(
            vertex,
            FeatureId::Settlement(vertex),
            SETTLEMENT_GLYPH,
            SETTLEMENT_COLOR,
            dst,
        );
    }
}

/// One FACET of the walk band's own perception packet
/// (`scene/surrounds/v2`), flattened to exactly what
/// [`draw_perception_layer`] needs to place and rank it.
///
/// **The flattening is the interface, not a convenience.** `bin` reads the
/// real `hornvale_scene::SurroundsScene` (through
/// `hornvale_vessel::Session::purview`) and this module does not: `plate.rs`
/// is handed rosters, never domain objects, exactly as
/// [`draw_feature_layer`] takes `BTreeSet<Vertex>` rather than the ledger it
/// was read out of. `driver.rs` owns the flattening (`perceived_cells`) and
/// with it the wire's field names and state strings; this module never learns
/// that a facet's epistemic state is spelled `"here"`, which is also what
/// lets the layer be tested from a literal instead of from a genesis.
///
/// `kind` is borrowed rather than owned: a band is 31 facets and is rebuilt
/// per redraw, so there is nothing here worth an allocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Perceived<'a> {
    /// The packed [`FacetId`] of this facet's own room — the ONE field that
    /// makes this layer land on the raster's own squares rather than near
    /// them. See [`draw_perception_layer`]'s doc for the measurement that
    /// ruled out the alternative.
    pub room: u64,
    /// Whether this is the observer's own facet (`state == "here"`).
    pub here: bool,
    /// The facet's dominant mark, as `(salience, kind)` — `salience` is a
    /// RANK where LOWER is more salient
    /// (`hornvale_scene::Mark::salience`), and `kind` is the wire's own
    /// mark kind (`"agent"`, `"settlement"`, `"cave"`, or a future one).
    /// `None` on a facet carrying no marks at all.
    pub mark: Option<(u32, &'a str)>,
}

/// The glyph for a marked facet of the perception packet.
///
/// The vocabulary is `bin`'s existing one wherever one already exists:
/// a settlement and a cave mouth draw exactly what
/// [`draw_feature_layer`] draws for the same substance
/// ([`SETTLEMENT_GLYPH`], [`CAVE_GLYPH`]), because a creature standing next
/// to a cave mouth and a discovered cave mouth on the world map are the same
/// thing seen through two channels, and giving them two glyphs would say
/// otherwise. An `"agent"` has no world-map counterpart — point sites are
/// terrain-fixed and an agent is not — so it gets its own.
///
/// **An UNRECOGNISED kind still draws**, and that is the same infallibility
/// `hornvale_scene::Mark::kind`'s own doc asks of a renderer ("a consumer
/// that does not recognize a kind should still render the mark"): a future
/// mark kind must not silently vanish from the map, so the fallback is the
/// generic agent glyph rather than nothing.
fn mark_glyph(kind: &str) -> char {
    match kind {
        "settlement" => SETTLEMENT_GLYPH,
        "cave" => CAVE_GLYPH,
        _ => AGENT_GLYPH,
    }
}

/// LAYER THREE: the walk band's own perception packet — the observer's
/// position and the marks standing around them — composed over the terrain
/// raster (The Quadrat, Task 6).
///
/// This is the layer `plate.rs` did not have at Task 4, and the Task 4
/// report was right that it had no input then: the surrounds packet reaches
/// `bin` through `Session::purview`, which nothing in this module had a
/// reason to touch until band B joined the raster ladder.
///
/// ## Why the placement comes from `room` and not from the wire's polar pair
///
/// Every facet of the packet carries `bearing_deg` and `distance_rad` — a
/// polar coordinate about the observer — and `core/src/chart.rs` places its
/// own chart from exactly that pair. Placing THIS layer from it was the
/// first design, and it was **replaced before it was built**. The raster's
/// tile index is `floor` of an ABSOLUTE Mercator coordinate
/// ([`mercator::project`]); a polar pair gives a RELATIVE offset, so it has
/// to be turned back into an absolute coordinate first — and `core`, which
/// was to do it, cannot: its parsed mirror of the packet drops the
/// `observer` block, and the kernel has no inverse of
/// `bearing_to`/`distance_rad_to`. Reprojecting WITHOUT that step leaves
/// which side of a tile boundary a facet lands on to the observer's own
/// **sub-tile phase**. Measured on the seed-42 band across 200 sub-tile
/// phases: best 0 of 31 marks misplaced, worst 24, mean 11.5, and only 2 of
/// the 200 phases agreed exactly.
///
/// **What that measurement is NOT.** It is not evidence the wire withholds
/// the phase — the campaign wrote that, and it is false.
/// `SurroundsObserver` carries the centre's own centroid latitude and
/// longitude, and bearing and distance run centroid to centroid, so the
/// spherical direct problem recovers every facet's absolute coordinate
/// exactly. Quantization does not stand in the way either, though it is
/// coarser than "8 digits" sounds: 8 SIGNIFICANT digits leaves ~1 cm of
/// latitude near the equator, ~11 cm near the ±85° clamp, and at worst — a
/// longitude of magnitude ~145, where 8 significant digits is only 5
/// decimals — ~1.1 m between storable values, so sub-metre rounding, against
/// a facet 1.87 km across. This route is chosen because it is EXACT AND
/// ALREADY IN THE TREE, not because the other one is impossible.
///
/// So this layer projects the SAME coordinate through the SAME function the
/// raster does: `room` unpacks to a [`Facet`]
/// ([`hornvale_kernel::FacetId::unpack`]), whose [`Facet::coord`] goes
/// through [`mercator::project`]. Agreement is by construction — one
/// projection, not two that have to agree — and there is no arithmetic here
/// for a future edit to get subtly wrong.
///
/// ## What it paints, and what it deliberately does not
///
/// The observer's [`HERE_GLYPH`] and the dominant mark of any facet carrying
/// marks. **An ordinary, unmarked facet paints NOTHING.** The raster
/// underneath already draws that ground, at a resolution the packet cannot
/// improve on, so a glyph per packet facet would obliterate the layer this
/// campaign was built to add. (`core/src/chart.rs`'s `PLACED_GLYPH` — `'+'`
/// on every placed lattice unit — exists because the walk view had no terrain
/// vocabulary; it now has one, and `chart.rs` is untouched because it is
/// still the renderer for the plate with no raster under it.)
///
/// ## Collisions
///
/// A band-B facet and a plate tile are within a small factor of each other
/// in size, so two packet facets CAN land in one tile — measured 31 facets
/// into 20 tiles on the seed-42 band. The rule is
/// `windows/scene/src/surrounds_ascii.rs::box_rank`'s, stated the same way
/// in all three of Hornvale's chart renderers and not re-invented here:
///
/// 1. **the observer never loses their own box** — the view is egocentric;
/// 2. a marked facet beats an unmarked one, and among marked facets the
///    numerically SMALLEST `salience` wins (a rank, never a magnitude);
/// 3. ties break on document order, which the producer fixes as ascending
///    `room`.
///
/// Resolved through a [`BTreeMap`] pass rather than by overdrawing, so the
/// winner is a stated rule instead of a consequence of which facet the loop
/// happened to write last.
///
/// **`dst` is drawn ONTO, never replaced**, and the window's SIZE is read
/// from `dst` itself with deliberately no `w`/`h` parameter to disagree with
/// it — both for the reasons [`draw_feature_layer`]'s own doc gives (fix
/// round 1, Minor 1).
pub(crate) fn draw_perception_layer(
    dst: &mut Grid,
    f: &Frame,
    win: &Window,
    colour_allowed: bool,
    perceived: &[Perceived<'_>],
) {
    let width = u32::from(dst.width());
    let height = u32::from(dst.height());
    let (virtual_w, virtual_h) = virtual_dims(win.depth);

    for ((drow, dcol), index) in
        perception_boxes(f, win, virtual_w, virtual_h, width, height, perceived)
    {
        let won = &perceived[index];
        let (glyph, weight, color) = if won.here {
            (HERE_GLYPH, Weight::Bold, HERE_COLOR)
        } else {
            (
                mark_glyph(won.mark.expect("a box winner is `here` or marked").1),
                Weight::Normal,
                MARK_COLOR,
            )
        };
        dst.set(
            dcol as u16,
            drow as u16,
            Cell {
                glyph: Some(glyph),
                weight,
                ink: Ink::resolve(Some(color), colour_allowed),
                // The perception packet IS the walk-band chart's channel,
                // even though a different module paints it here: this is
                // `scene/surrounds/v2` data, not world terrain, and
                // `core`'s provenance discipline reads this field to say
                // which channel a drawn glyph came off.
                source: Source::Chart,
            },
        );
    }
}

/// The ordering key [`perception_boxes`] settles a contested box with; see
/// [`draw_perception_layer`]'s doc for the clauses, in the tuple's own
/// order. Named for the same reason
/// `windows/scene/src/surrounds_ascii.rs::BoxRank` is: the tuple IS the
/// rule, so it gets a name rather than being re-read off a signature.
type BoxRank = (bool, bool, u32, usize);

/// Which perception facet owns each screen box of a `width`x`height` plate —
/// the ONE place the placement and the collision rule run, shared by
/// [`draw_perception_layer`] (which paints the winner) and `bin`'s band-B
/// resolver (which names it).
///
/// **That sharing is the point, not tidiness.** `core`'s own chart keeps the
/// identical discipline for the identical reason
/// its own chart's query and paint paths share one `boxes_of`): two copies
/// of "which facet is at this box" is how a picture
/// and a strip come to name different things, which is the defect The
/// Portolan part II's Task 3b fixed once already, in the other direction.
///
/// Only DRAWABLE facets appear — the observer's own and any facet carrying a
/// mark. An unmarked packet facet is ground the raster drew, so it is neither
/// painted nor resolvable here, and the resolver falls through to the tile
/// under the cursor. Values are indices into `perceived`.
///
/// The rank's tuple order IS the rule stated in
/// [`draw_perception_layer`]'s doc: observer, then marked-over-unmarked,
/// then smallest `salience`, then document order.
fn perception_boxes(
    f: &Frame,
    win: &Window,
    virtual_w: u32,
    virtual_h: u32,
    width: u32,
    height: u32,
    perceived: &[Perceived<'_>],
) -> BTreeMap<(u32, u32), usize> {
    let mut placed: BTreeMap<(u32, u32), (BoxRank, usize)> = BTreeMap::new();
    for (index, seen) in perceived.iter().enumerate() {
        if !seen.here && seen.mark.is_none() {
            continue;
        }
        let Some((drow, dcol)) = perception_tile_on_screen(f, win, virtual_w, virtual_h, seen.room)
        else {
            continue;
        };
        if dcol >= width || drow >= height {
            continue;
        }
        let rank = (
            !seen.here,
            seen.mark.is_none(),
            seen.mark.map_or(0, |(salience, _)| salience),
            index,
        );
        match placed.get(&(drow, dcol)) {
            Some((held, _)) if *held <= rank => {}
            _ => {
                placed.insert((drow, dcol), (rank, index));
            }
        }
    }
    placed.into_iter().map(|(at, (_, i))| (at, i)).collect()
}

/// [`perception_boxes`] for ONE screen box — which perception facet, if any,
/// the picture drew at `(row, col)` of a `width`x`height` plate. `bin`'s
/// band-B resolver's own entry point; see [`perception_boxes`] for why the
/// resolver asks this rather than re-deriving the placement.
pub(crate) fn perceived_at(
    f: &Frame,
    win: &Window,
    width: u32,
    height: u32,
    perceived: &[Perceived<'_>],
    row: u32,
    col: u32,
) -> Option<usize> {
    let (virtual_w, virtual_h) = virtual_dims(win.depth);
    perception_boxes(f, win, virtual_w, virtual_h, width, height, perceived)
        .get(&(row, col))
        .copied()
}

/// Where a packed room id lands on a `win`-scrolled plate, as
/// `(screen row, screen col)` — `None` when the facet does not unpack, is
/// above the projection's polar clamp, or sits above/left of the window.
///
/// **Shared by [`draw_perception_layer`] (which paints there) and `bin`'s
/// own band-B tests (which assert it), so the two can never disagree about
/// where a facet went** — the same one-copy discipline `core`'s own chart
/// keeps behind its single `boxes_of`.
///
/// The window offset comes from [`tile_on_screen`], shared with
/// [`draw_feature_layer`] rather than written a second time here — see that
/// function's doc for what the duplication cost before M1 removed it.
pub(crate) fn perception_tile_on_screen(
    f: &Frame,
    win: &Window,
    virtual_w: u32,
    virtual_h: u32,
    room: u64,
) -> Option<(u32, u32)> {
    let (plate_row, plate_col) = perception_tile(f, virtual_w, virtual_h, room)?;
    tile_on_screen(win, virtual_w, plate_row, plate_col)
}

/// The VIRTUAL chart tile a packed room id occupies — the absolute
/// `(row, col)` [`mercator::project`] gives for the facet's own centroid,
/// before the window's origin is subtracted. `None` when the id does not
/// unpack to a facet, or when the facet is above the projection's polar
/// clamp and so is on no chart at all.
pub(crate) fn perception_tile(
    f: &Frame,
    virtual_w: u32,
    virtual_h: u32,
    room: u64,
) -> Option<(u32, u32)> {
    let facet = FacetId(room).unpack().ok()?;
    let coord = facet.coord();
    mercator::project(f, coord.latitude, coord.longitude, virtual_w, virtual_h)
}

/// What one chart tile's terrain resolves to: the class the glyph is
/// painted from, the mesh [`Facet`] the tile IS, and a [`Vertex`] of the
/// painted class that a resolver may name.
///
/// **`ocean` and `vertex` can never contradict each other**, by
/// construction: `vertex` is chosen from the corners the class was decided
/// on, so `terrain.is_ocean(vertex) == ocean` always. That invariant is the
/// one The Portolan's fix round 1, Finding 2 bought with a 49-point vote —
/// the strip must never name a land feature on a character drawn `~` — and
/// it survives the vote's removal.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TileTerrain {
    /// Whether the tile is painted ocean.
    /// type-audit: bare-ok(flag)
    pub ocean: bool,
    /// The mesh facet the tile's own centre falls in, at the WINDOW's rung
    /// — `Facet::containing(centre, win.depth)` exactly. At
    /// [`BAND_B_RUNG`] this is the walk band's own room.
    pub facet: Facet,
    /// A grid-level [`Vertex`] of the painted class — the corner of
    /// [`Self::facet`]'s grid-level triangle nearest the tile's own centre.
    pub vertex: Vertex,
}

/// ONE chart tile's terrain, by direct mesh addressing (The Quadrat, Task
/// 3) — SHARED by [`draw_with`] (which paints the class as a glyph) and
/// `bin`'s world-view resolver (which names a feature at the vertex).
///
/// `row`/`col` are screen-relative, before `win.origin_row`/`origin_col`
/// are added. `virtual_w`/`virtual_h` are [`virtual_dims`]`(win.depth)`'s
/// own output, passed in rather than recomputed per cell.
///
/// **This is the campaign's H1 in code.** The path it replaces
/// (`area_majority`) ran [`NearestVertexIndex::nearest`] — a spatial SEARCH
/// from a point to a vertex — 49 times per tile, for an area-majority vote
/// over the tile's footprint. A rung is a mesh depth now, so:
///
/// 1. the tile's centre unprojects to a position, and
///    [`Facet::containing`] turns that position into an ADDRESS by
///    descending the mesh — no search over vertices;
/// 2. the address's grid-level ancestor names the triangle terrain is
///    actually defined on ([`hornvale_terrain::GLOBE_LEVEL`]), and
///    [`Facet::corner_weights_memo`] gives that triangle's three corner
///    vertices, memoized;
/// 3. the tile's class is the nearest of those three corners.
///
/// **Step 3 is three dot products, and it is not an approximation of the
/// old query — it is the same answer.** A point inside a grid-level
/// triangle has its nearest mesh vertex among that triangle's own three
/// corners, so this reproduces what
/// [`NearestVertexIndex::nearest`] would have returned at the tile's centre
/// without asking it. `mesh_addressing_agrees_with_the_spatial_search`
/// ASSERTS that agreement exactly — `assert_eq!(agree, total)`, 5,000 of
/// 5,000 cells. It was written to merely MEASURE a ratio, on the theory
/// that the icosphere's dual is not exactly a Voronoi diagram and a
/// seam-straddling point might disagree; no such point was found, so the
/// test asserts what it actually observed rather than leaving a threshold
/// that never gates. **Where the two DO disagree, off the shipped ladder,
/// brute force says this path is the correct one and
/// [`NearestVertexIndex::nearest`] is the wrong one** (37/37, all at
/// latitude exactly 0.0 — a latent equator-band defect in the kernel's
/// windowed scan, captured separately and deliberately not fixed here).
///
/// **The weights [`Facet::corner_weights_memo`] also returns are
/// deliberately unused.** They are the barycentric position of the ADDRESSED
/// facet's own centroid; at the grid level they are uniform (`1,1,1`), which
/// would make every tile inside one triangle identical and blocky. The tile's
/// own centre is a strictly finer thing to compare against, and comparing
/// against it costs three dot products rather than a memo key per tile.
///
/// A rung COARSER than the grid has no ancestor at the grid level, so the
/// tile's own centre is re-addressed at the grid level instead. Nothing on
/// the shipped ladder reaches there ([`GLOBE_RUNG`] is the floor and is the
/// grid level itself); `driver.rs`'s rung-5 disclosure test does.
#[allow(clippy::too_many_arguments)] // mirrors `draw_with`'s own allow, one level down
pub fn terrain_at_tile(
    terrain: &GeneratedTerrain,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    memo: &mut RoomMeshMemo,
    f: &Frame,
    win: &Window,
    virtual_w: u32,
    virtual_h: u32,
    row: u32,
    col: u32,
) -> TileTerrain {
    let plate_row = win.origin_row + row;
    let plate_col = win.origin_col + col;
    let (lat, lon) = mercator::unproject(f, plate_row, plate_col, virtual_w, virtual_h);
    let pos = hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon);
    let facet = Facet::containing(pos, win.depth);

    // The address terrain is actually defined on. `GeneratedTerrain` lives
    // on the vertices of the `Geosphere` it was generated against and has
    // nothing finer to disclose, so every rung at or below the grid resolves
    // through the grid-level triangle.
    //
    // **Read from `geo`, never written as the literal `6` or as
    // `GLOBE_RUNG`.** Today `hornvale_terrain::GLOBE_LEVEL == GLOBE_RUNG ==
    // 6`, so all three are the same number and most of this suite cannot
    // tell them apart — `the_grid_level_is_read_from_the_geosphere` builds a
    // world at a DIFFERENT level precisely so one test can.
    let grid_level = geo.depth();
    let addr = match facet.ancestor(grid_level) {
        Some(anc) => anc,
        None => Facet::containing(pos, grid_level),
    };
    let corners = addr
        .corner_weights_memo(geo, index, memo)
        .expect("a facet AT the grid's own level is never coarser than the grid");

    // The nearest of the triangle's three corners to the tile's own centre.
    // Ties break to the lower `Vertex`, the same direction
    // `NearestVertexIndex`'s own scan breaks them.
    let mut vertex = corners[0].0;
    let mut best = f64::NEG_INFINITY;
    for &(candidate, _weight) in &corners {
        let q = geo.position(candidate);
        let d = q[0] * pos[0] + q[1] * pos[1] + q[2] * pos[2];
        // Exact-equality tie detection is intentional, exactly as
        // `NearestVertexIndex::scan_at` does it: it selects the vertex a
        // strict-`>` first hit in ascending order would have.
        #[allow(clippy::float_cmp)]
        let tie = d == best;
        if d > best || (tie && candidate < vertex) {
            best = d;
            vertex = candidate;
        }
    }

    TileTerrain {
        ocean: terrain.is_ocean(vertex),
        facet,
        vertex,
    }
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
        test_world_at(hornvale_terrain::GLOBE_LEVEL)
    }

    /// [`test_world`] at an arbitrary globe level.
    ///
    /// **This parameter exists to break a three-way constant collision, not
    /// to be general.** `hornvale_terrain::GLOBE_LEVEL`, [`GLOBE_RUNG`] and
    /// the literal `6` are the same number today, so a mutation replacing
    /// `geo.depth()` inside [`terrain_at_tile`] with either of the other two
    /// is undetectable by construction — no test built on a level-6 world
    /// can tell the three apart. `the_grid_level_is_read_from_the_geosphere`
    /// is the one caller that passes anything else. `hornvale_terrain::
    /// generate` takes the geosphere as a parameter and never consults
    /// `GLOBE_LEVEL` itself, so a world at another level is a real world,
    /// not a fixture.
    fn test_world_at(level: u32) -> (GeneratedTerrain, Geosphere) {
        let geo = Geosphere::new(level);
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

        // The ceiling clamp: past BAND_B_RUNG the chart stops growing, so
        // the chart's own width can never leave `u32` and no rung finer
        // than the walk band is ever drawn. A caller building a `Window`
        // by hand -- Task 5's tile cache -- is the one that can reach here.
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
    /// `point_site_at`; it is now the DRAW ORDER in `draw_feature_layer`,
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
            let rep = terrain_at_tile(
                &terrain,
                &geo,
                &index,
                &mut RoomMeshMemo::default(),
                &f,
                &anywhere,
                vw,
                vh,
                row,
                col,
            )
            .vertex;
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

    /// **Mesh addressing answers the question the spatial search answered.**
    /// [`terrain_at_tile`] never calls [`NearestVertexIndex::nearest`]: it
    /// addresses the tile's grid-level triangle and takes the nearest of
    /// that triangle's own three corners. The claim that makes this a
    /// replacement rather than an approximation is that a point inside a
    /// grid-level triangle HAS its nearest mesh vertex among those three
    /// corners — so every drawn cell's glyph must equal the glyph a plain
    /// `nearest()` query at that same cell's own centre would produce.
    ///
    /// **Retargeted, not renamed away.** This test was
    /// `at_a_fine_enough_window_area_majority_agrees_with_point_sampling`,
    /// and it pinned the same comparison against the 49-point vote The
    /// Quadrat's Task 3 removed; the subject moved, the assertion did not.
    /// Its threshold TIGHTENED from `> 0.97` to exact equality on every
    /// cell, because the two are now the same function rather than two
    /// samplings that mostly agree — a `> 0.97` threshold against a method
    /// that agrees exactly is a gate that never gates.
    #[test]
    fn mesh_addressing_agrees_with_the_spatial_search() {
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
        // THE VACUITY GUARD. An all-ocean patch would make every
        // comparison `~` against `~`, which a gutted `terrain_at_tile`
        // painting a constant would also pass. The compared region must
        // straddle a real coastline for the equality below to discriminate.
        let land = (0..h)
            .flat_map(|y| (0..w).map(move |x| (x, y)))
            .filter(|&(x, y)| g.get(x, y).and_then(|c| c.glyph) == Some(LAND_GLYPH))
            .count();
        assert!(
            land > 0 && land < usize::from(w) * usize::from(h),
            "the compared region must straddle a coastline or this test proves \
             nothing: land={land} of {}",
            usize::from(w) * usize::from(h)
        );
        assert_eq!(
            agree, total,
            "mesh addressing must reproduce the spatial search exactly: \
             {agree}/{total} cells agreed"
        );
    }

    /// **A tile resolves to the facet that contains it, at the WINDOW's own
    /// rung.** The old path ran `index.nearest(geo, lat, lon)` -- a spatial
    /// SEARCH -- 49 times per cell. The new path asks the MESH which facet
    /// contains the tile's centre and reads that facet's grid-level corners
    /// by direct addressing. The observable difference is that the answer
    /// must agree with [`Facet::containing`] exactly, for every tile.
    ///
    /// **Both ENDS of the ladder, because one end alone is vacuous.** A
    /// first draft ran only at [`GLOBE_RUNG`], and a mutation replacing
    /// `win.depth` with `GLOBE_RUNG` inside `terrain_at_tile` SURVIVED the
    /// whole lib suite (109/109 green) -- the rung the test picked was the
    /// constant the mutation hardcoded. Sweeping both rungs kills it,
    /// because the facet's own depth is then a thing the test varies.
    #[test]
    fn a_tile_resolves_to_the_facet_that_contains_it() {
        let (terrain, geo) = test_world();
        let index = NearestVertexIndex::new(&geo);
        let mut memo = RoomMeshMemo::default();
        let f = mercator::frame_for(false);
        for depth in [GLOBE_RUNG, GLOBE_RUNG + 3, BAND_B_RUNG] {
            let win = Window {
                depth,
                origin_col: 0,
                origin_row: 0,
            };
            let (vw, vh) = virtual_dims(win.depth);
            // A DISCRIMINATION GUARD, not decoration: the assertion below is
            // vacuous if every tile lands in the same facet (a gutted
            // `terrain_at_tile` returning a constant facet would pass). Count
            // the distinct facets the sampled block actually spans.
            let mut seen: BTreeSet<String> = BTreeSet::new();
            for row in 0..4u32 {
                for col in 0..4u32 {
                    let got = terrain_at_tile(
                        &terrain, &geo, &index, &mut memo, &f, &win, vw, vh, row, col,
                    );
                    assert_eq!(
                        got.facet.depth(),
                        depth,
                        "tile ({row},{col}) resolved at the wrong rung"
                    );
                    let (lat, lon) = mercator::unproject(&f, row, col, vw, vh);
                    let pos = hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon);
                    assert_eq!(
                        got.facet,
                        Facet::containing(pos, win.depth),
                        "tile ({row},{col}) resolved to the wrong facet at rung {depth}"
                    );
                    seen.insert(format!("{:?}", got.facet));
                }
            }
            assert!(
                seen.len() > 1,
                "the sampled block at rung {depth} must span more than one facet or this \
                 test proves nothing: {} distinct facets over 16 tiles",
                seen.len()
            );
        }
    }

    /// **At the finest rung there is nothing inside a tile to sub-sample.**
    /// A tile IS a facet at [`BAND_B_RUNG`], so there is no footprint to take
    /// a majority over. Sub-sampling existed only because a COARSE tile
    /// spanned many mesh cells. This is the mechanism H1 predicts the speedup
    /// from, asserted rather than timed -- a timing assertion is a flake on a
    /// contended box.
    ///
    /// The probe counter is [`RoomMeshMemo::corner_weights_misses`], which
    /// the kernel already exposes (`kernel/src/room.rs`, built by The Forebay
    /// for this exact question). `corner_weights_memo` runs three
    /// [`NearestVertexIndex::nearest_to_position`] scans on a MISS and none on
    /// a HIT, so a miss count IS a search count, divided by three.
    #[test]
    fn the_finest_rung_needs_no_subsampling() {
        let (terrain, geo) = test_world();
        let index = NearestVertexIndex::new(&geo);
        let mut memo = RoomMeshMemo::default();
        let f = mercator::frame_for(false);
        let win = Window {
            depth: BAND_B_RUNG,
            origin_col: 0,
            origin_row: 0,
        };
        let (vw, vh) = virtual_dims(win.depth);
        // Warm the memo on the tile's own grid-level ancestor first.
        let _ = terrain_at_tile(&terrain, &geo, &index, &mut memo, &f, &win, vw, vh, 0, 0);
        let before = memo.corner_weights_misses();
        assert_eq!(
            before, 1,
            "the first tile of a cold memo costs exactly one miss"
        );
        let _ = terrain_at_tile(&terrain, &geo, &index, &mut memo, &f, &win, vw, vh, 0, 1);
        // BOUNDED, not zero: tiles (0,0) and (0,1) are not guaranteed to
        // share a grid-level ancestor, and a cold ancestor costs exactly one
        // miss. An exact-zero assertion would flake on an ancestor boundary
        // rather than fail on a real regression. One miss is three vertex
        // scans; the OLD path ran 49 unmemoised ones per tile, so this still
        // discriminates by more than an order of magnitude.
        assert!(
            memo.corner_weights_misses() - before <= 1,
            "the finest rung took {} memo misses for one tile; direct addressing costs \
             at most 1",
            memo.corner_weights_misses() - before
        );
    }

    /// **The whole plate's search count is bounded by the mesh, not by the
    /// screen** — the H1 mechanism at plate scale rather than tile scale.
    /// One 64x32 band-B plate covers a handful of grid-level facets, so it
    /// costs a handful of misses; the path this replaces cost `64 * 32 * 49
    /// = 100,352` unmemoised searches for the same picture.
    ///
    /// Asserted against the plate's own tile count, never a hardcoded
    /// number: a bound that says "fewer than one miss per hundred tiles" is
    /// a claim about the MECHANISM, and it fails loudly if a future change
    /// reintroduces a per-tile search.
    #[test]
    fn a_band_b_plate_costs_far_fewer_searches_than_it_has_tiles() {
        let (terrain, geo) = test_world();
        let index = NearestVertexIndex::new(&geo);
        let mut memo = RoomMeshMemo::default();
        let f = mercator::frame_for(false);
        let (vw, vh) = virtual_dims(BAND_B_RUNG);
        let (w, h) = (64u32, 32u32);
        let win = Window {
            depth: BAND_B_RUNG,
            origin_col: vw / 2,
            origin_row: vh / 2,
        };
        for row in 0..h {
            for col in 0..w {
                let _ = terrain_at_tile(
                    &terrain, &geo, &index, &mut memo, &f, &win, vw, vh, row, col,
                );
            }
        }
        let misses = memo.corner_weights_misses();
        let tiles = u64::from(w * h);
        assert!(
            misses * 100 < tiles,
            "a {w}x{h} band-B plate took {misses} memo misses over {tiles} tiles; \
             direct addressing must cost far fewer searches than it has tiles"
        );
        assert_eq!(
            memo.corner_weights_hits() + misses,
            tiles,
            "every tile must consult the memo exactly once"
        );
    }

    /// **The grid level comes from the `Geosphere`, not from a constant that
    /// happens to equal it.** `hornvale_terrain::GLOBE_LEVEL`, [`GLOBE_RUNG`]
    /// and the literal `6` are all the same number, so every other test in
    /// this module is blind to which one [`terrain_at_tile`] actually reads —
    /// the third appearance in three tasks of one vacuity shape (Task 1's
    /// all-ocean grids, Task 3's single-rung facet test, this).
    ///
    /// A world at `GLOBE_LEVEL - 1` breaks the tie. The instrument is the
    /// memo's own KEY: [`RoomMeshMemo::corner_weights_lookup`] answers only
    /// for the address the memo was filled at, so if `terrain_at_tile`
    /// addressed through a hardcoded `6` on a level-5 world it would have
    /// filled a DEPTH-6 key and this lookup at the level-5 address would come
    /// back `None`. The resolved vertex alone cannot discriminate: a depth-6
    /// facet's level-5 ancestor is the same triangle, so both spellings
    /// return the same vertex and only the key differs.
    #[test]
    fn the_grid_level_is_read_from_the_geosphere() {
        let level = hornvale_terrain::GLOBE_LEVEL - 1;
        assert_ne!(
            level, GLOBE_RUNG,
            "this test is vacuous unless the world's level differs from the rung constant"
        );
        let (terrain, geo) = test_world_at(level);
        assert_eq!(geo.depth(), level, "sanity: the world really is at {level}");
        let index = NearestVertexIndex::new(&geo);
        let mut memo = RoomMeshMemo::default();
        let f = mercator::frame_for(false);
        let win = Window {
            depth: GLOBE_RUNG,
            origin_col: 0,
            origin_row: 0,
        };
        let (vw, vh) = virtual_dims(win.depth);
        let got = terrain_at_tile(&terrain, &geo, &index, &mut memo, &f, &win, vw, vh, 0, 0);

        assert_eq!(
            memo.corner_weights_geo_level(),
            Some(level),
            "the memo must be filled against the world's OWN level"
        );
        let (lat, lon) = mercator::unproject(&f, 0, 0, vw, vh);
        let pos = hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon);
        let grid_addr = Facet::containing(pos, level);
        assert_eq!(
            grid_addr.depth(),
            level,
            "sanity: the grid-level address is at the world's level"
        );
        assert!(
            memo.corner_weights_lookup(&grid_addr).is_some(),
            "the memo must be keyed on the GRID-level address ({level}), not on a \
             hardcoded depth"
        );
        // And the answer is still correct against the level-5 mesh's own
        // spatial search -- a wrong grid level that somehow filled the right
        // key would still have to survive this.
        assert_eq!(
            got.vertex,
            index.nearest(&geo, lat, lon),
            "the resolved vertex must be the level-{level} mesh's nearest"
        );
        assert_eq!(got.ocean, terrain.is_ocean(got.vertex));
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

    // -- Task 4: the layer split ------------------------------------------

    /// A real point site, the window that shows it, and the screen cell it
    /// lands in — the same route
    /// [`tests::draw_with_gates_a_point_site_on_discovery`] takes (project
    /// the vertex, then move the window to it), lifted out so the two layer
    /// tests below share it rather than each hunting for a site.
    ///
    /// A CAVE vertex, because a cave is the one point site this module's
    /// own inputs can find: `GeneratedTerrain::cave_at` answers from the
    /// terrain alone, while the settlement roster is built from the
    /// world's LEDGER by `driver.rs` and never reaches here. The roster
    /// parameters are the caller's either way — `draw_feature_layer` takes
    /// two `BTreeSet<Vertex>` and asks the terrain nothing — so the same
    /// vertex exercises the settlement path when it is handed to the
    /// settlement roster instead.
    fn a_point_site(
        terrain: &GeneratedTerrain,
        geo: &Geosphere,
        f: &Frame,
        depth: u32,
        w: u16,
        h: u16,
    ) -> (Vertex, Window, u16, u16) {
        let vertex = geo
            .vertices()
            .find(|&c| terrain.cave_at(c).is_some())
            .expect("seed 42 at GLOBE_LEVEL has at least one cave vertex");
        let (virtual_w, virtual_h) = virtual_dims(depth);
        let g = geo.coord(vertex);
        let (row, col) = crate::mercator::project(f, g.latitude, g.longitude, virtual_w, virtual_h)
            .expect("the site is inside the projection's clamp");
        let (win, x, y) = window_showing(depth, row, col, w, h);
        (vertex, win, x, y)
    }

    /// **The invalidation argument, as an assertion.** Terrain and point
    /// sites drew in ONE pass, so a tile keyed on the discovery version was
    /// invalidated by every discovery — the whole pyramid, for one
    /// settlement. `draw_terrain_layer` does not take a `Discovered` at
    /// all, which is the strongest form the property can take: the call
    /// below COMPILING is half the test.
    ///
    /// **The other half is the guard, and it is why this test is not
    /// vacuous.** A window containing no discoverable site would satisfy
    /// "a discovery changes nothing" trivially and prove nothing at all —
    /// the failure shape this campaign hit three times before. So the
    /// window is positioned on a real cave and the test first PROVES the
    /// feature layer draws there, before asserting the terrain layer's own
    /// output is unmoved by that same discovery.
    #[test]
    fn a_discovery_does_not_change_the_terrain_layer() {
        let (terrain, geo) = test_world();
        let index = NearestVertexIndex::new(&geo);
        let f = mercator::frame_for(false);
        let (w, h) = (32u16, 16u16);
        let (site, win, x, y) = a_point_site(&terrain, &geo, &f, GLOBE_RUNG, w, h);
        let caves: BTreeSet<Vertex> = std::iter::once(site).collect();
        let mut discovered = Discovered::default();
        discovered.record(FeatureId::Cave(site));

        let mut memo = RoomMeshMemo::default();
        let bare = draw_terrain_layer(&terrain, &geo, &index, &mut memo, &f, &win, w, h, false);
        let bare_text = bare.to_plain_text();

        // GUARD: this window really does hold a site the feature layer
        // draws. Without this, everything below passes on an empty ocean.
        let mut overlaid = bare.clone();
        draw_feature_layer(
            &mut overlaid,
            &geo,
            &f,
            &win,
            false,
            &BTreeSet::new(),
            &caves,
            &discovered,
        );
        assert_ne!(
            bare.get(x, y).unwrap().glyph,
            Some(CAVE_GLYPH),
            "guard: the terrain layer must not already be drawing the site's glyph"
        );
        assert_eq!(
            overlaid.get(x, y).unwrap().glyph,
            Some(CAVE_GLYPH),
            "guard: this window holds no site the feature layer would draw, so the \
             assertion below would be vacuous"
        );
        assert_ne!(
            overlaid.to_plain_text(),
            bare_text,
            "guard: the discovery must actually move the composed plate"
        );

        // THE PROPERTY: the terrain layer, redrawn with that discovery in
        // hand, is byte-identical — it cannot see it, by signature.
        let mut memo = RoomMeshMemo::default();
        let after = draw_terrain_layer(&terrain, &geo, &index, &mut memo, &f, &win, w, h, false);
        assert_eq!(
            after.to_plain_text(),
            bare_text,
            "a discovery moved the terrain layer"
        );
    }

    /// **Nothing is drawn and then hidden** (spec Amendment 1 §A7): the
    /// undiscovered site's glyph is never chosen, so an undiscovered
    /// roster leaves the terrain layer's grid untouched — not painted and
    /// then cleared. Both rosters are exercised, because the two take
    /// different `FeatureId` arms and a gate wired to one of them would
    /// pass a single-roster test.
    #[test]
    fn the_feature_layer_draws_only_discovered_sites() {
        let (terrain, geo) = test_world();
        let index = NearestVertexIndex::new(&geo);
        let f = mercator::frame_for(false);
        let (w, h) = (32u16, 16u16);
        let (site, win, x, y) = a_point_site(&terrain, &geo, &f, GLOBE_RUNG, w, h);
        let roster: BTreeSet<Vertex> = std::iter::once(site).collect();
        let empty = BTreeSet::new();
        let mut memo = RoomMeshMemo::default();
        let bare = draw_terrain_layer(&terrain, &geo, &index, &mut memo, &f, &win, w, h, false);
        let bare_text = bare.to_plain_text();

        for (settlements, caves, id, glyph) in [
            (&empty, &roster, FeatureId::Cave(site), CAVE_GLYPH),
            (
                &roster,
                &empty,
                FeatureId::Settlement(site),
                SETTLEMENT_GLYPH,
            ),
        ] {
            let mut g = bare.clone();
            draw_feature_layer(
                &mut g,
                &geo,
                &f,
                &win,
                false,
                settlements,
                caves,
                &Discovered::default(),
            );
            assert_eq!(
                g.to_plain_text(),
                bare_text,
                "an UNdiscovered site was drawn ({glyph})"
            );

            let mut discovered = Discovered::default();
            discovered.record(id);
            draw_feature_layer(
                &mut g,
                &geo,
                &f,
                &win,
                false,
                settlements,
                caves,
                &discovered,
            );
            assert_eq!(
                g.get(x, y).unwrap().glyph,
                Some(glyph),
                "a DISCOVERED site was not drawn"
            );
        }
    }

    /// **`draw_with` is still exactly its two layers, composed** — the
    /// no-caller-changes half of the split. A composition that drew
    /// something the layers do not (or dropped something they do) would
    /// pass every test above and still break every existing caller.
    #[test]
    fn draw_with_is_the_composition_of_its_layers() {
        let (terrain, geo) = test_world();
        let index = NearestVertexIndex::new(&geo);
        let f = mercator::frame_for(false);
        let (w, h) = (32u16, 16u16);
        let (site, win, _, _) = a_point_site(&terrain, &geo, &f, GLOBE_RUNG, w, h);
        let caves: BTreeSet<Vertex> = std::iter::once(site).collect();
        let settlements = BTreeSet::new();
        let mut discovered = Discovered::default();
        discovered.record(FeatureId::Cave(site));

        let composed = draw_with(
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

        let mut memo = RoomMeshMemo::default();
        let mut by_hand =
            draw_terrain_layer(&terrain, &geo, &index, &mut memo, &f, &win, w, h, false);
        // Sanity: the layers genuinely differ, so the equality below is not
        // a comparison of a plate with itself.
        let terrain_only = by_hand.to_plain_text();
        draw_feature_layer(
            &mut by_hand,
            &geo,
            &f,
            &win,
            false,
            &settlements,
            &caves,
            &discovered,
        );
        assert_ne!(
            by_hand.to_plain_text(),
            terrain_only,
            "sanity: the feature layer must have drawn something here"
        );
        assert_eq!(
            composed.to_plain_text(),
            by_hand.to_plain_text(),
            "`draw_with` is no longer its two layers composed"
        );
    }

    // -- LAYER THREE: the band-B perception overlay (Task 6) -------------
    //
    // These drive `draw_perception_layer` from LITERAL `Perceived` values
    // rather than from a genesis, which is the whole reason `Perceived`
    // exists as a flattening: the rules under test here (what paints, what
    // does not, and who keeps a contested box) are decidable without a
    // world, and a test that had to build one could not force a collision
    // at all.

    /// A `Perceived` whose `room` is the packed id of the facet CONTAINING
    /// `(lat, lon)` at `depth` — so a test can place a facet at a chosen
    /// geographic point without knowing a real room id.
    fn perceived_at_lat_lon(
        lat: f64,
        lon: f64,
        depth: u32,
        here: bool,
        mark: Option<(u32, &str)>,
    ) -> Perceived<'_> {
        let pos = hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon);
        let facet = Facet::containing(pos, depth);
        Perceived {
            room: facet.pack().expect("a facet at a shipped depth packs").0,
            here,
            mark,
        }
    }

    /// The observer draws [`HERE_GLYPH`], a marked facet draws its kind's own
    /// glyph, and **an ordinary unmarked facet draws NOTHING** — the raster
    /// underneath already shows that ground, and painting over it is what
    /// this layer exists not to do.
    ///
    /// Non-vacuity: the unmarked facet is placed at a DIFFERENT point from
    /// the other two and its own box is asserted blank, so "nothing was
    /// drawn" cannot be satisfied by the facet simply having landed off the
    /// plate or under one of its neighbours.
    #[test]
    fn the_perception_overlay_paints_the_observer_and_marks_and_never_bare_ground() {
        let f = crate::mercator::frame_for(false);
        let win = Window {
            depth: BAND_B_RUNG,
            origin_col: 0,
            origin_row: 0,
        };
        let (vw, vh) = virtual_dims(win.depth);
        // Three points far enough apart to occupy three distinct tiles.
        let pts = [(0.0, 0.0), (0.0, 0.02), (0.0, 0.04)];
        let perceived = vec![
            perceived_at_lat_lon(pts[0].0, pts[0].1, win.depth, true, None),
            perceived_at_lat_lon(pts[1].0, pts[1].1, win.depth, false, Some((3, "agent"))),
            perceived_at_lat_lon(pts[2].0, pts[2].1, win.depth, false, None),
        ];
        let boxes: Vec<(u32, u32)> = perceived
            .iter()
            .map(|c| {
                perception_tile(&f, vw, vh, c.room).expect("an equatorial facet is in the clamp")
            })
            .collect();
        assert_eq!(
            boxes.iter().collect::<BTreeSet<_>>().len(),
            3,
            "the three points must land in three distinct tiles or this test proves nothing"
        );
        // A window whose origin is the tightest bounding box of the three,
        // so all three are on the plate and none is off its top or left.
        let origin_row = boxes.iter().map(|b| b.0).min().unwrap();
        let origin_col = boxes.iter().map(|b| b.1).min().unwrap();
        let win = Window {
            depth: BAND_B_RUNG,
            origin_col,
            origin_row,
        };
        let w = (boxes.iter().map(|b| b.1).max().unwrap() - origin_col + 1) as u16;
        let h = (boxes.iter().map(|b| b.0).max().unwrap() - origin_row + 1) as u16;
        let mut grid = Grid::new(w, h);
        draw_perception_layer(&mut grid, &f, &win, true, &perceived);
        // The screen position of a virtual tile, spelled out here rather
        // than taken from `perception_tile_on_screen`: this test is checking
        // WHAT WAS PAINTED WHERE, so the expected position has to come from
        // somewhere other than the function that decided it.
        let at = |b: (u32, u32)| {
            let drow = b.0 - win.origin_row;
            let dcol = (b.1 + vw - win.origin_col) % vw;
            *grid
                .get(dcol as u16, drow as u16)
                .expect("the three tiles are inside this plate")
        };
        assert_eq!(
            at(boxes[0]).glyph,
            Some(HERE_GLYPH),
            "the observer draws '@'"
        );
        assert_eq!(at(boxes[0]).weight, Weight::Bold, "and draws bold");
        assert_eq!(
            at(boxes[1]).glyph,
            Some(AGENT_GLYPH),
            "a marked facet draws its kind's glyph"
        );
        assert!(
            at(boxes[2]).is_blank(),
            "an unmarked facet must leave the raster's own ground alone"
        );
    }

    /// The collision rule, on a FORCED collision — two facets at the very
    /// same facet, so they cannot help sharing a tile. Asserted in BOTH
    /// document orders, which is what separates "the rule ran" from "the
    /// later write won".
    ///
    /// Clause 1 (the observer never loses their own box) and clause 2 (the
    /// numerically smallest `salience` wins among marked facets) are checked
    /// separately, because a single fixture satisfying both would not say
    /// which clause did the work.
    #[test]
    fn the_perception_overlay_settles_a_contested_box_by_rank_in_either_order() {
        let f = crate::mercator::frame_for(false);
        let win = Window {
            depth: BAND_B_RUNG,
            origin_col: 0,
            origin_row: 0,
        };
        let (vw, vh) = virtual_dims(win.depth);
        let here = perceived_at_lat_lon(0.0, 0.0, win.depth, true, None);
        let loud = perceived_at_lat_lon(0.0, 0.0, win.depth, false, Some((0, "settlement")));
        let quiet = perceived_at_lat_lon(0.0, 0.0, win.depth, false, Some((9, "cave")));
        let (row, col) = perception_tile(&f, vw, vh, here.room).expect("in the clamp");
        let win = Window {
            depth: BAND_B_RUNG,
            origin_col: col,
            origin_row: row,
        };

        // Clause 1: the observer keeps their box against the most salient
        // mark there is, in either order.
        for perceived in [vec![here, loud], vec![loud, here]] {
            let mut grid = Grid::new(2, 2);
            draw_perception_layer(&mut grid, &f, &win, true, &perceived);
            assert_eq!(
                grid.get(0, 0).unwrap().glyph,
                Some(HERE_GLYPH),
                "the view is egocentric: the observer never loses their own box"
            );
        }

        // Clause 2: among marks, the SMALLEST salience wins — a rank, never
        // a magnitude.
        for perceived in [vec![quiet, loud], vec![loud, quiet]] {
            let mut grid = Grid::new(2, 2);
            draw_perception_layer(&mut grid, &f, &win, true, &perceived);
            assert_eq!(
                grid.get(0, 0).unwrap().glyph,
                Some(SETTLEMENT_GLYPH),
                "salience 0 outranks salience 9, whichever order they arrive in"
            );
        }
    }

    /// An unrecognised mark kind still DRAWS
    /// (`hornvale_scene::Mark::kind`'s own instruction to a renderer), so a
    /// future kind cannot vanish from the map. Pinned because the fallback
    /// arm of [`mark_glyph`] is otherwise unreachable from any real band.
    #[test]
    fn an_unrecognised_mark_kind_still_draws() {
        assert_eq!(mark_glyph("settlement"), SETTLEMENT_GLYPH);
        assert_eq!(mark_glyph("cave"), CAVE_GLYPH);
        assert_eq!(mark_glyph("agent"), AGENT_GLYPH);
        assert_eq!(
            mark_glyph("some-kind-from-a-later-campaign"),
            AGENT_GLYPH,
            "an unknown kind must still get a glyph, never be skipped"
        );
    }
}
