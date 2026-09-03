//! The world plate: the whole-planet Mercator backdrop, drawn in `bin`
//! (never `core`, which carries no hornvale crate — see
//! `hornvale-game-core`'s `Cargo.toml`) and handed to `core` as an
//! already-rendered [`hornvale_game_core::Grid`].
//!
//! [`draw`]/[`draw_with`] paint one glyph per grid cell, from that cell's
//! water class and elevation band (The Legend, Task 6 — see
//! [`glyph_and_color_for`]). Since The Quadrat a tile is a MESH FACET and
//! its terrain is read from that facet's own grid-level triangle by direct
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
//! **History, not current behaviour**: the glyph vocabulary used to be a
//! bare `` `~` `` ocean / `` `. ``  land binary, the spike's own
//! (`windows/worldgen/examples/portolan_spike.rs`, `glyph_for`, line
//! 171 -- deleted at this campaign's close, git history at `0292de87f^`)
//! — reused rather than invented at the time. The Legend (Task 6) retired
//! that binary — see [`glyph_and_color_for`] for the vocabulary this
//! module draws now — but the vertex-lookup mechanism it sits on top of
//! was never the spike's: `glyph_for` asked
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
//! per plate instead of a few million: which four `Vertex` ids the corners of
//! a grid-level QUAD resolve to.** That is
//! [`hornvale_kernel::Facet::corner_weights`]'s four
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
use hornvale_terrain::landscape::{FeatureClass, FeatureId as LandscapeFeatureId};
use std::collections::{BTreeMap, BTreeSet};

use crate::discovery::{Discovered, FeatureId};
use crate::mercator::{self, Frame};

/// Every placed settlement's nearest terrain vertex, valued by that
/// settlement's own committed population — every subject carrying
/// [`hornvale_settlement::IS_SETTLEMENT`], resolved through its own
/// committed `LATITUDE`/`LONGITUDE` to the [`Vertex`] [`NearestVertexIndex::nearest`]
/// says is closest. A settlement missing either coordinate fact is skipped
/// rather than guessed at (genesis always commits the pair together; a
/// hand-built world might not, and this must not panic on one). Two
/// settlements resolving to the SAME nearest vertex are, for a drawn map's
/// purposes, the same point (`crate::discovery::FeatureId::Settlement`'s own
/// doc already says so for identity); population takes the larger of the
/// two rather than the last one scanned, so map iteration order cannot
/// silently pick a smaller town's population for the shared marker.
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
/// taking a resolved roster rather than a ledger to read.
///
/// **Widened from `BTreeSet<Vertex>` to `BTreeMap<Vertex, u64>` (The
/// Legend, Task 7)**, keyed the same way, valued by
/// `hornvale_settlement::POPULATION` — the size
/// [`draw_feature_layer`]'s own viewport-relative major/minor split ranks
/// by. Both callers draw through [`draw_with`], whose own `settlements`
/// parameter widened the same way at the same time, so there is no
/// caller left that only ever wanted membership; a bare vertex set is
/// still one `.keys()` away for a reader that does.
pub fn settlements_of(
    world: &World,
    geo: &Geosphere,
    nearest: &NearestVertexIndex,
) -> BTreeMap<Vertex, u64> {
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
            let population = match world
                .ledger
                .value_of(fact.subject, hornvale_settlement::POPULATION)
            {
                Some(Value::Number(n)) => *n as u64,
                _ => return None,
            };
            Some((nearest.nearest(geo, lat, lon), population))
        })
        .fold(BTreeMap::new(), |mut map, (vertex, population)| {
            map.entry(vertex)
                .and_modify(|p: &mut u64| *p = (*p).max(population))
                .or_insert(population);
            map
        })
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
/// Matches `hornvale_locale::walk_depth` (`hornvale_vessel::walk_depth` is a
/// re-export of it, not a second definition) for the canonical globe level of
/// 6; a world pinned to another globe level moves both together.
///
/// **13, not 12, since The Pavement (decision 0511).** That campaign moved the
/// walk band one rung finer to preserve the ground covered by one step when
/// the lattice became an 8-connected cube-sphere quad grid — see
/// `hornvale_locale::walk_depth`'s own doc for the measured step lengths. This
/// constant restates that depth ABSOLUTELY, which is invisible to any scan
/// looking for the arithmetic, so it is pinned against the running function by
/// `tests/walk_band_agreement.rs` in this crate and by
/// `cli/tests/suite/walk_depth_agreement.rs`'s absolute roster in the
/// workspace.
/// type-audit: bare-ok(count)
pub const BAND_B_RUNG: u32 = 13;

/// The coarsest rung: the canonical grid level, below which the terrain
/// fields have no resolution to disclose (decision 0196).
/// type-audit: bare-ok(count)
pub const GLOBE_RUNG: u32 = 6;

/// The central angle one base FACE of the cube-sphere subtends along a great
/// circle through four face centres: `pi/2` exactly, 90°. Every refinement
/// level halves it, which is the whole content of
/// [`tiles_around_a_great_circle`].
///
/// # IT WAS THE ICOSAHEDRON'S NUMBER UNTIL FIX ROUND 1, AND BAND B WAS DRAWN 1.4188x TOO FINE
///
/// This function returned `acos(1/sqrt(5))` = 1.1071487 rad — 63.4349°, the
/// central angle an ICOSAHEDRON's base-face edge subtends at the centre of its
/// circumsphere — and its caller's doc justified the halving with "the facet
/// count is `20 << (2 * depth)`". Both premises died with the base mesh
/// (decision 0506): the count is `6 << (2 * depth)`, and the chart's width is
/// a property of the CUBE. At rung 13 the old arithmetic produced
/// `TAU / (1.1071487 / 2^13)` = **46,490** columns over a band that has
/// **32,768** facets around its equator — a factor of 1.4188 — so
/// [`terrain_at_tile`] and `driver.rs`'s cursor resolver mapped ~1.42 chart
/// columns onto each walk-band room and moving the cursor one column
/// frequently did not change room. `plate.rs`'s own "one tile per facet" and
/// `driver.rs`'s "the finest rung is band B: one tile per facet" were both
/// false by 42%.
///
/// # WHERE `pi/2` COMES FROM, and why it is exact rather than derived
///
/// A cube has six faces and four of them meet the equator, each spanning a
/// quarter of it; a great circle through four face centres therefore crosses
/// four base facets, so one base facet subtends `TAU/4 = pi/2` along that
/// circle. Refining `depth` levels splits each facet into `4^depth` children,
/// `2^depth` of them along each axis, so the count around the circle is
/// `4 * 2^depth` — 32,768 at rung 13, which
/// [`the_chart_width_is_the_meshs_own_equatorial_facet_count`] checks against
/// the mesh itself rather than against this derivation.
///
/// **No transcendental, and that is a simplification rather than a loss.** The
/// icosahedral figure needed an `acos` and a comment explaining why it was not
/// tabulated; the cube's is a quarter turn, so `FRAC_PI_2` IS the derivation.
/// The tangent warp redistributes facets WITHIN a face and does not change how
/// many of them a face has, so the count is exact even though the individual
/// facet arcs are not equal.
/// The central angle one edge of the subdivided ICOSAHEDRON subtends at the
/// centre of its circumsphere, at `grid_level` — `acos(1/sqrt(5)) / 2^level`,
/// 0.01729920 rad at level 6.
///
/// # THIS IS THE NUMBER FIX ROUND 1 DELETED, AND DELETING IT FROM ITS OLD HOME WAS RIGHT
///
/// [`base_facet_arc_rad`] used to return `acos(1/sqrt(5))` and its caller
/// justified the halving with "the facet count is `20 << (2 * depth)`". Both
/// premises died with the base mesh (decision 0506) and the chart's width is a
/// property of the CUBE, so the icosahedral figure had no business deciding how
/// many TILES a rung carries. That correction stands and this function does not
/// reopen it.
///
/// **What the correction lost is that the icosahedral figure still decides
/// something — just not that.** `Geosphere` is a subdivided ICOSAHEDRON
/// (`kernel/src/geosphere.rs`, `10·4^L + 2` vertices — 40,962 at level 6), and
/// terrain is sampled on its VERTICES. So the mesh's own sample spacing is an
/// icosahedral property while the chart's tile count is a cube one, and the two
/// lattices are incommensurate: at level 6 the mesh carries ~363 samples around
/// a great circle where rung 6's chart carries 256 tiles.
///
/// Conflating the two is what the old code did in one direction, and a reader
/// who takes fix round 1 to mean "the icosahedron is gone" makes the same
/// mistake in the other. Two lattices, two functions, each named for the one it
/// measures.
///
/// `sqrt` stays intrinsic and `acos` routes through
/// [`hornvale_kernel::math`] — `kernel/CLAUDE.md`'s own split.
fn mesh_edge_arc_rad(grid_level: u32) -> f64 {
    hornvale_kernel::math::acos(1.0 / 5.0_f64.sqrt())
        / hornvale_kernel::math::powf(2.0, f64::from(grid_level))
}

/// How many TERRAIN SAMPLES the mesh carries around a great circle at
/// `grid_level` — the data's own resolution, against which a rung's tile count
/// is either enough or not.
///
/// The partner of [`tiles_around_a_great_circle`], deliberately the same shape
/// from the other lattice's base angle: `TAU` divided by
/// [`mesh_edge_arc_rad`]. 363 at level 6.
/// type-audit: bare-ok(count: grid_level), bare-ok(count: return)
pub fn mesh_samples_around_a_great_circle(grid_level: u32) -> u32 {
    ((std::f64::consts::TAU / mesh_edge_arc_rad(grid_level)).round() as u32).max(1)
}

/// The rung a map CONSULTATION opens at: the coarsest rung whose chart is at
/// least as fine as the mesh it draws (The Hachure, Stage 0).
///
/// **Why the map needs an entry rung at all.** `Driver::start` leaves
/// `window.depth` at [`BAND_B_RUNG`] — the walk band's own rung, which is where
/// the WALKER belongs — and `enter_map` used to inherit it. Band B is seven
/// rungs finer than the grid and [`terrain_at_tile`] resolves every rung
/// through the grid-level ancestor, so a consultation opened there shows one
/// vertex's reading across the whole screen: measured at **1** distinct terrain
/// vertex on a 120x40 plate, seed 42.
///
/// **This invents nothing and re-tunes nothing.** Decision 0196 lets a view
/// render coarser than the world but never finer, and decision 0287 makes that
/// structural by holding that a tile IS a facet at the rung's depth. Both are
/// untouched here: [`virtual_dims`] is unchanged, the ladder is unchanged, and
/// every rung on it remains reachable by zooming. Only the rung a consultation
/// *starts* at moves.
///
/// **Derived, never tabulated.** A literal `7` becomes a tuned number the first
/// time `GLOBE_LEVEL` moves — the same argument
/// [`tiles_around_a_great_circle`] makes for computing its own ladder. The
/// answer is the smallest `d` with `tiles_around_a_great_circle(d) >=
/// mesh_samples_around_a_great_circle(grid_level)`, clamped into the shipped
/// ladder so a pathological grid level cannot return a rung the client has no
/// picture for.
/// type-audit: bare-ok(count: grid_level), bare-ok(count: return)
pub fn map_entry_rung(grid_level: u32) -> u32 {
    let want = mesh_samples_around_a_great_circle(grid_level);
    (GLOBE_RUNG..=BAND_B_RUNG)
        .find(|&d| tiles_around_a_great_circle(d) >= want)
        .unwrap_or(BAND_B_RUNG)
}

fn base_facet_arc_rad() -> f64 {
    std::f64::consts::FRAC_PI_2
}

/// How many facets fit around a great circle at mesh depth `depth`.
///
/// **Derived from the cube-sphere's own geometry, not a hardcoded ladder.**
/// A table becomes a tuned number the first time the globe level moves. (This
/// sentence used to cite `hornvale_vessel::course::step_length_rad`'s own doc as
/// the precedent for avoiding that; The Pavement deleted the whole `course`
/// module along with the rhumb it served, so the principle is stated here
/// directly rather than pointed at a path that no longer resolves.) One base
/// facet subtends [`base_facet_arc_rad`]; each of the `depth` refinement
/// levels halves it (the facet count is `6 << (2 * depth)`, i.e. four facets
/// per facet per level, so each axis doubles), so the count around a great
/// circle is `2*pi` divided by that angle — `4 * 2^depth`.
///
/// `2.0^depth` is computed through `math::powf` rather than a shift, so a
/// depth at or past 32 saturates instead of overflowing.
fn tiles_around_a_great_circle(depth: u32) -> u32 {
    let edge = base_facet_arc_rad() / hornvale_kernel::math::powf(2.0, f64::from(depth));
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

/// The glyph for open ocean — `WaterKind::Ocean`, index 0
/// ([`hornvale_terrain::WaterKind::LEGEND`]). Retained from the spike's own
/// vocabulary (see the module doc), but it now names one specific water
/// class rather than "everything the old ocean/land binary called wet" —
/// see [`glyph_and_color_for`].
const OCEAN_GLYPH: char = '~';
/// The glyph for a terminal endorheic sink — `WaterKind::SaltBasin`, index
/// one. New at The Legend: before this task every non-ocean cell drew
/// [`OCEAN_GLYPH`]'s complement regardless of what kind of water it
/// actually was.
const SALT_BASIN_GLYPH: char = '=';
/// The glyph for a river channel — `WaterKind::River`, index 2. See
/// [`SALT_BASIN_GLYPH`].
const RIVER_GLYPH: char = '"';

/// The RELIEF ladder's glyphs, index-matched to
/// [`hornvale_scene::RELIEF_LEGEND`] (`abyss, shelf, lowland, upland,
/// highland, alpine`) — drawn only for `WaterKind::DryLand` (index 3;
/// every wetter class draws its own water glyph instead, see
/// [`glyph_and_color_for`]).
///
/// **The allocation rule in code** (spec §2, restated in this module's own
/// doc): ink ASCENDS with the band, so adjacent bands stay tellable apart
/// by weight alone even in monochrome. This is Task 5's specimen sheet
/// (`docs/audits/glyph-specimen-sheet.txt`) "stipple" candidate — the only
/// one of its three ladders with that property — **with the highland mark
/// re-picked twice**:
///
/// The sheet's own stipple highland glyph was `*`, and Nathan assigned `*`
/// to cave mouths (`.superpowers/sdd/2026-08-28-the-legend/progress.md`,
/// "Nathan's glyph assignments", 2026-08-30), so Task 6 first moved
/// highland to `{`, one of the sheet's own two listed alternatives at that
/// rung (the other was `)`) — `{` was chosen over `)` because `)` reads as
/// a stray, unbalanced parenthesis with nothing before it, while `{` kept
/// the same "opening" shape-family the semicolon just below it already
/// suggests.
///
/// **Nathan's fix-round-1 review moved it again, to `^`**: "carets work
/// better for most mountains" — "most", so only this one rung moves; the
/// other five bands are untouched. This is a deliberate departure from
/// strict ink-ordinality, not an oversight of it: decision 0389 lets a
/// glyph carry order OR identity, and while `^` is not obviously heavier
/// ink than `;` (the upland glyph directly below it), `^` *depicts* a
/// mountain — the universal roguelike convention for one — which is the
/// stronger of the two claims. A future reader tempted to "fix" this ladder
/// back to a pure ink ramp should not; the identity claim is the point.
///
/// `^` is not a collision with `windows/scene/src/surrounds_ascii.rs`'s own
/// `impedance_glyph` (which also emits `^`, for impedance band 4): that is
/// the sim's own walk-band vocabulary, outside this client's register by
/// prior ruling, and high/steep ground is the same concept at globe scale
/// (highland) and walk scale (impedance 4) — a future task porting that
/// ladder into this crate's register should bind `^` once, to cover both,
/// rather than mint a second glyph for the same idea.
const RELIEF_GLYPHS: [char; 6] = [' ', '`', ',', ';', '^', '%'];

/// The colour claim for open ocean, when colour is allowed. An invented
/// client-side palette, not a wire value: the world plate has no snapshot
/// channel to carry a colour off (see
/// [`hornvale_game_core::Source::World`]'s own doc).
const OCEAN_COLOR: [u8; 3] = [20, 60, 160];
/// The colour claim for a salt basin. See [`OCEAN_COLOR`].
const SALT_BASIN_COLOR: [u8; 3] = [230, 230, 200];
/// The colour claim for a river. See [`OCEAN_COLOR`].
const RIVER_COLOR: [u8; 3] = [90, 180, 220];
/// The RELIEF ladder's colours, index-matched to [`RELIEF_GLYPHS`] — lifted
/// unchanged from Task 5's specimen sheet (the "stipple" row's own ramp),
/// since re-tuning them here would silently fork the two artifacts.
const RELIEF_COLORS: [[u8; 3]; 6] = [
    [20, 20, 90],
    [30, 130, 150],
    [50, 150, 70],
    [160, 160, 50],
    [150, 95, 45],
    [235, 235, 235],
];

/// The terrain-layer glyph and colour for one tile, from its water class
/// ([`TileTerrain::water`], `WaterKind::index()`'s own order) and elevation
/// band ([`TileTerrain::band`], [`hornvale_scene::relief_band`]'s own
/// order).
///
/// **Water outranks elevation for the three WET classes** (indices 0-2,
/// ocean/salt-basin/river): a river channel draws as the river mark
/// regardless of which relief band its own vertex would otherwise fall
/// in — water is "what substance is here", which the register's own
/// allocation rule (spec §2) says a glyph should carry as IDENTITY, not as
/// a second-guess against the land's height. Only `WaterKind::DryLand`
/// (index 3) has no water glyph of its own, so only there does the RELIEF
/// band decide the mark.
///
/// `band` is clamped defensively to the legend's own length rather than
/// indexing unchecked — [`hornvale_scene::relief_band`]'s own contract
/// already guarantees `0..6`, so the clamp is a belt no caller is expected
/// to need, not a silent tolerance for a wider range.
fn glyph_and_color_for(water: u8, band: u32) -> (char, [u8; 3]) {
    match water {
        0 => (OCEAN_GLYPH, OCEAN_COLOR),
        1 => (SALT_BASIN_GLYPH, SALT_BASIN_COLOR),
        2 => (RIVER_GLYPH, RIVER_COLOR),
        _ => {
            let i = (band as usize).min(RELIEF_GLYPHS.len() - 1);
            (RELIEF_GLYPHS[i], RELIEF_COLORS[i])
        }
    }
}

/// The glyph for a DISCOVERED MINOR settlement (Task 5, §A3's "point
/// sites"; Task 7 splits the old single `SETTLEMENT_GLYPH` in two).
/// Never drawn undiscovered — see [`draw_feature_layer`]'s own doc for why
/// "not yet drawn" is the only state an undiscovered site is ever in.
///
/// **Nathan's own glyph assignment (2026-08-30, `progress.md`'s "Nathan's
/// glyph assignments"), not a Task 6 leftover.** The old `#` collided with
/// `plan.rs`'s WALL in three renderers; Nathan picked `o`/`O` (lowercase for
/// an ordinary settlement, capital for a MAJOR one) rather than leaving the
/// replacement to the specimen sheet. That freed `o` from its old job —
/// [`CAVE_GLYPH`] moves to `*` in the same commit, never leaving a stale
/// claim on `o` for the two characters to collide over.
///
/// **Ruling AG resolves the letter's apparent collision with the creature
/// codespace.** The register's own rule reserves `a`-`z`/`A`-`Z` for a
/// creature's noun-initial, and `o` is an owlbear's initial — but that rule
/// is scoped to the WALK band and floor plan (`chart.rs`), where telling one
/// creature from another is the whole point. The world map draws every
/// creature as the generic [`AGENT_GLYPH`] regardless of species, so `o`/`O`
/// here never compete with a creature mark for the same cell, and Nathan's
/// assignment stands unmodified.
pub(crate) const SETTLEMENT_MINOR_GLYPH: char = 'o';
/// The glyph for a DISCOVERED MAJOR settlement — the top
/// [`MAJOR_SETTLEMENT_QUANTILE`] of settlements ranked by population among
/// those actually IN FRAME (discovered and on-screen; see
/// [`draw_feature_layer`]'s own doc for the ranking). See
/// [`SETTLEMENT_MINOR_GLYPH`] for the rest of the reasoning; this is the
/// same identity claim (a settlement), one size tier up.
pub(crate) const SETTLEMENT_MAJOR_GLYPH: char = 'O';
/// The glyph for a DISCOVERED cave mouth. Moved here from `o` (Task 7,
/// Nathan's glyph assignments) once `o`/`O` were claimed by settlements —
/// see [`SETTLEMENT_MINOR_GLYPH`]'s doc for why the two moves are one
/// commit, not two.
pub(crate) const CAVE_GLYPH: char = '*';
/// The glyph for a DISCOVERED volcano (Task 7): the map's own edifice
/// marker, gated on [`crate::discovery::FeatureId::Extent`] exactly like
/// any other landscape feature — see [`draw_feature_layer`]'s own doc on
/// why this is the one landform Task 7 draws THROUGH the existing
/// discovery mechanism rather than unconditionally.
///
/// `pub`, not `pub(crate)` like [`CAVE_GLYPH`]/[`SETTLEMENT_MINOR_GLYPH`]:
/// `bin/tests/plate_vocabulary.rs` (a separate crate to `rustc`) needs this
/// to pin the landform reachability guard against a real render, the same
/// reason [`terrain_at_tile`] is `pub`.
pub const VOLCANO_GLYPH: char = '!';
/// The glyph for a waterfall (Task 7). Drawn UNCONDITIONALLY — see
/// [`draw_feature_layer`]'s own doc on why a waterfall has no discovery
/// identity to gate on. `pub` for the same integration-test reason
/// [`VOLCANO_GLYPH`] is.
///
/// **A third landform, a river delta, was drawn here through fix round 1
/// and was removed outright**: its glyph (`:`) collided with the sim's own
/// impedance ladder (`windows/scene/src/surrounds_ascii.rs`, band 3), which
/// cannot move (it is pinned to the sim by
/// `the_shape_matches_the_sims_own_ascii_render`), and unlike `^` a delta
/// and moderately rough going are unrelated referents that cannot share a
/// binding. See `clients/game/core/src/register.rs`'s own `REGISTER` doc
/// for the fuller rationale, including the tier spike's own finding that a
/// third point marker read as noise rather than invitation.
pub const WATERFALL_GLYPH: char = '|';

/// The colour claim for a discovered settlement — a warm tint distinct from
/// both terrain colours, so a settlement reads as a different SUBSTANCE
/// (§A5: colour carries substance, never the epistemic channel — an
/// undiscovered site simply is not drawn at all, so there is no
/// discovered/undiscovered pair of colours to confuse with one another).
/// Shared by [`SETTLEMENT_MINOR_GLYPH`] and [`SETTLEMENT_MAJOR_GLYPH`]:
/// colour carries the SUBSTANCE ("a settlement"), and size is the glyph's
/// own job, so a second colour for the major tier would say two different
/// things stand there.
const SETTLEMENT_COLOR: [u8; 3] = [220, 180, 60];
/// The colour claim for a discovered cave mouth. See [`SETTLEMENT_COLOR`].
const CAVE_COLOR: [u8; 3] = [130, 120, 110];
/// The colour claim for a discovered volcano — a hot, saturated tint
/// distinct from every relief band and from [`SETTLEMENT_COLOR`]/
/// [`CAVE_COLOR`], so an edifice reads as its own substance rather than an
/// intense alpine band.
const VOLCANO_COLOR: [u8; 3] = [220, 70, 30];
/// The colour claim for a waterfall — brighter and cooler than
/// [`RIVER_COLOR`], so a knickpoint reads as a distinct substance on the
/// same channel rather than a re-tinted river cell.
const WATERFALL_COLOR: [u8; 3] = [190, 230, 245];

/// The top fraction of settlements IN FRAME, by population, that draw
/// [`SETTLEMENT_MAJOR_GLYPH`] rather than [`SETTLEMENT_MINOR_GLYPH`] (Task
/// 7). Nathan's own range is 10-25%; this picks the LOW end.
///
/// **Measured, not guessed** (`.superpowers/sdd/2026-08-28-the-legend/
/// task-7-report.md` carries the full distribution): seed 42 mints 386
/// settlements. In the campaign's own reference window (the GLOBE_RUNG,
/// 80x24, equator-centred plate `plate_vocabulary.rs` renders), 16 land
/// in frame, and 10% of that count (rounding up) is 2 majors. A sweep of
/// every possible 80x24 window found the densest a plate can ever be —
/// 54 settlements in one frame — where 10% is 6 majors, still rare against
/// the picture. 25% at that same density would be 14, which starts to
/// read as "half the towns are major", the exact noise the tier spike
/// already warned against. The low end keeps `O` an event, not a texture,
/// at every measured density.
///
/// **A single named constant, deliberately** (task brief): the rule may
/// become rung-tied or world-relative later, and that should cost one
/// line here, not a hunt through `draw_feature_layer`.
///
/// **Rank-based, not value-based.** The top `ceil(quantile * n)` settlements
/// BY RANK draw major, not every settlement whose population clears some
/// absolute or percentile VALUE — a world with one dominant capital and 300
/// hamlets should not draw zero majors just because the capital is a
/// long way above everyone else on the value axis.
const MAJOR_SETTLEMENT_QUANTILE: f64 = 0.10;

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
    settlements: &BTreeMap<Vertex, u64>,
    caves: &BTreeSet<Vertex>,
    volcanoes: &BTreeSet<Vertex>,
    waterfalls: &[Vertex],
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
        volcanoes,
        waterfalls,
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
/// **re-measured 2026-09-01 by The Pavement — BOTH figures moved, for two
/// independent reasons**), against the 1,960,000 scans the 49-point vote cost
/// at every rung: **12 at [`BAND_B_RUNG`]** (a 163,000x reduction, because
/// tens of thousands of tiles share one grid-level facet — 4 memo misses
/// against 39,996 hits over 40,000 tiles) but **38,496 at [`GLOBE_RUNG`]**
/// (50.9x, because there a chart tile is already about the size of a facet
/// and there is little to share — 12,832 misses against 27,168 hits). The
/// mechanism holds at every rung — the coarse end is 51x — but the magnitude
/// does not, and `GLOBE_RUNG` is a shipped rung a player reaches by holding
/// `-`. The module doc states the rule this is an instance of.
///
/// **Why both moved, stated because only one of the two causes is obvious.**
/// The fine figure (90 -> 12) moved because `BAND_B_RUNG` itself moved
/// 12 -> 13 with the walk band (decision 0511): a finer rung means smaller
/// tiles, so a 200x200 plate spans fewer grid-level facets and shares each of
/// them harder. The coarse figure (88,986 -> 38,496) moved even though
/// `GLOBE_RUNG` did not, because the *base geometry* did — a cube-sphere
/// carries 6*4^6 = 24,576 facets at level 6 where the icosphere carried
/// 20*4^6 = 81,920, so a grid-level facet is about 3.3x larger and a chart
/// tile covers proportionally less of one. A figure keyed to a named constant
/// goes stale when the constant moves; a figure keyed to the MESH goes stale
/// when the mesh moves, and nothing in its label says which kind it is. Hence
/// the re-measurement date beside the original.
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
#[allow(clippy::too_many_arguments)] // `index` (fix round 1: build-once-pass-in, per Nathan's ruling) pushed this to 8; Task 5's `settlements`/`discovered` push it to 10; Task 7's `volcanoes`/`waterfalls` push it to 12 — mirroring `hornvale_game_core::render_with`'s own allow
pub fn draw_with(
    terrain: &GeneratedTerrain,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    f: &Frame,
    win: &Window,
    w: u16,
    h: u16,
    colour_allowed: bool,
    settlements: &BTreeMap<Vertex, u64>,
    caves: &BTreeSet<Vertex>,
    volcanoes: &BTreeSet<Vertex>,
    waterfalls: &[Vertex],
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
        volcanoes,
        waterfalls,
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
            let (glyph, color) = glyph_and_color_for(tile.water, tile.band);
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

/// Where `vertex` lands on a `width`x`height` `dst`, or `None` if the
/// projection puts it above the polar clamp or [`tile_on_screen`] puts it
/// off the window (above/left) or off the drawn plate (right/bottom).
///
/// **Factored out at Task 7, not written twice.** [`draw_feature_layer`]'s
/// own `place` closure needs this to decide where to paint; its
/// viewport-relative settlement ranking (see that function's own doc) needs
/// the SAME answer BEFORE any glyph is chosen, to know which settlements
/// are even candidates for the comparison. Two copies of "is this vertex
/// on screen, and where" is exactly the shape Task 6's fix round 1 already
/// paid down once for [`tile_on_screen`] itself (see that function's own
/// doc on the mutation it cost).
fn project_onto_screen(
    geo: &Geosphere,
    f: &Frame,
    win: &Window,
    width: u32,
    height: u32,
    vertex: Vertex,
) -> Option<(u32, u32)> {
    let (virtual_w, virtual_h) = virtual_dims(win.depth);
    let g = geo.coord(vertex);
    let (plate_row, plate_col) =
        mercator::project(f, g.latitude, g.longitude, virtual_w, virtual_h)?;
    let (drow, dcol) = tile_on_screen(win, virtual_w, plate_row, plate_col)?;
    if dcol >= width || drow >= height {
        return None;
    }
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
/// **Draw precedence, low to high (a later kind wins a shared cell), and
/// where it comes from (Task 7 pre-dispatch ruling AA):**
/// waterfalls, then volcanoes, then caves, then settlements. (A river
/// delta briefly sat between waterfalls and volcanoes here; fix round 1
/// removed the feature outright — see [`WATERFALL_GLYPH`]'s own doc — so
/// this order no longer names it.) Caves-then-settlements is unchanged
/// from Task 5 (`if/else` precedence carried forward as draw order).
/// Waterfall is new to this layer and carries no [`FeatureClass`] of its
/// own (`GeneratedTerrain` tracks it as bare vertices, not an individuated
/// feature), so it takes the LOWEST rank among the new additions — a
/// documented placement, not a derivation. Volcano is the one new kind
/// that IS a landscape feature (`FeatureClass::Volcano`), and Nathan's
/// ruling is explicit: use the EXISTING salience ordering rather than
/// invent a second one. `FeatureClass::salience` puts `Volcano` at 0 — the
/// single most specific class the landscape system has — so it sits
/// directly below the two point-site kinds this layer already drew, ahead
/// of the landform kind that has no salience of its own to consult. Real
/// collisions between any two of these four kinds are rare in practice
/// (measured: none in the seed 42 windows this task's own tests exercise),
/// so this order is a documented tie-break for the case, not a
/// load-bearing gameplay rule.
///
/// **Volcanoes are discovery-gated; a waterfall is not, and that split is
/// deliberate, not an oversight.** A volcano is an EXTENT feature in
/// `hornvale_terrain::landscape` — `FeatureClass::Volcano`, already
/// wrapped as [`crate::discovery::FeatureId::Extent`] — so the SAME
/// discovery mechanism that already fires when a possession walks onto
/// any vertex of any landscape feature's extent
/// (`Driver::update_discovery`'s `for id in self.index.at(vertex)` loop,
/// already shipped, untouched by this task) already records a volcano the
/// instant its slopes are walked. This layer only had to start reading
/// that existing fact to draw it. A waterfall is a bare `Vertex`
/// `GeneratedTerrain` reports (`waterfalls()`) — the landscape feature
/// system does not carry an identity for it, and the task's own interface
/// note forbids minting a new feature enum to give it one. Rather than
/// invent that identity, it draws as GROUND TRUTH, unconditionally — the
/// same epistemic status the relief and water ladders already have (a
/// river or a mountain range is never gated on "has this been
/// discovered", so a knickpoint on that same channel is not either). This
/// is a judgement call flagged for review, not a claim that the design
/// space has only one right answer here.
///
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
    settlements: &BTreeMap<Vertex, u64>,
    caves: &BTreeSet<Vertex>,
    volcanoes: &BTreeSet<Vertex>,
    waterfalls: &[Vertex],
    discovered: &Discovered,
) {
    let width = u32::from(dst.width());
    let height = u32::from(dst.height());

    // Task 7: which settlements draw MAJOR ([`SETTLEMENT_MAJOR_GLYPH`])
    // rather than minor. The ranking is VIEWPORT-RELATIVE — only
    // settlements that would actually be drawn HERE (discovered AND
    // on-screen) enter the comparison set — so a town can flip between
    // the two glyphs as the reader pans. Known and accepted (task brief):
    // "what is notable here" changes with what "here" is. Computed once,
    // before any glyph is chosen, so drawing itself never influences the
    // ranking it depends on.
    let mut in_frame: Vec<(Vertex, u64)> = settlements
        .iter()
        .filter(|&(&vertex, _)| discovered.contains(FeatureId::Settlement(vertex)))
        .filter(|&(&vertex, _)| project_onto_screen(geo, f, win, width, height, vertex).is_some())
        .map(|(&vertex, &population)| (vertex, population))
        .collect();
    // Population descending, ties broken by vertex ascending — total and
    // deterministic (no `total_cmp` needed: population is an integer
    // count), matching this module's own no-`HashMap` discipline.
    in_frame.sort_unstable_by_key(|&(vertex, population)| (std::cmp::Reverse(population), vertex));
    let major_count = (MAJOR_SETTLEMENT_QUANTILE * in_frame.len() as f64).ceil() as usize;
    let major: BTreeSet<Vertex> = in_frame
        .iter()
        .take(major_count)
        .map(|&(vertex, _)| vertex)
        .collect();

    // `gate`: `None` draws unconditionally (ground truth — waterfalls);
    // `Some(id)` draws only when `discovered` already carries `id` (a
    // point site or an extent feature — caves, settlements, volcanoes).
    let place =
        |vertex: Vertex, gate: Option<FeatureId>, glyph: char, color: [u8; 3], grid: &mut Grid| {
            if let Some(id) = gate
                && !discovered.contains(id)
            {
                return;
            }
            let Some((drow, dcol)) = project_onto_screen(geo, f, win, width, height, vertex) else {
                return;
            };
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

    for &vertex in waterfalls {
        place(vertex, None, WATERFALL_GLYPH, WATERFALL_COLOR, dst);
    }
    for &vertex in volcanoes {
        let id = FeatureId::Extent(LandscapeFeatureId {
            class: FeatureClass::Volcano,
            vertex,
        });
        place(vertex, Some(id), VOLCANO_GLYPH, VOLCANO_COLOR, dst);
    }
    for &vertex in caves {
        place(
            vertex,
            Some(FeatureId::Cave(vertex)),
            CAVE_GLYPH,
            CAVE_COLOR,
            dst,
        );
    }
    for &vertex in settlements.keys() {
        let glyph = if major.contains(&vertex) {
            SETTLEMENT_MAJOR_GLYPH
        } else {
            SETTLEMENT_MINOR_GLYPH
        };
        place(
            vertex,
            Some(FeatureId::Settlement(vertex)),
            glyph,
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
/// ([`SETTLEMENT_MINOR_GLYPH`], [`CAVE_GLYPH`]), because a creature standing
/// next to a cave mouth and a discovered cave mouth on the world map are the
/// same thing seen through two channels, and giving them two glyphs would
/// say otherwise. An `"agent"` has no world-map counterpart — point sites
/// are terrain-fixed and an agent is not — so it gets its own.
///
/// **Always the MINOR settlement glyph, never major, and that is a scope
/// choice, not an oversight.** [`SETTLEMENT_MAJOR_GLYPH`] vs
/// [`SETTLEMENT_MINOR_GLYPH`] is a rank among the settlements in the WORLD
/// MAP's own frame (Task 7) — a single perception-packet mark carries no
/// such frame to rank against, so it draws the substance ("a settlement is
/// here") without a size claim it has no comparison set to justify.
///
/// **An UNRECOGNISED kind still draws**, and that is the same infallibility
/// `hornvale_scene::Mark::kind`'s own doc asks of a renderer ("a consumer
/// that does not recognize a kind should still render the mark"): a future
/// mark kind must not silently vanish from the map, so the fallback is the
/// generic agent glyph rather than nothing.
fn mark_glyph(kind: &str) -> char {
    match kind {
        "settlement" => SETTLEMENT_MINOR_GLYPH,
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
    /// Whether the tile is painted ocean. RETAINED alongside
    /// [`Self::water`] rather than derived from it at every call site — the
    /// strip's own invariant (this struct's own doc) is stated on this
    /// field, and `WaterKind::Ocean` disagreeing with `terrain.is_ocean` is
    /// exactly the discrepancy `waterline_probe.rs` documents (the two
    /// predicates disagree on ~8,162 of seed 42's vertices), so collapsing
    /// them into one boolean here would be a silent choice about which
    /// definition the strip means.
    /// type-audit: bare-ok(flag)
    pub ocean: bool,
    /// The mesh facet the tile's own centre falls in, at the WINDOW's rung
    /// — `Facet::containing(centre, win.depth)` exactly. At
    /// [`BAND_B_RUNG`] this is the walk band's own room.
    pub facet: Facet,
    /// A grid-level [`Vertex`] of the painted class — the corner of
    /// [`Self::facet`]'s grid-level triangle nearest the tile's own centre.
    pub vertex: Vertex,
    /// [`Self::vertex`]'s elevation band — [`hornvale_scene::relief_band`]
    /// applied to `terrain.elevation_at(vertex).above(terrain.sea_level())`,
    /// the SAME classifier `windows/scene/src/region.rs` uses for the
    /// `scene/surrounds/v2` wire field (Task 3's shared classifier; see
    /// [`glyph_and_color_for`] for how this and [`Self::water`] together
    /// pick a mark). `u32`, matching `relief_band`'s own return type —
    /// carried at its own width rather than cast to match [`Self::water`].
    /// type-audit: bare-ok(index)
    pub band: u32,
    /// [`Self::vertex`]'s water class — `terrain.water_kind_at(vertex)
    /// .index()` against `hornvale_terrain::WaterKind::LEGEND`, the
    /// canonical pair `region.rs` itself uses (no second classifier is
    /// introduced here). `u8`, matching `WaterKind::index`'s own return
    /// type — carried at its own width rather than cast to match
    /// [`Self::band`].
    /// type-audit: bare-ok(index)
    pub water: u8,
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
/// 2. the address's grid-level ancestor names the QUAD whose corners resolve
///    to the grid vertices terrain is defined on
///    ([`hornvale_terrain::GLOBE_LEVEL`]), and
///    [`Facet::corner_weights_memo`] gives that quad's FOUR corner
///    vertices, memoized;
/// 3. the tile's class is the nearest of those four corners.
///
/// **THE "SAME ANSWER, NOT AN APPROXIMATION" CLAIM BELOW RESTS ON A PREMISE
/// THE PAVEMENT RETIRED, and this paragraph is a pointer, not a verdict.** It
/// argued that a point inside a grid-level TRIANGLE has its nearest mesh
/// vertex among that triangle's three corners, which held while a facet's
/// corners WERE geosphere vertices (decision 0287's corner-is-a-vertex
/// corollary). A cube-sphere quad's corners are not geosphere vertices at all,
/// so the argument no longer runs. Spec section 7's H3a owns the consequence
/// and Task 10 step 3 reports it; nothing here measures or re-states a result.
/// The counts above (a quad, four corners) are corrected because they are
/// facts about the code as it now stands.
///
/// **Step 3 is four dot products.** The retired argument was that a point
/// inside a grid-level triangle has its nearest mesh vertex among that
/// triangle's own three corners, so this reproduced what
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
/// deliberately unused.** They are the bilinear position of the ADDRESSED
/// facet's own centroid; at the grid level they are uniform (`1,1,1,1` — the
/// centroid of the ancestor is the quad's own centre, equidistant from all four
/// corners), which would make every tile inside one quad identical and blocky.
/// The tile's own centre is a strictly finer thing to compare against, and
/// comparing against it costs four dot products rather than a memo key per
/// tile.
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

    // The nearest of the quad's four corners to the tile's own centre.
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

    let band = hornvale_scene::relief_band(terrain.elevation_at(vertex).above(terrain.sea_level()));
    let water = terrain.water_kind_at(vertex).index();

    TileTerrain {
        ocean: terrain.is_ocean(vertex),
        facet,
        vertex,
        band,
        water,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;
    use hornvale_terrain::TerrainPins;

    /// The bound clause 1 of [`mesh_addressing_agrees_with_the_spatial_search`]
    /// asserts: how many grid spacings farther from a tile's own centre the
    /// mesh-addressed vertex may sit than the true nearest vertex does, one
    /// spacing being [`min_edge_rad`] — the GEOSPHERE's own smallest edge.
    ///
    /// **Measured max 1.0622, mean 0.2409** on that test's own window
    /// (fix round 2; the test prints both). **THE BOUND IS ABOVE ONE SPACING,
    /// AND THAT IS THE HONEST STATEMENT OF IT** — the measurement itself is,
    /// so a doc claiming the addressed vertex lands within a single spacing
    /// would be false about a number printed two lines away. What the bound
    /// does exclude is a WHOLE FACET's worth of error: one cube-sphere facet
    /// arc at this rung is `(pi/2)/64 / 0.01729920 = 1.4188` of these
    /// spacings, and 1.25 sits below that, so a misresolution that reached
    /// into a neighbouring facet's corners cannot pass. That is the ceiling
    /// this value is capped by; the 17.7% it leaves over the measurement is
    /// what is left after the cap, not a comfort margin chosen first.
    ///
    /// # THE HISTORY, BECAUSE THE UNIT MOVED TWICE AND THE VALUE DID NOT
    ///
    /// The Pavement's Task 8 measured **1.1051** against `acos(1/sqrt(5))/
    /// 2^depth` — the icosphere's minimum edge, i.e. the same unit as today —
    /// on a chart 1.4188x finer than the mesh. Fix round 1 corrected the
    /// chart's resolution and, as collateral, switched this divisor to the
    /// CUBE's `pi/2` facet arc, reporting **0.7487** and reading it as a 32%
    /// tightening. It was not: the raw angular excess fell only 3.9%
    /// (0.019119 -> 0.018378 rad) and the rest was a 1.4188x larger unit. In
    /// real spacings the figure went 1.1051 -> 1.0622, and the unchanged
    /// threshold of 1.5 silently came to admit 2.13 real spacings — a ~42%
    /// loosening invisible in a diff, which is why the unit is now measured
    /// off the mesh ([`min_edge_rad`]) rather than derived from a base angle
    /// that belongs to the other lattice. 1.25 is the first value this
    /// constant has carried that was set FROM a measurement in the unit it
    /// is stated in.
    ///
    /// **Not a ratchet** — a breach means addressing resolved the wrong facet,
    /// which is a defect and not a drift. If another seed or window ever
    /// exceeds 1.25, the thing to do is find out which facet it resolved, not
    /// to raise this number.
    const MAX_ADDRESSING_EXCESS_SPACINGS: f64 = 1.25;

    /// The floor clause 2 of [`mesh_addressing_agrees_with_the_spatial_search`]
    /// asserts: the fraction of tiles on which mesh addressing and a plain
    /// `nearest()` query pick the SAME vertex. Exact agreement was guaranteed
    /// on the icosphere and is not on the cube-sphere.
    ///
    /// **Measured 0.5036 (2,518 of 5,000) at fix round 1, down from 0.5824 at
    /// The Pavement's Task 8, and the drop is the CHART getting coarser rather
    /// than the addressing getting worse.** `base_facet_arc_rad` was returning
    /// the icosahedron's edge angle, so this window's chart was 1.4188x finer
    /// than the band it drew; a smaller tile keeps its centre nearer a facet
    /// corner, so nearest-of-four-corners agreed with true-nearest more often.
    /// Drawing at the mesh's own resolution doubles a tile's area and spreads
    /// its centre further from any corner.
    ///
    /// # WHY THE LOWERING IS DEFENSIBLE, STATED CORRECTLY THIS TIME
    ///
    /// This doc used to carry the argument that **clause 1 tightened 32% over
    /// the same change**, which was false: 1.1051 -> 0.7487 was mostly a
    /// 1.4188x larger denominator (see
    /// [`MAX_ADDRESSING_EXCESS_SPACINGS`]). The true reasons, all three
    /// measured rather than argued:
    ///
    /// 1. **The real geometric error did not worsen — it improved 3.9%.**
    ///    Raw angular excess 0.019119 -> 0.018378 rad; in the mesh's own
    ///    spacings, 1.1051 -> 1.0622. So the quantity that actually matters
    ///    for whether a tile is painted with the right terrain moved in the
    ///    GOOD direction while this proxy fell.
    /// 2. **`a_tile_resolves_to_the_facet_that_contains_it` still holds
    ///    exactly**, so nothing about which facet is resolved has moved; what
    ///    changed is only which of that facet's four corners wins a
    ///    tie-adjacent comparison.
    /// 3. **This floor is a proxy whose value tracks the chart's resolution
    ///    relative to the mesh, not the addressing's quality.** It fell
    ///    because the chart stopped being finer than the mesh it draws —
    ///    which was itself the fix. A floor that refused to move here would
    ///    be pinning the old resolution bug, not guarding the addressing.
    ///
    /// The measurement has 18 tiles of headroom over 0.50 and is
    /// deterministic — a fixed seed-42 world and a fixed 5,000-tile window,
    /// so it cannot flap.
    ///
    /// **A ratchet**: raising it is always allowed and is the direction of
    /// travel. This is the second time it has been lowered, and both times for
    /// a stated geometric reason rather than to make a run pass.
    const MIN_ADDRESSING_AGREEMENT: f64 = 0.50;

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

    /// One grid spacing of the GEOSPHERE, in radians of central angle —
    /// the unit clause 1 of
    /// [`mesh_addressing_agrees_with_the_spatial_search`] reports its excess
    /// in, measured off the mesh being normalised rather than derived from a
    /// base angle.
    ///
    /// # WHY THIS IS NOT [`base_facet_arc_rad`] OVER `2^depth`
    ///
    /// **The two meshes came apart at decision 0506 and this quantity
    /// belongs to the one that did not move.** 0506 replaced the walk band's
    /// OCCUPANCY lattice with an 8-connected cube-sphere and deliberately
    /// kept the icosphere as the FIELD substrate;
    /// [`hornvale_kernel::Geosphere`]'s own first sentence still says
    /// "icosphere region graph". Clause 1 measures a distance from a tile
    /// centre to a geosphere VERTEX, so its unit is a geosphere edge. Fix
    /// round 1 switched this divisor to the cube's `pi/2` facet arc along
    /// with the chart's width, where it does not belong: at level 6 that is
    /// `(pi/2)/64 = 0.02454369` rad, **18.7% above the largest edge the
    /// geosphere has**, so it is not any spacing of the mesh under
    /// measurement and it silently loosened the assertion ~42% in real units
    /// while the threshold constant looked unchanged.
    ///
    /// # WHY THE MINIMUM EDGE, AND NOT THE MEAN OR THE MAX
    ///
    /// **An icosphere's edges are not uniform, so the choice is a real one:**
    /// at level 6 (40,962 vertices) the central angles run min `0.01729920`,
    /// mean `0.01888557`, max `0.02067341` — the max is 19.5% above the min,
    /// so the same raw excess reports as three visibly different numbers.
    /// The minimum is taken for two reasons. It is the CONSERVATIVE choice:
    /// the smallest real spacing yields the largest ratio, so a bound stated
    /// against it is the strongest of the three and cannot be satisfied by
    /// picking a generous unit. And it is the HISTORICAL unit — the divisor
    /// this test used before fix round 1, `acos(1/sqrt(5))/2^depth`, is
    /// exactly the icosphere's minimum edge, so the figure recorded on
    /// [`MAX_ADDRESSING_EXCESS_SPACINGS`] stays comparable across the whole
    /// campaign instead of resetting its meaning a second time.
    ///
    /// Each undirected edge is visited once (`n > v`); the minimum is
    /// unaffected by that either way, and it halves the `acos` count.
    fn min_edge_rad(geo: &Geosphere) -> f64 {
        let mut min = f64::INFINITY;
        for v in geo.vertices() {
            let p = geo.position(v);
            for &n in geo.neighbors(v) {
                if n <= v {
                    continue;
                }
                let q = geo.position(n);
                let d = hornvale_kernel::math::acos(
                    (p[0] * q[0] + p[1] * q[1] + p[2] * q[2]).clamp(-1.0, 1.0),
                );
                if d < min {
                    min = d;
                }
            }
        }
        min
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
        // independently. **The rounding no longer bites at all, and the history
        // is worth keeping**: on the ICOSAHEDRAL base angle this function used
        // until fix round 1, rung 11 gave 11,623 and rung 12 gave 23,245, so
        // doubling THAT coarse rung OVERSHOT by one -- a one-sided tolerance
        // failed there, which is what the first draft of this assertion did. On
        // the cube-sphere's own quarter turn the width is `4 * 2^depth`
        // exactly, an integer at every rung (rung 12 gives 16,384 and rung 13
        // gives 32,768), so nothing rounds anywhere. The two-sided tolerance
        // stays: which pair is exact is a property of the arithmetic and not of
        // the rule being asserted, and a future projection whose base angle is
        // not a quarter turn would want it back.
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

    /// **THE CHART'S WIDTH IS THE MESH'S OWN EQUATORIAL FACET COUNT** — asked
    /// of the mesh, never of this module's derivation.
    ///
    /// This is the assertion whose absence let band B be drawn **1.4188x too
    /// fine** for the whole of The Pavement. [`base_facet_arc_rad`] returned
    /// the ICOSAHEDRON's base-face edge angle after decision 0506 replaced the
    /// icosahedron, so `virtual_dims(13)` sized a 46,490-column chart over a
    /// 32,768-facet band and every claim of "one tile per facet" in this crate
    /// was false by 42%. Nothing caught it because
    /// `tests/walk_band_agreement.rs` and
    /// `cli/tests/suite/walk_depth_agreement.rs` pin the depth INTEGER, and
    /// nothing compared chart RESOLUTION to mesh spacing.
    ///
    /// The reference is `Facet::containing`, walked along the equator at 4x the
    /// expected column count and counted as a set — the mesh answering the
    /// question in its own terms rather than this module restating its own
    /// arithmetic. Every rung from the base face to [`BAND_B_RUNG`] must agree
    /// EXACTLY, not within a tolerance: the count is an integer property of the
    /// lattice.
    ///
    /// **What it is blind to.** It measures the EQUATOR, which on this base
    /// mesh is a great circle through four face centres — the circle
    /// [`base_facet_arc_rad`] is defined against and the one a Mercator chart's
    /// width is. It says nothing about the chart's HEIGHT (that is
    /// [`the_clamped_mercator_is_nearly_square_in_tiles`]'s job), nothing about
    /// whether individual facets are equal in arc (the tangent warp makes them
    /// unequal, and the COUNT is exact anyway), and nothing about a meridian,
    /// where the two polar faces make facets-per-degree vary.
    #[test]
    fn the_chart_width_is_the_meshs_own_equatorial_facet_count() {
        for depth in [0u32, 1, 2, 3, GLOBE_RUNG, BAND_B_RUNG] {
            let expect = 4u32 << depth;
            // 4x oversampling: the tangent warp makes equatorial facets unequal
            // in arc, so a 2x walk could step over a narrow one and undercount.
            let samples = 4u64 * u64::from(expect);
            let mut seen: std::collections::BTreeSet<hornvale_kernel::Facet> =
                std::collections::BTreeSet::new();
            for k in 0..samples {
                let lon = -180.0 + 360.0 * (k as f64) / (samples as f64);
                let pos = hornvale_kernel::math::unit_sphere_from_lat_lon(0.0, lon);
                seen.insert(hornvale_kernel::Facet::containing(pos, depth));
            }
            assert_eq!(
                seen.len() as u32,
                expect,
                "the mesh has {} facets around its equator at depth {depth}, not the {expect} \
                 this module's arithmetic assumes",
                seen.len()
            );
            let (w, _) = virtual_dims(depth);
            assert_eq!(
                w,
                expect,
                "virtual_dims({depth}) sizes a {w}-column chart over a band with {expect} \
                 facets around its equator. One tile per facet is what this client claims; a \
                 ratio of {:.4} is what it would be drawing.",
                f64::from(w) / f64::from(expect)
            );
        }
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
            &BTreeMap::new(),
            &BTreeSet::new(),
            &BTreeSet::new(),
            &[],
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
            &BTreeMap::new(),
            &BTreeSet::new(),
            &BTreeSet::new(),
            &[],
            &Discovered::default(),
        );
        // THE VACUITY GUARD: the compared region must straddle a real
        // coastline. A monochrome patch makes every assertion below
        // trivially true no matter what `draw_with` does with `w`.
        let glyphs: Vec<Option<char>> = (0..8u16)
            .flat_map(|y| (0..8u16).map(move |x| (x, y)))
            .map(|(x, y)| narrow.get(x, y).and_then(|c| c.glyph))
            .collect();
        // The Legend: the terrain vocabulary is no longer a `LAND_GLYPH`/
        // `OCEAN_GLYPH` binary, so "land" here is "drawn, and not the
        // ocean glyph" — any of the water or relief marks — rather than
        // one hardcoded character. The guard's own point (a real
        // coastline sits in the compared region) is unchanged.
        let ocean = glyphs.iter().filter(|g| **g == Some(OCEAN_GLYPH)).count();
        let land = glyphs
            .iter()
            .filter(|g| g.is_some() && **g != Some(OCEAN_GLYPH))
            .count();
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

        let both_caves: BTreeSet<Vertex> = std::iter::once(shared).collect();
        let both_settlements: BTreeMap<Vertex, u64> = std::iter::once((shared, 1)).collect();
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
            &both_settlements,
            &both_caves,
            &BTreeSet::new(),
            &[],
            &discovered,
        );
        assert_eq!(
            grid.get(sx, sy).and_then(|c| c.glyph),
            // The sole in-frame settlement is trivially its own top 10%
            // (ceil(0.1 * 1) == 1), so it draws MAJOR here — this test's
            // subject is draw ORDER, not the size tier.
            Some(SETTLEMENT_MAJOR_GLYPH),
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
            &BTreeMap::new(),
            &caves,
            &BTreeSet::new(),
            &[],
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
        let empty_settlements = BTreeMap::new();
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
            &BTreeSet::new(),
            &[],
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

        let empty_settlements = BTreeMap::new();
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
            &BTreeSet::new(),
            &[],
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
            &BTreeSet::new(),
            &[],
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

    /// **Mesh addressing is a BOUNDED approximation of the spatial search,
    /// and was an exact replacement for it until The Pavement.**
    /// [`terrain_at_tile`] never calls [`NearestVertexIndex::nearest`]: it
    /// addresses the tile's grid-level facet and takes the nearest of that
    /// facet's own corners.
    ///
    /// **The claim this test used to make is DISSOLVED, and this is the
    /// deliberate call The Pavement's Task 8 took on it.** The claim was
    /// exact equality on every tile — sound because on the icosphere a
    /// grid-level triangle's corners WERE geosphere vertices, so a point
    /// inside a facet had its nearest mesh vertex among that facet's corners
    /// by construction. A cube-sphere quad's corners are not geosphere
    /// vertices ([`terrain_at_tile`]'s own doc says so), so the guarantee is
    /// gone: measured on this test's own 5,000-tile equatorial window, the
    /// two methods now agree on **2,518 of 5,000 tiles (50.36%)** — it was
    /// 2,912 (58.24%) until fix round 1 corrected the chart's resolution, and
    /// [`MIN_ADDRESSING_AGREEMENT`]'s doc has the mechanism. Deleting
    /// the test would drop the only coverage `terrain_at_tile`'s addressing
    /// has; re-pinning the exact equality would pin a claim the geometry no
    /// longer supports. So the assertion is replaced by the two claims that
    /// ARE true, both measured before being written down:
    ///
    /// 1. **The error is bounded well inside one FACET, though not inside one
    ///    grid spacing.** The addressed vertex is at most
    ///    [`MAX_ADDRESSING_EXCESS_SPACINGS`] grid spacings farther from the
    ///    tile's own centre than the true nearest vertex is — measured max
    ///    **1.0622**, mean **0.2409**, one grid spacing being
    ///    [`min_edge_rad`], the geosphere's own smallest edge at its level.
    ///    The max exceeding 1.0 is stated rather than smoothed over: a
    ///    handful of tiles do land a vertex past their immediate neighbour,
    ///    and the bound's job is to keep that inside one cube facet (1.4188
    ///    of these spacings) rather than to claim it never happens. That is
    ///    what makes this an approximation rather than a wrong answer: the
    ///    client samples a NEARBY vertex, never a distant one, so no tile is
    ///    ever painted with terrain from across the map.
    /// 2. **Exact agreement stays above a recorded floor**
    ///    ([`MIN_ADDRESSING_AGREEMENT`]), a ratchet in the direction of
    ///    travel: lowering it is a deliberate act, and raising it is always
    ///    allowed.
    ///
    /// The two halves catch different failures, which is why both are here.
    /// Clause 1 catches an addressing that resolves the wrong facet
    /// altogether. Clause 2 catches one that resolves the right facet and
    /// then picks badly among its corners — a mutation returning the first
    /// corner unconditionally stays inside clause 1's bound and would sail
    /// through it.
    ///
    /// **What this no longer proves, stated plainly:** the drawn terrain is
    /// NOT guaranteed to be the terrain at the tile's nearest mesh vertex.
    /// Spec section 7's H3a owns whether that matters visually; this test
    /// bounds it rather than deciding it.
    ///
    /// **Retargeted, not renamed away.** This test was
    /// `at_a_fine_enough_window_area_majority_agrees_with_point_sampling`,
    /// and it pinned the same comparison against the 49-point vote The
    /// Quadrat's Task 3 removed; the subject moved, the assertion did not.
    /// Its threshold TIGHTENED from `> 0.97` to exact equality on every
    /// cell, because the two are now the same function rather than two
    /// samplings that mostly agree — a `> 0.97` threshold against a method
    /// that agrees exactly is a gate that never gates.
    ///
    /// **Retargeted a second time, at The Legend (Task 6).** This test used
    /// to compare the drawn GLYPH against `if point_ocean { OCEAN_GLYPH }
    /// else { LAND_GLYPH }` — sound only because the terrain vocabulary was
    /// exactly those two characters, so a glyph was a faithful proxy for
    /// "which vertex got addressed". The Legend retires that binary (the
    /// whole point of the task this test's own module now serves), so the
    /// proxy is retired with it: the comparison now asks the mesh-addressed
    /// [`TileTerrain::vertex`] directly, which is the thing the old glyph
    /// comparison was always standing in for, and states the invariant
    /// MORE strictly than before — vertex identity, not merely agreement on
    /// which side of the ocean/land boundary the vertex fell.
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
        let mut memo = RoomMeshMemo::default();

        let mut agree = 0u32;
        let mut total = 0u32;
        let mut land = 0u32;
        let mut max_excess = 0.0f64;
        // The MEAN excess is accumulated as well as the max, because this
        // test's own doc quotes both and a figure a doc quotes should be a
        // figure the test prints.
        let mut sum_excess = 0.0f64;
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
                let pos = hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon);
                let tile = terrain_at_tile(
                    &terrain, &geo, &index, &mut memo, &f, &win, vw, vh, row, col,
                );
                total += 1;
                if tile.vertex == point_vertex {
                    agree += 1;
                }
                if !terrain.is_ocean(point_vertex) {
                    land += 1;
                }
                // How much FARTHER the addressed vertex sits from this tile's
                // own centre than the true nearest one does — clause 1's
                // quantity. Central angle, so it is comparable to the mesh's
                // own edge length without a length scale anywhere in it.
                let angle_to = |v| {
                    let q: [f64; 3] = geo.position(v);
                    hornvale_kernel::math::acos(
                        (q[0] * pos[0] + q[1] * pos[1] + q[2] * pos[2]).clamp(-1.0, 1.0),
                    )
                };
                let ex = angle_to(tile.vertex) - angle_to(point_vertex);
                if ex > max_excess {
                    max_excess = ex;
                }
                sum_excess += ex;
            }
        }
        // THE VACUITY GUARD. An all-ocean (or all-land) patch would make
        // every comparison trivially agree by construction. The compared
        // region must straddle a real coastline for the comparison below to
        // discriminate.
        let total_cells = u32::from(w) * u32::from(h);
        assert!(
            land > 0 && land < total_cells,
            "the compared region must straddle a coastline or this test proves \
             nothing: land={land} of {total_cells}"
        );

        // CLAUSE 1: the error is bounded by about one grid spacing.
        // The GEOSPHERE's own smallest edge, measured off the mesh whose
        // vertices the excess above is a distance between — NOT the cube's
        // facet arc, which is the occupancy lattice's unit and 18.7% above
        // the largest edge this mesh has. `min_edge_rad`'s doc has the
        // whole of why.
        let spacing = min_edge_rad(&geo);
        assert!(
            spacing.is_finite() && spacing > 0.0,
            "the measured grid spacing must be a real angle, got {spacing}"
        );
        let excess = max_excess / spacing;
        let mean_excess = sum_excess / f64::from(total) / spacing;
        // Printed, not merely asserted: both quantities are the ones the two
        // recorded constants below were set from, and fix round 1 moved both
        // (the chart's width, and the spacing's own base angle). A number a
        // test computes and never shows is a number nobody can re-record.
        eprintln!(
            "mesh addressing: agree {agree}/{total} ({:.4}), max excess {excess:.4} grid \
             spacings, mean {mean_excess:.4} (one spacing = the geosphere's min edge, \
             {spacing:.8} rad at grid depth {})",
            f64::from(agree) / f64::from(total),
            geo.depth()
        );
        assert!(
            excess <= MAX_ADDRESSING_EXCESS_SPACINGS,
            "the mesh-addressed vertex sat {excess:.4} grid spacings farther from a tile's \
             own centre than the true nearest vertex, over the bound of \
             {MAX_ADDRESSING_EXCESS_SPACINGS}. Addressing resolved the wrong facet, or the \
             grid level it resolves through is not the one terrain lives on — this is not a \
             ratchet to loosen."
        );

        // CLAUSE 2: exact agreement stays above the recorded floor.
        let rate = f64::from(agree) / f64::from(total);
        assert!(
            rate >= MIN_ADDRESSING_AGREEMENT,
            "mesh addressing agreed with the spatial search on {agree}/{total} tiles \
             ({rate:.4}), under the recorded floor of {MIN_ADDRESSING_AGREEMENT}. Exact \
             agreement is no longer guaranteed (see this test's own doc), but a drop this \
             far means the corner chosen inside the facet is wrong, not that the geometry \
             changed. Lowering this floor is a deliberate act; say why."
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
            let (vw, vh) = virtual_dims(depth);
            // PARKED AT THE EQUATOR, and the row offset is load-bearing.
            // This sampled the chart's top-left corner (`origin_row: 0`),
            // which is the north pole, where Mercator's stretch is at its
            // most extreme: on the square lattice a 4x4 block of tiles up
            // there falls entirely inside ONE grid-level facet, and the
            // discrimination guard below fired correctly — 1 distinct facet
            // over 16 tiles — the moment the mesh became a cube-sphere. The
            // equator is where the chart is closest to the mesh's own
            // spacing, which is both the honest place to sample and the
            // hardest place for the two addressing methods to agree; it is
            // where `mesh_addressing_agrees_with_the_spatial_search` parks
            // for the same reason.
            let win = Window {
                depth,
                origin_col: 0,
                origin_row: vh / 2,
            };
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
                    // THROUGH THE WINDOW ORIGIN, exactly as `terrain_at_tile`
                    // does. While `origin_row` was 0 the two forms were the
                    // same expression and nothing could tell them apart; the
                    // equator offset above pulls them apart, which makes this
                    // a real comparison against the function's own input
                    // rather than a coincidence of the fixture.
                    let (lat, lon) =
                        mercator::unproject(&f, win.origin_row + row, win.origin_col + col, vw, vh);
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
        let settlements = BTreeMap::new();
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
            &BTreeSet::new(),
            &[],
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
            &BTreeSet::new(),
            &[],
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
    /// a `BTreeSet<Vertex>` for caves and a `BTreeMap<Vertex, u64>` for
    /// settlements (Task 7 widens the latter to carry population), and
    /// asks the terrain nothing — so the same vertex exercises the
    /// settlement path when it is handed to that roster instead, with an
    /// arbitrary population.
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
            &BTreeMap::new(),
            &caves,
            &BTreeSet::new(),
            &[],
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
        let cave_roster: BTreeSet<Vertex> = std::iter::once(site).collect();
        let settlement_roster: BTreeMap<Vertex, u64> = std::iter::once((site, 1)).collect();
        let empty_caves = BTreeSet::new();
        let empty_settlements = BTreeMap::new();
        let mut memo = RoomMeshMemo::default();
        let bare = draw_terrain_layer(&terrain, &geo, &index, &mut memo, &f, &win, w, h, false);
        let bare_text = bare.to_plain_text();

        // The cave and settlement cases take DIFFERENT roster types since
        // Task 7 widened settlements to carry population, so they are two
        // blocks rather than one homogeneous loop over both — each still
        // proves the identical property: an undiscovered site is never
        // drawn, and a discovered one is drawn at its own resolved cell.
        {
            let mut g = bare.clone();
            draw_feature_layer(
                &mut g,
                &geo,
                &f,
                &win,
                false,
                &empty_settlements,
                &cave_roster,
                &BTreeSet::new(),
                &[],
                &Discovered::default(),
            );
            assert_eq!(
                g.to_plain_text(),
                bare_text,
                "an UNdiscovered cave was drawn"
            );

            let mut discovered = Discovered::default();
            discovered.record(FeatureId::Cave(site));
            draw_feature_layer(
                &mut g,
                &geo,
                &f,
                &win,
                false,
                &empty_settlements,
                &cave_roster,
                &BTreeSet::new(),
                &[],
                &discovered,
            );
            assert_eq!(
                g.get(x, y).unwrap().glyph,
                Some(CAVE_GLYPH),
                "a DISCOVERED cave was not drawn"
            );
        }

        {
            let mut g = bare.clone();
            draw_feature_layer(
                &mut g,
                &geo,
                &f,
                &win,
                false,
                &settlement_roster,
                &empty_caves,
                &BTreeSet::new(),
                &[],
                &Discovered::default(),
            );
            assert_eq!(
                g.to_plain_text(),
                bare_text,
                "an UNdiscovered settlement was drawn"
            );

            let mut discovered = Discovered::default();
            discovered.record(FeatureId::Settlement(site));
            draw_feature_layer(
                &mut g,
                &geo,
                &f,
                &win,
                false,
                &settlement_roster,
                &empty_caves,
                &BTreeSet::new(),
                &[],
                &discovered,
            );
            assert_eq!(
                g.get(x, y).unwrap().glyph,
                // The sole in-frame settlement is trivially its own top
                // 10% (ceil(0.1 * 1) == 1) — see
                // `a_settlement_outranks_a_cave_on_the_same_cell`'s own
                // note on why a one-settlement roster always draws MAJOR.
                Some(SETTLEMENT_MAJOR_GLYPH),
                "a DISCOVERED settlement was not drawn"
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
        let settlements = BTreeMap::new();
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
            &BTreeSet::new(),
            &[],
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
            &BTreeSet::new(),
            &[],
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
                Some(SETTLEMENT_MINOR_GLYPH),
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
        assert_eq!(mark_glyph("settlement"), SETTLEMENT_MINOR_GLYPH);
        assert_eq!(mark_glyph("cave"), CAVE_GLYPH);
        assert_eq!(mark_glyph("agent"), AGENT_GLYPH);
        assert_eq!(
            mark_glyph("some-kind-from-a-later-campaign"),
            AGENT_GLYPH,
            "an unknown kind must still get a glyph, never be skipped"
        );
    }
}
