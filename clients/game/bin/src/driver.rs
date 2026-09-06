//! The driver: the one place in `hornvale-game` allowed to know `Session`,
//! `Body`, or `WorldContext` exist.
//!
//! The architecture rule this module exists to hold (The Quire spec section
//! 6): **the driver drives across the linker; everything displayed comes
//! across the serializer.** `Driver` links `hornvale-vessel` and owns
//! exactly the two functions the spec names — start a session, hand it a
//! line — and only [`String`] (a `vessel/session/v2` document) ever crosses
//! back out. `hornvale-game-core` never depends on `hornvale-vessel` at all,
//! so no render path built on top of this module can reach `npc_grievance`,
//! `would_turn_hostile`, `agent()` or `knowledge()` — there is no symbol to
//! reach. If a typed value ever looks convenient to return from here, that
//! is the signal the containment is about to break, not a reason to add one.
//!
//! Mirrors `hv_start`/`hv_handle` in `clients/vessel/wasm/src/lib.rs`, the
//! existing precedent for a native program that links the sim and reads it
//! back out only as JSON.
//!
//! **The Portolan adds a second, narrower channel: [`Cursor`] and a resolved
//! `&str`.** Neither is vessel-typed — `Cursor` is `hornvale-game-core`'s own
//! screen-position struct and the resolved text is a plain name, the same
//! category of thing `snapshot()`'s narration prose already is — so this
//! does not reopen the containment described above; it is still true that
//! no `Body`/`Knowledge`/`WorldContext` value ever crosses out.
//!
//! **Resolution genuinely tracks the cursor.** Spec §3.1 assigns the cursor
//! query to `bin`; §9 defers "the chamber / delve resolvers" (session-level
//! marks and agents at those bands) to a future campaign — so the chamber
//! band still honestly answers [`NOTHING_HERE_YET`]. Either way, the
//! *pointed-at* vertex is what gets resolved, not the observer's own (an
//! earlier revision of this module resolved the observer's vertex
//! unconditionally — a fix round caught that this made the strip
//! position-invariant while the cursor visibly moved, exactly the "a wrong
//! name is indistinguishable from a right one" failure the design spec warns
//! against).
//!
//! **TWO RESOLVERS ON THE WALK BAND, most specific first (The Quadrat, Task
//! 6).** Every rung of the walk band draws the Mercator raster now, band B
//! included, so [`Driver::resolve_world_view`] — the tile
//! [`plate::terrain_at_tile`] painted — is the answer almost everywhere.
//! [`Driver::resolve_walk_band`] is tried first and answers only where the
//! band-B PERCEPTION OVERLAY drew something under the cursor, which is the
//! more specific fact and the one the picture is showing there.
//!
//! `Driver::world_view` is gone with the mode it named; the rung question it
//! used to answer is [`Driver::at_walk_band_rung`] and the band question is
//! [`Driver::raster_is_drawn`].
//!
//! The chain from a screen position to a [`hornvale_kernel::Vertex`], for
//! both resolvers, is the same one and it is `bin`'s own: a
//! `scene/surrounds/v2` facet's `room: u64` unpacks
//! ([`hornvale_kernel::FacetId::unpack`]) to a real
//! [`hornvale_kernel::Facet`], whose [`hornvale_kernel::Facet::coord`] feeds
//! the same `NearestVertexIndex` lookup already used for the observer. The
//! scene is [`Driver::walk_band_scene`]'s cached packet, refreshed once per
//! turn rather than re-derived with `self.session.purview(0)` on every
//! redraw (The Gallery, Task 10 round 2) — the same underlying `purview(0)`
//! call `Session::snapshot` itself makes for the walk band. The resolver and
//! the picture still cannot disagree, now because both read the one shared
//! cache rather than because two independently-called derivations happened
//! to agree.
//!
//! **This used to route through `hornvale_game_core::chart::cell_at`**, the
//! `core` chart's own polar box lookup, matched back to the real scene by
//! INDEX. That step is gone (Task 6): `core`'s chart is not what band B
//! draws any more, so asking it which box the cursor was in would have named
//! a facet from a picture nobody drew. The overlay's own placement
//! ([`plate::perceived_at`]) answers instead, and it is `bin` that placed
//! those facets in the first place, so no index round-trip is needed.
//!
//! No new geometry was written for any of this: `FacetId::unpack`,
//! `Facet::coord` and `NearestVertexIndex::nearest` all already existed, and
//! `kernel::room` has no inverse of `bearing_to`/`distance_rad_to` (a
//! destination from an origin, a bearing and a distance), so this
//! deliberately does not need one.

use crate::discovery::{Discovered, FeatureId, Visited};
use crate::history::History;
use crate::input::Action;
use crate::line::Line;
use crate::mercator::{self, Frame};
use crate::plate::{self, BAND_B_RUNG, GLOBE_RUNG, Window};
use hornvale_astronomy::SkyPins;
use hornvale_game_core::{ChartMarks, CurrentTurnNouns, Cursor, Focus, Lexicon};
use hornvale_kernel::{Facet, FacetId, NearestVertexIndex, Seed, Vertex, World};
use hornvale_language::{MorphOptions, Phonology};
use hornvale_terrain::GeneratedTerrain;
use hornvale_terrain::TerrainPins;
use hornvale_terrain::landscape::{FeatureClass, VertexFeatureIndex};
use hornvale_vessel::{
    PossessOpts, PossessTarget, Session, Turn, VesselError, WorldContext, snapshot_json,
};
use hornvale_worldgen::{
    BuildError, SettlementPins, WorldComponents, build_world, gazetteer_features, language_of_in,
    morph_options, resolve_chain_at, terrain_of,
};
use std::collections::BTreeSet;

/// What the strip says, with the map focused, over a band this campaign has
/// no resolver for (walk and chamber both draw a real plate; only the
/// terrain-feature index answers a query — spec §3.1/§9). Faking a
/// resolution would be worse than refusing: a wrong name is
/// indistinguishable from a right one, and this string never is one.
const NOTHING_HERE_YET: &str = "nothing here yet";

/// The rung line's opening word — see [`Driver::rung_caption`]. Named once
/// so the tests that look for the line match on the shipped wording rather
/// than on a second copy of it.
const RUNG_OPENING: &str = "rung";

/// The fixed, sim-authored prefix `windows/vessel/src/session.rs`'s
/// `delve_at` prints on a SUCCESSFUL delve (`Session::delve`'s own
/// `Surface -> Undercroft` transition, F9's cited arrival predicate for
/// caves) — never on any refusal (a sealed cave, no cave at all, already
/// underground, or already inside a structure each print their own
/// distinct refusal text). This is the only signal that exists for cave
/// discovery.
///
/// **This paragraph used to claim the wire's own `band` tag "deliberately
/// folds underground into `\"walk\"`", citing a test named
/// `the_underground_band_folds_into_walk_as_map_does`. That has been false
/// since The Gallery's Task 7**, which gave the pane its own `band:
/// "underground"` tag (`vessel/level/v1`) — the cited test was renamed to
/// `the_pane_and_the_verb_agree_underground` at Task 8's landing, once the
/// `map` verb caught up to the same distinction (see
/// `hornvale_game_core::snapshot`'s own module doc for the full history).
/// The band tag distinguishing `underground` from `walk` does not make it a
/// typed alternative here, though: what this constant answers is not "which
/// band is the possession on" (that question has its own typed field now)
/// but "did a delve **succeed this turn**" — a one-shot EVENT, not a
/// standing STATE. Standing underground on the turn after a successful
/// delve reads identically to standing underground ten turns later on the
/// band tag alone; only the turn's own narration says which turn the
/// transition happened on. So there is still no typed alternative to
/// reading it — the same category of read `Driver` already does everywhere
/// else (the strip, the snapshot text itself), never a fabricated string.
///
/// **Guarded on the sim's own side, not just here.**
/// `windows/vessel/src/session.rs`'s
/// `delve_success_narration_matches_the_clients_own_literal` pins this
/// EXACT literal (via `str::starts_with`, the same method used below) as
/// `delve_at`'s own committed output — no shared constant is possible
/// (`windows/vessel` cannot depend on `clients/game`, and `clients/` sits
/// outside the workspace), so that test is the one thing that goes red if
/// this string ever drifts out from under this constant. If this literal
/// ever needs to change, update that test in the SAME commit.
const DELVE_SUCCESS_PREFIX: &str = "You worm down into the dark.";

/// The plate's content height Driver assumes before the first real
/// terminal size is known — [`hornvale_game_core::spread::content_height`]
/// at the 80x24 floor. **Not a standing assumption**: `main`'s `play` loop
/// calls [`Driver::resize`] with the live terminal height at startup and on
/// every resize event, which overwrites `self.plate_height` with the real
/// number — this constant only covers the brief window before that first
/// call (fix round 2: an earlier revision used this fixed floor value
/// unconditionally, which named the wrong vertex on any terminal taller than
/// 24 rows — see the module doc).
const FLOOR_PLATE_CONTENT_HEIGHT: u16 =
    hornvale_game_core::spread::content_height(hornvale_game_core::MIN_HEIGHT);

/// Why a [`Driver`] could not start.
///
/// Each variant wraps the real typed error from the layer that failed
/// (`hornvale-worldgen`'s [`BuildError`], `hornvale-vessel`'s
/// [`VesselError`] twice over) rather than flattening any of them to a
/// `String` — a caller that wants to match on the underlying cause still
/// can, and `Display` below is the only place formatting happens.
#[derive(Debug)]
pub enum DriverError {
    /// Genesis refused the seed.
    Genesis(BuildError),
    /// The world-scoped [`WorldContext`] could not be derived.
    Context(VesselError),
    /// The possession itself failed (no settlement, no species, ...).
    Possess(VesselError),
}

impl std::fmt::Display for DriverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DriverError::Genesis(e) => write!(f, "genesis refused: {e}"),
            DriverError::Context(e) => write!(f, "building the world context: {e}"),
            DriverError::Possess(e) => write!(f, "the possession failed: {e}"),
        }
    }
}

impl std::error::Error for DriverError {}

// **`PlateKey` is gone (The Quadrat, Task 5), and its narrowing lives on
// in [`crate::tiles::TileCache`].** It keyed the whole drawn plate on
// `(frame, window, w, h)`, which is why scrolling one column threw a plate
// away and redrew it — measured before this campaign, a 104x52 plate is
// 130 ms against 1.208 ms for one column. The cache unit is a CHART TILE
// now, so the drawn plate's size is not an input at all and a scroll
// re-renders only the tile column it uncovers.
//
// Everything that key deliberately did NOT carry, it still does not, and
// for the same reasons: **`Discovered`**, because a key carrying the
// discovery version is invalidated by every discovery — the whole pyramid,
// for one settlement (`CLIENT-tiles-need-the-overlay-split`, spec §3, and
// Task 4's layer split is the fix); and **the terrain, the `Geosphere`,
// the `NearestVertexIndex`, the settlement and cave rosters (all built once
// in `start`) and `colour_allowed`** (resolved from `NO_COLOR` by
// `plate::colour_allowed`, which cannot change under a running process),
// because they are fixed for the process. If any of those ever becomes
// mutable, it belongs in `TileKey`.

/// One live game: an owned [`World`], the [`WorldContext`] derived from it
/// exactly once (the campaign's own headline — release-and-repossess reuses
/// this rather than re-deriving it), and the live [`Session`] borrowing
/// both.
///
/// Self-referential by construction (`ctx` borrows `world`, `session`
/// borrows `ctx`), so `world` and `ctx` are heap-allocated and held as raw
/// pointers cast to `'static`, exactly as `clients/vessel/wasm/src/lib.rs`'s
/// `Possession` holds its `world: *mut World` — the existing precedent for
/// one owner holding a world a session borrows. `Drop` reclaims both by
/// hand; nothing outside this module ever sees the raw pointers or the
/// lifetimes they paper over.
pub struct Driver {
    /// The owned world genesis produced. Never `None` between `start`
    /// returning and `Drop` running.
    world: *mut World,
    /// The world-scoped derivation, paid exactly once.
    ctx: *mut WorldContext<'static>,
    /// The live possession, borrowing `ctx` (which borrows `world`).
    session: Session<'static>,
    /// The current turn's `vessel/session/v2` JSON — the only shape any
    /// world state takes once it leaves this struct. Refreshed by `start`
    /// and by every `handle`, so `snapshot()` never needs to touch the
    /// session again.
    cached: String,
    /// Whether the possession is currently on the WALK band, read off
    /// [`Self::cached`]'s own `spatial` tag by [`Self::refresh`].
    ///
    /// **Cached rather than re-parsed, and the reason is a cost.** Four
    /// things now depend on the band — whether a world plate is supplied at
    /// all, the active plate's size, whether a cursor move scrolls, and
    /// whether the strip has a resolver — and three of those are asked on
    /// every keypress. Parsing the snapshot JSON in each of them would put
    /// several `serde_json` passes on the cursor path to answer a question
    /// that changes only when a turn does. [`Self::refresh`] is the one
    /// choke point that already parses (for the completion scope), and it is
    /// the only place a turn's band can change.
    ///
    /// Opens `true`: the session begins on the walk band, and `start`'s own
    /// `refresh()` corrects it before any caller sees it.
    on_walk_band: bool,
    /// The live walk-band perception packet, cached alongside
    /// [`Self::on_walk_band`] and invalidated at exactly the same point
    /// (Task 10 round 2; The Gallery).
    ///
    /// **Why this one too, once `on_walk_band` already exists.** Round 1
    /// (The Quadrat's F11) retired the per-redraw `Snapshot::parse` this
    /// method's callers used to pay, but left the per-redraw
    /// `Session::purview(0)` call standing — and measurement showed THAT
    /// call, not the parse, is the dominant cost of a walk-band redraw
    /// (`walk_band_scene`'s own doc has the numbers). `purview` is a pure
    /// read (`&self`, no mutation) over session state — position,
    /// knowledge, eyes, day — that changes only when a turn does, by the
    /// same argument `on_walk_band`'s own doc already makes for the band
    /// question. So it is cached the same way: computed once per turn in
    /// [`Self::refresh`], read (never recomputed) by every call site that
    /// used to call `purview` itself.
    ///
    /// **The whole `Option`, not just the `Some` case.** Off the walk band
    /// this is `None` by construction of the fill step in `refresh` (the
    /// same band test `on_walk_band` already answers), so a reader here —
    /// [`Self::walk_band_scene`] — makes no decision the cache has not
    /// already made; it never re-asks "is this the walk band" the way a
    /// half-cached design (Some-only, with `None` re-derived per read)
    /// would have to.
    walk_scene: Option<hornvale_scene::SurroundsScene>,
    /// Which pane keys currently drive — the walk view's movement keys, the
    /// command line, or the map cursor. Startup is [`Focus::Walk`] (arrow
    /// keys move immediately); replaces The Portolan part I's `Mode {
    /// Normal, Look }`.
    focus: Focus,
    /// The map cursor's screen position, in plate grid cells. Held
    /// regardless of `focus` (so leaving and re-entering the map does not
    /// reset it); [`Driver::cursor`] reports it only in [`Focus::Map`].
    cursor: Cursor,
    /// The map strip's cached text — recomputed by [`Driver::enter_map`]
    /// (the map's one door) and on `CursorBy`, since fix round 1 the resolved name
    /// genuinely depends on the cursor's screen position (see the module
    /// doc for the chain); F2 measures the real per-call cost, which is why
    /// recomputing on every `CursorBy` rather than something more
    /// elaborate is fine.
    strip: Option<String>,
    /// F3's own scroll driver: a counter advanced once per
    /// [`Self::refresh_strip`] call — never a clock. `Instant`/`SystemTime`
    /// are banned workspace-wide (decision 0001) and `clients/game` runs no
    /// animation loop, so the strip's scroll position is driven by how many
    /// times the client has actually redrawn the strip (a cursor move, a
    /// zoom, a re-centre, a resize while the map is focused — every site
    /// that calls `refresh_strip`), exactly the same "driven by the same
    /// redraw the rest of the client uses" F3 asks for (design spec §5,
    /// §7 F3). See [`Self::strip_offset`] for how this becomes a column.
    redraw_count: u32,
    /// How many MARQUEE TICKS have elapsed — the strip's scroll driver
    /// since `fix/marquee-ticks-on-time`.
    ///
    /// **Why this replaced `redraw_count` for that job.** F3 asked for a
    /// scroll that introduces no clock, and `redraw_count` satisfied it
    /// literally: the offset advanced once per real redraw. In play that
    /// reads as a marquee that only moves when you press a key, which is
    /// not what a marquee is — Nathan, 2026-08-24: "the marquee text
    /// scrolls on the actions I take, not at a smooth, steady background
    /// rate."
    ///
    /// **The wall-clock ban does not reach this crate.** `clippy.toml`'s
    /// `disallowed-types` lives at the repo root; `clients/game` is its
    /// own workspace, outside it and outside determinism (decision 0055,
    /// and `clients/CLAUDE.md` says the workspace rules "do not bind this
    /// tree"). `bin/examples/repossess_cost.rs` already uses `Instant` and
    /// the client gate passes it — so the constraint F3 honoured was real
    /// for the SIM and never bound the client's own render loop.
    ///
    /// The clock itself stays in `main.rs`'s loop; this type only counts
    /// ticks, so every test drives the marquee by calling
    /// [`Driver::tick_marquee`] and no test depends on real time.
    marquee_ticks: u32,
    /// The world's landscape features, indexed by vertex, built once here at
    /// `start` and never rebuilt — the feature stack is immutable for the
    /// world's lifetime (`VertexFeatureIndex`'s own doc).
    index: VertexFeatureIndex,
    /// Nearest-vertex lookup over the same `Geosphere` `index` was built from,
    /// built once alongside it — turns the possessed agent's fine-grained
    /// [`hornvale_kernel::Facet`] position into the coarse `Vertex` the
    /// terrain feature index answers for.
    nearest: NearestVertexIndex,
    /// The world's `Geosphere`, held for `nearest`'s queries.
    geo: hornvale_kernel::Geosphere,
    /// The world's reconstructed tectonic terrain, held so the world plate
    /// can be redrawn on demand ([`Driver::world_plate`]) without
    /// re-deriving `GeneratedTerrain` from the ledger on every call — the
    /// same "build once, reuse for the session" idiom `index`/`nearest`
    /// already follow (see the module doc's `terrain_of` note at `start`).
    terrain: GeneratedTerrain,
    /// The oblique Mercator frame the world's own rotation regime chooses
    /// (spec §3.1): the geographic pole for a spinning world, the
    /// substellar point for a tidally locked one. Derived once at `start`
    /// from the committed `TIDALLY_LOCKED` fact and never recomputed — the
    /// central line does not move as the player does (spec §3.2).
    frame: Frame,
    /// The world plate's window: which mesh RUNG and which VIRTUAL-chart
    /// cell the window's own origin sits at (`plate::virtual_dims`'s own
    /// doc distinguishes the virtual chart from the drawn plate). Starts at
    /// `Window { depth: BAND_B_RUNG, origin_col: 0, origin_row: 0 }` — the
    /// FINEST rung, which is band B itself, the chart the walk band already
    /// draws — and moves one rung per `Action::Zoom` from there
    /// (`Driver::apply_zoom`). **Nothing resets it any more**: the ladder is
    /// continuous, so `depth` is the only state a zoom touches. Since The
    /// Quadrat the coarsest rung is NOT "the whole planet on screen": the
    /// chart is 363x362 tiles there and the plate is a subrect of it.
    window: Window,
    /// The world plate's TERRAIN LAYER, cached one CHART TILE at a time
    /// (The Quadrat, Task 5). The feature layer is composed on top per
    /// redraw and is never stored here — see
    /// [`crate::tiles::TileCache`]'s own doc, and the note above this
    /// struct for what the key carries and what it deliberately does not.
    ///
    /// **A cursor move inside the plate changes none of a tile's inputs,
    /// and neither does a resize to a subrect, and neither does a scroll
    /// except at the edge it uncovers.** The last of those three is the one
    /// the plate-shaped cache this replaces could not have: it keyed the
    /// drawn plate's own `(w, h)`, so one column of scroll cost a whole
    /// plate. **The magnitude that used to be cited here is retired, not
    /// merely stale**: "~265k `nearest()` calls at the design size,
    /// measured 88% of a redraw" was true of the 49-point vote The
    /// Quadrat's Task 3 removed, and the search it named is now made a few
    /// dozen times per plate rather than a few hundred thousand.
    tiles: crate::tiles::TileCache,
    /// The world's seed, needed to draw a feature's name.
    seed: Seed,
    /// Every terrain vertex the world's ledger commits at least one
    /// settlement nearest to, valued by that settlement's own committed
    /// population (Task 7 widens this from a bare `BTreeSet<Vertex>`,
    /// The Portolan part II Task 5's original shape) — built ONCE here at
    /// `start`, the same "derive once, never per-turn" discipline
    /// `index`/`nearest` already follow. Not itself a discovery record:
    /// this is the plate's own point-site ROSTER (where each site stands
    /// and, for a settlement, how big it is — ground truth, always known to
    /// the client that draws the map), gated by [`Self::discovered`] at draw
    /// time, never here. The population is what
    /// [`plate::draw_feature_layer`]'s own viewport-relative major/minor
    /// ranking reads.
    ///
    /// **One roster over all three `SiteKind`s since The Prospect's Task 8,
    /// where it was two (`settlements`, `caves`) and the third kind had
    /// none.** A settlement stands at its own committed coordinate; a cave
    /// or an exotic site stands on the facet
    /// `hornvale_worldgen::site_facet_for` places it on
    /// ([`plate::MapSite::placed`]) — which is also what
    /// [`Self::update_discovery`] matches a possession's own room against,
    /// so the roster that DRAWS a site is the roster that DISCOVERS it.
    sites: Vec<plate::MapSite>,
    /// Every volcano's own anchor vertex (Task 7), read once from the same
    /// `features` list `index` is built from. Discovery is the EXISTING
    /// extent-feature mechanism (`Self::index`/`Self::discovered`'s own
    /// `FeatureId::Extent`); this roster only supplies where to draw an
    /// already-discovered marker.
    volcanoes: BTreeSet<Vertex>,
    /// Every waterfall vertex `GeneratedTerrain::waterfalls()` reports
    /// (Task 7), scanned once at `start`. Drawn UNCONDITIONALLY by
    /// [`plate::draw_feature_layer`] — see that function's own doc for why
    /// this roster carries no discovery gate, unlike `sites`/`volcanoes`.
    waterfalls: Vec<Vertex>,
    /// Every walk-band room the possession has stood in this session
    /// (spec Amendment 1 §A4a: "where have I been"). Never consulted by
    /// [`Self::discovered`] and never consults it — see `discovery`'s
    /// module doc for why that absence is load-bearing.
    visited: Visited,
    /// Every feature the possession has DISCOVERED this session (spec
    /// Amendment 1 §A4b: "what do I know is there"). Updated only by
    /// [`Self::update_discovery`], and only by encounter — never by mere
    /// co-location with [`Self::visited`].
    discovered: Discovered,
    /// The plate's REAL content height, in grid rows — synced from the live
    /// terminal by [`Driver::resize`] (fix round 2: an earlier revision
    /// used a floor-sized constant unconditionally here, which named the
    /// wrong vertex — see the module doc). Starts at
    /// [`FLOOR_PLATE_CONTENT_HEIGHT`] before the first real size is known;
    /// `main`'s `play` loop calls `resize` before the first draw, so a
    /// live session never resolves against the stale default.
    plate_height: u16,
    /// The live terminal's own width, synced by [`Driver::resize`] — needed
    /// (alongside `term_h`) to derive the world plate's real width via
    /// [`hornvale_game_core::spread::world_plate_width`], which takes the
    /// raw terminal size, not the already-reduced `plate_height`. Starts at
    /// [`hornvale_game_core::MIN_WIDTH`], the same floor-before-first-resize
    /// convention `plate_height` follows.
    term_w: u16,
    /// The live terminal's own height, synced by [`Driver::resize`] — see
    /// `term_w`'s doc. Starts at [`hornvale_game_core::MIN_HEIGHT`].
    term_h: u16,
    /// The possessed agent's species, phonology and naming morphology,
    /// resolved once at `start` (an agent's species does not change during
    /// a session) — the same triple [`resolve_at`] needs on every call.
    namer: (String, Phonology, MorphOptions),
    /// The command line's editable buffer — the one reversible thing this
    /// struct owns (The Stylus, Task 3).
    line: Line,
    /// Recalled command lines, walked by `Action::HistoryPrev`/
    /// `HistoryNext`.
    history: History,
    /// The most recently submitted line, echoed for the record so the
    /// display can read ask-then-answer (spec §6) — `None` before the first
    /// submission this session. Not reset on later turns other than being
    /// overwritten by the next submission: it names "what was asked most
    /// recently", not "what was asked this exact turn".
    echo: Option<String>,
    /// The completion vocabulary's first scope: the current turn's
    /// examinable catalog, refreshed from every parsed snapshot (see
    /// [`Driver::refresh`]).
    ///
    /// **The day a second scope landed (The Newel, Task 4).** This field
    /// used to be held directly, with a doc note explaining that v1
    /// registered exactly one scope so a [`hornvale_game_core::Lexicon`]
    /// fold had nothing to fold. [`Self::chart_scope`] is that second
    /// scope, and [`Self::scope`] is the `Lexicon` the two fold into —
    /// this field now holds the mutable half `Lexicon` itself cannot (a
    /// `Box<dyn CandidateSource>` cannot be updated in place; only
    /// re-folded).
    current_turn_scope: hornvale_game_core::CurrentTurnNouns,
    /// The completion vocabulary's second scope (The Newel, Task 4):
    /// settlement/cave marks drawn on the walk-band chart, gated by
    /// discovery — see [`hornvale_game_core::ChartMarks`]'s own doc for
    /// why the gate is supplied here rather than inside `core`.
    chart_scope: hornvale_game_core::ChartMarks,
    /// The completion vocabulary itself: an ordered, first-wins fold of
    /// [`Self::current_turn_scope`] then [`Self::chart_scope`], rebuilt at
    /// the end of every [`Driver::refresh`] from those two fields. First
    /// wins so a noun in the room you are standing in beats a distant
    /// chart mark of the same name (seed 42 has 390 settlements under 240
    /// distinct names, so a collision is real, not hypothetical).
    scope: hornvale_game_core::Lexicon,
    /// How an ambiguous completion presents (spec §4.2). [`TabStyle::Cycle`]
    /// is implemented and unit-tested but unbound — no preference mechanism
    /// exists yet, so every session runs [`TabStyle::Hint`].
    tab_style: TabStyle,
    /// The pending ambiguity under [`TabStyle::Hint`] — the stem the buffer
    /// was extended to and the full match list it came from, for the strip
    /// to present. Cleared by any edit or submission.
    hint: Option<Hint>,
    /// The pending rotation under [`TabStyle::Cycle`] — unreachable while
    /// `tab_style` is [`TabStyle::Hint`], but implemented and tested so the
    /// preference mechanism only has to flip a field. Cleared by any edit
    /// or submission.
    cycle: Option<CycleState>,
    /// The bare-`x` noun prompt (spec §4.4): `Some` while the line
    /// temporarily reads `examine …`, holding the buffer it replaced. The
    /// prompt is a MODE, not a picker — the client still sends text
    /// unconditionally, and the sim answers unknown nouns in its own prose.
    noun_prompt: Option<NounPrompt>,
    /// The world's derived [`hornvale_astronomy::Calendar`], when its sky can
    /// be reconstructed. Read once here via [`hornvale_worldgen::sky_of`],
    /// the same "derive once,
    /// never per-frame" discipline `terrain`/`geo`/`nearest` already
    /// follow above: `sky_of` regenerates the sky deterministically from
    /// the world's own committed seed and pins (the same reconstruction
    /// idiom `terrain_of` uses for `Self::terrain`), so paying it once at
    /// `start` and reading a plain field from then on is strictly cheaper
    /// than re-deriving it wherever a season is needed.
    // Task 6 IS the reader now: `Driver::plate_light` folds this field
    // through `season_bucket_for` and `plate_illuminant` on every plate
    // draw, so the `#[allow(dead_code)]` this carried is gone.
    calendar: Option<hornvale_astronomy::Calendar>,
    /// The plate's `(FacetId, season)` reflectance cache (The Wash, Task 4):
    /// the store [`plate::terrain_at_tile`] will consult and fill once a
    /// real [`hornvale_locale::LocaleContext`] is threaded through it.
    /// Reflectance is seasonal-rate (the rate spine's own invariant, Task
    /// 1), so the key carries the season a tile was drawn in — a season
    /// change mints a new entry rather than serving a stale one, which is
    /// what makes it safe to sit above the plate's own never-invalidated
    /// TERRAIN layer cache ([`Self::tiles`]'s own doc).
    // Task 6 IS the reader now (it was `#[allow(dead_code)]` until this
    // task): `Driver::world_plate` and `Driver::world_plate_for_redraw`
    // both hand it to `plate::Spectral`, which is what makes the ~1,920
    // per-frame reflectance consults cost one locale call per
    // `(facet, season)` instead of one per drawn tile.
    //
    // `plate::ReflectanceCache`, not a bare `ComponentStore`: the wrapper
    // carries the hit/miss counters that make the cache's hit path
    // observable without timing anything (`Instant` is banned) — see its
    // own doc for the assertion that could not fail before it existed.
    //
    // `world_view_tile` still does NOT thread it (fix round 1, Fix 1;
    // widened at Task 5): that call site never populates or reads a hit
    // here, so making three read-only queries advertise mutation to reach
    // it would buy nothing.
    reflectance_cache: plate::ReflectanceCache,
}

/// What [`Driver::noun_prompt`] saves for `Esc` to restore.
#[derive(Debug, Clone, PartialEq, Eq)]
struct NounPrompt {
    /// The buffer as the player left it before submitting bare `x`.
    saved_buffer: String,
    /// Its caret position.
    saved_caret: usize,
}

/// The prefix the noun prompt dispatches — the line reads exactly this
/// plus whatever noun the player has typed.
const EXAMINE_PROMPT_PREFIX: &str = "examine ";

/// How an ambiguous completion presents (spec §4.2). Cycle is implemented
/// and unit-tested but unbound — no preference mechanism exists yet.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TabStyle {
    /// Extend to the shared stem and show the alternatives once.
    Hint,
    /// Each successive press rotates through the matches.
    Cycle,
}

/// One pending ambiguity under [`TabStyle::Hint`]: the stem the buffer now
/// carries and every name that shares it, in candidate order.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Hint {
    /// The longest common prefix of all matches — what the buffer holds.
    stem: String,
    /// Every matching name, input order preserved.
    matches: Vec<String>,
}

/// One pending rotation under [`TabStyle::Cycle`]: where the completed word
/// begins (in char offsets, matching [`Line::caret`]'s units) and which
/// match is currently filled.
#[derive(Debug, Clone, PartialEq, Eq)]
struct CycleState {
    /// Char offset of the word being rotated.
    start: usize,
    /// Every matching name, input order preserved.
    matches: Vec<String>,
    /// Index into `matches` of the currently-filled name.
    index: usize,
}

/// What a Complete keypress decides, before any mutation — factored out of
/// [`Driver::apply`] so the decision is unit-testable without minting a
/// world (a full [`Driver`] costs a genesis; this function does not).
#[derive(Debug, Clone, PartialEq, Eq)]
enum CompletionDecision {
    /// No token at the caret, or nothing matched: change nothing.
    Noop,
    /// Exactly one candidate matched: fill it whole.
    Fill(String),
    /// Several matched: extend to the shared stem and offer the rest.
    Ambiguous {
        /// The longest common prefix of ALL matches.
        stem: String,
        /// Every matching name, input order preserved.
        matches: Vec<String>,
    },
}

/// Char offset where the whitespace-delimited word ending at `caret`
/// begins, or `None` when no token ends there.
fn word_start_at_caret(text: &str, caret: usize) -> Option<usize> {
    let prefix: Vec<char> = text.chars().take(caret).collect();
    if prefix.last().is_none_or(|c| c.is_whitespace()) {
        return None;
    }
    Some(
        prefix
            .iter()
            .rposition(|c| c.is_whitespace())
            .map_or(0, |i| i + 1),
    )
}

/// Decide what a Complete keypress does for the token ending at `caret` in
/// `text`, against `candidates`. Pure.
fn completion_decision(
    text: &str,
    caret: usize,
    candidates: &[hornvale_game_core::Candidate],
) -> CompletionDecision {
    let Some(start) = word_start_at_caret(text, caret) else {
        return CompletionDecision::Noop;
    };
    let chars: Vec<char> = text.chars().collect();
    let token: String = chars[start..caret].iter().collect();
    if token.is_empty() {
        return CompletionDecision::Noop;
    }
    match hornvale_game_core::complete(&token, candidates) {
        hornvale_game_core::Completion::None => CompletionDecision::Noop,
        hornvale_game_core::Completion::Unique(name) => {
            // Filling the token with itself is a documented no-op (the
            // engine's own note on `Unique`).
            if name == token {
                CompletionDecision::Noop
            } else {
                CompletionDecision::Fill(name)
            }
        }
        hornvale_game_core::Completion::Prefix { stem, matches } => {
            CompletionDecision::Ambiguous { stem, matches }
        }
    }
}

/// Append the sight-disclosure caption to a resolved strip text — the
/// honesty line that says whose eyes the coloured chart was drawn through.
/// The colour probe is the same one the drawing path uses
/// ([`hornvale_game_core::Ink::from_wire`]): a non-empty `NO_COLOR` means
/// the reader declined colour, and the caption goes with it — the picture
/// and its honesty line are one channel, suppressed together. Pure apart
/// from that one environment read, so tests can drive it hermetically.
fn colour_allowed() -> bool {
    hornvale_game_core::Ink::from_wire(Some([0, 0, 0])) != hornvale_game_core::Ink::Plain
}

/// Flatten a walk-band perception packet into what
/// [`plate::draw_perception_layer`] places and ranks — the boundary that
/// keeps `plate.rs` free of the wire's field names and state strings, the
/// same way [`plate::draw_feature_layer`] takes vertex rosters rather than
/// the ledger they were read out of.
///
/// The dominant mark is the numerically SMALLEST `salience`
/// (`hornvale_scene::Mark::salience`: a RANK, lower is more salient — never
/// a magnitude), and `Iterator::min_by_key` keeps the FIRST minimum, which
/// is the producer's own ascending-`room` document order. That is clause 3
/// of the collision rule, obtained rather than re-implemented.
fn perceived_facets(scene: &hornvale_scene::SurroundsScene) -> Vec<plate::Perceived<'_>> {
    scene
        .cells // lexicon: `SurroundsCell` is the wire's frozen name for a FACET, an area
        .iter()
        .map(|seen| plate::Perceived {
            room: seen.room,
            here: seen.state == "here",
            mark: seen
                .marks
                .iter()
                .min_by_key(|m| m.salience)
                .map(|m| (m.salience, m.kind.as_str())),
        })
        .collect()
}

fn caption(base: String, sight: Option<&hornvale_game_core::schema::Sight>) -> String {
    let coloured = colour_allowed();
    match (sight, coloured) {
        (Some(sight), true) => {
            let mut text = base;
            text.push_str(" — ");
            text.push_str(&hornvale_game_core::chart::disclosure(sight));
            text
        }
        _ => base,
    }
}

/// The `(FacetId, season)` cache's season component for `at`, resolved
/// against `calendar` (The Wash, Task 4) — the glue between
/// [`Driver::calendar`] and [`plate::season_bucket`], and the ONE place
/// that glue is written: [`plate::terrain_at_tile`]'s own doc names this
/// function as the contract every caller of `season` must route through
/// rather than re-deriving. `pub` (fix round 1) so `wash.rs`'s test fixture
/// can call the real derivation instead of hardcoding a bucket that may not
/// correspond to its own `at`.
///
/// **Two `None`s fold to the same bucket 0.** The optional `calendar` is
/// absent when sky reconstruction failed. A present calendar can still
/// report `Calendar::season_phase(..) == None` — its own
/// documented contract, for zero obliquity AND zero eccentricity — which is
/// the same "no seasons to render" case reached a different way. Neither is
/// a silent `unwrap_or(0)` standing in for a default this comment is the
/// only record of: both branches are named here because "no calendar" and
/// "a calendar with nothing to report" are the two ways a world can
/// honestly have no seasons.
pub fn season_bucket_for(
    calendar: Option<&hornvale_astronomy::Calendar>,
    at: hornvale_kernel::WorldTime,
) -> u32 {
    let Some(calendar) = calendar else {
        return 0;
    };
    let Ok(instant) = hornvale_astronomy::StdInstant::new(at.as_std_days()) else {
        return 0;
    };
    calendar
        .season_phase(instant)
        .map(plate::season_bucket)
        .unwrap_or(0)
}

/// A flat, colourless illuminant — every band at unit weight (The Wash,
/// Task 5's plate-scale echo of [`hornvale_vessel`]'s own room-scale
/// `flat_illuminant`, `windows/vessel/src/eyes.rs:58` — private to that
/// crate, so this is a deliberate second copy of the same one-line
/// definition rather than a shared symbol). The fallback for a world with
/// no solar geometry to place a real sun by, used by both
/// [`plate_illuminant`]'s `None` cases.
pub fn flat_illuminant() -> hornvale_kernel::color::Illuminant {
    hornvale_kernel::color::Illuminant::new([1.0; hornvale_kernel::color::BANDS])
        .expect("a unit illuminant is finite and non-negative")
}

/// The illuminant at a given sun elevation: [`hornvale_astronomy::daylight`]
/// for `world`'s own star, reddened by [`hornvale_astronomy::at_elevation`]
/// for `sun_elevation_deg`.
///
/// Separated from [`plate_illuminant`] so a test can drive the elevation
/// directly (The Wash, Task 5's own brief) rather than needing to find a
/// world time and latitude that happen to produce one.
///
/// **The star is `generate_star(world.seed.derive(streams::ROOT))`, never
/// `Sky::system()`.** The generated star is a pure function of the world's
/// own seed, defined for every world. By the time a caller has a
/// `sun_elevation_deg` to pass here, calendar placement is already resolved.
pub fn plate_illuminant_at(
    world: &World,
    sun_elevation_deg: f64,
) -> hornvale_kernel::color::Illuminant {
    let star =
        hornvale_astronomy::generate_star(world.seed.derive(hornvale_astronomy::streams::ROOT));
    let base = hornvale_astronomy::daylight(&star);
    hornvale_astronomy::at_elevation(&base, sun_elevation_deg)
}

/// The illuminant for one whole draw, anchored to the OBSERVER rather than
/// the tile (spec §4.5, corrected before this task was dispatched). An
/// earlier draft of the spec claimed the illuminant is "diurnal and uniform
/// across the plate" — false at coarse rungs, where one plate spans every
/// latitude and longitude a globe has, so a single illuminant computed
/// per-tile would light the night side as noon. The map is instead lit as
/// it is *where the reader stands*: one call, at the observer's own
/// `latitude_deg` and `day`, applied uniformly across the whole plate. That
/// is a stated cartographic convention, not an approximation pretending to
/// be a fact — and the cost is named rather than hidden: no terminator
/// sweeps the map, so at a coarse rung the far side of the world carries
/// the reader's own sunlight.
///
/// Follows the exact composition [`hornvale_vessel`]'s own room-scale
/// `eyes::daylight_at` uses (`windows/vessel/src/eyes.rs:81-97`): resolve
/// the sun altitude from `calendar` at `(day, latitude_deg)`, then hand it
/// to [`plate_illuminant_at`]. Meant to be called **once per draw**, above
/// the tile loop, and the result threaded through by reference — nothing
/// about its inputs (`world`, `calendar`, `day`, `latitude_deg`) varies per
/// tile, so there is no tile-shaped parameter for a caller to loop over.
///
/// **Two `None` cases share the same fallback — see
/// [`season_bucket_for`]'s own doc for the identical fold.** `calendar`
/// can be absent when sky reconstruction failed; a present calendar can
/// still report [`hornvale_astronomy::Calendar::solar_altitude_at`] as
/// `None` (zero obliquity AND zero eccentricity — that method's own
/// documented contract). Both resolve to [`flat_illuminant`] rather than
/// guessing a sun that cannot be honestly placed — the same fallback
/// [`hornvale_vessel`]'s `eyes::flat_illuminant` uses, for the same reason.
/// type-audit: bare-ok(diagnostic-value: latitude_deg)
pub fn plate_illuminant(
    world: &World,
    calendar: Option<&hornvale_astronomy::Calendar>,
    day: hornvale_kernel::WorldTime,
    latitude_deg: f64,
) -> hornvale_kernel::color::Illuminant {
    let altitude = calendar.and_then(|cal| {
        hornvale_astronomy::StdInstant::new(day.as_std_days())
            .ok()
            .and_then(|t| cal.solar_altitude_at(t, latitude_deg))
    });
    match altitude {
        Some(sun_elevation_deg) => plate_illuminant_at(world, sun_elevation_deg),
        None => flat_illuminant(),
    }
}

impl Driver {
    /// Build a fresh world for `seed` (default sky/terrain/settlement pins,
    /// generated sky — the same defaults `clients/vessel/wasm`'s `hv_start`
    /// uses), derive its [`WorldContext`] once, and start a possession of
    /// `target`.
    ///
    /// Genesis and session start are two separable halves, and
    /// [`Driver::start_from_world`] is the second one — see its doc for who
    /// needs the seam. This function is the whole thing, unchanged, for every
    /// caller that does not.
    pub fn start(seed: u64, target: PossessTarget) -> Result<Driver, DriverError> {
        let world = build_world(
            Seed(seed),
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .map_err(DriverError::Genesis)?;
        Driver::start_from_world(world, target)
    }

    /// Everything [`Driver::start`] does EXCEPT building the world: derive the
    /// [`WorldContext`], start a possession of `target`, and build the
    /// session's indices.
    ///
    /// **The seam exists because genesis moved to a worker thread** (The
    /// Overture, ruling R5). The startup frame needs the build to run somewhere
    /// it can draw around, and `build_world_observed` returns a plain [`World`],
    /// which is `Send`; a `Driver` is not (it holds `*mut World`) and a
    /// `WorldContext` cannot be either (it stores `Box<dyn PhenomenaSource>`,
    /// and that trait declares no `Send` bound). So the worker builds the world,
    /// hands it back, and this half runs on the thread that owns the terminal.
    /// Task 8's world cache needs the same seam for the same shape of reason: it
    /// has a world already and only wants the second half.
    ///
    /// Takes the world BY VALUE and keeps it — it is never rebuilt, re-read or
    /// re-derived, so a caller that has just paid ~2.2 s for one pays nothing
    /// again here.
    // Named construction site (decision 0092): `terrain_of` re-derives the
    // tectonic globe once here, at world load, to build the Portolan's
    // terrain-feature index — never per-turn (see `VertexFeatureIndex`'s doc).
    #[allow(clippy::disallowed_methods)]
    pub fn start_from_world(world: World, target: PossessTarget) -> Result<Driver, DriverError> {
        let world = Box::into_raw(Box::new(world));
        // SAFETY: `world` is a fresh heap allocation this function owns.
        // Nothing else can alias it yet, and it outlives `ctx`/`session`
        // below (both reclaimed, in order, by `Drop for Driver`) before it
        // is itself reclaimed.
        let world_ref: &'static World = unsafe { &*world };

        let ctx = match WorldContext::build(world_ref) {
            Ok(ctx) => ctx,
            Err(e) => {
                // SAFETY: nothing has borrowed `world` yet on this path.
                drop(unsafe { Box::from_raw(world) });
                return Err(DriverError::Context(e));
            }
        };
        let ctx = Box::into_raw(Box::new(ctx));
        // SAFETY: same argument as `world_ref` — a fresh allocation this
        // function owns, outliving the `session` that borrows it.
        let ctx_ref: &'static WorldContext<'static> = unsafe { &*ctx };

        let opts = PossessOpts {
            target,
            ..PossessOpts::default()
        };
        let session = match Session::start_in(ctx_ref, &opts) {
            Ok((session, _opening)) => session,
            Err(e) => {
                // SAFETY: the failed start returned no session; nothing
                // borrows `ctx`/`world` on this path.
                drop(unsafe { Box::from_raw(ctx) });
                drop(unsafe { Box::from_raw(world) });
                return Err(DriverError::Possess(e));
            }
        };
        // The opening text is not read here: `SessionSnapshot::narration`
        // carries it byte-for-byte (`Narration::prose`, "carried verbatim"
        // per its own doc), so the snapshot read below already has it.

        // The Portolan: the terrain-feature index, built once here (never
        // per-turn — see the module doc and `VertexFeatureIndex`'s own).
        // `terrain_of` re-derives `GeneratedTerrain` deterministically from
        // the world's committed seed and pin facts — the same
        // reconstruction idiom `sky_of`/the CLI's `map` command use, not a
        // second, drifting genesis.
        let terrain = terrain_of(world_ref).map_err(DriverError::Genesis)?;
        let geo = terrain.geosphere().clone();
        let features = gazetteer_features(world_ref.seed, &geo, &terrain);
        let index = VertexFeatureIndex::build(&features);
        let nearest = NearestVertexIndex::new(&geo);

        // The Wash, Task 4: the world's own calendar, read once here (the
        // same "derive once, never per-frame" idiom `terrain`/`geo`/
        // `nearest` above already follow) via the same `sky_of`
        // reconstruction idiom `terrain_of` uses for `terrain` — never a
        // second, drifting genesis. A build failure here is folded to
        // `None` rather than propagated: a world whose sky cannot be
        // reconstructed still has a terrain and a session (both already
        // built above), and a plate that cannot resolve a season is
        // exactly the "no calendar" case below, not a harder failure.
        //
        // A calendar whose own `season_phase` reports `None`, for zero
        // obliquity and zero eccentricity, has no season to render.
        // `season_bucket_for` below folds it to bucket 0 rather than
        // panicking or guessing.
        let calendar = hornvale_worldgen::sky_of(world_ref)
            .ok()
            .map(|sky| sky.calendar().clone());

        // The Portolan part II, Task 5: the point-site roster — every
        // terrain vertex a live settlement's own committed `(latitude,
        // longitude)` resolves nearest to. Built once here, the same
        // ledger read `h4_locked_worlds_settlements_are_never_in_the_clamp`
        // already exercises dev-only; this is the shipped-path use of it.
        // Ground truth, never gated — [`plate::draw_with`] is where
        // `discovered` decides whether a member of this set is ever drawn.
        //
        // **Task 7 widens the roster from a `BTreeSet<Vertex>` to a
        // `BTreeMap<Vertex, u64>`**, keyed the same way, valued by the
        // settlement's own committed `hornvale_settlement::POPULATION` —
        // the size the world map's O/o major/minor split ranks by
        // (`plate::draw_feature_layer`'s own doc).
        //
        // **`plate::settlements_of` (fix round 1, R10, The Overture Task 5)
        // widened the same way, at the same time** — this used to be
        // inlined here separately from `atlas`'s own copy; now both callers
        // of [`plate::draw_with`] share the one derivation, population
        // included, so a future change to the skip-on-missing or
        // tie-breaking rule cannot land on one caller and not the other.
        //
        // THE PROSPECT, TASK 8: one roster over all three site kinds. The
        // cave half used to be an inline `(0..geo.vertex_count()).filter(|c|
        // terrain.cave_at(c).is_some())` scan here — a second copy of
        // `GeneratedTerrain::cave_site_vertices`, which is what `Session`
        // itself reads and therefore what decides enterability. Two copies of
        // "where is there a cave" is the exact defect shape Task 4's own
        // correction deleted one level down (it removed an invented second
        // predicate); reintroducing it at the client would have let the map
        // and the walker disagree about which caves exist.
        let caves: Vec<Vertex> = terrain.cave_site_vertices();
        // The exotic roster: the vertices `hornvale_locale`'s strangeness
        // budget warrants a site at. A cheap read over a budget the
        // `LocaleContext` already built at `start` — the same call
        // `Session::brief_here` makes per turn, so this is not a second
        // derivation, and the vertices are all this needs (the facet comes
        // from `site_facet_for`, below, inside `sites_of`).
        let exotics: Vec<Vertex> = session
            .context()
            .strange_sites()
            .iter()
            .map(|site| Vertex(site.vertex))
            .collect();
        // The world's own walk band — the resolution a placed site's address
        // is minted at, so it must be the world's value and never this
        // module's `plate::BAND_B_RUNG` constant, which restates the
        // canonical-globe answer only.
        let walk = hornvale_vessel::walk_depth(session.context());
        let sites = plate::sites_of(world_ref, &geo, &nearest, &caves, &exotics, walk);

        // The Legend, Task 7: the volcano roster — every volcano's ANCHOR
        // vertex (`hornvale_terrain::landscape::Feature::anchor`, the
        // canonical vertex `feature_salt`/naming already key on), read off
        // the SAME `features` this constructor already built for the
        // gazetteer/cursor system two lines up. Discovery for these is not
        // a new mechanism: `Driver::update_discovery`'s existing
        // `for id in self.index.at(vertex)` loop already records
        // `FeatureId::Extent` the instant a possession walks onto ANY
        // vertex of a volcano's extent (an edifice can span many vertices,
        // §A4b's "the ground and the feature are the same object" for an
        // extent class) — this roster only supplies WHERE to draw the
        // already-discovered marker, at its one anchor.
        let volcanoes: BTreeSet<Vertex> = features
            .iter()
            .filter(|f| f.id.class == FeatureClass::Volcano)
            .map(|f| f.anchor)
            .collect();

        // The waterfall roster (Task 7): bare vertices `GeneratedTerrain`
        // already computed at genesis (`waterfalls()`, sorted ascending
        // `Vertex` by its own doc), read once here for the same reason the
        // cave roster is — `plate::draw_feature_layer` projects sites, it
        // does not scan for them. It carries no `FeatureClass` (the
        // landscape feature system does not individuate it, and this
        // task's own interface note forbids minting one to give it one),
        // so it draws UNCONDITIONALLY as ground truth — see that
        // function's own doc for why that split from the volcano roster
        // above is deliberate. (A river-delta roster stood here too,
        // through fix round 1; the feature was removed outright — see
        // `plate::WATERFALL_GLYPH`'s own doc for why.)
        let waterfalls: Vec<Vertex> = terrain.waterfalls().to_vec();

        // The Portolan part II: the projection's central line is derived
        // from the world's own physics (spec §3.1), not assumed —
        // `TIDALLY_LOCKED` is committed only when the world's rotation
        // regime is `Rotation::Locked` (`domains/astronomy/src/facts.rs`'s
        // `genesis`), so its mere presence in the ledger, on any subject,
        // answers the question `frame_for` needs.
        let locked = world_ref
            .ledger
            .find(hornvale_astronomy::facts::TIDALLY_LOCKED)
            .next()
            .is_some();
        let frame = mercator::frame_for(locked);
        // The session opens on the walk band, and band B IS the ladder's
        // finest rung (The Quadrat, Task 2) — so the opening rung is
        // `BAND_B_RUNG`, not a separate off-ladder state. Band B DRAWS the
        // Mercator as of Task 6, so `Focus::Map` here shows the raster with
        // the perception overlay over it; the origin below is the chart's
        // corner and `enter_map` centres it on the observer before anything
        // is drawn (`centre_band_b_on_the_observer`).
        let window = Window {
            depth: BAND_B_RUNG,
            origin_col: 0,
            origin_row: 0,
        };

        // The possessed agent's species, phonology and morphology — needed
        // to draw a feature's name (`resolve_at`), resolved once since the
        // agent's species is fixed for the session.
        let species = session.driven_body().species.clone();
        let wc = WorldComponents::assemble().map_err(DriverError::Genesis)?;
        let ph = language_of_in(world_ref, &wc, &species);
        let mind = wc
            .psyche
            .get_by_label(&species)
            .expect("a minted agent's species has a mind vector (referential integrity)");
        let society = wc
            .society
            .get_by_label(&species)
            .expect("a minted agent's species has a society vector (referential integrity)");
        let morph = morph_options(mind, society);

        let mut driver = Driver {
            world,
            ctx,
            session,
            cached: String::new(),
            focus: Focus::Walk,
            cursor: Cursor {
                x: hornvale_game_core::spread::PLATE_WIDTH / 2,
                y: FLOOR_PLATE_CONTENT_HEIGHT / 2,
            },
            strip: None,
            redraw_count: 0,
            marquee_ticks: 0,
            index,
            nearest,
            geo,
            terrain,
            frame,
            window,
            tiles: crate::tiles::TileCache::default(),
            seed: world_ref.seed,
            sites,
            volcanoes,
            waterfalls,
            visited: Visited::default(),
            discovered: Discovered::default(),
            plate_height: FLOOR_PLATE_CONTENT_HEIGHT,
            term_w: hornvale_game_core::MIN_WIDTH,
            term_h: hornvale_game_core::MIN_HEIGHT,
            namer: (species, ph, morph),
            line: Line::new(),
            history: History::new(),
            echo: None,
            current_turn_scope: CurrentTurnNouns::default(),
            chart_scope: ChartMarks::default(),
            scope: Lexicon::new(vec![]),
            tab_style: TabStyle::Hint,
            hint: None,
            cycle: None,
            noun_prompt: None,
            on_walk_band: true,
            walk_scene: None,
            calendar,
            reflectance_cache: plate::ReflectanceCache::new(),
        };
        driver.refresh();
        Ok(driver)
    }

    /// Sync the plate's REAL content height, and the raw terminal size,
    /// from the live terminal's own `w`x`h` (`main`'s `play` loop calls
    /// this at startup and on every resize event, before the first/next
    /// draw). Uses [`hornvale_game_core::spread::content_height`] — the
    /// SAME computation `compose` itself applies to derive the plate it
    /// actually draws into, not a second copy of it (fix round 2's own
    /// lesson, applied to itself: two independent "what is the plate's
    /// content height" computations is exactly the shape that produced the
    /// bug this method fixes). `term_w`/`term_h` are stored raw (not
    /// reduced) because the world plate's width
    /// ([`hornvale_game_core::spread::world_plate_width`]) needs the raw
    /// terminal size, not the already-content-reduced height.
    ///
    /// Re-clamps the cursor into the new bounds: a terminal shrinking after
    /// the cursor moved into rows a taller plate offered must not leave it
    /// parked outside the plate that is about to be drawn. With the world
    /// view active, also re-clamps the window (its virtual chart's own
    /// size depends on the plate's width, which just changed). With the map
    /// focused, also re-resolves the strip: the cursor's SCREEN position is
    /// unchanged by a resize, but which real vertex that screen position
    /// names can change (the plate's centre moves), so the displayed text
    /// must not go on describing whatever the old size resolved.
    pub fn resize(&mut self, w: u16, h: u16) {
        self.term_w = w;
        self.term_h = h;
        self.plate_height = hornvale_game_core::spread::content_height(h);
        // UNCONDITIONAL since band B joined the raster (Task 6): every rung
        // has a window whose origin depends on the plate's size, so there is
        // no rung left for which this is meaningless.
        self.reclamp_window();
        self.centre_band_b_on_the_observer();
        self.move_cursor(0, 0);
        if self.focus == Focus::Map {
            self.refresh_strip();
        }
    }

    /// Which of the three modes keys currently drive — the walk view, the
    /// command line, or the map cursor. `main`'s input loop reads this
    /// every keypress to choose how [`crate::input::action_for`] routes the
    /// key.
    pub fn focus(&self) -> Focus {
        self.focus
    }

    /// Move focus to the other pane — Walk→Cli, Cli→Walk, Map→Walk (Esc
    /// always leaves the map for the walk view).
    ///
    /// This is Esc's transition and Esc's ONLY. It can never ARRIVE at the
    /// map: every arm below lands on Cli or Walk, so the map has exactly
    /// one door ([`Self::enter_map`], from a bare `map` submission) and
    /// this function is always a departure from it. The strip is therefore
    /// unconditionally cleared here — an earlier revision carried a
    /// `focus == Focus::Map` branch that refreshed it, which was dead the
    /// moment `map` entry moved to its own function, and read as though Esc
    /// could still open the map.
    pub fn toggle_focus(&mut self) {
        self.leave_the_map();
        self.focus = match self.focus {
            Focus::Cli => Focus::Walk,
            Focus::Map => Focus::Walk,
            Focus::Walk => Focus::Cli,
        };
    }

    /// End a map consultation: clear the strip, and — if the map was
    /// actually focused — return the ladder to the walker's own band and
    /// centre it on them.
    ///
    /// **The defect this closes (The Quadrat, Task 9, fix round 1).** Type
    /// `map`, press `-`, press `Esc`: six keystrokes from the opening
    /// screen. Before Task 9 that state drew [`crate::chart::draw`]'s own
    /// picture, which is centred on the observer by construction. After
    /// Task 9 the walk view draws the raster through `self.window` — and
    /// nothing returned the rung, so the plate showed a coarse chart with
    /// [`Self::compose_perception_layer`] correctly refusing off band B: no
    /// `@`, no creatures, no cursor, no strip, no signal of any kind, while
    /// the entry pane went on narrating the player's immediate
    /// surroundings. **The picture and the prose described different
    /// places.**
    ///
    /// **A RUNG BELONGS TO THE CONSULTATION, NOT TO THE WALKER**, and that
    /// is the whole rule. Zoom is reachable only from [`Focus::Map`]
    /// (`input::action_for` gives `-`/`+` to no other focus), so a coarse
    /// rung is a thing the reader asked the MAP for; the walker never asked
    /// for it and cannot undo it, because the walk view has no zoom
    /// gesture. Leaving the map is where it ends.
    ///
    /// **Why this shape and not the two others considered.** (a) *Gate the
    /// plate on `at_walk_band_rung()` outside `Focus::Map`* would hand the
    /// walk view back to `chart::draw`'s hex scatter in a state reachable
    /// in six keystrokes — reintroducing the very picture this campaign
    /// exists to replace, in the default view. (b) *Give `Focus::Walk` its
    /// own band-B window, leaving `self.window` to the map*, preserves the
    /// reader's rung across consultations, and costs a SECOND window: this
    /// module keeps exactly one, and [`Self::active_plate_dims`],
    /// [`Self::move_cursor`], [`Self::reclamp_window`],
    /// [`Self::refresh_strip`] and [`Self::resolve_world_view`] all read
    /// it. None of them runs in `Focus::Walk` today, so the second window
    /// would be correct today and would become the two-independent-copies
    /// bug this file's own docs warn about repeatedly the moment one of
    /// them did. What (b) buys is the reader's RUNG across a consultation —
    /// and [`Self::enter_map`] already discards the reader's PAN on every
    /// entry ("centre on arrival"), so a consultation already restarts
    /// where the player stands. This makes it restart at the band they
    /// stand in too, which is the same ruling carried one step, not a new
    /// one.
    ///
    /// **Two doors, one owner.** `Esc` ([`Self::toggle_focus`]) is not the
    /// only departure: `Action::FocusAndType` also leaves the map, for the
    /// command line, on any printable key. The strip's own invariant
    /// already had those two owners and the second arm's comment records
    /// what that cost once; this method is now the single owner of both
    /// halves, so a third door cannot acquire one and miss the other.
    fn leave_the_map(&mut self) {
        // Unconditional — the strip belongs to the map, and every departure
        // clears it whatever focus we came from (`strip_text()` re-checks
        // focus, so this is belt and braces, but the invariant is stated
        // here rather than relying on that).
        let was_consulting = self.focus == Focus::Map;
        self.strip = None;
        if !was_consulting {
            return;
        }
        self.window.depth = BAND_B_RUNG;
        // The origin as well as the rung: a coarse rung's origin, reinterpreted
        // at band B, is some arbitrary corner of a 23,245-wide chart — the
        // arctic-corner state `centre_band_b_on_the_observer`'s own doc calls
        // "not shippable". Centring is what makes the returned band a place.
        self.centre_on_the_observer();
    }

    /// The map cursor's screen position, or `None` unless the map is
    /// focused — matching `render_with`'s `map_cursor` parameter, which
    /// hides the terminal's hardware cursor on `None` (only consulted at
    /// all under `Focus::Map`; see `render_with`'s own doc).
    pub fn cursor(&self) -> Option<Cursor> {
        (self.focus == Focus::Map).then_some(self.cursor)
    }

    /// The map strip's current text, or `None` unless the map is focused —
    /// matching `render_with`'s `strip` parameter.
    pub fn strip_text(&self) -> Option<&str> {
        if self.focus == Focus::Map {
            self.strip.as_deref()
        } else {
            None
        }
    }

    /// The whole-world Mercator plate, freshly rendered for a `w`-by-`h`
    /// terminal — The Portolan part II, Task 3a wires this into the redraw
    /// path.
    ///
    /// **The plate's own size is derived from the SAME region `compose`
    /// itself draws into, never a second copy of it** — see
    /// [`Self::world_plate_dims`], which is the one place the width rule
    /// ([`hornvale_game_core::spread::world_plate_width`]) and the height
    /// rule ([`hornvale_game_core::spread::content_height`]) are read. This
    /// is what lets those functions' `pub`-ness do its job: `compose` and
    /// this method size their two `Grid`s from the identical computation.
    ///
    /// **Unconditional — draws the Mercator whether or not the world view is
    /// active.** [`Driver::world_plate_for_redraw`] is the gated entry point
    /// every real caller (`main.rs`'s `redraw`) must use instead; this
    /// method stays `pub` because it is also the seam a test drives to
    /// exercise the plate's own rendering directly, unconditionally. The
    /// plate is far cheaper than it was — one mesh address per cell since
    /// The Quadrat's Task 3, not 49 spatial searches (`plate.rs`'s own
    /// doc) — but a full redraw is still a full redraw, so a caller
    /// reaching this directly still owns not paying it needlessly.
    pub fn world_plate(&mut self, w: u16, h: u16) -> hornvale_game_core::Grid {
        let (plate_width, plate_height) = Self::world_plate_dims(w, h);
        // `&mut self` since The Wash's Task 6, and the reason is the cache:
        // the plate is drawn through `Self::reflectance_cache`, the one
        // store that keeps a `(facet, season)` reflectance from being
        // recomputed for every tile of every draw.
        let (illuminant, observer, at, season) = self.plate_light();
        let mut spectral = plate::Spectral {
            ctx: Some(self.session.context()),
            illuminant: &illuminant,
            observer: &observer,
            at,
            season,
            cache: Some(&mut self.reflectance_cache),
        };
        plate::draw(
            &self.terrain,
            &self.geo,
            &self.nearest,
            &self.frame,
            &self.window,
            plate_width,
            plate_height,
            &self.sites,
            &self.volcanoes,
            &self.waterfalls,
            &self.discovered,
            &mut spectral,
        )
    }

    /// The light, the observer, the instant and the season this session's
    /// plate is resolved through (The Wash, Task 6) — everything
    /// [`plate::Spectral`] needs EXCEPT the two halves that are borrows of
    /// this struct's own fields (the locale context and the reflectance
    /// cache), which is why they are not returned here: a `&self` method
    /// handing back a `Spectral` would borrow the whole `Driver` and make
    /// `&mut self.reflectance_cache` impossible at the call site.
    ///
    /// **The illuminant is resolved ONCE per draw, above the tile loop**, as
    /// [`plate_illuminant`]'s own doc requires: it is anchored to where the
    /// reader stands (`session.day()` and the possession's own latitude),
    /// never to the tile, so a coarse rung lights the whole plate with the
    /// reader's own sunlight rather than sweeping a terminator across it.
    ///
    /// **The observer is two-valued because `NO_COLOR` is the only depth
    /// probe this client has** — see [`crate::observer::terminal_observer`].
    fn plate_light(
        &self,
    ) -> (
        hornvale_kernel::color::Illuminant,
        crate::observer::TerminalObserver,
        hornvale_kernel::WorldTime,
        u32,
    ) {
        // SAFETY: `world` was allocated by `Box::into_raw` in `start`, is
        // reclaimed only in `Drop`, and is never aliased mutably — this is
        // the same shared read `Self::sites`' own construction takes.
        let world = unsafe { &*self.world };
        let at = self.session.day();
        let latitude_deg = self.session.position().coord().latitude;
        (
            plate_illuminant(world, self.calendar.as_ref(), at, latitude_deg),
            crate::observer::terminal_observer(plate::colour_allowed()),
            at,
            season_bucket_for(self.calendar.as_ref(), at),
        )
    }

    /// The world plate's own size for a `w`-by-`h` terminal — the SAME
    /// region `compose` draws into, computed in one place because three
    /// callers now need it ([`Self::world_plate`] draws a whole plate;
    /// [`Self::world_plate_for_redraw`] draws its three layers separately
    /// and must size all of them identically; [`Self::active_plate_dims`]
    /// clamps the cursor and the window to it). A second copy of this
    /// arithmetic is the bug shape `content_height`'s own doc warns about.
    ///
    /// **The height is [`hornvale_game_core::spread::content_height`], not
    /// `width / GLYPH_ASPECT` (The Quadrat, Task 9).** The old derivation
    /// agreed with this one for every width the old fit could produce —
    /// that fit's own ceiling was `GLYPH_ASPECT * content_height(h)`, so
    /// halving it landed exactly on `content_height(h)`. Task 9 raised the
    /// width floor to half the terminal, which on a wide, short terminal
    /// exceeds that ceiling (200x50: 100 columns against a ceiling of 92),
    /// and half of 100 is 50 rows on a page with room for 46. Those four
    /// rows would have been drawn into a grid the page then clipped, and —
    /// worse — [`Self::active_plate_dims`] would have let the cursor walk
    /// into them, which is the Task 3a review finding with the axes
    /// swapped.
    fn world_plate_dims(w: u16, h: u16) -> (u16, u16) {
        (
            hornvale_game_core::spread::world_plate_width(w, h),
            hornvale_game_core::spread::content_height(h),
        )
    }

    /// The world plate to hand [`hornvale_game_core::render_with`] for a
    /// `w`-by-`h` redraw — `Some` exactly when the band is one whose plate
    /// is the raster ([`Self::raster_is_drawn`]), `None` otherwise.
    ///
    /// **THE GATE IS THE BAND, AND ONLY THE BAND (The Quadrat, Task 9).**
    /// It was `Focus::Map && raster_is_drawn()` until Task 9, and the focus
    /// clause is what left this campaign's third reported defect half
    /// fixed: the default focus is [`Focus::Walk`] (decision 0160), so the
    /// view a player looks at while WALKING took the `None` arm and
    /// `spread::compose` drew the walk band's old hex scatter into a fixed
    /// 40-column pane. Task 6 had put band B on the square raster, but only
    /// while the map was focused — which is not the view the complaint was
    /// about.
    ///
    /// Dropping the clause is safe BY CONSTRUCTION rather than by care,
    /// because [`Self::raster_is_drawn`] was already a question about the
    /// band alone: the chamber band still answers `false`, so Task 6's fix
    /// round 1 (typing `map` indoors must not replace the floor plan) is
    /// preserved without anything else being asked to preserve it. What
    /// FOCUS still decides is unchanged and is all it ever should have
    /// decided: where the keys go, whether a cursor is reported, and
    /// whether the map strip has text.
    ///
    /// **The RUNG is not part of the gate either (The Quadrat, Task 6 and
    /// its fix round 1).** It used to be `Focus::Map && world_view()`, where
    /// `world_view()` meant "some rung coarser than band B"; band B draws
    /// the raster now, so that clause had no discriminating answer left.
    ///
    /// **This is the ONE place that decision is made** (The Portolan part
    /// II's own fix round 1). Before this method existed, `main.rs`'s
    /// `redraw` computed `Some(driver.world_plate(w, h))` whenever
    /// `focus() == Focus::Map`, with no further gate — reproducing the exact
    /// class of bug `content_height`'s own doc warns about elsewhere in this
    /// crate: two independent computations of "is the raster showing" would
    /// have been one too many, this time on the ACTIVATION question rather
    /// than a dimension. `main.rs` calls only this method now, and every
    /// test that wants to know what a redraw would draw calls it too rather
    /// than reimplementing the check a third time.
    ///
    /// **Three layers now, and the lowest is assembled from TILES (The
    /// Quadrat, Tasks 4, 5 and 6).** The perception overlay is the third —
    /// see [`Self::compose_perception_layer`], which is a no-op off band B. The terrain raster comes from
    /// [`crate::tiles::TileCache::compose`], which draws only the chart
    /// tiles this window covers that it does not already hold;
    /// [`plate::draw_feature_layer`] is then composed over the assembled
    /// grid, on every call, hit or miss. So a discovery is visible on the
    /// next redraw exactly as before — it just no longer repaints the
    /// raster underneath, which is the whole of
    /// `CLIENT-tiles-need-the-overlay-split`. This is also why the composed
    /// grid is never stored back: the cache holds terrain, and a plate with
    /// features already burned into it could not answer a later frame whose
    /// discovery set had moved.
    ///
    /// **`compose` returns a fresh `Grid`, so there is no `clone` here any
    /// more** — the tiles are the cached objects, and the assembled plate
    /// was never one.
    pub fn world_plate_for_redraw(&mut self, w: u16, h: u16) -> Option<hornvale_game_core::Grid> {
        if !self.raster_is_drawn() {
            return None;
        }
        let (plate_width, plate_height) = Self::world_plate_dims(w, h);
        // THE LIVE SESSION'S OWN PATH (The Wash, Task 6). `main.rs`'s
        // `redraw` calls this method and never `world_plate`, so this is the
        // call that decides what a player actually sees — it is threaded
        // with a real `Some(ctx)` for that reason, and `TileCache`'s key
        // carries `(season, illuminant)` so a cached tile can never outlive
        // the light it was drawn under.
        let (illuminant, observer, at, season) = self.plate_light();
        let mut spectral = plate::Spectral {
            ctx: Some(self.session.context()),
            illuminant: &illuminant,
            observer: &observer,
            at,
            season,
            cache: Some(&mut self.reflectance_cache),
        };
        let mut grid = self.tiles.compose(
            &self.terrain,
            &self.geo,
            &self.nearest,
            &self.frame,
            &self.window,
            plate_width,
            plate_height,
            plate::colour_allowed(),
            &mut spectral,
        );
        // No `w`/`h` here, deliberately: the feature layer reads the window's
        // size from the grid it is drawing onto, so this path cannot hand it a
        // bound the cached terrain grid disagrees with (fix round 1, Minor 1).
        plate::draw_feature_layer(
            &mut grid,
            &self.geo,
            &self.frame,
            &self.window,
            plate::colour_allowed(),
            &self.sites,
            &self.volcanoes,
            &self.waterfalls,
            &self.discovered,
        );
        // LAYER THREE, band B only (The Quadrat, Task 6): the observer's own
        // position and the marks standing around them, composed over the
        // raster rather than instead of it. This is what Ruling 19's
        // surviving concern names — `spread::compose`'s plate selection is
        // either/or, so handing band B a plate at all would otherwise have
        // removed the only thing that draws `'@'` and the creatures. Redrawn
        // every frame like the feature layer and for the same reason: it is a
        // handful of projections, and giving it an invalidation key is the
        // defect `CLIENT-tiles-need-the-overlay-split` records.
        self.compose_perception_layer(&mut grid);
        Some(grid)
    }

    /// Apply one input [`Action`], returning whether the session RELEASED.
    ///
    /// This is now the loop's own answer to "was that the last keypress" —
    /// replacing `main.rs`'s old check on the literal verb line it had just
    /// sent, which could only ever see the `"release"` spelling (ledger #8:
    /// the sim honours `"quit"` too, and once free text reaches the buffer
    /// the caller cannot tell which synonym a player typed without asking
    /// the driver). Only [`Action::Submit`] can ever return `true`.
    ///
    /// `Type`/`FocusAndType` insert into the buffer (`FocusAndType` also
    /// returns focus to the CLI — spec §2's one-keypress bounce);
    /// `DeleteBack`/`CaretBy` edit it; `HistoryPrev`/`HistoryNext` recall a
    /// remembered line (past the newest empties the buffer, matching
    /// [`crate::history::History::next`]'s own contract); `Submit` sends
    /// the buffer — or, on an empty buffer, does nothing at all and returns
    /// `false` (spec §6: the buffer is the last reversible thing before an
    /// irreversible act, so a stray `Enter` must not cost a turn). A
    /// submitted line that is exactly `map` enters the map focus INSTEAD OF
    /// being sent (exact-after-trim, mirroring `Session::handle`'s
    /// first-token convention) — decision 0290, a mode gesture is not a
    /// fetch; it costs no turn and returns `false`.
    /// `Move` executes a walk-mode direction exactly like a submitted line
    /// (echo + history + `handle`), minus any buffer involvement.
    /// `CursorBy`/`ToggleFocus` are unchanged from part I.
    ///
    /// **`Zoom` is now implemented (Task 3b): one continuous ladder built
    /// from already-routed keys, chosen over a typed `world` verb (no
    /// reply channel — the entry pane's prose is wire-carried) and over a
    /// new letter key (would break `Focus::Map`'s deliberately total
    /// routing table).** With the world view off, zooming out (`-`) turns it
    /// on at the coarsest rung; with the world view on at its finest rung,
    /// zooming in (`+`/`=`) turns it back off; between those, the keys move
    /// the world map's own rung, clamped to [`GLOBE_RUNG`]`..=`[`BAND_B_RUNG`]. See
    /// [`Self::apply_zoom`].
    ///
    /// **`Recentre` (`.`) rolls the projection to the cursor (spec §3.2),
    /// only on command — never a side effect of cursor movement.** A no-op
    /// unless the world view is active. See [`Self::recentre`].
    pub fn apply(&mut self, action: Action) -> bool {
        match action {
            Action::ToggleFocus => {
                // Esc during the noun prompt CANCELS it rather than
                // toggling focus — restoring the buffer verbatim is the
                // stash convention the history semantics established.
                if let Some(saved) = self.noun_prompt.take() {
                    self.line.set(saved.saved_buffer);
                    while self.line.caret() < saved.saved_caret {
                        self.line.caret_right();
                    }
                    return false;
                }
                self.toggle_focus();
                false
            }
            Action::CursorBy(dx, dy) => {
                self.move_cursor(dx, dy);
                self.refresh_strip();
                false
            }
            Action::Type(c) => {
                self.line.insert(c);
                self.clear_completion();
                false
            }
            Action::FocusAndType(c) => {
                // Land on `Focus::Cli` EXPLICITLY rather than routing
                // through `toggle_focus()`: `toggle_focus` now maps BOTH
                // Map and Walk to Walk (Esc's contract), so a toggle from
                // either producing focus would leave the player on the walk
                // view instead of the command line — and never on Cli at
                // all. `FocusAndType` is produced under both Map and Walk
                // (`input::action_for`) and means "I want to type": one
                // keypress lands on the CLI and types (spec §2). The strip
                // is still cleared here by hand, keeping the invariant
                // `toggle_focus`'s own doc states ("leaving it clears the
                // strip") with this arm named as its second owner; an
                // earlier revision left that invariant with two owners and
                // one violator, masked only because `strip_text()`
                // re-checks focus before returning.
                // THE MAP'S SECOND DOOR (Task 9, fix round 1). This arm used
                // to clear the strip itself, which was the whole of leaving
                // the map at the time; leaving it also returns the rung now,
                // so both halves are taken from the one owner rather than
                // this arm re-spelling either. Called BEFORE the focus moves
                // — `leave_the_map` asks what focus we are leaving.
                self.leave_the_map();
                self.focus = Focus::Cli;
                self.line.insert(c);
                self.clear_completion();
                false
            }
            Action::DeleteBack => {
                // In the noun prompt, backspace stops at the dispatched
                // prefix: deleting into "examine " would corrupt what
                // Enter sends.
                if self.noun_prompt.is_some()
                    && self.line.text().chars().count() <= EXAMINE_PROMPT_PREFIX.chars().count()
                {
                    return false;
                }
                self.line.backspace();
                self.clear_completion();
                false
            }
            Action::CaretBy(dx) => {
                match dx.cmp(&0) {
                    std::cmp::Ordering::Less => self.line.caret_left(),
                    std::cmp::Ordering::Greater => self.line.caret_right(),
                    std::cmp::Ordering::Equal => {}
                }
                self.clear_completion();
                false
            }
            Action::HistoryPrev => {
                // Recalling history commits the prompt: the composed line
                // becomes the ordinary buffer the recall replaces.
                self.noun_prompt = None;
                if let Some(text) = self.history.prev() {
                    self.line.set(text.to_string());
                }
                self.clear_completion();
                false
            }
            Action::HistoryNext => {
                self.noun_prompt = None;
                match self.history.next() {
                    Some(text) => self.line.set(text.to_string()),
                    None => self.line.set(String::new()),
                }
                self.clear_completion();
                false
            }
            Action::Submit => {
                if self.line.is_empty() {
                    return false;
                }
                let caret = self.line.caret();
                let taken = self.line.take();
                // BARE `x` — the conventional examine shorthand — enters the
                // noun prompt instead of dispatching and burning a turn on
                // "Examine what?" (spec §4.4). Nothing is echoed, nothing
                // enters history, no turn advances: the prompt is a question
                // the client asks locally.
                if taken.trim() == "x" {
                    if self.noun_prompt.is_none() {
                        self.noun_prompt = Some(NounPrompt {
                            saved_buffer: taken,
                            saved_caret: caret,
                        });
                        self.line.set(EXAMINE_PROMPT_PREFIX.to_string());
                    } else {
                        // Already prompting: a bare `x` typed INTO the
                        // prompt is just text — put it back.
                        self.line.set(taken);
                    }
                    return false;
                }
                // Any other submission exits the prompt: the composed line
                // IS what the player chose to send.
                self.noun_prompt = None;
                self.clear_completion();
                self.history.push(taken.clone());
                self.echo = Some(taken.clone());
                // BARE `map` — and only bare `map` — enters the map focus,
                // AND IT DOES NOT SEND THE VERB (decision 0290). The
                // comment that used to sit here already argued this ("the
                // plate is redrawn from `Spatial` every turn regardless,
                // which is why submitting `map` is a MODE GESTURE and not a
                // fetch") while the code sent it anyway, and that gap IS
                // The Quadrat's second reported defect: the sim answered a
                // mode gesture with a picture, and the picture landed in
                // the prose pane over the top of the plate the player had
                // just asked to look at. The acknowledgement is the focus
                // changing — the precedent `Self::recentre` states for
                // itself ("the acknowledgement is the map redrawing").
                //
                // The bare-from-argument split mirrors a convention the sim
                // already keeps rather than inventing one: `Session::handle`
                // splits its own forms on `rest.is_empty()` (see the
                // `"map" if self.inside.is_some() && rest.is_empty()` and
                // `"eyes" if rest.is_empty()` arms), because the two mean
                // different things. So `" map "` is the gesture; `"map x"`,
                // `"map out 2"` and `"examine map"` are sent as typed.
                //
                // **`map out N` STAYS A FETCH, and that is load-bearing
                // rather than conventional** (spec §4.2). It is the
                // diagnostic path that caught The Quire's wrong projection,
                // where the client and the sim were rendered side by side
                // over the identical thirty-one facets and only one was
                // right; removing every route to the sim's own picture
                // would delete the comparison guarding spec §5's H2. It
                // also must not focus the map: `Session::map` takes `&self`
                // and returns prose, so no argument form moves the plate,
                // and focusing after `map out 2` would hand the player a
                // cursor on an unzoomed plate they did not ask about.
                //
                // The focus test is what keeps re-submitting `map` from the
                // map paying a strip refresh for an identical answer; the
                // gesture still swallows the line either way, because a
                // bare `map` is never a fetch, focused or not.
                if taken.trim() == "map" {
                    if self.focus != Focus::Map {
                        self.enter_map();
                    }
                    return false;
                }
                self.handle(&taken)
            }
            Action::Move(word) => {
                // A walk-mode arrow key acts like a submitted line — same
                // echo, same history recall — but never touches the buffer:
                // the keystroke never entered it, so nothing is left behind
                // or cleared by moving.
                let taken = word.to_string();
                self.history.push(taken.clone());
                self.echo = Some(taken.clone());
                self.handle(&taken)
            }
            // Completion (spec §4.2). Under [`TabStyle::Hint`] the token
            // at the caret is completed against the lexicon: a unique match
            // is filled whole, an ambiguous one extends to the shared stem
            // and parks the alternatives in `self.hint` for the strip to
            // show, and a no-match changes nothing. Under [`TabStyle::Cycle`]
            // an open rotation steps BEFORE the engine is consulted — the
            // filled name alone would only ever complete to itself. Never
            // advances the turn.
            Action::Complete => {
                if self.tab_style == TabStyle::Cycle {
                    self.cycle_step();
                } else {
                    let decision = completion_decision(
                        &self.line.text(),
                        self.line.caret(),
                        &self.scope.candidates(),
                    );
                    self.apply_hint(decision);
                }
                false
            }
            Action::Zoom(delta) => {
                self.apply_zoom(delta);
                false
            }
            Action::Recentre => {
                self.recentre();
                false
            }
            Action::None => false,
        }
    }

    /// Apply one hint-style completion decision. Always leaves the turn
    /// unadvanced (the caller returns `false` regardless).
    fn apply_hint(&mut self, decision: CompletionDecision) {
        match decision {
            CompletionDecision::Noop => {}
            CompletionDecision::Fill(name) => {
                self.line.replace_word_at_caret(&name);
                self.hint = None;
            }
            CompletionDecision::Ambiguous { stem, matches } => {
                self.line.replace_word_at_caret(&stem);
                self.hint = Some(Hint { stem, matches });
            }
        }
    }

    /// One Complete keypress under [`TabStyle::Cycle`]. While a rotation is
    /// open over the word at the caret (same char offset it opened at), it
    /// steps to the next match, wrapping — this check happens FIRST,
    /// because a filled name re-completed through the engine would only
    /// ever yield itself. Otherwise a fresh ambiguous match fills its first
    /// alternative and opens the rotation; unique fills and no-matches
    /// behave exactly as under [`TabStyle::Hint`], closing any rotation.
    fn cycle_step(&mut self) {
        let text = self.line.text();
        let caret = self.line.caret();
        let here = word_start_at_caret(&text, caret);
        if let Some(state) = &mut self.cycle
            && matches!(here, Some(s) if s == state.start)
        {
            state.index = (state.index + 1) % state.matches.len();
            let name = state.matches[state.index].clone();
            self.line.replace_word_at_caret(&name);
            return;
        }
        match completion_decision(&text, caret, &self.scope.candidates()) {
            CompletionDecision::Ambiguous { stem, matches } => {
                self.line.replace_word_at_caret(&stem);
                let name = matches[0].clone();
                let start = word_start_at_caret(&self.line.text(), self.line.caret()).unwrap_or(0);
                self.line.replace_word_at_caret(&name);
                // No hint here by choice (the strip stays quiet under a
                // rotation): `Hint.stem` is documented as the longest
                // common prefix, and the only thing this path could store
                // is a full match — so it stores nothing instead. The
                // cycle presents via token rotation, not a hint line.
                self.cycle = Some(CycleState {
                    start,
                    matches,
                    index: 0,
                });
            }
            CompletionDecision::Fill(name) => {
                self.line.replace_word_at_caret(&name);
                self.clear_completion();
            }
            CompletionDecision::Noop => {}
        }
    }

    /// Drop any pending hint and cycle state — every action that mutates
    /// or submits the line owns one of these calls.
    fn clear_completion(&mut self) {
        self.hint = None;
        self.cycle = None;
    }

    /// Enter the map focus and resolve its strip. Also called by `apply`'s
    /// `Submit` arm when a submitted line's first token is exactly `map`
    /// (see there).
    fn enter_map(&mut self) {
        self.focus = Focus::Map;
        // BEFORE the strip resolves, not after: the strip names what the
        // cursor points at, and centring moves what that is.
        //
        // **AT ANY RUNG, not band B's alone** (fix round 1, Important 2, and
        // a defect this task created). Task 7 took the observer-centring off
        // the zoom path, which was right — see
        // [`Self::centre_band_b_on_the_observer`] — but "zoom out one, zoom
        // in one" had been an ACCIDENTAL way home, and removing it left a
        // coarse rung with no way home at all: the centring returned early
        // off band B, so `map` + Enter did nothing, and
        // [`Self::compose_perception_layer`] deliberately refuses off band B,
        // so no marker is drawn and the reader cannot even see which
        // direction home is. `.` is no help — it rolls the projection about
        // the CURSOR, carrying you further off. The only route left was `+`
        // to the ladder's ceiling and THEN `map`.
        //
        // THE ARRIVAL RUNG, BEFORE THE CENTRING (The Hachure, Stage 0).
        // Order matters: `centre_on_the_observer` resolves against
        // `window.depth`'s own chart, so setting the rung afterwards would
        // centre on the old rung's chart and leave the window somewhere
        // arbitrary on the new one — the same arctic-corner state
        // `leave_the_map` calls "not shippable", arrived at from the other
        // side.
        //
        // **Why the map does not inherit the walker's rung.** `Driver::start`
        // leaves `window.depth` at `BAND_B_RUNG`, which is where the WALKER
        // belongs — but band B is seven rungs finer than the terrain grid and
        // `terrain_at_tile` resolves every rung through the grid-level
        // ancestor, so a consultation opened there draws ONE vertex's reading
        // across the whole screen (measured: 1 distinct vertex on a 120x40
        // plate, seed 42). A map that shows one flat reading is not a map.
        //
        // This is an ENTRY rung, not a clamp: the whole ladder stays
        // reachable by zooming, `virtual_dims` is untouched, and a tile is
        // still a facet at its rung's depth (decision 0287). `leave_the_map`
        // restores band B for the walker.
        self.window.depth = plate::map_entry_rung(self.geo.depth());
        // Entering the map is an ARRIVAL, and arrival is what centres
        // (the rule this task settled is "centre on arrival, anchor on
        // gesture"). Nothing about that rule was ever specific to a rung;
        // only the implementation was.
        self.centre_on_the_observer();
        self.refresh_strip();
    }

    /// Whether the ladder is on the WALK BAND's own rung — [`BAND_B_RUNG`],
    /// the finest rung, the one the observer actually stands on.
    ///
    /// **This REPLACES `world_view()`, and the replacement is a narrowing
    /// rather than a rename** (The Quadrat, Task 6). `world_view()` asked
    /// "does this rung draw the Mercator raster instead of the walk band's
    /// own chart", and its own doc predicted it would lose its referent the
    /// moment band B joined the raster. It has: every rung draws the raster
    /// now, band B included, so that question has no discriminating answer
    /// left and a method still called `world_view` would read as though one
    /// rung were not a world view.
    ///
    /// What survives is a DIFFERENT distinction, and it is the one every
    /// remaining caller actually wanted: **band B has a plate AND a
    /// perception overlay AND a sight caption; a coarser rung has a plate
    /// and neither.** On the shipped ladder
    /// ([`GLOBE_RUNG`]`..=`[`BAND_B_RUNG`], which [`Self::apply_zoom`]
    /// clamps to) this is the exact complement of what `world_view()`
    /// returned, so every call site that used to ask `world_view()` asks
    /// `!at_walk_band_rung()` and means the same thing.
    ///
    /// **Derived, never stored** — the property Task 2 introduced it for,
    /// and the reason the zoom keys mean one thing at every rung:
    /// [`Self::apply_zoom`] changes one number and which layers appear
    /// follows from it.
    fn at_walk_band_rung(&self) -> bool {
        self.window.depth == BAND_B_RUNG
    }

    /// Whether the RASTER is what the plate region draws right now — which
    /// is a question about the BAND, not the rung.
    ///
    /// **Band A keeps its own renderer, and fix round 1's F5 is why this
    /// method exists.** `world_plate_for_redraw`'s gate briefly became
    /// `Focus::Map` alone, so typing `map` inside a chamber replaced the
    /// chamber floor plan with the world raster — and
    /// [`Self::compose_perception_layer`] correctly refuses off the walk
    /// band, so nothing marked the player's position there at all. The
    /// campaign's spec says "Band A keeps its own renderer and this campaign
    /// does not touch it", and that is a promise: whether consulting a world
    /// map from indoors is DESIRABLE is a real question, but it is a
    /// deliberate design change for a later campaign, not a side effect of a
    /// raster task.
    ///
    /// So: no plate at the chamber band, which means `spread::compose` draws
    /// `plan::draw` exactly as it did before this campaign, the cursor
    /// clamps to the narrow plate that is really on screen, a cursor move
    /// does not silently scroll a window nobody can see, and the strip
    /// answers [`NOTHING_HERE_YET`] — spec §9 still defers the chamber
    /// resolver.
    fn raster_is_drawn(&self) -> bool {
        self.on_walk_band
    }

    /// The active plate's own width and height, in grid columns and rows —
    /// the chamber band's fixed
    /// [`hornvale_game_core::spread::PLATE_WIDTH`] and `self.plate_height`
    /// when the raster is not what is drawn, or [`Self::world_plate_dims`]
    /// outright when it is: the SAME pair [`Driver::world_plate`] itself
    /// draws into, read from that one function rather than restated here.
    ///
    /// **Task 3a's own review finding, closed here (Task 3b):**
    /// `move_cursor` used to clamp to the fixed walk-band width regardless
    /// of which plate was actually on screen, leaving columns 40-103 of a
    /// 104-column world plate unreachable. Every caller that needs "how
    /// big is the plate right now" — the cursor clamp, the scroll math,
    /// the world-view resolver — reads it from here, never a second copy
    /// of the fit.
    fn active_plate_dims(&self) -> (u16, u16) {
        // TWO ARMS, KEYED ON THE BAND rather than on the rung (Task 6, fix
        // round 1's F5). Before Task 6 the split was `world_view()`, a rung
        // comparison, because band B drew `core`'s own chart into the narrow
        // plate. Band B draws the raster now — so the rung no longer decides
        // — but the CHAMBER band still draws `plan::draw` into
        // `spread::PLATE_WIDTH`, and clamping the cursor to a 104-column fit
        // over a 40-column floor plan is the Task 3a review finding with the
        // operands swapped. `spread::compose` widens the plate region only
        // when it is actually handed a plate, which is exactly
        // [`Self::raster_is_drawn`].
        if self.raster_is_drawn() {
            // The IDENTICAL call `world_plate`/`world_plate_for_redraw`
            // size their grids from — not a second spelling of it (Task 9;
            // an earlier revision derived the height as
            // `width / GLYPH_ASPECT` here and in `world_plate_dims`, two
            // copies of one rule that Task 9's wider floor would have
            // broken in both places at once).
            Self::world_plate_dims(self.term_w, self.term_h)
        } else {
            (hornvale_game_core::spread::PLATE_WIDTH, self.plate_height)
        }
    }

    /// Re-clamp `self.window`'s origin into the CURRENT zoom's virtual
    /// bounds (`plate::virtual_dims`), for whichever plate is active.
    /// Needed whenever the active plate's own size or the window's zoom
    /// changes: `origin_col` wraps (longitude wraps), `origin_row` clamps
    /// (latitude stops at the clamp — spec §4.2). Only ever meaningful
    /// while the world view is on; callers only invoke it then.
    fn reclamp_window(&mut self) {
        let (_, plate_h) = self.active_plate_dims();
        let (virtual_w, virtual_h) = plate::virtual_dims(self.window.depth);
        if virtual_w > 0 {
            self.window.origin_col %= virtual_w;
        }
        let max_origin_row = virtual_h.saturating_sub(u32::from(plate_h));
        self.window.origin_row = self.window.origin_row.min(max_origin_row);
    }

    /// Move the cursor by `(dx, dy)` grid cells, clamped to the ACTIVE
    /// plate's REAL bounds ([`Self::active_plate_dims`] — fix round 2: an
    /// earlier revision clamped to the 80x24 floor's fixed height
    /// regardless of the terminal's actual size; Task 3a's review found
    /// the width half of the identical bug for the world plate).
    ///
    /// **With the world view on, an out-of-bounds move SCROLLS instead of
    /// clamping (spec §4.2): the window moves, the cursor stays at the
    /// edge.** Every unit of movement beyond the screen edge becomes one
    /// unit of window movement — the same delta, split between "how far
    /// the cursor got" and "how far the window had to move to make room
    /// for the rest," so a `dx`/`dy` far larger than the plate (a resolver
    /// test's deliberate stress case, not just an arrow key) is handled by
    /// the identical formula as a single-cell nudge. Longitude wraps
    /// (`origin_col`, modulo the virtual width); latitude clamps
    /// (`origin_row`, stopped at the projection's own bound) — spec §4.2's
    /// own asymmetry.
    fn move_cursor(&mut self, dx: i16, dy: i16) {
        let (plate_w, plate_h) = self.active_plate_dims();
        let max_x = i64::from(plate_w).saturating_sub(1).max(0);
        let max_y = i64::from(plate_h).saturating_sub(1).max(0);

        let raw_x = i64::from(self.cursor.x) + i64::from(dx);
        let clamped_x = raw_x.clamp(0, max_x);
        self.cursor.x = clamped_x as u16;

        let raw_y = i64::from(self.cursor.y) + i64::from(dy);
        let clamped_y = raw_y.clamp(0, max_y);
        self.cursor.y = clamped_y as u16;

        // Band B is a scrollable Mercator window like every other RUNG since
        // Task 6 — but only where a Mercator is drawn at all. Scrolling a
        // window the chamber band never shows would move `self.window`
        // invisibly and leave the map somewhere else on the way back out
        // (fix round 1, F5).
        if self.raster_is_drawn() {
            let (virtual_w, virtual_h) = plate::virtual_dims(self.window.depth);
            let spill_x = raw_x - clamped_x;
            if virtual_w > 0 {
                self.window.origin_col = (i64::from(self.window.origin_col) + spill_x)
                    .rem_euclid(i64::from(virtual_w))
                    as u32;
            }
            let spill_y = raw_y - clamped_y;
            let max_origin_row = i64::from(virtual_h)
                .saturating_sub(i64::from(plate_h))
                .max(0);
            self.window.origin_row =
                (i64::from(self.window.origin_row) + spill_y).clamp(0, max_origin_row) as u32;
        }
    }

    /// Zoom the map view: ONE continuous ladder of mesh rungs, driven by
    /// already-routed keys (`-`/`+`/`=`) rather than a new binding or a
    /// fourth focus state — chosen over a typed `world` verb (no reply
    /// channel exists; the entry pane's prose is wire-carried) and over a
    /// new letter key (would break `Focus::Map`'s deliberately total
    /// routing table).
    ///
    /// - `delta > 0` (zoom IN) RAISES `depth` one rung, saturating at
    ///   [`BAND_B_RUNG`] — the finest rung, one tile per band-B facet.
    /// - `delta < 0` (zoom OUT) LOWERS `depth` one rung, saturating at
    ///   [`GLOBE_RUNG`] — the canonical grid level, below which the terrain
    ///   fields have no resolution to disclose.
    ///
    /// **The two keys change exactly one number and nothing else** (The
    /// Quadrat, Task 2). They used to mean two different things at the
    /// ladder's ends: a `world_view` boolean sat beside the rung, and
    /// zooming past either end flipped that boolean AND reset the window to
    /// the coarsest rung — the reported "zooms based on criteria I have not
    /// identified". Band B is a rung now, so WHICH LAYERS appear is a
    /// function of `depth` alone ([`Self::at_walk_band_rung`]), and crossing
    /// the band-B boundary is an ordinary step rather than a mode change.
    ///
    /// **"Exactly one number" is enforced by the `!= before` guard, and it
    /// has to be.** Everything this method does beyond moving `depth` — the
    /// re-clamp and band B's re-centring — is inside that guard, so a press
    /// that saturates at either end of the ladder really does nothing. Fix
    /// round 1's F1 is what this sentence is for: the re-centring call
    /// briefly sat outside it and a `+` at the ceiling discarded ~19,000
    /// tiles of scroll.
    ///
    /// **The ladder's direction inverted with The Quadrat**: `depth` is a
    /// mesh subdivision level, so zooming IN raises it and zooming OUT
    /// lowers it, where the old `zoom` field counted steps up from the
    /// coarsest.
    ///
    /// Either transition re-clamps the cursor into whichever plate is now
    /// active ([`Self::move_cursor`]`(0, 0)`) and re-resolves the strip —
    /// both the plate's own size and what the cursor points at can change
    /// on every one of these transitions.
    fn apply_zoom(&mut self, delta: i8) {
        use std::cmp::Ordering;
        let before = self.window.depth;
        // READ THE ANCHOR BEFORE THE RUNG MOVES. The point is expressed in
        // degrees precisely so it survives the move: the tile that carries
        // it is about to change size, address and chart, and degrees are the
        // one address both rungs share.
        let anchor = self
            .raster_is_drawn()
            .then(|| self.geographic_point_under_cursor());
        match delta.cmp(&0) {
            Ordering::Less => {
                self.window.depth = self.window.depth.saturating_sub(1).max(GLOBE_RUNG);
            }
            Ordering::Greater => {
                self.window.depth = self.window.depth.saturating_add(1).min(BAND_B_RUNG);
            }
            Ordering::Equal => {}
        }
        if self.window.depth != before {
            self.reclamp_window();
            // **INSIDE THE GUARD** — and read the next paragraph before
            // trusting any claim about what enforces that.
            //
            // Fix round 1's F1 is why the guard exists: what used to sit
            // here sat OUTSIDE, so a SATURATING press at the ladder's
            // ceiling acted anyway — measured jumping `origin_col`
            // 21169 -> 2176, throwing away ~19,000 tiles of the player's own
            // scroll on a keypress this method's own doc says changes
            // nothing. `zoom_plus_at_the_ladders_ceiling_keeps_the_players_
            // scroll` pins THAT defect and still discriminates it: a
            // re-introduced NON-idempotent action outside the guard fails it.
            //
            // **AN EARLIER REVISION OF THIS COMMENT CLAIMED THAT TEST ALSO
            // COVERS THIS BLOCK'S POSITION. IT DOES NOT, AND THE CLAIM WAS
            // DISPROVED BY APPLYING THE REGRESSION** (fix round 1, Important
            // 1): moving this block outside the guard leaves all eleven
            // relevant tests green. The comment's own premise defeated it —
            // the anchor is IDEMPOTENT at an unchanged rung (a tile centre
            // re-projects to its own tile, pinned by
            // `the_anchor_is_a_no_op_at_an_unchanged_rung`), so a press that
            // saturates cannot tell the two placements apart. The guard is
            // therefore belt-and-braces TODAY and load-bearing the moment
            // anything non-idempotent joins it; it stays for that reason,
            // not because a test is watching it.
            //
            // The one state that CAN tell them apart is the one state where
            // the anchor is not idempotent — a plate taller than the whole
            // chart, where the row clamp bites — and
            // `a_saturating_press_on_a_plate_taller_than_the_chart_moves_
            // nothing` drives exactly that. It is a narrow lever, and it is
            // the only one there is; saying so is the point.
            if let Some((lat, lon)) = anchor {
                self.anchor_geographic_point(lat, lon);
            }
        }
        self.move_cursor(0, 0);
        self.refresh_strip();
    }

    /// Roll the projection so the point currently under the cursor sits on
    /// the projection's own central line — spec §3.2's explicit re-centre
    /// command, bound to `.` in [`Focus::Map`] — chosen over a typed
    /// `recentre` command because `Focus::Map` routes every other
    /// `Char(c)` to `FocusAndType(c)` (a deliberately total table decision
    /// 0159's predictability rule rests on), `-`/`+`/`=` already establish
    /// punctuation as a map verb, and a typed client-only command has no
    /// reply channel (the entry pane's prose is wire-carried). A no-op
    /// unless the world view is active: the walk-band chart is not a
    /// Mercator projection and has no central line to roll onto.
    ///
    /// Also re-derives `self.window`'s origin so the cursor's own SCREEN
    /// position keeps naming the SAME geographic point after the roll,
    /// rather than the picture jumping out from under the reader: it
    /// re-projects that point under the NEW frame and sets the origin so
    /// the projected cell lands exactly where the cursor already sits.
    /// "The acknowledgement is the map redrawing" (`task-3b-brief.md`) —
    /// there is no reply channel to say anything else.
    fn recentre(&mut self) {
        // NO RUNG GATE since Task 6 — band B IS a Mercator projection now, so
        // the gesture is meaningful at every rung. A BAND gate remains, for
        // the reason the rung gate originally gave: the chamber band's floor
        // plan is not a Mercator projection and has no central line to roll
        // onto (fix round 1, F5).
        if !self.raster_is_drawn() {
            return;
        }
        let (lat, lon) = self.geographic_point_under_cursor();
        self.frame = mercator::centre_on(lat, lon);
        // ONE COPY OF THE RE-ANCHOR, shared with [`Self::apply_zoom`] (The
        // Quadrat, Task 7). It used to be spelled out here and would have
        // had to be spelled out again there; this file's own
        // `content_height` note records what two independent copies of one
        // computation cost the last time.
        self.anchor_geographic_point(lat, lon);
        self.refresh_strip();
    }

    /// The geographic `(latitude, longitude)`, in degrees, of the tile the
    /// map cursor points at right now — the reading every anchored gesture
    /// takes before it moves anything, and the one address that survives a
    /// change of rung.
    ///
    /// **Tile-resolution by construction, which is the map's whole address
    /// space.** What comes back is the CENTRE of the tile under the cursor,
    /// never a sub-tile position: nothing on screen distinguishes two points
    /// inside one character, so there is no finer answer to give. Every
    /// guarantee stated in terms of "the point under the cursor" inherits
    /// that, and the campaign's own zoom invariant is therefore asserted as
    /// a CONTAINMENT (the old point lies in the new tile) rather than as a
    /// tolerance in degrees.
    ///
    /// The column wraps and the row is held on the chart. The wrap is
    /// substantive rather than tidiness: `origin_col` may sit one short of
    /// the chart's width, so `origin_col + cursor.x` routinely runs past it,
    /// and `mercator::unproject` does no wrapping of its own — an unwrapped
    /// read hands back a longitude outside `[-180, 180)`, which
    /// `mercator::centre_on` turns into a `Frame` that compares unequal to
    /// the geometrically identical one. The row bound is Task 5's carried
    /// M3: `reclamp_window`'s `max_origin_row` is 0 whenever the plate is
    /// taller than the whole chart, which a wide enough terminal reaches at
    /// [`GLOBE_RUNG`], and a row past the chart unprojects to a latitude the
    /// projection never drew.
    pub fn geographic_point_under_cursor(&self) -> (f64, f64) {
        let (virtual_w, virtual_h) = plate::virtual_dims(self.window.depth);
        let row =
            (self.window.origin_row + u32::from(self.cursor.y)).min(virtual_h.saturating_sub(1));
        let col = (self.window.origin_col + u32::from(self.cursor.x)) % virtual_w;
        mercator::unproject(&self.frame, row, col, virtual_w, virtual_h)
    }

    /// Move the window — and, where the window cannot move, the cursor — so
    /// that `(lat, lon)` sits at the cursor's own screen position. The
    /// shared tail of [`Self::recentre`] and [`Self::apply_zoom`], and
    /// the whole mechanism behind The Quadrat's founding complaint: **the
    /// geographic point under the cursor does not move across a zoom step.**
    ///
    /// **TWO KNOBS, and both are needed.** Normally the WINDOW ORIGIN moves
    /// and the cursor holds still, which is what a reader expects: the
    /// picture slides under a steady pointer. Where the origin has nowhere
    /// left to go, the CURSOR moves instead and the guarantee is kept by the
    /// other hand. A test that only ever anchored in the middle of a chart
    /// would exercise one knob and never learn the second existed.
    ///
    /// **TWO LIMITS, DECLARED RATHER THAN HIDDEN** — decision 0142's own
    /// shape for a lost axis, applied to a kept one:
    ///
    /// 1. **Longitude holds exactly, always.** The chart wraps, so the
    ///    column arithmetic is a `rem_euclid` with no clamp in it and no
    ///    residue to spend. There is no east-west case in which this fails.
    /// 2. **Latitude holds except where Mercator's polar clamp binds.** Two
    ///    distinct ways it can bind. Where the desired origin row falls
    ///    outside `0..=max_origin_row` — the chart's top and bottom edges —
    ///    the residue is handed to the cursor and the point is still held,
    ///    exactly. Where the cursor would have to leave the plate to take
    ///    it, or where the point is past `mercator::LAT_CLAMP_DEG` and so
    ///    projects to no row at all, the point is on no chart this rung
    ///    draws and the guarantee genuinely fails. `mercator::clamp_caption`
    ///    is what already tells the reader that region exists; nothing here
    ///    invents a position for it.
    fn anchor_geographic_point(&mut self, lat: f64, lon: f64) {
        let (_, plate_h) = self.active_plate_dims();
        let (virtual_w, virtual_h) = plate::virtual_dims(self.window.depth);
        let Some((row, col)) = mercator::project(&self.frame, lat, lon, virtual_w, virtual_h)
        else {
            // Past the clamp: no row to anchor to, so the window stays put
            // rather than being sent somewhere invented.
            return;
        };

        // KNOB ONE, east-west: the origin absorbs the whole offset, because
        // longitude wraps. `virtual_dims` never returns a zero width, so the
        // modulus is always safe.
        self.window.origin_col =
            (i64::from(col) - i64::from(self.cursor.x)).rem_euclid(i64::from(virtual_w)) as u32;

        // KNOB ONE, north-south, as far as the clamp allows — then KNOB TWO
        // for the rest. `wanted - settled` is exactly what the origin
        // refused to travel, and adding it to the cursor puts the point back
        // under the cursor: `origin_row + cursor.y` comes to `row` either
        // way.
        let max_origin_row = i64::from(virtual_h)
            .saturating_sub(i64::from(plate_h))
            .max(0);
        let wanted = i64::from(row) - i64::from(self.cursor.y);
        let settled = wanted.clamp(0, max_origin_row);
        self.window.origin_row = settled as u32;
        let residue = wanted - settled;
        if residue != 0 {
            let max_y = i64::from(plate_h).saturating_sub(1).max(0);
            self.cursor.y = (i64::from(self.cursor.y) + residue).clamp(0, max_y) as u16;
        }
    }

    /// The world plate's current [`Window`] — which zoom level and virtual
    /// origin the plate is scrolled to. `pub` so a caller (a future task,
    /// or a test) can read the zoom ladder's state without reaching into
    /// private fields.
    pub fn window(&self) -> &Window {
        &self.window
    }

    /// The Mercator projection's current [`Frame`] — the world's own
    /// rotation-derived central line at `start`, rolled by
    /// [`Self::recentre`] on command thereafter.
    pub fn frame(&self) -> &Frame {
        &self.frame
    }

    /// Recompute `self.strip` for the current band and cursor position. See
    /// the module doc: only the walk band resolves, and it resolves the
    /// vertex the CURSOR points at, not the observer's own — every other band
    /// answers [`NOTHING_HERE_YET`] honestly.
    ///
    /// **Also advances [`Self::redraw_count`], F3's scroll driver.** Every
    /// call site here is a real client redraw of the strip (a cursor move,
    /// a zoom, a re-centre, a resize while the map is focused), so this is
    /// the one place both jobs belong together — a caller that recomputes
    /// the strip always also advances its scroll position by construction,
    /// rather than the two drifting out of step because some call site
    /// remembered one and not the other.
    fn refresh_strip(&mut self) {
        self.strip = Some(self.resolve());
        self.redraw_count = self.redraw_count.wrapping_add(1);
    }

    /// Advance the marquee by one tick. Called by the render loop when its
    /// input poll times out — never by an input handler, which is the
    /// whole point: the strip must scroll while the player does nothing.
    pub fn tick_marquee(&mut self) {
        self.marquee_ticks = self.marquee_ticks.wrapping_add(1);
    }

    /// Whether the strip currently has more text than plate width, i.e.
    /// whether there is anything for a tick to move.
    ///
    /// The loop blocks on input unless this is true, so an idle client
    /// with a strip that fits wakes for nothing at all — a marquee is not
    /// a reason to spin a terminal.
    pub fn strip_is_scrolling(&self) -> bool {
        let Some(text) = &self.strip else {
            return false;
        };
        let (plate_w, _) = self.active_plate_dims();
        text.chars().count() > usize::from(plate_w)
    }

    /// F3's own answer, in a number: the map strip's current scroll offset,
    /// a character count into `self.strip`'s own text — never a clock (see
    /// [`Self::redraw_count`]'s doc). `0` whenever the strip fits the
    /// active plate's width outright (nothing to scroll, and the common
    /// case); otherwise cycles forward through every valid starting column
    /// as [`Self::redraw_count`] advances, so a client that keeps
    /// redrawing (any key press, any resize) eventually shows the whole
    /// text a window at a time, and a client that never redraws again
    /// (there is no idle-time animation loop — see the module constraint)
    /// simply stops advancing, which is honest: nothing moved.
    pub fn strip_offset(&self) -> u16 {
        let Some(text) = &self.strip else {
            return 0;
        };
        let len = text.chars().count();
        let (plate_w, _) = self.active_plate_dims();
        let overflow = len.saturating_sub(usize::from(plate_w));
        if overflow == 0 {
            return 0;
        }
        // `overflow + 1` valid starting columns: `0..=overflow`.
        (self.marquee_ticks % (overflow as u32 + 1)) as u16
    }

    /// The strip text for the current turn.
    ///
    /// **With the world view on, this resolves against the world plate —
    /// Task 3a's review's other finding, closed here (Task 3b): the strip
    /// used to keep resolving the walk band's own chart even while the
    /// world plate was on screen, naming terrain that was not drawn.** See
    /// [`Self::resolve_world_view`]; no sight caption applies there (the
    /// world plate carries no observer-vision channel — that caption is
    /// specific to the walk band's chart).
    ///
    /// Otherwise: [`NOTHING_HERE_YET`] unless the current snapshot is a
    /// walk-band scene, in which case the vertex the cursor points at
    /// ([`Self::resolve_walk_band`]; see the module doc for the chain from
    /// a screen position to a `Vertex`) is resolved against the
    /// terrain-feature index, falling back to [`Self::resolve_world_view_at`]
    /// (the SAME fallback the world view itself uses) wherever
    /// `resolve_walk_band` finds no perceived facet under the cursor — the
    /// common case, per that method's own doc.
    ///
    /// **B4 (Task 5): the fallback branch also carries [`Self::site_note`]**
    /// — decision 0670's own gate, applied here exactly as Task 4 applied it
    /// to a chart mark's name. It is asked separately from [`Self::resolve_
    /// world_view_at`], not folded into it, because that method's own
    /// structural blindness to [`Self::sites`] is a pinned property (see its
    /// doc). **`UNNAMED_TERRAIN` is retired at this task**: since
    /// [`Self::resolve_world_view`] is total (`plate::terrain_at_tile` never
    /// fails), there is no longer a case where this method has nothing to
    /// say about the tile under the cursor.
    fn resolve(&self) -> String {
        let Ok(snap) = hornvale_game_core::Snapshot::parse(&self.cached) else {
            return NOTHING_HERE_YET.to_string();
        };
        // A BAND WITH NO PLATE OF OURS HAS NO RESOLVER OF OURS. The chamber
        // band draws `plan::draw`, not the raster (see
        // [`Self::raster_is_drawn`]), and spec §9 still defers the chamber
        // resolver — so resolving the tile under the cursor here would name
        // terrain that is not on screen, which is the exact "the strip
        // contradicts the picture" defect The Portolan part II's Task 3b
        // closed on the other side. Fix round 1's F5.
        let hornvale_game_core::Spatial::Walk { chart } = &snap.spatial else {
            return NOTHING_HERE_YET.to_string();
        };
        // The sight caption belongs to the walk band's own perception
        // channel, so it is read off the snapshot rather than inferred from
        // the rung.
        let sight = chart.sight.clone();
        // TWO RESOLVERS, MOST SPECIFIC FIRST. The perception overlay's own
        // facet under the cursor is the better answer where there is one —
        // it is the facet the picture actually drew there — and the tile the
        // raster painted answers everywhere else. `resolve_walk_band`
        // returns `None` off band B and on any tile the overlay left to the
        // raster, which is what makes the fallback the common case rather
        // than an error path.
        let base = match self.resolve_walk_band() {
            Some(text) => text,
            None => {
                // `resolve_world_view` (the chain plus the terrain readout)
                // and `site_note` (the decision-0670 gate) each resolve the
                // cursor's own tile independently rather than sharing one —
                // `resolve_world_view`'s signature is exercised directly by
                // several tests (`the_vertex_under_the_cursor_matches_the_
                // window_at_every_zoom_and_offset` among them), so it stays
                // a self-contained `&self -> Option<String>` query rather
                // than threading a pre-resolved tile through it.
                let mut text = self
                    .resolve_world_view()
                    .expect("Self::resolve_world_view is total since Task 5");
                if let Some(note) = self.site_note(&self.world_view_tile().facet) {
                    text.push_str(" — ");
                    text.push_str(&note);
                }
                text
            }
        };
        let text = self.world_view_caption(base);
        if self.at_walk_band_rung() {
            caption(text, sight.as_ref())
        } else {
            text
        }
    }

    /// Append §3.3's clamp/central-line caption, and F5's resolution
    /// disclosure when the active zoom covers more than one terrain vertex
    /// per character, to `base` (the resolved containment chain plus B4's
    /// terrain readout — see [`Self::resolve`]). Unlike the walk band's [`caption`], this runs
    /// UNCONDITIONALLY — the world view carries no sight channel to gate on
    /// ([`Self::resolve_world_view`]'s own doc), and both captions are true
    /// of the picture itself, independent of whether the cursor happens to
    /// sit on a named feature.
    fn world_view_caption(&self, base: String) -> String {
        let mut text = base;
        // THE RUNG LINE COMES FIRST, and the order is a measurement rather
        // than a preference (fix round 1, Important 3). The strip MARQUEES:
        // on the 80x24 floor the plate is 40 columns and a tick is 300 ms, so
        // a clause's position in this string is a delay before the reader can
        // read it. With the rung line emitted third it began at character 90
        // — the word "rung" first scrolled into view after ~15 s and the
        // clause was readable after ~44 s. That made the one disclosure that
        // answers "criteria I have not identified" the SLOWEST thing on the
        // strip to reach. `mercator::clamp_caption` is 69 characters, static,
        // and says the same thing at every rung and every cursor position, so
        // it is exactly what should be waited for instead.
        text.push_str(" — ");
        text.push_str(&self.rung_caption());
        text.push_str(" — ");
        text.push_str(&mercator::clamp_caption(&self.frame));
        if let Some(disclosure) = self.resolution_disclosure() {
            text.push_str(" — ");
            text.push_str(&disclosure);
        }
        text
    }

    /// **THE RUNG LINE**, and the half of Nathan's founding report that no
    /// amount of correct anchoring would have answered on its own: "the game
    /// map appears to zoom in and out based on criteria I have not
    /// identified". Nothing on screen ever said which rung was showing, so
    /// even a zoom that behaved perfectly was unreadable — the picture
    /// changed and the reader had no name for what it changed to.
    ///
    /// Two clauses, both DERIVED from [`plate::virtual_dims`] and the
    /// terrain's own vertex count, never tabulated:
    ///
    /// - **Where on the ladder.** The rung, and the ladder's own ends, so
    ///   a reader knows both where they are and how much further either
    ///   key goes. The chart's width in tiles is the scale: it is the
    ///   number of tiles around a great circle at this rung, so it doubles
    ///   as the reader zooms in and is the one number that makes two rungs
    ///   comparable without a unit this client has no way to obtain (the
    ///   world's radius is not on the wire).
    /// - **THE MIRROR OF [`Self::resolution_disclosure`]**, and the reason
    ///   this method carries it (Task 1's carried disclosure note). That
    ///   method is ONE-SIDED: it speaks only when a character stands for
    ///   MORE than one terrain vertex, and since Task 1 the whole shipped
    ///   ladder sits on the other side of 1:1 — even [`GLOBE_RUNG`]'s
    ///   363x362 chart carries about three tiles per vertex, and band B
    ///   carries some thirteen thousand. So the coarse-end disclosure is
    ///   silent everywhere and the map was left disclosing nothing at all,
    ///   which decision 0196's "a map may disclose its own resolution" is
    ///   poorly served by. This clause is that sentence written for the
    ///   oversampled end: how many characters share one terrain reading.
    ///   It also closes Task 6's own concern 2 — that some hundred band-B
    ///   characters share one grid-level vertex with nothing disclosing it
    ///   — because the two are the same fact counted the same way.
    ///
    /// The pair is deliberately not merged into one method. They are the
    /// two sides of one ratio, but they answer different questions ("the
    /// picture hides detail the field has" against "the picture claims
    /// detail the field lacks"), and `resolution_disclosure`'s own tests
    /// pin its silence as the honest answer on today's ladder.
    fn rung_caption(&self) -> String {
        let (virtual_w, virtual_h) = plate::virtual_dims(self.window.depth);
        let mut text = format!(
            "{RUNG_OPENING} {} of {GLOBE_RUNG}–{BAND_B_RUNG} — {virtual_w} tiles around the planet",
            self.window.depth
        );
        if let Some(share) = self.oversample_disclosure(virtual_w, virtual_h) {
            text.push_str(" — ");
            text.push_str(&share);
        }
        text
    }

    /// The oversampled half of the resolution disclosure: how many screen
    /// characters share ONE terrain reading, or `None` once a character no
    /// longer outruns the field. See [`Self::rung_caption`] for why this
    /// end of the ratio needed saying at all, and
    /// [`Self::resolution_disclosure`] for the other end.
    fn oversample_disclosure(&self, virtual_w: u32, virtual_h: u32) -> Option<String> {
        let terrain_vertices = self.geo.vertex_count() as u64;
        if terrain_vertices == 0 {
            return None;
        }
        let virtual_tiles = u64::from(virtual_w) * u64::from(virtual_h);
        let share = virtual_tiles as f64 / terrain_vertices as f64;
        if share <= 1.0 {
            return None;
        }
        Some(format!(
            "about {} characters to one terrain reading",
            share.round() as u64
        ))
    }

    /// F5's resolution disclosure (decision 0123, "disclose a resolution
    /// rather than refine a field", applied here to a lost SAMPLE rather
    /// than a lost axis — decision 0142's own rule for a lost axis is the
    /// same shape one level up): at any zoom where one screen character
    /// stands for more than one real terrain vertex, the strip says so,
    /// rather than reporting with the exact same confident phrasing it
    /// uses once the mesh's own resolution is reached.
    ///
    /// **Derived, never hardcoded.** `plate::virtual_dims` gives the
    /// virtual chart's own cell count at the active zoom; the terrain's
    /// own vertex count ([`hornvale_kernel::Geosphere::vertex_count`]) divided
    /// by it is the mean number of real terrain vertices behind one screen
    /// character — never a hand-picked ratio. `None` once that mean is
    /// `<= 1` (one character names at most one vertex, on average).
    ///
    /// **Since The Quadrat this returns `None` at every SHIPPED rung, and
    /// that is the honest answer rather than a regression.** The chart no
    /// longer shrinks to the plate: [`GLOBE_RUNG`], the coarsest rung the
    /// ladder reaches, is the canonical grid level itself (decision 0196) —
    /// 363x362 tiles for 40,962 vertices — so one character never stands
    /// for more than one vertex anywhere on the ladder, and there is
    /// nothing to disclose. The method is kept, not deleted: it is the
    /// derivation, and a future rung coarser than the mesh would make it
    /// speak again. The strip's own rung line is Task 7's.
    fn resolution_disclosure(&self) -> Option<String> {
        let (virtual_w, virtual_h) = plate::virtual_dims(self.window.depth);
        let virtual_cells = u64::from(virtual_w) * u64::from(virtual_h);
        if virtual_cells == 0 {
            return None;
        }
        let terrain_vertices = self.geo.vertex_count() as u64;
        let ratio = terrain_vertices as f64 / virtual_cells as f64;
        if ratio <= 1.0 {
            return None;
        }
        Some(format!(
            "one character stands for roughly {} terrain cells at this zoom",
            ratio.round() as u64
        ))
    }

    /// The world view's own resolved [`plate::TileTerrain`] at the
    /// cursor's current screen position — [`plate::terrain_at_tile`], the
    /// SAME per-tile mesh addressing [`plate::draw_with`] itself paints
    /// from (one source of truth; see that function's own doc), asked for
    /// the painted class's own representative tile.
    ///
    /// **Fix round 1, Finding 2 (the reviewer's own framing, which
    /// improved on this task's first pass): the earlier single-centre-
    /// point resolver was not a contract violation — it named a genuinely
    /// resolved feature — but it let the strip contradict the picture.**
    /// F5's original measurement (52.5% agreement, 420/800, between that
    /// resolver and the drawn glyph at the coarsest zoom) is **RETRACTED
    /// AS MEASURED**: it was taken on a grid rendered at 32x16 against
    /// coordinates computed for 40x20, a double-fit bug in the test
    /// harness, not a property of the resolver — the magnitude is
    /// UNMEASURED, not smaller. The kind of disagreement it pointed at is
    /// still real: a player could point at a character drawn `~` and have
    /// the strip name a real feature on a LAND vertex nearest that
    /// character's exact centre. This method closes that: it can never
    /// return a tile whose class
    /// disagrees with what [`plate::draw_with`] would paint at the same
    /// screen position, because it asks the identical question for the
    /// identical answer, and picks a REPRESENTATIVE of the painted class.
    /// [`plate::TileTerrain`]'s own doc states that invariant: `ocean` and
    /// `vertex` cannot contradict each other, because the vertex is chosen
    /// from the corners the class was decided on.
    ///
    /// **The Quadrat, Task 3 replaced the 49-point vote with mesh
    /// addressing and the invariant survived unchanged** — what moved is
    /// how the class is decided, not the guarantee that the strip agrees
    /// with the picture.
    ///
    /// The [`hornvale_kernel::RoomMeshMemo`] is local because this resolves
    /// exactly ONE tile per keypress: a session-lived memo would save at
    /// most the three `nearest_to_position` scans of a repeated cursor
    /// position, against holding cross-call state for a pure function.
    ///
    /// **Stays `&self` (fix round 1, Fix 1: reverted from an earlier `&mut
    /// self`).** Before Task 5 this method returned only `.vertex` and
    /// discarded the rest of the struct — a context here "would buy a
    /// discarded reflectance per keypress", and that argument is
    /// STRUCTURAL, not until-Task-6: this
    /// call site can never populate the `(FacetId, season)` cache and can
    /// never read a hit from it, so threading [`Self::reflectance_cache`]
    /// through by `&mut` here bought a false appearance of use at the cost
    /// of making three read-only queries (`resolve`/`resolve_world_view`/
    /// this one) advertise mutation to every future caller.
    /// `season`/`reflectance_cache` below stay `0`/`None` for that reason —
    /// and note that this is now the ONLY `terrain_at_tile` call in the
    /// shipped tree that passes `ctx: None` deliberately:
    /// [`plate::draw_terrain_layer`]'s call was the other one and The
    /// Wash's Task 6 flipped it. See [`Self::reflectance_cache`]'s own doc
    /// for where the field's real readers are.
    ///
    /// **Widened from a bare `Vertex` at Task 5 (B4).** The tile readout
    /// (`Self::terrain_readout`) needs `water`/`band`/`height_asl`, and the
    /// site note (`Self::site_note`) needs `facet` — three more fields off
    /// the exact tile this call already resolves, so [`Self::resolve`] and
    /// [`Self::resolve_world_view_at`] share the one struct rather than
    /// each re-resolving the cursor's screen position.
    fn world_view_tile(&self) -> plate::TileTerrain {
        let (virtual_w, virtual_h) = plate::virtual_dims(self.window.depth);
        plate::terrain_at_tile(
            &self.terrain,
            &self.geo,
            &self.nearest,
            &mut hornvale_kernel::RoomMeshMemo::default(),
            &self.frame,
            &self.window,
            virtual_w,
            virtual_h,
            u32::from(self.cursor.y),
            u32::from(self.cursor.x),
            // This reads `.vertex`/`.water`/`.band`/`.height_asl`/`.facet`
            // and nothing else, so a context here would buy a discarded
            // reflectance per keypress.
            None,
            hornvale_kernel::WorldTime::GENESIS,
            0,
            None,
        )
    }

    /// **B4's terrain readout** (spec §4.4): the tile's own water class,
    /// relief band and height above sea level, in prose — independent of
    /// whether any gazetteer feature is named there at all. Before this
    /// task the strip's standing content was 13.3% tile / 86.7% map (188
    /// characters measured at the flagship, 25 about the tile); this is
    /// the fill for that gap, built entirely from fields already carried
    /// on [`plate::TileTerrain`] (`plate.rs:2170-2233`) rather than a new
    /// derivation.
    ///
    /// `water`/`band` are indices into the SAME legends the renderer
    /// itself classifies by (`hornvale_terrain::WaterKind::LEGEND`,
    /// `hornvale_scene::RELIEF_LEGEND`), so this can never name a class the
    /// picture disagrees with. `height_asl` is rounded to the nearest
    /// metre for prose — the continuous reading is what
    /// [`plate::TileTerrain::height_asl`]'s own doc says a sub-band
    /// consumer would want, and a marquee line is not that consumer.
    fn terrain_readout(tile: &plate::TileTerrain) -> String {
        let water = hornvale_terrain::WaterKind::LEGEND[usize::from(tile.water)];
        let relief = hornvale_scene::RELIEF_LEGEND[tile.band as usize];
        let height = tile.height_asl.get().round() as i64;
        format!("{water}, {relief}, {height} m above sea level")
    }

    /// The world view's own resolution chain plus B4's terrain readout, at
    /// an ALREADY-RESOLVED `tile` — the shared body [`Self::resolve_world_
    /// view`] and [`Self::resolve`] both build on, so the tile is paid for
    /// once per keypress rather than once per consumer.
    ///
    /// **Deliberately blind to [`Self::sites`]**, and that is a pinned
    /// property, not an oversight: `site_drawing_never_depends_on_
    /// discovery_and_the_cursor_never_leaks_a_name` proves the cursor
    /// readout is a pure function of `self.index` and the cursor position,
    /// structurally unable to read `self.sites` at all. A placed site's
    /// mention belongs to [`Self::site_note`], called separately by
    /// [`Self::resolve`] — never folded in here, or that proof would stop
    /// being true.
    fn resolve_world_view_at(&self, tile: &plate::TileTerrain) -> String {
        let (species, ph, morph) = &self.namer;
        let chain = resolve_chain_at(
            &self.index,
            tile.vertex,
            self.seed,
            species,
            ph,
            morph,
            &|id| self.discovered.contains(FeatureId::Extent(id)),
        );
        let readout = Self::terrain_readout(tile);
        match chain {
            Some(chain) => format!("{chain} — {readout}"),
            None => readout,
        }
    }

    /// The world view's own resolution chain plus B4's terrain readout, at
    /// the cursor's current screen position — [`Self::world_view_tile`]
    /// widened to the FULL containment chain there (Task 4, Step 1)
    /// plus [`Self::terrain_readout`] (Task 5, B4). Every feature at the
    /// resolved vertex, most specific first, each with its class named in
    /// prose (design spec §5), followed by the tile's own water/relief/
    /// height reading.
    ///
    /// **Always `Some` now** (Task 5): [`plate::terrain_at_tile`] is total,
    /// so there is always a terrain readout to report even where the
    /// containment chain is empty — the `None` this used to return for an
    /// unfeatured vertex is gone, and with it the whole reason `Self::resolve`
    /// ever needed a literal `"unnamed terrain"` fallback string (retired
    /// at this task; see [`Self::resolve`]'s own doc).
    fn resolve_world_view(&self) -> Option<String> {
        Some(self.resolve_world_view_at(&self.world_view_tile()))
    }

    /// **Decision 0670's gate, applied to the world view's tile readout**
    /// (Task 5, B4) — exactly the rule Task 4 applied to a chart mark's
    /// proper name: a placed site's glyph draws whether or not it has been
    /// discovered, but a mention of it in text does not. `MapSite` carries
    /// no proper name at all (spec §7 non-goal), so there is no name for
    /// this gate to redact; what it withholds is the MENTION itself — even
    /// naming the site's KIND before encounter would tell a reader
    /// something the glyph alone does not.
    ///
    /// `None` when nothing placed stands on `facet`, or when something does
    /// but has not yet been discovered — the two cases the strip must not
    /// tell apart, or a reader could infer "something is here" from the
    /// readout falling silent versus not.
    ///
    /// **Structurally live only at band B's own rung, and that is
    /// correct, not a gap.** `facet` is always [`Self::world_view_tile`]'s
    /// `.facet` — a facet at the ACTIVE window's own depth — while
    /// `MapSite::placed` is addressed at walk-band granularity
    /// (`hornvale_worldgen::site_facet_for`'s own doc). The two can only
    /// ever agree where the active depth IS [`BAND_B_RUNG`], which
    /// [`Self::resolve`] reaches whenever [`Self::resolve_walk_band`]
    /// finds no perceived facet at BAND_B_RUNG itself — never at a coarser
    /// world-view rung, where a screen character already spans many real
    /// facets ([`Self::oversample_disclosure`]) and a specific site mention
    /// would be a false precision the picture does not have. Task 4's own
    /// chart-mark gate has the identical shape: `ChartMarks::update` only
    /// ever reads a WALK-band chart, never a coarser one.
    fn site_note(&self, facet: &Facet) -> Option<String> {
        plate::sites_standing_in(&self.sites, facet)
            .into_iter()
            .find(|id| self.discovered.contains(*id))
            .map(|id| match id {
                FeatureId::Settlement(_) => "a settlement stands here".to_string(),
                FeatureId::Cave(_) => "a cave mouth stands here".to_string(),
                FeatureId::Exotic(_) => "something unusual stands here".to_string(),
                FeatureId::Extent(_) => {
                    unreachable!("plate::sites_standing_in never yields a FeatureId::Extent")
                }
            })
    }

    /// The walk band's own resolution chain, cursor position to the full
    /// containment chain (Task 4, Step 1) — every feature at the resolved
    /// vertex, most specific first. `None` means "no perception facet was
    /// drawn under the cursor", which after Task 6 is the COMMON case rather
    /// than an error path (the packet is 31 facets on a plate of hundreds),
    /// and the caller falls through to the tile the raster painted; `None`
    /// also covers the honest absences it always did — the room address
    /// does not unpack, or the terrain index has no feature at the resolved
    /// vertex. Never [`NOTHING_HERE_YET`] (that string is reserved for a
    /// band with no resolver at all, which this is not).
    ///
    /// **RETARGETED, not rewritten (The Quadrat, Task 6).** Its "which facet
    /// is under the cursor" step used to be `core`'s own chart lookup —
    /// that module's polar
    /// projection onto a fixed [`hornvale_game_core::spread::PLATE_WIDTH`]
    /// plate. Band B draws the raster now, and the perception facets sit
    /// where [`plate::draw_perception_layer`] put them, so asking `core`
    /// would name a facet from a picture nobody drew: exactly the
    /// "strip contradicts the picture" defect Task 3b closed on the other
    /// side. Everything below that step — `room` to [`FacetId::unpack`] to
    /// [`hornvale_kernel::Facet::coord`] to the nearest-vertex lookup to
    /// `resolve_chain_at` — is unchanged. The module doc now describes that
    /// chain WITHOUT the `chart::cell_at` step, so the cross-reference is
    /// whole again rather than half-true (fix round 1, F4).
    fn resolve_walk_band(&self) -> Option<String> {
        if !self.at_walk_band_rung() {
            return None;
        }
        // The real scene — the SAME cached `Self::walk_scene` packet
        // `compose_perception_layer` draws from (both read it through
        // `Self::walk_band_scene`), derived once per turn by the same call
        // `Session::snapshot` itself makes for the walk band (`purview(0)`,
        // `windows/vessel/src/session.rs`'s own comment on that call site).
        // Sharing the one cached value, rather than each independently
        // re-deriving it, is what keeps the resolver and the picture from
        // disagreeing about which facets exist (The Gallery, Task 10 round
        // 2 — previously true only because both calls were deterministic
        // and made within the same redraw; now true by construction).
        let scene = self.walk_band_scene()?;
        let perceived = perceived_facets(&scene);
        let (plate_w, plate_h) = self.active_plate_dims();
        let index = plate::perceived_at(
            &self.frame,
            &self.window,
            u32::from(plate_w),
            u32::from(plate_h),
            &perceived,
            u32::from(self.cursor.y),
            u32::from(self.cursor.x),
        )?;
        let real_cell = scene.cells.get(index)?;
        let room = FacetId(real_cell.room).unpack().ok()?;
        let coord = room.coord();
        let vertex_id = self
            .nearest
            .nearest(&self.geo, coord.latitude, coord.longitude);
        let (species, ph, morph) = &self.namer;
        resolve_chain_at(
            &self.index,
            vertex_id,
            self.seed,
            species,
            ph,
            morph,
            &|id| self.discovered.contains(FeatureId::Extent(id)),
        )
    }

    /// The live walk-band perception packet, or `None` when the possession
    /// is not on the walk band at all.
    ///
    /// **Round 1 (The Gallery, Task 10; The Quadrat's F11) retired the
    /// per-redraw `Snapshot::parse` this method's callers used to pay,
    /// reading [`Self::on_walk_band`] instead of the snapshot's own
    /// `spatial` tag.** That measured a real but modest win (~5%), because
    /// the parse was never the dominant cost: `Session::purview(0)` was, and
    /// round 1 left it standing here, called fresh on every redraw.
    ///
    /// **Round 2 caches the packet itself, in [`Self::walk_scene`], for the
    /// identical argument `on_walk_band`'s own doc already makes for the
    /// band question.** `purview` is a pure read (`&self`, no mutation) over
    /// session state — position, knowledge, eyes, day — that changes only
    /// when a turn does; a keypress that is only typing changes none of
    /// them. [`Self::refresh`] is the one choke point that already
    /// recomputes `on_walk_band` from that same fact once per turn, so it is
    /// where `walk_scene` is recomputed too. This method is now a plain
    /// accessor: it makes no decision the cache has not already made,
    /// including the `None` case for a non-walk band, which `refresh` sets
    /// explicitly rather than leaving this method to re-derive it.
    ///
    /// **Why `on_walk_band`, not `purview`'s own success, decided the band
    /// (still true, now decided once per turn instead of once per read).**
    /// `Session::purview(0)` charts the session's walk-band position and
    /// succeeds indoors too — it is the surface around the structure you are
    /// standing in — so using it as the band test would cache a perception
    /// overlay of outdoor facets for a chamber-band spread. It stays correct
    /// with three bands: `on_walk_band` is `Walk`-or-not, and underground is
    /// not walk, the same distinction the retired (round 1) match made
    /// explicit in its `Chamber | Underground => None` arm.
    ///
    /// **Whether the underground band ever gets its own perception overlay
    /// is still an open question, unresolved by either round** — the
    /// original match's own comment raised it and this doc carries the
    /// pointer forward rather than letting the question disappear with the
    /// arm that used to name it.
    fn walk_band_scene(&self) -> Option<hornvale_scene::SurroundsScene> {
        self.walk_scene.clone()
    }

    /// Compose the band-B perception overlay onto an already-drawn plate —
    /// the third layer (`plate::draw_perception_layer`), and the answer to
    /// the Task 4 follow-up that recorded this layer as having no input:
    /// its input is [`Self::walk_band_scene`], which nothing in `plate.rs`
    /// had a reason to touch until band B joined the raster ladder.
    ///
    /// A no-op off band B and off the walk band. Off band B because the
    /// packet's 31 facets are finer than a coarse rung's own tile — every
    /// one of them would collapse onto the observer's single tile and the
    /// overlay would claim to place facets it had merged — and off the walk
    /// band because there is no packet to draw.
    fn compose_perception_layer(&self, dst: &mut hornvale_game_core::Grid) {
        let Some(scene) = self.walk_band_scene() else {
            return;
        };
        let mut perceived = perceived_facets(&scene);
        // OFF BAND B, THE OBSERVER ALONE (The Hachure, Stage 0).
        //
        // This gate used to be an early `return` on `!at_walk_band_rung()`,
        // and Stage 0 turned that into a defect the moment the map stopped
        // opening on band B: the opening view had no `@` on it, which is the
        // exact state `enter_map`'s own comment calls unshippable ("the
        // reader cannot even see which direction home is").
        //
        // The split is by WHAT each half claims, not by how many boxes land.
        // At the entry rung one tile spans tens of kilometres, so the whole
        // 31-facet band collapses onto one tile; painting the band's MARKS
        // there would assert positions the rung cannot resolve, while
        // painting the OBSERVER asserts only "you are in this tile", which is
        // true at every rung. `draw_perception_layer` is already
        // rung-agnostic — it reads `virtual_dims(win.depth)` — so the filter
        // is the whole change.
        if !self.at_walk_band_rung() {
            perceived.retain(|p| p.here);
        }
        plate::draw_perception_layer(
            dst,
            &self.frame,
            &self.window,
            plate::colour_allowed(),
            &perceived,
        );
    }

    /// Scroll band B's window so the observer's own facet sits at the middle
    /// of the plate a redraw would draw. A no-op at every other rung.
    ///
    /// **The ruling this implements (Task 6), and its deliberate
    /// narrowness.** Band B's window origin is `(0, 0)` at `start`, which at
    /// [`BAND_B_RUNG`] is some eleven thousand rows and two thousand columns
    /// from the observer — the arctic corner of a 23,245-wide chart. A
    /// band-B view showing arctic ocean while you stand in a rainforest is
    /// not shippable, and the walk view has always had you in the middle of
    /// it, so band B follows the observer. Where entering a COARSE rung
    /// lands, and cursor-anchored zoom, are Task 7's and are not decided
    /// here.
    ///
    /// **Called on ARRIVAL, never per redraw.** [`Self::enter_map`] and
    /// [`Self::resize`] are the two moments the window could be somewhere
    /// the observer is not; a redraw is not one of them. Re-centring per
    /// redraw would make band B unscrollable — every arrow key would snap
    /// the window back — and the player genuinely can scroll here: the
    /// observer cannot MOVE while [`Focus::Map`] is focused (the arrows
    /// drive the cursor there), so the window can only go stale across one
    /// of those two arrivals.
    ///
    /// **[`Self::apply_zoom`] WAS the third arrival and is not one any
    /// more** (The Quadrat, Task 7), which is a decision this method's own
    /// doc handed forward: "Where entering a COARSE rung lands, and
    /// cursor-anchored zoom, are Task 7's and are not decided here." They
    /// turned out to be the same question. A zoom no longer leaves the
    /// window anywhere arbitrary — it anchors on the point under the cursor
    /// ([`Self::anchor_geographic_point`]) — so there is nothing left for
    /// this to rescue there, and snapping the reader back to the observer
    /// after they had deliberately scrolled somewhere and pressed `+` would
    /// BE the founding complaint: a map that moves for a reason the reader
    /// did not give. The same anchor closes Task 2's carried finding at the
    /// other end of the ladder — a zoom-out from band B used to land on
    /// rung 11 at an arbitrary origin, and now lands where the reader was
    /// looking. Entering the map from a keypress still centres here,
    /// because at `start` the window really is at the corner and the cursor
    /// has no point of the reader's own to preserve.
    fn centre_band_b_on_the_observer(&mut self) {
        if !self.at_walk_band_rung() {
            return;
        }
        self.centre_on_the_observer();
    }

    /// Scroll the window so the observer's own facet sits at the middle of
    /// the plate a redraw would draw, AT WHATEVER RUNG IS SHOWING — the
    /// rung-gated [`Self::centre_band_b_on_the_observer`]'s body, lifted out
    /// so [`Self::enter_map`] can reach it everywhere (fix round 1,
    /// Important 2).
    ///
    /// The gate is the caller's, and the two callers want different ones,
    /// which is why this carries neither. `enter_map` is an ARRIVAL at the
    /// map and centres at every rung — it is the reader's signposted way
    /// home, and off band B it is the ONLY one, because no observer marker
    /// is drawn there. `resize` keeps the rung gate: a resize is not an
    /// arrival, and re-centring a coarse rung the reader had deliberately
    /// scrolled somewhere would be a map moving for a reason the reader did
    /// not give — the founding complaint, in the shape this task exists to
    /// remove.
    fn centre_on_the_observer(&mut self) {
        let (plate_w, plate_h) = Self::world_plate_dims(self.term_w, self.term_h);
        let coord = self.session.position().coord();
        // Above the projection's polar clamp the observer is on no chart at
        // all, so there is nothing to centre on and the window stays where
        // it is — `mercator::clamp_caption` already tells the reader what
        // falls off the map. Inventing a position would be worse.
        let _ = self.centre_window_on(coord.latitude, coord.longitude, plate_w, plate_h);
    }

    /// Scroll the window so the geographic point `(lat, lon)` sits at the
    /// middle of a `w`x`h` plate. `None` when the point is above the
    /// projection's polar clamp and so is on no chart at all.
    ///
    /// **Promoted from a test helper, WITH the size guard Task 1's review
    /// flagged as the condition of promoting it.** The helper computed the
    /// column as `(col + virtual_w - w / 2) % virtual_w` in `u32`, which
    /// UNDERFLOWS — a debug panic, a wrong column in release — whenever
    /// `w / 2` exceeds `col + virtual_w`. That is unreachable on the shipped
    /// ladder ([`GLOBE_RUNG`]'s chart is 363 tiles wide) but reachable at a
    /// rung coarser than the mesh, which [`plate::virtual_dims`] honours
    /// deliberately and this module's own disclosure test constructs. The
    /// wrap is done in `i64` with `rem_euclid` here — the identical
    /// arithmetic [`Self::move_cursor`]'s scroll spill already uses for the
    /// identical wrap, rather than a second spelling of it — and
    /// `virtual_dims` never returns a zero width, so the modulus is always
    /// safe.
    ///
    /// LONGITUDE WRAPS, LATITUDE CLAMPS — spec §4.2's own asymmetry, the
    /// same one [`Self::move_cursor`] and `plate::draw_feature_layer` obey.
    fn centre_window_on(&mut self, lat: f64, lon: f64, w: u16, h: u16) -> Option<()> {
        let (virtual_w, virtual_h) = plate::virtual_dims(self.window.depth);
        let (row, col) = mercator::project(&self.frame, lat, lon, virtual_w, virtual_h)?;
        let max_origin_row = i64::from(virtual_h).saturating_sub(i64::from(h)).max(0);
        self.window.origin_row =
            (i64::from(row) - i64::from(h) / 2).clamp(0, max_origin_row) as u32;
        self.window.origin_col =
            (i64::from(col) - i64::from(w) / 2).rem_euclid(i64::from(virtual_w)) as u32;
        Some(())
    }

    /// The current turn's `vessel/session/v2` JSON — what `hornvale-game-
    /// core::render` consumes. Does not advance the turn.
    pub fn snapshot(&self) -> String {
        self.cached.clone()
    }

    /// Hand one line to the session, refresh the cached snapshot, and
    /// report whether the possession RELEASED.
    ///
    /// The client never validates a move before sending it: `Session::
    /// handle` tokenizes and parses, and an illegal or unrecognised verb
    /// comes back as the sim's own prose (e.g. "No way n from here."),
    /// already folded into the returned snapshot's `narration.prose` — a
    /// caller that only wants to display the reply can read [`Self::
    /// snapshot`] afterward and never needs the bool. What it cannot get
    /// from the snapshot is whether the possession is OVER: `Turn::Out` and
    /// `Turn::Released` both carry ordinary prose, so this is the one place
    /// that still inspects the [`Turn`] itself, and it does so to answer
    /// exactly that question (ledger #8 — see [`Self::apply`]'s doc).
    pub fn handle(&mut self, line: &str) -> bool {
        let turn = self.session.handle(line);
        self.refresh();
        matches!(turn, Turn::Released(_))
    }

    /// The command buffer's current text — what has been typed but not yet
    /// submitted.
    pub fn line_text(&self) -> String {
        self.line.text()
    }

    /// The command buffer's caret, as a character offset — see
    /// [`crate::line::Line::caret`].
    pub fn caret(&self) -> usize {
        self.line.caret()
    }

    /// The most recently submitted line, echoed for the record (see the
    /// `echo` field's own doc for what "most recent" means across turns).
    pub fn echo(&self) -> Option<&str> {
        self.echo.as_deref()
    }

    /// The pending completion ambiguity under [`TabStyle::Hint`], as an
    /// owned `(stem, matches)` pair for the frame builder to hand the core
    /// renderer ([`hornvale_game_core::entry::Hint`] borrows, so the caller
    /// rebuilds the borrowed view from these strings at the call site).
    /// `None` whenever no ambiguity is pending — cleared by any edit or
    /// submission, matching the field's own discipline.
    pub fn hint_parts(&self) -> Option<(String, Vec<String>)> {
        self.hint
            .as_ref()
            .map(|h| (h.stem.clone(), h.matches.clone()))
    }

    /// The discovery predicate [`Self::refresh`] feeds
    /// [`hornvale_game_core::ChartMarks::update`] (The Newel, Task 4;
    /// decision 0670). A free associated function rather than an inline
    /// closure so a test can drive it directly against a known room,
    /// without needing that room's own settlement or cave to actually
    /// appear on a live chart within walking distance of wherever a test
    /// driver happens to start.
    ///
    /// Resolves the same chain [`Self::resolve_walk_band`] resolves a
    /// cursor position with: a packed room id, [`FacetId::unpack`] to a
    /// [`Facet`], [`Facet::coord`] to a lat/lon,
    /// [`NearestVertexIndex::nearest`] to a [`hornvale_kernel::Vertex`] —
    /// and then the same `discovered` ledger [`Self::update_discovery`]
    /// writes. `kind` is only ever `"settlement"` or `"cave"`
    /// (`ChartMarks::update`'s own doc: an `"agent"` mark never reaches
    /// this function at all, because it is not a placed site and decision
    /// 0670 never asked to gate it). An unpackable room id gates closed
    /// (`false`) rather than panicking — the same defensive posture
    /// `Self::resolve_walk_band`'s own `FacetId::unpack().ok()?` takes.
    fn site_is_discovered(
        geo: &hornvale_kernel::Geosphere,
        nearest: &NearestVertexIndex,
        discovered: &Discovered,
        kind: &str,
        room: u64,
    ) -> bool {
        let Ok(facet) = FacetId(room).unpack() else {
            return false;
        };
        let coord = facet.coord();
        let vertex = nearest.nearest(geo, coord.latitude, coord.longitude);
        let feature = if kind == "settlement" {
            FeatureId::Settlement(vertex)
        } else {
            FeatureId::Cave(vertex)
        };
        discovered.contains(feature)
    }

    /// Re-derive `cached` from the live session. A snapshot read can fail
    /// only when the session itself is not live, which cannot happen
    /// between `start` succeeding and `Drop` running — so a failure here
    /// clears the cache rather than panicking, the same defensive posture
    /// `clients/vessel/wasm`'s `set_snapshot` takes.
    fn refresh(&mut self) {
        self.cached = self
            .session
            .snapshot()
            .map(|snap| snapshot_json(&snap))
            .unwrap_or_default();
        // The completion scope rides every refresh: parse (which can fail
        // only as `refresh`'s own doc describes — a dead session) and
        // replace both scopes from the new snapshot, then re-fold them into
        // `self.scope`. On failure every stale value stands rather than
        // being cleared: stale candidates complete nothing harmful, and an
        // empty one mid-session would be a regression masquerading as
        // caution.
        if let Ok(snap) = hornvale_game_core::Snapshot::parse(&self.cached) {
            self.current_turn_scope.update(&snap.narration);
            // The chart scope's own discovery gate (decision 0670: a
            // placed site's glyph draws ungated, its proper name does
            // not). Resolved the same way `Self::resolve_walk_band`
            // resolves a cursor position — a packed room id,
            // `FacetId::unpack` to a `Facet`, `Facet::coord` to a
            // lat/lon, `NearestVertexIndex::nearest` to a `Vertex` — and
            // then the same `self.discovered` ledger `update_discovery`
            // writes. `ChartMarks::update`'s own doc: an `"agent"` mark
            // never reaches this closure at all, because it is not a
            // placed site and decision 0670 never asked to gate it.
            let geo = &self.geo;
            let nearest = &self.nearest;
            let discovered = &self.discovered;
            self.chart_scope.update(&snap.spatial, |kind, room| {
                Self::site_is_discovered(geo, nearest, discovered, kind, room)
            });
            self.scope = Lexicon::new(vec![
                Box::new(self.current_turn_scope.clone()),
                Box::new(self.chart_scope.clone()),
            ]);
            // The band, from the same parse (see `on_walk_band`'s own doc for
            // why it is cached and not re-derived per keypress). On a parse
            // failure the previous value stands, the same posture the scope
            // above takes: a dead session is not a band change.
            self.on_walk_band = matches!(snap.spatial, hornvale_game_core::Spatial::Walk { .. });
            // The walk-band perception packet itself, cached alongside the
            // band question and invalidated at the same point (see
            // `Self::walk_scene`'s own doc for why: round 1 of this fix
            // cached the band and left `purview(0)` itself standing as a
            // per-redraw cost, and measurement showed `purview` -- not the
            // parse -- was the dominant one). `purview` is a pure read over
            // session state that changes only on a turn, so it is safe to
            // pay here, once, rather than at every caller. On a parse
            // failure the previous packet stands too, for the identical
            // reason `on_walk_band` does.
            self.walk_scene = if self.on_walk_band {
                self.session.purview(0).ok()
            } else {
                None
            };
        }
        self.update_discovery();
        self.follow_the_walker();
    }

    /// Keep the walk view centred on the observer as they MOVE — called from
    /// [`Self::refresh`], which is every turn and nothing else.
    ///
    /// **This restores a property the picture it replaced had for free, and
    /// which Task 9 removed without noticing** (fix round 1). `chart::draw`
    /// anchors the observer to the plate's centre by construction, so the
    /// walk view had always had the player in the middle of it. The raster
    /// is drawn through a STORED window instead, and
    /// [`Self::centre_band_b_on_the_observer`] was called only on arrival at
    /// the map and on resize — neither of which is a step. Measured at
    /// 200x50 before this existed: the observer drifted about one plate row
    /// per 1.6 steps, from row 23 to row 18 in eight `go n`s, and would have
    /// walked clean off a 46-row plate in under forty. Arrow keys ARE the
    /// walking gesture (`input::action_for` gives `Focus::Walk` `Move`, not
    /// `CursorBy`), so this is the most ordinary thing a player does.
    ///
    /// **It does not contradict "called on ARRIVAL, never per redraw".**
    /// That ruling is [`Self::centre_band_b_on_the_observer`]'s and its
    /// stated premise is exact: re-centring per REDRAW would make band B
    /// unscrollable, *"the player genuinely can scroll here: the observer
    /// cannot MOVE while [`Focus::Map`] is focused"*. Both halves of that
    /// premise are about the map. A turn is not a redraw — it is the one
    /// event that moves the observer — and `Focus::Map` has no `Submit` and
    /// no `Move` at all (`input::action_for`), so no turn can occur while a
    /// reader is scrolled somewhere deliberately. The focus test below is
    /// therefore belt and braces rather than the load-bearing part, and it
    /// is kept because it states which view this is for.
    fn follow_the_walker(&mut self) {
        if self.focus == Focus::Map {
            return;
        }
        self.centre_band_b_on_the_observer();
    }

    /// Update [`Self::visited`] and [`Self::discovered`] from the
    /// possession's CURRENT state — called from [`Self::refresh`], the one
    /// choke point that already updates `cached` on `start` and on every
    /// `handle`. Two independent updates, kept structurally apart per the
    /// `discovery` module's own doc:
    ///
    /// - **Visited (§A4a)**: record the possession's own walk-band room.
    ///   Reading `session.position()` here is licensed — `Driver` is
    ///   documented as "the one place in `hornvale-game` allowed to know
    ///   `Session`, `Body`, or `WorldContext` exist" (the module doc); what
    ///   this method never does is let a `Facet`/`Body` VALUE escape
    ///   `Driver` itself — `visited`/`discovered` are plain fields this
    ///   struct owns, queried only through [`Self::visited`]/
    ///   [`Self::discovered`]'s own `bool`/reference-returning accessors.
    /// - **Discovered, extent features (§A4b)**: every feature whose
    ///   extent contains the possession's CURRENT terrain vertex is
    ///   discovered, by definition ("standing on a volcano IS meeting
    ///   it"). [`VertexFeatureIndex::at`] already returns every such feature
    ///   at that vertex, most-specific-first; ALL of them are recorded, not
    ///   only the first, so standing on a volcano inside a landmass
    ///   discovers both in the same step.
    /// - **Discovered, point sites (§A4b, F9)**: a settlement is
    ///   discovered when `enter` succeeds — read off the wire's own
    ///   `band` tag (`hornvale_game_core::Spatial::Chamber`, a PLAIN
    ///   schema type this crate already depends on, never a
    ///   `Session`/`Knowledge` value) rather than any private `Session`
    ///   field, which does not exist to read. A cave is discovered when
    ///   `delve` succeeds — [`DELVE_SUCCESS_PREFIX`]'s own doc explains why
    ///   the turn's own narration text is the only signal available for
    ///   it. Both checks read `self.cached`, the same plain JSON string
    ///   every other client read already uses.
    /// - **Discovered, PLACED point sites (The Prospect, Task 8)**: a cave
    ///   or an exotic site is discovered when the possession stands in the
    ///   walk-band ROOM it was placed in — [`plate::MapSite::placed`],
    ///   matched against `truncate_to_walk(position)`.
    ///
    ///   **This is an encounter, not co-location, and the distinction is
    ///   the room's size.** The `discovery` module's own doc refuses to
    ///   infer knowledge from sharing a VERTEX, which spans 110-132 km
    ///   ("going to Paris is not visiting the Catacombs"). A placed site's
    ///   facet is 1.126 km, it is the only room the site is in, and
    ///   `Session::describe_here`'s own site clause has just told the
    ///   player "You can enter the cave here." — so a map that still
    ///   withheld the mark would be hiding what the prose in the same
    ///   snapshot said. The `delve`-narration rule above is KEPT rather
    ///   than replaced: it is a second, independent route to the same
    ///   record for a cave, and discovery is monotonic (H6), so two routes
    ///   cannot conflict.
    ///
    ///   **It matches on the FACET and keys on the VERTEX**, both taken
    ///   from the same [`plate::MapSite`] the glyph is drawn from. The
    ///   older shape — resolve the position's nearest vertex and record
    ///   `Cave(that vertex)` — happens to be right on seed 42 (measured: 0
    ///   of 874 caves and 0 of 103 exotic sites resolve to a neighbour) but
    ///   rests on a guarantee `hornvale_worldgen::site_facet_for` expressly
    ///   declines to make, since the cube-sphere quad mesh and the
    ///   icosphere vertex mesh have been unrelated since The Pavement. A
    ///   site discovered through the roster is drawable by construction.
    fn update_discovery(&mut self) {
        let position = self.session.position();
        self.visited.record(position.clone());

        let coord = position.coord();
        let vertex = self
            .nearest
            .nearest(&self.geo, coord.latitude, coord.longitude);
        for id in self.index.at(vertex) {
            self.discovered.record(FeatureId::Extent(*id));
        }

        let walk = hornvale_vessel::walk_depth(self.session.context());
        self.discover_placed_sites_at(&hornvale_vessel::truncate_to_walk(&position, walk));

        if let Ok(snap) = hornvale_game_core::Snapshot::parse(&self.cached) {
            if matches!(snap.spatial, hornvale_game_core::Spatial::Chamber { .. }) {
                self.discovered.record(FeatureId::Settlement(vertex));
            }
            if snap.narration.prose.starts_with(DELVE_SUCCESS_PREFIX) {
                self.discovered.record(FeatureId::Cave(vertex));
            }
        }
    }

    /// Record every PLACED site standing in the walk-band room `here`
    /// ([`plate::sites_standing_in`], which owns the rule; this method owns
    /// only the recording).
    ///
    /// **Separate from [`Self::update_discovery`] so a test can supply the
    /// one input it cannot walk to.** A placed site occupies about one facet
    /// in 9,830 (decision 0669's ceiling), so no sequence of `go` commands
    /// reaches one inside a test, and a feature nothing can exercise is a
    /// feature nothing has checked — this campaign's own progress ledger
    /// records three tests that turned out vacuous for exactly that shape of
    /// reason. Everything downstream of `here` is the shipped path: the real
    /// roster, the real identities, and the real plate.
    fn discover_placed_sites_at(&mut self, here: &Facet) {
        // Collected before recording because `self.sites` and
        // `self.discovered` are two fields of the same `&mut self`.
        for id in plate::sites_standing_in(&self.sites, here) {
            self.discovered.record(id);
        }
    }

    /// Every walk-band room the possession has stood in this session
    /// (spec Amendment 1 §A4a). See the `discovery` module's own doc --
    /// **this accessor currently has no caller**: the world map does not
    /// yet draw visitedness. `pub` for a future campaign and for tests.
    pub fn visited(&self) -> &Visited {
        &self.visited
    }

    /// How many CHART TILES of the world plate's terrain layer this session
    /// has actually drawn (as opposed to served from the cache). Exists
    /// because **a cache with no observable hit is indistinguishable from a
    /// cache that never hits**: every correctness test passes either way, so
    /// the counter is what makes
    /// `a_cursor_move_inside_the_plate_does_not_re_render_it` a real
    /// assertion rather than a hopeful one.
    ///
    /// **The unit is a TILE, not a plate, since The Quadrat's Task 5**, and
    /// the rename from `plate_renders` is the point rather than tidiness: a
    /// plate is no longer a cache unit at all, so "how many plates were
    /// rendered" has no referent. A partial re-render — the tile column a
    /// one-column scroll uncovers — is the ordinary case now, and a counter
    /// of whole plates could not see it.
    ///
    /// **It counts the TERRAIN layer only, since The Quadrat's Task 4.** The
    /// feature layer is redrawn on every single call and is deliberately not
    /// counted — counting it would make this number constant and useless,
    /// and the expensive half is the raster.
    /// type-audit: bare-ok(count)
    pub fn tile_renders(&self) -> u64 {
        self.tiles.misses()
    }

    /// Test-only mutable access to the discovery set, so a test can vary
    /// that one cache-key input without walking a possession into a
    /// settlement.
    #[cfg(test)]
    pub fn discovered_mut_for_test(&mut self) -> &mut Discovered {
        &mut self.discovered
    }

    /// TEST-ONLY: the committed proper name of the settlement whose own
    /// (latitude, longitude) resolves nearest to `vertex` — read directly
    /// off the world's ledger (`hornvale_kernel::NAME`), independent of
    /// anything `MapSite`/`Driver` exposes to a real caller.
    ///
    /// **This is the oracle Decision 0670's own test needs, and it must
    /// stay a test-only backdoor, not a new production path.** `MapSite`
    /// carries no name field at all — that absence is the type-level half
    /// of the leak-proof argument (`draw_feature_layer` writes only a
    /// `char` and an RGB triple) — so a test asserting a name is ABSENT
    /// from the cursor readout needs an independent route to the real name
    /// to compare against, or the assertion has nothing to check for. This
    /// reads the SAME facts `plate::settlements_of` does
    /// (`IS_SETTLEMENT`/`LATITUDE`/`LONGITUDE`), plus the one it does not
    /// need for drawing: `hornvale_kernel::NAME`, committed once per
    /// settlement at genesis (`domains/settlement/src/genesis.rs`).
    ///
    /// `None` if no settlement resolves to `vertex`, or the ledger holds no
    /// name for it — a test using this should treat either as "nothing to
    /// assert the absence of" rather than a hard failure, since the caller
    /// is expected to have already confirmed `vertex` is a real settlement
    /// vertex some other way.
    #[cfg(test)]
    fn settlement_name_for_test(&self, vertex: hornvale_kernel::Vertex) -> Option<String> {
        // SAFETY: identical reborrow to `Driver::start`'s own
        // `unsafe { &*world }` — `self.world` is a live `Box::into_raw`
        // pointer for the whole of `Driver`'s lifetime, reclaimed exactly
        // once in `Drop`, which cannot run while `&self` is held.
        let world: &World = unsafe { &*self.world };
        world
            .ledger
            .find(hornvale_settlement::IS_SETTLEMENT)
            .find_map(|fact| {
                let lat = match world
                    .ledger
                    .value_of(fact.subject, hornvale_settlement::LATITUDE)
                {
                    Some(hornvale_kernel::Value::Number(n)) => *n,
                    _ => return None,
                };
                let lon = match world
                    .ledger
                    .value_of(fact.subject, hornvale_settlement::LONGITUDE)
                {
                    Some(hornvale_kernel::Value::Number(n)) => *n,
                    _ => return None,
                };
                if self.nearest.nearest(&self.geo, lat, lon) != vertex {
                    return None;
                }
                world
                    .ledger
                    .text_of(fact.subject, hornvale_kernel::NAME)
                    .map(str::to_string)
            })
    }

    /// Every feature the possession has discovered this session (spec
    /// Amendment 1 §A4b). See the `discovery` module's own doc.
    pub fn discovered(&self) -> &Discovered {
        &self.discovered
    }
}

impl Drop for Driver {
    fn drop(&mut self) {
        // SAFETY: `ctx` and `world` were allocated by `Box::into_raw` in
        // `start` and are reclaimed exactly once, here. `session` (a plain
        // struct field, dropped automatically immediately after this
        // function returns) never owns either allocation — it only borrows
        // through `&'static` references, whose drop glue is a no-op — so the
        // order between reclaiming them and `session`'s own drop does not
        // matter for soundness. Freed in the reverse of allocation order
        // regardless, to match `clients/vessel/wasm/src/lib.rs`'s
        // `teardown()`.
        unsafe {
            drop(Box::from_raw(self.ctx));
            drop(Box::from_raw(self.world));
        }
    }
}

/// Task 3b: zoom, cursor-driven scroll, re-centre, and the world-view
/// resolver — the ladder from the walk band into the world map and back,
/// and the two things Task 3a's own review left pointing at the wrong
/// plate (the cursor clamp and the strip's resolver).
///
/// In-module rather than `tests/driver.rs` (the task's own instruction:
/// "Test: in-module in both") — several of these reach private fields
/// (`self.cursor`, `self.window`, `self.nearest`, `self.geo`, `self.namer`)
/// directly, on purpose, to reconstruct an EXPECTED answer independently
/// of the code under test rather than calling it a second time and
/// comparing it with itself.
#[cfg(test)]
mod portolan_tests {
    use super::*;
    use hornvale_astronomy::RotationPin;
    use hornvale_kernel::Value;

    /// A fresh seed-42 flagship driver — the same construction every
    /// acceptance test in `tests/driver.rs` uses.
    fn test_driver() -> Driver {
        Driver::start(42, PossessTarget::Flagship).expect("seed 42 generates")
    }

    /// The Wash, Task 4: `season_bucket_for` folds a missing calendar to
    /// bucket 0. This exercises that input contract directly.
    #[test]
    fn a_starless_world_resolves_season_bucket_zero() {
        assert_eq!(
            season_bucket_for(None, hornvale_kernel::WorldTime::GENESIS),
            0,
            "no calendar to consult must resolve to bucket 0, not panic or guess"
        );
    }

    /// **The roster the map draws from holds all three site kinds, and the
    /// counts are the campaign's own headline.** The Prospect exists because
    /// a world's caves and exotic sites were invisible; before Task 8 this
    /// client's rosters were 389 settlement vertices and 874 cave vertices
    /// in two separate structures, with NO representation of an exotic site
    /// at all — so the 103 asserted here are the sites the map could not
    /// draw at any rung, in any window, however much a reader explored.
    ///
    /// **The three numbers are goldens, deliberately.** They move on a
    /// terrain epoch, on a change to `GeneratedTerrain::cave_at`, on a
    /// change to `hornvale_locale`'s strangeness budget, and on a change to
    /// settlement siting — every one of which is a change somebody should
    /// have to look at. 874 and 103 are the same figures the campaign spec
    /// and `book/src/gallery/strange-sites-seed-42.md` carry; a red here
    /// against a green artifact means the client and the listing have come
    /// apart, which is the whole class of defect this task was fixing.
    #[test]
    fn the_site_roster_carries_every_kind_and_only_placed_kinds_carry_a_facet() {
        use hornvale_vessel::site::SiteKind;
        let d = test_driver();
        let count = |kind: SiteKind| d.sites.iter().filter(|site| site.kind == kind).count();

        assert_eq!(count(SiteKind::Cave), 874, "seed 42's cave roster moved");
        assert_eq!(
            count(SiteKind::Exotic),
            103,
            "seed 42's exotic-site roster moved — this is the tier the map \
             could not draw at all before The Prospect's Task 8"
        );
        assert_eq!(
            count(SiteKind::Settlement),
            389,
            "seed 42's settlement-vertex roster moved"
        );
        assert_eq!(
            d.sites.len(),
            874 + 103 + 389,
            "the roster holds nothing else"
        );

        for site in &d.sites {
            match site.kind {
                // A cave or an exotic site is ADDRESSED by
                // `hornvale_worldgen::site_facet_for`, so the roster must
                // carry that address or the map is back to drawing the
                // vertex.
                SiteKind::Cave | SiteKind::Exotic => assert!(
                    site.placed.is_some(),
                    "a placed {:?} reached the roster with no facet",
                    site.kind
                ),
                // A settlement's address is its own committed coordinate and
                // `site_facet_for` is expressly not its authority.
                SiteKind::Settlement => assert!(
                    site.placed.is_none(),
                    "a settlement was given a placement address it does not have"
                ),
            }
        }
    }

    /// **The whole feature, end to end: standing in a placed site's own room
    /// discovers it (Gate B's bookkeeping), and the map draws its glyph
    /// throughout — before that room is ever entered and after.**
    ///
    /// This test used to be the one that kept Task 8 from being vacuous by
    /// proving the glyph appeared only AFTER discovery; The Prospect's Gate A
    /// ungating (fix round) made that premise false on purpose — a placed
    /// site's KIND now draws whether or not it has been discovered (Nathan's
    /// ruling: "show placed sites on the world map... just don't show their
    /// labels"). So this test is rewritten in place rather than deleted: the
    /// half that is still real and still worth proving end to end,
    /// undiluted by a hand-built roster or a hand-built `Discovered`, is that
    /// entering a placed site's room genuinely records its discovery
    /// ([`Driver::discover_placed_sites_at`]) — the fact Gate B's cursor
    /// readout and the walk-band/chamber prose still key their own naming
    /// off — while [`Driver::world_plate`] draws the identical glyph before
    /// and after, because Gate A never consulted that fact in the first
    /// place. It has to inject the possession's position: a placed site
    /// occupies about one facet in 9,830, so no sequence of `go` commands in
    /// a test reaches one.
    #[test]
    fn standing_in_a_placed_sites_room_discovers_it_and_the_map_keeps_drawing_it() {
        use hornvale_vessel::site::SiteKind;
        for (kind, glyph) in [
            (SiteKind::Cave, plate::CAVE_GLYPH),
            (SiteKind::Exotic, plate::EXOTIC_GLYPH),
        ] {
            let mut d = test_driver();
            enter_world_view(&mut d);
            let (w, h) = (104u16, 56u16);
            let (plate_w, plate_h) = Driver::world_plate_dims(w, h);
            let site = centre_on_a_placed_site(&mut d, kind, plate_w, plate_h);
            let here = site
                .placed
                .clone()
                .expect("a placed kind carries its facet");

            let before = d.world_plate(w, h);
            // A whole-plate `contains` here is near-vacuous for a cave
            // specifically (review fix round 1, item 6): seed 42 has 874 of
            // them, and this window (104x56, the client's own real floor)
            // is wide enough that more than one can be in frame, so this
            // proves only "some cave, somewhere on screen, is drawn" — not
            // that THIS site is. Pinpointing this site's own exact screen
            // position would mean duplicating `plate.rs`'s own private
            // projection math here (a different module, no `pub` seam for
            // it); the message is downgraded to say only what this check
            // actually proves, per the coordinator's own offered
            // alternative, rather than implying a precision it does not
            // have. `plate.rs`'s own tests (`draw_with_draws_a_point_site_
            // whether_or_not_it_is_discovered`, `the_feature_layer_draws_
            // every_site_whether_or_not_it_is_discovered`) are what pin the
            // exact-position property, inside the module that can compute
            // the position without duplicating it.
            assert!(
                before.to_plain_text().contains(glyph),
                "no {kind:?} glyph is drawn anywhere on the visible plate before \
                 discovery — Gate A ungating is not reaching this kind at all \
                 (this does not confirm THIS specific site drew; see this test's \
                 own doc for where that IS pinned)"
            );
            assert!(
                !d.discovered().contains(site.feature_id()),
                "guard: the {kind:?} must not already read as discovered, or the \
                 assertion below would be vacuous"
            );

            d.discover_placed_sites_at(&here);
            assert!(
                d.discovered().contains(site.feature_id()),
                "standing in a {kind:?}'s own room did not discover it"
            );
            assert_eq!(
                d.world_plate(w, h).to_plain_text(),
                before.to_plain_text(),
                "discovering a {kind:?} moved the drawn plate — Gate A must not consult \
                 `discovered` for a placed site's own glyph"
            );
        }
    }

    /// **The wiring, pinned separately from the rule.**
    /// [`Driver::discover_placed_sites_at`] holds the recording and
    /// `plate::sites_standing_in` holds the matching, and both are tested on
    /// their own — but the one line in [`Driver::update_discovery`] that
    /// derives `here` from the possession's position and calls them is not
    /// covered by either. Delete that line and every other Task 8 test stays
    /// green while no site is ever discovered by walking, which is the whole
    /// feature.
    ///
    /// So this test comes at it from the other end: it puts a site's placed
    /// address AT the possession's own starting room and drives the real
    /// `update_discovery`. A test cannot walk the possession to a placed
    /// site (about one facet in 9,830), so it moves the site to the
    /// possession instead — the same injection, from the opposite side.
    #[test]
    fn update_discovery_records_a_placed_site_the_possession_stands_in() {
        use hornvale_vessel::site::SiteKind;
        let mut d = test_driver();
        let walk = hornvale_vessel::walk_depth(d.session.context());
        let here = hornvale_vessel::truncate_to_walk(&d.session.position(), walk);

        // A vertex the real exotic roster does not use, so the identity
        // asserted below can only have come from the entry injected here.
        let vertex = Vertex(0);
        let injected = plate::MapSite {
            kind: SiteKind::Exotic,
            vertex,
            placed: Some(here),
            population: 0,
        };
        assert!(
            !d.sites
                .iter()
                .any(|site| site.feature_id() == injected.feature_id()),
            "guard: seed 42's own roster must not already hold this identity"
        );
        assert!(
            !d.discovered().contains(injected.feature_id()),
            "guard: it must not already be discovered"
        );

        d.sites.push(injected.clone());
        d.update_discovery();
        assert!(
            d.discovered().contains(injected.feature_id()),
            "the possession is standing in this site's own room and \
             update_discovery did not record it"
        );
    }

    /// The opening clause of `Driver::resolution_disclosure`'s message —    /// The opening clause of `Driver::resolution_disclosure`'s message —
    /// the substring the two tests below match on. Named once so neither
    /// repeats the shipped wording's own vertex-sense noun, which
    /// `cli/tests/suite/lexicon_guard.rs` ratchets against.
    const DISCLOSURE_OPENING: &str = "one character stands for";

    /// Enter the map and walk the ladder all the way OUT, to
    /// [`GLOBE_RUNG`] — the coarsest rung, which is where every test below
    /// that says "the world view" means to stand.
    ///
    /// **Since The Quadrat's Task 2 that is six presses, not one.** The
    /// ladder is continuous now: one zoom-out from band B steps to the rung
    /// immediately coarser than band B, not to the globe.
    ///
    /// **The count is BOUNDED, deliberately, rather than a
    /// `while depth > GLOBE_RUNG` walk.** About twenty-five tests route
    /// through this helper. An unbounded walk would spin forever the moment
    /// `apply_zoom`'s zoom-out arm regressed to a no-op — turning the exact
    /// regression this file's ladder tests exist to catch into a HUNG suite
    /// rather than a failure list, which is the worst symptom available to a
    /// future reader. The two assertions below stay as the real check: the
    /// bound only decides when to stop pressing, never whether the walk
    /// arrived.
    /// Enter the map and climb the ladder all the way IN, to
    /// [`BAND_B_RUNG`] — the walk band's own rung.
    ///
    /// **New at The Hachure's Stage 0, and it exists because `enter_map` no
    /// longer lands there.** The map now opens at
    /// [`plate::map_entry_rung`] — the coarsest rung the mesh can fill —
    /// because band B is seven rungs finer than the grid and a plate there
    /// draws one vertex's reading everywhere. Ten tests in this module were
    /// written when `enter_map` *was* the way to band B, and they are about
    /// band-B behaviour: the observer's own facet, the sight caption, the
    /// perception overlay's full sight cone, the ladder's ceiling. So they
    /// now have to say they mean band B, which is what this helper is.
    ///
    /// **It sets the rung rather than ZOOMING to it, and the first draft of
    /// this helper zoomed.** Zoom is anchored on the CURSOR (decision 0292,
    /// "centre on arrival, anchor on gesture"), so seven zoom-ins multiply
    /// the chart by 128 and amplify any sub-tile cursor offset until the
    /// observer leaves the window entirely — three tests failed exactly that
    /// way, one of them reporting the observer at row 8347 against a window
    /// origin of 7819 on a 52-row plate.
    ///
    /// These tests are about what band B DRAWS, not about how a reader gets
    /// there, and what they were written against is the pre-Stage-0
    /// `enter_map`: band B, centred on the observer. So this reproduces that
    /// arrival directly and makes no claim about the zoom path — which
    /// `zoom_in_climbs_to_band_b_and_saturates_there` and
    /// `zooming_out_of_band_b_lands_where_the_reader_was_looking` own.
    fn enter_band_b(d: &mut Driver) {
        d.enter_map();
        d.window.depth = BAND_B_RUNG;
        d.centre_on_the_observer();
        d.refresh_strip();
        assert_eq!(
            d.window.depth, BAND_B_RUNG,
            "the helper must land on the walk band's rung"
        );
        assert!(d.at_walk_band_rung(), "band B is the walk band's own rung");
    }

    fn enter_world_view(d: &mut Driver) {
        d.enter_map();
        for _ in 0..(BAND_B_RUNG - GLOBE_RUNG) {
            d.apply(Action::Zoom(-1));
        }
        assert_eq!(
            d.window.depth, GLOBE_RUNG,
            "the helper must land on the coarsest rung"
        );
        assert!(
            !d.at_walk_band_rung(),
            "zooming out from the walk band must enter the world view"
        );
    }

    /// Scroll `d`'s window so the possessed agent's own position sits at the
    /// middle of a `w`x`h` plate.
    ///
    /// **Before Task 1 the coarsest rung WAS the whole planet in one
    /// screen**, so a test could read a 40x20 plate at origin `(0, 0)` and
    /// expect to see the world. The chart is the rung now — 363x362 tiles at
    /// [`GLOBE_RUNG`] — and the origin corner of it is a patch of arctic
    /// ocean, so a test that means to look at somewhere real has to say
    /// where. The player's own position is not an arbitrary choice: it is
    /// where a client opening the map would put the window, and Task 7 makes
    /// that the shipped gesture.
    /// **DELEGATES to [`Driver::centre_window_on`] since Task 6's fix round
    /// 1 (F2), and the delegation is the point.** Task 1's review made a size
    /// guard the CONDITION of promoting this helper to production; Task 6
    /// wrote the guarded version and left this unguarded copy of the same
    /// name standing beside it, still computing
    /// `(col + vw - u32::from(w) / 2) % vw` in `u32` — the exact underflow
    /// the guard exists for. With 6 call sites here against 1 for the
    /// production method, the UNGUARDED copy was what almost the whole suite
    /// exercised, and two same-named functions with divergent arithmetic in
    /// one file is how the next reader picks the wrong one. There is one
    /// implementation now and the tests drive it.
    fn centre_window_on_the_player(d: &mut Driver, w: u16, h: u16) {
        let c = d.session.position().coord();
        d.centre_window_on(c.latitude, c.longitude, w, h)
            .expect("the flagship's own position is inside the projection's clamp");
    }

    /// Move `d`'s window onto a real cave mouth out of `d`'s OWN roster —
    /// the roster `plate::draw_feature_layer` projects from — and return its
    /// warranting vertex, which is the key its
    /// [`crate::discovery::FeatureId`] carries.
    ///
    /// The first cave the projection actually PLACES: a position above the
    /// polar clamp is on no chart at all, and skipping those is the clamp's
    /// own rule, not a search for a convenient answer.
    ///
    /// **Centred on the site's DRAWN coordinate, never its vertex's** (The
    /// Prospect, Task 8). A cave stands on a facet up to ~40 walk-facet
    /// edges from the vertex that warrants it, so a window centred on the
    /// vertex is not reliably a window the glyph lands in — and the two
    /// tests below both depend on the mark actually being on screen.
    /// `plate::MapSite::coord` is the one route from a site to a position;
    /// this asks it rather than reconstructing one.
    fn centre_on_a_cave(d: &mut Driver, plate_w: u16, plate_h: u16) -> Vertex {
        centre_on_a_placed_site(d, hornvale_vessel::site::SiteKind::Cave, plate_w, plate_h).vertex
    }

    /// [`centre_on_a_cave`]'s counterpart for the ONE point-site kind The
    /// Prospect's Gate A ungating left discovery-gated: a volcano. Unlike a
    /// cave, exotic site or settlement, a volcano is not in `d.sites` at
    /// all — it is addressed through `d.volcanoes`
    /// (`crate::discovery::FeatureId::Extent`) — so this does not delegate
    /// to [`centre_on_a_placed_site`].
    fn centre_on_a_volcano(d: &mut Driver, plate_w: u16, plate_h: u16) -> Vertex {
        let vertex = *d
            .volcanoes
            .iter()
            .find(|&&v| {
                let c = d.geo.coord(v);
                let (vw, vh) = plate::virtual_dims(d.window.depth);
                mercator::project(&d.frame, c.latitude, c.longitude, vw, vh).is_some()
            })
            .expect("seed 42 has a volcano inside the projection's clamp");
        let c = d.geo.coord(vertex);
        d.centre_window_on(c.latitude, c.longitude, plate_w, plate_h)
            .expect("the volcano just passed the clamp above");
        vertex
    }

    /// [`centre_on_a_cave`] over any placed [`hornvale_vessel::site::
    /// SiteKind`], returning the whole roster entry so a caller can reach
    /// the placed facet as well as the identity.
    fn centre_on_a_placed_site(
        d: &mut Driver,
        kind: hornvale_vessel::site::SiteKind,
        plate_w: u16,
        plate_h: u16,
    ) -> plate::MapSite {
        let site = d
            .sites
            .iter()
            .filter(|site| site.kind == kind)
            .find(|candidate| {
                let c = candidate.coord(&d.geo);
                let (vw, vh) = plate::virtual_dims(d.window.depth);
                mercator::project(&d.frame, c.latitude, c.longitude, vw, vh).is_some()
            })
            .cloned()
            .expect("seed 42 has at least one such site inside the projection's clamp");
        let c = site.coord(&d.geo);
        d.centre_window_on(c.latitude, c.longitude, plate_w, plate_h)
            .expect("the site just passed the clamp above");
        site
    }

    // -- The world-plate memo (perf/world-plate-memo) ----------------
    //
    // MEASURED, on seed 42 at the 210x56 design size: one `draw_with` is
    // 264,992 samples (5,408 cells x 49), of which `nearest()` is 88% at
    // ~712 ns/sample -- hundreds of milliseconds, paid on EVERY keystroke
    // because `redraw` re-rendered unconditionally. A cursor move inside
    // the plate changes none of `draw_with`'s inputs.

    /// THE TEST THAT MAKES THE CACHE NON-VACUOUS. A cache with no
    /// observable hit is indistinguishable from one that never hits, and
    /// every correctness test would pass either way — so the counter is
    /// part of the design, not scaffolding.
    #[test]
    fn a_cursor_move_inside_the_plate_does_not_re_render_it() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        let _ = d.world_plate_for_redraw(104, 56);
        let after_first = d.tile_renders();
        assert!(after_first > 0, "the first redraw must actually render");

        // Interior moves: the window cannot scroll, so nothing changes.
        for _ in 0..8 {
            d.apply(Action::CursorBy(1, 0));
            let _ = d.world_plate_for_redraw(104, 56);
        }
        assert_eq!(
            d.tile_renders(),
            after_first,
            "eight interior cursor moves re-rendered the plate"
        );
    }

    /// The other half: something that DOES change an input must re-render.
    /// Without this, "never re-render" would pass the test above.
    #[test]
    fn a_zoom_re_renders_the_plate() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        let _ = d.world_plate_for_redraw(104, 56);
        let before = d.tile_renders();
        d.apply(Action::Zoom(1));
        let _ = d.world_plate_for_redraw(104, 56);
        assert!(
            d.tile_renders() > before,
            "a zoom changes the window and must re-render"
        );
    }

    /// KEY COMPLETENESS, field by field. A key missing an input serves a
    /// stale plate -- the "wrong name indistinguishable from a right one"
    /// shape this campaign hit repeatedly. Each arm varies ONE input.
    #[test]
    fn every_input_the_plate_reads_is_in_its_cache_key() {
        // SIZE — and this arm's assertion is now the OPPOSITE of what it
        // was, deliberately, because the thing it was asserting has been
        // FIXED rather than lost (The Quadrat, Task 5). The drawn plate's
        // own `(w, h)` used to be in the cache key, so ANY resize threw the
        // plate away and redrew it, and this arm asserted that. A tile's
        // size follows from its rung and its position, so a resize draws
        // only the ground the new plate reaches that the old one did not.
        // Both directions are asserted, and each rescues the other: "grows
        // when it must" alone would pass on a cache that never hit, and
        // "shrinks for free" alone would pass on a cache that had stopped
        // drawing altogether.
        let mut d = test_driver();
        enter_world_view(&mut d);
        let _ = d.world_plate_for_redraw(80, 24);
        let small = d.tile_renders();
        assert!(small > 0, "the first redraw must actually draw tiles");
        let _ = d.world_plate_for_redraw(104, 56);
        let large = d.tile_renders();
        assert!(
            large > small,
            "growing the plate drew none of the ground it uncovered"
        );
        let _ = d.world_plate_for_redraw(80, 24);
        assert_eq!(
            d.tile_renders(),
            large,
            "shrinking the plate redrew tiles it already held"
        );

        // frame (re-centre)
        let mut d = test_driver();
        enter_world_view(&mut d);
        let _ = d.world_plate_for_redraw(104, 56);
        let n = d.tile_renders();
        d.apply(Action::CursorBy(9, 4));
        d.apply(Action::Recentre);
        let _ = d.world_plate_for_redraw(104, 56);
        assert!(
            d.tile_renders() > n,
            "a re-centre changes the frame and must re-render"
        );

        // DISCOVERY IS DELIBERATELY NOT AN ARM HERE ANY MORE (The Quadrat,
        // Task 4). It used to be, asserting that a discovery re-rendered the
        // plate — and that is precisely the coupling
        // `CLIENT-tiles-need-the-overlay-split` records as the defect: a key
        // carrying the discovery version is invalidated by every discovery,
        // the whole pyramid for one settlement. The property it was really
        // reaching for — a discovery still reaches the drawn plate — is
        // asserted, together with its new other half, by
        // `a_discovery_redraws_the_features_without_re_rendering_the_terrain`
        // below. It is not dropped; it is stated about the right layer.
    }

    /// **The layer split's own headline, as an assertion**: a discovery that
    /// DOES change what is drawn (a volcano — the one point-site kind The
    /// Prospect's Gate A ungating left discovery-gated) changes it on the
    /// very next redraw, and does NOT re-render the terrain raster
    /// underneath it.
    ///
    /// **This test used to use a cave mouth, and Gate A made that premise
    /// false.** A cave, exotic site or settlement now draws its glyph
    /// whether or not it has been discovered (see `plate::draw_feature_layer`'s
    /// own doc), so discovering one no longer changes the drawn plate at
    /// all — the property this test asserts is now specific to the one kind
    /// still gated. `the_feature_layer_draws_every_site_whether_or_not_it_
    /// is_discovered` in `plate.rs` and
    /// `standing_in_a_placed_sites_room_discovers_it_and_the_map_keeps_
    /// drawing_it` above are what now pin the ungated kinds' own "discovery
    /// changes nothing" property; this test keeps the layer-split property
    /// alive on the one kind that can still exercise it.
    ///
    /// Both halves below are load-bearing and each rescues the other from
    /// vacuity. "Does not re-render" alone would pass on a cache that never
    /// invalidated at all — including one that had stopped showing
    /// discoveries entirely, which is a real regression this split could
    /// introduce. "Changes the plate" alone would pass on the old,
    /// discovery-keyed plate cache the split exists to remove.
    ///
    /// The site is a real volcano out of the driver's OWN roster (the one
    /// `draw_feature_layer` projects from), and the window is moved to it,
    /// because a site off screen would satisfy "does not re-render"
    /// trivially — the `assert_ne!` is what refuses that.
    #[test]
    fn a_discovery_redraws_the_features_without_re_rendering_the_terrain() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        let (w, h) = (104u16, 56u16);
        let (plate_w, plate_h) = Driver::world_plate_dims(w, h);

        let vertex = centre_on_a_volcano(&mut d, plate_w, plate_h);

        let before = d
            .world_plate_for_redraw(w, h)
            .expect("the world view is on");
        let renders = d.tile_renders();
        assert!(renders > 0, "the first redraw must actually render");
        assert!(
            !before.to_plain_text().contains(plate::VOLCANO_GLYPH),
            "guard: the volcano must not already be drawn, or the assertion below \
             would be vacuous"
        );

        d.discovered_mut_for_test()
            .record(crate::discovery::FeatureId::Extent(
                hornvale_terrain::landscape::FeatureId {
                    class: hornvale_terrain::landscape::FeatureClass::Volcano,
                    vertex,
                },
            ));
        let after = d
            .world_plate_for_redraw(w, h)
            .expect("the world view is on");

        assert_ne!(
            before.to_plain_text(),
            after.to_plain_text(),
            "a discovery never reached the drawn plate"
        );
        assert_eq!(
            d.tile_renders(),
            renders,
            "a discovery re-rendered the TERRAIN layer — the whole pyramid, for one site"
        );
    }

    /// **The two driver paths into the layers must still agree** (fix round
    /// 1, Minor 2).
    ///
    /// Before The Quadrat's Task 4, [`Driver::world_plate_for_redraw`]
    /// CALLED [`Driver::world_plate`], so their agreement was structural and
    /// unfalsifiable. They are now two independent argument lists into the
    /// same two layers — plate dims, roster order, colour source — and
    /// `plate.rs`'s own `draw_with_is_the_composition_of_its_layers` covers
    /// only the `plate.rs` half. A divergence here would show as the
    /// unconditional path (which `world_plate` is, and which every test that
    /// drives the plate directly uses) disagreeing with what a player
    /// actually sees.
    ///
    /// **Compared cell by cell, not by `to_plain_text`**, because the colour
    /// SOURCE is one of the three things that could diverge and glyph text
    /// cannot see [`hornvale_game_core::Ink`] at all.
    ///
    /// The window is put on a real, DISCOVERED cave first, so the comparison
    /// covers the feature layer's arguments and not merely the raster's — two
    /// terrain-only plates would agree whatever the roster arguments did.
    #[test]
    fn the_cached_redraw_path_draws_what_an_uncached_world_plate_would() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        let (w, h) = (104u16, 56u16);
        let (plate_w, plate_h) = Driver::world_plate_dims(w, h);
        let site = centre_on_a_cave(&mut d, plate_w, plate_h);
        d.discovered_mut_for_test()
            .record(crate::discovery::FeatureId::Cave(site));

        let direct = d.world_plate(w, h);
        assert!(
            direct.to_plain_text().contains(plate::CAVE_GLYPH),
            "guard: the discovered site must actually be on this plate, or the \
             comparison below covers only the raster"
        );

        // A MISS and then a HIT: the composed output must match the
        // unconditional path on both, since the feature layer is drawn over a
        // clone either way.
        for pass in ["miss", "hit"] {
            let composed = d
                .world_plate_for_redraw(w, h)
                .expect("the world view is on");
            assert_eq!(
                (composed.width(), composed.height()),
                (direct.width(), direct.height()),
                "the two paths sized the plate differently ({pass})"
            );
            for y in 0..direct.height() {
                for x in 0..direct.width() {
                    assert_eq!(
                        composed.get(x, y),
                        direct.get(x, y),
                        "the two driver paths disagree at ({x}, {y}) on the {pass}"
                    );
                }
            }
        }
    }

    /// A cache that returns a DIFFERENT grid than a fresh render is worse
    /// than no cache. Compare the hit against the uncached truth.
    #[test]
    fn a_cache_hit_returns_what_a_fresh_render_would_have_drawn() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        let first = d.world_plate_for_redraw(104, 56).expect("world view is on");
        let hit = d.world_plate_for_redraw(104, 56).expect("world view is on");
        assert_eq!(
            first.to_plain_text(),
            hit.to_plain_text(),
            "a cache hit drew something else"
        );
    }

    // -- Step 1: the zoom ladder (zoom out past the walk band enters the
    //    world view; zoom in past its finest rung leaves it) -----------

    /// **Retargeted by The Quadrat's Task 2, not deleted.** This used to
    /// be `..._enters_the_world_view_at_the_coarsest_rung`, and that intent
    /// has no referent any more: the jump to [`GLOBE_RUNG`] WAS the mode
    /// flip. What survives — and is the thing worth pinning — is that the
    /// first zoom-out off band B crosses into the world view, one rung at a
    /// time.
    #[test]
    fn zoom_out_from_band_b_steps_one_rung_into_the_world_view() {
        let mut d = test_driver();
        enter_band_b(&mut d);
        assert_eq!(
            d.window.depth, BAND_B_RUNG,
            "the session opens on band B, the ladder's finest rung"
        );
        assert!(d.at_walk_band_rung());
        d.apply(Action::Zoom(-1));
        assert!(!d.at_walk_band_rung());
        assert_eq!(
            d.window.depth,
            BAND_B_RUNG - 1,
            "one press is one rung — not a jump to the globe"
        );
        // THE ORIGIN IS NO LONGER THE CORNER HERE, and asserting it were
        // would now pin the arctic-corner bug Task 6 exists to fix:
        // `enter_map` centres band B on the observer, and one zoom-out
        // carries that origin into the coarser rung's own chart. What
        // survives is that the step did not RESET the window, which is the
        // mode-flip behaviour Task 2 removed and this test was retargeted
        // from.
        assert!(
            (d.window.origin_row, d.window.origin_col) != (0, 0),
            "a zoom-out must carry the window, not reset it to the chart's corner"
        );
    }

    #[test]
    fn zoom_out_at_the_coarsest_rung_does_nothing_further() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        let before = d.window;
        d.apply(Action::Zoom(-1));
        assert_eq!(
            d.window, before,
            "there is nothing further out than the whole planet"
        );
        assert!(!d.at_walk_band_rung(), "must still be in the world view");
    }

    #[test]
    fn zoom_in_climbs_one_step_at_a_time() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        d.apply(Action::Zoom(1));
        assert_eq!(d.window.depth, GLOBE_RUNG + 1);
        assert!(!d.at_walk_band_rung());
    }

    /// Entering the map must not open on the WALK BAND's own rung (The
    /// Hachure, Stage 0).
    ///
    /// Band B is seven rungs finer than the terrain mesh, and
    /// [`plate::terrain_at_tile`] resolves every rung through the grid-level
    /// ancestor, so a plate there shows ONE vertex's reading across the whole
    /// screen — measured at 1 distinct vertex on a 120x40 plate, seed 42.
    /// A consultation that opens on a single flat reading is not a map.
    ///
    /// This is the narrow half of Stage 0 and is deliberately an `assert_ne`:
    /// which rung the map *should* open at is
    /// [`the_map_opens_at_the_coarsest_rung_the_mesh_can_fill`]'s claim, and
    /// keeping the two apart means a future change to the entry rung breaks
    /// the specific test rather than this one.
    #[test]
    fn entering_the_map_does_not_open_at_the_walk_band_rung() {
        let mut d = test_driver();
        d.enter_map();
        assert_ne!(
            d.window.depth, BAND_B_RUNG,
            "the map must not open on the walk band's rung: the mesh cannot \
             fill it, so the plate draws one vertex's reading everywhere"
        );
    }

    /// The entry rung is the COARSEST one the mesh can fill (The Hachure,
    /// Stage 0) — asserted as the two-sided property rather than against the
    /// literal `7`, so the claim survives `GLOBE_LEVEL` moving.
    ///
    /// The two clauses are what make it the *coarsest such* rung and not
    /// merely *a* sufficient one: at the entry rung the chart is at least as
    /// fine as the mesh, and one rung coarser it is not. A single clause would
    /// pass on band B, which is the state this stage exists to leave.
    #[test]
    fn the_map_opens_at_the_coarsest_rung_the_mesh_can_fill() {
        let mut d = test_driver();
        d.enter_map();
        let entry = d.window.depth;
        let want = plate::mesh_samples_around_a_great_circle(d.geo.depth());

        assert!(
            plate::virtual_dims(entry).0 >= want,
            "rung {entry}'s chart is {} tiles, under the mesh's {want} samples \
             around a great circle — the map would undersample its own data",
            plate::virtual_dims(entry).0
        );
        assert!(
            entry > GLOBE_RUNG && plate::virtual_dims(entry - 1).0 < want,
            "rung {} already covers the mesh's {want} samples, so {entry} is \
             not the coarsest rung that does",
            entry - 1
        );
    }

    /// The map must show WHERE YOU ARE at the rung it opens at (The Hachure,
    /// Stage 0).
    ///
    /// **This test exists because Stage 0 created the defect it guards.**
    /// [`Driver::compose_perception_layer`] returned early unless
    /// `at_walk_band_rung()`, so moving the entry rung off band B silently
    /// removed the observer marker from the opening view —
    /// [`Driver::enter_map`]'s own comment already names that state ("no
    /// marker is drawn and the reader cannot even see which direction home
    /// is") as the thing a previous fix round existed to prevent. Ten
    /// existing tests went red on the rung change; none of them would have
    /// caught this, because they all assert about band B.
    ///
    /// Asserted on the drawn plate rather than on the gate, because the gate
    /// is the implementation: what matters is that `@` is on the screen.
    #[test]
    fn the_map_shows_the_observer_at_the_rung_it_opens_at() {
        let mut d = test_driver();
        d.resize(120, 40);
        d.enter_map();
        assert_ne!(
            d.window.depth, BAND_B_RUNG,
            "sanity: this test is about the overlay OFF band B"
        );
        let plate = d.world_plate_for_redraw(120, 40).expect("a plate");
        let observers = (0..plate.height())
            .flat_map(|y| (0..plate.width()).map(move |x| (x, y)))
            .filter(|&(x, y)| plate.get(x, y).is_some_and(|c| c.glyph == Some('@')))
            .count();
        assert_eq!(
            observers, 1,
            "the map must paint the observer exactly once at its entry rung — \
             a consultation that cannot show where you stand is the state \
             `enter_map`'s own comment calls unshippable"
        );
    }

    /// Off band B the overlay paints the OBSERVER ALONE — the sight cone is a
    /// walk-band claim and does not survive coarsening.
    ///
    /// At the entry rung one tile spans tens of kilometres, so the whole
    /// 31-facet band collapses onto a single tile. Painting the band's marks
    /// there would claim mark positions the rung cannot resolve; painting the
    /// observer says only "you are in this tile", which is true at every
    /// rung. So the split is by WHAT the claim is, not by how many boxes land.
    #[test]
    fn off_band_b_the_overlay_paints_the_observer_and_no_marks() {
        let mut d = test_driver();
        d.resize(120, 40);
        d.enter_map();
        let plate = d.world_plate_for_redraw(120, 40).expect("a plate");
        let chart_glyphs: Vec<char> = (0..plate.height())
            .flat_map(|y| (0..plate.width()).map(move |x| (x, y)))
            .filter_map(|(x, y)| {
                plate.get(x, y).and_then(|c| {
                    (c.source == hornvale_game_core::Source::Chart)
                        .then_some(c.glyph)
                        .flatten()
                })
            })
            .collect();
        assert_eq!(
            chart_glyphs,
            vec!['@'],
            "off band B the overlay's whole output must be the observer"
        );
    }

    /// Leaving the map still hands the walker back band B — the entry rung is
    /// the CONSULTATION's, never the walker's.
    ///
    /// `leave_the_map` is the owner of that restore (`self.window.depth =
    /// BAND_B_RUNG` plus a re-centre), and Stage 0 changes the arrival rung
    /// without touching the departure. Pinned because the two now differ:
    /// before this stage they were the same number, so nothing could tell
    /// whether the restore was doing anything.
    #[test]
    fn leaving_the_map_returns_the_walker_to_band_b() {
        let mut d = test_driver();
        d.enter_map();
        assert_ne!(
            d.window.depth, BAND_B_RUNG,
            "sanity: the map opened coarser"
        );
        d.apply(Action::ToggleFocus);
        assert_eq!(
            d.window.depth, BAND_B_RUNG,
            "leaving the map must return the walker to the band they stand in"
        );
        assert!(d.at_walk_band_rung());
    }

    /// **Retargeted by The Quadrat's Task 2.** This used to be
    /// `zoom_in_stops_at_max_zoom_then_the_next_press_leaves_the_world_view`:
    /// arriving at [`BAND_B_RUNG`] kept the world view on, and ONE MORE
    /// press flipped it off while resetting the window to the globe. Band B
    /// is the walk band's own rung now, so the arrival IS the handover and
    /// the press past it does nothing at all.
    #[test]
    fn zoom_in_climbs_to_band_b_and_saturates_there() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        for _ in 0..(BAND_B_RUNG - GLOBE_RUNG - 1) {
            d.apply(Action::Zoom(1));
        }
        assert_eq!(
            d.window.depth,
            BAND_B_RUNG - 1,
            "the finest rung the RASTER draws is one below band B"
        );
        assert!(
            !d.at_walk_band_rung(),
            "still the world plate one rung below band B"
        );

        d.apply(Action::Zoom(1));
        assert_eq!(
            d.window.depth, BAND_B_RUNG,
            "the finest rung is band B: one tile per facet"
        );
        assert!(
            d.at_walk_band_rung(),
            "band B is the walk band's own chart until a later task moves it"
        );

        let at_ceiling = d.window;
        d.apply(Action::Zoom(1));
        assert_eq!(
            d.window, at_ceiling,
            "a press past the ceiling must change nothing — no mode to flip"
        );
    }

    /// Sixty-four presses is far more than the ladder's own height, so
    /// this must walk all the way up the ladder and STOP at band B — the
    /// ladder's ceiling must be a real stop, not merely a slow climb, and
    /// not a wrap back to the globe either.
    #[test]
    fn many_zoom_ins_climb_to_band_b_and_stop_there() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        for _ in 0..64 {
            d.apply(Action::Zoom(1));
        }
        assert_eq!(d.window.depth, BAND_B_RUNG);
        assert!(d.at_walk_band_rung());
    }

    /// THE LADDER'S ENDS. Both are saturations of one number now — there is
    /// no mode to flip at either end, so twenty presses in either direction
    /// must land on the end rung and stay there.
    ///
    /// **It walks OUT before it walks in, and that ordering is the test.**
    /// `Driver::start` opens the session ON [`BAND_B_RUNG`], so a version
    /// that began with the zoom-ins would assert the ceiling against the
    /// state it started in — an assertion a gutted `apply_zoom` satisfies
    /// for free. Descending first makes every assertion below a real result
    /// of the arm it names: gut the zoom-IN arm and the ceiling assertion
    /// fails; gut the zoom-OUT arm and the floor assertion fails.
    #[test]
    fn the_ladder_runs_from_the_globe_rung_to_band_b_and_refuses_past_both() {
        let mut d = test_driver();
        d.enter_map();
        for _ in 0..20 {
            d.apply(Action::Zoom(-1));
        }
        for _ in 0..20 {
            d.apply(Action::Zoom(1));
        }
        assert_eq!(
            d.window().depth,
            plate::BAND_B_RUNG,
            "zoomed in past band B"
        );
        for _ in 0..20 {
            d.apply(Action::Zoom(-1));
        }
        assert_eq!(
            d.window().depth,
            plate::GLOBE_RUNG,
            "zoomed out past the globe"
        );
    }

    /// Seven rungs, SIX zoom-out steps — the bound `Session::map` already
    /// enforces as `depth - globe_level`. Stated as both numbers because
    /// they differ by one and this is where that discrepancy would be
    /// minted.
    ///
    /// **Walks OUT first for the reason the test above states**: the
    /// session opens on [`BAND_B_RUNG`], so without the descent the climb
    /// would not have to climb, and the seed `seen` collects would be the
    /// start state rather than the zoom-IN arm's own answer.
    #[test]
    fn every_rung_of_the_ladder_is_reachable_and_distinct() {
        let mut d = test_driver();
        d.enter_map();
        for _ in 0..20 {
            d.apply(Action::Zoom(-1));
        }
        for _ in 0..20 {
            d.apply(Action::Zoom(1));
        }
        // DERIVED, not pinned: the ladder is `GLOBE_RUNG..=BAND_B_RUNG`, so
        // its length is a function of the two constants. It read a literal 6
        // and a literal 7 until The Pavement moved `BAND_B_RUNG` to 13, and
        // then failed with "left: 7, right: 6" — a count nobody could read as
        // a band. Whichever end moves next, this follows.
        let rungs = (plate::BAND_B_RUNG - plate::GLOBE_RUNG + 1) as usize;
        let mut seen = std::collections::BTreeSet::new();
        seen.insert(d.window().depth);
        for _ in 0..rungs - 1 {
            d.apply(Action::Zoom(-1));
            assert!(seen.insert(d.window().depth), "a rung repeated");
        }
        assert_eq!(seen.len(), rungs, "expected {rungs} rungs, saw {seen:?}");
        assert_eq!(*seen.iter().next().unwrap(), plate::GLOBE_RUNG);
        assert_eq!(*seen.iter().next_back().unwrap(), plate::BAND_B_RUNG);
    }

    #[test]
    fn zoom_plus_on_the_walk_band_alone_is_a_no_op() {
        let mut d = test_driver();
        enter_band_b(&mut d);
        assert!(d.at_walk_band_rung());
        let before_window = d.window;
        d.apply(Action::Zoom(1));
        assert!(
            d.at_walk_band_rung(),
            "band B is already the ladder's finest rung"
        );
        assert_eq!(d.window, before_window);
    }

    /// THE SAME NO-OP, AFTER SCROLLING — fix round 1's F1, and the reason the
    /// test above was not enough.
    ///
    /// `zoom_plus_on_the_walk_band_alone_is_a_no_op` compares the window
    /// against its state at `enter_map`, which is the state band B's own
    /// centring just produced — so re-centring on a saturating press is
    /// INVISIBLE to it, and its premise had silently narrowed to "a press
    /// that changes nothing changes nothing, provided nothing had changed".
    /// Measured against the defect: `origin_col` jumped 21169 -> 2176, about
    /// nineteen thousand tiles of the player's own scroll discarded by a
    /// keypress documented as doing nothing. That is Nathan's founding
    /// complaint — "the map zooms based on criteria I have not identified" —
    /// reintroduced by its own fix.
    ///
    /// So this one scrolls FIRST, a long way, and then presses `+`.
    #[test]
    fn zoom_plus_at_the_ladders_ceiling_keeps_the_players_scroll() {
        let mut d = test_driver();
        d.resize(210, 56);
        enter_band_b(&mut d);
        assert!(d.at_walk_band_rung(), "sanity: the ladder's ceiling");
        let centred = d.window;

        // Scroll east, hard, past the plate's own edge — the only way to
        // travel at this rung (spec §4.2: the cursor clamps and the window
        // moves).
        for _ in 0..20 {
            d.apply(Action::CursorBy(i16::MAX, 0));
        }
        let scrolled = d.window;
        assert_ne!(
            scrolled.origin_col, centred.origin_col,
            "the premise: the scroll must actually have moved the window, or a \
             re-centre would be undetectable — which is exactly how the defect \
             this test exists for stayed hidden"
        );

        d.apply(Action::Zoom(1));
        assert_eq!(
            d.window, scrolled,
            "a press past the ladder's ceiling must change nothing at all — not \
             the rung, and not the window the player scrolled to"
        );
    }

    // -- Step 2: the cursor clamps to the ACTIVE plate -------------------

    #[test]
    fn the_cursor_reaches_columns_the_walk_bands_fixed_width_cannot() {
        let mut d = test_driver();
        d.resize(210, 56); // a realistic terminal, world plate wider than PLATE_WIDTH
        enter_world_view(&mut d);
        let (plate_w, _) = d.active_plate_dims();
        assert!(
            plate_w > hornvale_game_core::spread::PLATE_WIDTH,
            "the test terminal must actually produce a wider-than-walk-band plate"
        );
        d.apply(Action::CursorBy(i16::MAX, 0));
        assert_eq!(
            d.cursor.x,
            plate_w - 1,
            "the cursor must reach the world plate's own far edge, not stop at PLATE_WIDTH - 1"
        );
    }

    /// **RETARGETED by Task 6, and the retarget is the point.** This used to
    /// be `..._clamps_back_to_the_walk_bands_width_once_the_world_view_is_off`
    /// and asserted the cursor's clamp reverted to the fixed
    /// [`hornvale_game_core::spread::PLATE_WIDTH`] on returning to band B.
    /// Band B draws the raster now, at the same `world_plate_width` fit every
    /// other rung uses, so a clamp that reverted would be the Task 3a review
    /// finding all over again — columns of a drawn plate that no cursor can
    /// reach. The surviving property is that the clamp tracks the ACTIVE
    /// plate at EVERY rung, band B included, which is what
    /// `active_plate_dims` having one arm means.
    #[test]
    fn the_cursor_clamp_tracks_the_active_plate_at_band_b_too() {
        let mut d = test_driver();
        d.resize(210, 56);
        enter_world_view(&mut d);
        d.apply(Action::CursorBy(i16::MAX, 0));
        let (coarse_w, _) = d.active_plate_dims();
        assert_eq!(
            d.cursor.x,
            coarse_w - 1,
            "sanity: the coarse rung's own edge"
        );
        for _ in 0..(BAND_B_RUNG - GLOBE_RUNG) + 1 {
            d.apply(Action::Zoom(1)); // climb back to band B
        }
        assert!(d.at_walk_band_rung());
        d.apply(Action::CursorBy(i16::MAX, 0));
        let (band_b_w, _) = d.active_plate_dims();
        assert!(
            band_b_w > hornvale_game_core::spread::PLATE_WIDTH,
            "the test terminal must produce a plate wider than the old fixed \
             walk-band one, or this proves nothing: got {band_b_w}"
        );
        assert_eq!(
            d.cursor.x,
            band_b_w - 1,
            "band B's cursor must reach its own plate's far edge, not stop at \
             the retired PLATE_WIDTH - 1"
        );
    }

    // -- Step 3: the strip resolves against what is drawn -----------------

    #[test]
    fn the_strip_stops_naming_the_walk_bands_chart_once_the_world_view_is_on() {
        let mut d = test_driver();
        d.enter_map();
        let walk_band_strip = d.strip_text().map(str::to_string);
        assert!(
            walk_band_strip
                .as_deref()
                .is_some_and(|t| t.starts_with("Vngashngatva")),
            "sanity: the walk band must resolve the observer's own name at the default \
             cursor, got {walk_band_strip:?}"
        );

        d.apply(Action::Zoom(-1)); // enter the world view
        let world_view_strip = d.strip_text().map(str::to_string);
        assert_ne!(
            walk_band_strip, world_view_strip,
            "the strip must stop naming the walk-band chart once the world plate is drawn \
             instead of it"
        );
        assert_ne!(
            world_view_strip.as_deref(),
            Some(NOTHING_HERE_YET),
            "the world view has a real resolver now; NOTHING_HERE_YET is reserved for a \
             band with none at all"
        );
    }

    // -- Step 4 / H3: scroll and cursor stay coherent, at MORE THAN ONE
    //    offset and MORE THAN ONE zoom ------------------------------------

    /// H3, the hypothesis at real risk: after scrolling, the vertex under the
    /// cursor must be the vertex the window/frame state actually names.
    ///
    /// **Fix round 1, Finding 1 (reviewer): this reconstruction is NOT an
    /// independent derivation — it calls `active_plate_dims`,
    /// `plate::virtual_dims` and `plate::terrain_at_tile` in the same order
    /// over the same fields `resolve_world_view` itself does, so it is a
    /// hand-mirrored copy of the implementation, not an alternate one.**
    /// Said plainly rather than smoothed over: this test cannot catch a
    /// bug shared by both copies (one living in `virtual_dims`, or in
    /// `move_cursor`'s scroll math). What it DOES catch, and the reason it
    /// is kept rather than deleted: a future edit that reintroduces a
    /// stale or hardcoded value INSIDE `resolve_world_view`/`world_view_tile`
    /// alone — a revert-to-no-op mutation of the whole zoom/scroll feature
    /// failed 12 of 15 tests in this module (task report, "behavioural
    /// REDs"), proving this is not vacuous even though it is not
    /// independent. `the_resolved_vertex_matches_the_actually_drawn_glyph_
    /// at_fine_zoom` below is the genuine ground-truth check the reviewer
    /// asked for: it reads [`plate::draw_with`]'s ACTUAL rendered grid,
    /// never re-derives the arithmetic.
    ///
    /// Sweeps three zooms and five offsets, including one (`(39, 19)`)
    /// larger than the floor plate itself, to force real scrolling — a
    /// test pinned at one offset and one zoom cannot see drift between the
    /// window's own offset and the resolver's.
    #[test]
    fn the_vertex_under_the_cursor_matches_the_window_at_every_zoom_and_offset() {
        let mut d = test_driver();
        enter_world_view(&mut d);

        for target_depth in [GLOBE_RUNG, GLOBE_RUNG + 1, GLOBE_RUNG + 3] {
            while d.window.depth < target_depth {
                d.apply(Action::Zoom(1));
            }
            assert_eq!(d.window.depth, target_depth);

            for &(dx, dy) in &[(0i16, 0i16), (7, 0), (0, 5), (39, 19), (-7, 3)] {
                d.apply(Action::CursorBy(dx, dy));

                let (virtual_w, virtual_h) = plate::virtual_dims(d.window().depth);
                let expected_tile = plate::terrain_at_tile(
                    &d.terrain,
                    &d.geo,
                    &d.nearest,
                    &mut hornvale_kernel::RoomMeshMemo::default(),
                    d.frame(),
                    d.window(),
                    virtual_w,
                    virtual_h,
                    u32::from(d.cursor.y),
                    u32::from(d.cursor.x),
                    None,
                    hornvale_kernel::WorldTime::GENESIS,
                    0,
                    None,
                );
                let (species, ph, morph) = &d.namer;
                // `resolve_chain_at`, not `resolve_at`: Task 4 made
                // `resolve_world_view` (`resolved`, below) return the FULL
                // containment chain, so the independent reconstruction here
                // must build the same chain to stay comparable — this test's
                // own H3 claim (window/cursor state agrees with the
                // resolver) is orthogonal to how many features get named.
                //
                // **Widened at Task 5 (B4) to the chain plus the terrain
                // readout**, the same combination `resolve_world_view_at`
                // builds — `resolve_world_view` is no longer chain-only, so
                // an independent reconstruction that stopped at the chain
                // would silently compare against a value the resolver no
                // longer returns.
                let expected_chain = resolve_chain_at(
                    &d.index,
                    expected_tile.vertex,
                    d.seed,
                    species,
                    ph,
                    morph,
                    &|id| d.discovered.contains(FeatureId::Extent(id)),
                );
                let expected_readout = Driver::terrain_readout(&expected_tile);
                let expected = Some(match expected_chain {
                    Some(chain) => format!("{chain} — {expected_readout}"),
                    None => expected_readout,
                });

                let resolved = d.resolve_world_view();
                assert_eq!(
                    resolved, expected,
                    "rung {target_depth}, offset ({dx}, {dy}), cursor {:?}, window {:?}: \
                     the resolver drifted from the window/cursor state",
                    d.cursor, d.window
                );
            }
        }
    }

    /// The genuine ground-truth check Finding 1 asked for: resolve at the
    /// cursor, then read [`plate::draw_with`]'s ACTUAL rendered grid (via
    /// `Driver::world_plate`, the real production drawing path) at that
    /// same screen position, and assert the resolved vertex's ocean/land
    /// class agrees with the drawn glyph. Two genuinely different code
    /// paths — one produces pixels, the other a `Vertex` — cross-checked
    /// against each other's real OUTPUT, not a shared re-derivation of the
    /// same arithmetic.
    ///
    /// Run at `BAND_B_RUNG - 1`, the finest rung the RASTER draws — The
    /// Quadrat's Task 2 hands band B itself back to the walk band's own
    /// chart, so `active_plate_dims`/`world_plate` no longer describe the
    /// same picture there. One rung coarser is where the 49-vote
    /// majority and the true-centre sample are expected to coincide (one
    /// terrain vertex per character — `plate::SUBSAMPLES_PER_AXIS`'s own
    /// doc), across several offsets: this is the region where the check is
    /// unambiguous, so a failure here means the draw/resolve SEAM itself
    /// has drifted, not merely that a coarse character's footprint
    /// straddled a coastline (the F5 finding, which this fine-zoom check
    /// is deliberately not measuring — see `f5_drawn_majority_vs_resolved_
    /// single_point_at_the_coarsest_zoom` for the coarse-zoom, whole-plate
    /// version of this same comparison, which the Finding 2 fix below
    /// makes an EXACT agreement rather than a measured ratio).
    #[test]
    fn the_resolved_vertex_matches_the_actually_drawn_glyph_at_fine_zoom() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        for _ in 0..(BAND_B_RUNG - GLOBE_RUNG - 1) {
            d.apply(Action::Zoom(1));
        }
        assert_eq!(d.window.depth, BAND_B_RUNG - 1);
        assert!(
            !d.at_walk_band_rung(),
            "sanity: the raster is what is drawn here"
        );

        for &(dx, dy) in &[(0i16, 0i16), (5, 0), (0, 3), (-4, 2), (9, -6)] {
            d.apply(Action::CursorBy(dx, dy));

            // GROUND TRUTH: the real drawing path's actual grid. `world_plate`
            // takes the RAW terminal size and fits the plate itself (the
            // same call `main.rs`'s `redraw` makes) — NOT the already-fitted
            // `active_plate_dims()` output, which would double-apply the fit
            // and read a smaller, wrong grid (fix round 1's own bug, caught
            // by this test itself measuring 52.88% instead of the expected
            // ~100% on the first run — see the task report's fix-round-1
            // section).
            let grid = d.world_plate(d.term_w, d.term_h);
            let (plate_w, plate_h) = d.active_plate_dims();
            assert_eq!(
                (grid.width(), grid.height()),
                (plate_w, plate_h),
                "sanity: the rendered grid must be the same size active_plate_dims() reports"
            );
            let drawn_ocean = grid.get(d.cursor.x, d.cursor.y).and_then(|c| c.glyph) == Some('~');

            let resolved_vertex = d.world_view_tile().vertex;
            let resolved_ocean = d.terrain.is_ocean(resolved_vertex);

            assert_eq!(
                resolved_ocean, drawn_ocean,
                "cursor {:?} at zoom {}: the resolver named a vertex whose class \
                 contradicts the actually drawn glyph",
                d.cursor, d.window.depth
            );
        }
    }

    // -- Step 5: `.` re-centres, and the routing table stays total --------

    #[test]
    fn recentre_rolls_the_projection_and_cursor_motion_alone_does_not() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        let before = *d.frame();

        d.apply(Action::CursorBy(12, 4));
        assert_eq!(
            *d.frame(),
            before,
            "the map must hold still while the cursor merely moves"
        );

        d.apply(Action::Recentre);
        assert_ne!(
            *d.frame(),
            before,
            "an explicit `.` must roll the projection"
        );
    }

    /// Split B4's terrain readout at its trailing height clause:
    /// `("… relief,", height_m)`. `height_asl` is a CONTINUOUS reading
    /// (`Self::terrain_readout`'s own doc), unlike the water/relief/chain
    /// classes beside it, which are all discrete lookups keyed on the
    /// NEAREST mesh vertex — kilometres-scale snapping that a sub-pixel
    /// re-projection can never cross. `recentre_keeps_the_same_geographic_
    /// point_under_the_cursor` needs this split for exactly that reason.
    fn split_off_height(s: &str) -> (String, i64) {
        let suffix = " m above sea level";
        let with_suffix = s
            .strip_suffix(suffix)
            .expect("a world-view readout always ends in the height clause");
        let comma = with_suffix
            .rfind(", ")
            .expect("the readout always has a relief clause before the height");
        let (prefix, height) = with_suffix.split_at(comma);
        (
            prefix.to_string(),
            height[2..]
                .parse()
                .expect("the height clause is a signed integer"),
        )
    }

    /// **Finding, not a regression (Task 5).** Before B4 this compared the
    /// two readouts for exact equality, which held because the only content
    /// was the containment chain — a NAME keyed to the nearest mesh vertex,
    /// kilometres apart, so no sub-pixel re-projection jitter could ever
    /// change it. B4 adds `height_asl`, a CONTINUOUS reading
    /// (`plate::TileTerrain::height_asl`'s own doc): `recentre` re-derives
    /// the window's origin so the cursor's own SCREEN tile lands where it
    /// already sat (`Self::recentre`'s own doc), not so the exact
    /// real-valued (lat, lon) under it is bit-identical — Mercator's
    /// non-linearity means those are not quite the same promise. Measured
    /// on this fixture: a 1 m drift (-2321 to -2322) survives the round
    /// trip. So this test now asserts what recentring actually promises —
    /// the SAME feature named, at the SAME water class and relief band —
    /// exactly, and the continuous height within a bound generous enough to
    /// absorb re-projection noise without being vacuous (a real
    /// off-by-a-band error, ≥300 m, would still fail it).
    #[test]
    fn recentre_keeps_the_same_geographic_point_under_the_cursor() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        d.apply(Action::CursorBy(9, 5));
        let before_name = d
            .resolve_world_view()
            .expect("Self::resolve_world_view is total since Task 5");

        d.apply(Action::Recentre);
        let after_name = d
            .resolve_world_view()
            .expect("Self::resolve_world_view is total since Task 5");

        let (before_prefix, before_height) = split_off_height(&before_name);
        let (after_prefix, after_height) = split_off_height(&after_name);
        assert_eq!(
            before_prefix, after_prefix,
            "recentring must roll the map UNDER a still cursor, not change what the cursor \
             names, its water class or its relief band"
        );
        assert!(
            (before_height - after_height).abs() <= 50,
            "recentring moved the cursor's own height reading by more than \
             re-projection noise should ever cost: {before_height} m before, \
             {after_height} m after"
        );
    }

    /// **RETARGETED by Task 6: recentre WORKS at band B now.** It used to
    /// return early off the world view, because "the walk-band chart is not
    /// a Mercator projection and has no central line to roll onto". Band B
    /// IS a Mercator projection now, so the gesture is meaningful at every
    /// rung and a no-op would be the surprise. Its inverse — that the
    /// cursor's own screen position keeps naming the same geographic point
    /// across the roll — is pinned by
    /// `recentre_keeps_the_cursor_on_the_same_place` at the coarse rung and
    /// is unchanged.
    #[test]
    fn recentre_rolls_the_projection_at_band_b_too() {
        let mut d = test_driver();
        enter_band_b(&mut d);
        assert!(d.at_walk_band_rung());
        let before = *d.frame();
        assert_eq!(
            before,
            mercator::frame_for(false),
            "sanity: seed 42 spins, so the opening frame is the geographic one"
        );
        d.apply(Action::Recentre);
        assert_ne!(
            *d.frame(),
            before,
            "band B is a Mercator window like every other rung; `.` must roll it"
        );
    }

    // -- Step 6 / H4: a LOCKED world's habitable band is not in the clamp -

    /// H4: generate real `--rotation locked` worlds (not seed 42 alone —
    /// a spinning-world fixture cannot see this defect at all) and confirm
    /// every real settlement's committed `(latitude, longitude)` projects
    /// inside the clamp under the locked frame, at the 80x24 floor's own
    /// plate size.
    ///
    /// claim: invariant(forall-seed) — checked across seeds 42/7/1337 (the
    /// task brief's own median-terminator-distance seed set), not a claim
    /// about every seed; a locked world's real settlements never fall in
    /// the projection's clamp.
    #[test]
    fn h4_locked_worlds_settlements_are_never_in_the_clamp() {
        let plate_w = hornvale_game_core::spread::PLATE_WIDTH;
        let plate_h = plate_w / hornvale_game_core::spread::GLYPH_ASPECT;
        let frame = mercator::frame_for(true);

        for seed in [42u64, 7, 1337] {
            let pins = hornvale_astronomy::SkyPins {
                rotation: Some(RotationPin::Locked),
                ..hornvale_astronomy::SkyPins::default()
            };
            let world = build_world(
                Seed(seed),
                &pins,
                &TerrainPins::default(),
                &SettlementPins::default(),
            )
            .expect("a locked world still generates");

            let settlements: Vec<_> = world
                .ledger
                .find(hornvale_settlement::IS_SETTLEMENT)
                .map(|f| f.subject)
                .collect();
            assert!(
                !settlements.is_empty(),
                "seed {seed} minted no settlements to check"
            );

            for id in settlements {
                let lat = match world.ledger.value_of(id, hornvale_settlement::LATITUDE) {
                    Some(Value::Number(n)) => *n,
                    other => panic!("seed {seed}, settlement {id:?}: no latitude fact ({other:?})"),
                };
                let lon = match world.ledger.value_of(id, hornvale_settlement::LONGITUDE) {
                    Some(Value::Number(n)) => *n,
                    other => {
                        panic!("seed {seed}, settlement {id:?}: no longitude fact ({other:?})")
                    }
                };
                assert!(
                    mercator::project(&frame, lat, lon, u32::from(plate_w), u32::from(plate_h))
                        .is_some(),
                    "seed {seed}: settlement {id:?} at ({lat}, {lon}) fell in the clamp on a \
                     locked world"
                );
            }
        }
    }

    // -- F5, fix round 1: the resolved vertex must never contradict the
    //    drawn glyph (Finding 2) -----------------------------------------

    /// F5, RE-MEASURED after the Finding 2 fix (task report fix round 1).
    /// **The pre-fix "52.5% agreement (420/800)" figure this doc used to
    /// state here is RETRACTED AS MEASURED**: it was produced by a test
    /// harness bug (both the measurement and this test's own earlier
    /// version called `Driver::world_plate(w, h)` with an already-fitted
    /// plate size, double-applying the fit and rendering a misaligned
    /// 32x16 grid against coordinates computed for 40x20) rather than by
    /// a property of the pre-fix resolver. The pre-fix disagreement rate
    /// is UNMEASURED, not smaller; the disagreement in KIND — a 49-vote
    /// majority and a single centre-point answer can disagree at a
    /// coastline — was and remains real.
    ///
    /// The fix ([`Self::world_view_tile`], now built directly on
    /// [`plate::terrain_at_tile`]) makes the resolver ask the SAME question
    /// the plate draws from, and take the representative of the PAINTED
    /// class — so agreement is no longer a measured ratio, it is a
    /// GUARANTEE the code's own structure enforces. The Quadrat's Task 3
    /// replaced the 49-point vote with mesh addressing and left that
    /// guarantee untouched ([`plate::TileTerrain`]'s own doc). This test still measures and prints the ratio (per the
    /// reviewer's own instruction: "if it is not ~100% by construction,
    /// something about the fix is wrong and I want to see the number") and
    /// then asserts it is exact, across every cell of the floor plate.
    ///
    /// **A screen position drawing a point-site or landform glyph is
    /// excluded from the comparison** (The Prospect, Gate A ungating).
    /// Before this, `d.world_plate(..)` on a fresh, nothing-discovered
    /// driver was indistinguishable from its own terrain layer — no site
    /// drew without being discovered first, so this test never had to
    /// consider the feature layer at all. Now a cave, exotic site or
    /// settlement draws its glyph unconditionally, and a point site's glyph
    /// OVERRIDES the terrain glyph at its own position BY DESIGN, unrelated
    /// to this fix (`draw_with`'s own doc, §A3: a point site "is not in the
    /// terrain render at all"). A settlement or cave mouth standing on what
    /// the raw terrain resolves as ocean (a coastal site) is exactly such a
    /// position: `resolved_ocean` reads the vertex's true class,
    /// `drawn_ocean` reads a glyph that was never claiming to be a terrain
    /// glyph, and the two disagreeing there is not the Finding 2 defect
    /// this test exists to catch.
    ///
    /// **Measured on seed 42's default floor plate: 118 of 800 tiles are
    /// excluded — 14.75% of the plate, not the "12 of 800" an earlier
    /// version of this doc claimed.** That number was a review-caught
    /// mistake, not a rounding difference: 12 is the count of excluded
    /// tiles whose vertex ALSO happens to resolve to ocean — i.e. the
    /// subset that would actually have disagreed and reddened the test —
    /// mismeasured as the exclusion's own size. The other 106 excluded
    /// tiles draw a site glyph over non-ocean terrain and would have agreed
    /// anyway; excluding them changes no verdict TODAY, but they are still
    /// genuinely outside what this test can vouch for, which is why
    /// `EXCLUDED_TILES` below is a pinned count and not merely a printed
    /// one — a regression that grew the excluded set (say, a bug drawing
    /// site glyphs far more broadly than the roster warrants) would
    /// otherwise silently shrink the guarantee while `agree == total` kept
    /// reporting a perfect, and decreasingly meaningful, ratio.
    #[test]
    fn f5_the_resolved_vertex_always_matches_the_drawn_glyph_after_the_fix() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        // `world_plate` takes the RAW terminal size — see the fine-zoom
        // ground-truth test's own comment for why passing the already-fitted
        // `active_plate_dims()` output here instead was a real bug in this
        // task's first fix-round attempt (it silently rendered a SMALLER,
        // mismatched grid, and the F5 ratio barely moved as a result).
        let grid = d.world_plate(d.term_w, d.term_h);
        let (plate_w, plate_h) = d.active_plate_dims();
        assert_eq!((grid.width(), grid.height()), (plate_w, plate_h));
        let (virtual_w, virtual_h) = plate::virtual_dims(d.window.depth);

        // Point-site and landform glyphs are not terrain claims at all (see
        // this test's own doc) — excluded from the comparison rather than
        // silently counted as agreement or disagreement.
        let site_glyphs = [
            plate::CAVE_GLYPH,
            plate::EXOTIC_GLYPH,
            plate::SETTLEMENT_MINOR_GLYPH,
            plate::SETTLEMENT_MAJOR_GLYPH,
            plate::VOLCANO_GLYPH,
            plate::WATERFALL_GLYPH,
        ];

        let mut agree = 0u32;
        let mut total = 0u32;
        let mut excluded = 0u32;
        let mut memo = hornvale_kernel::RoomMeshMemo::default();
        for y in 0..plate_h {
            for x in 0..plate_w {
                let drawn_glyph = grid.get(x, y).and_then(|c| c.glyph);
                if drawn_glyph.is_some_and(|g| site_glyphs.contains(&g)) {
                    excluded += 1;
                    continue;
                }
                let vertex = plate::terrain_at_tile(
                    &d.terrain,
                    &d.geo,
                    &d.nearest,
                    &mut memo,
                    &d.frame,
                    &d.window,
                    virtual_w,
                    virtual_h,
                    u32::from(y),
                    u32::from(x),
                    None,
                    hornvale_kernel::WorldTime::GENESIS,
                    0,
                    None,
                )
                .vertex;
                let resolved_ocean = d.terrain.is_ocean(vertex);

                let drawn_ocean = drawn_glyph == Some('~');
                total += 1;
                if resolved_ocean == drawn_ocean {
                    agree += 1;
                }
            }
        }
        assert!(total > 0);
        let ratio = f64::from(agree) / f64::from(total);
        println!(
            "F5 (fix round 1): the resolved vertex's class agrees with the drawn \
             glyph on {agree}/{total} = {ratio:.4} of the {plate_w}x{plate_h} \
             floor plate at the coarsest zoom, excluding {excluded} point-site/landform \
             tiles (the pre-fix figure once printed here, \
             420/800 = 0.5250, is RETRACTED as measured on a misaligned 32x16-vs-40x20 \
             harness bug; the pre-fix rate is unmeasured, not smaller)"
        );
        assert_eq!(
            agree, total,
            "Finding 2's fix guarantees this by construction: `terrain_at_tile` only ever \
             returns a representative of the PAINTED class, so the resolved vertex can \
             never disagree with the glyph drawn from that same read — a non-1.0 ratio \
             here means the fix itself is broken"
        );
        // PINNED, not merely printed (review fix round 1): an unasserted
        // `excluded` can grow without bound and this test would keep
        // reporting a perfect ratio over a shrinking, decreasingly
        // meaningful `total`. 118 is this test's own doc's measured figure
        // for seed 42's default floor plate at the coarsest zoom — a
        // golden that moves on a terrain epoch, a site-roster change
        // (caves/exotic/settlements) or a site-glyph vocabulary change,
        // every one of which is a change somebody should look at, same as
        // `the_site_roster_carries_every_kind_and_only_placed_kinds_carry_
        // a_facet`'s own three golden counts.
        assert_eq!(
            excluded, 118,
            "the point-site/landform exclusion moved — update this test's own doc \
             (and re-measure, do not just paste the new number) if this is expected"
        );
    }

    // -- Task 4, Step 1: the containment chain -----------------------------

    /// The strip carries the whole containment chain, most specific first
    /// (§5), not only the most specific feature — the cut §5 withdraws.
    ///
    /// **Not seed 42's default flagship position** — that position turned
    /// out (found by running this, not by reasoning about it) to resolve
    /// to a SINGLE-feature vertex, "Vngashngatva (a landmass)": the design
    /// spec's own illustrative example text ("Vngashngatva (a volcano),
    /// on Kxsokxkxzhakx (a landmass)", §5) uses the same name for a
    /// volcano that this real seed-42 world gives its landmass — a
    /// coincidence of the example's own invented names, not a fact about
    /// this fixture. So this test SEARCHES the coarsest (whole-planet)
    /// world view for a real multi-feature vertex, through the actual
    /// `Driver::world_view_tile`/`resolve_chain_at` production path,
    /// rather than assuming one at a fixed position.
    #[test]
    fn the_strip_carries_the_whole_containment_chain_most_specific_first() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        assert_eq!(
            d.window.depth, GLOBE_RUNG,
            "sanity: the coarsest rung the ladder reaches"
        );

        let (plate_w, plate_h) = d.active_plate_dims();
        // Since Task 1 the plate no longer holds the whole planet at the
        // coarsest rung, so the search below covers one screen's worth of a
        // 363x362 chart — put that screen somewhere the world actually has
        // features stacked on features.
        centre_window_on_the_player(&mut d, plate_w, plate_h);
        let mut found: Option<hornvale_kernel::Vertex> = None;
        'search: for y in 0..plate_h {
            for x in 0..plate_w {
                d.cursor = hornvale_game_core::Cursor { x, y };
                let vertex = d.world_view_tile().vertex;
                if d.index.at(vertex).len() >= 2 {
                    found = Some(vertex);
                    break 'search;
                }
            }
        }
        let vertex = found.expect(
            "seed 42's real world has at least one multi-feature vertex reachable at the \
             coarsest zoom (the cursor was left at that position by the search above)",
        );
        let stack: Vec<_> = d.index.at(vertex).to_vec();
        assert!(
            stack.len() >= 2,
            "sanity: the found vertex is genuinely multi-feature"
        );

        // Task 5's real discovery gate means MERELY POINTING the cursor at
        // `vertex` (what the search above does) no longer discovers anything
        // — that is the whole point of the gate (co-location is not
        // discovery, and even the cursor's own gaze is a form of
        // co-location, not encounter). This test's own purpose is the
        // chain's ORDER and CONTENT once every link IS discovered, which is
        // orthogonal to the gate itself (H6b, elsewhere, is what tests the
        // gate); simulate that every link has been encountered so the
        // assertions below exercise what they always meant to.
        for id in &stack {
            d.discovered.record(FeatureId::Extent(*id));
        }

        d.refresh_strip();
        let s = d
            .strip_text()
            .expect("the world view always resolves once active")
            .to_string();

        let (species, ph, morph) = d.namer.clone();
        let mut last_pos = 0usize;
        for id in &stack {
            let name = hornvale_worldgen::feature_name(d.seed, *id, &species, &ph, &morph).roman;
            let pos = s
                .find(&name)
                .unwrap_or_else(|| panic!("{name:?} missing from chain {s:?}"));
            assert!(
                pos >= last_pos,
                "chain out of order: {name:?} at byte {pos} precedes byte {last_pos} in {s:?}"
            );
            last_pos = pos;
        }
    }

    // -- Task 4, Step 3 / §3.3: the clamp caption names the central line --

    /// The world view's strip carries §3.3's caption. Seed 42's flagship
    /// world is spinning (the default `SkyPins`, no `RotationPin::Locked`),
    /// so `Driver::frame` holds the geographic equator — the spinning arm
    /// of `mercator::clamp_caption`'s own two regimes. **The locked arm is
    /// pinned separately, at the pure-function level
    /// (`mercator::the_clamp_caption_names_the_terminator_on_a_locked_
    /// world`)**: `Driver::start` has no rotation-pin entry point (a
    /// `for_test` seam to add one was rejected for a single test — see
    /// `map_focus_at_an_unresolved_band_refuses_rather_than_resolving`'s
    /// own comment in `tests/driver.rs`), so a real end-to-end locked
    /// `Driver` is not reachable without that seam. `mercator::frame_for`
    /// is pure and exactly what `Driver::start` calls, so exercising it
    /// directly is the same physics, not a weaker proxy.
    #[test]
    fn the_world_view_strip_names_the_equator_on_the_spinning_default_world() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        let s = d
            .strip_text()
            .expect("the world view always resolves once active");
        assert!(
            s.contains("equator"),
            "spec §3.3: the caption must name the central line, got {s:?}"
        );
        assert!(
            !s.contains("terminator"),
            "a spinning world's central line is the equator, not the terminator, got {s:?}"
        );
    }

    /// Fix round 1 (reviewer finding 1, CRITICAL): the reviewer's own
    /// repro — the seed-42 SPINNING default world, cursor moved off the
    /// equator, then one ordinary `.` (`Action::Recentre`, a spec-
    /// mandated §3.2 gesture) — used to make the strip state "clamped ...
    /// from the terminator — the substellar desert and antistellar ice
    /// are off the map" on a world with none of those things. This pins
    /// the fix end-to-end, through the real `Driver::recentre` path, not
    /// only against `mercator::clamp_caption` called directly (that half
    /// lives in `mercator.rs`'s own
    /// `a_recentred_spinning_frame_claims_neither_the_equator_nor_the_
    /// terminator`).
    #[test]
    fn recentring_a_spinning_world_off_the_equator_never_claims_the_terminator() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        // The default cursor position at zoom 0 sits near the plate's own
        // vertical centre, which projects near the equator itself
        // (`mercator::centre_on_the_equator_itself_is_well_defined`'s own
        // finding) — recentring there would be close to a no-op, so this
        // moves the cursor off it first, the same way the reviewer's own
        // repro did.
        d.apply(Action::CursorBy(0, 5));
        d.apply(Action::Recentre);
        let s = d
            .strip_text()
            .expect("the world view always resolves once active");
        assert!(
            !s.contains("terminator"),
            "a spinning world has no terminator, recentred or not, got {s:?}"
        );
        assert!(
            !s.contains("substellar") && !s.contains("antistellar"),
            "no locked-world claim may leak into a recentred spinning world's strip, got {s:?}"
        );
        assert!(
            !s.contains("desert"),
            "a spinning world has no substellar desert, got {s:?}"
        );
    }

    // -- Task 4, Step 4 / F5: the resolution disclosure -------------------

    /// F5, RESTATED FOR THE MESH LADDER: the strip discloses a resolution
    /// only where one character stands for MORE than one terrain vertex,
    /// and since The Quadrat no shipped rung does.
    ///
    /// **The coarse half of this test was a real assertion and is now a
    /// falsified one, so it is inverted rather than deleted.** It used to
    /// read "the coarsest zoom must disclose": the chart was the plate,
    /// 40x20 = 800 characters for 40,962 vertices, ~51 vertices apiece.
    /// The chart is the RUNG now, and [`GLOBE_RUNG`] is the canonical grid
    /// level itself (decision 0196) — 363x362 tiles — so even the coarsest
    /// rung is finer than the mesh and the honest disclosure is silence.
    /// See [`Driver::resolution_disclosure`]'s own doc. What this test
    /// still pins is that the derivation runs at every rung the raster
    /// draws and agrees; a rung coarser than the mesh would make it speak.
    ///
    /// **Widened by The Quadrat's Task 2 rather than retargeted.** It used
    /// to read the two ENDS of the ladder, and band B's end now draws the
    /// walk band's chart instead — whose strip never carries a disclosure
    /// for reasons that have nothing to do with the ratio, so asserting
    /// there would have gone quietly vacuous. Walking the raster's whole
    /// range instead keeps every assertion pointed at the derivation under
    /// test.
    #[test]
    fn the_resolution_disclosure_is_silent_at_every_shipped_rung() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        assert_eq!(d.window.depth, GLOBE_RUNG, "sanity: the coarsest rung");

        let mut rungs_checked = 0;
        loop {
            assert!(
                !d.at_walk_band_rung(),
                "sanity: rung {} must be a raster rung",
                d.window.depth
            );
            let said = d
                .strip_text()
                .expect("the world view always resolves once active");
            // Matched on the disclosure's opening clause, never its noun --
            // see `DISCLOSURE_OPENING`.
            assert!(
                !said.contains(DISCLOSURE_OPENING),
                "rung {} is at or finer than the mesh's own level and has \
                 nothing to disclose, got {said:?}",
                d.window.depth
            );
            rungs_checked += 1;
            if d.window.depth == BAND_B_RUNG - 1 {
                break;
            }
            d.apply(Action::Zoom(1));
        }
        assert_eq!(
            rungs_checked,
            BAND_B_RUNG - GLOBE_RUNG,
            "every rung the raster draws must have been read"
        );
    }

    /// The disclosure's `Some(...)` branch — the half the inversion above
    /// left with no test in either direction.
    ///
    /// Depth 5 is BELOW the shipped ladder's floor and reachable only by
    /// constructing a [`Window`] directly, which is exactly the point: what
    /// is under test is the ratio arithmetic, not the ladder. A rung-5 chart
    /// is 128x128 tiles for seed 42's 40,962 vertices — 2.501 vertices per
    /// character — so the strip must say so, through the same `strip_text`
    /// instrument its silent sibling above reads.
    ///
    /// **It was 182x181 (1.243 per character) until fix round 1**, when
    /// `plate::base_facet_arc_rad` stopped returning the ICOSAHEDRON's edge
    /// angle on a cube-sphere mesh. The chart is `4 * 2^depth` columns wide
    /// now, exactly, so a rung-5 chart is 128 wide rather than 182 and this
    /// rung is coarser than the mesh by MORE than it used to claim — which
    /// only strengthens what this test is here for.
    #[test]
    fn the_resolution_disclosure_speaks_at_a_rung_coarser_than_the_mesh() {
        let mut d = test_driver();
        enter_world_view(&mut d);

        let (w, h) = plate::virtual_dims(5);
        assert_eq!((w, h), (128, 128), "sanity: the rung-5 chart");
        let ratio = d.geo.vertex_count() as f64 / (u64::from(w) * u64::from(h)) as f64;
        assert!(
            ratio > 1.0,
            "sanity: rung 5 must be coarser than the mesh, got {ratio}"
        );

        d.window = Window {
            depth: 5,
            origin_col: 0,
            origin_row: 0,
        };
        d.refresh_strip();
        let said = d
            .strip_text()
            .expect("the world view always resolves once active");
        assert!(
            said.contains(DISCLOSURE_OPENING),
            "a rung coarser than the mesh ({ratio:.3} vertices per character) must \
             disclose its resolution, got {said:?}"
        );
    }

    // -- Task 4, Step 2 / F3: scrolling is driven by the redraw counter,
    //    never a clock -----------------------------------------------------

    /// `Driver::strip_offset` advances as the client redraws (here: cursor
    /// moves), and stays `0` once nothing is left to scroll (`refresh_strip`
    /// still advances `redraw_count`, but `strip_offset` clamps to `0` once
    /// `overflow` is `0`). Exercised against the CHAMBER band, whose strip
    /// is the short, fixed [`NOTHING_HERE_YET`] refusal (no resolver exists
    /// there yet — the module doc) rather than the walk band's own: Task 4
    /// made the walk band's default seed-42 position resolve to the FULL
    /// containment chain ("Vngashngatva (a volcano), on Kxsokxkxzhakx (a
    /// landmass)"), which is itself longer than the 40-column floor plate —
    /// confirmed by `strip_offset_is_a_pure_function_of_actions_taken_not_
    /// of_time_elapsed` below actually scrolling — so the walk band is no
    /// longer a text that fits, and using it here would pin a premise Task
    /// 4 itself falsified.
    /// THE BUG, AS A TEST. The marquee used to advance on `redraw_count`,
    /// so it moved only when the player pressed a key — Nathan, in play:
    /// "the marquee text scrolls on the actions I take, not at a smooth,
    /// steady background rate." Ticks are now the driver, so the offset
    /// must move with NO action at all.
    #[test]
    fn the_marquee_advances_on_ticks_with_no_player_action() {
        let mut d = test_driver();
        d.strip = Some("x".repeat(400));
        let before = d.strip_offset();
        let redraws_before = d.redraw_count;

        for _ in 0..3 {
            d.tick_marquee();
        }

        assert_ne!(
            d.strip_offset(),
            before,
            "three ticks did not move the marquee"
        );
        assert_eq!(
            d.redraw_count, redraws_before,
            "a tick is not a redraw and must not be counted as one"
        );
    }

    /// The converse, and the one that would catch a regression to the old
    /// behaviour: an ACTION alone must no longer move the marquee.
    #[test]
    fn a_player_action_alone_does_not_move_the_marquee() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        d.strip = Some("x".repeat(400));
        let before = d.strip_offset();
        for _ in 0..5 {
            d.apply(Action::CursorBy(1, 0));
        }
        d.strip = Some("x".repeat(400)); // refresh_strip overwrote it
        assert_eq!(
            d.strip_offset(),
            before,
            "cursor moves scrolled the marquee — it is back on redraw_count"
        );
    }

    /// An idle client must not be woken to animate nothing: the loop only
    /// polls with a timeout while this is true.
    #[test]
    fn a_strip_that_fits_is_not_scrolling() {
        let mut d = test_driver();
        d.strip = Some("short".to_string());
        assert!(
            !d.strip_is_scrolling(),
            "a fitting strip must not spin the loop"
        );
        d.strip = Some("x".repeat(400));
        assert!(d.strip_is_scrolling(), "an overflowing strip must scroll");
        d.strip = None;
        assert!(!d.strip_is_scrolling(), "no strip is not scrolling");
    }

    #[test]
    fn strip_offset_stays_zero_when_the_text_fits_the_plate() {
        let mut d = test_driver();
        // `handle`'s bool return is whether the possession RELEASED, not
        // whether the verb succeeded, so the real check is the snapshot's
        // own `spatial` tag just below.
        d.handle("enter");
        let snap = hornvale_game_core::Snapshot::parse(&d.cached)
            .expect("a live session always yields a parseable snapshot");
        assert!(
            matches!(snap.spatial, hornvale_game_core::Spatial::Chamber { .. }),
            "seed 42's flagship must land in the chamber band after one `enter`"
        );
        d.enter_map();
        // UNCHANGED, and briefly wasn't (fix round 1's F3/F5): Task 6 handed
        // the chamber band the raster, which made this a long containment
        // chain instead of a short refusal, and this test was retargeted to a
        // 240-column terminal to keep its premise. F5 restored the chamber's
        // own renderer, so the short refusal is back and so is the premise.
        assert_eq!(
            d.strip_text(),
            Some(NOTHING_HERE_YET),
            "sanity: the chamber band has no resolver, so the strip is the short refusal"
        );
        let before = d.redraw_count;
        assert_eq!(
            d.strip_offset(),
            0,
            "the chamber band's strip fits the plate at the floor"
        );
        d.apply(Action::CursorBy(1, 0));
        assert!(
            d.redraw_count > before,
            "a cursor move is a redraw, and must advance the F3 counter"
        );
        assert_eq!(
            d.strip_offset(),
            0,
            "still nothing to scroll, regardless of how many redraws happened"
        );
    }

    /// F3 is driven by `redraw_count`, never a clock: two `Driver`s built
    /// identically and driven through the identical sequence of actions
    /// must report the identical `strip_offset` every step — if a clock
    /// were involved, real wall-clock skew between the two constructions
    /// (however small) would risk observable divergence. This is the
    /// closest thing to a direct clock-absence proof available without
    /// reaching into `main.rs`'s own event loop (which needs a real
    /// terminal and is explicitly off-limits to a test — see the module
    /// constraint on never launching the TUI binary).
    #[test]
    fn strip_offset_is_a_pure_function_of_actions_taken_not_of_time_elapsed() {
        let mut a = test_driver();
        let mut b = test_driver();
        a.enter_map();
        // Real wall-clock time passes here, between constructing `a` and
        // `b`'s identical sequence and reading `a`'s own first offset --
        // if `strip_offset` depended on a clock, this gap could already
        // have moved `a` off whatever `b` reports below.
        std::thread::yield_now();
        b.enter_map();
        assert_eq!(a.strip_offset(), b.strip_offset());
        for _ in 0..5 {
            a.apply(Action::CursorBy(1, 0));
            b.apply(Action::CursorBy(1, 0));
            assert_eq!(
                a.strip_offset(),
                b.strip_offset(),
                "two drivers given the identical action sequence must report the identical \
                 scroll offset -- a clock in the loop would risk this drifting"
            );
        }
    }

    // -- Task 5: discovery (H5, H6, H6b, H7) --------------------------------

    /// H5 — the map is useful before it is complete: from a cold start,
    /// before a single verb has been sent, the whole-planet plate still
    /// draws BOTH a land glyph and an ocean glyph — a legible coastline,
    /// not a monochrome blob. Task 5 adds POINT-SITE gating on top of the
    /// terrain glyphs Task 2/3 already draw; this test's own job is only
    /// to confirm that gating never degrades what was already there. The
    /// stronger terrain-fidelity claim is Amendment 1's own H8
    /// (§A10b/§A11, measured independently against a finer probe) — out
    /// of this task's scope to re-litigate.
    #[test]
    fn h5_the_map_is_useful_before_it_is_complete() {
        let mut d = test_driver();
        // The session now OPENS on band B (The Quadrat, Task 2), whose
        // chart is 23,214 tiles wide — a 40x20 subrect of it is a few
        // hundred metres of whatever the player is standing on, and the
        // claim under test is about the WHOLE-PLANET plate. Walk out to the
        // coarsest rung first; that is what "the whole-planet plate" has
        // named since Task 1 moved the chart onto the mesh.
        enter_world_view(&mut d);
        // The plate is a SUBRECT of the chart since Task 1, so "the plate"
        // is not a place until the window says which one. Look where the
        // player is standing.
        //
        // THE TWO CALLS TAKE DIFFERENT UNITS, and an earlier revision passed
        // `40, 20` to both (Task 9): `centre_window_on` wants the PLATE's
        // own size, `world_plate` wants the TERMINAL's. They agreed by
        // accident while the width rule could never exceed
        // `GLYPH_ASPECT * content_height(h)` — `world_plate_dims(40, 20)`
        // happened to come back 32x16, near enough to 40x20 for the
        // assertions below. Task 9's `MIN_ENTRY_WIDTH` ceiling makes a
        // 40-column TERMINAL yield a zero-column plate, which is the honest
        // answer for a terminal below the client's own 80x24 floor. So the
        // terminal is now stated as the floor, whose plate is exactly the
        // 40x20 this test always meant.
        let (plate_w, plate_h) = Driver::world_plate_dims(
            hornvale_game_core::MIN_WIDTH,
            hornvale_game_core::MIN_HEIGHT,
        );
        centre_window_on_the_player(&mut d, plate_w, plate_h);
        let g = d.world_plate(
            hornvale_game_core::MIN_WIDTH,
            hornvale_game_core::MIN_HEIGHT,
        );
        let text = g.to_plain_text();
        // The Legend (Task 6) retired the `~` ocean / `.` land binary for a
        // water-class-and-elevation-band vocabulary (`plate::
        // glyph_and_color_for`), so "shows land" is no longer one hardcoded
        // character — it is any drawn glyph that is not the ocean mark.
        assert!(
            text.contains('~'),
            "a cold-start plate must still show ocean: {text:?}"
        );
        assert!(
            text.chars().any(|c| !c.is_whitespace() && c != '~'),
            "a cold-start plate must still show land: {text:?}"
        );
    }

    /// H6 — discovery is monotonic: once a feature is discovered, it
    /// stays discovered for the rest of the session, through further real
    /// turns. The possession's own starting vertex discovers whatever
    /// extent features it stands on immediately (§A4b: standing on the
    /// ground IS meeting it) — this test walks several real turns after
    /// that and confirms nothing already known is ever un-known. The
    /// OTHER direction ("nothing is named before it is encountered") is
    /// H6b's own claim, checked there against a feature genuinely never
    /// encountered.
    #[test]
    fn h6_discovery_is_monotonic() {
        let mut d = test_driver();

        let start_coord = d.session.position().coord();
        let start_vertex = d
            .nearest
            .nearest(&d.geo, start_coord.latitude, start_coord.longitude);
        let stack: Vec<_> = d.index.at(start_vertex).to_vec();
        assert!(
            !stack.is_empty(),
            "sanity: seed 42's flagship starts within at least one extent feature"
        );

        for id in &stack {
            assert!(
                d.discovered.contains(FeatureId::Extent(*id)),
                "the starting vertex's own extent features must be discovered from turn one"
            );
        }

        for dir in ["go n", "go s", "go e", "go w"] {
            d.handle(dir);
            for id in &stack {
                assert!(
                    d.discovered.contains(FeatureId::Extent(*id)),
                    "a feature discovered earlier must stay discovered after {dir:?}"
                );
            }
        }
    }

    /// H6b — co-location does not discover. **The test this task exists
    /// for.** Seed 42's flagship starts at a vertex where `enter` succeeds
    /// immediately (established elsewhere by
    /// `strip_offset_stays_zero_when_the_text_fits_the_plate`) — i.e. a
    /// real settlement vertex — so this needs no hand-built world. The
    /// possession walks PAST it (several real `go` turns, in and out)
    /// without ever issuing `enter`, and the settlement must stay
    /// undiscovered — checked against the real `Discovered` state. A
    /// positive control on a fresh, identically-seeded driver proves the
    /// DISCOVERY-RECORDING mechanism can fire at all.
    ///
    /// **This test used to also assert the settlement's glyph never
    /// appeared on the drawn plate at any zoom, and that assertion is
    /// GONE, not merely relaxed.** The Prospect's Gate A ungating made it
    /// false on purpose (Nathan's ruling: "show placed sites on the world
    /// map... just don't show their labels") — a settlement now draws
    /// whether or not it has been discovered, so "co-location does not
    /// disclose" is a claim about the settlement's NAME now, never its
    /// glyph. What replaces the old glyph-absence loop is
    /// `site_drawing_never_depends_on_discovery_and_the_cursor_never_
    /// leaks_a_name` below, which pins both halves of the new claim
    /// together (drawn regardless of discovery; the cursor readout
    /// unaffected by that same discovery) across all three site kinds,
    /// on sites CENTRED so their glyph is provably on screen — closing the
    /// old test's own final-review finding 8 (this settlement's glyph was
    /// undrawable at the default small window, at any zoom, discovered or
    /// not, which made the old loop vacuous evidence either way).
    #[test]
    fn h6b_co_location_does_not_discover_a_settlement() {
        let mut d = test_driver();

        let start = d.session.position();
        let coord = start.coord();
        let vertex = d.nearest.nearest(&d.geo, coord.latitude, coord.longitude);
        assert!(
            d.sites.iter().any(|site| site.vertex == vertex
                && site.kind == hornvale_vessel::site::SiteKind::Settlement),
            "sanity: seed 42's flagship starts at a settlement vertex"
        );
        let site = FeatureId::Settlement(vertex);

        // We were there — `Visited` records the starting room the instant
        // `Driver::start` runs its own initial `refresh`, before any
        // movement at all.
        assert!(
            d.visited.contains_at_rung(&start, start.depth()),
            "the possession's own starting room must read as visited"
        );
        assert!(
            !d.discovered.contains(site),
            "merely starting at a settlement vertex must not discover it"
        );

        // Walk PAST it: several real `go` turns, in and out — never
        // `enter`. Not every bearing has an exit at every room reached,
        // so several are tried; the assertion holds after each one
        // regardless of whether that particular bearing moved anything.
        for dir in ["go n", "go s", "go e", "go w", "go n", "go s"] {
            d.handle(dir);
            assert!(
                !d.discovered.contains(site),
                "walking without entering must never discover the settlement (after {dir:?})"
            );
        }

        // Positive control, on a FRESH identically-seeded driver: `enter`
        // from this exact starting position DOES discover the settlement
        // — proving the negative checks above are not vacuous.
        let mut fresh = test_driver();
        let fresh_coord = fresh.session.position().coord();
        let fresh_vertex =
            fresh
                .nearest
                .nearest(&fresh.geo, fresh_coord.latitude, fresh_coord.longitude);
        assert_eq!(
            fresh_vertex, vertex,
            "sanity: two seed-42 flagships start at the identical vertex"
        );
        fresh.handle("enter");
        assert!(
            fresh
                .discovered
                .contains(FeatureId::Settlement(fresh_vertex)),
            "sanity: `enter` must actually discover the settlement it succeeds at"
        );
    }

    /// **The pair, pinned together (The Prospect, Gate A/B — the coordinator's
    /// own correction to the campaign's premise): a placed site's glyph
    /// draws whether or not it has been discovered, and discovering it
    /// changes NOTHING the cursor readout can say.** Nathan's ruling: "We
    /// can say it's a cave, a village, etc, just don't give its name."
    /// Nothing pinned this pairing as one invariant before this test — the
    /// glyph half is covered piecemeal elsewhere (`plate.rs`'s
    /// `the_feature_layer_draws_every_site_whether_or_not_it_is_discovered`,
    /// this file's own `standing_in_a_placed_sites_room_discovers_it_and_
    /// the_map_keeps_drawing_it`) and the readout half was never covered at
    /// all.
    ///
    /// **Why "changes nothing" is provable rather than merely observed.**
    /// [`Driver::resolve_world_view`]/[`Driver::resolve_walk_band`] — the
    /// cursor readout's only two producers — resolve a vertex against
    /// `self.index`, a `VertexFeatureIndex` built purely from
    /// `hornvale_terrain::landscape` EXTENT features (a volcano, a landmass,
    /// a sea, a salt lake, a river). A cave, an exotic site or a settlement
    /// is none of those — `self.sites`, the roster [`plate::draw_feature_
    /// layer`] draws from, is a wholly separate structure `self.index` never
    /// reads — so the `is_discovered` closure the readout asks
    /// (`|id| self.discovered.contains(FeatureId::Extent(id))`,
    /// `windows/worldgen::resolve_chain_at`) can only ever be asked about an
    /// `Extent` id, and this test injects discovery through
    /// `FeatureId::Cave`/`Exotic`/`Settlement` — a different enum variant
    /// entirely. So the readout's own text is a pure function of `self.index`
    /// and the cursor position, structurally blind to whichever of these
    /// three site kinds sits at that vertex; the before/after equality
    /// below is that argument, run.
    ///
    /// **Centred on the site's own coordinate** ([`centre_on_a_placed_site`]),
    /// not left at the driver's default window — the OLD `h6b`'s own
    /// finding 8 measured that the flagship's starting settlement was
    /// undrawable at the default 40x20 window at ANY shipped rung, which
    /// would make "the glyph is drawn" vacuously true or false regardless of
    /// discovery. Centring removes that confound: the glyph's presence here
    /// is evidence about Gate A, not about window placement.
    #[test]
    fn site_drawing_never_depends_on_discovery_and_the_cursor_never_leaks_a_name() {
        use hornvale_vessel::site::SiteKind;

        let (w, h) = (104u16, 56u16);
        for kind in [SiteKind::Cave, SiteKind::Exotic, SiteKind::Settlement] {
            let mut d = test_driver();
            let (plate_w, plate_h) = Driver::world_plate_dims(w, h);
            let site = centre_on_a_placed_site(&mut d, kind, plate_w, plate_h);
            assert!(
                !d.discovered().contains(site.feature_id()),
                "guard: the {kind:?} must not already read as discovered"
            );

            // Find the exact screen position the site's own glyph occupies
            // — searched, not computed from the projection math a second
            // time, so a disagreement between the two would show up as
            // "glyph never found" rather than silently comparing the wrong
            // tile.
            let glyph_matches = |g: Option<char>| match kind {
                SiteKind::Cave => g == Some(plate::CAVE_GLYPH),
                SiteKind::Exotic => g == Some(plate::EXOTIC_GLYPH),
                SiteKind::Settlement => {
                    g == Some(plate::SETTLEMENT_MINOR_GLYPH)
                        || g == Some(plate::SETTLEMENT_MAJOR_GLYPH)
                }
            };
            let grid = d.world_plate(w, h);
            let mut found: Option<(u16, u16)> = None;
            'search: for y in 0..plate_h {
                for x in 0..plate_w {
                    if glyph_matches(grid.get(x, y).and_then(|c| c.glyph)) {
                        found = Some((x, y));
                        break 'search;
                    }
                }
            }
            let (x, y) =
                found.expect("an UNdiscovered, centred site must be drawn — Gate A ungating");

            d.cursor = hornvale_game_core::Cursor { x, y };
            let before = d.resolve_world_view();

            // ABSENCE, not just invariance (review fix round 1: the
            // original version of this test asserted only `before ==
            // after`, which a NAME THAT LEAKS IN BOTH STATES also
            // satisfies — proved by mutation, see this test's own doc).
            // A settlement is the only `SiteKind` with a real committed
            // name (`windows/vessel/src/brief.rs` passes `None` for both
            // a cave and an exotic site), so this only fires for it;
            // `settlement_name_for_test` returns `None` for the other two
            // kinds' vertices and the check is skipped rather than
            // vacuously passing on an absent name.
            if let Some(name) = d.settlement_name_for_test(site.vertex) {
                assert!(
                    !name.is_empty(),
                    "sanity: the settlement's own committed name must not be empty, or the absence check below is vacuous"
                );
                let text = before.clone().unwrap_or_default();
                assert!(
                    !text.contains(&name),
                    "the {kind:?}'s own proper name {name:?} is reachable through the cursor readout while undiscovered: {text:?}"
                );
            } else if kind == SiteKind::Settlement {
                panic!(
                    "sanity: a centred, real settlement site must resolve to a real committed name, or the absence check above never ran"
                );
            }

            d.discovered_mut_for_test().record(site.feature_id());
            assert!(
                d.discovered().contains(site.feature_id()),
                "sanity: the injected discovery must actually be recorded"
            );
            let after = d.resolve_world_view();

            // The absence check restated after discovery: Gate B has no
            // mechanism that could reveal a placed site's name at all
            // (Decision 0670's own table — the cursor readout's "name
            // withheld" row carries no discovered/undiscovered split), so
            // this must hold in both states, not just the one a reader
            // might expect to be interesting.
            if let Some(name) = d.settlement_name_for_test(site.vertex) {
                let text = after.clone().unwrap_or_default();
                assert!(
                    !text.contains(&name),
                    "the {kind:?}'s own proper name {name:?} is reachable through the cursor readout after discovery: {text:?}"
                );
            }

            // Kept as a cheap regression guard (per review): a fine
            // property on its own, just not the one this test's name
            // promises — that is the absence check above.
            assert_eq!(
                before, after,
                "discovering the {kind:?} changed the cursor readout at its own screen position — a name (or a fact derived from one) leaked through Gate B"
            );

            // STRONGER STILL, and the check that actually falsifies the
            // review's own reproduction. The name-absence check above can
            // only catch a leak of the REAL name; the review's own mutation
            // injects an unconditional but FABRICATED string
            // (`format!("Kxarrabeth-{:?}", site.kind)`) keyed on
            // `self.sites.iter().find(|s| s.vertex == vertex_id)` — content
            // that contains no real name at all, so neither the absence
            // check nor the before/after equality above can see it (an
            // unconditional fabrication is trivially invariant too). What
            // DOES falsify it: the readout must be unaffected by whether
            // this site is in the roster AT ALL, which is the actual
            // structural property `resolve_world_view`/`resolve_walk_band`
            // are supposed to have (see this test's own doc on why —
            // `self.index` never reads `self.sites`). Removing the site
            // from `self.sites` and re-resolving must reproduce `before`
            // exactly; if a mutation reads `self.sites` for anything, this
            // is what catches it regardless of what it injects.
            let mut without_site = d.sites.clone();
            without_site.retain(|s| s.feature_id() != site.feature_id());
            let with_site = std::mem::replace(&mut d.sites, without_site);
            let baseline = d.resolve_world_view();
            d.sites = with_site;
            assert_eq!(
                baseline, before,
                "the cursor readout changed depending on whether the {kind:?} was even in the roster — Gate B must be structurally blind to `self.sites`, not merely to its discovery state"
            );

            // The glyph itself is unmoved too, for the same reason
            // `the_feature_layer_draws_every_site_whether_or_not_it_is_
            // discovered` asserts it in `plate.rs`: restated here so the
            // pairing this test exists to pin is visible in one place.
            let after_grid = d.world_plate(w, h);
            assert_eq!(
                grid.to_plain_text(),
                after_grid.to_plain_text(),
                "discovering the {kind:?} moved the drawn plate"
            );
        }
    }

    /// H7 — the gate costs nothing in the ledger: a possession that opens
    /// the world map and interacts with it produces a BYTE-IDENTICAL
    /// sequence of `vessel/session/v2` snapshots to one that takes the
    /// identical real turns and never touches the map at all.
    /// `Visited`/`Discovered` never call into `self.session` except
    /// through accessors the client already reads elsewhere
    /// (`session.position()`), so this is expected to hold by construction —
    /// this test is the preregistered evidence, not a tuning knob. A
    /// difference here is a STOP (spec Amendment 1's own H7 doc), not
    /// something to patch.
    #[test]
    fn h7_the_map_gate_costs_nothing_in_the_ledger() {
        let mut plain = test_driver();
        let mut mapped = test_driver();
        assert_eq!(
            plain.cached, mapped.cached,
            "sanity: two seed-42 flagships start identically"
        );

        for dir in ["go n", "go s", "go e", "go w", "go n", "go s"] {
            // Pure client-side map interaction between every real turn —
            // none of it may touch the session.
            mapped.enter_map();
            mapped.apply(Action::Zoom(-1));
            mapped.apply(Action::CursorBy(5, 3));
            mapped.apply(Action::Recentre);
            for _ in 0..(BAND_B_RUNG - GLOBE_RUNG) {
                mapped.apply(Action::Zoom(1));
            }
            // Terminal dims, not plate dims (Task 9 — see
            // `h5_the_map_is_useful_before_it_is_complete`).
            let _ = mapped.world_plate(
                hornvale_game_core::MIN_WIDTH,
                hornvale_game_core::MIN_HEIGHT,
            );
            let _ = mapped.resolve_world_view();

            plain.handle(dir);
            mapped.handle(dir);

            assert_eq!(
                plain.cached, mapped.cached,
                "opening and using the world map must never change what the session \
                 reports for the identical real turn {dir:?}"
            );
        }
    }

    // -- Task 6: band B joins the raster ladder --------------------------
    //
    // THE MEASUREMENT THAT RE-PLANNED THIS TASK, so the tests below are
    // pointed at it. The first design placed this overlay from the wire's
    // own polar pair (`bearing_deg`/`distance_rad`) the way
    // `core/src/chart.rs` places its chart. That cannot land on the
    // raster's squares AS `core` STANDS: the raster's tile index is `floor`
    // of an ABSOLUTE Mercator coordinate, a polar pair gives a RELATIVE
    // offset, and turning one into the other needs the observer's centroid
    // — which `core`'s mirror of the packet drops — and an inverse of
    // `bearing_to`/`distance_rad_to`, which the kernel does not have (this
    // module's own doc, at the top of the file, states both). Reprojecting
    // without that step leaves which side of a tile boundary a facet falls
    // on to the observer's sub-tile phase. Swept over 200 sub-tile phases on
    // the seed-42 band: best 0 of 31 marks misplaced, worst 24, mean 11.5,
    // only 2 of 200 phases exact.
    //
    // THE WIRE DOES CARRY THE PHASE. The observer's own centroid latitude
    // and longitude are on it, so the spherical direct problem recovers
    // every facet's absolute coordinate exactly; the sweep measures the
    // shortcut, not the contract. The campaign asserted otherwise in six
    // documents and it was corrected at close.

    /// `walk_band_scene` (and so `compose_perception_layer`, its only
    /// caller off the resolver path) answers off [`Driver::on_walk_band`],
    /// never a fresh `Snapshot::parse(&self.cached)` (The Gallery, Task 10
    /// round 1; The Quadrat's F11 measured the parse-and-`purview` shape at
    /// 24x a bare redraw, size-independent, on **every** keypress including
    /// plain typing).
    ///
    /// **The check corrupts `cached` rather than counting parses**, because
    /// a counter would need its own seam and this property is more direct:
    /// if the method still consulted `self.cached`'s own `spatial` tag, a
    /// cache that cannot parse as JSON would read as "not walk band" and
    /// `walk_band_scene` would go `None` even though the driver's own
    /// `on_walk_band` (set by the last real `refresh()`) says the possession
    /// is standing on the walk band right now. Answering `Some` here is
    /// only possible if the parse is genuinely gone from this path.
    ///
    /// **A weaker property than the one below, kept because it guards a
    /// different regression.** Round 2 (below) proves `purview(0)` itself is
    /// no longer called per read; this proves `self.cached` specifically is
    /// no longer consulted per read. Since `walk_band_scene` now touches
    /// neither, both hold — but a future edit could reintroduce a parse of
    /// `self.cached` without reintroducing a fresh `purview` call (or vice
    /// versa), so both stay.
    #[test]
    fn composing_the_perception_layer_does_not_parse_the_snapshot() {
        let mut d = test_driver();
        assert!(
            d.on_walk_band,
            "seed 42's flagship opens on the walk band, per Driver::start's own doc"
        );
        d.cached = "not valid snapshot json".to_string();
        assert!(
            d.walk_band_scene().is_some(),
            "walk_band_scene must answer from the cache, not a parse of \
             the (here, corrupted) cached snapshot"
        );
    }

    /// **Round 2** (The Gallery, Task 10 round 2): `walk_band_scene` answers
    /// off the CACHED [`Driver::walk_scene`], never a fresh
    /// `Session::purview(0)` call — the cost round 1 left standing, and
    /// measurement showed it was the dominant one (`walk_scene`'s own doc
    /// has the numbers).
    ///
    /// **The check poisons the cache to a value a live `purview(0)` call
    /// would never produce here**, the same technique the property above
    /// uses one field over: the possession is genuinely on the walk band
    /// (`on_walk_band` is `true`, unchanged), so a fresh `purview(0)` call
    /// would answer `Some`. Forcing `walk_scene` to `None` and then seeing
    /// `walk_band_scene()` answer `None` too is only possible if the method
    /// is reading the cache rather than recomputing.
    #[test]
    fn walk_band_scene_reads_the_cached_packet_not_a_fresh_purview_call() {
        let mut d = test_driver();
        assert!(
            d.on_walk_band,
            "seed 42's flagship opens on the walk band, per Driver::start's own doc"
        );
        d.walk_scene = None;
        assert!(
            d.walk_band_scene().is_none(),
            "walk_band_scene must answer from the cached walk_scene, not a fresh \
             purview(0) call (which would answer Some here, since the possession \
             really is on the walk band)"
        );
    }

    /// Every facet of the walk-band packet lands on the tile that HOLDS its
    /// own facet — checked against the projection's own INVERSE rather than
    /// against a second call to the projection.
    ///
    /// **Why the inverse.** `perception_tile` is
    /// `mercator::project(facet.coord())`, so asserting it equals
    /// `mercator::project(facet.coord())` would be a tautology dressed as a
    /// test — the vacuity shape this campaign has now hit repeatedly. So the
    /// expected answer is derived the other way: `mercator::unproject` gives
    /// each candidate tile's own centre, and the tile a facet belongs to is
    /// the one whose centre is NEARER the facet's centroid than either
    /// axis-neighbour's is. Floor-containment and nearest-centre are the
    /// same predicate on a uniform grid, and the plate's grid is uniform in
    /// longitude exactly and in Mercator `y` exactly; the residual
    /// non-linearity of latitude across ONE band-B tile is ~1e-8 rad against
    /// a tile of ~2.7e-4 rad, four orders below the thing being decided.
    ///
    /// This discriminates: a wrong scale, a wrong `depth`, a swapped
    /// row/col, and the rejected polar formula each move a facet off the tile
    /// containing it. `the_rejected_polar_placement_really_does_disagree`
    /// below is the standing witness that the last of those is not
    /// hypothetical.
    #[test]
    fn the_perception_overlay_lands_exactly_where_the_raster_puts_that_facet() {
        let d = test_driver();
        let scene = d
            .walk_band_scene()
            .expect("seed 42's flagship opens on the walk band");
        let win = Window {
            depth: BAND_B_RUNG,
            origin_col: 0,
            origin_row: 0,
        };
        let (vw, vh) = plate::virtual_dims(win.depth);
        let f = *d.frame();

        let facets = &scene.cells; // lexicon: `SurroundsCell` is the wire's own frozen name for a FACET — an area, not a vertex
        let mut checked = 0;
        for seen in facets {
            let facet = FacetId(seen.room)
                .unpack()
                .expect("a wire facet id unpacks");
            let coord = facet.coord();
            let got = plate::perception_tile(&f, vw, vh, seen.room)
                .expect("a walk-band facet is inside the clamp");

            // The chosen tile's centre must be nearer the facet's centroid
            // than either neighbour's, on each axis independently.
            let centre = |row: u32, col: u32| mercator::unproject(&f, row, col, vw, vh);
            let (lat_here, lon_here) = centre(got.0, got.1);
            let dlat = (coord.latitude - lat_here).abs();
            let dlon = |lon: f64| ((coord.longitude - lon + 540.0) % 360.0 - 180.0).abs();
            let dlon_here = dlon(lon_here);

            for step in [-1i64, 1] {
                let row = (got.0 as i64 + step).clamp(0, i64::from(vh) - 1) as u32;
                if row != got.0 {
                    let (lat_there, _) = centre(row, got.1);
                    assert!(
                        dlat < (coord.latitude - lat_there).abs(),
                        "facet {facet:?} is nearer row {row} than the row {} it was placed in",
                        got.0
                    );
                }
                let col = (got.1 as i64 + step).rem_euclid(i64::from(vw)) as u32;
                let (_, lon_there) = centre(got.0, col);
                assert!(
                    dlon_here < dlon(lon_there),
                    "facet {facet:?} is nearer col {col} than the col {} it was placed in",
                    got.1
                );
            }
            checked += 1;
        }
        // Non-vacuity: an empty band, or a projection that placed nothing,
        // would sail through the loop above.
        assert_eq!(
            checked,
            facets.len(),
            "every facet of the band must have been placed and checked"
        );
        // DERIVED FROM THE WIRE'S OWN RADIUS, not pinned. This read
        // `assert_eq!(checked, 31, "seed 42's flagship band is 31 facets")`
        // — the count a radius-4 purview held on the old 3-connected
        // triangular mesh. The Pavement's 8-connected square lattice makes
        // the same radius a full `(2r+1)^2` block, so the literal was 31
        // against a live 81 and the vacuity guard it was standing in for had
        // silently become a fixture pin. A seed whose purview straddles a
        // cube corner would legitimately hold FEWER than the full square; if
        // this ever fires for that reason, weaken it to a floor and say so,
        // rather than re-pinning a second literal.
        let full_square = (2 * scene.radius as usize + 1).pow(2);
        assert_eq!(
            checked, full_square,
            "a radius-{} purview on the 8-connected lattice is the full {full_square}-facet \
             square; the band held {checked}",
            scene.radius
        );
    }

    /// THE STANDING WITNESS that the equality above has teeth: the rejected
    /// design — the wire's polar pair, scaled by the mesh's own
    /// tiles-per-radian and rounded — really does put marks on tiles that do
    /// not hold them.
    ///
    /// Without this, `the_perception_overlay_lands_exactly_where_the_raster_
    /// puts_that_facet` could be satisfied by a projection nobody had reason
    /// to trust, and the measurement that re-planned this task would live
    /// only in prose. The polar formula here is the BEST version of the
    /// rejected design (right scale, isotropic, half-away-from-zero
    /// rounding), so the disagreement it shows is a floor on the error, not
    /// a straw man.
    #[test]
    fn the_rejected_polar_placement_really_does_disagree() {
        let d = test_driver();
        let scene = d.walk_band_scene().expect("the walk band");
        let win = Window {
            depth: BAND_B_RUNG,
            origin_col: 0,
            origin_row: 0,
        };
        let (vw, vh) = plate::virtual_dims(win.depth);
        let f = *d.frame();

        let facets = &scene.cells; // lexicon: `SurroundsCell` is the wire's own frozen name for a FACET — an area, not a vertex
        let observer = facets
            .iter()
            .find(|c| c.state == "here")
            .expect("the band contains the observer's own facet");
        let (obs_row, obs_col) =
            plate::perception_tile(&f, vw, vh, observer.room).expect("in the clamp");
        // Tiles per radian: the plate's own chart width over a full turn.
        let per_radian = f64::from(vw) / std::f64::consts::TAU;

        let mut disagreed = 0;
        for seen in facets {
            let (row, col) = plate::perception_tile(&f, vw, vh, seen.room).expect("in the clamp");
            let theta = seen.bearing_deg.to_radians();
            let r = seen.distance_rad * per_radian;
            let polar_row =
                obs_row as i64 + (-hornvale_kernel::math::cos(theta) * r).round() as i64;
            let polar_col = obs_col as i64 + (hornvale_kernel::math::sin(theta) * r).round() as i64;
            if (row as i64, col as i64) != (polar_row, polar_col) {
                disagreed += 1;
            }
        }
        assert!(
            disagreed > 0,
            "the polar placement agreed on all {} facets, so the equality test above \
             is not discriminating — re-derive the measurement before trusting it",
            facets.len()
        );
    }

    /// RULING 19's SURVIVING CONCERN, as an assertion. `spread::compose`'s
    /// plate selection is either/or, so handing band B a raster at all would
    /// otherwise have removed the ONLY thing that draws the observer's own
    /// position — silently, with both suites green, because the plate tests
    /// assert terrain and `core`'s chart tests assert the chart in
    /// isolation.
    #[test]
    fn band_b_still_shows_the_observer_over_its_own_terrain() {
        let mut d = test_driver();
        d.resize(120, 40);
        enter_band_b(&mut d);
        assert!(
            d.at_walk_band_rung(),
            "sanity: the session opens on band B, which is where this test means to stand"
        );
        let plate = d
            .world_plate_for_redraw(120, 40)
            .expect("band B draws a plate now");
        let text = plate.to_plain_text();
        assert!(
            text.contains('@'),
            "band B lost the observer's own position marker:\n{text}"
        );
        // AND the terrain is still under it — an overlay that had wiped the
        // raster would satisfy the assertion above on its own. The Legend
        // retired the `~`/`.` binary, so "the raster survived" is any
        // drawn glyph besides the observer's own marker, not one hardcoded
        // pair.
        assert!(
            text.chars().any(|c| c != '@' && !c.is_whitespace()),
            "band B lost its terrain raster:\n{text}"
        );
        // Exactly one observer: the packet has exactly one `here` facet, and
        // a layer that painted `'@'` per facet would still contain one.
        assert_eq!(
            text.chars().filter(|&c| c == '@').count(),
            1,
            "exactly one observer marker:\n{text}"
        );
    }

    /// The overlay is an OVERLAY: it paints the observer and the marks, and
    /// leaves every other packet facet to the raster underneath. Counted
    /// against the packet's own composition, so the assertion moves with the
    /// band rather than pinning a number.
    ///
    /// Non-vacuity is the second assertion: the band must actually CONTAIN
    /// unmarked facets, or "the unmarked ones were not painted" is a claim
    /// about the empty set. Seed 42's flagship band is 31 facets of which
    /// only the observer's carries a mark at all.
    #[test]
    fn the_overlay_leaves_ordinary_ground_to_the_raster() {
        let mut d = test_driver();
        d.resize(120, 40);
        enter_band_b(&mut d);
        let scene = d.walk_band_scene().expect("the walk band");
        let facets = &scene.cells; // lexicon: `SurroundsCell` is the wire's own frozen name for a FACET — an area, not a vertex
        let drawable = facets
            .iter()
            .filter(|c| c.state == "here" || !c.marks.is_empty())
            .count();
        assert!(
            facets.len() > drawable,
            "this band is all observer-and-marks, so it cannot show that ordinary \
             ground is left alone ({} of {} drawable)",
            drawable,
            facets.len()
        );
        let plate = d.world_plate_for_redraw(120, 40).expect("a plate");
        let overlay: usize = (0..plate.height())
            .flat_map(|y| (0..plate.width()).map(move |x| (x, y)))
            .filter(|&(x, y)| {
                plate
                    .get(x, y)
                    .is_some_and(|c| c.source == hornvale_game_core::Source::Chart)
            })
            .count();
        assert!(
            overlay <= drawable,
            "the overlay painted {overlay} boxes for a band with only {drawable} \
             drawable ones — it is covering ground the raster drew"
        );
        assert!(overlay > 0, "the overlay painted nothing at all");
    }

    /// THE WINDOW-ORIGIN RULING. Band B's origin is `(0, 0)` at `start`,
    /// which at [`BAND_B_RUNG`] is some eleven thousand rows and two
    /// thousand columns from seed 42's observer — the arctic corner of a
    /// 23,245-wide chart.
    ///
    /// Asserted as "the observer's own facet is INSIDE the drawn window",
    /// which is the property that matters and which a hardcoded expected
    /// origin would not survive a mesh change.
    ///
    /// **RE-POINTED, on this test's own instruction (The Quadrat, Task 9,
    /// fix round 1).** It used to open by asserting that `start` leaves the
    /// origin at `(0, 0)`, "which is what makes this test meaningful at
    /// all", and closed with "if `start` gains a centring step, re-point
    /// it". `start` has gained one: `refresh` now ends in
    /// [`Self::follow_the_walker`], so the very first frame of an
    /// unresized driver is centred rather than arctic — strictly better,
    /// since Task 9 means the walk view draws a plate before any arrival
    /// happens at all.
    ///
    /// So the corner is now CONSTRUCTED rather than inherited, which makes
    /// this stronger than it was: it shows the centring actually rescues a
    /// corner origin (the negative control below fails without it), instead
    /// of resting on `start` happening to leave one lying around.
    #[test]
    fn band_b_centres_on_the_observer_not_the_arctic_corner() {
        let mut d = test_driver();
        d.resize(120, 40);
        // The corner, put there on purpose. At `BAND_B_RUNG` this is some
        // eleven thousand rows and two thousand columns from seed 42's
        // observer.
        d.window.origin_row = 0;
        d.window.origin_col = 0;
        let arctic = d
            .world_plate_for_redraw(120, 40)
            .expect("band B draws a plate");
        assert!(
            !arctic.to_plain_text().contains('@'),
            "NEGATIVE CONTROL: the observer must NOT be visible from the chart's \
             corner, or the centring below proves nothing"
        );
        d.enter_map();
        let plate = d
            .world_plate_for_redraw(120, 40)
            .expect("band B draws a plate");
        assert!(
            plate.to_plain_text().contains('@'),
            "the observer is outside the band-B window; it was not centred"
        );
        assert_ne!(
            (d.window.origin_row, d.window.origin_col),
            (0, 0),
            "the window must actually have moved off the corner"
        );
    }

    /// A COARSE rung draws the OBSERVER and no marks — the coarsest rung's
    /// half of the same split
    /// [`off_band_b_the_overlay_paints_the_observer_and_no_marks`] asserts at
    /// the entry rung.
    ///
    /// # THIS TEST'S CLAIM WAS NARROWED BY THE HACHURE, STAGE 0
    ///
    /// It asserted that a coarse rung draws **nothing**, on this reasoning:
    /// "the packet's 31 facets are all finer than one coarse tile, so every
    /// one of them collapses onto the observer's single tile and an overlay
    /// drawn there would claim to place facets it had merged."
    ///
    /// **That reasoning is kept in full and it only ever covered the MARKS.**
    /// A mark drawn on a merged tile does claim a placement the rung cannot
    /// resolve. The OBSERVER does not: "you are in this tile" is true at
    /// every rung, and it is the one claim that survives coarsening. Drawing
    /// nothing was the right conclusion while the map only ever opened on
    /// band B; once it opens coarser (`plate::map_entry_rung`) it becomes
    /// decision 0293's own documented defect — a view with no `@`, where
    /// "the picture and the prose described different places".
    ///
    /// So the refusal is now scoped to what it was always arguing about, and
    /// the non-vacuity below is what keeps it a real refusal: the band must
    /// carry marks for "no marks were drawn" to mean anything.
    #[test]
    fn a_coarse_rung_draws_the_observer_and_no_marks() {
        let mut d = test_driver();
        d.resize(120, 40);
        enter_world_view(&mut d);
        assert!(!d.at_walk_band_rung(), "sanity: a coarse rung");
        // NON-VACUITY: the band must carry marks, or "no marks were drawn"
        // is a claim about the empty set.
        let scene = d.walk_band_scene().expect("the walk band");
        let marked = scene.cells.iter().filter(|c| !c.marks.is_empty()).count(); // lexicon: `SurroundsCell` is the wire's own frozen name for a FACET — an area, not a vertex
        assert!(
            marked > 0,
            "this band carries no marks, so the refusal below proves nothing"
        );
        let plate = d.world_plate_for_redraw(120, 40).expect("a plate");
        let overlay: Vec<char> = (0..plate.height())
            .flat_map(|y| (0..plate.width()).map(move |x| (x, y)))
            .filter_map(|(x, y)| {
                plate.get(x, y).and_then(|c| {
                    (c.source == hornvale_game_core::Source::Chart)
                        .then_some(c.glyph)
                        .flatten()
                })
            })
            .collect();
        assert_eq!(
            overlay,
            vec!['@'],
            "a coarse rung must draw the observer and nothing else"
        );
    }

    /// **THE CHAMBER BAND'S PICTURE — fix round 1's F5, and the test whose
    /// absence made that defect silent.** Nothing in `bin` pinned what the
    /// chamber band draws, so when `world_plate_for_redraw`'s gate became
    /// `Focus::Map` alone, typing `map` indoors replaced the chamber floor
    /// plan with the world raster and the suite stayed green. The perception
    /// overlay correctly refuses off the walk band, so nothing marked the
    /// player's position there either.
    ///
    /// The campaign's spec says "Band A keeps its own renderer and this
    /// campaign does not touch it". This is that promise, as an assertion
    /// about the composed page rather than about a gate — so it holds however
    /// the gate is later spelled.
    #[test]
    fn the_chamber_band_keeps_its_own_plan_and_never_the_raster() {
        let mut d = test_driver();
        d.resize(210, 56);
        // `handle`'s bool is whether the possession RELEASED, so the real
        // check is the snapshot's own `spatial` tag.
        d.handle("enter");
        let snap = hornvale_game_core::Snapshot::parse(&d.cached).expect("a live session");
        assert!(
            matches!(snap.spatial, hornvale_game_core::Spatial::Chamber { .. }),
            "seed 42's flagship must land in the chamber band after one `enter`"
        );
        d.enter_map();

        assert!(
            d.world_plate_for_redraw(210, 56).is_none(),
            "the chamber band must not be handed a world plate"
        );

        // The PICTURE, through the same `render_with` `main.rs`'s redraw
        // calls — a gate assertion alone would not have caught this, because
        // `spread::compose` is what turns a supplied plate into a replaced
        // floor plan.
        let json = d.snapshot();
        let empty = String::new();
        let world_plate = d.world_plate_for_redraw(210, 56);
        let (grid, _) = hornvale_game_core::render_with(
            &json,
            210,
            56,
            d.focus(),
            d.cursor(),
            hornvale_game_core::CommandLine {
                text: &empty,
                caret: 0,
            },
            d.strip_text(),
            d.echo(),
            world_plate.as_ref(),
            d.strip_offset(),
            None,
        )
        .expect("the fixture renders");

        let content_h = hornvale_game_core::spread::content_height(56);
        let plate_w = hornvale_game_core::spread::world_plate_width(210, 56);
        let sources: BTreeSet<hornvale_game_core::Source> = (0..content_h)
            .flat_map(|y| (0..plate_w).map(move |x| (x, y)))
            .filter_map(|(x, y)| grid.get(x, y).filter(|c| !c.is_blank()).map(|c| c.source))
            .collect();
        assert!(
            sources.contains(&hornvale_game_core::Source::Plan),
            "the chamber band must draw its own floor plan, got {sources:?}"
        );
        assert!(
            !sources.contains(&hornvale_game_core::Source::World),
            "the world raster must never appear on a chamber-band spread, got {sources:?}"
        );
        assert!(
            !sources.contains(&hornvale_game_core::Source::Chart),
            "and neither must a perception overlay with no packet behind it, got {sources:?}"
        );

        // The cursor clamps to the plate that IS drawn, not to the world
        // plate's wider fit — the Task 3a review finding with the operands
        // swapped.
        d.apply(Action::CursorBy(i16::MAX, 0));
        assert_eq!(
            d.cursor.x,
            hornvale_game_core::spread::PLATE_WIDTH - 1,
            "the chamber band's cursor must stop at the floor plan's own edge"
        );
        // And a cursor move must not have scrolled a window nobody can see.
        let window = d.window;
        d.apply(Action::CursorBy(i16::MAX, 0));
        assert_eq!(
            d.window, window,
            "the chamber band draws no Mercator, so nothing may scroll one"
        );
    }

    /// The band-B strip keeps the walk band's SIGHT CAPTION — the honesty
    /// line naming whose eyes the chart was drawn through. It is the half of
    /// the old `world_view()` branch that had to survive the branch:
    /// band B has a plate AND an overlay AND a sight caption, where a
    /// coarser rung has a plate and neither.
    #[test]
    fn band_b_keeps_the_sight_caption_and_a_coarse_rung_does_not() {
        let mut d = test_driver();
        enter_band_b(&mut d);
        let band_b = d.strip_text().map(str::to_string).expect("a strip");
        assert!(
            band_b.contains("seen through"),
            "band B must keep the sight disclosure, got {band_b:?}"
        );
        enter_world_view(&mut d);
        let coarse = d.strip_text().map(str::to_string).expect("a strip");
        assert!(
            !coarse.contains("seen through"),
            "a coarse rung carries no observer-vision channel, got {coarse:?}"
        );
    }

    /// The promoted [`Driver::centre_window_on`]'s SIZE GUARD (Task 1's
    /// review's condition for promoting the test helper). The helper it came
    /// from computed the column as `(col + virtual_w - w / 2)` in `u32`,
    /// which underflows — a debug panic — once `w / 2` exceeds
    /// `col + virtual_w`. Driven at a rung COARSER than the mesh, where the
    /// virtual chart is narrower than the plate: unreachable through
    /// `apply_zoom`, reachable by any caller setting `depth` directly, which
    /// this module's own disclosure test already does.
    #[test]
    fn centring_survives_a_plate_wider_than_the_whole_chart() {
        let mut d = test_driver();
        d.window.depth = 0; // 23 tiles around the planet
        let (vw, _) = plate::virtual_dims(d.window.depth);
        assert!(
            vw < 200,
            "this test needs a chart narrower than the plate below; got {vw}"
        );
        let coord = d.session.position().coord();
        d.centre_window_on(coord.latitude, coord.longitude, 200, 100)
            .expect("the flagship is inside the clamp");
        assert!(
            d.window.origin_col < vw,
            "the wrapped origin must stay inside the chart, got {}",
            d.window.origin_col
        );
    }

    // -- Task 7: the cursor-anchored zoom invariant (H3) -----------------

    /// A driver parked somewhere REAL, off-centre in both axes, with the
    /// premise asserted rather than assumed.
    ///
    /// The Quadrat has now caught seven tests whose input space had
    /// collapsed to a single value, and a zoom-invariant test is the
    /// obvious next candidate: with the cursor at the plate's middle, or
    /// with the window already centred on the point under the cursor, the
    /// anchor has nothing to do and the invariant holds for free. So this
    /// helper moves the cursor away from the middle in BOTH axes and
    /// asserts it actually got there — the remedy Task 6's F1 fix
    /// established, applied one task later.
    fn off_centre_world_view(d: &mut Driver) {
        d.resize(210, 56);
        enter_world_view(d);
        let (plate_w, plate_h) = d.active_plate_dims();
        centre_window_on_the_player(d, plate_w, plate_h);
        d.apply(Action::CursorBy(0, -i16::MAX)); // park at a known corner
        d.apply(Action::CursorBy(-i16::MAX, 0));
        d.apply(Action::CursorBy(
            i16::try_from(plate_w / 4).expect("a plate quarter fits an i16"),
            i16::try_from(plate_h / 3).expect("a plate third fits an i16"),
        ));
        assert_ne!(
            (d.cursor.x, d.cursor.y),
            (plate_w / 2, plate_h / 2),
            "the premise: a cursor at the plate's middle makes the invariant \
             hold trivially, and would prove nothing"
        );
    }

    /// The tile the cursor points at right now, as `(row, col)` on the
    /// active rung's own virtual chart, with the column wrapped the way
    /// `mercator::unproject` reads it.
    fn tile_under_cursor(d: &Driver) -> (u32, u32) {
        let (virtual_w, _) = plate::virtual_dims(d.window.depth);
        (
            d.window.origin_row + u32::from(d.cursor.y),
            (d.window.origin_col + u32::from(d.cursor.x)) % virtual_w,
        )
    }

    /// **H3, THE CAMPAIGN'S FOUNDING DEFECT: the geographic point under the
    /// cursor does not move across a zoom step.**
    ///
    /// Asserted EXACTLY, not within a tolerance, and the exactness is the
    /// point. A degrees comparison would have to allow half a tile of the
    /// destination rung — the map has no address finer than a tile — and at
    /// [`GLOBE_RUNG`] half a tile is about half a degree, which is the same
    /// size as the slop a broken anchor would produce. So the assertion is
    /// the containment statement instead: the point that was under the
    /// cursor before the step is inside the tile that is under the cursor
    /// after it. That is true or false with no threshold to tune.
    ///
    /// Walked over EVERY rung of the shipped ladder in both directions, not
    /// one step at one rung: the anchor's two knobs (window origin, and the
    /// cursor where the origin cannot move) are selected by where the window
    /// happens to be, so a single-rung test would exercise one of them.
    #[test]
    fn a_zoom_step_keeps_the_same_geographic_point_under_the_cursor() {
        for direction in [1i8, -1i8] {
            let mut d = test_driver();
            off_centre_world_view(&mut d);
            if direction < 0 {
                for _ in 0..(BAND_B_RUNG - GLOBE_RUNG) {
                    d.apply(Action::Zoom(1));
                }
                assert_eq!(d.window.depth, BAND_B_RUNG, "walked to the far end");
            }
            for _ in 0..(BAND_B_RUNG - GLOBE_RUNG) {
                let before_depth = d.window.depth;
                let before = d.geographic_point_under_cursor();
                d.apply(Action::Zoom(direction));
                assert_ne!(d.window.depth, before_depth, "the rung must have moved");

                let (virtual_w, virtual_h) = plate::virtual_dims(d.window.depth);
                let landed = mercator::project(&d.frame, before.0, before.1, virtual_w, virtual_h)
                    .expect("the point was on the chart a moment ago");
                assert_eq!(
                    landed,
                    tile_under_cursor(&d),
                    "rung {before_depth} -> {}: the point {before:?} left the cursor; \
                     it now reads {:?}",
                    d.window.depth,
                    d.geographic_point_under_cursor()
                );
            }
        }
    }

    /// **THE SECOND KNOB.** Anchoring normally moves the WINDOW ORIGIN. At
    /// the chart's polar edge the origin has nowhere left to go — latitude
    /// clamps where longitude wraps (spec §4.2) — so the guarantee is kept
    /// by moving the CURSOR instead, and a test that only ever zoomed in
    /// the middle of the chart would never reach that branch.
    ///
    /// Note which end of the ladder this needs. The brief's sketch reached
    /// for [`GLOBE_RUNG`] on the theory that the whole planet fits the plate
    /// there and the origin is pinned at `(0, 0)`. That was true before Task
    /// 1: it is not true now, because the chart is the RUNG rather than the
    /// plate, and `GLOBE_RUNG`'s chart is 363x362 tiles against a plate of
    /// about 104x52. The origin is pinned where the CLAMP binds, which is
    /// the chart's top and bottom rows at any rung.
    #[test]
    fn at_the_charts_polar_edge_the_cursor_moves_because_the_window_cannot() {
        let mut d = test_driver();
        d.resize(210, 56);
        enter_band_b(&mut d);
        for _ in 0..4 {
            d.apply(Action::Zoom(-1));
        }
        // Scroll north until the origin sticks, then step the cursor down
        // off the top row so there is something for the anchor to spend.
        d.apply(Action::CursorBy(0, -i16::MAX));
        d.apply(Action::CursorBy(0, 12));
        assert_eq!(
            d.window.origin_row, 0,
            "the premise: the window must be against the chart's own top edge"
        );
        assert_eq!(d.cursor.y, 12, "the premise: the cursor is off that edge");

        let before = d.geographic_point_under_cursor();
        d.apply(Action::Zoom(-1));

        assert_eq!(
            d.window.origin_row, 0,
            "the clamp must still hold the origin against the edge"
        );
        assert_ne!(
            d.cursor.y, 12,
            "the origin could not move, so the cursor had to; it did not"
        );
        let (virtual_w, virtual_h) = plate::virtual_dims(d.window.depth);
        let landed = mercator::project(&d.frame, before.0, before.1, virtual_w, virtual_h)
            .expect("the point was on the chart a moment ago");
        assert_eq!(
            landed,
            tile_under_cursor(&d),
            "the point {before:?} left the cursor; it now reads {:?}",
            d.geographic_point_under_cursor()
        );
    }

    /// The strip names the rung. Part of "the map zooms based on criteria I
    /// have not identified" is that nothing on screen ever said which rung
    /// was showing, so a zoom that worked perfectly was still unreadable.
    #[test]
    fn the_strip_names_the_current_rung() {
        let mut d = test_driver();
        d.resize(210, 56);
        enter_world_view(&mut d);
        let coarse = d.strip_text().map(str::to_string).expect("a strip");
        d.apply(Action::Zoom(1));
        let finer = d.strip_text().map(str::to_string).expect("a strip");
        assert_ne!(
            coarse, finer,
            "the strip did not change across a zoom step: {coarse:?}"
        );
        assert!(
            coarse.contains(RUNG_OPENING) && finer.contains(RUNG_OPENING),
            "both strips must carry the rung line, got {coarse:?} and {finer:?}"
        );

        // AND IT COMES BEFORE THE CLAMP CAPTION (fix round 1, Important 3).
        // The strip marquees, so a clause's POSITION is a delay: with the
        // rung line emitted after the 69-character clamp caption it began at
        // character 90, first scrolling into view after ~15 s on the 80x24
        // floor and readable after ~44 s. The clamp caption is static and
        // says the same thing at every rung; the rung line is the one that
        // answers the founding report. Order is therefore a property, not a
        // formatting accident, and `contains` alone would not have held it.
        let rung_at = coarse
            .find(RUNG_OPENING)
            .expect("the rung line is on the strip");
        let clamp_at = coarse
            .find("clamped at")
            .expect("the clamp caption is on the strip");
        assert!(
            rung_at < clamp_at,
            "the rung line must reach the reader before the static clamp caption, \
             got {coarse:?}"
        );
    }

    /// The rung line is DERIVED, and it is different at every rung. A line
    /// that named the rung but said the same thing at all seven of them
    /// would satisfy the test above (which only compares two neighbours)
    /// while telling the reader nothing about where the ladder's ends are.
    #[test]
    fn the_rung_line_is_distinct_at_every_rung_and_names_the_ladders_ends() {
        let mut d = test_driver();
        d.resize(210, 56);
        enter_world_view(&mut d);
        let mut seen: Vec<String> = Vec::new();
        for expected in GLOBE_RUNG..=BAND_B_RUNG {
            assert_eq!(d.window.depth, expected, "the walk lost its place");
            let line = d.rung_caption();
            assert!(
                line.contains(&format!("{RUNG_OPENING} {expected} ")),
                "the line must name the rung it is on, got {line:?}"
            );
            assert!(
                line.contains(&format!("{GLOBE_RUNG}–{BAND_B_RUNG}")),
                "the line must name the ladder's own ends, got {line:?}"
            );
            seen.push(line);
            d.apply(Action::Zoom(1));
        }
        assert_eq!(d.window.depth, BAND_B_RUNG, "the walk walked the ladder");
        let mut distinct = seen.clone();
        distinct.sort();
        distinct.dedup();
        assert_eq!(
            distinct.len(),
            seen.len(),
            "two rungs printed the same line: {seen:?}"
        );
    }

    /// **THE MIRROR DISCLOSURE** — Task 1's carried note, that the
    /// resolution disclosure is one-sided and the ladder now lives entirely
    /// on the other side of 1:1, so the map disclosed nothing at all. Also
    /// Task 6's concern 2: some hundred band-B characters share one
    /// grid-level vertex and nothing said so.
    ///
    /// Asserted as MONOTONE, not against a magic number: the share must
    /// rise at every rung, because every rung quadruples the tiles over an
    /// unchanged field. `Driver::resolution_disclosure` — the coarse half —
    /// must stay silent throughout, which is what makes this half the only
    /// thing the reader has.
    #[test]
    fn the_rung_line_discloses_that_the_picture_outruns_the_terrain_field() {
        let mut d = test_driver();
        d.resize(210, 56);
        enter_world_view(&mut d);
        let mut shares: Vec<u64> = Vec::new();
        for _ in GLOBE_RUNG..=BAND_B_RUNG {
            let (vw, vh) = plate::virtual_dims(d.window.depth);
            let said = d
                .oversample_disclosure(vw, vh)
                .expect("every shipped rung outruns the terrain field");
            assert!(
                d.rung_caption().contains(&said),
                "the rung line must carry the disclosure it derives"
            );
            assert!(
                d.resolution_disclosure().is_none(),
                "the coarse-end disclosure must stay silent — this test's premise"
            );
            shares.push(
                said.split_whitespace()
                    .nth(1)
                    .and_then(|n| n.parse::<u64>().ok())
                    .unwrap_or_else(|| panic!("the disclosure must carry a count, got {said:?}")),
            );
            d.apply(Action::Zoom(1));
        }
        for pair in shares.windows(2) {
            assert!(
                pair[1] > pair[0],
                "every finer rung must share one reading across MORE characters, got {shares:?}"
            );
        }
    }

    /// **THE CORRECTED CLAIM** (fix round 1, Important 1). The comment in
    /// [`Driver::apply_zoom`] used to assert that
    /// `zoom_plus_at_the_ladders_ceiling_keeps_the_players_scroll` covers
    /// where the anchor block sits relative to the `depth != before` guard.
    /// It does not — the reviewer moved the block outside and every relevant
    /// test stayed green — and the reason is exactly the property the
    /// comment cited in its own defence: **the anchor is idempotent at an
    /// unchanged rung.** A tile centre re-projects to its own tile, so
    /// re-anchoring what is already anchored moves nothing.
    ///
    /// So the claim that rotted is replaced by a test of the FACT the claim
    /// rested on. That is the half a reader can act on: if this ever goes
    /// red, the guard has become load-bearing and needs a test of its own.
    ///
    /// Driven at every rung and from an off-centre cursor, because
    /// idempotence at the plate's middle is the trivial case.
    #[test]
    fn the_anchor_is_a_no_op_at_an_unchanged_rung() {
        let mut d = test_driver();
        off_centre_world_view(&mut d);
        for _ in GLOBE_RUNG..=BAND_B_RUNG {
            let window = d.window;
            let cursor = d.cursor;
            let (lat, lon) = d.geographic_point_under_cursor();
            d.anchor_geographic_point(lat, lon);
            assert_eq!(
                (d.window, d.cursor),
                (window, cursor),
                "rung {}: re-anchoring an already-anchored point moved something",
                d.window.depth
            );
            d.apply(Action::Zoom(1));
        }
    }

    /// **THE ONE LEVER THAT DISCRIMINATES THE GUARD'S POSITION**, and it
    /// exists only because it is the one state in which the anchor is NOT
    /// idempotent: a plate TALLER than the whole chart. There
    /// `reclamp_window`'s `max_origin_row` is 0, the cursor may legally sit
    /// on a row the chart does not have, and re-anchoring pulls it back onto
    /// the chart — a real move, on a press documented as changing nothing.
    ///
    /// Reachable rather than contrived: it needs a terminal wide enough for
    /// a plate over 362 rows deep at [`GLOBE_RUNG`], which is Task 5's
    /// carried M3 finding stated from the cursor's side. It is driven
    /// through the shipped `Action` path, not by poking `depth`.
    #[test]
    fn a_saturating_press_on_a_plate_taller_than_the_chart_moves_nothing() {
        let mut d = test_driver();
        d.resize(1600, 400);
        enter_world_view(&mut d);
        let (plate_w, plate_h) = d.active_plate_dims();
        let (_, virtual_h) = plate::virtual_dims(d.window.depth);
        assert!(
            plate_h > virtual_h as u16,
            "the premise: this test needs a plate ({plate_w}x{plate_h}) taller than \
             the whole chart ({virtual_h} rows)"
        );

        // Park the cursor on a row the chart does not have — legal, because
        // the cursor clamps to the PLATE and the plate is the taller of the
        // two.
        d.apply(Action::CursorBy(0, i16::MAX));
        assert!(
            u32::from(d.cursor.y) >= virtual_h,
            "the premise: the cursor must be off the chart's own bottom, at \
             {} against {virtual_h} rows",
            d.cursor.y
        );

        let window = d.window;
        let cursor = d.cursor;
        d.apply(Action::Zoom(-1)); // saturating: GLOBE_RUNG is the floor
        assert_eq!(
            (d.window, d.cursor),
            (window, cursor),
            "a press past the ladder's floor must move nothing — not the window, \
             and not the cursor an out-of-guard anchor would have dragged onto \
             the chart"
        );
    }

    /// **THE WAY HOME, AT A COARSE RUNG** (fix round 1, Important 2 — a
    /// defect this task created). Taking the observer-centring off the zoom
    /// path was right, but "zoom out one, zoom in one" had been an
    /// accidental home gesture, and its removal left a coarse rung with none
    /// at all: the centring returned early off band B, and no observer
    /// marker is drawn there either, so a reader who had scrolled away could
    /// neither return nor see which way to go.
    ///
    /// Asserted with the premise moved first: scroll a long way, assert the
    /// observer really is off the window, THEN type `map`.
    #[test]
    fn typing_map_at_a_coarse_rung_brings_the_reader_home() {
        let mut d = test_driver();
        d.resize(210, 56);
        enter_world_view(&mut d);
        assert!(!d.at_walk_band_rung(), "sanity: a coarse rung");

        let observer_is_shown = |d: &Driver| {
            let (plate_w, plate_h) = d.active_plate_dims();
            let (vw, vh) = plate::virtual_dims(d.window.depth);
            let coord = d.session.position().coord();
            let Some((row, col)) =
                mercator::project(&d.frame, coord.latitude, coord.longitude, vw, vh)
            else {
                return false;
            };
            let down = i64::from(row) - i64::from(d.window.origin_row);
            let across =
                (i64::from(col) - i64::from(d.window.origin_col)).rem_euclid(i64::from(vw));
            down >= 0 && down < i64::from(plate_h) && across < i64::from(plate_w)
        };

        // Get lost, DETERMINISTICALLY. An earlier revision drove this with
        // thirty `CursorBy(i16::MAX, 0)` sweeps, which is the honest gesture
        // but not a controlled one: each sweep spills a huge remainder into
        // `origin_col` and the chart is 363 tiles around, so thirty of them
        // wrap to an arbitrary column — and on seed 42 they wrapped back
        // ONTO the observer, failing the premise. How the reader got lost is
        // not the property under test; that they are lost is.
        let coord = d.session.position().coord();
        let (plate_w, plate_h) = d.active_plate_dims();
        d.centre_window_on(-coord.latitude, coord.longitude + 180.0, plate_w, plate_h)
            .expect("the observer's antipode is inside the clamp when the observer is");
        assert!(
            !observer_is_shown(&d),
            "the premise: the reader must actually be off the observer, or \
             arriving home proves nothing"
        );

        d.enter_map(); // what a submitted `map` line does
        assert!(
            observer_is_shown(&d),
            "typing `map` at a coarse rung must bring the reader home; window {:?}",
            d.window
        );
        assert!(
            !d.at_walk_band_rung(),
            "and must NOT smuggle in a rung change while doing it"
        );
    }

    /// TASK 2'S CARRIED FINDING, closed by the anchor rather than by a
    /// second centring rule: leaving band B for the coarse rungs used to
    /// land at an arbitrary origin on an ~11,600-tile-wide chart. It now
    /// lands where the reader was looking, and at `enter_map` that is the
    /// observer.
    #[test]
    fn zooming_out_of_band_b_lands_where_the_reader_was_looking() {
        let mut d = test_driver();
        d.resize(210, 56);
        enter_band_b(&mut d);
        assert!(d.at_walk_band_rung(), "sanity: the map opens on band B");
        d.apply(Action::Zoom(-1));
        assert_eq!(d.window.depth, BAND_B_RUNG - 1);

        let (plate_w, plate_h) = d.active_plate_dims();
        let (vw, vh) = plate::virtual_dims(d.window.depth);
        let coord = d.session.position().coord();
        let (row, col) = mercator::project(&d.frame, coord.latitude, coord.longitude, vw, vh)
            .expect("seed 42's flagship is inside the clamp");
        let within_rows =
            row >= d.window.origin_row && row < d.window.origin_row + u32::from(plate_h);
        let across = (i64::from(col) - i64::from(d.window.origin_col)).rem_euclid(i64::from(vw));
        assert!(
            within_rows && across < i64::from(plate_w),
            "the observer fell outside the coarse rung's own window: observer at \
             ({row}, {col}), window {:?}, plate {plate_w}x{plate_h}",
            d.window
        );
    }

    // -- The Newel, Task 5 / B4: the strip's standing content is the tile -

    /// B4 (spec §4.4): the strip's standing content is the tile under the
    /// cursor, not the map's own resolution. Measured before the fix: 188
    /// characters, of which 25 (13.3%) described the tile and 160 (86.7%)
    /// were the four map-wide clauses (the rung line, the tile count, the
    /// oversample disclosure, the clamp caption) — the last of those
    /// reachable only after 44.4 s of marquee scroll on the 80x24 floor.
    ///
    /// **Asserts the strip LEADS WITH the tile readout, not that a
    /// map-wide clause is absent** (controller ruling on this task's own
    /// brief): Task 6 makes those clauses appear-and-decay on a rung
    /// change, and entering the world view (this test's own setup) is such
    /// a change, so an absence assertion here would redden the moment that
    /// lands. Leading with the tile is true both before and after Task 6 —
    /// only what follows it changes.
    #[test]
    fn the_strip_describes_the_tile_under_the_cursor() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        let s = d.strip_text().expect("the world view has a strip");
        assert!(
            !s.starts_with(RUNG_OPENING),
            "the standing line must not lead with the map-wide rung clause: {s}"
        );
        assert!(
            s.contains("above sea level"),
            "the standing line says nothing about the tile's own height: {s}"
        );
        let tile_pos = s
            .find("above sea level")
            .expect("checked immediately above");
        let rung_pos = s
            .find(RUNG_OPENING)
            .expect("the map-wide rung clause is still appended after the tile");
        assert!(
            tile_pos < rung_pos,
            "the tile's own height reading must reach the reader before the map-wide \
             rung clause, got {s:?}"
        );
    }

    /// **Decision 0670 gates a placed site's MENTION here, exactly as Task
    /// 4 gated its NAME on the chart** (`Self::site_note`'s own doc):
    /// `MapSite` carries no proper name at all (spec §7 non-goal), so what
    /// is withheld before discovery is the fact that anything stands there
    /// at all, not merely a name string.
    #[test]
    fn an_undiscovered_site_is_not_mentioned_but_a_discovered_one_is() {
        use hornvale_vessel::site::SiteKind;

        // AT BAND B'S OWN RUNG, deliberately, not a coarser world-view one:
        // a placed site's own `Facet` (`MapSite::placed`) is addressed at
        // walk-band granularity (`hornvale_worldgen::site_facet_for`'s own
        // doc: "up to ~40 walk-facet edges"), and `Self::site_note` reads
        // `Self::world_view_tile`'s `.facet`, which is a facet at the
        // ACTIVE window's own depth — so the two can only ever agree where
        // that depth IS band B's. A coarser rung's tile facet is a
        // different, larger patch at a different level of the same
        // hierarchy and can never equal a site's own placed facet.
        let (w, h) = (104u16, 56u16);
        let mut d = test_driver();
        enter_band_b(&mut d);
        let (plate_w, plate_h) = d.active_plate_dims();
        let site = centre_on_a_placed_site(&mut d, SiteKind::Cave, plate_w, plate_h);
        assert!(
            !d.discovered().contains(site.feature_id()),
            "guard: the cave must not already read as discovered"
        );

        // Find the exact screen position the cave's own glyph occupies —
        // searched, not assumed at the plate's own centre, the same reason
        // `site_drawing_never_depends_on_discovery_and_the_cursor_never_
        // leaks_a_name` searches rather than computes.
        let grid = d.world_plate(w, h);
        let mut found: Option<(u16, u16)> = None;
        'search: for y in 0..plate_h {
            for x in 0..plate_w {
                if grid.get(x, y).and_then(|c| c.glyph) == Some(plate::CAVE_GLYPH) {
                    found = Some((x, y));
                    break 'search;
                }
            }
        }
        let (x, y) = found.expect("an undiscovered, centred cave must still be drawn");
        d.cursor = hornvale_game_core::Cursor { x, y };
        d.refresh_strip();

        let before = d
            .strip_text()
            .expect("the world view has a strip")
            .to_string();
        assert!(
            !before.contains("cave"),
            "an undiscovered site's kind leaked into the standing line: {before:?}"
        );

        // Discover whichever site actually sits under the CURSOR's own
        // resolved facet — not necessarily `site`, the one
        // `centre_on_a_placed_site` happened to centre on: the glyph search
        // above finds the first cave glyph the whole plate draws, which at
        // this rung can be a DIFFERENT cave than the centred one if more
        // than one is in view. `site_note` gates on exactly this facet, so
        // discovery must be recorded against the same one it reads.
        let facet = d.world_view_tile().facet;
        let ids = plate::sites_standing_in(&d.sites, &facet);
        let id = ids.first().copied().unwrap_or_else(|| site.feature_id());
        d.discovered_mut_for_test().record(id);
        d.refresh_strip();
        let after = d
            .strip_text()
            .expect("the world view has a strip")
            .to_string();
        assert!(
            after.contains("a cave mouth stands here"),
            "a discovered cave must be mentioned once encountered: {after:?}"
        );
    }
}

#[cfg(test)]
mod caption_tests {
    use super::caption;
    use hornvale_game_core::schema::Sight;

    /// The seed-42 turn-0 fixture's sight values, as a literal.
    fn fixture_sight() -> Sight {
        Sight {
            observer: "bugbear".into(),
            channels: 3,
            chromatic: 2,
            projection: "yellow-blue".into(),
            preserves: "the short-to-long opposition; the red-green axis is not carried".into(),
            sun_altitude_deg: -56.010669,
            channel_roles: vec!["chromatic".into(), "chromatic".into(), "achromatic".into()],
            projection_slots: Some([1, 1, 0]),
            projection_norms: Some([3.862, 3.862, 1.98]),
        }
    }

    /// Serialises this binary's `NO_COLOR` regime flips. This bin's unit
    /// tests run as threads of one process under plain `cargo test`, and
    /// `NO_COLOR` is process-global state, so every mutation — and every
    /// caption assertion that reads it through the draw path — must hold
    /// this lock.
    static ENV_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    /// Run `f` with `NO_COLOR` removed, restoring whatever it was after.
    fn with_no_color_removed<R>(f: impl FnOnce() -> R) -> R {
        with_no_color_set_inner(None, f)
    }

    /// Run `f` with `NO_COLOR` set to a non-empty value, restoring
    /// whatever it was after.
    fn with_no_color_set<R>(value: &str, f: impl FnOnce() -> R) -> R {
        with_no_color_set_inner(Some(value), f)
    }

    fn with_no_color_set_inner<R>(value: Option<&str>, f: impl FnOnce() -> R) -> R {
        let _env = ENV_LOCK.lock().unwrap();
        let saved = std::env::var_os("NO_COLOR");
        // SAFETY: the caller holds ENV_LOCK, so no sibling thread in this
        // test binary touches the environment concurrently.
        unsafe {
            match value {
                Some(w) => std::env::set_var("NO_COLOR", w),
                None => std::env::remove_var("NO_COLOR"),
            }
        }
        let out = f();
        // SAFETY: as above — ENV_LOCK is held for the whole body.
        unsafe {
            match saved {
                Some(v) => std::env::set_var("NO_COLOR", v),
                None => std::env::remove_var("NO_COLOR"),
            }
        }
        out
    }

    /// Colour allowed and a sight declaration present: the disclosure is
    /// APPENDED to the resolver's text, never replacing it.
    #[test]
    fn colour_allowed_appends_the_disclosure() {
        with_no_color_removed(|| {
            let out = caption("googo ridge".to_string(), Some(&fixture_sight()));
            assert!(out.starts_with("googo ridge"), "{out}");
            assert!(out.contains("bugbear"), "{out}");
            assert!(out.contains("yellow-blue"), "{out}");
            assert!(out.contains("red-green"), "{out}");
        });
    }

    /// A non-empty `NO_COLOR` is the reader declining colour, and the
    /// caption goes with it — the picture and its honesty line are one
    /// channel, suppressed together.
    #[test]
    fn no_color_suppresses_the_caption() {
        with_no_color_set("1", || {
            let out = caption("googo ridge".to_string(), Some(&fixture_sight()));
            assert_eq!(out, "googo ridge");
        });
    }

    /// No sight declaration (an uncoloured scene omits the key entirely):
    /// the strip text passes through untouched.
    #[test]
    fn no_sight_no_caption() {
        with_no_color_removed(|| {
            let out = caption("googo ridge".to_string(), None);
            assert_eq!(out, "googo ridge");
        });
    }
}

#[cfg(test)]
mod completion_tests {
    use super::*;
    use crate::input::Action;
    use hornvale_game_core::CandidateSource;
    use hornvale_game_core::schema::{Narration, NounEntry};

    /// A narration whose noun catalog names the completion fixtures: two
    /// creatures sharing a long prefix, one unique thing.
    fn fixture_narration() -> Narration {
        Narration {
            prose: String::new(),
            nouns: vec![
                NounEntry {
                    noun: "Gnarlash".into(),
                    datum: String::new(),
                    kind: "creature".into(),
                },
                NounEntry {
                    noun: "Gnarlwood".into(),
                    datum: String::new(),
                    kind: "place".into(),
                },
                NounEntry {
                    noun: "bramble".into(),
                    datum: String::new(),
                    kind: "thing".into(),
                },
            ],
        }
    }

    /// A live driver whose scope has been overwritten with the fixture
    /// catalog — the real refresh path (`Driver::refresh`, from a parsed
    /// snapshot) is exercised by the integration suite; these tests need a
    /// KNOWN vocabulary.
    ///
    /// **`chart_scope` is explicitly reset, not merely left alone (fix
    /// round 1).** `Driver::start` already runs one `refresh()` before
    /// returning, against the REAL seed-42 flagship position — so
    /// `d.chart_scope` is genuinely non-empty at this point (see
    /// `a_chart_only_agent_name_completes_from_a_fresh_driver` in
    /// `tests/driver.rs`, which completes a live chart mark from a bare
    /// `Driver::start` with no setup at all). A doc comment here once
    /// claimed the opposite — that `start`-time default meant empty — which
    /// was false the moment `chart_scope` stopped being a field nobody
    /// wrote to. Resetting it is what makes "these tests need a KNOWN
    /// vocabulary" true rather than aspirational; pinned by
    /// `seeded_driver_offers_exactly_the_fixture_vocabulary`, immediately
    /// below, so a future collision between a fixture prefix and a real
    /// chart mark reddens here instead of silently passing by luck.
    pub(crate) fn seeded_driver() -> Driver {
        let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
        d.current_turn_scope.update(&fixture_narration());
        d.chart_scope = hornvale_game_core::ChartMarks::default();
        d.scope = hornvale_game_core::Lexicon::new(vec![
            Box::new(d.current_turn_scope.clone()),
            Box::new(d.chart_scope.clone()),
        ]);
        d
    }

    /// Pins the guarantee `seeded_driver`'s own doc makes, rather than
    /// leaving it to prose (fix round 1's own lesson: a comment is what
    /// went stale here, so a second comment is not the fix). Without this,
    /// a green suite proves nothing about whether `chart_scope` was
    /// actually cleared — every prefix this file's tests complete against
    /// (`"bram"`, `"gnar"`) simply happens not to collide with seed 42's
    /// real chart, today.
    #[test]
    fn seeded_driver_offers_exactly_the_fixture_vocabulary() {
        let d = seeded_driver();
        assert!(
            d.chart_scope.candidates().is_empty(),
            "chart_scope must be reset, not inherited from Driver::start's own refresh"
        );
        let candidates = d.scope.candidates();
        let mut names: Vec<&str> = candidates.iter().map(|c| c.name.as_str()).collect();
        names.sort_unstable();
        assert_eq!(
            names,
            vec!["Gnarlash", "Gnarlwood", "bramble"],
            "seeded_driver's fold must offer exactly the fixture vocabulary — anything \
             else means a real chart candidate leaked in and every prefix test below is \
             trusting collision-avoidance luck rather than a known catalog"
        );
    }

    #[test]
    fn unique_match_replaces_the_token() {
        let mut d = seeded_driver();
        d.line.set("examine bram".to_string());
        assert!(!d.apply(Action::Complete));
        assert_eq!(d.line.text(), "examine bramble");
        assert_eq!(d.hint, None);
    }

    #[test]
    fn ambiguous_match_extends_to_stem_and_sets_hint() {
        let mut d = seeded_driver();
        d.line.set("examine gnar".to_string());
        d.apply(Action::Complete);
        assert_eq!(d.line.text(), "examine Gnarl");
        assert_eq!(
            d.hint,
            Some(Hint {
                stem: "Gnarl".into(),
                matches: vec!["Gnarlash".into(), "Gnarlwood".into()],
            })
        );
    }

    #[test]
    fn typing_clears_a_pending_hint() {
        let mut d = seeded_driver();
        d.hint = Some(Hint {
            stem: "Gnarl".into(),
            matches: vec!["Gnarlash".into()],
        });
        d.apply(Action::Type('x'));
        assert_eq!(d.hint, None);
    }

    #[test]
    fn backspace_and_submit_also_clear_completion_state() {
        let mut d = seeded_driver();
        d.hint = Some(Hint {
            stem: "s".into(),
            matches: vec!["s".into()],
        });
        d.apply(Action::DeleteBack);
        assert_eq!(d.hint, None);
        // Submit on an empty buffer is its own documented no-op, but it must
        // still close any pending state (the buffer cannot be non-empty here,
        // so drive `clear_completion` through FocusAndType instead).
        d.hint = Some(Hint {
            stem: "s".into(),
            matches: vec!["s".into()],
        });
        d.apply(Action::FocusAndType('a'));
        assert_eq!(d.hint, None);
    }

    #[test]
    fn complete_on_an_empty_buffer_is_a_noop() {
        let mut d = seeded_driver();
        assert!(!d.apply(Action::Complete));
        assert_eq!(d.line.text(), "");
        assert_eq!(d.hint, None);
    }

    #[test]
    fn cycle_rotation_walks_matches_in_order_then_wraps() {
        let mut d = seeded_driver();
        d.tab_style = TabStyle::Cycle;
        d.line.set("examine gnar".to_string());
        d.apply(Action::Complete); // opens at matches[0]
        assert_eq!(d.line.text(), "examine Gnarlash");
        d.apply(Action::Complete);
        assert_eq!(d.line.text(), "examine Gnarlwood");
        d.apply(Action::Complete); // wraps to matches[0]
        assert_eq!(d.line.text(), "examine Gnarlash");
    }

    /// A history recall must close an open rotation: otherwise the stale
    /// state survives at the same caret offset and the next Tab rewrites
    /// the RECALLED line with a rotation match instead of completing it.
    #[test]
    fn history_recall_closes_an_open_cycle_rotation() {
        let mut d = seeded_driver();
        d.tab_style = TabStyle::Cycle;
        d.history.push("examine bramble".to_string());
        d.line.set("examine gnar".to_string());
        d.apply(Action::Complete); // opens a rotation at offset 8
        assert_eq!(d.line.text(), "examine Gnarlash");
        d.apply(Action::HistoryPrev); // recall overwrites the buffer
        assert_eq!(d.line.text(), "examine bramble");
        // The stale rotation (same start offset) must not step here and
        // rewrite "bramble" to "Gnarlwood"; a fresh completion of the
        // unique token is a no-op fill.
        d.apply(Action::Complete);
        assert_eq!(d.line.text(), "examine bramble");
    }

    #[test]
    fn typing_closes_an_open_cycle_rotation() {
        let mut d = seeded_driver();
        d.tab_style = TabStyle::Cycle;
        d.line.set("examine gnar".to_string());
        d.apply(Action::Complete);
        d.apply(Action::Type('h'));
        assert_eq!(d.cycle, None);
        // The stale rotation no longer steps: a fresh press re-completes
        // against the edited token ("Gnarlash" → still ambiguous? No —
        // "Gnarlah" matches nothing, so the buffer stands).
        d.apply(Action::Complete);
        assert_eq!(d.line.text(), "examine Gnarlashh");
    }

    /// The pure decision layer, exercised directly so the token-scan edge
    /// cases need no world at all.
    #[test]
    fn decision_layer_edge_cases() {
        let cands = |names: &[&str]| -> Vec<hornvale_game_core::Candidate> {
            names
                .iter()
                .map(|n| hornvale_game_core::Candidate {
                    name: n.to_string(),
                    category: hornvale_game_core::Category::Thing,
                })
                .collect()
        };
        // Whitespace before the caret: no token, no-op.
        assert_eq!(
            completion_decision("examine ", 8, &cands(&["bramble"])),
            CompletionDecision::Noop
        );
        // Empty text: no token.
        assert_eq!(
            completion_decision("", 0, &cands(&["bramble"])),
            CompletionDecision::Noop
        );
        // A token already equal to its single match: documented no-op.
        assert_eq!(
            completion_decision("bramble", 7, &cands(&["bramble"])),
            CompletionDecision::Noop
        );
        // No candidate starts with the token: no-op.
        assert_eq!(
            completion_decision("zz", 2, &cands(&["bramble"])),
            CompletionDecision::Noop
        );
        // Caret mid-word completes only up to the caret ("examine br|amble").
        assert_eq!(
            completion_decision("examine bramble", 11, &cands(&["bramble"])),
            CompletionDecision::Fill("bramble".into())
        );
    }
}

/// The Newel, Task 4: the chart scope's own discovery gate (decision
/// 0670), proven against `Driver::refresh`'s REAL wiring — not a
/// test-supplied closure. `hornvale-game-core`'s own test suite
/// (`clients/game/core/tests/lexicon.rs`) proves `ChartMarks`' abstract
/// filtering mechanism is non-vacuous in isolation; these tests prove the
/// production closure this crate feeds it (`Driver::site_is_discovered`:
/// `FacetId::unpack` → `Facet::coord` → `NearestVertexIndex::nearest` →
/// `Discovered::contains`) resolves and gates correctly against a real
/// seed-42 world.
#[cfg(test)]
mod chart_scope_tests {
    use super::*;
    use crate::discovery::FeatureId;
    use hornvale_game_core::CandidateSource;

    fn test_driver() -> Driver {
        Driver::start(42, hornvale_vessel::PossessTarget::Flagship).expect("seed 42 generates")
    }

    /// `Driver::site_is_discovered` directly, against a real,
    /// self-consistent round trip: pack the observer's OWN position,
    /// unpack it back, resolve its vertex the same way the closure does.
    /// No second settlement needs to stand within chart radius of
    /// wherever a test driver happens to start.
    #[test]
    fn site_is_discovered_gates_a_settlement_and_a_cave_separately() {
        let d = test_driver();
        let position = d.session.position();
        let room = position
            .pack()
            .expect("a live possession's own position always packs")
            .0;
        let vertex = d.nearest.nearest(
            &d.geo,
            position.coord().latitude,
            position.coord().longitude,
        );

        let empty = Discovered::default();
        assert!(
            !Driver::site_is_discovered(&d.geo, &d.nearest, &empty, "settlement", room),
            "nothing has been discovered yet"
        );

        let mut settlement_discovered = Discovered::default();
        settlement_discovered.record(FeatureId::Settlement(vertex));
        assert!(
            Driver::site_is_discovered(
                &d.geo,
                &d.nearest,
                &settlement_discovered,
                "settlement",
                room
            ),
            "the settlement at this room's own vertex is now discovered"
        );

        // A CAVE discovery at the identical vertex must not leak into the
        // SETTLEMENT gate — the two are different `FeatureId` variants
        // (`discovery.rs`'s own module doc on why a point site's kind is
        // part of its identity).
        let mut cave_discovered = Discovered::default();
        cave_discovered.record(FeatureId::Cave(vertex));
        assert!(
            !Driver::site_is_discovered(&d.geo, &d.nearest, &cave_discovered, "settlement", room),
            "a discovered cave must not gate a settlement mark open"
        );
    }

    /// An unpackable room id gates closed rather than panicking — the same
    /// defensive posture `Self::resolve_walk_band`'s own
    /// `FacetId::unpack().ok()?` takes.
    #[test]
    fn an_unpackable_room_id_gates_closed_not_panics() {
        let d = test_driver();
        // `FacetId(0)`'s sentinel bit is unset — `FacetId::unpack`'s own
        // doc names this malformed.
        assert!(!Driver::site_is_discovered(
            &d.geo,
            &d.nearest,
            d.discovered(),
            "settlement",
            0
        ));
    }

    /// End to end through `Driver::refresh` itself: the flagship's own
    /// home settlement is on `chart_scope`'s own roster only once
    /// `Self::discovered_mut_for_test` records it — proving `refresh`'s
    /// wiring, not merely `ChartMarks`' abstract mechanism.
    #[test]
    fn refresh_gates_chart_scope_against_the_real_discovery_ledger() {
        let mut d = test_driver();
        let home = hornvale_game_core::Snapshot::parse(&d.snapshot())
            .unwrap()
            .me
            .settlement;
        assert!(!home.is_empty(), "sanity: the flagship names its own home");

        assert!(
            !d.chart_scope.candidates().iter().any(|c| c.name == home),
            "the home settlement must not be offered by the chart scope before discovery"
        );

        let position = d.session.position();
        let vertex = d.nearest.nearest(
            &d.geo,
            position.coord().latitude,
            position.coord().longitude,
        );
        d.discovered_mut_for_test()
            .record(FeatureId::Settlement(vertex));
        d.refresh();

        assert!(
            d.chart_scope.candidates().iter().any(|c| c.name == home),
            "the home settlement must be offered by the chart scope once discovered"
        );
    }
}

/// The bare-`x` noun prompt (spec §4.4): submitting `x` alone asks for a
/// noun instead of burning a turn on "Examine what?".
#[cfg(test)]
mod noun_prompt_tests {
    use super::completion_tests::seeded_driver;
    use super::*;
    use crate::input::Action;

    /// Enter the modal by submitting bare `x`, the way a player would.
    fn entered_driver() -> Driver {
        let mut d = seeded_driver();
        d.line.set("x".to_string());
        d.apply(Action::Submit);
        d
    }

    #[test]
    fn bare_x_enters_the_prompt_instead_of_burning_a_turn() {
        let mut d = seeded_driver();
        d.line.set("x".to_string());
        assert!(
            !d.apply(Action::Submit),
            "entering the prompt is not a turn"
        );
        assert_eq!(d.line.text(), "examine ");
        assert!(d.noun_prompt.is_some());
        assert_eq!(d.echo, None, "nothing was submitted");
    }

    #[test]
    fn typing_builds_the_noun_after_the_prefix() {
        let mut d = entered_driver();
        d.apply(Action::Type('b'));
        d.apply(Action::Type('r'));
        assert_eq!(d.line.text(), "examine br");
    }

    #[test]
    fn backspace_clamps_at_the_dispatched_prefix() {
        let mut d = entered_driver();
        d.apply(Action::DeleteBack);
        assert_eq!(d.line.text(), "examine ");
    }

    #[test]
    fn submitting_the_prompt_dispatches_examine_and_exits() {
        let mut d = entered_driver();
        for c in "bramble".chars() {
            d.apply(Action::Type(c));
        }
        let released = d.apply(Action::Submit);
        assert!(!released, "examining bramble does not end the possession");
        assert!(d.noun_prompt.is_none(), "the modal exits on submit");
        assert_eq!(d.echo.as_deref(), Some("examine bramble"));
        d.history.prev();
        assert!(d.history.next().is_none(), "the dispatch entered history");
    }

    #[test]
    fn esc_cancels_and_restores_the_saved_buffer() {
        let mut d = seeded_driver();
        d.focus = Focus::Cli; // the player submits `x` from the command line
        d.line.set("look".to_string());
        for _ in 0..2 {
            d.line.caret_left();
        }
        d.line.set("x".to_string()); // entering saves; see entered-by-hand below
        // enter by hand so we control what was saved
        d.noun_prompt = None;
        d.line.set("look".to_string());
        while d.line.caret() < 2 {
            d.line.caret_right();
        }
        d.line.set("x".to_string());
        d.apply(Action::Submit);
        d.apply(Action::ToggleFocus); // Esc
        assert!(d.noun_prompt.is_none());
        assert_eq!(d.line.text(), "x", "the saved buffer is restored verbatim");
        assert_eq!(
            d.focus,
            Focus::Cli,
            "Esc in the modal does not toggle focus"
        );
    }
}

/// The prose pane's two client-side halves (The Quadrat, Task 8, spec
/// §4.2): a bare `map` is a MODE GESTURE and not a fetch (decision 0290),
/// and no escape sequence reaches the prose channel.
#[cfg(test)]
mod prose_pane_tests {
    use super::*;

    /// A fresh seed-42 flagship driver — the same construction every other
    /// test module in this file uses.
    fn test_driver() -> Driver {
        Driver::start(42, PossessTarget::Flagship).expect("seed 42 generates")
    }

    /// Type `line` into the command buffer and submit it, the way a player
    /// does. `FocusAndType` rather than `Type` because that is what the
    /// input table produces for a printable key under every focus.
    fn submit(d: &mut Driver, line: &str) -> bool {
        for ch in line.chars() {
            d.apply(Action::FocusAndType(ch));
        }
        d.apply(Action::Submit)
    }

    /// `driver.rs`'s own comment already argued `map` is a mode gesture and
    /// not a fetch. The code called `self.handle("map")` first anyway, so
    /// the sim answered the gesture with a picture — the reported defect.
    ///
    /// **The snapshot is what discriminates.** The focus assertion alone
    /// passed before this change too; only the unchanged snapshot can tell
    /// "entered the map" from "entered the map AND burned a turn on a chart
    /// nobody asked for".
    #[test]
    fn a_bare_map_gesture_does_not_send_the_verb() {
        let mut d = test_driver();
        let before = d.snapshot();
        assert!(!submit(&mut d, "map"), "a mode gesture never releases");
        assert_eq!(d.focus(), Focus::Map, "the gesture did not focus the map");
        assert_eq!(
            d.snapshot(),
            before,
            "the gesture sent a verb and advanced the session"
        );
    }

    /// The gesture is a gesture from the map too. Re-submitting `map` while
    /// already focused must still swallow the line rather than falling
    /// through to the sim — the old guard was about not paying for a
    /// redundant strip refresh, never about sending the verb.
    #[test]
    fn a_bare_map_gesture_from_the_map_still_sends_nothing() {
        let mut d = test_driver();
        submit(&mut d, "map");
        let before = d.snapshot();
        submit(&mut d, "map");
        assert_eq!(d.focus(), Focus::Map);
        assert_eq!(
            d.snapshot(),
            before,
            "a second gesture advanced the session"
        );
    }

    /// Only the BARE form changes, and that is load-bearing: `map out N` is
    /// the diagnostic path that caught The Quire's wrong projection, where
    /// the client and the sim were rendered side by side over the identical
    /// thirty-one facets and only one was right. It guards spec §5's H2.
    #[test]
    fn map_out_n_still_returns_the_sims_own_picture() {
        let mut d = test_driver();
        submit(&mut d, "map out 1");
        let v: serde_json::Value = serde_json::from_str(&d.snapshot()).expect("a live snapshot");
        let prose = v["narration"]["prose"]
            .as_str()
            .expect("the snapshot carries this turn's prose");
        assert!(
            prose.contains('\n'),
            "map out 1 did not return a multi-line picture, got {prose:?}"
        );
        assert_ne!(d.focus(), Focus::Map, "an argument form moved the plate");
    }

    /// A REAL seed-42 walk-band chart, made tinted.
    ///
    /// The escape defect is latent, and latent is why it survived a whole
    /// campaign of probes: seed 42 at turn 0 draws a band of water, marks
    /// and the observer, so the `colour` lens reports "0 tinted, 31
    /// withheld" and emits no escape at all. `p.ground && p.color.is_some()`
    /// is what the lens tints, so a colour alone is not enough — the facet
    /// must also DRAW its ground. So this takes the session's own chart and
    /// makes every facet but the observer's a coloured piece of dry land:
    /// no marks (a mark substitutes `#`/`&` and withholds the tint), a
    /// water index outside the legend (which `terrain_glyph` resolves to
    /// `dry-land`, the one arm that draws the impedance glyph), and a
    /// colour.
    ///
    /// Everything else — the projection, the observer, the facet count — is
    /// the sim's own, so the picture this renders is a picture the world
    /// really can produce.
    fn tinted_chart_fixture() -> hornvale_scene::SurroundsScene {
        let d = test_driver();
        let mut scene = d.session.purview(0).expect("seed 42 charts its walk band");
        let dry = scene.water_legend.len() as u32;
        for cell in &mut scene.cells {
            if cell.state == "here" {
                continue;
            }
            cell.marks.clear();
            cell.water = dry;
            cell.color = Some([120, 140, 60]);
        }
        scene
    }

    /// Drop every SGR escape from `s`, leaving the glyphs they wrapped in
    /// their own columns — the sim's picture as the prose channel is supposed
    /// to receive it. `\x1b[` through the terminating `m`, and nothing else:
    /// this exists to derive an expectation from the sim's own output, never
    /// to filter what the client draws.
    fn strip_sgr(s: &str) -> String {
        let mut out = String::with_capacity(s.len());
        let mut chars = s.chars();
        while let Some(c) = chars.next() {
            if c != '\u{1b}' {
                out.push(c);
                continue;
            }
            for c in chars.by_ref() {
                if c == 'm' {
                    break;
                }
            }
        }
        out
    }

    /// What this client's prose channel would show for `scene`: the sim's
    /// own chart, drawn through the lens the session picks for a possession
    /// with eyes, laid out by the prose pane and read back off the grid.
    ///
    /// Rendered through the REAL pane rather than asserted on the renderer's
    /// output, because the renderer is not where the defect lives — the
    /// prose channel is. The grid stores one `char` per column and carries
    /// no colour, so the question worth asking is what actually reached it.
    fn render_for_client(scene: &hornvale_scene::SurroundsScene) -> String {
        let prose = hornvale_scene::render_surrounds_ascii(scene, "colour", &[]);
        let narration = hornvale_game_core::Narration {
            prose,
            nouns: Vec::new(),
        };
        let (w, h) = (120u16, 60u16);
        let mut grid = hornvale_game_core::Grid::new(w, h);
        hornvale_game_core::entry::draw(
            &narration,
            &mut grid,
            (0, 0),
            w,
            h,
            Focus::Cli,
            hornvale_game_core::CommandLine { text: "", caret: 0 },
            None,
            None,
        );
        (0..h)
            .map(|y| {
                (0..w)
                    .map(|x| grid.get(x, y).and_then(|c| c.glyph).unwrap_or(' '))
                    .collect::<String>()
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// The premise, asserted rather than assumed: the fixture really is
    /// tinted, and the sim really does emit SGR for it. Without this the
    /// test below would pass on an untinted chart and prove nothing — which
    /// is exactly the state seed 42 turn 0 is in.
    #[test]
    fn the_tinted_fixture_really_does_make_the_sim_emit_escapes() {
        let scene = tinted_chart_fixture();
        let prose = hornvale_scene::render_surrounds_ascii(&scene, "colour", &[]);
        assert!(
            prose.contains('\u{1b}'),
            "the fixture is not tinted, so the test below cannot discriminate"
        );
        assert!(
            prose.matches("\u{1b}[38;2;").count() > 1,
            "one tinted glyph is not a picture; the fixture must tint the band, \
             got {prose:?}"
        );
    }

    /// `surrounds_ascii.rs` wraps every tinted glyph in
    /// `\x1b[38;2;r;g;bm … \x1b[0m`; the grid stores one `char` per column
    /// and has nowhere to put an SGR parameter, so those bytes would be
    /// drawn as glyphs — `[`, `3`, `8`, `;` and the rest, one per column,
    /// across a picture. The client applies its own ink from the wire's
    /// `color` field and has no use for SGR.
    #[test]
    fn no_escape_sequence_reaches_the_prose_channel() {
        let scene = tinted_chart_fixture();
        let out = render_for_client(&scene);
        assert!(
            !out.contains('\u{1b}'),
            "an escape sequence reached the prose channel"
        );
        assert!(
            !out.contains("38;2;"),
            "an SGR parameter reached the prose channel as literal glyphs"
        );
    }

    /// And the picture SURVIVES the strip: dropping the escapes must leave
    /// the glyphs they wrapped, in their own columns, not a blank pane.
    #[test]
    fn stripping_the_escapes_leaves_the_picture_standing() {
        let scene = tinted_chart_fixture();
        let out = render_for_client(&scene);
        assert!(
            out.contains('@'),
            "the observer's own glyph did not survive, got {out:?}"
        );
        // And the picture's SHAPE with it: an indented picture row must
        // reach the grid with its indent intact. Dropping the escapes and
        // then collapsing the spaces would be the same defect wearing the
        // fix's clothes.
        //
        // **RE-DERIVED FROM THE SIM, not pinned.** This read
        // `out.lines().any(|l| l.starts_with("      _"))` — six columns then
        // an underscore, which was the old triangular chart's own top row.
        // The Pavement's square lattice draws a different row shape (eight
        // columns then `::`), so the literal went stale and the assertion
        // failed for a reason that had nothing to do with escapes. The indent
        // now comes from the sim's own output with the SGR stripped, which is
        // exactly the thing the client is supposed to reproduce.
        let sim = strip_sgr(&hornvale_scene::render_surrounds_ascii(
            &scene,
            "colour",
            &[],
        ));
        let indented: Vec<(usize, char)> = sim
            .lines()
            .filter_map(|l| {
                let indent = l.chars().take_while(|c| *c == ' ').count();
                let first = l.trim_start().chars().next()?;
                // PICTURE rows only. A caption carries lowercase letters and
                // is reflowed by the pane at 120 columns, so its line shape
                // is the pane's business and not this assertion's; the chart's
                // own glyph vocabulary carries no lowercase letter at all.
                if indent == 0 || l.chars().any(|c| c.is_ascii_lowercase()) {
                    return None;
                }
                Some((indent, first))
            })
            .collect();
        assert!(
            !indented.is_empty(),
            "the sim's own picture has no indented row, so this assertion would check \
             nothing. Got {sim:?}"
        );
        for (indent, first) in indented {
            let want = format!("{}{first}", " ".repeat(indent));
            assert!(
                out.lines().any(|l| l.starts_with(&want)),
                "the picture's indent did not survive: no line starts with {want:?}, got {out:?}"
            );
        }
    }

    /// **The premise [`hornvale_game_core`]'s prose-vs-picture classifier
    /// rests on, made mechanical.** `entry::reads_as_prose` calls a line
    /// prose when it carries two DISTINCT letters, which is sound exactly
    /// as long as the sim's pre-formatted vocabularies carry at most one
    /// letter between them. A chamber plan carries none
    /// (`windows/vessel/src/lattice/render.rs`: `. # + @`); a chart
    /// carries `A`, the top impedance rung, and nothing else — but that is
    /// a fact about `surrounds_ascii.rs`'s glyph table, in another crate,
    /// which nothing else would notice changing.
    ///
    /// So this renders a REAL chart at every impedance rung and reads the
    /// picture back. `core` cannot write this test — it has no hornvale
    /// dependency, by design — and that is precisely why the guard lives
    /// on this side of the boundary rather than as a sentence in a doc
    /// comment.
    ///
    /// The slice is exact rather than heuristic: with `ways` empty and the
    /// legend cleared, the `terrain` lens emits three caption lines and
    /// then nothing but grid rows, and the three prefixes are asserted
    /// before the slice is taken.
    #[test]
    fn the_chart_vocabulary_carries_at_most_one_letter() {
        let d = test_driver();
        let base = d.session.purview(0).expect("seed 42 charts its walk band");
        let dry = base.water_legend.len() as u32;
        let mut letters: std::collections::BTreeSet<char> = std::collections::BTreeSet::new();
        let mut glyphs: std::collections::BTreeSet<char> = std::collections::BTreeSet::new();
        for relief in 0..=5u32 {
            for (openness, roughness) in [(-1.0, 1.0), (1.0, 0.0), (0.0, -1.0)] {
                let mut scene = base.clone();
                scene.legend.clear();
                for cell in &mut scene.cells {
                    if cell.state == "here" {
                        continue;
                    }
                    cell.marks.clear();
                    cell.water = dry;
                    cell.relief = relief;
                    cell.micro.openness = openness;
                    cell.micro.relief = roughness;
                }
                let out = hornvale_scene::render_surrounds_ascii(&scene, "terrain", &[]);
                let lines: Vec<&str> = out.lines().collect();
                assert!(lines[0].starts_with("[lens: terrain"), "{:?}", lines[0]);
                assert!(lines[1].starts_with("  placement:"), "{:?}", lines[1]);
                assert!(lines[2].starts_with("  epistemic:"), "{:?}", lines[2]);
                for row in &lines[3..] {
                    assert!(
                        !row.starts_with("  legend:") && !row.starts_with("  ways on:"),
                        "the slice caught a caption line: {row:?}"
                    );
                    for ch in row.chars().filter(|c| !c.is_whitespace()) {
                        glyphs.insert(ch);
                        if ch.is_alphabetic() {
                            letters.insert(ch);
                        }
                    }
                }
            }
        }
        // The premise of the premise: the sweep must actually have reached
        // the alpine rung, or an empty `letters` proves nothing.
        assert!(
            letters.contains(&'A'),
            "the sweep never drew the alpine glyph; it cannot say anything \
             about the vocabulary. glyphs drawn: {glyphs:?}"
        );
        assert_eq!(
            letters.len(),
            1,
            "the chart vocabulary grew a second letter ({letters:?}), so a \
             picture row can now carry two distinct letters and \
             `entry::reads_as_prose` will word-wrap it — see that function"
        );
    }
}
