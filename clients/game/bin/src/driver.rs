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
//! scene is re-derived with `self.session.purview(0)` — the identical call
//! `Session::snapshot` itself makes for the walk band.
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
use hornvale_game_core::{CandidateSource, Cursor, Focus};
use hornvale_kernel::{FacetId, NearestVertexIndex, Seed, Value, Vertex, World};
use hornvale_language::{MorphOptions, Phonology};
use hornvale_terrain::GeneratedTerrain;
use hornvale_terrain::TerrainPins;
use hornvale_terrain::landscape::VertexFeatureIndex;
use hornvale_vessel::{
    PossessOpts, PossessTarget, Session, Turn, VesselError, WorldContext, snapshot_json,
};
use hornvale_worldgen::{
    BuildError, SettlementPins, SkyChoice, WorldComponents, build_world, gazetteer_features,
    language_of_in, morph_options, resolve_chain_at, terrain_of,
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

/// What the strip says when the resolver ran and genuinely found no
/// individuated feature at the observer's vertex — real terrain below every
/// class's individuation floor, not "nothing there" (seed 42 measured 377
/// of 40,962 vertices like this; spec §3.4).
const UNNAMED_TERRAIN: &str = "unnamed terrain";

/// The fixed, sim-authored prefix `windows/vessel/src/session.rs`'s
/// `delve_at` prints on a SUCCESSFUL delve (`Session::delve`'s own
/// `Surface -> Undercroft` transition, F9's cited arrival predicate for
/// caves) — never on any refusal (a sealed cave, no cave at all, already
/// underground, or already inside a structure each print their own
/// distinct refusal text). This is the only signal that exists for cave
/// discovery: the wire's own `band` tag deliberately folds underground
/// into `"walk"` (see that module's own test,
/// `the_underground_band_folds_into_walk_as_map_does`), so there is no
/// typed alternative to reading the turn's own narration — the same
/// category of read `Driver` already does everywhere else (the strip, the
/// snapshot text itself), never a fabricated string.
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
    /// settlement nearest to (The Portolan part II, Task 5) — built ONCE
    /// here at `start`, the same "derive once, never per-turn" discipline
    /// `index`/`nearest` already follow. Not itself a discovery record:
    /// this is the plate's own point-site ROSTER (where a settlement
    /// stands, ground truth, always known to the client that draws the
    /// map), gated by [`Self::discovered`] at draw time, never here.
    settlements: BTreeSet<Vertex>,
    /// Every vertex carrying a cave mouth, scanned once here at `start` for
    /// the same reason `settlements` is: `plate::draw_feature_layer`
    /// PROJECTS each site rather than asking every screen cell whether its
    /// sample happens to be one, so it needs the roster up front. A
    /// per-render scan of all 40,962 vertices would be the cost the projection
    /// exists to avoid.
    caves: BTreeSet<Vertex>,
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
    /// The completion vocabulary's v1 scope, refreshed from every parsed
    /// snapshot (see [`Driver::refresh`]). Held DIRECTLY rather than behind
    /// a [`hornvale_game_core::Lexicon`]: spec §3 registers exactly one
    /// scope in v1, so the fold (`Lexicon::candidates`'s ordered
    /// first-wins dedup over scopes) has nothing to fold — wrapping a single
    /// scope in a `Vec<Box<dyn CandidateSource>>` would buy allocation and
    /// indirection without behaviour. The Lexicon becomes the right shape
    /// the day a second scope lands; this field swaps for it then.
    scope: hornvale_game_core::CurrentTurnNouns,
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

impl Driver {
    /// Build a fresh world for `seed` (default sky/terrain/settlement pins,
    /// generated sky — the same defaults `clients/vessel/wasm`'s `hv_start`
    /// uses), derive its [`WorldContext`] once, and start a possession of
    /// `target`.
    // Named construction site (decision 0092): `terrain_of` re-derives the
    // tectonic globe once here, at world load, to build the Portolan's
    // terrain-feature index — never per-turn (see `VertexFeatureIndex`'s doc).
    #[allow(clippy::disallowed_methods)]
    pub fn start(seed: u64, target: PossessTarget) -> Result<Driver, DriverError> {
        let world = build_world(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .map_err(DriverError::Genesis)?;
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

        // The Portolan part II, Task 5: the point-site roster — every
        // terrain vertex a live settlement's own committed `(latitude,
        // longitude)` resolves nearest to. Built once here, the same
        // ledger read `h4_locked_worlds_settlements_are_never_in_the_clamp`
        // already exercises dev-only; this is the shipped-path use of it.
        // Ground truth, never gated — [`plate::draw_with`] is where
        // `discovered` decides whether a member of this set is ever drawn.
        let settlements: BTreeSet<Vertex> = world_ref
            .ledger
            .find(hornvale_settlement::IS_SETTLEMENT)
            .filter_map(|fact| {
                let lat = match world_ref
                    .ledger
                    .value_of(fact.subject, hornvale_settlement::LATITUDE)
                {
                    Some(Value::Number(n)) => *n,
                    _ => return None,
                };
                let lon = match world_ref
                    .ledger
                    .value_of(fact.subject, hornvale_settlement::LONGITUDE)
                {
                    Some(Value::Number(n)) => *n,
                    _ => return None,
                };
                Some(nearest.nearest(&geo, lat, lon))
            })
            .collect();

        // The cave roster, scanned once. `cave_at` is a pure read of the
        // vertex's own stratigraphic column, so this is a scan of the mesh
        // rather than a derivation — and doing it here rather than per
        // render is the whole point of projecting sites instead of
        // sampling for them.
        let caves: BTreeSet<Vertex> = (0..geo.vertex_count())
            .map(|i| Vertex(i as u32))
            .filter(|&c| terrain.cave_at(c).is_some())
            .collect();

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
            settlements,
            caves,
            visited: Visited::default(),
            discovered: Discovered::default(),
            plate_height: FLOOR_PLATE_CONTENT_HEIGHT,
            term_w: hornvale_game_core::MIN_WIDTH,
            term_h: hornvale_game_core::MIN_HEIGHT,
            namer: (species, ph, morph),
            line: Line::new(),
            history: History::new(),
            echo: None,
            scope: hornvale_game_core::CurrentTurnNouns::default(),
            tab_style: TabStyle::Hint,
            hint: None,
            cycle: None,
            noun_prompt: None,
            on_walk_band: true,
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
        self.focus = match self.focus {
            Focus::Cli => Focus::Walk,
            Focus::Map => Focus::Walk,
            Focus::Walk => Focus::Cli,
        };
        self.strip = None;
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
    /// **The plate's own size is derived from the SAME fit `compose` itself
    /// applies, never a second copy of it**:
    /// [`hornvale_game_core::spread::world_plate_width`] gives the width,
    /// and the height follows from
    /// [`hornvale_game_core::spread::GLYPH_ASPECT`] — the ratio `core`
    /// exposes rather than this module hardcoding a second `2` (see that
    /// constant's own doc). This is what lets `world_plate_width`'s
    /// `pub`-ness do its job: `compose` and this method size their two
    /// `Grid`s from the identical computation.
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
    pub fn world_plate(&self, w: u16, h: u16) -> hornvale_game_core::Grid {
        let (plate_width, plate_height) = Self::world_plate_dims(w, h);
        plate::draw(
            &self.terrain,
            &self.geo,
            &self.nearest,
            &self.frame,
            &self.window,
            plate_width,
            plate_height,
            &self.settlements,
            &self.caves,
            &self.discovered,
        )
    }

    /// The world plate's own size for a `w`-by-`h` terminal — the SAME fit
    /// `compose` applies, computed in one place because two callers now need
    /// it ([`Self::world_plate`] draws a whole plate;
    /// [`Self::world_plate_for_redraw`] draws its two layers separately and
    /// must size both identically). A second copy of this arithmetic is the
    /// bug shape `content_height`'s own doc warns about.
    fn world_plate_dims(w: u16, h: u16) -> (u16, u16) {
        let width = hornvale_game_core::spread::world_plate_width(w, h);
        (width, width / hornvale_game_core::spread::GLYPH_ASPECT)
    }

    /// The world plate to hand [`hornvale_game_core::render_with`] for a
    /// `w`-by-`h` redraw — `Some` when [`Focus::Map`] is focused AND the
    /// band is one whose plate is the raster ([`Self::raster_is_drawn`]),
    /// `None` otherwise.
    ///
    /// **The RUNG is no longer part of the gate, and the BAND is (The
    /// Quadrat, Task 6 and its fix round 1).** It used to be
    /// `Focus::Map && world_view()`, where `world_view()` meant "some rung
    /// coarser than band B"; band B draws the raster now, so that clause had
    /// no discriminating answer left. What replaced it is not "nothing":
    /// dropping to `Focus::Map` alone let a chamber-band `map` replace the
    /// floor plan with the world raster, which the campaign's spec promises
    /// not to touch — see [`Self::raster_is_drawn`].
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
        if self.focus != Focus::Map || !self.raster_is_drawn() {
            return None;
        }
        let (plate_width, plate_height) = Self::world_plate_dims(w, h);
        let mut grid = self.tiles.compose(
            &self.terrain,
            &self.geo,
            &self.nearest,
            &self.frame,
            &self.window,
            plate_width,
            plate_height,
            plate::colour_allowed(),
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
            &self.settlements,
            &self.caves,
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
    /// submitted line whose first token is exactly `map` additionally
    /// enters the map focus (exact-after-trim, mirroring `Session::handle`'s
    /// first-token convention).
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
                self.focus = Focus::Cli;
                self.strip = None;
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
                let released = self.handle(&taken);
                // BARE `map` — and only bare `map` — enters the map focus.
                // This mirrors a convention the sim already keeps rather
                // than inventing one: `Session::handle` splits its own
                // bare-from-argument forms on `rest.is_empty()` (see the
                // `"map" if self.inside.is_some() && rest.is_empty()` and
                // `"eyes" if rest.is_empty()` arms), because the two mean
                // different things. So `" map "` triggers; `"map x"`,
                // `"map out 2"` and `"examine map"` do not.
                //
                // `map out N` is the case worth stating, because "it drew a
                // chart, so focus it" is the plausible wrong answer:
                // `Session::map` takes `&self` and returns prose, so NO
                // argument form can move the plate. The plate is redrawn
                // from `Spatial` every turn regardless (`spread::compose`),
                // which is why submitting `map` is a MODE GESTURE and not a
                // fetch. Focusing after `map out 2` would hand the player a
                // cursor on an unzoomed plate they did not ask about.
                //
                // Guarded on not already being focused so re-submitting
                // `map` from the map does not pay a strip refresh for an
                // identical answer.
                if taken.trim() == "map" && self.focus != Focus::Map {
                    self.enter_map();
                }
                released
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
        self.centre_band_b_on_the_observer();
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

    /// The active plate's own width and height, in grid cells — the walk
    /// band's fixed [`hornvale_game_core::spread::PLATE_WIDTH`] and
    /// `self.plate_height` when the world view is off, or the world
    /// plate's own fit when it is on: the SAME
    /// [`hornvale_game_core::spread::world_plate_width`]
    /// [`Driver::world_plate`] itself draws into, its height following
    /// [`hornvale_game_core::spread::GLYPH_ASPECT`] the identical way that
    /// method derives it.
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
            let width = hornvale_game_core::spread::world_plate_width(self.term_w, self.term_h);
            let height = width / hornvale_game_core::spread::GLYPH_ASPECT;
            (width, height)
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
            // **INSIDE THE GUARD, and the placement is the whole point.**
            // Fix round 1, F1: what used to sit here sat OUTSIDE, so a
            // SATURATING press at the ladder's ceiling acted anyway —
            // measured jumping `origin_col` 21169 -> 2176, throwing away
            // ~19,000 tiles of the player's own scroll on a keypress this
            // method's own doc says changes nothing.
            // `zoom_plus_at_the_ladders_ceiling_keeps_the_players_scroll`
            // still pins it, and is NOT made redundant by the anchor: the
            // anchor is a no-op at an unchanged rung (a tile centre
            // re-projects to its own tile), so a regression that moved this
            // block back outside would once again be invisible to every
            // other test in this file.
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
    /// terrain-feature index — [`UNNAMED_TERRAIN`] if that chain comes up
    /// empty at any step.
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
        let base = self
            .resolve_walk_band()
            .or_else(|| self.resolve_world_view())
            .unwrap_or_else(|| UNNAMED_TERRAIN.to_string());
        let text = self.world_view_caption(base);
        if self.at_walk_band_rung() {
            caption(text, sight.as_ref())
        } else {
            text
        }
    }

    /// Append §3.3's clamp/central-line caption, and F5's resolution
    /// disclosure when the active zoom covers more than one terrain vertex
    /// per character, to `base` (the resolved containment chain, or
    /// [`UNNAMED_TERRAIN`]). Unlike the walk band's [`caption`], this runs
    /// UNCONDITIONALLY — the world view carries no sight channel to gate on
    /// ([`Self::resolve_world_view`]'s own doc), and both captions are true
    /// of the picture itself, independent of whether the cursor happens to
    /// sit on a named feature.
    fn world_view_caption(&self, base: String) -> String {
        let mut text = base;
        text.push_str(" — ");
        text.push_str(&mercator::clamp_caption(&self.frame));
        text.push_str(" — ");
        text.push_str(&self.rung_caption());
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

    /// The world view's own resolved [`hornvale_kernel::Vertex`] at the
    /// cursor's current screen position — [`plate::terrain_at_tile`], the
    /// SAME per-tile mesh addressing [`plate::draw_with`] itself paints
    /// from (one source of truth; see that function's own doc), asked for
    /// the painted class's own representative vertex.
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
    /// return a vertex whose class
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
    fn world_view_vertex(&self) -> hornvale_kernel::Vertex {
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
        )
        .vertex
    }

    /// The world view's own resolution chain: [`Self::world_view_vertex`] to
    /// the FULL containment chain there (Task 4, Step 1) — every feature at
    /// the resolved vertex, most specific first, each with its class named in
    /// prose (design spec §5).
    fn resolve_world_view(&self) -> Option<String> {
        let vertex_id = self.world_view_vertex();
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
        // The real scene, re-derived with the SAME call `Session::snapshot`
        // itself makes for the walk band (`purview(0)`, `windows/vessel/src/
        // session.rs`'s own comment on that call site) — and the same one
        // the draw path flattens, so the resolver and the picture cannot
        // disagree about which facets exist either.
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
    /// **The band is read off the SNAPSHOT, not off `purview`'s success.**
    /// `Session::purview(0)` charts the session's walk-band position and
    /// succeeds indoors too — it is the surface around the structure you are
    /// standing in — so using it as the band test would draw a perception
    /// overlay of outdoor facets over a chamber-band spread. The snapshot's
    /// own `spatial` tag is the wire's answer to "which band is this", and
    /// it is the one the rest of this module already asks.
    fn walk_band_scene(&self) -> Option<hornvale_scene::SurroundsScene> {
        let snap = hornvale_game_core::Snapshot::parse(&self.cached).ok()?;
        match snap.spatial {
            hornvale_game_core::Spatial::Walk { .. } => self.session.purview(0).ok(),
            hornvale_game_core::Spatial::Chamber { .. } => None,
        }
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
        if !self.at_walk_band_rung() {
            return;
        }
        let Some(scene) = self.walk_band_scene() else {
            return;
        };
        plate::draw_perception_layer(
            dst,
            &self.frame,
            &self.window,
            plate::colour_allowed(),
            &perceived_facets(&scene),
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
        // replace the noun catalog from the new narration. On failure the
        // previous catalog stands rather than being cleared: stale
        // candidates complete nothing harmful, and an empty one mid-session
        // would be a regression masquerading as caution.
        if let Ok(snap) = hornvale_game_core::Snapshot::parse(&self.cached) {
            self.scope.update(&snap.narration);
            // The band, from the same parse (see `on_walk_band`'s own doc for
            // why it is cached and not re-derived per keypress). On a parse
            // failure the previous value stands, the same posture the scope
            // above takes: a dead session is not a band change.
            self.on_walk_band = matches!(snap.spatial, hornvale_game_core::Spatial::Walk { .. });
        }
        self.update_discovery();
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

        if let Ok(snap) = hornvale_game_core::Snapshot::parse(&self.cached) {
            if matches!(snap.spatial, hornvale_game_core::Spatial::Chamber { .. }) {
                self.discovered.record(FeatureId::Settlement(vertex));
            }
            if snap.narration.prose.starts_with(DELVE_SUCCESS_PREFIX) {
                self.discovered.record(FeatureId::Cave(vertex));
            }
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

    /// The opening clause of `Driver::resolution_disclosure`'s message —
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
    /// the roster `plate::draw_feature_layer` projects from — and return it.
    ///
    /// The first cave the projection actually PLACES: a vertex above the
    /// polar clamp is on no chart at all, and skipping those is the clamp's
    /// own rule, not a search for a convenient answer.
    fn centre_on_a_cave(d: &mut Driver, plate_w: u16, plate_h: u16) -> Vertex {
        let site = d
            .caves
            .iter()
            .copied()
            .find(|&v| {
                let c = d.geo.coord(v);
                let (vw, vh) = plate::virtual_dims(d.window.depth);
                mercator::project(&d.frame, c.latitude, c.longitude, vw, vh).is_some()
            })
            .expect("seed 42 has at least one cave mouth inside the projection's clamp");
        let c = d.geo.coord(site);
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

    /// **The layer split's own headline, as an assertion**: a discovery
    /// changes what the player sees on the very next redraw, and does NOT
    /// re-render the terrain raster underneath it.
    ///
    /// Both halves are load-bearing and each rescues the other from
    /// vacuity. "Does not re-render" alone would pass on a cache that never
    /// invalidated at all — including one that had stopped showing
    /// discoveries entirely, which is a real regression this split could
    /// introduce. "Changes the plate" alone would pass on the old,
    /// discovery-keyed plate cache the split exists to remove.
    ///
    /// The site is a real cave mouth out of the driver's OWN roster (the
    /// one `draw_feature_layer` projects from), and the window is moved to
    /// it, because a site off screen would satisfy "does not re-render"
    /// trivially — the `assert_ne!` is what refuses that.
    #[test]
    fn a_discovery_redraws_the_features_without_re_rendering_the_terrain() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        let (w, h) = (104u16, 56u16);
        let (plate_w, plate_h) = Driver::world_plate_dims(w, h);

        let site = centre_on_a_cave(&mut d, plate_w, plate_h);

        let before = d
            .world_plate_for_redraw(w, h)
            .expect("the world view is on");
        let renders = d.tile_renders();
        assert!(renders > 0, "the first redraw must actually render");

        d.discovered_mut_for_test()
            .record(crate::discovery::FeatureId::Cave(site));
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
        d.enter_map();
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
        let mut seen = std::collections::BTreeSet::new();
        seen.insert(d.window().depth);
        for _ in 0..6 {
            d.apply(Action::Zoom(-1));
            assert!(seen.insert(d.window().depth), "a rung repeated");
        }
        assert_eq!(seen.len(), 7, "expected 7 rungs, saw {seen:?}");
        assert_eq!(*seen.iter().next().unwrap(), plate::GLOBE_RUNG);
        assert_eq!(*seen.iter().next_back().unwrap(), plate::BAND_B_RUNG);
    }

    #[test]
    fn zoom_plus_on_the_walk_band_alone_is_a_no_op() {
        let mut d = test_driver();
        d.enter_map();
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
        d.enter_map();
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
    /// stale or hardcoded value INSIDE `resolve_world_view`/`world_view_vertex`
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
                let expected_vertex = plate::terrain_at_tile(
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
                )
                .vertex;
                let (species, ph, morph) = &d.namer;
                // `resolve_chain_at`, not `resolve_at`: Task 4 made
                // `resolve_world_view` (`resolved`, below) return the FULL
                // containment chain, so the independent reconstruction here
                // must build the same chain to stay comparable — this test's
                // own H3 claim (window/cursor state agrees with the
                // resolver) is orthogonal to how many features get named.
                let expected = resolve_chain_at(
                    &d.index,
                    expected_vertex,
                    d.seed,
                    species,
                    ph,
                    morph,
                    &|id| d.discovered.contains(FeatureId::Extent(id)),
                );

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

            let resolved_vertex = d.world_view_vertex();
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

    #[test]
    fn recentre_keeps_the_same_geographic_point_under_the_cursor() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        d.apply(Action::CursorBy(9, 5));
        let before_name = d.resolve_world_view();

        d.apply(Action::Recentre);
        let after_name = d.resolve_world_view();

        assert_eq!(
            before_name, after_name,
            "recentring must roll the map UNDER a still cursor, not change what the cursor \
             names"
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
        d.enter_map();
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
                SkyChoice::Generated,
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
    /// The fix ([`Self::world_view_vertex`], now
    /// [`plate::terrain_at_tile`]) makes the resolver ask the SAME question
    /// the plate draws from, and take the representative of the PAINTED
    /// class — so agreement is no longer a measured ratio, it is a
    /// GUARANTEE the code's own structure enforces. The Quadrat's Task 3
    /// replaced the 49-point vote with mesh addressing and left that
    /// guarantee untouched ([`plate::TileTerrain`]'s own doc). This test still measures and prints the ratio (per the
    /// reviewer's own instruction: "if it is not ~100% by construction,
    /// something about the fix is wrong and I want to see the number") and
    /// then asserts it is exact, across every cell of the floor plate.
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

        let mut agree = 0u32;
        let mut total = 0u32;
        let mut memo = hornvale_kernel::RoomMeshMemo::default();
        for y in 0..plate_h {
            for x in 0..plate_w {
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
                )
                .vertex;
                let resolved_ocean = d.terrain.is_ocean(vertex);

                let drawn_ocean = grid.get(x, y).and_then(|c| c.glyph) == Some('~');
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
             floor plate at the coarsest zoom (the pre-fix figure once printed here, \
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
    /// `Driver::world_view_vertex`/`resolve_chain_at` production path,
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
                let vertex = d.world_view_vertex();
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
    /// is 182x181 tiles for seed 42's 40,962 vertices — 1.243 vertices per
    /// character — so the strip must say so, through the same `strip_text`
    /// instrument its silent sibling above reads.
    #[test]
    fn the_resolution_disclosure_speaks_at_a_rung_coarser_than_the_mesh() {
        let mut d = test_driver();
        enter_world_view(&mut d);

        let (w, h) = plate::virtual_dims(5);
        assert_eq!((w, h), (182, 181), "sanity: the rung-5 chart");
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
        centre_window_on_the_player(&mut d, 40, 20);
        let g = d.world_plate(40, 20);
        let text = g.to_plain_text();
        // `~` ocean, `.` land — plate.rs's own module doc names this
        // vocabulary; hardcoded here rather than widening that module's
        // private surface for two already-documented characters.
        assert!(
            text.contains('~'),
            "a cold-start plate must still show ocean: {text:?}"
        );
        assert!(
            text.contains('.'),
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

    /// H6b — co-location does not disclose. **The test this task exists
    /// for.** Seed 42's flagship starts at a vertex where `enter` succeeds
    /// immediately (established elsewhere by
    /// `strip_offset_stays_zero_when_the_text_fits_the_plate`) — i.e. a
    /// real settlement vertex — so this needs no hand-built world. The
    /// possession walks PAST it (several real `go` turns, in and out)
    /// without ever issuing `enter`, and the settlement must stay
    /// undiscovered — checked against the real `Discovered` state AND
    /// against the actually-rendered plate at EVERY zoom rung. A positive
    /// control on a fresh, identically-seeded driver proves the
    /// DISCOVERY-RECORDING mechanism can fire at all.
    ///
    /// **What the plate-rendered checks below do NOT prove, verified by
    /// final review (finding 8):** that this settlement's glyph is ever
    /// REACHABLE at any resolution this client's zoom ladder ships. It is
    /// not — see the comment above the (deliberately not asserted, ~180s)
    /// high-resolution positive control at the end of this test. So the
    /// plate-rendered loop below is a real, honest negative check (the
    /// glyph genuinely never appears), but it is VACUOUS as evidence for
    /// the discovery gate specifically: nothing this client can zoom to
    /// would draw this settlement's glyph whether or not it were
    /// discovered. It stays in the suite because it is still correct
    /// behaviour to pin (the glyph really must not appear), just not
    /// proof of what its own doc used to claim.
    #[test]
    fn h6b_co_location_does_not_disclose_a_settlement() {
        let mut d = test_driver();

        let start = d.session.position();
        let coord = start.coord();
        let vertex = d.nearest.nearest(&d.geo, coord.latitude, coord.longitude);
        assert!(
            d.settlements.contains(&vertex),
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

        // At EVERY zoom rung the plate can draw, the settlement's glyph
        // must never appear — never drawn, not drawn-then-hidden (§A3/A7).
        for depth in GLOBE_RUNG..=BAND_B_RUNG {
            d.window = Window {
                depth,
                origin_col: 0,
                origin_row: 0,
            };
            let g = d.world_plate(40, 20);
            assert!(
                !g.to_plain_text().contains(plate::SETTLEMENT_GLYPH),
                "co-location leaked at rung {depth}: the settlement's glyph appeared \
                 on an undiscovered map"
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

        // **Second control attempted, and downgraded to a documented
        // finding rather than a shipped assertion (final-review finding
        // 8).** The mechanism-only control above proves `Discovered::
        // record` fires; it says nothing about whether the plate can
        // ever DRAW the glyph, which finding 8 asked to check. It was
        // checked, empirically, by hand rather than in the shipped
        // suite, because the honest answer costs real wall-clock time to
        // reach:
        //
        //   - at the design plate (40x20), origin (0,0), EVERY rung
        //     GLOBE_RUNG..=BAND_B_RUNG: glyph absent.
        //   - at the design plate, window CENTRED on this settlement's
        //     own projected position, EVERY rung: still absent.
        //   - at 400x200 (the resolution `plate.rs`'s own
        //     `draw_with_gates_a_point_site_on_discovery` uses, and which
        //     is enough for a real CAVE vertex): still absent.
        //   - at 1200x600 -- finer, in the pre-Quadrat ladder's own terms,
        //     i.e. finer than any rung this client's zoom ladder ever
        //     reaches: present. ~180s to render, which is why this is a
        //     comment and not a test.
        //
        // So THIS settlement -- the seed-42 flagship's own starting
        // site -- is drawable in principle (the paint mechanism is not
        // broken; `point_site_at`'s gate genuinely fires once
        // `area_majority` ever lands its vote on the exact vertex) and
        // undrawable in practice, at every resolution this client's own
        // zoom ladder ever reaches. The negative checks earlier in this
        // test are therefore VACUOUS at every rung they cover, for this
        // specific settlement: the glyph's absence there is not evidence
        // the discovery gate is doing anything, because nothing this
        // client can zoom to would draw it whether or not it were
        // discovered. Recorded as `MAP-settlement-glyph-may-be-
        // unreachable-at-any-shipped-zoom`; not fixed here, since the fix
        // is the same one `MAP-vertical-axis-undersamples-the-mesh`
        // already defers (widening `virtual_h` independently of
        // `GLYPH_ASPECT`'s horizontal role).
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
            let _ = mapped.world_plate(40, 20);
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
    // raster's squares: the raster's tile index is `floor` of an ABSOLUTE
    // Mercator coordinate, a polar pair gives a RELATIVE offset, and which
    // side of a tile boundary a facet falls on is decided by the observer's
    // own sub-tile phase, which the wire does not carry. Swept over 200
    // sub-tile phases on the seed-42 band: best 0 of 31 marks misplaced,
    // worst 24, mean 11.5, only 2 of 200 phases exact.

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
        assert_eq!(checked, 31, "seed 42's flagship band is 31 facets");
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
        d.enter_map();
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
        // raster would satisfy the assertion above on its own.
        assert!(
            text.contains('~') || text.contains('.'),
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
        d.enter_map();
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
    /// origin would not survive a mesh change. The `(0, 0)` half is asserted
    /// too, so this cannot pass by the window happening never to have moved.
    #[test]
    fn band_b_centres_on_the_observer_not_the_arctic_corner() {
        let mut d = test_driver();
        // BEFORE any arrival: `start` leaves the corner alone, which is what
        // makes this test meaningful at all. Asserted before `resize`,
        // because `resize` is itself one of the three arrivals that centre.
        assert_eq!(
            (d.window.origin_row, d.window.origin_col),
            (0, 0),
            "sanity: this test is only meaningful if the untouched origin really is \
             the corner — if `start` gains a centring step, re-point it"
        );
        d.resize(120, 40);
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

    /// A COARSE rung gets no perception overlay, and that is a refusal
    /// rather than an omission: the packet's 31 facets are all finer than
    /// one coarse tile, so every one of them collapses onto the observer's
    /// single tile and an overlay drawn there would claim to place facets it
    /// had merged. Task 7 owns what a coarse rung shows.
    #[test]
    fn a_coarse_rung_draws_no_perception_overlay() {
        let mut d = test_driver();
        d.resize(120, 40);
        enter_world_view(&mut d);
        assert!(!d.at_walk_band_rung(), "sanity: a coarse rung");
        let plate = d.world_plate_for_redraw(120, 40).expect("a plate");
        let overlay = (0..plate.height())
            .flat_map(|y| (0..plate.width()).map(move |x| (x, y)))
            .filter(|&(x, y)| {
                plate
                    .get(x, y)
                    .is_some_and(|c| c.source == hornvale_game_core::Source::Chart)
            })
            .count();
        assert_eq!(overlay, 0, "a coarse rung must draw no perception glyphs");
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
        d.enter_map();
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
        d.enter_map();
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

    /// TASK 2'S CARRIED FINDING, closed by the anchor rather than by a
    /// second centring rule: leaving band B for the coarse rungs used to
    /// land at an arbitrary origin on an ~11,600-tile-wide chart. It now
    /// lands where the reader was looking, and at `enter_map` that is the
    /// observer.
    #[test]
    fn zooming_out_of_band_b_lands_where_the_reader_was_looking() {
        let mut d = test_driver();
        d.resize(210, 56);
        d.enter_map();
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
    /// catalog — the real refresh path (`scope.update` from a parsed
    /// snapshot) is exercised by the integration suite; these tests need a
    /// KNOWN vocabulary.
    pub(crate) fn seeded_driver() -> Driver {
        let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
        d.scope.update(&fixture_narration());
        d
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
