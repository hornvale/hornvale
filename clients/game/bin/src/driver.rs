//! The driver: the one place in `hornvale-game` allowed to know `Session`,
//! `Agent`, or `WorldContext` exist.
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
//! no `Agent`/`Knowledge`/`WorldContext` value ever crosses out.
//!
//! **Resolution genuinely tracks the cursor.** Spec §3.1 assigns the cursor
//! query to `bin`; §9 defers "the chamber / delve resolvers" (session-level
//! cells/marks/agents at those bands) to a future campaign — so those bands
//! honestly answer [`NOTHING_HERE_YET`]. The walk band resolves against its
//! own chart ([`Driver::resolve_walk_band`]); The Portolan part II, Task 3b
//! adds a second resolver for the whole-world Mercator plate
//! ([`Driver::resolve_world_view`]), active in place of the walk band's own
//! whenever `self.world_view` is on (`spread::compose`'s own doc: the world
//! view is a lens over whichever band the character occupies, never a new
//! band). Either way, the *pointed-at* cell is what gets resolved, not the
//! observer's own (an earlier revision of this module resolved the
//! observer's cell unconditionally — a fix round caught that this made the
//! strip position-invariant while the cursor visibly moved, exactly the "a
//! wrong name is indistinguishable from a right one" failure the design
//! spec warns against).
//!
//! The chain from a screen position to a [`hornvale_kernel::CellId`]:
//! [`hornvale_game_core::chart::cell_at`] (new, shared with `draw` via a
//! common `boxes_of` helper — never a second copy of the projection) finds
//! which wire [`hornvale_game_core::ChartCell`] occupies the cursor's box
//! and its index into `chart.cells`. `chart.cells` is a field-for-field,
//! order-preserving mirror of the real `hornvale_scene::SurroundsScene`
//! (`Session::snapshot` embeds that scene directly — nothing reorders or
//! filters it in transit), so re-deriving the SAME scene with
//! `self.session.purview(0)` (the identical call `Session::snapshot` itself
//! makes for the walk band) and indexing into its `cells` at that same
//! position names the identical real cell — no float matching, no new
//! trigonometry. That real cell's `room: u64` unpacks
//! ([`hornvale_kernel::RoomId::unpack`]) to a real
//! [`hornvale_kernel::RoomAddr`], whose [`hornvale_kernel::RoomAddr::coord`]
//! feeds the same `NearestCellIndex` lookup already used for the observer.
//!
//! No new geometry was written for this: `RoomId::unpack`, `RoomAddr::coord`
//! and `NearestCellIndex::nearest` all already existed: `kernel::room` has
//! no inverse of `bearing_to`/`distance_rad_to` (a destination from an
//! origin, a bearing and a distance), so this deliberately does not need
//! one — the index-into-a-freshly-rebuilt-scene route reaches the same
//! room a bearing-inversion would have, using data both `chart::cell_at`
//! and `Session::purview` already carry.

use crate::history::History;
use crate::input::Action;
use crate::line::Line;
use crate::mercator::{self, Frame};
use crate::plate::{self, Window};
use hornvale_astronomy::SkyPins;
use hornvale_game_core::{Cursor, Focus};
use hornvale_kernel::{NearestCellIndex, RoomId, Seed, World};
use hornvale_language::{MorphOptions, Phonology};
use hornvale_terrain::GeneratedTerrain;
use hornvale_terrain::TerrainPins;
use hornvale_terrain::landscape::CellFeatureIndex;
use hornvale_vessel::{
    PossessOpts, PossessTarget, Session, Turn, VesselError, WorldContext, snapshot_json,
};
use hornvale_worldgen::{
    BuildError, SettlementPins, SkyChoice, WorldComponents, build_world, gazetteer_features,
    language_of_in, morph_options, resolve_at, terrain_of,
};

/// What the strip says, with the map focused, over a band this campaign has
/// no resolver for (walk and chamber both draw a real plate; only the
/// terrain-feature index answers a query — spec §3.1/§9). Faking a
/// resolution would be worse than refusing: a wrong name is
/// indistinguishable from a right one, and this string never is one.
const NOTHING_HERE_YET: &str = "nothing here yet";

/// What the strip says when the resolver ran and genuinely found no
/// individuated feature at the observer's cell — real terrain below every
/// class's individuation floor, not "nothing there" (seed 42 measured 377
/// of 40,962 cells like this; spec §3.4).
const UNNAMED_TERRAIN: &str = "unnamed terrain";

/// The plate's content height Driver assumes before the first real
/// terminal size is known — [`hornvale_game_core::spread::content_height`]
/// at the 80x24 floor. **Not a standing assumption**: `main`'s `play` loop
/// calls [`Driver::resize`] with the live terminal height at startup and on
/// every resize event, which overwrites `self.plate_height` with the real
/// number — this constant only covers the brief window before that first
/// call (fix round 2: an earlier revision used this fixed floor value
/// unconditionally, which named the wrong cell on any terminal taller than
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
    /// The world's landscape features, indexed by cell, built once here at
    /// `start` and never rebuilt — the feature stack is immutable for the
    /// world's lifetime (`CellFeatureIndex`'s own doc).
    index: CellFeatureIndex,
    /// Nearest-cell lookup over the same `Geosphere` `index` was built from,
    /// built once alongside it — turns the possessed agent's fine-grained
    /// [`hornvale_kernel::RoomAddr`] position into the coarse `CellId` the
    /// terrain feature index answers for.
    nearest: NearestCellIndex,
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
    /// The world plate's window: which zoom level and which VIRTUAL-chart
    /// cell the window's own origin sits at (`plate::virtual_dims`'s own
    /// doc distinguishes the virtual chart from the drawn plate). Reset to
    /// `Window { zoom: 0, origin_col: 0, origin_row: 0 }` on entering the
    /// world view (`Driver::apply_zoom`) — the whole planet, no scroll
    /// needed at the coarsest rung — and moved by zoom/scroll from there.
    window: Window,
    /// Whether the whole-world Mercator view is active. **Fix round 1
    /// (Task 3a's own review):** `Focus::Map` alone used to gate this in
    /// `main.rs`, which silently retired the walk-band cursor/strip feature
    /// shipped across `9f69e4e2a`/`81d940d9c`/`64c80be36` and chronicled in
    /// `book/src/chronicle/the-stride.md`/`the-stylus.md` — that feature
    /// ALSO lives behind `Focus::Map` (entering the map to point at, and
    /// read the name of, the walk band's own local terrain), so reusing the
    /// same focus value for the world view meant a 210x56 redraw drew a
    /// 104-column Mercator while the cursor stayed clamped to the OLD
    /// 40-column plate and the strip kept resolving the walk band's own
    /// (now invisible) chart — a picture and a cursor/strip that no longer
    /// agreed at all. This field makes the world view its own explicit
    /// state, defaulting OFF ([`Driver::start`] never sets it).
    ///
    /// **Task 3b's own gesture (ruling T3-f): zooming OUT past the walk
    /// band turns this on, at the coarsest rung; zooming IN past the
    /// world view's finest rung turns it back off.** See
    /// [`Driver::apply_zoom`]. `Focus::Map` alone still reproduces the
    /// pre-Task-3a behaviour byte-identically — this field is untouched by
    /// focus changes on their own, only by `Action::Zoom`; see
    /// [`Driver::world_plate_for_redraw`].
    world_view: bool,
    /// The world's seed, needed to draw a feature's name.
    seed: Seed,
    /// The plate's REAL content height, in grid rows — synced from the live
    /// terminal by [`Driver::resize`] (fix round 2: an earlier revision
    /// used a floor-sized constant unconditionally here, which named the
    /// wrong cell — see the module doc). Starts at
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
    // terrain-feature index — never per-turn (see `CellFeatureIndex`'s doc).
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
        // per-turn — see the module doc and `CellFeatureIndex`'s own).
        // `terrain_of` re-derives `GeneratedTerrain` deterministically from
        // the world's committed seed and pin facts — the same
        // reconstruction idiom `sky_of`/the CLI's `map` command use, not a
        // second, drifting genesis.
        let terrain = terrain_of(world_ref).map_err(DriverError::Genesis)?;
        let geo = terrain.geosphere().clone();
        let features = gazetteer_features(world_ref.seed, &geo, &terrain);
        let index = CellFeatureIndex::build(&features);
        let nearest = NearestCellIndex::new(&geo);

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
        let window = Window {
            zoom: 0,
            origin_col: 0,
            origin_row: 0,
        };

        // The possessed agent's species, phonology and morphology — needed
        // to draw a feature's name (`resolve_at`), resolved once since the
        // agent's species is fixed for the session.
        let agent = session.agent();
        let species = agent.species.clone();
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
            index,
            nearest,
            geo,
            terrain,
            frame,
            window,
            world_view: false,
            seed: world_ref.seed,
            plate_height: FLOOR_PLATE_CONTENT_HEIGHT,
            term_w: hornvale_game_core::MIN_WIDTH,
            term_h: hornvale_game_core::MIN_HEIGHT,
            namer: (species, ph, morph),
            line: Line::new(),
            history: History::new(),
            echo: None,
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
    /// unchanged by a resize, but which real cell that screen position
    /// names can change (the plate's centre moves), so the displayed text
    /// must not go on describing whatever the old size resolved.
    pub fn resize(&mut self, w: u16, h: u16) {
        self.term_w = w;
        self.term_h = h;
        self.plate_height = hornvale_game_core::spread::content_height(h);
        if self.world_view {
            self.reclamp_window();
        }
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
    /// plate resamples `SUBSAMPLES_PER_AXIS` squared points per cell
    /// (`plate.rs`'s own doc), so a caller reaching this directly still owns
    /// not paying that cost needlessly.
    pub fn world_plate(&self, w: u16, h: u16) -> hornvale_game_core::Grid {
        let plate_width = hornvale_game_core::spread::world_plate_width(w, h);
        let plate_height = plate_width / hornvale_game_core::spread::GLYPH_ASPECT;
        plate::draw(
            &self.terrain,
            &self.geo,
            &self.nearest,
            &self.frame,
            &self.window,
            plate_width,
            plate_height,
        )
    }

    /// The world plate to hand [`hornvale_game_core::render_with`] for a
    /// `w`-by-`h` redraw, gated on the world view actually being active —
    /// `Some(`[`Driver::world_plate`]`(w, h))` when [`Focus::Map`] is
    /// focused AND `self.world_view` is on, `None` otherwise.
    ///
    /// **Fix round 1: this is the ONE place that decision is made.** Before
    /// this method existed, `main.rs`'s `redraw` computed
    /// `Some(driver.world_plate(w, h))` whenever `focus() == Focus::Map`,
    /// with no `world_view` gate at all — `Focus::Map` already meant
    /// something else (the walk-band cursor/strip feature this module's
    /// `world_view` field doc names), so that reproduced the exact
    /// class of bug `content_height`'s own doc warns about elsewhere in
    /// this crate: two independent computations of "is the world view
    /// showing" would have been one too many, this time on the ACTIVATION
    /// question rather than a dimension. `main.rs` calls only this method
    /// now, never re-deriving the condition itself, and every test that
    /// wants to know what a redraw would draw calls it too rather than
    /// reimplementing the check a third time.
    ///
    /// `self.world_view` defaults to `false` and, since Task 3b, [`Self::
    /// apply_zoom`] is the gesture that sets it `true` — zooming out past
    /// the walk band (ruling T3-f, `-` on [`Focus::Map`]).
    pub fn world_plate_for_redraw(&self, w: u16, h: u16) -> Option<hornvale_game_core::Grid> {
        (self.world_view && self.focus == Focus::Map).then(|| self.world_plate(w, h))
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
    /// **`Zoom` is now implemented (Task 3b): one continuous ladder, per
    /// ruling T3-f.** With the world view off, zooming out (`-`) turns it
    /// on at the coarsest rung; with the world view on at its finest rung,
    /// zooming in (`+`/`=`) turns it back off; between those, the keys move
    /// the world map's own zoom, clamped to `0..=`[`plate::MAX_ZOOM`]. See
    /// [`Self::apply_zoom`].
    ///
    /// **`Recentre` (`.`) rolls the projection to the cursor (spec §3.2),
    /// only on command — never a side effect of cursor movement.** A no-op
    /// unless the world view is active. See [`Self::recentre`].
    pub fn apply(&mut self, action: Action) -> bool {
        match action {
            Action::ToggleFocus => {
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
                false
            }
            Action::DeleteBack => {
                self.line.backspace();
                false
            }
            Action::CaretBy(dx) => {
                match dx.cmp(&0) {
                    std::cmp::Ordering::Less => self.line.caret_left(),
                    std::cmp::Ordering::Greater => self.line.caret_right(),
                    std::cmp::Ordering::Equal => {}
                }
                false
            }
            Action::HistoryPrev => {
                if let Some(text) = self.history.prev() {
                    self.line.set(text.to_string());
                }
                false
            }
            Action::HistoryNext => {
                match self.history.next() {
                    Some(text) => self.line.set(text.to_string()),
                    None => self.line.set(String::new()),
                }
                false
            }
            Action::Submit => {
                if self.line.is_empty() {
                    return false;
                }
                let taken = self.line.take();
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

    /// Enter the map focus and resolve its strip. Also called by `apply`'s
    /// `Submit` arm when a submitted line's first token is exactly `map`
    /// (see there).
    fn enter_map(&mut self) {
        self.focus = Focus::Map;
        self.refresh_strip();
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
        if self.world_view {
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
        let (plate_w, plate_h) = self.active_plate_dims();
        let (virtual_w, virtual_h) = plate::virtual_dims(&self.window, plate_w);
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

        if self.world_view {
            let (virtual_w, virtual_h) = plate::virtual_dims(&self.window, plate_w);
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

    /// Zoom the map view — ruling T3-f (2026-08-23, `task-3b-brief.md`):
    /// one continuous ladder out of already-routed keys, rather than a new
    /// binding or a fourth focus state.
    ///
    /// - `delta < 0` (zoom OUT): with the world view off, turn it ON at
    ///   its coarsest rung (`Window { zoom: 0, .. }` — the whole planet,
    ///   nothing further out); with it on and already at zoom `0`, do
    ///   nothing (there IS nothing further out); otherwise zoom out one
    ///   step.
    /// - `delta > 0` (zoom IN): with the world view on and already at
    ///   [`plate::MAX_ZOOM`] (the finest rung — one character per terrain
    ///   cell, decision 0123), turn it OFF, returning to the walk-band
    ///   chart; with it on and below the ceiling, zoom in one step;
    ///   with it off, do nothing (the walk band has no zoom of its own).
    ///
    /// Either transition re-clamps the cursor into whichever plate is now
    /// active ([`Self::move_cursor`]`(0, 0)`) and re-resolves the strip —
    /// both the plate's own size and what the cursor points at can change
    /// on every one of these transitions.
    fn apply_zoom(&mut self, delta: i8) {
        use std::cmp::Ordering;
        match delta.cmp(&0) {
            Ordering::Less => {
                if !self.world_view {
                    self.world_view = true;
                    self.window = Window {
                        zoom: 0,
                        origin_col: 0,
                        origin_row: 0,
                    };
                } else if self.window.zoom > 0 {
                    self.window.zoom -= 1;
                    self.reclamp_window();
                }
            }
            Ordering::Greater => {
                if self.world_view && self.window.zoom >= plate::MAX_ZOOM {
                    self.world_view = false;
                    self.window = Window {
                        zoom: 0,
                        origin_col: 0,
                        origin_row: 0,
                    };
                } else if self.world_view {
                    self.window.zoom += 1;
                    self.reclamp_window();
                }
            }
            Ordering::Equal => {}
        }
        self.move_cursor(0, 0);
        self.refresh_strip();
    }

    /// Roll the projection so the point currently under the cursor sits on
    /// the projection's own central line — spec §3.2's explicit re-centre
    /// command, bound to `.` in [`Focus::Map`] (ruling T3-a). A no-op
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
        if !self.world_view {
            return;
        }
        let (plate_w, plate_h) = self.active_plate_dims();
        let (virtual_w, virtual_h) = plate::virtual_dims(&self.window, plate_w);
        let plate_row = self.window.origin_row + u32::from(self.cursor.y);
        let plate_col = self.window.origin_col + u32::from(self.cursor.x);
        let (lat, lon) =
            mercator::unproject(&self.frame, plate_row, plate_col, virtual_w, virtual_h);

        self.frame = mercator::centre_on(lat, lon);

        if let Some((new_row, new_col)) =
            mercator::project(&self.frame, lat, lon, virtual_w, virtual_h)
        {
            if virtual_w > 0 {
                self.window.origin_col = (i64::from(new_col) - i64::from(self.cursor.x))
                    .rem_euclid(i64::from(virtual_w))
                    as u32;
            }
            let max_origin_row = i64::from(virtual_h)
                .saturating_sub(i64::from(plate_h))
                .max(0);
            self.window.origin_row =
                (i64::from(new_row) - i64::from(self.cursor.y)).clamp(0, max_origin_row) as u32;
        }
        self.refresh_strip();
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
    /// cell the CURSOR points at, not the observer's own — every other band
    /// answers [`NOTHING_HERE_YET`] honestly.
    fn refresh_strip(&mut self) {
        self.strip = Some(self.resolve());
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
    /// walk-band scene, in which case the cell the cursor points at
    /// ([`Self::resolve_walk_band`]; see the module doc for the chain from
    /// a screen position to a `CellId`) is resolved against the
    /// terrain-feature index — [`UNNAMED_TERRAIN`] if that chain comes up
    /// empty at any step.
    fn resolve(&self) -> String {
        if self.world_view {
            return self
                .resolve_world_view()
                .unwrap_or_else(|| UNNAMED_TERRAIN.to_string());
        }
        let Ok(snap) = hornvale_game_core::Snapshot::parse(&self.cached) else {
            return NOTHING_HERE_YET.to_string();
        };
        let hornvale_game_core::Spatial::Walk { chart } = snap.spatial else {
            return NOTHING_HERE_YET.to_string();
        };
        let base = self
            .resolve_walk_band(&chart)
            .unwrap_or_else(|| UNNAMED_TERRAIN.to_string());
        caption(base, chart.sight.as_ref())
    }

    /// The world view's own resolved [`hornvale_kernel::CellId`] at the
    /// cursor's current screen position — `plate::area_majority`, the
    /// SAME 49-point vote [`plate::draw_with`] itself paints from (one
    /// source of truth; see that function's own doc), asked for the
    /// MAJORITY-class sample nearest the screen cell's own true centre.
    ///
    /// **Fix round 1, Finding 2 (the reviewer's own framing, which
    /// improved on this task's first pass): the earlier single-centre-
    /// point resolver was not a contract violation — it named a genuinely
    /// resolved feature — but it let the strip contradict the picture.**
    /// F5 measured 52.5% agreement between that resolver and the drawn
    /// glyph at the coarsest zoom (see the task report): a player could
    /// point at a character drawn `~` and have the strip name a real
    /// feature on a LAND cell nearest that character's exact centre. This
    /// method closes that: it can never return a cell whose class
    /// disagrees with what [`plate::draw_with`] would paint at the same
    /// screen position, because it asks the identical vote for the
    /// identical answer, and picks a REPRESENTATIVE of the winning class
    /// rather than an unconstrained nearest point. See `plate::
    /// area_majority`'s own doc for why its `(3, 3)` sub-sample is the
    /// same point the earlier single-point query asked for, so this is
    /// strictly more constrained, never coarser.
    fn world_view_cell(&self) -> hornvale_kernel::CellId {
        let (plate_w, _) = self.active_plate_dims();
        let (virtual_w, virtual_h) = plate::virtual_dims(&self.window, plate_w);
        let (_ocean, cell) = plate::area_majority(
            &self.terrain,
            &self.geo,
            &self.nearest,
            &self.frame,
            &self.window,
            virtual_w,
            virtual_h,
            u32::from(self.cursor.y),
            u32::from(self.cursor.x),
        );
        cell
    }

    /// The world view's own resolution chain: [`Self::world_view_cell`] to
    /// its most specific feature's name.
    fn resolve_world_view(&self) -> Option<String> {
        let cell_id = self.world_view_cell();
        let (species, ph, morph) = &self.namer;
        resolve_at(&self.index, cell_id, self.seed, species, ph, morph)
    }

    /// The walk band's own resolution chain, cursor position to name.
    /// `None` at any step means "genuinely nothing individuated there" (no
    /// chart cell occupies the cursor's box, the real scene could not be
    /// re-derived, the room address does not unpack, or the terrain index
    /// has no feature at the resolved cell) — the caller maps `None` to
    /// [`UNNAMED_TERRAIN`], never to [`NOTHING_HERE_YET`] (that string is
    /// reserved for a band with no resolver at all, which this is not).
    fn resolve_walk_band(&self, chart: &hornvale_game_core::Chart) -> Option<String> {
        let (index, _cell) = hornvale_game_core::chart::cell_at(
            chart,
            (0, 0),
            hornvale_game_core::spread::PLATE_WIDTH,
            self.plate_height,
            self.cursor.x,
            self.cursor.y,
        )?;
        // The real scene, re-derived with the SAME call `Session::snapshot`
        // itself makes for the walk band (`purview(0)`, `windows/vessel/src/
        // session.rs`'s own comment on that call site) — see the module doc
        // for why `chart.cells[index]` and `scene.cells[index]` name the
        // identical cell.
        let scene = self.session.purview(0).ok()?;
        let real_cell = scene.cells.get(index)?;
        let room = RoomId(real_cell.room).unpack().ok()?;
        let coord = room.coord();
        let cell_id = self
            .nearest
            .nearest(&self.geo, coord.latitude, coord.longitude);
        let (species, ph, morph) = &self.namer;
        resolve_at(&self.index, cell_id, self.seed, species, ph, morph)
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

    /// Enter the map and zoom out once — the T3-f gesture that turns the
    /// world view on, at its coarsest rung.
    fn enter_world_view(d: &mut Driver) {
        d.enter_map();
        d.apply(Action::Zoom(-1));
        assert!(
            d.world_view,
            "zooming out from the walk band must enter the world view"
        );
    }

    // -- Step 1: the zoom ladder, per ruling T3-f -----------------------

    #[test]
    fn zoom_out_from_the_walk_band_enters_the_world_view_at_the_coarsest_rung() {
        let mut d = test_driver();
        d.enter_map();
        assert!(!d.world_view);
        d.apply(Action::Zoom(-1));
        assert!(d.world_view);
        assert_eq!(d.window.zoom, 0, "the coarsest rung is zoom 0");
        assert_eq!(d.window.origin_col, 0);
        assert_eq!(d.window.origin_row, 0);
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
        assert!(d.world_view, "must still be in the world view");
    }

    #[test]
    fn zoom_in_climbs_one_step_at_a_time() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        d.apply(Action::Zoom(1));
        assert_eq!(d.window.zoom, 1);
        assert!(d.world_view);
    }

    #[test]
    fn zoom_in_stops_at_max_zoom_then_the_next_press_leaves_the_world_view() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        for _ in 0..plate::MAX_ZOOM {
            d.apply(Action::Zoom(1));
        }
        assert!(d.world_view);
        assert_eq!(
            d.window.zoom,
            plate::MAX_ZOOM,
            "maximum zoom is one character per terrain cell (decision 0123)"
        );
        d.apply(Action::Zoom(1));
        assert!(
            !d.world_view,
            "one more zoom-in at the finest rung must leave the world view (ruling T3-f)"
        );
    }

    /// Sixty-four presses is far more than [`plate::MAX_ZOOM`] steps, so
    /// this must walk all the way up the ladder AND back off the top —
    /// the ladder's ceiling must be a real stop, not merely a slow climb.
    #[test]
    fn many_zoom_ins_walk_off_the_top_of_the_ladder_and_back_to_the_walk_band() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        for _ in 0..64 {
            d.apply(Action::Zoom(1));
        }
        assert!(!d.world_view);
    }

    #[test]
    fn zoom_plus_on_the_walk_band_alone_is_a_no_op() {
        let mut d = test_driver();
        d.enter_map();
        assert!(!d.world_view);
        let before_window = d.window;
        d.apply(Action::Zoom(1));
        assert!(!d.world_view, "the walk band has no zoom of its own");
        assert_eq!(d.window, before_window);
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

    #[test]
    fn the_cursor_clamps_back_to_the_walk_bands_width_once_the_world_view_is_off() {
        let mut d = test_driver();
        d.resize(210, 56);
        enter_world_view(&mut d);
        d.apply(Action::CursorBy(i16::MAX, 0));
        for _ in 0..plate::MAX_ZOOM + 1 {
            d.apply(Action::Zoom(1)); // walk back off the top of the ladder
        }
        assert!(!d.world_view);
        d.apply(Action::CursorBy(i16::MAX, 0));
        assert_eq!(
            d.cursor.x,
            hornvale_game_core::spread::PLATE_WIDTH - 1,
            "back on the walk band, the clamp must be the fixed PLATE_WIDTH again"
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

    /// H3, the hypothesis at real risk: after scrolling, the cell under the
    /// cursor must be the cell the window/frame state actually names.
    ///
    /// **Fix round 1, Finding 1 (reviewer): this reconstruction is NOT an
    /// independent derivation — it calls `active_plate_dims`,
    /// `plate::virtual_dims` and `plate::area_majority` in the same order
    /// over the same fields `resolve_world_view` itself does, so it is a
    /// hand-mirrored copy of the implementation, not an alternate one.**
    /// Said plainly rather than smoothed over: this test cannot catch a
    /// bug shared by both copies (one living in `virtual_dims`, or in
    /// `move_cursor`'s scroll math). What it DOES catch, and the reason it
    /// is kept rather than deleted: a future edit that reintroduces a
    /// stale or hardcoded value INSIDE `resolve_world_view`/`world_view_cell`
    /// alone — a revert-to-no-op mutation of the whole zoom/scroll feature
    /// failed 12 of 15 tests in this module (task report, "behavioural
    /// REDs"), proving this is not vacuous even though it is not
    /// independent. `the_resolved_cell_matches_the_actually_drawn_glyph_
    /// at_fine_zoom` below is the genuine ground-truth check the reviewer
    /// asked for: it reads [`plate::draw_with`]'s ACTUAL rendered grid,
    /// never re-derives the arithmetic.
    ///
    /// Sweeps three zooms and five offsets, including one (`(39, 19)`)
    /// larger than the floor plate itself, to force real scrolling — a
    /// test pinned at one offset and one zoom cannot see drift between the
    /// window's own offset and the resolver's.
    #[test]
    fn the_cell_under_the_cursor_matches_the_window_at_every_zoom_and_offset() {
        let mut d = test_driver();
        enter_world_view(&mut d);

        for target_zoom in [0u8, 1, 3] {
            while d.window.zoom < target_zoom {
                d.apply(Action::Zoom(1));
            }
            assert_eq!(d.window.zoom, target_zoom);

            for &(dx, dy) in &[(0i16, 0i16), (7, 0), (0, 5), (39, 19), (-7, 3)] {
                d.apply(Action::CursorBy(dx, dy));

                let (plate_w, _) = d.active_plate_dims();
                let (virtual_w, virtual_h) = plate::virtual_dims(d.window(), plate_w);
                let (_ocean, expected_cell) = plate::area_majority(
                    &d.terrain,
                    &d.geo,
                    &d.nearest,
                    d.frame(),
                    d.window(),
                    virtual_w,
                    virtual_h,
                    u32::from(d.cursor.y),
                    u32::from(d.cursor.x),
                );
                let (species, ph, morph) = &d.namer;
                let expected = resolve_at(&d.index, expected_cell, d.seed, species, ph, morph);

                let resolved = d.resolve_world_view();
                assert_eq!(
                    resolved, expected,
                    "zoom {target_zoom}, offset ({dx}, {dy}), cursor {:?}, window {:?}: \
                     the resolver drifted from the window/cursor state",
                    d.cursor, d.window
                );
            }
        }
    }

    /// The genuine ground-truth check Finding 1 asked for: resolve at the
    /// cursor, then read [`plate::draw_with`]'s ACTUAL rendered grid (via
    /// `Driver::world_plate`, the real production drawing path) at that
    /// same screen position, and assert the resolved cell's ocean/land
    /// class agrees with the drawn glyph. Two genuinely different code
    /// paths — one produces pixels, the other a `CellId` — cross-checked
    /// against each other's real OUTPUT, not a shared re-derivation of the
    /// same arithmetic.
    ///
    /// Run at [`plate::MAX_ZOOM`] (the finest rung), where the 49-vote
    /// majority and the true-centre sample are expected to coincide (one
    /// terrain cell per character — `plate::SUBSAMPLES_PER_AXIS`'s own
    /// doc), across several offsets: this is the region where the check is
    /// unambiguous, so a failure here means the draw/resolve SEAM itself
    /// has drifted, not merely that a coarse character's footprint
    /// straddled a coastline (the F5 finding, which this fine-zoom check
    /// is deliberately not measuring — see `f5_drawn_majority_vs_resolved_
    /// single_point_at_the_coarsest_zoom` for the coarse-zoom, whole-plate
    /// version of this same comparison, which the Finding 2 fix below
    /// makes an EXACT agreement rather than a measured ratio).
    #[test]
    fn the_resolved_cell_matches_the_actually_drawn_glyph_at_fine_zoom() {
        let mut d = test_driver();
        enter_world_view(&mut d);
        for _ in 0..plate::MAX_ZOOM {
            d.apply(Action::Zoom(1));
        }
        assert_eq!(d.window.zoom, plate::MAX_ZOOM);

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

            let resolved_cell = d.world_view_cell();
            let resolved_ocean = d.terrain.is_ocean(resolved_cell);

            assert_eq!(
                resolved_ocean, drawn_ocean,
                "cursor {:?} at zoom {}: the resolver named a cell whose class \
                 contradicts the actually drawn glyph",
                d.cursor, d.window.zoom
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

    #[test]
    fn recentre_is_a_no_op_off_the_world_view() {
        let mut d = test_driver();
        d.enter_map();
        assert!(!d.world_view);
        let before = *d.frame();
        d.apply(Action::Recentre);
        assert_eq!(
            *d.frame(),
            before,
            "the walk-band chart has no central line to roll"
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

    // -- F5, fix round 1: the resolved cell must never contradict the
    //    drawn glyph (Finding 2) -----------------------------------------

    /// F5, RE-MEASURED after the Finding 2 fix (task report fix round 1).
    /// Before the fix, this test measured 52.5% agreement (420/800) between
    /// the drawn 49-vote-majority glyph and a single-centre-point resolver
    /// at the coarsest zoom — a real, substantial disagreement, reported
    /// plainly rather than smoothed over (the task report's own words).
    ///
    /// The fix ([`Self::world_view_cell`], `plate::area_majority`) makes
    /// the resolver ask the SAME 49-point vote the plate draws from, and
    /// pick only a sample of the WINNING class — so agreement is no longer
    /// a measured ratio, it is a GUARANTEE the code's own structure
    /// enforces. This test still measures and prints the ratio (per the
    /// reviewer's own instruction: "if it is not ~100% by construction,
    /// something about the fix is wrong and I want to see the number") and
    /// then asserts it is exact, across every cell of the floor plate.
    #[test]
    fn f5_the_resolved_cell_always_matches_the_drawn_glyph_after_the_fix() {
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
        let (virtual_w, virtual_h) = plate::virtual_dims(&d.window, plate_w);

        let mut agree = 0u32;
        let mut total = 0u32;
        for y in 0..plate_h {
            for x in 0..plate_w {
                let (_ocean, cell) = plate::area_majority(
                    &d.terrain,
                    &d.geo,
                    &d.nearest,
                    &d.frame,
                    &d.window,
                    virtual_w,
                    virtual_h,
                    u32::from(y),
                    u32::from(x),
                );
                let resolved_ocean = d.terrain.is_ocean(cell);

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
            "F5 (fix round 1): the resolved cell's class agrees with the drawn 49-vote \
             majority glyph on {agree}/{total} = {ratio:.4} of the {plate_w}x{plate_h} \
             floor plate at the coarsest zoom (was 420/800 = 0.5250 before the fix)"
        );
        assert_eq!(
            agree, total,
            "Finding 2's fix guarantees this by construction: `area_majority` only ever \
             returns a sample of the WINNING class, so the resolved cell can never \
             disagree with the glyph drawn from that same vote — a non-1.0 ratio here \
             means the fix itself is broken"
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
