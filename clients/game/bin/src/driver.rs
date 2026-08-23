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
//! query to `bin`; §9 defers "the walk / chamber / delve resolvers"
//! (session-level cells/marks/agents) to a future campaign — so only the
//! walk band resolves here, and every other band honestly answers
//! [`NOTHING_HERE_YET`]. At the walk band, the *pointed-at* cell is what
//! gets resolved, not the observer's own (an earlier revision of this
//! module resolved the observer's cell unconditionally — a fix round
//! caught that this made the strip position-invariant while the cursor
//! visibly moved, exactly the "a wrong name is indistinguishable from a
//! right one" failure the design spec warns against).
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
use hornvale_astronomy::SkyPins;
use hornvale_game_core::{Cursor, Focus};
use hornvale_kernel::{NearestCellIndex, RoomId, Seed, World};
use hornvale_language::{MorphOptions, Phonology};
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
            seed: world_ref.seed,
            plate_height: FLOOR_PLATE_CONTENT_HEIGHT,
            namer: (species, ph, morph),
            line: Line::new(),
            history: History::new(),
            echo: None,
        };
        driver.refresh();
        Ok(driver)
    }

    /// Sync the plate's REAL content height from the live terminal's row
    /// count `h` (`main`'s `play` loop calls this at startup and on every
    /// resize event, before the first/next draw). Uses
    /// [`hornvale_game_core::spread::content_height`] — the SAME
    /// computation `compose` itself applies to derive the plate it actually
    /// draws into, not a second copy of it (fix round 2's own lesson,
    /// applied to itself: two independent "what is the plate's content
    /// height" computations is exactly the shape that produced the bug this
    /// method fixes).
    ///
    /// Re-clamps the cursor into the new bounds: a terminal shrinking after
    /// the cursor moved into rows a taller plate offered must not leave it
    /// parked outside the plate that is about to be drawn. With the map
    /// focused, also re-resolves the strip: the cursor's SCREEN position is
    /// unchanged by a resize, but which real cell that screen position
    /// names can change (the plate's centre moves), so the displayed text
    /// must not go on describing whatever the old height resolved.
    pub fn resize(&mut self, h: u16) {
        self.plate_height = hornvale_game_core::spread::content_height(h);
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
    /// `CursorBy`/`ToggleFocus` are unchanged from part I. `Zoom` is
    /// accepted and ignored — zoom itself belongs to The Portolan part II,
    /// a paused follow-on campaign; this arm is where it will be
    /// implemented. What matters now is only that a zoom key never falls
    /// through to the buffer and types itself.
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
            Action::Zoom(_) => false,
            // Routing exists first (spec §4.1); what completion DOES is a
            // later task — for now the key press is accepted and inert.
            Action::Complete => false,
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

    /// Move the cursor by `(dx, dy)` grid cells, clamped to the plate's
    /// REAL bounds (`self.plate_height`, kept live by [`Driver::resize`] —
    /// fix round 2: an earlier revision clamped to the 80x24 floor's fixed
    /// height regardless of the terminal's actual size, which could not
    /// reach rows a taller plate actually draws).
    fn move_cursor(&mut self, dx: i16, dy: i16) {
        let max_x = i32::from(hornvale_game_core::spread::PLATE_WIDTH) - 1;
        let max_y = i32::from(self.plate_height).saturating_sub(1).max(0);
        let nx = (i32::from(self.cursor.x) + i32::from(dx)).clamp(0, max_x);
        let ny = (i32::from(self.cursor.y) + i32::from(dy)).clamp(0, max_y);
        self.cursor.x = nx as u16;
        self.cursor.y = ny as u16;
    }

    /// Recompute `self.strip` for the current band and cursor position. See
    /// the module doc: only the walk band resolves, and it resolves the
    /// cell the CURSOR points at, not the observer's own — every other band
    /// answers [`NOTHING_HERE_YET`] honestly.
    fn refresh_strip(&mut self) {
        self.strip = Some(self.resolve());
    }

    /// The strip text for the current turn: [`NOTHING_HERE_YET`] unless the
    /// current snapshot is a walk-band scene, in which case the cell the
    /// cursor points at ([`Self::resolve_walk_band`]; see the module doc
    /// for the chain from a screen position to a `CellId`) is resolved
    /// against the terrain-feature index — [`UNNAMED_TERRAIN`] if that
    /// chain comes up empty at any step.
    fn resolve(&self) -> String {
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
