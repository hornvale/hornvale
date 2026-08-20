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
//! **Resolution is scoped honestly, not fully.** Spec §3.1 assigns the
//! cursor query to `bin`; §9 defers "the walk / chamber / delve resolvers"
//! (session-level cells/marks/agents) to a future campaign. This campaign
//! wires the terrain-feature index (declared salience, `CellFeatureIndex`,
//! `resolve_at` — all real, tested, and measured directly against seed 42 in
//! the task report's F2/F3/H1). But the terminal client has no plate that
//! draws the "world"/Mercator band those measurements assume — only the
//! walk-band chart and the chamber-band floor plan exist today — and
//! inverting the walk-band chart's screen projection back to a `CellId`
//! needs geometry no task in this campaign's file list touches (`chart.rs`'s
//! `project` is private and per-band-scaled; a wire `ChartCell` carries no
//! `CellId` at all — the Room Mesh, a different addressing scheme, is the
//! closest thing that does, and reconciling it is deferred walk-band-
//! resolver work, not this one). So this campaign's resolver answers for
//! exactly the one `CellId` it can obtain honestly and cheaply — the
//! **observer's own cell** — for as long as look mode is open at the walk
//! band, regardless of where the on-screen cursor sits; every other band
//! honestly refuses (`NOTHING_HERE_YET`). The cursor's on-screen motion is
//! real (Task 1/2 shipped it); its resolution's spatial precision is the
//! part carried forward. Flagged explicitly for review, in the same spirit
//! as the task brief's own "Driver is delicate" note asking scope calls to
//! be surfaced rather than forced.

use crate::input::{Action, Mode};
use hornvale_astronomy::SkyPins;
use hornvale_game_core::Cursor;
use hornvale_kernel::{NearestCellIndex, Seed, World};
use hornvale_language::{MorphOptions, Phonology};
use hornvale_terrain::TerrainPins;
use hornvale_terrain::landscape::CellFeatureIndex;
use hornvale_vessel::{
    PossessOpts, PossessTarget, Session, VesselError, WorldContext, snapshot_json,
};
use hornvale_worldgen::{
    BuildError, SettlementPins, SkyChoice, WorldComponents, build_world, gazetteer_features,
    language_of_in, morph_options, resolve_at, terrain_of,
};

/// What the strip says in look mode over a band this campaign has no
/// resolver for (walk and chamber both draw a real plate; only the
/// terrain-feature index answers a query — spec §3.1/§9). Faking a
/// resolution would be worse than refusing: a wrong name is
/// indistinguishable from a right one, and this string never is one.
const NOTHING_HERE_YET: &str = "nothing here yet";

/// What the strip says when the resolver ran and genuinely found no
/// individuated feature at the observer's cell — real terrain below every
/// class's individuation floor, not "nothing there" (seed 42 measured 377
/// of 40,962 cells like this; spec §3.4).
const UNNAMED_TERRAIN: &str = "unnamed terrain";

/// The plate's content height at the 80x24 floor. `hornvale-game-core`'s
/// `RESERVED_ROWS` (private to that crate) is 4 at this floor — the strip's
/// own row, a gutter, the endpaper, and a margin (see `spread.rs`'s module
/// doc) — so content occupies the remaining rows. The look-mode cursor is
/// bounded to this fixed floor-sized plate regardless of the terminal's
/// actual size: a known simplification (this campaign does not thread the
/// live terminal size into the driver — see the task report).
const PLATE_CONTENT_HEIGHT: u16 = hornvale_game_core::MIN_HEIGHT - 4;

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
    /// Which thing keys drive — the character, or the look-mode cursor.
    mode: Mode,
    /// The look-mode cursor's screen position, in plate grid cells. Held
    /// regardless of `mode` (so leaving and re-entering look mode does not
    /// reset it); [`Driver::cursor`] reports it only in [`Mode::Look`].
    cursor: Cursor,
    /// The look-mode strip's cached text — recomputed on `EnterLook` and
    /// `CursorBy` (see module doc: this campaign's resolver does not vary
    /// with the cursor's screen position, so recomputation is cheap and
    /// simple rather than actually necessary; F2 measures the real cost).
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
    /// The possessed agent's species, phonology and naming morphology,
    /// resolved once at `start` (an agent's species does not change during
    /// a session) — the same triple [`resolve_at`] needs on every call.
    namer: (String, Phonology, MorphOptions),
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
            mode: Mode::Normal,
            cursor: Cursor {
                x: hornvale_game_core::spread::PLATE_WIDTH / 2,
                y: PLATE_CONTENT_HEIGHT / 2,
            },
            strip: None,
            index,
            nearest,
            geo,
            seed: world_ref.seed,
            namer: (species, ph, morph),
        };
        driver.refresh();
        Ok(driver)
    }

    /// Which thing keys currently drive — the character, or the look-mode
    /// cursor. `main`'s input loop reads this every keypress to choose
    /// between [`crate::input::verb_for`]'s normal-mode dispatch and
    /// [`crate::input::action_for`]'s mode-aware one.
    pub fn mode(&self) -> Mode {
        self.mode
    }

    /// The look-mode cursor's screen position, or `None` outside look mode
    /// — matching `render_with`'s `cursor` parameter, which hides the
    /// terminal's hardware cursor on `None`.
    pub fn cursor(&self) -> Option<Cursor> {
        (self.mode == Mode::Look).then_some(self.cursor)
    }

    /// The look-mode strip's current text, or `None` outside look mode —
    /// matching `render_with`'s `strip` parameter.
    pub fn strip_text(&self) -> Option<&str> {
        if self.mode == Mode::Look {
            self.strip.as_deref()
        } else {
            None
        }
    }

    /// Apply one input [`Action`]: send a verb line exactly as before this
    /// campaign, move the cursor, or toggle look mode. Costs a turn only
    /// for [`Action::Verb`] — cursor motion and mode toggles are free,
    /// matching `input`'s own "costs no turn" contract for everything it
    /// does not map to a verb.
    pub fn apply(&mut self, action: Action) {
        match action {
            Action::Verb(line) => {
                self.handle(&line);
            }
            Action::EnterLook => {
                self.mode = Mode::Look;
                self.refresh_strip();
            }
            Action::LeaveLook => {
                self.mode = Mode::Normal;
                self.strip = None;
            }
            Action::CursorBy(dx, dy) => {
                self.move_cursor(dx, dy);
                self.refresh_strip();
            }
            Action::None => {}
        }
    }

    /// Move the cursor by `(dx, dy)` grid cells, clamped to the plate's
    /// bounds at the 80x24 floor (see `PLATE_CONTENT_HEIGHT`'s doc for the
    /// known simplification this is).
    fn move_cursor(&mut self, dx: i16, dy: i16) {
        let max_x = i32::from(hornvale_game_core::spread::PLATE_WIDTH) - 1;
        let max_y = i32::from(PLATE_CONTENT_HEIGHT) - 1;
        let nx = (i32::from(self.cursor.x) + i32::from(dx)).clamp(0, max_x);
        let ny = (i32::from(self.cursor.y) + i32::from(dy)).clamp(0, max_y);
        self.cursor.x = nx as u16;
        self.cursor.y = ny as u16;
    }

    /// Recompute `self.strip` for the current band and mode. See the module
    /// doc: only the walk band resolves, and only for the observer's own
    /// cell — every other band answers [`NOTHING_HERE_YET`] honestly.
    fn refresh_strip(&mut self) {
        self.strip = Some(self.resolve());
    }

    /// The strip text for the current turn: [`NOTHING_HERE_YET`] unless the
    /// current snapshot is a walk-band scene, in which case the observer's
    /// own cell is resolved against the terrain-feature index —
    /// [`UNNAMED_TERRAIN`] if the resolver runs and finds nothing there.
    fn resolve(&self) -> String {
        let Ok(snap) = hornvale_game_core::Snapshot::parse(&self.cached) else {
            return NOTHING_HERE_YET.to_string();
        };
        if !matches!(snap.spatial, hornvale_game_core::Spatial::Walk { .. }) {
            return NOTHING_HERE_YET.to_string();
        }
        let coord = self.session.agent().position.coord();
        let cell = self
            .nearest
            .nearest(&self.geo, coord.latitude, coord.longitude);
        let (species, ph, morph) = &self.namer;
        resolve_at(&self.index, cell, self.seed, species, ph, morph)
            .unwrap_or_else(|| UNNAMED_TERRAIN.to_string())
    }

    /// The current turn's `vessel/session/v2` JSON — what `hornvale-game-
    /// core::render` consumes. Does not advance the turn.
    pub fn snapshot(&self) -> String {
        self.cached.clone()
    }

    /// Hand one line to the session and return the resulting snapshot JSON.
    ///
    /// The client never validates a move before sending it: `Session::
    /// handle` tokenizes and parses, and an illegal or unrecognised verb
    /// comes back as the sim's own prose (e.g. "No way n from here."),
    /// already folded into the returned snapshot's `narration.prose` — this
    /// function has no need to branch on `Turn::Out` vs `Turn::Released` to
    /// report that back, because both carry the same text the snapshot
    /// already carries.
    pub fn handle(&mut self, line: &str) -> String {
        let _turn = self.session.handle(line);
        self.refresh();
        self.cached.clone()
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
