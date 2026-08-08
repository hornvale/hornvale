//! The possession session: a pure step function over a frozen world. Every
//! verb is read-only; possessing a world never changes it.

use crate::liveness::{
    AGENT_AT, Affect, AffectLabel, DRANK, DriveKind, DriveMovements, EATEN, HomeNavCache,
    LocaleTerrain, Npc, Occupancy, PrimaryAfraidMemo, RESTED, SUSTENANCE, affect_of_memo_occupied,
    agent_position, built_rooms, derive_npcs, derive_wild_npcs,
};
use crate::snapshot::{
    KnownChannel, KnownEntry, Narration, NounEntry, PresentEntry, SESSION_SCHEMA, SelfChannel,
    SensedChannel, SessionSnapshot, SocialEntry, SpatialChannel,
};
use crate::{
    Agent, Focalized, Focalizer, IdentityProjection, Knowledge, PossessOpts, Projection,
    TemplateFocalizer, Turn, VesselError, absorb_common, mint_flagship, observable, reader_set,
};
use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, Ledger, RoomAddr, RoomId, Seed, Value, World, WorldTime, tick,
};
use hornvale_locale::{Compass, Direction, ExitKind, LocaleContext};

/// How many NPCs a session derives (spec §4: a small authored constant, not
/// every settlement — the flagship's own leader plus a couple of neighbors).
const NPC_COUNT: usize = 3;

/// How many WILD beast agents a session derives (The Wilding) — a small handful
/// of the world's fauna (a herd, a lair) walking alongside the peoples.
const WILD_COUNT: usize = 4;

/// The closed fallback line `consult` renders when no initiated line
/// unlocks (spec §3.2; the Global Constraints' closed-strings list).
const CONSULT_FALLBACK: &str = "The Book holds more for the initiated.";

/// How far the possession sees inside a chamber, as a **Chebyshev** radius in
/// cells — the metric [`crate::lattice::shadowcast`] itself bounds, so the
/// constant and the algorithm cannot disagree about what "four" means.
///
/// **It is a stand-in, and which kind of stand-in matters.** Hornvale has no
/// indoor lighting model, so no physical quantity fixes this number. What fixes
/// it is the requirement that it BIND. A chamber [`crate::lattice::allocate`]
/// draws is a rectangle; a rectangle is convex; so occlusion alone never hides
/// one floor cell of a chamber from another. And every structure a possession
/// can enter takes that method — `embed_with` selects on `brief.built`, and
/// `structure_at` returns `None` without it — so occlusion is not a live
/// narrowing today at all. A chamber spans [`crate::lattice::CHAMBER_SIDE`] = 8
/// cells, whose Chebyshev diameter is 7, so **at radius 7 or more the narrowing
/// is decoration**: it would remove nothing any built world can produce. Half a
/// chamber is the largest round number that is not decoration.
///
/// One named constant rather than a literal at the call site, so that the day a
/// light model arrives there is exactly one place to replace, and so no second
/// caller can quietly disagree with the first.
/// type-audit: bare-ok(count)
const SIGHT_RADIUS: i32 = crate::lattice::CHAMBER_SIDE / 2;

// A creature's `kind`, `datum` and `salience` on the plan are NOT this module's
// to invent: they are `crate::purview`'s `AGENT_MARK_KIND`, `creature_datum` and
// `AGENT_SALIENCE`, the same three the walk-band chart marks the same creature
// with. Fix round 1's finding is why they are shared rather than restated — this
// module's first draft wrote its own `"creature"` kind and a felt-state datum,
// so one creature answered `examine` with two different sentences depending on
// which side of a doorway the player was standing. See `Session::sighting`.

/// The ways-on name for the aperture leading DEEPER into a structure — a
/// direction, not a thing, because a chamber address carries no bearing and the
/// chambers of one structure are prose-identical, so only depth distinguishes
/// them.
const FURTHER_IN: &str = "further in";

/// Every token `enter` accepts for [`FURTHER_IN`]. `in` and `on` are here
/// because a player who read `Ways on: out, further in.` may reasonably type
/// either half of it.
const FURTHER_IN_WORDS: [&str; 3] = ["further in", "in", "on"];

/// What `map out [N]` says INDOORS. A plan is ONE building, so there is no
/// coarser rung of it to draw: zooming out of a chart is path truncation up the
/// address space, and truncating a structure's plan does not reach a bigger
/// building — it reaches the LAND the building stands on, which is a different
/// question with a different answer, and it is asked out of doors. Names the verb
/// that gets there rather than refusing blankly.
///
/// (`INDOOR_EXAMINE_REFUSAL` used to stand here. Its own doc comment deferred
/// authored chamber detail to a later campaign; this is that campaign, so it is
/// retired rather than reworded — see `chamber_prose::detail`.)
const INDOOR_CHART_REFUSAL: &str =
    "Inside, the chart is the floor you stand on; step 'out' to read the land.";

/// What `back` says INDOORS.
///
/// `INDOOR_LATERAL_REFUSAL` used to stand here and covered `go` as well. The
/// Blocking reversed the `go` half and left this one, and the split is the whole
/// point: **metaplan §1b.6's law — lateral movement never changes band — is
/// unchanged**, because a cell step stays inside the chamber band. What was wrong
/// was the inference the old constant recorded, that "a chamber address carries no
/// bearing to walk along": true of a chamber with no interior, false of one with a
/// lattice, which is what this campaign built.
///
/// `back` is refused for the reason that survives regardless: it retraces the
/// WALK-band trail (`Session::trail` holds walk-band addresses), so it is a
/// walk-band operation whatever the interior looks like. Un-refusing both at once
/// would have blurred which capability justified which reversal.
///
/// A chamber trail — "back one cell", or "back one chamber" — is a different
/// feature with its own state to keep correct, and nothing asks for one yet.
const INDOOR_BACK_REFUSAL: &str =
    "Inside, there is no trail to retrace; the way you came is 'out'.";

/// What a DIAGONAL step says indoors.
///
/// [`crate::lattice::HEADINGS`] is orthogonal only: a diagonal step through the
/// corner where two walls meet is not a way through a building. Refused with the
/// geometry as the reason, not with a parse complaint, and it names the four
/// bearings that do work — the same standard `INDOOR_CHART_REFUSAL` holds itself
/// to.
const INDOOR_DIAGONAL_REFUSAL: &str =
    "There is no slipping through a corner; step north, south, east or west.";

/// What lateral movement says while SUBMERGED (The Column). Swimming between
/// coordinates is a later campaign; for now the water column is entered and left
/// at one place, so a compass step from a stratum is refused rather than
/// silently surfacing the possession into the next locale. Diegetic, not a parse
/// error.
///
/// (Main attached this doc comment to the retired `INDOOR_LATERAL_REFUSAL`'s
/// prose by accident when it inserted this constant above it; the indoor half of
/// that text died with the constant, so only the water's own reason remains.)
const SUBMERGED_LATERAL_REFUSAL: &str = "Not while you are under. Surface first, then swim.";

/// What lateral movement says while underground (The Deep Realm). The cave
/// lattice this campaign ships has no walkable interior — only the entrance
/// chamber is reachable — so a compass step from it is refused for the same
/// reason a step from a water stratum is: there is nowhere down here for a
/// bearing to mean. Diegetic, not a parse error, matching
/// [`SUBMERGED_LATERAL_REFUSAL`]'s own reasoning one realm over.
const UNDERGROUND_LATERAL_REFUSAL: &str = "Not down here. Climb out first, then walk.";

/// The player-authored disposition-shift predicate (The First Mark): the
/// first fact the possessing player, not a world system, ever commits.
/// type-audit: bare-ok(identifier-text)
pub const DISPOSITION_SHIFT: &str = "disposition-shift";

/// How hard one committed `disposition-shift` fact leans on an NPC's
/// grievance toward the possessing player (The First Mark, direct social
/// consequence — decision-ledger #6: the first slice's consequence is
/// direct social, not an ambient drive tip). A game-design coefficient, not
/// a tuned physical constant.
/// type-audit: bare-ok(ratio)
pub const GRIEVANCE_GAIN: f64 = 1.0;

/// Net grievance at which a neutral NPC turns hostile toward the player —
/// three net provokes, one per day (same-day repeats dedup, Task 1), so
/// three distinct days of antagonism. A game-design constant, not an
/// empirical drive value: this mechanic never reads or perturbs the
/// homeostatic drive layer (`liveness.rs`), and there is no seed-42
/// calibration behind it.
/// type-audit: bare-ok(ratio)
pub const HOSTILITY_THRESHOLD: f64 = 3.0;

/// The one-hop forward integration (The First Mark): an NPC whose grievance
/// crosses `HOSTILITY_THRESHOLD` commits this fact toward the possessing
/// player. Functional per subject — an NPC turns hostile once, so the `wait`
/// tick's firing (guarded by `Ledger::value_of`) is idempotent by
/// construction, not by a separate dedup check.
/// type-audit: bare-ok(identifier-text)
pub const TURNED_HOSTILE: &str = "turned-hostile";

/// An NPC's grievance toward the possessing player: the additive fold over
/// their committed `disposition-shift` facts (The First Mark, direct social
/// consequence). Zero with no player facts, so an unplayed world — or a
/// session that never provokes/soothes this NPC — is byte-identical to that
/// zero by construction.
pub(crate) fn grievance(ledger: &Ledger, npc: EntityId) -> f64 {
    ledger
        .facts_about(npc)
        .filter(|f| f.predicate == DISPOSITION_SHIFT)
        .map(|f| match f.object {
            Value::Number(n) => n,
            _ => 0.0,
        })
        // `Iterator::sum::<f64>()` folds from `-0.0` (the float additive
        // identity that Rust's stdlib picks so an all-negative-zero sum
        // stays negative), so an NPC with no disposition-shift facts would
        // otherwise serialize as `-0.0` rather than the plain `0.0` the doc
        // comment above promises. Fold from an explicit `0.0` instead.
        .fold(0.0, |acc, n| acc + n)
        * GRIEVANCE_GAIN
}

const HELP: &str = "\
verbs:
  look             where you stand, focalized
  map [out N]      the chart of what lies around you (N rungs coarser);
                   indoors, the floor plan of the building you are in
  eyes [who]       whose eyes you see colour through (a species, 'own',
                   'standard', or 'off'); bare, it says what yours drop
  go <dir>         walk a compass exit, out of doors (n ne e se s sw w nw);
                   the bare direction works on its own too
  dive             descend a layer of the water column; 'surface' comes back
  surface          rise a layer, and at the top return to the open air
  delve            descend into the cave at this cell, if the rock admits
                   one; 'climb' comes back
  climb            return to the surface from underground
  enter [way]      step inside what is built here; once inside, 'enter further
                   in' goes deeper and 'out' leaves
  out              step back out of doors
  examine <thing>  anything look or the floor plan names
  back             retrace your last step, out of doors
  wait [N]         let N days pass overhead (default 1); the world moves too
  whoami           the one you possess
  knows            everything they have seen
  npcs             the derived NPCs sharing this world (label, id)
  why <who>        recount an NPC's dated history (by label or id)
  needs            read the felt state of anyone sharing this room
  provoke [who]    shift a co-located NPC's disposition, your own mark
  soothe [who]     ease a co-located NPC's disposition, your own mark
  write <sentence> speak a line of Common; you absorb what it says, written
                   into your own margin
  consult          read the Book's Reckoning at your own day, and whatever
                   your margin has initiated you into
  release          let go (quit works too)
";

/// A live possession over a frozen world. The possessed agent's own senses
/// stay pinned to the frozen `world` (byte-identical, never mutated); only
/// the NPC layer evolves, in a session-owned ledger clone (the-quickening).
pub struct Session<'w> {
    world: &'w World,
    ctx: LocaleContext,
    agent: Agent,
    knowledge: Knowledge,
    trail: Vec<RoomAddr>,
    day: WorldTime,
    focalizer: TemplateFocalizer,
    projection: IdentityProjection,
    /// The evolving ledger: a clone of the frozen world's ledger, mutated
    /// only by `wait`'s tick (NPC `agent-at` facts). Never written back.
    ledger: Ledger,
    /// A clone of the world's registry, extended with `AGENT_AT` (registered
    /// per-session, never at genesis — spec §3).
    registry: ConceptRegistry,
    /// Whose eyes the possession's chart is coloured through (The Beholding,
    /// Task 4), carried from `PossessOpts::eyes`.
    eyes: crate::eyes::Eyes,
    /// The NPCs this session derived at `start` (re-derivable, never saved).
    npcs: Vec<Npc>,
    /// The world's terrain, sculpted once at `start` (The Shuttle), so every
    /// book-reading verb (`write`, `consult`) shares one sculpt instead of
    /// re-sculpting the globe per call; `None` on a world whose committed
    /// terrain pins fail to parse. Threaded into the worldgen/book `_from`
    /// readout family (`reckoning_at_from`, `esoteric_lines_from`,
    /// `hornvale_book::parse_context_from`) whenever both this and
    /// [`Session::climate`] are present; the unthreaded (`_of`/bare) form is
    /// the fallback on a `None`, matching what those calls already did
    /// before this campaign.
    terrain: Option<hornvale_terrain::GeneratedTerrain>,
    /// The world's climate, sculpted once at `start` from [`Session::terrain`]
    /// (The Shuttle); `None` exactly when `terrain` is `None` or the climate
    /// fit itself fails. See `terrain`'s doc for what shares it.
    climate: Option<hornvale_climate::GeneratedClimate>,
    /// The world's calendar, built once at `start`, so the NPC wake cycle reads
    /// the real sun (The Slumber Tier-1); `None` on a world with no sky.
    calendar: Option<hornvale_astronomy::Calendar>,
    /// The world's predator-pressure field (The Quarry), computed once at
    /// `start`, so the danger drive senses carnivore territory; `None` if the
    /// demography fit fails.
    predator: Option<hornvale_kernel::CellMap<f64>>,
    /// The world's prey-pressure field (The Teeth), computed once at `start`, so
    /// a carnivore's hunger senses prey territory; `None` if the demography fit
    /// fails.
    prey: Option<hornvale_kernel::CellMap<f64>>,
    /// The world's settlement-territory set (The Threshold, task 5b —
    /// `built_rooms`), computed once at `start`, so a room a settlement
    /// actually occupies reads as built and can draw a real hearth.
    /// `Session::start` requires `mint_flagship` to resolve a settlement
    /// first, so in practice this always carries at least the possessed
    /// agent's own home room by the time a session exists.
    built: std::collections::BTreeSet<RoomId>,
    /// Each NPC's within-room anchor as of the most recent `wait` tick's own
    /// walk (The Threshold whole-branch review, Important 4) — recovered via
    /// [`DriveMovements::step_with_occupancy`] the same way the lab's health
    /// battery (task 6b) does, so a narration read of a co-located NPC's felt
    /// state (`Session::needs`, the snapshot's present-entry read) samples
    /// warmth where the NPC actually walked to rather than unconditionally
    /// falling back to the room's landing anchor. Empty before the first
    /// `wait` (turn 0's affect reads fall back exactly as they always did).
    occupancy: Occupancy,
    /// Commits since the possession began; 0 is the opening. Advanced by
    /// `handle` for every non-empty verb line, so the snapshot can label
    /// which turn it describes.
    /// type-audit: bare-ok(count: turn)
    turn: u64,
    /// This turn's rendered text, exactly as the prose ABI returned it —
    /// the opening at `start`, then each verb's own response. The snapshot
    /// carries it verbatim so that EVERY pane, transcript included, is a
    /// pure projection of one snapshot; a client forced to read the turn
    /// text from a second channel would break that, which is the campaign's
    /// central claim.
    /// type-audit: bare-ok(prose: last_text)
    last_text: String,
    /// Where the possession is indoors, or `None` at the walk band.
    ///
    /// The possessed agent's own `position` stays at the WALK band throughout —
    /// descent is recorded here, not there — so every walk-band read (`map`,
    /// `whoami`, `purview`, the snapshot, the NPC layer) is untouched by being
    /// indoors.
    inside: Option<Inside>,
    /// The stratum the possession has descended to within the water column,
    /// if any. `None` is the surface — standing on land, or afloat on the sea.
    /// The depth band, mirroring `inside`: a second way of being somewhere
    /// other than out of doors at ground level.
    submerged: Option<hornvale_climate::Stratum>,
    /// The chamber the possession has descended into within the cave lattice
    /// beneath this cell, if any (The Deep Realm, Task 5). `None` is the
    /// surface. Mirrors `submerged`: the whole resolved value is carried
    /// rather than just an address, so `climb` and a later `look` never need
    /// to re-derive it (`chamber_at` is pure and would return the same
    /// content either way, but there is nothing to gain by re-deriving what
    /// is already in hand). This campaign's lattice has only the entrance
    /// address reachable from the vessel seam — no deeper descent verb
    /// exists yet — so this is always the entrance chamber (`band = 0,
    /// slot = 0`) when `Some`.
    underground: Option<hornvale_worldgen::chamber::Chamber>,
    /// The session-lived geometry memo (the-waymark fix round, Finding 2):
    /// `RoomMeshMemo` is fixed for this session's whole lifetime (`neighbors`
    /// is world-independent; `corner_weights` is fixed once `ctx`'s
    /// `(Geosphere, NearestCellIndex)` pair is built at `start` and never
    /// changes), so it is owned HERE — one level above
    /// `DriveMovements::step_with_occupancy`'s own per-tick loop, not rebuilt
    /// (and discarded) inside it every `wait`. `wait` prefills it for each
    /// NPC's current position and neighbours before building this tick's
    /// `LocaleTerrain` (Finding 1) and threads it `&mut` into
    /// `step_with_occupancy` (for the neighbours half); `snapshot`/`needs`
    /// (both `&self`) read whatever it already holds without adding to it.
    mesh_memo: hornvale_kernel::RoomMeshMemo,
    /// The session-lived, CROSS-tick home-plan cache (the-waymark, Task 4):
    /// unlike `mesh_memo` above (whose per-tick geometry is re-prefilled every
    /// `wait`), this one is never rebuilt — a stationary NPC with an unchanged
    /// believed-hazard set must pay zero `plan_to_room` searches on every
    /// `wait` after its first, which requires the cache itself, not merely
    /// its backing memo, to outlive one tick. See `HomeNavCache`'s own doc.
    home_nav_cache: HomeNavCache,
}

/// Where the possession is while indoors. `FRAME`-tier in its entirety: derived
/// at `enter`, dropped at `leave`, never serialized (decision 0069), so entering
/// and leaving cannot alter the world and re-walking a place is byte-identical by
/// construction rather than by policy.
///
/// **A struct rather than the tuple this was.** Tasks 1–4 carried
/// `Option<(Structure, usize)>` at nine call sites, which read acceptably at two
/// elements; Task 5 adds a cell, and `inside.2` for a chamber index beside a
/// `Cell` is where those sites stop being readable.
///
/// The lattice is CARRIED rather than re-derived per turn. It is a pure function
/// of the structure and the locale's seed, so re-deriving it every turn would be
/// correct and wasteful — while caching it would be neither if it outlived the
/// descent, which is exactly why it dies with this struct instead of living in a
/// map on the session. `the_carried_lattice_is_the_one_the_place_derives` asserts
/// the copy has not drifted from the derivation.
///
/// **Untagged for the type audit, deliberately.** The plan's snippet gave `at` a
/// `bare-ok(index: at)`; the audit extracts only `pub` items
/// (`tools/type-audit/src/extract.rs::is_bare_pub`), so a tag on a private struct
/// is never enforced — the same "a tag the tool never reads is worse than absent"
/// rule the Global Constraints state for untracked signatures. `at` is a chamber
/// index into `structure.chambers`, said in prose above instead.
struct Inside {
    /// The structure being stood in.
    structure: crate::structure::Structure,
    /// Which chamber, as an index into `structure.chambers`.
    at: usize,
    /// The floor plan of the whole structure — every chamber, not just this one,
    /// because a plan is a property of the building (`lattice_of`).
    lattice: crate::lattice::Lattice,
    /// Which cell of it the possession occupies. Always a cell that `serves(at)`
    /// and is passable; a `Floor` cell, never a `Threshold`, so the drawn mark
    /// cannot hide a doorway (`lattice::cell_beyond`).
    cell: crate::lattice::Cell,
    /// The seed this frame's geometry is drawn from — the locale's own seed
    /// ([`Session::frame_seed`]), the one `lattice` above was embedded with and
    /// the one [`crate::lattice::anchor_cells`] places anchors with.
    ///
    /// Carried rather than re-derived for the reason `lattice` is: it is a
    /// property of the STRUCTURE, fixed for as long as the possession stands in
    /// it, and re-deriving it per snapshot would invite the two to disagree.
    /// It is also the one lever The Sighting's negative control needs — perturb
    /// this and the embedding moves while nothing else does, which is exactly
    /// the experiment spec §2.1 asks for.
    seed: Seed,
}

/// What the fine layer says about the chamber the possession is standing in:
/// where each co-located creature has been drawn, and which cells the
/// possession can see from where it stands.
///
/// `FRAME`-tier in its entirety, like everything else in this band (decision
/// 0069): derived inside one [`Session::snapshot`] call and dropped when it
/// returns. Nothing here is committed, and that is the campaign's central
/// constraint rather than an implementation detail — the embedding may decide
/// what a client is SHOWN, never what an agent comes to BELIEVE (spec §2.1).
/// `Session::knowledge` is not read or written on this path.
struct Sighting {
    /// Every cell the possession can see, [`SIGHT_RADIUS`] Chebyshev cells out
    /// and stopping at the fabric.
    lit: std::collections::BTreeSet<crate::lattice::Cell>,
    /// Where each co-located creature the embedding could place stands. A
    /// creature is ABSENT here for four distinct reasons, all legitimate:
    /// nothing has recorded its within-room anchor yet (no tick has run), the
    /// recorded anchor no longer names a place this room composes, this chamber
    /// composes no anchor of that anchor's kind, or the cell it would take is
    /// already held (§7 rule 5). Absence therefore never means "hidden" — which
    /// is why [`Session::snapshot`] narrows `sensed.present` only on a creature
    /// this map DOES place.
    placed: std::collections::BTreeMap<EntityId, crate::lattice::Cell>,
}

impl<'w> Session<'w> {
    /// Begin a possession: build the locale context, mint the flagship
    /// agent, absorb the first projection, and return the opening text.
    /// type-audit: bare-ok(prose: return)
    // Named construction site (decision 0092): the motivating fix — sculpts/
    // fits exactly ONCE per session (The Weir, Stage 2), threaded below.
    #[allow(clippy::disallowed_methods)]
    pub fn start(
        world: &'w World,
        opts: &PossessOpts,
    ) -> Result<(Session<'w>, String), VesselError> {
        // ONE derivation block (The Weir, Stage 2): terrain, climate, the
        // locale context, the species roster and the demography report are
        // each derived EXACTLY ONCE here, then threaded into everything
        // below — `LocaleContext::build_from`, the predator/prey pressures,
        // the wild-NPC concentration fit — instead of a consumer quietly
        // re-sculpting or re-fitting its own copy. Terrain/climate failure
        // is a hard failure for `start`, exactly the failure
        // `LocaleContext::build` used to surface on this identical call
        // (`build` is still the right entry point for a caller that has not
        // already sculpted its own pair — see its doc).
        let terrain = hornvale_worldgen::terrain_of(world)
            .map_err(|e| VesselError::Locale(hornvale_locale::LocaleError::Build(e.to_string())))?;
        let climate = hornvale_worldgen::climate_from(world, &terrain)
            .map_err(|e| VesselError::Locale(hornvale_locale::LocaleError::Build(e.to_string())))?;
        let ctx = LocaleContext::build_from(world, &terrain, &climate);
        // A cheap failure path (a settlement/species lookup) — resolved
        // before the expensive coexistence-stack fit below (Task 3 review
        // carry-over), so a settlement-less or unspecied world fails fast
        // rather than paying for a fit `start` would then discard.
        let agent = mint_flagship(world, &ctx)?;
        // The species roster and the demography report, assembled/fit ONCE
        // per session (The Weir, Stage 1b/2): shared below by `predator`/
        // `prey` and by the wild-NPC derivation instead of each
        // independently re-running the coexistence-stack fit over the same
        // `(world, wc, terrain, climate)`. `None` whenever `wc` or the fit
        // itself fails — the same `Option` posture as `calendar`/
        // `predator`/`prey` below.
        let wc = hornvale_worldgen::WorldComponents::assemble().ok();
        let report = match wc.as_ref() {
            Some(wc) => {
                hornvale_worldgen::demography_report_from(world, wc, &terrain, &climate).ok()
            }
            None => None,
        };
        // Wrapped in `Some` from here on: both derivations above already
        // succeeded (the `?`s), so `Session::terrain`/`Session::climate`
        // are `Option` only for the field's own defensive posture (see its
        // doc), never because a second, independent derivation could fail
        // where this one didn't.
        let terrain = Some(terrain);
        let climate = Some(climate);
        let mut ledger = world.ledger.clone();
        let mut registry = world.registry.clone();
        // Idempotent (same def every session): never conflicts, since
        // AGENT_AT is never registered at genesis (spec §3).
        registry
            .register_predicate(AGENT_AT, false, "an agent's position on a day")
            .expect("AGENT_AT registers identically every session");
        // Idempotent (same def every session): never conflicts, since DRANK
        // is never registered at genesis either (spec §3).
        registry
            .register_predicate(DRANK, false, "an agent satisfied its sustenance goal")
            .expect("DRANK registers identically every session");
        registry
            .register_predicate(
                RESTED,
                false,
                "an agent rested (eased its fatigue) on a day",
            )
            .expect("RESTED registers identically every session");
        registry
            .register_predicate(EATEN, false, "an agent ate (eased its hunger) on a day")
            .expect("EATEN registers identically every session");
        // The player's disposition mark — the first player-authored predicate.
        // Non-functional (a subject may be provoked and later soothed; each is
        // one dated fact). Additive: registering a new predicate perturbs
        // nothing already committed.
        registry
            .register_predicate(
                DISPOSITION_SHIFT,
                false,
                "an agent's disposition was shifted by the possessing player",
            )
            .expect("DISPOSITION_SHIFT registers identically every session");
        // The consequence of the mark above (The First Mark, one-hop forward
        // integration): functional (an NPC turns hostile once — the second
        // commit attempt is a guaranteed no-op, not just a discouraged one).
        registry
            .register_predicate(
                TURNED_HOSTILE,
                true,
                "an NPC turned hostile toward the possessing player",
            )
            .expect("TURNED_HOSTILE registers identically every session");
        // Guarantee the possessed agent's OWN settlement contributes a
        // derived NPC (the-quickening T3 review): otherwise no NPC is ever
        // co-located with the player and the observation payoff can't fire.
        let mut npcs = derive_npcs(world, &ctx, &mut ledger, NPC_COUNT, agent.village.id);
        // The Wilding: append a few wild beast agents (a herd, a lair) so the
        // world's fauna walks alongside its peoples — and a herbivore beast
        // finally fears predator ground (The Quarry, live). Off only for the
        // settled-population narration unit tests that isolate the peopled path.
        if opts.wild_agents {
            // The wild-concentration roster, from the same shared `report`
            // (The Weir, Stage 1b) rather than a fourth independent fit.
            let concentrations = match (wc.as_ref(), report.as_ref()) {
                (Some(wc), Some(report)) => {
                    hornvale_worldgen::wild_concentrations_from(wc, report, WILD_COUNT)
                }
                _ => Vec::new(),
            };
            npcs.extend(derive_wild_npcs(world, &ctx, &mut ledger, concentrations));
        }
        // Build the world's calendar once, for the NPC wake cycle's real-sun
        // read (The Slumber Tier-1). Absent (no sky) → the fractional-day sun.
        let calendar = hornvale_worldgen::sky_of(world)
            .ok()
            .and_then(|sky| sky.calendar().cloned());
        // The predator-pressure field (The Quarry), so the danger drive
        // senses carnivore territory — from the shared `report` above (The
        // Weir, Stage 1b) rather than its own fit. `None` on a missing
        // input (danger simply loses its PREDATOR axis).
        let predator = match (wc.as_ref(), terrain.as_ref(), report.as_ref()) {
            (Some(wc), Some(terrain), Some(report)) => Some(
                hornvale_worldgen::predator_pressure_from(wc, terrain, report),
            ),
            _ => None,
        };
        // The prey-pressure field (The Teeth), so a carnivore's hunger senses
        // prey territory — the dual of the predator field, same shared fit.
        let prey = match (wc.as_ref(), terrain.as_ref(), report.as_ref()) {
            (Some(wc), Some(terrain), Some(report)) => {
                Some(hornvale_worldgen::prey_pressure_from(wc, terrain, report))
            }
            _ => None,
        };
        // The settlement-territory set (The Threshold, task 5b), so a room a
        // settlement actually occupies reads as built and can draw a real
        // hearth — the real answer Task 5's arming had nothing to read before
        // this. Built once here, the same one-shot-at-start discipline as
        // `calendar`/`predator`/`prey`.
        let built = built_rooms(world, &ctx);
        let mut session = Session {
            world,
            ctx,
            agent,
            knowledge: Knowledge::default(),
            trail: Vec::new(),
            day: opts.day,
            focalizer: TemplateFocalizer,
            projection: IdentityProjection,
            ledger,
            registry,
            eyes: opts.eyes.clone(),
            npcs,
            terrain,
            climate,
            calendar,
            predator,
            prey,
            built,
            occupancy: Occupancy::default(),
            turn: 0,
            last_text: String::new(),
            inside: None,
            submerged: None,
            underground: None,
            mesh_memo: hornvale_kernel::RoomMeshMemo::new(),
            home_nav_cache: HomeNavCache::new(),
        };
        session.absorb_here()?;
        let opening = session.describe_here()?;
        session.last_text = opening.clone();
        Ok((session, opening))
    }

    /// The possessed agent (read-only).
    pub fn agent(&self) -> &Agent {
        &self.agent
    }

    /// The accumulated knowledge (read-only).
    pub fn knowledge(&self) -> &Knowledge {
        &self.knowledge
    }

    /// The locale context this session walks (for the battery's checks).
    pub fn context(&self) -> &LocaleContext {
        &self.ctx
    }

    /// This turn as `vessel/session/v1` — a pure read, grouped by epistemic
    /// channel (The Snapshot spec §3). Never commits, never advances the
    /// turn counter, and costs nothing on turns where no caller asks: the
    /// CLI never does, so its measured per-turn cost is unchanged. For a
    /// caller that *does* ask — the Casement, over wasm — the cost is not
    /// nothing: `snapshot() + json` measured 0.173 → 1.249 ms (7.22×), and
    /// the bytes grew per band — walk 4235 → 11582 (2.73×), chamber 4064 →
    /// 4759 (1.17×) (`windows/vessel/examples/turn_cost.rs`).
    ///
    /// This method's failure surface is wider than a per-channel read: the
    /// only error path below is `observable`'s single `VesselError::Build`
    /// (a purview failure), and a whole snapshot fails on it rather than
    /// just the spatial channel. At the ABI, `set_snapshot()` calls
    /// `.and_then(|p| p.session.snapshot().ok())`, so that failure empties
    /// the snapshot buffer and the client falls back to prose — losing
    /// every channel that turn (self, sensed, known, social, structured
    /// narration), not just the map.
    pub fn snapshot(&self) -> Result<SessionSnapshot, VesselError> {
        let vantage = observable(self.world, &self.ctx, &self.agent, self.day)?;
        // The noun catalog comes from the focalizer; the PROSE comes from
        // `last_text` (this turn's real response), not from here.
        let focalized = self.focalizer.render(&vantage);

        // `&self`-only: can read whatever `self.mesh_memo` already holds
        // (Finding 1's cache field is a shared borrow, not a mutation) but
        // cannot prefill it fresh — `wait`'s tick is where that happens.
        let terrain = LocaleTerrain::with_fields(
            &self.ctx,
            self.calendar.as_ref(),
            self.predator.as_ref(),
            self.prey.as_ref(),
            Some(&self.built),
            Some(&self.mesh_memo),
        );
        let mut afraid_memo = PrimaryAfraidMemo::new();
        // A throwaway `RoomMeshMemo` for `affect_of_memo_occupied`'s own
        // `neighbors_memo` write-through (rider (b)): `&self` here cannot
        // reach `&mut self.mesh_memo`, so this specific read does not grow
        // the session-owned memo — it still benefits from `terrain`'s
        // prefilled `corner_weights` cache above, just not from a warm
        // `neighbors` cache of its own.
        let mut mesh_memo = hornvale_kernel::RoomMeshMemo::new();
        // A throwaway `HomeNavCache` (the-waymark, Task 4 fix round): `&self`
        // cannot reach a session-lived one, same as `mesh_memo` above. Unlike
        // `mesh_memo`, this buys no in-call sharing either — the cache is
        // keyed by `EntityId`, so distinct colocated NPCs never share an
        // entry regardless of scope; it is exactly as cheap as the
        // pre-Task-4 unconditional search, never cheaper, for this call.
        let mut home_nav_cache = HomeNavCache::new();
        // The fine layer, derived ONCE per snapshot: `anchor_cells` costs 42 us
        // at the median and 410 us at p99 against this call's own measured
        // 1.249 ms, so a second derivation — or one per creature — would be a
        // budget item rather than noise. `None` out of doors.
        let sighting = self.sighting();
        // SIGHT NARROWS WHAT IS SENT (spec §2.1, `CLIENT-redaction-panes`),
        // and it narrows it HERE — at the roster, before any affect is read —
        // so that this channel and the two verbs that answer about creatures
        // (`needs`, `examine`) share one predicate rather than three
        // reimplementations of it. See `sensed_npcs` for the rule and for the
        // unplaced row, which is the one that is easy to get wrong. Nothing on
        // this path touches `self.knowledge`: that deferral is the whole of
        // §2.1, held by
        // `perturbing_the_embedding_moves_what_is_drawn_and_not_what_is_known`.
        //
        // The species rides along beside the `PresentEntry` because a creature's
        // MARK datum is an identity line (`purview::creature_datum`), not the
        // felt state `present` carries — and `PresentEntry` has no species field.
        let here: Vec<(EntityId, String, PresentEntry)> = self
            .sensed_npcs(sighting.as_ref())
            .iter()
            .map(|npc| {
                let affect = affect_of_memo_occupied(
                    &self.ledger,
                    npc,
                    &self.npcs,
                    self.day,
                    &terrain,
                    &mut afraid_memo,
                    Some(&self.occupancy),
                    &mut mesh_memo,
                    &mut home_nav_cache,
                );
                (
                    npc.entity,
                    npc.species.clone(),
                    PresentEntry {
                        entity: npc.entity.0.get(),
                        label: npc.label.clone(),
                        felt: felt_phrase(&affect),
                    },
                )
            })
            .collect();

        let present: Vec<PresentEntry> = here.iter().map(|(_, _, entry)| entry.clone()).collect();

        // The same shadowcast decides the marks, so the pane and the sensed
        // channel cannot disagree about who is here. `marks` is a strict SUBSET
        // of `present`: a creature is drawn only when it was placed AND lit,
        // which is one of the three rows `sensed_npcs` keeps.
        //
        // `kind`, `datum` and `salience` are the walk-band chart's own
        // (`crate::purview`), not this module's. Fix round 1's finding: the
        // first draft minted a `"creature"` kind and a felt-state datum here, so
        // one creature answered `examine` with two different sentences depending
        // on which side of a doorway the player stood — the exact drift §6
        // forbids, one band lower than The Lintel's jar.
        let marks: Vec<crate::plan::PlanMark> = sighting
            .as_ref()
            .map(|s| {
                here.iter()
                    .filter_map(|(who, species, entry)| {
                        let cell = *s.placed.get(who)?;
                        s.lit.contains(&cell).then(|| crate::plan::PlanMark {
                            x: cell.0,
                            y: cell.1,
                            noun: entry.label.clone(),
                            kind: crate::purview::AGENT_MARK_KIND.to_string(),
                            datum: crate::purview::creature_datum(&entry.label, species),
                            salience: crate::purview::AGENT_SALIENCE,
                        })
                    })
                    .collect()
            })
            .unwrap_or_default();

        let social = self
            .npcs
            .iter()
            .map(|npc| {
                let g = grievance(&self.ledger, npc.entity);
                SocialEntry {
                    entity: npc.entity.0.get(),
                    label: npc.label.clone(),
                    grievance: g,
                    hostile: g >= HOSTILITY_THRESHOLD,
                }
            })
            .collect();

        let entries = self
            .knowledge
            .0
            .iter()
            .map(|(key, value)| KnownEntry {
                key: key.clone(),
                value: value.clone(),
            })
            .collect();

        // The band the possession is in decides the channel. `inside` is the
        // same discriminator `handle`'s `map` arm uses, so pane and verb can
        // never disagree about which band is current — and that is the whole
        // reason this matches on `inside` alone rather than on the three
        // "not out of doors" states the session now carries (`inside`,
        // `submerged`, `underground`). `map`'s arms guard on `inside` too, so
        // the other two fall through to the surface chart in the verb and
        // must fall through here identically or the pane would start showing
        // something the verb refuses to. Adding a band to the session without
        // deciding what the pane shows there is the failure this comment
        // exists to catch: see `SpatialChannel`'s doc.
        let spatial = match self.inside.as_ref() {
            Some(inside) => {
                let chamber = chamber_id(&inside.structure.chambers[inside.at])?;
                // The Lantern's seam, in the order it runs: what the building is
                // made of, what light reaches each cell, and whose eyes are
                // looking. Any one of the three missing is a WITHHOLDING (see
                // `plan::Shading`) — the plan comes back exactly as it did
                // before this campaign rather than carrying an invented colour.
                let fabric = self.fabric_here();
                let light =
                    crate::light::light_field(&inside.lattice, &self.chamber_sources(inside));
                let observer = crate::eyes::resolve(&self.eyes, &self.agent).map(|(o, _)| o);
                let shading = match (observer.as_ref(), fabric.as_ref()) {
                    (Some(observer), Some(fabric)) => Some(crate::plan::Shading {
                        observer,
                        fabric,
                        light: &light,
                    }),
                    _ => None,
                };
                SpatialChannel::Chamber {
                    plan: crate::plan::plan_of(
                        &inside.lattice,
                        inside.at,
                        inside.structure.chambers.len(),
                        chamber,
                        inside.cell,
                        marks,
                        shading.as_ref(),
                    ),
                }
            }
            // `purview(0)` is the same call `map` makes out of doors, at the
            // same zoom, so the pane shows what the verb would have shown.
            None => SpatialChannel::Walk {
                chart: self.purview(0)?,
            },
        };

        Ok(SessionSnapshot {
            schema: SESSION_SCHEMA.to_string(),
            turn: self.turn,
            day: self.day.day,
            me: SelfChannel {
                agent: self.agent.id.0,
                species: self.agent.species.clone(),
                settlement: self.agent.village.name.clone(),
                population: self.agent.village.population,
                room: self
                    .agent
                    .position
                    .pack()
                    // `RoomAddrError` implements `Debug` but not `Display`, so
                    // `{e:?}` is the only rendering available here — the same
                    // choice `windows/locale`'s `LocaleError::Unaddressable`
                    // makes for the identical error type.
                    .map_err(|e| VesselError::Build(format!("{e:?}")))?
                    .0,
            },
            sensed: SensedChannel {
                room: vantage.locale.clone(),
                sky: vantage.sky.clone(),
                present,
            },
            known: KnownChannel { entries },
            social,
            narration: Narration {
                prose: self.last_text.clone(),
                nouns: focalized
                    .nouns
                    .into_iter()
                    .map(|n| NounEntry {
                        noun: n.display,
                        datum: n.datum,
                    })
                    .collect(),
            },
            spatial,
        })
    }

    /// How many `agent-at` facts the session's owned ledger has committed —
    /// zero until the first `wait` (test accessor: T3's day-zero guard).
    /// type-audit: bare-ok(count: return)
    pub fn committed_agent_at_count(&self) -> usize {
        self.ledger.find(AGENT_AT).count()
    }

    /// How many `drank` facts the session's owned ledger has committed —
    /// zero until the first `wait` (test accessor: The Confluence's
    /// on-water settlements can satisfy sustenance without ever committing
    /// an `agent-at`, so `committed_agent_at_count` alone can no longer
    /// stand in for "the world moved").
    /// type-audit: bare-ok(count: return)
    pub fn committed_drank_count(&self) -> usize {
        self.ledger.find(DRANK).count()
    }

    /// How many player disposition-shift facts the session's owned ledger
    /// has committed — zero until the first `provoke`/`soothe` (test
    /// accessor: The First Mark's one-fact-per-act guard).
    /// type-audit: bare-ok(count: return)
    pub fn committed_disposition_count(&self) -> usize {
        self.ledger.find(DISPOSITION_SHIFT).count()
    }

    /// How many `turned-hostile` facts the session's owned ledger has
    /// committed — zero until an NPC (co-located when the grievance was
    /// *earned*, not necessarily when the threshold is crossed) first has
    /// its grievance cross `HOSTILITY_THRESHOLD` on a `wait` tick (test
    /// accessor: The First Mark's one-hop forward integration).
    /// type-audit: bare-ok(count: return)
    pub fn committed_hostility_count(&self) -> usize {
        self.ledger.find(TURNED_HOSTILE).count()
    }

    /// How many facts the session's owned ledger holds, of every predicate —
    /// the count the FRAME-tier guards compare, where the four accessors above
    /// each watch one predicate.
    ///
    /// Total on purpose. A per-predicate accessor can only falsify a commit it
    /// was told to expect, and the claim being guarded here is that walking a
    /// chamber commits **nothing at all** (decision 0069): a new predicate
    /// introduced by a later campaign would slip past `committed_agent_at_count`
    /// and be caught by this.
    /// type-audit: bare-ok(count: return)
    pub fn committed_fact_count(&self) -> usize {
        self.ledger.len()
    }

    /// The possessed agent's own stable identity as a ledger `EntityId` — the
    /// object a hostile NPC's `turned-hostile` fact points at. The `Agent`
    /// struct itself is never committed to the ledger (derived fresh each
    /// session, spec's reversibility rule), but its `AgentId` is a
    /// deterministic, seed-derived `u64` (`mint_flagship`'s stream draw), so
    /// it is a stable, collision-free identity to reference AS an object —
    /// this never asserts the player has facts of their own, only that an
    /// NPC's own fact points at them.
    fn agent_entity(&self) -> EntityId {
        EntityId::new(self.agent.id.0)
            .expect("a minted agent id is a seeded stream draw, never exactly zero")
    }

    /// Would the named co-located NPC be hostile to the player right now
    /// (their grievance fold at or past `HOSTILITY_THRESHOLD`)? A pure read
    /// — never commits anything. `who` resolves exactly as `provoke`/
    /// `soothe` do (`colocated_npc`): empty selects the sole co-located
    /// NPC, else a numeric id or a case-insensitive label substring; an
    /// unresolved (not-here) `who` reads as not-hostile rather than
    /// erroring. This mechanic's whole consequence is Task 3's; this task
    /// stops at the gate.
    ///
    /// "Resolves exactly as `provoke`/`soothe` do" is load-bearing rather than
    /// descriptive, and The Sighting is why: `colocated_npc` is now narrowed by
    /// sight, so a creature the possession cannot see reads as not-hostile here
    /// — the same answer an absent one gives.
    ///
    /// **What that achieves, stated precisely** (fix round 4 corrects fix round
    /// 3's claim). It does NOT make a withheld creature's hostility
    /// undiscoverable: `SessionSnapshot::social` folds `self.npcs` *unfiltered*
    /// and ships `label` + `grievance` + this very `hostile` bool for every
    /// derived NPC, co-located or not. That is pre-existing and deliberately
    /// disclosed — [`crate::snapshot::SocialEntry`]'s own doc says membership is
    /// world truth, that a consumer must filter it, and that rendering it
    /// unfiltered ships a cheat pane — and The Sighting does not touch it.
    ///
    /// What the narrowing achieves is that this method cannot *drift* from the
    /// verbs it documents itself against. A caller reading `would_turn_hostile`
    /// as "is the creature I am about to provoke hostile" would otherwise get
    /// `true` for a creature `provoke` refuses to act on — an answer about a
    /// creature the same session has just declined to reach. One resolution,
    /// one answer.
    /// type-audit: bare-ok(identifier-text: who), bare-ok(flag: return)
    pub fn would_turn_hostile(&self, who: &str) -> bool {
        self.colocated_npc(who)
            .map(|npc| grievance(&self.ledger, npc.entity) >= HOSTILITY_THRESHOLD)
            .unwrap_or(false)
    }

    /// A named NPC's current grievance fold toward the player (test
    /// accessor: the byte-identity guard that an unprovoked NPC's grievance
    /// is exactly zero). Unlike `would_turn_hostile`, this resolves among
    /// ALL derived NPCs, not only co-located ones — grievance is a ledger
    /// fold over that NPC's own facts, not a proximity check — matched by
    /// numeric id or case-insensitive label substring; `None` if no derived
    /// NPC matches `who`.
    /// type-audit: bare-ok(identifier-text: who), bare-ok(diagnostic-value: return)
    pub fn npc_grievance(&self, who: &str) -> Option<f64> {
        who.parse::<u64>()
            .ok()
            .and_then(|id| self.npcs.iter().find(|n| n.entity.0.get() == id))
            .or_else(|| {
                let needle = who.to_lowercase();
                self.npcs
                    .iter()
                    .find(|n| n.label.to_lowercase().contains(&needle))
            })
            .map(|npc| grievance(&self.ledger, npc.entity))
    }

    /// The session's owned, evolving ledger, serialized — a determinism
    /// accessor: same seed + same waits must yield the same bytes (test
    /// accessor: T3's determinism test).
    /// type-audit: bare-ok(artifact: return)
    pub fn session_ledger_json(&self) -> String {
        serde_json::to_string(&self.ledger).expect("a ledger always serializes")
    }

    /// Consume the session and fold its evolved ledger + registry into a
    /// saveable `World` (The First Mark, Task 4: persistence). The evolved
    /// ledger is the bubble's forward integration made history — player
    /// facts, the consequences they triggered, and NPC ticks alike — not
    /// just the player's own marks, so a played world stays an ordinary
    /// `World { seed, registry, ledger }` and every existing tool (almanac,
    /// `why`, map) works over it unchanged. The caller supplies the seed
    /// (the frozen `world` this session possessed is only ever borrowed —
    /// `Session` never owns or mutates it, so there is nothing here to copy
    /// it from); the input world is never mutated in place.
    pub fn into_played_world(self, seed: Seed) -> World {
        World {
            seed,
            registry: self.registry,
            ledger: self.ledger,
            // Carried forward, never invented: the vessel cannot see the
            // composition root's label roster, so it preserves whatever the
            // possessed world recorded and leaves stamping to `cli`.
            derived_under: self.world.derived_under.clone(),
        }
    }

    /// The derived NPCs' labels (test accessor: the T3 review's colocation
    /// test names the specific NPC whose motion narrates in `wait`'s output,
    /// without hardcoding world-generated prose into the test itself).
    /// type-audit: bare-ok(identifier-text: return)
    pub fn npc_labels(&self) -> Vec<&str> {
        self.npcs.iter().map(|n| n.label.as_str()).collect()
    }

    /// The current room, focalized (for the battery's checks).
    pub fn focalized(&self) -> Result<Focalized, VesselError> {
        let v = observable(self.world, &self.ctx, &self.agent, self.day)?;
        Ok(self.focalizer.render(&v))
    }

    /// The lateral exits from here: each compass bearing paired with its
    /// destination's packed room id (three per room, always — the mesh's
    /// base-edge neighbors). For the walker battery's deterministic pick.
    /// type-audit: bare-ok(index: return)
    pub fn ways(&self) -> Vec<(Compass, u64)> {
        let v = observable(self.world, &self.ctx, &self.agent, self.day)
            .expect("the current position is always observable");
        v.locale
            .exits
            .iter()
            .filter(|e| e.kind == ExitKind::Edge)
            .filter_map(|e| match e.direction {
                Direction::Compass(c) => Some((c, e.to)),
                _ => None,
            })
            .collect()
    }

    /// This session's chart, `zoom_out` rungs coarser than the walk depth.
    /// Reads only — the chart never mutates the session.
    ///
    /// # A WALK-BAND read, and the assertion is what makes that true
    ///
    /// The chart marks **every derived NPC** with a noun and a datum, ungated
    /// ([`crate::purview_scene`]) — so calling it while the possession is inside
    /// a chamber would disclose exactly the creature the chamber band has
    /// withheld, straight past four gated verbs (The Sighting, fix round 5).
    ///
    /// Nothing does today, and that was verified rather than assumed: `map`
    /// indoors draws the plan and `map out` refuses; `snapshot`'s `Walk` arm
    /// runs only when `inside` is `None`; `examine` indoors routes to
    /// `examine_chamber`, and the one path that slips past that guard — a BARE
    /// `examine` while inside — returns "Examine what?" before reaching any
    /// chart. But every one of those is a fact about **dispatch**, and this
    /// method is `pub`: a caller that has not read `handle` can reach it from
    /// inside a chamber with nothing to stop them.
    ///
    /// So the precondition is asserted where a future caller would trip it,
    /// rather than stated in a doc a future caller will not read.
    /// `debug_assert!` rather than a hard refusal deliberately: it fires in
    /// every test and debug run — which is where this campaign's coverage lives
    /// — while costing nothing in release and, crucially, not changing a public
    /// `Result` contract that today has no error case for this. A caller who
    /// genuinely wants the walk-band chart from indoors is asking a real
    /// question (what does the land outside look like?) and should get a
    /// deliberate method with a redacted mark list, not a silent pass here.
    /// type-audit: bare-ok(count: zoom_out)
    pub fn purview(&self, zoom_out: u32) -> Result<hornvale_scene::SurroundsScene, VesselError> {
        debug_assert!(
            self.inside.is_none(),
            "the walk-band chart marks every derived NPC ungated, so drawing it \
             from inside a chamber would disclose a creature sight withheld"
        );
        crate::purview_scene(
            self.world,
            &self.ctx,
            &self.agent.position,
            &self.knowledge,
            &self.npcs,
            &self.ledger,
            self.day,
            zoom_out,
            &self.agent,
            &self.eyes,
            self.calendar.as_ref(),
        )
    }

    /// One verb, one response. `Turn::Released` ends the possession.
    /// type-audit: bare-ok(prose: line)
    pub fn handle(&mut self, line: &str) -> Turn {
        let line = line.trim();
        let (verb, rest) = match line.split_once(' ') {
            Some((v, r)) => (v, r.trim()),
            None => (line, ""),
        };
        if !verb.is_empty() {
            self.turn += 1;
        }
        let turn = match verb {
            "" => Turn::Out(String::new()),
            // `look` is the one existing verb that must become band-aware:
            // inside a structure it renders the chamber, out of doors the
            // locale. Everything else reads `self.agent.position`, which never
            // leaves the walk band, so nothing else changes.
            "look" if self.inside.is_some() => self.out(self.describe_chamber_here()),
            "look" if self.submerged.is_some() => self.out(self.describe_here()),
            // Underground (The Deep Realm, Task 5): the chamber lattice's
            // content is read straight from `self.underground`, never
            // through `describe_here`'s locale pipeline — that pipeline's
            // stratum handling (`expr_at_stratum`) is water-specific (a
            // vantage stratum that disagrees with the cell's own substitutes
            // `Formation::OpenWater`), so feeding it a rock `Stratum` would
            // render nonsense rather than a chamber.
            "look" if self.underground.is_some() => Turn::Out(self.describe_underground_here()),
            "look" => self.out(self.describe_here()),
            // `map` is band-aware for exactly the reason `look` is, and it is the
            // SAME verb rather than a new one: §6's contract is that any pane
            // capability must first BE a verb, so the fewer verbs meaning one
            // thing each, the better. Indoors the chart would draw the LOCALE the
            // structure sits in, which is not where the possession is standing. A
            // plan has no coarser rung, so an argument indoors is refused rather
            // than silently ignored — an ignored argument is how a player comes
            // to believe they asked for something and got it.
            "map" if self.inside.is_some() && rest.is_empty() => self.out(self.plan_here()),
            "map" if self.inside.is_some() => Turn::Out(INDOOR_CHART_REFUSAL.to_string()),
            "map" => self.map(rest),
            // Bare `eyes` reports whose eyes the chart is coloured through and
            // what their projection drops; `eyes <name>` switches them (The
            // Beholding, Task 5).
            "eyes" if rest.is_empty() => Turn::Out(self.eyes_report()),
            "eyes" => self.set_eyes(rest),
            // `go` is band-aware for the same reason `look` and `map` are, and
            // this arm is the reversal The Blocking owes The Lintel: indoors a
            // compass bearing means one CELL, not one locale. §1b.6's law is
            // untouched — a cell step stays inside the chamber band — and the
            // guard still matters exactly as much, because without it `go n` from
            // a chamber renders the NEIGHBOURING LOCALE with no sentence
            // acknowledging the building had been left.
            "go" if self.inside.is_some() => self.step(rest),
            // The water column, by contrast, is NOT reversed: it has no lattice
            // to step across, so a bearing under water still has nowhere to go
            // (The Column). Two bands, two answers, one verb.
            "go" if self.submerged.is_some() => Turn::Out(SUBMERGED_LATERAL_REFUSAL.to_string()),
            // The chamber lattice, likewise: this campaign ships only the
            // entrance address, no walkable interior, so a bearing from it
            // has nowhere to mean either (The Deep Realm, Task 5).
            "go" if self.underground.is_some() => {
                Turn::Out(UNDERGROUND_LATERAL_REFUSAL.to_string())
            }
            "go" => self.go(rest),
            // Band-aware, for the same reason `look` is: the outdoor path resolves
            // against the LOCALE's two grains, which know nothing of what stands
            // in a chamber, so it would answer "You see no <noun> here." about a
            // thing the chamber's own prose had just listed. Indoors it resolves
            // against the chamber's anchors and the floor plan's own legend
            // instead — the reversal of `INDOOR_EXAMINE_REFUSAL`, which stated
            // that limit honestly while nothing authored a detail. A BARE
            // `examine` is a different question — the player named nothing — so
            // it still falls through to `examine`'s own "Examine what?" hint,
            // which is as true indoors as out.
            "examine" if self.inside.is_some() && !rest.is_empty() => {
                Turn::Out(self.examine_chamber(rest))
            }
            // The underworld's own band, mirroring the two arms above: a cave
            // chamber's rock is not the surface locale's canopy and forest, so
            // resolving an underground `examine` against `examine`'s own prose
            // catalog is the defect The Handle's Task 4 fixes — it fell through
            // to the bare arm below, which reads the LOCALE overhead, and
            // answered "You see no rock here." about the very rock the descent
            // had just named.
            "examine" if self.underground.is_some() && !rest.is_empty() => {
                Turn::Out(self.examine_underground(rest))
            }
            "examine" => self.examine(rest),
            // `back` retraces the WALK-band trail, so it stays refused where `go`
            // no longer is: the capability this campaign built is intra-chamber
            // GEOMETRY, and a walk-band trail is not geometry.
            "back" if self.inside.is_some() => Turn::Out(INDOOR_BACK_REFUSAL.to_string()),
            "back" if self.submerged.is_some() => Turn::Out(SUBMERGED_LATERAL_REFUSAL.to_string()),
            "back" if self.underground.is_some() => {
                Turn::Out(UNDERGROUND_LATERAL_REFUSAL.to_string())
            }
            "back" => self.back(),
            "wait" => self.wait(rest),
            "whoami" => Turn::Out(self.whoami()),
            "knows" => Turn::Out(self.knows()),
            "npcs" => Turn::Out(self.list_npcs()),
            "why" => Turn::Out(self.why(rest)),
            "needs" => Turn::Out(self.needs()),
            "provoke" => self.act_on_disposition(rest, 1),
            "soothe" => self.act_on_disposition(rest, -1),
            "write" => Turn::Out(self.write(rest)),
            "consult" => Turn::Out(self.consult()),
            "dive" => self.dive(),
            "surface" => self.surface(),
            "delve" => self.delve(),
            "climb" => self.climb(),
            "enter" => self.enter(rest),
            "out" => self.leave(),
            // Coarse-ward is still refused: possessing a settlement, a culture
            // or a civilization is a deferred arc of its own (0077). This
            // sentence is byte-pinned in the galleries — do not reword it.
            "exit" => Turn::Out(
                "The grain of the world resists; that way lies another scale of things."
                    .to_string(),
            ),
            "help" => Turn::Out(HELP.to_string()),
            "release" | "quit" => Turn::Released("You let go.".to_string()),
            // A bare compass token IS a movement command. The room prints
            // "Ways on: SE, N, SW." and every one of those tokens must be
            // typeable; `parse_compass` already accepted them, and only this
            // dispatch arm was missing.
            //
            // It carries `go`'s own band guards, and must: this arm dispatches
            // to `self.go` directly, so without them repeated here a bare `n`
            // typed inside a structure would slip past the band split that
            // `"go" if self.inside.is_some()` exists to make, and silently
            // render the neighbouring locale from indoors. Indoors it therefore
            // means what `go n` means indoors — one CELL of the floor plan, and
            // `step` refuses a bare diagonal with the geometry as the reason.
            other if self.inside.is_some() && parse_compass(other).is_some() => self.step(other),
            other if self.submerged.is_some() && parse_compass(other).is_some() => {
                Turn::Out(SUBMERGED_LATERAL_REFUSAL.to_string())
            }
            other if self.underground.is_some() && parse_compass(other).is_some() => {
                Turn::Out(UNDERGROUND_LATERAL_REFUSAL.to_string())
            }
            other if parse_compass(other).is_some() => self.go(other),
            other => Turn::Out(format!("No verb '{other}' ('help' lists them).")),
        };
        if !verb.is_empty() {
            self.last_text = match &turn {
                Turn::Out(s) | Turn::Released(s) => s.clone(),
            };
        }
        turn
    }

    /// The water column at the room the possession stands on, shallowest
    /// first; empty on land.
    fn column_here(&self) -> Vec<hornvale_climate::Stratum> {
        let Ok(v) =
            crate::vantage::observable_at(self.world, &self.ctx, &self.agent, self.day, None)
        else {
            return Vec::new();
        };
        let Some(cw) = v.locale.corners.iter().max_by_key(|c| c.weight) else {
            return Vec::new();
        };
        self.ctx.water_column_at(hornvale_kernel::CellId(cw.cell))
    }

    /// The cave at the cell the possession stands on, if the terrain places
    /// one there — mirrors `column_here`: both resolve the same fuzzy
    /// corner-weighted cell under the possession and ask "is there a medium
    /// here to descend into," one for water, one for rock. `None` on a cell
    /// with no cave, or before terrain built at all.
    ///
    /// Returns the resolved [`hornvale_kernel::CellId`] alongside the cave
    /// rather than the bare `Cave` `column_here` analogy would suggest:
    /// addressing a chamber (`ChamberAddr`) needs the cell, where a water
    /// stratum needs no address at all, so the caller needs both.
    fn chamber_column_here(&self) -> Option<(hornvale_kernel::CellId, hornvale_terrain::Cave)> {
        let terrain = self.terrain.as_ref()?;
        let v = crate::vantage::observable_at(self.world, &self.ctx, &self.agent, self.day, None)
            .ok()?;
        let cw = v.locale.corners.iter().max_by_key(|c| c.weight)?;
        let cell = hornvale_kernel::CellId(cw.cell);
        terrain.cave_at(cell).map(|cave| (cell, cave))
    }

    /// Descend one layer of the water column.
    ///
    /// The column's depth is the sea floor's: fifty metres over a reef holds
    /// only the sunlit water, three thousand holds three layers. Diving past
    /// the floor is refused by naming the floor, because "you cannot" without
    /// saying what stopped you reads as a parse failure rather than the bottom
    /// of the sea.
    fn dive(&mut self) -> Turn {
        if self.inside.is_some() {
            return Turn::Out("There is no water in here.".to_string());
        }
        let column = self.column_here();
        if column.is_empty() {
            return Turn::Out("There is no water here to go down into.".to_string());
        }
        let next = match self.submerged {
            None => Some(column[0]),
            Some(at) => column
                .iter()
                .position(|s| *s == at)
                .and_then(|i| column.get(i + 1).copied()),
        };
        match next {
            Some(st) => {
                self.submerged = Some(st);
                self.out(self.describe_here())
            }
            None => Turn::Out(format!(
                "You are already as deep as this water goes; the floor is {}.",
                stratum_word(*column.last().expect("a non-empty column has a last"))
            )),
        }
    }

    /// Rise one layer; at the top of the column, break the surface.
    fn surface(&mut self) -> Turn {
        let Some(at) = self.submerged else {
            return Turn::Out("You are already at the surface.".to_string());
        };
        let column = self.column_here();
        let above = column
            .iter()
            .position(|s| *s == at)
            .filter(|i| *i > 0)
            .and_then(|i| column.get(i - 1).copied());
        self.submerged = above;
        let breaking = above.is_none();
        match self.describe_here() {
            Ok(d) if breaking => Turn::Out(format!("You break the surface.\n{d}")),
            other => self.out(other),
        }
    }

    /// Descend into the cave at this cell's entrance chamber (The Deep
    /// Realm, Task 5).
    ///
    /// Mirrors `dive`, but the chamber lattice has a THIRD outcome `dive`
    /// never needed. Task 3 measured that even where a cave exists, its own
    /// entrance address (`band = 0, slot = 0`) resolves to an actual chamber
    /// only 51.5% of the time — spec §3.4 rung 0, `Sealed`: "the void exists
    /// and is unreachable," a real chamber a later dig could find, not a
    /// defect. `dive`'s own doc warns what happens when a refusal doesn't
    /// name what stopped you: it reads as a parse failure rather than a fact
    /// about the world. So each of the three outcomes below is named:
    ///   1. no cave at this cell at all — say so;
    ///   2. a cave, but its entrance resolves to nothing — say it is
    ///      SEALED, not that there is simply nothing here;
    ///   3. a chamber — descend, and say what the rock here is.
    fn delve(&mut self) -> Turn {
        if self.inside.is_some() {
            return Turn::Out("There is no rock to delve into in here.".to_string());
        }
        if self.underground.is_some() {
            return Turn::Out(
                "You are already below; 'climb' brings you back up first.".to_string(),
            );
        }
        let Some((cell, cave)) = self.chamber_column_here() else {
            return Turn::Out("There is no cave here to delve into.".to_string());
        };
        self.delve_at(cell, cave)
    }

    /// The outcome of delving at a KNOWN cell and cave — split out of
    /// [`Self::delve`] so the sealed-vs-open decision can be exercised
    /// directly against a hand-picked cell (this campaign's own unit
    /// coverage) without steering the possession there first. Steering is
    /// impractical to do from a test: `chamber_column_here` resolves the
    /// possession's terrain cell through the same fuzzy corner-weighted walk-
    /// band lookup `column_here` uses, and a terrain cell spans many, many
    /// walk-band rooms, so hitting one particular cell (let alone one with a
    /// SEALED cave specifically, ~48.5% of caves per Task 3's measurement)
    /// by walking is not something a test should depend on landing.
    fn delve_at(&mut self, cell: hornvale_kernel::CellId, cave: hornvale_terrain::Cave) -> Turn {
        let addr = hornvale_worldgen::chamber::ChamberAddr {
            cell,
            entrance: 0,
            band: 0,
            slot: 0,
        };
        let overrides = hornvale_worldgen::chamber::ChamberOverrides::new();
        match hornvale_worldgen::chamber::chamber_at(self.world.seed, &cave, addr, &overrides) {
            None => Turn::Out(
                "The cave mouth is here, but the rock beyond is sealed; there is no way down."
                    .to_string(),
            ),
            Some(chamber) => {
                self.underground = Some(chamber);
                Turn::Out(format!(
                    "You worm down into the dark. The rock here is {}.",
                    stratum_word(chamber.stratum)
                ))
            }
        }
    }

    /// Return to the surface from the chamber lattice — `delve`'s inverse,
    /// mirroring `surface`. This campaign's lattice reaches only the
    /// entrance address, so unlike `surface` there is no intermediate layer
    /// to rise through: any descent climbs out in one step.
    fn climb(&mut self) -> Turn {
        if self.underground.take().is_none() {
            return Turn::Out(
                "You are not underground; there is nothing to climb out of.".to_string(),
            );
        }
        match self.describe_here() {
            Ok(d) => Turn::Out(format!("You climb back into the light.\n{d}")),
            other => self.out(other),
        }
    }

    /// The chamber rendering while underground (The Deep Realm, Task 5) —
    /// deliberately minimal, in `describe_chamber_here`'s spirit one realm
    /// over: this campaign ships no interior lattice for a cave the way a
    /// structure has one, only the entrance address, so there is no floor
    /// plan or anchor catalogue to draw from. Read straight off
    /// `self.underground` rather than re-deriving through `chamber_at` —
    /// re-deriving would be pure and would agree, but there is nothing to
    /// gain by paying for it a second time.
    fn describe_underground_here(&self) -> String {
        let chamber = self
            .underground
            .expect("guarded by self.underground.is_some() at the call site");
        format!(
            "[underground]\nThe rock here is {}. Ways on: out.",
            stratum_word(chamber.stratum)
        )
    }

    /// The underworld's examinable catalog. The band has its own because you
    /// cannot see the forest from inside the rock — resolving an underground
    /// `examine` against the surface locale's nouns is the defect this fixes
    /// (The Handle, Task 4).
    fn underground_nouns(&self) -> Vec<crate::focalize::Noun> {
        let chamber = self
            .underground
            .expect("guarded by self.underground.is_some() at the call site");
        let stratum = stratum_word(chamber.stratum);
        vec![
            crate::focalize::Noun::new("the rock", "rock", &format!("The rock here is {stratum}.")),
            crate::focalize::Noun::new(
                stratum,
                stratum,
                &format!("{stratum} — the rock of this chamber."),
            ),
        ]
    }

    /// `examine <noun>` UNDERGROUND: the band's own catalog only — never the
    /// surface locale's, which is the defect The Handle's Task 4 fixes. The
    /// refusal is BYTE-IDENTICAL to the outdoor and chamber paths' (§6):
    /// two wordings for one question is exactly the drift this campaign
    /// exists to remove.
    fn examine_underground(&self, noun: &str) -> String {
        let wanted = noun.trim().to_lowercase();
        match self.underground_nouns().iter().find(|n| n.matches(&wanted)) {
            Some(n) => n.datum.clone(),
            None => format!("You see no {noun} here."),
        }
    }

    /// Absorb the current room's projection into knowledge.
    fn absorb_here(&mut self) -> Result<(), VesselError> {
        let v = observable(self.world, &self.ctx, &self.agent, self.day)?;
        self.knowledge
            .absorb(self.projection.project(&v, &self.agent.perception));
        Ok(())
    }

    /// The full room rendering: room id, prose, ways on.
    fn describe_here(&self) -> Result<String, VesselError> {
        // Unsubmerged over water, the possession is AFLOAT — on the surface,
        // not down among whatever lives on the floor. Rendering the cell's own
        // expression there would put a walker "in" a coral reef while they are
        // still a thousand metres above it, which is the distinction the depth
        // band exists to draw.
        let vantage = match self.submerged {
            Some(st) => Some(st),
            None if !self.column_here().is_empty() => Some(hornvale_climate::Stratum::Surface),
            None => None,
        };
        let v =
            crate::vantage::observable_at(self.world, &self.ctx, &self.agent, self.day, vantage)?;
        let f = self.focalizer.render(&v);
        let ways: Vec<String> = v
            .locale
            .exits
            .iter()
            .filter(|e| e.kind == ExitKind::Edge)
            .filter_map(|e| match e.direction {
                Direction::Compass(c) => Some(format!("{c:?}").to_uppercase()),
                _ => None,
            })
            .collect();
        Ok(format!(
            "[room {}, day {}]\n{}\nWays on: {}.",
            v.locale.id,
            self.day.day,
            f.prose,
            ways.join(", ")
        ))
    }

    /// A lateral step at the walk band. Reached only out of doors: `handle`
    /// routes `go` to [`Self::step`] while `inside` is set, which is why nothing
    /// here clears `inside` — a structure belongs to ONE locale, so a step that
    /// carried `inside` across would leave the session holding chambers descending
    /// from the locale behind it, and the guard is what makes that unrepresentable
    /// rather than merely tidied up afterwards. That guard is UNCHANGED by The
    /// Blocking's reversal: what changed is what a bearing MEANS indoors, not
    /// whether a walk-band step can happen from a chamber (it cannot).
    fn go(&mut self, dir: &str) -> Turn {
        let Some(wanted) = parse_compass(dir) else {
            return Turn::Out(format!("Go where? '{dir}' is no direction I know."));
        };
        let v = match observable(self.world, &self.ctx, &self.agent, self.day) {
            Ok(v) => v,
            Err(e) => return Turn::Out(format!("error: {e}")),
        };
        let exit = v
            .locale
            .exits
            .iter()
            .find(|e| e.kind == ExitKind::Edge && e.direction == Direction::Compass(wanted));
        let Some(exit) = exit else {
            return Turn::Out(format!("No way {} from here.", dir.to_lowercase()));
        };
        // Lateral exits stay at walk depth: the destination is the
        // neighbor whose packed id the exit names.
        let dest = self
            .agent
            .position
            .neighbors()
            .into_iter()
            .find(|n| n.pack().map(|r| r.0) == Ok(exit.to));
        let Some(dest) = dest else {
            return Turn::Out("error: exit names no neighbor".to_string());
        };
        let from = std::mem::replace(&mut self.agent.position, dest);
        self.trail.push(from);
        if let Err(e) = self.absorb_here() {
            return Turn::Out(format!("error: {e}"));
        }
        self.out(self.describe_here())
    }

    /// Retrace one step of the walk-band trail. Like [`Self::go`], reached only
    /// out of doors: the trail holds walk-band addresses, so retracing it is a
    /// walk-band operation and `handle` refuses it indoors.
    fn back(&mut self) -> Turn {
        let Some(prev) = self.trail.pop() else {
            return Turn::Out("You have not walked anywhere yet.".to_string());
        };
        self.agent.position = prev;
        if let Err(e) = self.absorb_here() {
            return Turn::Out(format!("error: {e}"));
        }
        self.out(self.describe_here())
    }

    /// Descend into the structure at this locale, or move to a named chamber
    /// within the one already entered. Apertures, not stairs (§7): movement
    /// inside is by name, never by compass, because a chamber address is
    /// identity and carries no bearing.
    fn enter(&mut self, target: &str) -> Turn {
        // Already inside: `enter <named>` steps through an aperture.
        if let Some((structure, at)) = self.inside.as_ref().map(|i| (i.structure.clone(), i.at)) {
            let Some(next) = self.named_neighbour(&structure, at, target) else {
                // Asked for the deeper way where there is none: say which wall
                // was reached, not "no way to further in", which reads as a
                // parse failure rather than the end of the place.
                if FURTHER_IN_WORDS.contains(&target.trim().to_lowercase().as_str()) {
                    return Turn::Out("This is as far in as the place goes.".to_string());
                }
                // A CHOICE of apertures is never "no way" — it is an unanswered
                // question. Both refusals that land here mean the same thing:
                // an empty target (`named_neighbour` only defaults with exactly
                // one neighbour) or a prose noun, which cannot tell two
                // prose-identical chambers apart. Denying the ways exist would
                // be as false here as "no way to anywhere" was.
                let neighbours = Self::neighbours(&structure, at);
                if neighbours.len() > 1 {
                    // Count-aware rather than hard-coded: `structure_at` builds
                    // a path graph, so today every such chamber has exactly two
                    // apertures — but a richer topology (The Precincts) would
                    // make a fixed "two" a lie told to a real player, and a
                    // debug-only assertion would not catch it in release.
                    let how_many = match neighbours.len() {
                        2 => "two ways".to_string(),
                        n => format!("{n} ways"),
                    };
                    return Turn::Out(format!(
                        "There are {how_many} from here; say 'further in' to go deeper, \
                         or 'out' to leave."
                    ));
                }
                return Turn::Out(format!(
                    "There is no way to {} from here.",
                    if target.is_empty() {
                        "anywhere"
                    } else {
                        target
                    }
                ));
            };
            // Through an aperture, so the arrival cell is the far side of the
            // doorway that realizes this link — not the middle of the room. A
            // player who walks through a door is standing just inside it, and the
            // drawn mark then reads as the step they just took.
            let lattice = self.lattice_of(&structure);
            let through = crate::lattice::doorway_between(&lattice, at, next);
            let Some(cell) = through
                .and_then(|t| crate::lattice::cell_beyond(&lattice, t, next))
                .or_else(|| crate::lattice::standing_cell(&lattice, next))
            else {
                // §7 rule 1 already reports a chamber with no floor as the defect
                // it is; say so rather than panicking in a player's hands.
                return Turn::Out("error: that chamber has no floor to stand in".to_string());
            };
            let seed = self.frame_seed(&structure);
            self.inside = Some(Inside {
                structure,
                at: next,
                lattice,
                cell,
                seed,
            });
            return self.out(self.describe_chamber_here());
        }
        let brief = self.brief_here();
        let Some(structure) = crate::structure::structure_at(
            &crate::band::truncate_to_walk(&self.agent.position, self.walk_depth()),
            &brief,
            self.world.seed,
            self.walk_depth(),
        ) else {
            return Turn::Out("Nothing here is built; there is nothing to enter.".to_string());
        };
        let at = structure
            .chambers
            .iter()
            .position(|c| *c == structure.threshold)
            .expect("the threshold is one of the chambers");
        // In off the street: no doorway was crossed, so the arrival cell is the
        // threshold chamber's own standing cell.
        if self.descend(structure, at).is_none() {
            return Turn::Out("error: that chamber has no floor to stand in".to_string());
        }
        self.out(self.describe_chamber_here())
    }

    /// Put the possession inside `structure` at chamber `at`, standing wherever
    /// that chamber's own geometry says an arrival stands. `None` if the chamber
    /// holds no floor — §7 rule 1 reports that as the defect it is, so the caller
    /// refuses rather than standing the player in a wall.
    ///
    /// The one place an [`Inside`] is built from a structure ALONE: `enter` uses it
    /// for the descent from out of doors, where no doorway was crossed, and the
    /// tests use it to put a session inside a hand-built structure without
    /// replicating the derivation and drifting from it.
    fn descend(&mut self, structure: crate::structure::Structure, at: usize) -> Option<()> {
        let lattice = self.lattice_of(&structure);
        let cell = crate::lattice::standing_cell(&lattice, at)?;
        let seed = self.frame_seed(&structure);
        self.inside = Some(Inside {
            structure,
            at,
            lattice,
            cell,
            seed,
        });
        Some(())
    }

    /// A compass step INDOORS: one cell, in the bearing named.
    ///
    /// **The reversal of The Lintel's indoor `go` refusal**, and it is narrower
    /// than it sounds. Metaplan §1b.6's law — lateral movement never changes band
    /// — is untouched here: a cell step stays inside the chamber band, and the
    /// possessed agent's own walk-band `position` is not read or written by this
    /// function at all. What The Lintel got wrong was the INFERENCE it drew, that
    /// a chamber carries no bearing to walk along. True while a chamber had no
    /// interior; this campaign gave it one.
    ///
    /// Four things happen here and the order matters:
    ///
    /// 1. **A diagonal is refused** ([`INDOOR_DIAGONAL_REFUSAL`]) before anything
    ///    is looked up: `HEADINGS` is orthogonal, and slipping through the corner
    ///    where two walls meet is not a way through a building.
    /// 2. **An impassable target is refused with a physical reason.** Asked as
    ///    `passable()`, never as `== CellKind::Wall`, so the refusal survives the
    ///    day a `Rubble` cell arrives — the plan's constraint, not a preference.
    /// 3. **A threshold crossing changes CHAMBER**, and renders the new chamber in
    ///    full rather than reporting a cell move. It is a band-ish step in the
    ///    same sense `enter` is, so it gets `enter`'s answer; and the possession
    ///    lands BESIDE the doorway rather than in it (see
    ///    [`crate::lattice::cell_beyond`]).
    /// 4. **Otherwise the cell moves and the answer is brief.** A full chamber
    ///    description on every step would bury a transcript in repetitions of one
    ///    room's prose, so the step says what changed and what is now adjacent.
    fn step(&mut self, dir: &str) -> Turn {
        let Some(wanted) = parse_compass(dir) else {
            return Turn::Out(format!("Go where? '{dir}' is no direction I know."));
        };
        let Some(delta) = cell_delta(wanted) else {
            return Turn::Out(INDOOR_DIAGONAL_REFUSAL.to_string());
        };
        let Some(inside) = self.inside.as_ref() else {
            // Unreachable through `handle` (the arm checks first), the same guard
            // and the same reason as `plan_here`'s.
            return Turn::Out(
                "error: no chamber to step in: the possession is out of \
                              doors"
                    .to_string(),
            );
        };
        let target = crate::lattice::Cell(inside.cell.0 + delta.0, inside.cell.1 + delta.1);
        let kind = crate::lattice::kind_of(&inside.lattice, target);
        // `None` is outside the extent, which §7 rule 3(i) makes unreachable from
        // a passable cell — the outer ring is entirely `Wall`. Folded in with the
        // impassable case rather than given its own sentence, exactly as
        // `render::glyph` draws it as fabric and for the same reason: a cell the
        // map does not hold is not a cell a mover may enter.
        if !kind.is_some_and(|k| k.passable()) {
            return Turn::Out(format!(
                "A wall stands {} of you; there is no way through it.",
                bearing_word(wanted)
            ));
        }
        // A threshold whose far side is another chamber: this is a crossing, so it
        // answers as `enter` does. `serves(at)` is what distinguishes it from the
        // doorway back into the room already stood in.
        if let Some(crate::lattice::CellKind::Threshold(a, b)) = kind {
            let here = inside.at;
            let next = if a == here { b } else { a };
            if next != here {
                let Some(cell) = crate::lattice::cell_beyond(&inside.lattice, target, next) else {
                    return Turn::Out("error: that doorway opens on no floor at all".to_string());
                };
                let inside = self.inside.as_mut().expect("checked above");
                inside.at = next;
                inside.cell = cell;
                return self.out(self.describe_chamber_here());
            }
        }
        let inside = self.inside.as_mut().expect("checked above");
        inside.cell = target;
        Turn::Out(format!(
            "You step {}. {}",
            bearing_word(wanted),
            self.ways_from_cell()
        ))
    }

    /// What is adjacent to the cell stood in, as one brief clause.
    ///
    /// Bearings, not glyphs: a player who has just stepped wants to know where
    /// they may step next, and the plan is there for the shape of the room. Walls
    /// are named by their ABSENCE from the list rather than listed — partly
    /// because listing them is noise in a room with three of them, and partly
    /// because it keeps the word "wall" a REFUSAL word, which is what
    /// `go_indoors_moves_one_cell_and_says_where_you_are` discriminates on.
    ///
    /// A doorway is called out separately, in the render's own legend words
    /// ([`crate::lattice::render::DOORWAY_NOUN`]), because stepping into it
    /// changes chamber — and because a player who reads `a doorway` here can type
    /// exactly that at `examine`.
    fn ways_from_cell(&self) -> String {
        let Some(inside) = self.inside.as_ref() else {
            return String::new();
        };
        let mut open = Vec::new();
        let mut doors = Vec::new();
        for wanted in COMPASS_SQUARE {
            let delta = cell_delta(wanted).expect("COMPASS_SQUARE is orthogonal");
            let target = crate::lattice::Cell(inside.cell.0 + delta.0, inside.cell.1 + delta.1);
            match crate::lattice::kind_of(&inside.lattice, target) {
                Some(crate::lattice::CellKind::Threshold(_, _)) => {
                    doors.push(bearing_letter(wanted))
                }
                Some(k) if k.passable() => open.push(bearing_letter(wanted)),
                _ => {}
            }
        }
        let mut out = if open.is_empty() {
            "No way on but back the way you came.".to_string()
        } else {
            format!("Ways on: {}.", open.join(", "))
        };
        // One clause per doorway rather than a joined list, so the sentence stays
        // grammatical however many a cell happens to touch, and the noun stays
        // verbatim rather than being capitalized into a second wording.
        for d in doors {
            out.push_str(&format!(
                " There is {} to the {d}.",
                crate::lattice::render::DOORWAY_NOUN
            ));
        }
        out
    }

    /// Step back out of doors: `out` leaves the STRUCTURE, not one chamber —
    /// there is no chamber trail, so it returns to the locale from wherever
    /// inside the possession had got to. (A "back one chamber" step would want
    /// its own trail, the way `back` has one for the walk band; nothing asks
    /// for it yet, and inventing an unused one would be a second thing to keep
    /// correct.) Already out of doors, it says so rather than erroring.
    fn leave(&mut self) -> Turn {
        match self.inside.take() {
            None => Turn::Out("You are already out of doors.".to_string()),
            Some(_) => self.out(self.describe_here()),
        }
    }

    /// The world's walk depth, as this session's locale context defines it.
    /// A free function in `agent`, wrapped here so the handlers read as
    /// session state rather than as a module path.
    /// type-audit: bare-ok(count: return)
    fn walk_depth(&self) -> u32 {
        crate::agent::walk_depth(&self.ctx)
    }

    /// The terrain provider, built exactly as every other reader in this module
    /// builds it. NOT `LocaleTerrain::new`: that leaves `built: None`, which
    /// reads as *everything unbuilt*, and `enter` would then report nothing
    /// built anywhere.
    fn terrain_here(&self) -> LocaleTerrain<'_> {
        LocaleTerrain::with_fields(
            &self.ctx,
            self.calendar.as_ref(),
            self.predator.as_ref(),
            self.prey.as_ref(),
            Some(&self.built),
            // `&self`-only reader: shares whatever `self.mesh_memo` already
            // holds (free — no mutation), same posture as `snapshot`.
            Some(&self.mesh_memo),
        )
    }

    /// The brief for wherever the possession currently stands.
    fn brief_here(&self) -> crate::brief::Brief {
        let terrain = self.terrain_here();
        crate::brief::brief_of(
            self.world,
            self.ctx.climate().geosphere(),
            self.ctx.nearest_index(),
            &self.agent.position,
            &terrain,
            self.walk_depth(),
        )
    }

    /// The chambers one aperture away from `at`, in `links` order. Undirected:
    /// a link names its pair either way round.
    ///
    /// `structure_at` builds a PATH GRAPH rooted at `chambers[0]`, the
    /// threshold, so index order is depth order and a chamber has at most two
    /// neighbours: one back toward the threshold and one further in. Both
    /// [`Self::further_in`] and the ways-on footer rely on that ordering, but
    /// read it out of `links` rather than assuming `at ± 1` exists.
    fn neighbours(structure: &crate::structure::Structure, at: usize) -> Vec<usize> {
        structure
            .links
            .iter()
            .filter_map(|&(a, b)| {
                if a == at {
                    Some(b)
                } else if b == at {
                    Some(a)
                } else {
                    None
                }
            })
            .collect()
    }

    /// The aperture leading DEEPER from `at`: the lowest-numbered neighbour
    /// above it. The backward aperture needs no name — `out` already walks that
    /// direction — so this is the only one the footer advertises.
    fn further_in(structure: &crate::structure::Structure, at: usize) -> Option<usize> {
        Self::neighbours(structure, at)
            .into_iter()
            .filter(|&n| n > at)
            .min()
    }

    /// Resolve `target` to a chamber one aperture away.
    ///
    /// Two accepted forms, and the split between them is what makes every
    /// chamber reachable:
    ///
    /// 1. A [`FURTHER_IN_WORDS`] token — the DIRECTION, always unambiguous, and
    ///    the one the footer names. Repeating it walks the path graph to its far
    ///    end, so no chamber is stranded.
    /// 2. A case-insensitive substring of the destination's own PROSE nouns
    ///    (`chamber_nouns`, the same catalogue `describe_chamber` renders from),
    ///    accepted ONLY where the chamber has exactly one aperture. Task 6 made
    ///    chambers differ, so noun lists now *sometimes* tell two apertures apart
    ///    — but not reliably: every role's prose names a doorway, so `enter
    ///    doorway` with two apertures open is still ambiguous. The restriction is
    ///    kept rather than relaxed, because matching with a choice still open
    ///    would silently pick a direction the player never named on exactly the
    ///    nouns the roles happen to share.
    ///
    /// An empty `target` takes the sole neighbour, if there is exactly one; with
    /// a choice to make, silence is not an answer.
    fn named_neighbour(
        &self,
        structure: &crate::structure::Structure,
        at: usize,
        target: &str,
    ) -> Option<usize> {
        let neighbours = Self::neighbours(structure, at);
        let target = target.trim().to_lowercase();
        if target.is_empty() {
            return match neighbours.as_slice() {
                [only] => Some(*only),
                _ => None,
            };
        }
        if FURTHER_IN_WORDS.contains(&target.as_str()) {
            return Self::further_in(structure, at);
        }
        let [only] = neighbours.as_slice() else {
            return None;
        };
        let terrain = self.terrain_here();
        let brief = self.brief_here();
        crate::chamber_prose::chamber_nouns(&crate::interior::chamber_interior_of(
            &structure.chambers[*only],
            &terrain,
            self.walk_depth(),
            &brief,
            *only,
        ))
        .iter()
        .any(|noun| noun.to_lowercase().contains(&target))
        .then_some(*only)
    }

    /// The chamber rendering, in `describe_here`'s own shape one band down:
    /// address, prose, ways on. `[chamber …]` rather than `[room …]` because
    /// the band word IS the information — an id at depth 21 is not a locale.
    ///
    /// The ways are `out`, plus `further in` where a deeper chamber exists.
    /// Naming apertures by DIRECTION rather than by what lies through them is
    /// what makes the list navigable, and Task 6 did NOT change that: chambers
    /// now differ, but the two apertures of a middle chamber lead to a chamber
    /// nearer the door and one further in, and *both* of those are rooms whose
    /// prose names a doorway. So a noun-named aperture list would still advertise
    /// one way where two exist — which is how the deeper chambers became
    /// unreachable under The Lintel, where the reason was starker (every chamber
    /// derived the identical interior).
    fn describe_chamber_here(&self) -> Result<String, VesselError> {
        let Some(inside) = self.inside.as_ref() else {
            // Unreachable through `handle` (every caller checks first), but a
            // silent fabrication of chamber prose while out of doors would be
            // worse than a loud error.
            return Err(VesselError::Build(
                "no chamber to describe: the possession is out of doors".to_string(),
            ));
        };
        let (structure, at) = (&inside.structure, inside.at);
        let chamber = &structure.chambers[at];
        let terrain = self.terrain_here();
        let brief = self.brief_here();
        let interior =
            crate::interior::chamber_interior_of(chamber, &terrain, self.walk_depth(), &brief, at);
        let id = chamber_id(chamber)?;
        let mut ways = vec!["out"];
        if Self::further_in(structure, at).is_some() {
            ways.push(FURTHER_IN);
        }
        Ok(format!(
            "[chamber {}, day {}]\n{}\nWays on: {}.",
            id,
            self.day.day,
            crate::chamber_prose::describe_chamber(&interior, &brief),
            ways.join(", ")
        ))
    }

    /// Derive `structure`'s floor plan.
    ///
    /// The lattice is `FRAME`-tier (decision 0069): derived from the place, kept
    /// only for as long as the possession is standing in it ([`Inside`]), and never
    /// serialized. One call costs microseconds — **~9 us median in release at the
    /// worst 19x19 extent, 174.6 us in debug** (`lattice::tests::
    /// the_embedding_is_cheap_enough_to_re_derive`, measured in BOTH profiles in
    /// Task 5 because Task 4b reported its figure without naming one and it was
    /// compared against a debug number) — so this is cheap enough that `enter`
    /// simply calls it, and cheap enough that a future caller re-deriving it per
    /// turn would still be correct.
    ///
    /// Takes the structure as an argument rather than reading `self.inside`,
    /// because `enter` needs a plan for a structure it has not yet descended into
    /// — and because a derivation that reads no session state is a derivation a
    /// test can pin against the place alone.
    ///
    /// **Keyed to the LOCALE's own seed, never the world's.** `structure_at` keys
    /// its draw with `locale.seed(seed)` (`structure.rs`) precisely so no other
    /// locale's draw can perturb it, and the plan of that structure has to be
    /// keyed the same way. Keyed to `self.world.seed` instead, every building in
    /// the world gets ONE identical floor plan: a world that is self-consistent,
    /// satisfies all eight of `lattice::classify`'s rules, and is uniformly
    /// wrong. `the_plan_is_keyed_to_the_locale_not_the_world` is what catches it.
    ///
    /// The locale is taken from the THRESHOLD rather than from the chamber stood
    /// in. Every chamber of a structure truncates to the same walk-band locale,
    /// so the two agree — but reading it off the threshold says out loud that the
    /// plan is a property of the STRUCTURE, and so does not change as the
    /// possession walks deeper into it.
    fn lattice_of(&self, structure: &crate::structure::Structure) -> crate::lattice::Lattice {
        let brief = self.brief_here();
        crate::lattice::embed_with(
            structure,
            &brief,
            crate::lattice::extent_for(structure),
            self.frame_seed(structure),
        )
    }

    /// The seed every FRAME-tier derivation of `structure` is drawn from: the
    /// locale's own seed, read off the THRESHOLD for the reason [`Self::lattice_of`]
    /// gives (a plan is a property of the structure, not of how deep into it you
    /// have walked).
    ///
    /// A named derivation with two callers rather than an expression inlined
    /// twice: [`Self::lattice_of`] embeds the cells with it and
    /// [`crate::lattice::anchor_cells`] places anchors into those same cells with
    /// it, and a placement keyed differently from the plan it is placed into would
    /// be a silent second world.
    fn frame_seed(&self, structure: &crate::structure::Structure) -> Seed {
        crate::band::truncate_to_walk(&structure.threshold, self.walk_depth()).seed(self.world.seed)
    }

    /// The ground the building the possession stands in is built from (The
    /// Lantern, spec §3), or `None` above the canonical grid.
    ///
    /// **One context for the whole structure**: a building sits on one cell of
    /// the geosphere, so its stone comes from one bedrock however many chambers
    /// it has.
    ///
    /// The cell is `brief::containing_cell`'s — greatest blend weight, tie-broken
    /// to the lowest `CellId` — which is the SAME rule `brief_of` selects the
    /// building's own brief with and the same one `hornvale_locale`'s
    /// `dominant_corner` takes a room's biome, water and substrate from. Shared,
    /// never re-derived: a caption that says granite over a picture drawn in
    /// basalt grey is the failure `fabric.rs`'s module doc exists to prevent.
    ///
    /// Read through `self.ctx` rather than `self.terrain`/`self.climate` for the
    /// same reason — the locale context is what described this room, so the
    /// fabric and the prose read one world.
    fn fabric_here(&self) -> Option<crate::fabric::FabricContext> {
        // The possession's own position is already walk-band (`Inside` records
        // descent, `Agent::position` does not move), so this truncation is a
        // no-op today. Stated anyway, because `brief_of` truncates identically
        // before its own `containing_cell` call and two readings of one cell
        // that agree only by accident are what this method exists not to be.
        let locale = crate::band::truncate_to_walk(&self.agent.position, self.walk_depth());
        let cell = crate::brief::containing_cell(
            &locale,
            self.ctx.climate().geosphere(),
            self.ctx.nearest_index(),
        )?;
        Some(crate::fabric::FabricContext::at(
            self.ctx.terrain(),
            self.ctx.climate(),
            cell,
        ))
    }

    /// Every light burning where the possession stands (spec §4.2).
    ///
    /// Three kinds, and **one radius for all of them**: [`SIGHT_RADIUS`], whose
    /// own doc was written anticipating this campaign — *"so that the day a
    /// light model arrives there is exactly one place to replace, and so no
    /// second caller can quietly disagree with the first."* A torch reaching
    /// less far than sight would produce cells you can see with nothing
    /// illuminating them, which is not dim but incoherent.
    ///
    /// - **The implicit torch**, at the possession's own cell. Nathan's call at
    ///   G3: a possession is assumed to be carrying a light, which makes an
    ///   explicit carried torch a refinement rather than a new mechanism and
    ///   means nobody is ever stranded in the dark with no inventory to fix it.
    /// - **The hearth**, only where this chamber actually composes an
    ///   `AnchorKind::Hearth` — the interior graph decides whether there is a
    ///   fire, and [`crate::light::hearth_cell`] decides only where it sits.
    /// - **The doorways.** A declared approximation, and worth stating plainly:
    ///   the lattice records **no exterior door**, because a structure's way out
    ///   is a band transition (`out`), not a cell. The only aperture it models is
    ///   a `Threshold` between chambers, so that is where the day is admitted.
    ///   The light is `eyes::daylight_at`'s — the world's own star at the real
    ///   solar altitude for this day and latitude, which is the same call the
    ///   walk-band chart colours by — so a chamber genuinely darkens at night
    ///   rather than holding a permanent noon.
    fn chamber_sources(&self, inside: &Inside) -> Vec<crate::light::Source> {
        let mut sources = vec![crate::light::Source {
            at: inside.cell,
            illuminant: hornvale_kernel::color::blackbody(crate::light::TORCH_KELVIN),
            radius: SIGHT_RADIUS,
        }];

        let has_hearth = self.chamber_interior_here().is_some_and(|interior| {
            interior
                .ids()
                .iter()
                .any(|&a| interior.anchor(a).kind == crate::interior::AnchorKind::Hearth)
        });
        // Both halves are needed and neither implies the other: the interior
        // graph decides whether there IS a fire, and `hearth_cell` decides only
        // where it would sit — a chamber that owns no wall of its own has
        // nowhere to put one.
        if let (true, Some(at)) = (
            has_hearth,
            crate::light::hearth_cell(&inside.lattice, inside.at),
        ) {
            sources.push(crate::light::Source {
                at,
                illuminant: hornvale_kernel::color::blackbody(crate::light::HEARTH_KELVIN),
                radius: SIGHT_RADIUS,
            });
        }

        let (day, _altitude) = crate::eyes::daylight_at(
            self.world,
            self.calendar.as_ref(),
            self.day,
            self.agent.position.coord().latitude,
        );
        for &(_, _, at) in &inside.lattice.doorways {
            sources.push(crate::light::Source {
                at,
                illuminant: day,
                radius: SIGHT_RADIUS,
            });
        }
        sources
    }

    /// The drawn floor plan, in the chamber block's own shape: a bracketed
    /// header, the picture, an indented legend — the same three-part shape the
    /// locale chart uses, because they are one verb's two bands.
    ///
    /// The header names the chamber stood in; the PICTURE marks the cell, with
    /// `@` at exactly the standing cell. Task 4 left the mark out on purpose,
    /// because a "you are here" mark is a CELL position and the possession had
    /// none — marking a whole region would have claimed a precision the session
    /// did not have. Task 5 gives it the position, so the mark arrives with it.
    fn plan_here(&self) -> Result<String, VesselError> {
        let Some(inside) = self.inside.as_ref() else {
            // Unreachable through `handle` (the arm checks first), the same guard
            // and the same reason as `describe_chamber_here`: fabricating a plan
            // while out of doors would be worse than a loud error.
            return Err(VesselError::Build(
                "no plan to draw: the possession is out of doors".to_string(),
            ));
        };
        let plan = crate::lattice::render(&inside.lattice, Some(inside.cell));
        let id = chamber_id(&inside.structure.chambers[inside.at])?;
        let legend: Vec<String> = plan
            .legend
            .iter()
            .map(|(glyph, noun)| format!("{glyph} {noun}"))
            .collect();
        Ok(format!(
            "[plan: chamber {}, {} of {}]\n{}  legend: {}",
            id,
            inside.at + 1,
            inside.structure.chambers.len(),
            plan.picture,
            legend.join(", ")
        ))
    }

    /// What the floor plan here depicts: each glyph paired with the noun the
    /// legend gives it. Empty out of doors.
    ///
    /// Public because the parity test walks it. It reads the same structure the
    /// render does rather than re-parsing the picture — the same discipline
    /// `the_purview.rs` follows in reading `purview(0).legend` instead of the
    /// drawn chart.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn plan_legend(&self) -> Vec<(char, String)> {
        match self.inside.as_ref() {
            None => Vec::new(),
            // The SAME call `plan_here` draws with, mark included: the parity test
            // walks this legend, so a legend derived without the mark would leave
            // the one glyph the picture adds unchecked.
            Some(i) => crate::lattice::render(&i.lattice, Some(i.cell))
                .legend
                .into_iter()
                .map(|(glyph, noun)| (glyph, noun.to_string()))
                .collect(),
        }
    }

    /// The nouns the floor plan here names. Empty out of doors.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn plan_legend_nouns(&self) -> Vec<String> {
        self.plan_legend().into_iter().map(|(_, n)| n).collect()
    }

    /// The nouns the chamber here speaks of — the catalogue its own prose renders
    /// from. Empty out of doors.
    ///
    /// The plan's legend and this list are the two grains one band down, exactly
    /// as the chart's legend and the locale prose are one band up, and `examine`
    /// indoors answers for the union of them.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn chamber_nouns_here(&self) -> Vec<String> {
        match self.chamber_interior_here() {
            None => Vec::new(),
            Some(interior) => crate::chamber_prose::chamber_nouns(&interior)
                .into_iter()
                .map(str::to_string)
                .collect(),
        }
    }

    /// The interior of the chamber stood in, or `None` out of doors.
    fn chamber_interior_here(&self) -> Option<crate::interior::Interior> {
        let inside = self.inside.as_ref()?;
        let terrain = self.terrain_here();
        let brief = self.brief_here();
        Some(crate::interior::chamber_interior_of(
            &inside.structure.chambers[inside.at],
            &terrain,
            self.walk_depth(),
            &brief,
            inside.at,
        ))
    }

    /// Draw the fine layer: place this chamber's anchors into its cells, resolve
    /// each co-located creature onto one of them, and cast sight from where the
    /// possession stands. `None` out of doors, where there is no lattice and
    /// therefore nothing to narrow.
    ///
    /// # The join, and the honest name for it
    ///
    /// Hornvale's two fine layers meet here (spec §2). `liveness::Occupancy`
    /// records `(RoomAddr, AnchorId)` — the anchor a creature stands at in its
    /// ROOM's interior, [`crate::interior::interior_of`]'s graph. A chamber
    /// composes a DIFFERENT graph ([`crate::interior::chamber_interior_of`] is
    /// role-gated, so a threshold chamber and a hearthroom do not compose alike),
    /// and `Occupancy`'s own doc warns that an `AnchorId` is "only meaningful
    /// paired with the SPECIFIC `Interior` that produced it".
    ///
    /// So the two are joined **by anchor KIND, never by ordinal**: a creature at
    /// the room's threshold is drawn at this chamber's threshold, and a creature
    /// whose kind this chamber does not compose is simply not drawn. Reusing the
    /// raw offset would be the exact confusion that doc warns against — it would
    /// put a creature "at the hearth" wherever this chamber's second anchor
    /// happens to be — and it would make the drawn position mean nothing.
    ///
    /// What the join cannot do is decide WHICH CHAMBER a creature is in: the
    /// coarse layer persists a room, and every chamber of a structure truncates
    /// to one room. Every co-located creature is therefore drawn in the chamber
    /// the possession is standing in. That is the resolution the persisted layer
    /// has, stated rather than papered over; a chamber-scoped `Occupancy` is what
    /// would change it, and nothing today produces one.
    ///
    /// # §7 rule 5 has a caller
    ///
    /// [`crate::lattice::Occupancy::place`] refuses rather than overwrites, and
    /// the possession is seated FIRST — it is a creature standing in a cell like
    /// any other, and `you` is already drawn there. A creature whose cell is
    /// taken (by the possession, or by a creature earlier in `self.npcs`' own
    /// derivation order) is left unplaced rather than stacked.
    fn sighting(&self) -> Option<Sighting> {
        let inside = self.inside.as_ref()?;
        // The chamber's interior, through the SAME accessor `chamber_nouns_here`
        // and `examine_chamber` read it through — the plan asked for reuse rather
        // than a fourth derivation of `chamber_interior_of`, and this is it.
        let chamber = self.chamber_interior_here()?;
        // THE KIND JOIN IS ONLY WELL-DEFINED WHILE A CHAMBER'S KINDS ARE
        // DISTINCT, and nothing upstream enforces that: `pattern::compose` keeps
        // duplicates (`first_of.entry(p.kind).or_insert(id)`), and `INVENTORY`
        // already carries one duplicated kind (`Ground`) that only stays out of
        // one chamber because `draw` filters the pair on `built`. Add a second
        // pattern of an existing kind at the same `built` and the `find` below
        // silently collapses two distinct room anchors onto one chamber cell —
        // the second creature is then refused and vanishes from `marks` while
        // staying in `sensed.present`, indistinguishable from the legitimate
        // cell-taken case. Silent creature loss is the hardest class to notice
        // later, so it fails loudly in every test and debug run instead.
        debug_assert!(
            {
                let mut kinds: Vec<_> = chamber
                    .ids()
                    .iter()
                    .map(|&a| chamber.anchor(a).kind)
                    .collect();
                let before = kinds.len();
                kinds.sort();
                kinds.dedup();
                kinds.len() == before
            },
            "chamber {} composes two anchors of one kind, so the kind join is no \
             longer injective and a creature would be silently dropped",
            inside.at
        );
        let cells = crate::lattice::anchor_cells(&chamber, &inside.lattice, inside.at, inside.seed);

        let mut held = crate::lattice::Occupancy::default();
        // `Inside::cell` is documented passable (`standing_cell`/`cell_beyond`
        // both guarantee it), so this cannot refuse — asserted rather than
        // assumed, and bound to a local first so the placement itself still
        // happens in a release build.
        let seated = held.place(&inside.lattice, inside.cell, self.agent_entity());
        debug_assert!(
            seated.is_ok(),
            "the possession's own standing cell was refused: {seated:?}"
        );

        let terrain = self.terrain_here();
        let room = crate::interior::interior_of(&self.agent.position, &terrain);
        let mut placed = std::collections::BTreeMap::new();
        for npc in self.colocated_npcs() {
            // Room-CHECKED (`anchor_in`, not `at`): a creature whose recorded
            // anchor belongs to some other room is not standing anywhere here,
            // and reading it against this room's graph is what that method exists
            // to prevent.
            let Some(anchor) = self.occupancy.anchor_in(npc.entity, &self.agent.position) else {
                continue;
            };
            // RANGE-CHECKED before the read. `Interior::anchor` indexes straight
            // into its `Vec`, so an id recorded against a graph this room no
            // longer composes would not be merely wrong — it would panic in a
            // player's hands mid-turn. `anchor_in` rules out the wrong ROOM;
            // this rules out the wrong SIZE of the right room's graph, which is
            // what a furnishing epoch (`room/furnishing/v1`) would produce
            // between the tick that recorded the anchor and this read.
            if !room.ids().contains(&anchor) {
                continue;
            }
            let kind = room.anchor(anchor).kind;
            let Some(here) = chamber
                .ids()
                .into_iter()
                .find(|&a| chamber.anchor(a).kind == kind)
            else {
                continue;
            };
            // A missing cell is legitimate, not a bug: `anchor_cells` leaves
            // surplus anchors UNPLACED when a chamber holds fewer floor cells
            // than the interior holds anchors (3 of 256 on the grown corpus).
            let Some(&cell) = cells.get(&here) else {
                continue;
            };
            if held.place(&inside.lattice, cell, npc.entity).is_ok() {
                placed.insert(npc.entity, cell);
            }
        }

        Some(Sighting {
            lit: crate::lattice::shadowcast(&inside.lattice, inside.cell, SIGHT_RADIUS),
            placed,
        })
    }

    /// `examine <noun>` INDOORS: the chamber's own anchors first, then the floor
    /// plan's own legend.
    ///
    /// Anchors first because prose is the constitutionally primary surface (§3.5)
    /// — the same precedence the outdoor path keeps between prose and chart, and
    /// for the same reason. The plan's nouns are consulted second because two of
    /// the three (`the floor`, `a wall`) are things no anchor names but the
    /// picture depicts, and §6 obliges every depicted noun to answer.
    ///
    /// The standing mark is the one legend noun answered HERE rather than in
    /// `chamber_prose`: it resolves to `whoami`'s own words, because the possessed
    /// agent already has a self-description and a second one is exactly the drift
    /// §6 exists to prevent. `chamber_prose::glyph_detail` deliberately declines
    /// it, and `the_marks_answer_is_the_sessions_own_self_description` pins the two
    /// halves of that split together.
    ///
    /// The refusal is BYTE-IDENTICAL to the outdoor path's. Two wordings for one
    /// question — "what is this thing I cannot see?" — is precisely the drift §6
    /// exists to prevent, and the parity test asserts on the prefix.
    fn examine_chamber(&self, noun: &str) -> String {
        let wanted = noun.trim().to_lowercase();
        if let Some(interior) = self.chamber_interior_here() {
            for id in interior.ids() {
                let kind = interior.anchor(id).kind;
                if crate::chamber_prose::noun(kind).is_some_and(|n| n.to_lowercase() == wanted) {
                    return crate::chamber_prose::detail(kind).to_string();
                }
            }
        }
        if wanted == crate::lattice::render::YOU_NOUN {
            return self.whoami();
        }
        if let Some(detail) = crate::chamber_prose::glyph_detail(&wanted) {
            return detail.to_string();
        }
        // A CREATURE THE POSSESSION SENSES, answered last (The Sighting, fix
        // rounds 1-2).
        //
        // Three things about this arm, each of which was a decision:
        //
        // 1. **It closes a band regression.** Outdoors `examine <label>` resolves
        //    through the chart's legend and answers; before this arm, walking
        //    through a doorway made the same noun stop answering — while the plan
        //    inside was drawing a mark bearing exactly that noun. §6 obliges
        //    every depicted noun to answer, and The Lintel's water jar is what
        //    happens when it does not.
        // 2. **It answers with the SAME sentence the outdoor path does**
        //    ([`crate::purview::creature_datum`], one definition, three callers),
        //    because `a_noun_at_both_grains_resolves_to_one_datum` makes one
        //    noun → one datum a tested contract and a band boundary must not be
        //    the place it quietly stops holding.
        // 3. **Its predicate is `sensed_npcs`, not "placed and lit"** (fix round
        //    2). Those are not complements: an UNPLACED co-located creature — its
        //    cell taken, or a surplus anchor — is in `sensed.present` and would
        //    have been refused by a placed-and-lit test, so `present` did not
        //    imply examinable. Keying on the channel's own roster makes the two
        //    agree by construction, and keeps the withheld creature refused.
        //
        // Answered LAST, after the anchors and the glyph legend, because prose is
        // the constitutionally primary surface (§3.5) and an anchor noun must win
        // any tie. **The label match is hoisted ABOVE `sighting()`** so that an
        // ordinary indoor `examine` MISS — every noun that is not a creature's —
        // pays nothing: `sighting` is the one costly read on this path
        // (`anchor_cells`, 42 us median / 410 us p99), and before the hoist even
        // the parity test's own deliberate miss paid it.
        if let Some(npc) = self
            .colocated_npcs()
            .into_iter()
            .find(|npc| npc.label.to_lowercase() == wanted)
        {
            let sensed = self.sensed_npcs(self.sighting().as_ref());
            if sensed.iter().any(|n| n.entity == npc.entity) {
                return crate::purview::creature_datum(&npc.label, &npc.species);
            }
        }
        format!("You see no {noun} here.")
    }

    fn wait(&mut self, arg: &str) -> Turn {
        // The world moves without you: advance the day, then run the NPC
        // layer's tick against the session-owned ledger (the possessed
        // agent's own frozen reads are untouched — only `self.ledger`
        // evolves).
        let days: f64 = if arg.is_empty() {
            1.0
        } else {
            match arg.parse::<f64>() {
                Ok(d) if d.is_finite() && d > 0.0 => d,
                _ => return Turn::Out(format!("Wait how long? '{arg}' is no span of days.")),
            }
        };
        // Snapshot every NPC's position as of NOW (the day about to end),
        // before advancing — the "before" half of the departure/arrival
        // comparison `narrate_motion` needs to name a specific transition
        // rather than just count facts.
        let before: Vec<RoomAddr> = self
            .npcs
            .iter()
            .map(|npc| agent_position(&self.ledger, npc, self.day))
            .collect();
        // ...and WHO the possession could sense as of that same moment (The
        // Sighting, fix round 4). A departure is narrated about a creature that
        // is, by the time it is narrated, no longer here — so the CURRENT sensed
        // roster can never contain it, and gating a departure on "sensed now"
        // would silently delete every departure line. The honest question for a
        // departure is whether the player could see the creature WHILE IT WAS
        // HERE, and this is the only moment that question is still answerable.
        let sensed_before: std::collections::BTreeSet<EntityId> = self
            .sensed_npcs(self.sighting().as_ref())
            .iter()
            .map(|npc| npc.entity)
            .collect();
        let from = self.day;
        self.day = WorldTime {
            day: self.day.day + days,
        };
        // Prefill the session-owned geometry memo (the-waymark fix round,
        // Finding 1) for each NPC's CURRENT position (`before`, captured
        // above) and its three neighbours — the rooms this tick's drive
        // stack (Thermal/Hunger/Danger/is_water/forage/hazards, all read via
        // `LocaleTerrain`) will touch for a stationary or slow-moving
        // creature. Under `&mut self.mesh_memo`, strictly BEFORE any
        // `LocaleTerrain` (and so any drive) exists for this tick — a
        // creature that moves further than one hop this tick still gets a
        // correct answer, just an unmemoized one (`corner_weights_lookup`'s
        // fall-through), which is the whole point of the miss path.
        {
            let geo = self.ctx.climate().geosphere();
            let index = self.ctx.nearest_index();
            for pos in &before {
                pos.corner_weights_memo(geo, index, &mut self.mesh_memo);
                for n in pos.neighbors_memo(&mut self.mesh_memo) {
                    n.corner_weights_memo(geo, index, &mut self.mesh_memo);
                }
            }
        }
        // A read-only SNAPSHOT of the just-filled memo: `LocaleTerrain`
        // (below) needs a SHARED reference for the rest of this tick, while
        // `self.mesh_memo` stays independently `&mut`-able for
        // `step_with_occupancy`'s own `neighbors` threading — a live shared
        // borrow embedded in `terrain` AND a live `&mut` borrow passed to
        // `step_with_occupancy` in the SAME call would otherwise alias the
        // same field. Cloning a `BTreeMap` of a few dozen entries is cheap
        // next to the grid scans it is standing in for.
        let mesh_snapshot = self.mesh_memo.clone();
        let terrain = LocaleTerrain::with_fields(
            &self.ctx,
            self.calendar.as_ref(),
            self.predator.as_ref(),
            self.prey.as_ref(),
            Some(&self.built),
            Some(&mesh_snapshot),
        );
        let sys = DriveMovements {
            npcs: self.npcs.clone(),
            from,
            to: self.day,
            params: SUSTENANCE,
            // The planet's rotation period, so the action clock's tick divides
            // the local day exactly (The Action Clock, spec §4.1). `None` on a
            // tidally-locked world, which the rotation pin admits.
            day_length_std: self
                .calendar
                .as_ref()
                .and_then(|c| c.day_length())
                .map(|d| d.get()),
            terrain: &terrain,
        };
        // Recover this tick's within-room `Occupancy` alongside the facts
        // `tick()` (below) commits — the same walk, read twice, exactly the
        // pattern the lab's health battery uses (task 6b): a second, PURE
        // re-evaluation of the identical frozen `self.ledger` and `sys`,
        // not a second simulation with different consequences. Without
        // this, `needs()` and the snapshot's present-entry read sampled a
        // colder felt state than the NPC actually experienced — warmth at
        // the room's landing anchor, never wherever its own walk carried it
        // (Important 4, The Threshold whole-branch review).
        let (_facts, occupancy) =
            sys.step_with_occupancy(&self.ledger, &mut self.mesh_memo, &mut self.home_nav_cache);
        match tick(&self.ledger, &[&sys], &["drive-movements"], &self.registry) {
            Ok(next) => {
                let moved = next.len() - self.ledger.len();
                self.ledger = next;
                self.occupancy = occupancy;
                // The First Mark, one-hop forward integration: after the NPC
                // drive tick settles, any co-located-or-not NPC whose
                // grievance has crossed the hostility threshold commits its
                // `turned-hostile` fact — a discrete social consequence of
                // the player's own acts, not an ambient drive. Iterating
                // `self.npcs` in its existing (derivation) order keeps the
                // commit sequence deterministic.
                let player = self.agent_entity();
                for npc in self.npcs.iter() {
                    // The `value_of(...).is_none()` check below is the SOLE
                    // idempotency guarantee for this fact, not a second
                    // layer atop `TURNED_HOSTILE`'s `functional: true`
                    // registration: `Ledger::commit` only dedups via an
                    // exact full-envelope match, and `day` advances every
                    // tick, so a later-day re-fire is never an exact dup;
                    // and the functional flag only rejects a *different*
                    // object for the same subject/predicate, but `object`
                    // here is always the same constant `player`, so that
                    // flag can never trip either. Remove this guard and the
                    // loop silently refires (a new `turned-hostile` fact,
                    // same subject/predicate/object, only `day` differing)
                    // on every subsequent `wait` the NPC is still past
                    // threshold for.
                    if grievance(&self.ledger, npc.entity) >= HOSTILITY_THRESHOLD
                        && self.ledger.value_of(npc.entity, TURNED_HOSTILE).is_none()
                    {
                        let fact = Fact {
                            subject: npc.entity,
                            predicate: TURNED_HOSTILE.to_string(),
                            object: Value::Entity(player),
                            place: None,
                            day: Some(self.day.day),
                            provenance: "player-provoked".to_string(),
                        };
                        self.ledger
                            .commit(fact, &self.registry)
                            .expect("turned-hostile is registered and finite");
                    }
                }
                // Re-absorb the (possibly changed) here into knowledge; the
                // possessed agent's own scenery is still read from the
                // frozen `self.world`, so this cannot change day-0 output.
                if let Err(e) = self.absorb_here() {
                    return Turn::Out(format!("error: {e}"));
                }
                Turn::Out(self.narrate_motion(moved, &before, &sensed_before))
            }
            Err(e) => Turn::Out(format!("Time falters: {e}")),
        }
    }

    /// Narrate what the tick committed: silence if nothing moved, else name
    /// any derived NPC's PERCEPTIBLE TRANSITION through the possessed
    /// agent's own room — an arrival (the NPC was elsewhere, now shares the
    /// room) or a departure (the NPC was here, now elsewhere; an absence is
    /// a real observation too, not just an arrival). `before` is each NPC's
    /// position as of the day just ended (captured by `wait` prior to the
    /// tick); both halves are read back from ledgers, never decorative
    /// flavor text. The generic "stirred" line is the fallback only for
    /// motion that never touches the player's own room.
    ///
    /// # Gated on sight, and ASYMMETRICALLY — the fifth reader
    ///
    /// This is the richest of the disclosure channels The Sighting had to close
    /// (fix round 4) and the last one found: it asserts presence **unprompted**,
    /// with identity, without the player naming anything —
    /// `You notice <label> here now.` — and `handle` puts no band guard on
    /// `wait`, so it fires indoors.
    ///
    /// The gate cannot be one predicate, because the two transitions ask
    /// different questions of different moments:
    ///
    /// - **An arrival** is about a creature that is here NOW, so it is gated on
    ///   the CURRENT sensed roster. A creature that arrived into a cell sight
    ///   does not reach has not been observed arriving.
    /// - **A departure** is about a creature that is, by the time this runs,
    ///   no longer here at all — so the current roster can never contain it and
    ///   gating on it would delete every departure line ever printed. The honest
    ///   question is whether the player could see the creature WHILE IT WAS
    ///   HERE, which is why `wait` captures `sensed_before` at the same instant
    ///   it captures `before`. Watching something you never saw arrive go is the
    ///   same disclosure as watching it arrive.
    ///
    /// A redacted transition falls through to the generic "stirred" line, which
    /// reports a COUNT of committed facts world-wide and claims nothing about
    /// this room — motion without identity, which is what the player is entitled
    /// to.
    ///
    /// **Latent, not demonstrable end-to-end.** A 200-turn indoor sweep never
    /// fired either branch on seed 42, whose structure produces only the
    /// `stirred` fallback (`possession_moves.rs` books that lost end-to-end
    /// coverage as an open followup). The branch is live code all the same, and
    /// `narrate_motion_does_not_name_a_creature_sight_withheld` pins it by
    /// feeding the vector directly — "I could not reach it" is not coverage.
    fn narrate_motion(
        &self,
        moved: usize,
        before: &[RoomAddr],
        sensed_before: &std::collections::BTreeSet<EntityId>,
    ) -> String {
        if moved == 0 {
            return "Time passes; the world keeps its shape.".to_string();
        }
        let sensed_now: std::collections::BTreeSet<EntityId> = self
            .sensed_npcs(self.sighting().as_ref())
            .iter()
            .map(|npc| npc.entity)
            .collect();
        let mut arrived: Vec<&str> = Vec::new();
        let mut departed: Vec<&str> = Vec::new();
        for (npc, prior) in self.npcs.iter().zip(before) {
            let was_here = *prior == self.agent.position;
            let is_here = agent_position(&self.ledger, npc, self.day) == self.agent.position;
            match (was_here, is_here) {
                (false, true) if sensed_now.contains(&npc.entity) => {
                    arrived.push(npc.label.as_str())
                }
                (true, false) if sensed_before.contains(&npc.entity) => {
                    departed.push(npc.label.as_str())
                }
                _ => {}
            }
        }
        let mut parts: Vec<String> = Vec::new();
        if !departed.is_empty() {
            parts.push(format!("You watch {} go.", departed.join(", ")));
        }
        if !arrived.is_empty() {
            parts.push(format!("You notice {} here now.", arrived.join(", ")));
        }
        if parts.is_empty() {
            format!("Time passes. You sense movement nearby ({moved} stirred).")
        } else {
            format!("Time passes. {}", parts.join(" "))
        }
    }

    /// The chart. `map` draws the walk depth; `map out [N]` draws N rungs
    /// coarser — zoom in this mesh is path truncation, so a coarse chart is
    /// the same builder one rung up the address space, never an aggregate.
    /// The real bound on how far out a chart can zoom is not the walk depth
    /// but `depth - globe_level`: past that, `purview` truncates the address
    /// above the canonical grid's own refinement and the locale layer has
    /// nothing to inherit from. That bound is refused here in player-facing
    /// language, never as the locale layer's internal "canonical grid"
    /// wording.
    fn map(&self, rest: &str) -> Turn {
        let zoom = match rest.split_whitespace().collect::<Vec<_>>().as_slice() {
            [] => 0u32,
            ["out"] => 1,
            ["out", n] => match n.parse::<u32>() {
                Ok(v) => v,
                // `u32::from_str` overflows past 4294967295, but this arm
                // never sees a real value to check — the parse itself
                // failed — so quoting `u32::MAX` back at the player states a
                // bound that is false: the real ceiling is `depth -
                // globe_level` (six rungs on seed 42), enforced below. Rather
                // than inventing a second, wrong number here, saturate to
                // `u32::MAX` — certainly past any real chart's ceiling — and
                // let the ordinary bound check just below produce the one
                // honest refusal.
                Err(e) if matches!(e.kind(), std::num::IntErrorKind::PosOverflow) => u32::MAX,
                // `u32::from_str` reports a leading '-' as `InvalidDigit`,
                // not `NegOverflow` (there is no negative u32 to overflow
                // toward), so folding it into "'-1' is not a number" would
                // be false — it is a number, just a negative one, and there
                // is no such thing as zooming out a negative number of
                // rungs.
                Err(_)
                    if n.starts_with('-')
                        && n.len() > 1
                        && n[1..].bytes().all(|b| b.is_ascii_digit()) =>
                {
                    return Turn::Out(format!(
                        "Zoom out by how much? '{n}' is negative; there is no such rung."
                    ));
                }
                Err(_) => {
                    return Turn::Out(format!("Zoom out by how much? '{n}' is not a number."));
                }
            },
            _ => return Turn::Out("Say 'map' or 'map out [N]'.".to_string()),
        };
        let depth = self.agent.position.depth();
        let max_zoom = depth.saturating_sub(self.ctx.globe_level());
        if zoom > max_zoom {
            return Turn::Out(
                "There is no coarser rung to show; the chart already draws at the coarsest \
                 the world allows."
                    .to_string(),
            );
        }
        let scene = match self.purview(zoom) {
            Ok(s) => s,
            Err(e) => return Turn::Out(format!("error: {e}")),
        };
        // The footer must name the DRAWN cell's own exits, not the walk
        // depth's — the caption, not the picture, carries the honesty, and
        // `self.ways()` always answers for the fine-grained room the agent
        // actually stands in, which is a different cell than the one this
        // chart draws once `zoom > 0`. `chart_centre` is the SAME function
        // `purview_scene` uses to truncate — a second, independent copy of
        // this arithmetic is exactly how the footer and the drawn cell end
        // up disagreeing about which room is centred.
        let centre = crate::chart_centre(&self.agent.position, zoom);
        let ways: Vec<String> = match self.ctx.describe(&centre, self.day) {
            Ok(locale) => locale
                .exits
                .iter()
                .filter(|e| e.kind == ExitKind::Edge)
                .filter_map(|e| match e.direction {
                    Direction::Compass(c) => Some(format!("{c:?}").to_uppercase()),
                    _ => None,
                })
                .collect(),
            // Above the bound where `map`'s own clamp already refuses, but
            // reachable in principle for a future caller: an undrawable
            // footer is omitted, never fabricated from the wrong depth.
            Err(_) => Vec::new(),
        };
        // The colour lens is the default draw — Task 4's headline claim is
        // that a possession sees as its own kind does, so the chart must
        // already show that rather than requiring an opt-in. `Eyes::Off`
        // falls all the way back to the plain terrain lens: no observer, no
        // tint, no escape sequence — the same posture a screen reader takes.
        let lens = if self.eyes == crate::eyes::Eyes::Off {
            "terrain"
        } else {
            "colour"
        };
        Turn::Out(hornvale_scene::render_surrounds_ascii(&scene, lens, &ways))
    }

    /// Bare `eyes`: whose eyes the chart is coloured through, the arity of
    /// what they see, and what the projection drops. `Eyes::Off` reports the
    /// decline honestly rather than describing an observer that is not in
    /// use.
    /// type-audit: bare-ok(prose: return)
    fn eyes_report(&self) -> String {
        let Some((observer, name)) = crate::eyes::resolve(&self.eyes, &self.agent) else {
            return "Your eyes are off: the chart draws no colour, and carries no sight \
                    declaration."
                .to_string();
        };
        let channels = observer.channels();
        let chromatic = observer.chromatic_channels();
        let preserves = observer
            .projection()
            .map(hornvale_kernel::color::Projection::preserves)
            .unwrap_or("no projection");
        // `ocular_reason` wants the PERCEPTION VECTOR the observer was built
        // from, which `resolve` does not carry back out (it hands back the
        // built `Observer`) — so it is looked up a second time, by the same
        // name `resolve` used, from the same registry `observer_named`
        // reads. "standard" has no row (it is the kernel's own observer, not
        // a species'), so it gets its own sentence rather than a lookup that
        // would always miss.
        let reason = hornvale_species::perception_registry()
            .get_by_label(&name)
            .map(hornvale_worldgen::observer::ocular_reason)
            .unwrap_or_else(|| {
                "the standard observer is an authored full trichromat: every hue exemplar \
                 stays distinct, unmerged"
                    .to_string()
            });
        format!(
            "You see through {name}'s eyes: {channels} channels ({chromatic} chromatic). \
             {reason}. The projection preserves {preserves}."
        )
    }

    /// `eyes own` / `eyes off` / `eyes <name>`: switch whose eyes the chart
    /// is coloured through. An unknown name is refused loudly, naming what
    /// was asked for and listing the roster — generation never guesses (spec
    /// §4.6), so this never silently falls back to a default eye.
    fn set_eyes(&mut self, rest: &str) -> Turn {
        match rest {
            "own" => {
                self.eyes = crate::eyes::Eyes::Own;
                Turn::Out(self.eyes_report())
            }
            "off" => {
                self.eyes = crate::eyes::Eyes::Off;
                Turn::Out(self.eyes_report())
            }
            name => {
                if hornvale_worldgen::observer::observer_named(name).is_none() {
                    return Turn::Out(format!(
                        "There is no observer named '{name}'. Known: {}.",
                        hornvale_worldgen::observer::observer_roster().join(", ")
                    ));
                }
                self.eyes = crate::eyes::Eyes::Named(name.to_string());
                Turn::Out(self.eyes_report())
            }
        }
    }

    /// Every noun this lens has surfaced, at either grain: the prose's own
    /// catalog first (the fine grain wins a collision — prose is primary),
    /// then the chart's legend. This union IS the attention join.
    ///
    /// A genuine failure of either grain (the observable scene or the
    /// chart) is propagated as `Err`, never silently downgraded to an empty
    /// union — `examine` must be able to tell "the lens failed" from "no
    /// grain surfaced that noun", and only the latter is a bare absence.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn lens_nouns(&self) -> Result<Vec<crate::focalize::Noun>, VesselError> {
        let mut out: Vec<crate::focalize::Noun> = self.focalized()?.nouns;
        let scene = self.purview(0)?;
        for e in &scene.legend {
            if !out.iter().any(|n| n.display.eq_ignore_ascii_case(&e.noun)) {
                out.push(crate::focalize::Noun::new(&e.noun, &e.noun, &e.datum));
            }
        }
        Ok(out)
    }

    /// The common case is a prose noun, and the chart (~1.5 ms to build) is
    /// never needed to answer one: check the prose catalog first and only
    /// fall through to the chart on a miss, rather than routing through
    /// `lens_nouns` (which always builds both grains for its own contract —
    /// the full union other callers and the thesis test depend on). A noun
    /// named by both grains still resolves to the prose datum, because the
    /// prose catalog is checked, and answered from, first.
    fn examine(&self, noun: &str) -> Turn {
        if noun.is_empty() {
            return Turn::Out("Examine what?".to_string());
        }
        let wanted = noun.to_lowercase();
        let prose = match self.focalized() {
            Ok(f) => f,
            Err(e) => return Turn::Out(format!("error: {e}")),
        };
        if let Some(n) = prose.nouns.iter().find(|n| n.matches(&wanted)) {
            return Turn::Out(n.datum.clone());
        }
        let scene = match self.purview(0) {
            Ok(s) => s,
            Err(e) => return Turn::Out(format!("error: {e}")),
        };
        // The chart legend resolves by the same word rule as the prose catalog,
        // deriving a mark's words mechanically — safe for a plain
        // `<kind> of <place>` construction in a way it is not for a
        // comma-qualified room descriptor, which declares its noun phrase
        // instead (The Handle, spec §2).
        //
        // A legend entry DUPLICATING a prose entry is skipped, and that is the
        // load-bearing half. The legend keys its ground mark on the whole raw
        // descriptor, so deriving from it re-admits exactly the qualifiers the
        // prose entry deliberately declined: without this, `examine hollow`
        // failed against the prose catalog and then succeeded against the
        // legend's copy of the same name, in the same room, with different
        // wording. The documented precedence — "a noun named by both grains
        // resolves to the prose datum" — has to cover the grains DISAGREEING
        // about a word, not merely which datum to print.
        match scene
            .legend
            .iter()
            .filter(|e| {
                !prose
                    .nouns
                    .iter()
                    .any(|n| n.display.eq_ignore_ascii_case(&e.noun))
            })
            .find(|e| crate::focalize::Noun::new(&e.noun, &e.noun, &e.datum).matches(&wanted))
        {
            Some(e) => Turn::Out(e.datum.clone()),
            None => Turn::Out(format!("You see no {noun} here.")),
        }
    }

    fn whoami(&self) -> String {
        format!(
            "A {} of {} (agent {}), day {}, room {}.",
            self.agent.species,
            self.agent.village.name,
            self.agent.id.0,
            self.day.day,
            self.agent
                .position
                .pack()
                .map(|r| r.0.to_string())
                .unwrap_or_else(|_| "?".to_string()),
        )
    }

    /// List every derived NPC this session knows about, with the entity id
    /// `why` accepts (mirrors the repl's `beliefs` → `why <id>` pattern: an
    /// id-listing verb feeding the recount verb).
    fn list_npcs(&self) -> String {
        let mut lines = vec![format!("{} NPC(s) derived this session:", self.npcs.len())];
        for npc in &self.npcs {
            lines.push(format!("  [{}] {}", npc.entity.0, npc.label));
        }
        lines.join("\n")
    }

    /// Recount an NPC's dated history — the provenance read (the-quickening
    /// T4): the world remembers, so `why` over an NPC that has moved names
    /// each committed `agent-at` with the day it was asserted (`recount` in
    /// `windows/historiography` renders the day suffix). `who` is matched
    /// first as a numeric entity id, else as a case-insensitive substring of
    /// an NPC's label — this mirrors the CLI repl's `why <id>` (see
    /// `cli/src/repl.rs`) over the one kind of subject a possess session
    /// actually has on hand without a prior id-listing step: a name.
    fn why(&self, who: &str) -> String {
        let who = who.trim();
        if who.is_empty() {
            return "Why what? Name an NPC (label or id — see 'npcs').".to_string();
        }
        let target = who
            .parse::<u64>()
            .ok()
            .and_then(|id| self.npcs.iter().find(|n| n.entity.0.get() == id))
            .or_else(|| {
                let needle = who.to_lowercase();
                self.npcs
                    .iter()
                    .find(|n| n.label.to_lowercase().contains(&needle))
            });
        let Some(npc) = target else {
            return format!("No one here answers to '{who}' (see 'npcs').");
        };
        self.recount(npc.entity)
            .unwrap_or_else(|| format!("Nothing is yet recorded of {}.", npc.label))
    }

    /// The provenance read itself: a temporary `World` wrapping this
    /// session's OWN evolving ledger/registry (never the frozen `self.world`
    /// — an NPC's `agent-at` facts live only in the session's evolved
    /// state), handed to the domain-agnostic historiography window exactly
    /// as the CLI repl's `why` hands it the genesis world.
    fn recount(&self, entity: EntityId) -> Option<String> {
        let evolved = World {
            seed: self.world.seed,
            registry: self.registry.clone(),
            ledger: self.ledger.clone(),
            // Never serialized and never read by historiography; an empty
            // stamp is the accurate claim for a world that exists for the
            // duration of one provenance read.
            derived_under: std::collections::BTreeMap::new(),
        };
        hornvale_historiography::recount(&evolved, entity)
    }

    /// Every derived NPC sharing the possessed agent's current room — the
    /// co-located lookup `needs` and `provoke`/`soothe` both build on.
    fn colocated_npcs(&self) -> Vec<&Npc> {
        self.npcs
            .iter()
            .filter(|npc| agent_position(&self.ledger, npc, self.day) == self.agent.position)
            .collect()
    }

    /// Who is here **and sensed** — [`Self::colocated_npcs`] narrowed by sight.
    ///
    /// **ONE PREDICATE, THREE READERS** (The Sighting, fix round 2): the
    /// `sensed.present` channel, `needs`, and `examine_chamber` all key on this
    /// and nothing else, so a verb and the channel cannot disagree about who the
    /// possession can perceive. They did: `snapshot` withheld a creature and
    /// `needs` named it — by label *and* felt state — one verb later, which is
    /// the side channel around a structural redaction that gating `examine`
    /// alone was meant to close.
    ///
    /// # The rule, and the row worth stating out loud
    ///
    /// A creature is withheld only when the embedding **placed** it in a cell
    /// sight does not reach. An **unplaced** creature stays:
    ///
    /// | case | sensed here | examinable | drawn on the plan |
    /// |---|---|---|---|
    /// | placed and lit | yes | yes | yes |
    /// | placed and unlit | no | no | no |
    /// | **unplaced** | **yes** | **yes** | **no** |
    ///
    /// If the embedding could not place a creature we cannot say sight hid it —
    /// presence is the conservative default, and "present but undrawable" is
    /// honest where "absent" would be a lie. It is also what keeps spec §2.1
    /// intact: **presence must never depend on the embedder's free draws; only
    /// DRAWING may.** An unplaced creature arises for reasons that have nothing
    /// to do with visibility (its cell was already held; it was a surplus anchor
    /// — 3 of 256 on the grown corpus; no tick has recorded where it stands), so
    /// reading absence-from-the-map as hidden would let the placement scan decide
    /// what the player is told is *there*, not merely where it is drawn.
    ///
    /// Out of doors `sighting` is `None`, so this is exactly `colocated_npcs`
    /// and no band but the chamber narrows anything.
    fn sensed_npcs(&self, sighting: Option<&Sighting>) -> Vec<&Npc> {
        self.colocated_npcs()
            .into_iter()
            .filter(|npc| {
                !sighting.is_some_and(|s| {
                    s.placed
                        .get(&npc.entity)
                        .is_some_and(|cell| !s.lit.contains(cell))
                })
            })
            .collect()
    }

    /// Resolve `who` to one **sensed** co-located NPC (The First Mark): an empty
    /// argument selects the first such NPC (the common case — a lone co-located
    /// NPC needs no name), otherwise `who` is matched as a numeric entity id or
    /// a case-insensitive substring of an NPC's label, mirroring `why`'s
    /// resolution but restricted to NPCs actually here.
    ///
    /// **The fourth reader of [`Self::sensed_npcs`]** (The Sighting, fix round
    /// 3), and the leak it closes is the same one a third time. `provoke`/
    /// `soothe` resolve through here, and a *successful* act narrates the
    /// creature by name — `You provoke <label>. They bristles.` — so an
    /// unfiltered lookup disclosed exactly what the redaction was built to
    /// withhold: presence, and disposition state, through a verb's success line.
    /// A bare `provoke` was worse still, since it silently *selected* the hidden
    /// creature. [`Self::would_turn_hostile`] rides the same resolution and so
    /// narrows with it, which is what its own doc already promises.
    ///
    /// **This answers the game question conservatively: you cannot act on what
    /// you cannot see.** That is a choice, not a derivation — "strike the thing
    /// you heard but cannot see" is a perfectly good future mechanic. It would
    /// be a deliberate feature with its own narration, though, not the residue
    /// of a lookup nobody filtered.
    ///
    /// The unplaced row of `sensed_npcs`' table holds here as everywhere: a
    /// creature the embedding could not place is sensed, so it stays provokable.
    fn colocated_npc(&self, who: &str) -> Option<&Npc> {
        let here = self.sensed_npcs(self.sighting().as_ref());
        let who = who.trim();
        if who.is_empty() {
            return here.into_iter().next();
        }
        who.parse::<u64>()
            .ok()
            .and_then(|id| here.iter().find(|n| n.entity.0.get() == id).copied())
            .or_else(|| {
                let needle = who.to_lowercase();
                here.iter()
                    .find(|n| n.label.to_lowercase().contains(&needle))
                    .copied()
            })
    }

    /// Commit the first player-authored fact: a signed disposition shift on
    /// a co-located NPC. `sign` is +1 (provoke) / -1 (soothe). The fact
    /// carries a `player:` provenance so a reader (and contradiction
    /// checking) can tell it from every fact a world system commits.
    ///
    /// Same-day dedup is intentional, not a bug: exactly one disposition
    /// shift lands per (NPC, day, direction) — escalating a mark on the same
    /// NPC the same day requires time to pass first (a `wait`), not
    /// repeating the verb. Because `self.day` only advances on `wait`, a
    /// same-day repeat of `provoke` (or `soothe`) on the same NPC produces a
    /// byte-identical `Fact` envelope, and `Ledger::commit`'s idempotent
    /// dedup (`Ok(false)` = identical fact already present, nothing
    /// appended) makes it a true no-op. The narration below reads that
    /// return value rather than assuming success, so the player is never
    /// told a mark landed when the ledger disagrees.
    fn act_on_disposition(&mut self, who: &str, sign: i8) -> Turn {
        let Some(npc) = self.colocated_npc(who) else {
            return Turn::Out("There is no one here to provoke or soothe.".to_string());
        };
        let entity = npc.entity;
        let label = npc.label.clone();
        let verb = if sign >= 0 { "provoke" } else { "soothe" };
        let fact = Fact {
            subject: entity,
            predicate: DISPOSITION_SHIFT.to_string(),
            object: Value::Number(sign as f64),
            place: None,
            day: Some(self.day.day),
            provenance: format!("player: {verb}"),
        };
        let appended = self
            .ledger
            .commit(fact, &self.registry)
            .expect("disposition-shift is registered and finite");
        if appended {
            let felt = if sign >= 0 { "bristles" } else { "eases" };
            Turn::Out(format!("You {verb} {label}. They {felt}."))
        } else if sign >= 0 {
            Turn::Out(format!(
                "You round on {label} again, but the moment already holds all the edge it will take today."
            ))
        } else {
            Turn::Out(format!("{label} is already as eased as they'll be today."))
        }
    }

    /// The felt-state read (the-wanting T4, spec §4.5 as corrected by G4):
    /// diegetic prose for every CO-LOCATED NPC's drive, never a raw number.
    /// Deliberately reads the NPCs, not the possessed agent — the player's
    /// own moves are never committed as `agent-at` (only NPCs' are), so
    /// `drive_at` for the player would fold an empty history and read
    /// eternally parched (a followup, decision-ledger #8 / G4 correction (a)
    /// rides player-acts-mutate, Campaign IV). A co-located NPC's drive IS a
    /// real fold over its own committed history, so its felt state is
    /// meaningful the moment the drive model exists.
    fn needs(&self) -> String {
        // GATED ON SIGHT, through the same predicate `sensed.present` and
        // `examine` use (The Sighting, fix round 2). Ungated this verb was a
        // side channel straight around the structural redaction `snapshot` had
        // just performed: it named — by label AND by felt state — a creature the
        // pane had withheld one verb earlier. `sensed_npcs` is `colocated_npcs`
        // out of doors, so nothing outside the chamber band changes.
        let here = self.sensed_npcs(self.sighting().as_ref());
        if here.is_empty() {
            return "No one else is here to read.".to_string();
        }
        // Read each co-located NPC's felt state through the SAME arbitration
        // that drives it (spec §7) — the affect label coloured by what the
        // feeling is about (its intentional object), not a bare thirst scalar.
        // `&self`-only: shares whatever `self.mesh_memo` already holds
        // (free — no mutation), same posture as `snapshot`.
        let terrain = LocaleTerrain::with_fields(
            &self.ctx,
            self.calendar.as_ref(),
            self.predator.as_ref(),
            self.prey.as_ref(),
            Some(&self.built),
            Some(&self.mesh_memo),
        );
        let mut afraid_memo = PrimaryAfraidMemo::new();
        // A throwaway `RoomMeshMemo` for `affect_of_memo_occupied`'s own
        // `neighbors_memo` write-through (rider (b)) — see `snapshot`'s
        // identical comment for why `&self` cannot reach the session-owned
        // one here.
        let mut mesh_memo = hornvale_kernel::RoomMeshMemo::new();
        // A throwaway `HomeNavCache` (the-waymark, Task 4) — see `snapshot`'s
        // identical comment.
        let mut home_nav_cache = HomeNavCache::new();
        here.iter()
            .map(|npc| {
                let affect = affect_of_memo_occupied(
                    &self.ledger,
                    npc,
                    &self.npcs,
                    self.day,
                    &terrain,
                    &mut afraid_memo,
                    Some(&self.occupancy),
                    &mut mesh_memo,
                    &mut home_nav_cache,
                );
                format!("The {} {}.", npc.label, felt_phrase(&affect))
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Write a Common sentence into the margin: the session absorbs its own
    /// spoken line into its `Knowledge` via the transfer seam (The Echo
    /// T4). Renamed from `tell` at the Vessel Stitch (T2, G3 exchange) —
    /// the player writes what they have learned into their copy's margin,
    /// the program's own margin device turned toward the reader; the
    /// response is the closed string `Written in the margin.` regardless
    /// of how many facts the sentence carried (heard is not true, but
    /// written is initiation — spec §1). The acceptable floor shape — no
    /// NPC addressing yet (a future `write <npc> <sentence>` is a UX
    /// decision this spec doesn't commit to, G3 flag 2). Threaded (The
    /// Shuttle): calls `hornvale_book::parse_context_from` with
    /// `self.terrain`/`self.climate` when both are `Some`, so a session's
    /// repeated `write` calls share `start`'s one sculpt instead of
    /// re-sculpting the globe every turn, the same posture as `consult`.
    fn write(&mut self, line: &str) -> String {
        if line.is_empty() {
            return "Write what? Speak a line of Common.".to_string();
        }
        let ctx = match (self.terrain.as_ref(), self.climate.as_ref()) {
            (Some(t), Some(c)) => hornvale_book::parse_context_from(self.world, t, c),
            _ => hornvale_book::parse_context(self.world),
        };
        match absorb_common(&mut self.knowledge, line, &ctx) {
            Ok(_) => "Written in the margin.".to_string(),
            Err(e) => format!("That doesn't parse as Common: {e}"),
        }
    }

    /// Read the Book from inside the world (the Vessel Stitch, T2): the
    /// Reckoning of Years at the session's own day (`hornvale_book::
    /// reckoning_at` — the same accessor the CLI's `--at` lens calls, spec
    /// §3.1/§4.4), then whatever the session's own margin (`Knowledge`,
    /// via `write`) has initiated it into
    /// (`hornvale_book::esoteric_lines`) — or the closed fallback line when
    /// nothing has unlocked yet. Reads only: the session's owned `ledger`
    /// and `knowledge` are both untouched (the purity law, spec §4.3);
    /// this method takes `&self`, not `&mut self`. Threaded (The Shuttle):
    /// calls the `_from` twin of each with `self.terrain`/`self.climate`
    /// when both are `Some`, so a session's repeated `consult`/`write`
    /// calls share one sculpt instead of re-sculpting the globe every turn;
    /// falls back to the re-sculpting bare form on the `None` a failed
    /// build at `start` would leave.
    fn consult(&self) -> String {
        let day = self.day.day.trunc() as u64;
        let mut lines = vec![format!("The Reckoning, at day {day}.")];
        let at = hornvale_astronomy::StdDays::new(self.day.day)
            .expect("a session's day is always finite and non-negative");
        let epoch = match (self.terrain.as_ref(), self.climate.as_ref()) {
            (Some(t), Some(c)) => hornvale_book::reckoning_at_from(self.world, at, t, c),
            _ => hornvale_book::reckoning_at(self.world, at),
        };
        lines.extend(epoch.lines);
        lines.extend(epoch.margin);
        let reader = reader_set(&self.knowledge);
        let initiated = match (self.terrain.as_ref(), self.climate.as_ref()) {
            (Some(t), Some(c)) => hornvale_book::esoteric_lines_from(self.world, &reader, t, c),
            _ => hornvale_book::esoteric_lines(self.world, &reader),
        };
        if initiated.is_empty() {
            lines.push(CONSULT_FALLBACK.to_string());
        } else {
            lines.extend(initiated);
        }
        lines.join("\n")
    }

    fn knows(&self) -> String {
        let mut lines = vec![format!("{} things seen:", self.knowledge.0.len())];
        for (key, value) in &self.knowledge.0 {
            // char-safe truncation: byte slicing can split a UTF-8 boundary.
            let shown: String = if value.chars().count() > 60 {
                let head: String = value.chars().take(57).collect();
                format!("{head}…")
            } else {
                value.clone()
            };
            lines.push(format!("  {key} = {shown}"));
        }
        lines.join("\n")
    }

    fn out(&self, r: Result<String, VesselError>) -> Turn {
        match r {
            Ok(s) => Turn::Out(s),
            Err(e) => Turn::Out(format!("error: {e}")),
        }
    }
}

/// The arousal above which a still-Content (sub-act) creature reads as restless
/// rather than calm — the rising edge of a need felt before it is acted on.
const RESTLESS_AROUSAL: f64 = 0.4;

/// Render a creature's `Affect` as a felt-state phrase (spec §7): the
/// circumplex label coloured by its intentional object — what the feeling is
/// *about* — so a reader sees not just *that* it frets but *what for*. The
/// object/reason is the debuggable "message" a distressed creature emits.
fn felt_phrase(affect: &Affect) -> String {
    // Pick the object-appropriate wording (thirst / thermal / fatigue / hunger
    // / danger / social / none).
    let about = |thirst: &str,
                 thermal: &str,
                 fatigue: &str,
                 hunger: &str,
                 danger: &str,
                 social: &str,
                 none: &str| {
        match affect.object {
            Some(DriveKind::Thirst) => thirst,
            Some(DriveKind::Thermal) => thermal,
            Some(DriveKind::Fatigue) => fatigue,
            Some(DriveKind::Hunger) => hunger,
            Some(DriveKind::Danger) => danger,
            Some(DriveKind::Social) => social,
            None => none,
        }
        .to_string()
    };
    match affect.label {
        // Below the seek threshold the creature is puttering — but arousal still
        // rises with the need, so a reader can tell true calm from the restless
        // edge before it starts to act.
        AffectLabel::Content if affect.arousal >= RESTLESS_AROUSAL => "grows restless".to_string(),
        AffectLabel::Content => "seems content".to_string(),
        AffectLabel::Eager => about(
            "drinks its fill",
            "settles into a kinder warmth",
            "settles down to rest",
            "eats its fill",
            "reaches safer ground",
            "makes for home and its people",
            "looks pleased",
        ),
        AffectLabel::Searching => about(
            "casts about for water",
            "casts about for a kinder clime",
            "trudges wearily homeward",
            "forages for richer ground",
            "edges away from the uncanny ground",
            "drifts homeward, missing its people",
            "wanders, searching",
        ),
        AffectLabel::Frustrated => about(
            "frets, wanting water it cannot reach",
            "shivers, with no warmth within reach",
            "frets, too far from any rest",
            "frets, famished, with no food in reach",
            "recoils, hemmed in by dread on every side",
            "frets, cut off from home and its people",
            "frets, blocked at every turn",
        ),
        AffectLabel::Lost => "looks lost, unsure where to turn".to_string(),
        AffectLabel::Helpless => about(
            "has given up on water",
            "has given up on warmth",
            "has given up, bone-weary",
            "has given up, starving",
            "has given up, cowering",
            "has given up on ever getting home",
            "has given up",
        ),
    }
}

/// A chamber's packed room id, for the blocks that print one.
///
/// One place rather than two: `RoomAddrError` implements `Debug` but not
/// `Display` (the constraint `snapshot` documents at its own `pack` call), so the
/// mapping has a shape worth stating once — and the chamber block and the plan
/// block must print the same id for the same chamber.
fn chamber_id(chamber: &RoomAddr) -> Result<u64, VesselError> {
    Ok(chamber
        .pack()
        .map_err(|e| VesselError::Build(format!("{e:?}")))?
        .0)
}

/// The reader-facing word for a stratum.
/// type-audit: bare-ok(prose: return)
fn stratum_word(s: hornvale_climate::Stratum) -> &'static str {
    use hornvale_climate::Stratum;
    match s {
        Stratum::Surface => "the surface",
        Stratum::Epipelagic => "sunlit water",
        Stratum::Mesopelagic => "the twilight water",
        Stratum::Bathypelagic => "the lightless water",
        Stratum::Abyssal => "the abyss",
        Stratum::Hadal => "a trench",
        Stratum::Regolith => "the regolith",
        Stratum::Cover => "the cover rock",
        Stratum::Basement => "the basement rock",
        Stratum::Roots => "the roots of the world",
        Stratum::Underneath => "the underneath",
    }
}

/// Parse a compass token (case-insensitive, long names allowed).
/// The four bearings a mover may take between CELLS, in `HEADINGS`-ish order:
/// north first because that is how a reader scans the drawn plan.
///
/// A subset of [`Compass`] rather than a type of its own. The player's vocabulary
/// is one compass at both bands — a step indoors and a step outdoors are typed the
/// same way (§6.1 rejects a second `step` verb for exactly this reason) — so the
/// bands differ in what they DO with a bearing, never in how it is spelled.
const COMPASS_SQUARE: [Compass; 4] = [Compass::N, Compass::E, Compass::S, Compass::W];

/// A compass bearing as a CELL delta, or `None` for a diagonal.
///
/// **North is `-y`**, matching the render, which draws row `y` at line `y` from
/// the top. Getting this backwards produces a world that is internally consistent
/// and mirrored — every test about walls and doorways still passes, and only a
/// reader comparing a step against the drawn plan would ever notice — so it is
/// asserted against the picture (`a_step_north_moves_the_mark_up_the_picture`)
/// rather than against another copy of this table.
///
/// `None` for the diagonals is the honest answer, not an omission:
/// [`crate::lattice::HEADINGS`] is orthogonal because a diagonal step through the
/// corner where two walls meet is not a way through a building.
fn cell_delta(c: Compass) -> Option<(i32, i32)> {
    match c {
        Compass::N => Some((0, -1)),
        Compass::E => Some((1, 0)),
        Compass::S => Some((0, 1)),
        Compass::W => Some((-1, 0)),
        Compass::Ne | Compass::Se | Compass::Sw | Compass::Nw => None,
    }
}

/// A bearing spelled out, for a sentence: `north`.
fn bearing_word(c: Compass) -> &'static str {
    match c {
        Compass::N => "north",
        Compass::Ne => "north-east",
        Compass::E => "east",
        Compass::Se => "south-east",
        Compass::S => "south",
        Compass::Sw => "south-west",
        Compass::W => "west",
        Compass::Nw => "north-west",
    }
}

/// A bearing abbreviated, for a list: `N`. The SAME spelling the locale's own
/// `Ways on:` footer uses (`describe_here` uppercases the debug name), so one
/// player habit reads both bands.
fn bearing_letter(c: Compass) -> String {
    format!("{c:?}").to_uppercase()
}

fn parse_compass(s: &str) -> Option<Compass> {
    match s.to_lowercase().as_str() {
        "n" | "north" => Some(Compass::N),
        "ne" | "northeast" => Some(Compass::Ne),
        "e" | "east" => Some(Compass::E),
        "se" | "southeast" => Some(Compass::Se),
        "s" | "south" => Some(Compass::S),
        "sw" | "southwest" => Some(Compass::Sw),
        "w" | "west" => Some(Compass::W),
        "nw" | "northwest" => Some(Compass::Nw),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_astronomy::SkyPins;
    use hornvale_terrain::TerrainPins;
    use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

    fn seam_world() -> World {
        build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 42 builds")
    }

    fn world_at(seed: u64) -> Option<World> {
        build_world(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .ok()
    }

    /// The XOR applied to `Inside::seed` by
    /// [`perturbing_the_embedding_moves_what_is_drawn_and_not_what_is_known`]
    /// and by the search that picks its world. An arbitrary constant — its only
    /// job is to be a DIFFERENT draw of the same placement — but it must be the
    /// same constant in both places, or the search filters on one experiment
    /// while the test runs another.
    /// type-audit: bare-ok(constructor-edge)
    const PERTURBATION: u64 = 0x5169_4741_u64;

    /// The seeds [`world_where`] searches. Wide enough that "no world in here
    /// draws a creature" is a finding about the sim rather than about the
    /// sample, and cheap in practice because the search stops at its first hit
    /// — 19 of the first 24 seeds qualify.
    const SIGHT_SEEDS: std::ops::Range<u64> = 0..64;

    /// The first seed in [`SIGHT_SEEDS`] whose fresh possession satisfies
    /// `pred`, with the world it was built from.
    ///
    /// **Why a search and not a seed.** These tests originally stood on seed
    /// 42, on the accident that its opening chamber happened to hold a
    /// creature after one tick. The Tense reseeded that world and the accident
    /// went away — sight was untouched and still worked on most seeds, but the
    /// evidence for it had been pinned to one world that stopped exercising it.
    /// The sibling batteries in `lattice::anchor_cells` already sweep
    /// `0u64..64` rather than assert over one fixture; this is that idiom,
    /// applied to whole worlds.
    ///
    /// It panics, naming `what` and the range, when nothing matches. A sweep
    /// that quietly found nothing and let its caller pass would be strictly
    /// worse than the hardcoded seed it replaces: the loud preconditions are
    /// what caught the reseed.
    fn world_where(what: &str, pred: impl Fn(&mut Session<'_>) -> bool) -> (u64, World) {
        for seed in SIGHT_SEEDS {
            let Some(world) = world_at(seed) else {
                continue;
            };
            let hit = {
                let Ok((mut session, _)) = Session::start(&world, &PossessOpts::default()) else {
                    continue;
                };
                session.handle("wait");
                session.handle("enter");
                session.inside.is_some() && pred(&mut session)
            };
            if hit {
                return (seed, world);
            }
        }
        panic!(
            "no seed in {SIGHT_SEEDS:?} produces a world where {what} — the \
             search found nothing, so nothing below could be tested. That is a \
             finding about the sim, not a flaky fixture."
        );
    }

    /// The regression this pins: `examine` must be able to tell "the lens
    /// itself failed" from "no grain surfaced that noun" — before this fix,
    /// `lens_nouns` swallowed both `focalized()`'s and `purview(0)`'s errors
    /// into a bare empty `Vec`, so a genuine lens failure rendered as the
    /// same "You see no <noun> here." as an honest absence. We force
    /// `focalized()` to fail by corrupting the possessed agent's own
    /// position with an out-of-range path digit (`RoomAddr::pack` rejects
    /// any digit >= 4 — see `kernel/src/room.rs`), which `LocaleContext::
    /// describe` hits on its very first line, well before any geometry
    /// runs. This mutates the session's private field directly (this test
    /// lives inside the `session` module for exactly that access) rather
    /// than reaching for a public setter that would let ordinary callers
    /// corrupt a session's position too.
    #[test]
    fn examine_reports_a_genuine_lens_failure_loudly_not_as_an_absence() {
        let w = seam_world();
        let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        // Sanity: examine must work normally before we break anything.
        assert!(
            session.focalized().is_ok(),
            "the fixture session must start in a healthy state"
        );
        session.agent.position.path.push(99);
        assert!(
            session.focalized().is_err(),
            "the corrupted position must actually break the lens, or this \
             test proves nothing"
        );
        let reply = match session.handle("examine anything") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("examine must not release"),
        };
        assert!(
            reply.starts_with("error:"),
            "a lens failure must be reported loudly, not read as an \
             absence: got {reply:?}"
        );
        assert!(
            !reply.starts_with("You see no"),
            "a lens failure must never masquerade as 'nothing here': got \
             {reply:?}"
        );
    }

    /// The chart legend resolves by word, not by whole string. This arm is a
    /// SECOND matcher, separate from the prose catalog's, and The Handle's plan
    /// changed only the first — so a walker could `examine forest` but not
    /// `examine bugbear`, with the mark's full name sitting in the legend the
    /// `map` verb had just printed. Two matchers for one question is how they
    /// drift; this pins the second to the same rule as the first.
    #[test]
    fn a_legend_mark_resolves_by_word_and_not_only_by_its_whole_name() {
        let w = seam_world();
        let (session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        let scene = session.purview(0).expect("the chart builds");
        let mark = scene
            .legend
            .iter()
            .find(|e| e.noun.split_whitespace().count() > 1)
            .expect("some legend entry is a multi-word name");
        let head = mark
            .noun
            .split_whitespace()
            .next()
            .expect("a multi-word name has a first word")
            .to_lowercase();
        let reply = match session.examine(&head) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("examine must not release"),
        };
        assert!(
            !reply.starts_with("You see no"),
            "the legend names {:?} and examine refuses its first word {head:?}: {reply}",
            mark.noun
        );
    }

    /// The two grains must not disagree about a WORD. The chart legend keys its
    /// ground mark on the whole raw descriptor, so deriving words from it
    /// re-admits the qualifiers the prose entry declined by declaring only its
    /// noun phrase. Before the duplicate-skip, `examine hollow` was refused by
    /// the prose catalog and then answered by the legend's copy of the same
    /// name — same room, same thing, two different answers depending on which
    /// matcher got there.
    #[test]
    fn a_qualifier_the_prose_entry_declined_is_not_readmitted_by_the_legend() {
        let w = seam_world();
        let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        // Walk until the descriptor carries a qualifier; the flagship's own
        // ("buttressed canopy") has none, so it cannot exercise this.
        let mut qualifier = None;
        for _ in 0..8 {
            let prose = session.focalized().expect("the lens renders");
            let qualified = prose
                .nouns
                .iter()
                .find_map(|n| n.display.split_once(", ").map(|(_, tail)| tail.to_string()));
            if let Some(tail) = qualified {
                qualifier = tail
                    .split(|c: char| !c.is_alphanumeric())
                    .find(|w| w.chars().count() >= 4)
                    .map(str::to_lowercase);
                if qualifier.is_some() {
                    break;
                }
            }
            let _ = session.handle("go n");
        }
        let Some(word) = qualifier else {
            // Not a pass: say so rather than reporting green on nothing.
            panic!("no comma-qualified descriptor within 8 rooms of the flagship");
        };
        let reply = match session.handle(&format!("examine {word}")) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("examine must not release"),
        };
        assert!(
            reply.starts_with("You see no"),
            "{word:?} is a qualifier, not a noun, and the legend re-admitted it: {reply}"
        );
    }

    /// The ordinary path still refuses cleanly when both grains genuinely
    /// have nothing to say — `lens_nouns`'s new `Result` must not turn every
    /// refusal into an `Err`.
    #[test]
    fn examine_still_refuses_plainly_when_nothing_is_wrong() {
        let w = seam_world();
        let (session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        let reply = match session.examine("a-noun-no-grain-surfaced") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("examine must not release"),
        };
        assert!(
            reply.starts_with("You see no"),
            "a healthy lens with no matching noun must refuse plainly: \
             got {reply:?}"
        );
    }

    #[test]
    fn the_opening_snapshot_carries_every_channel() {
        let world = seam_world();
        let (session, opening) = Session::start(&world, &PossessOpts::default()).unwrap();
        let snap = session.snapshot().expect("a live session snapshots");

        assert_eq!(snap.schema, crate::SESSION_SCHEMA);
        assert_eq!(snap.turn, 0, "the opening is turn 0");
        assert_eq!(snap.day, 0.5, "PossessOpts::default() is noon");
        assert!(!snap.me.species.is_empty());
        assert_eq!(snap.me.room, session.agent().position.pack().unwrap().0);
        assert!(!snap.sensed.sky.is_empty());
        assert!(
            !snap.known.entries.is_empty(),
            "the opening projection lands"
        );
        assert_eq!(
            snap.social.len(),
            session.npc_labels().len(),
            "social covers every derived NPC, co-located or not"
        );
        assert!(
            snap.social.iter().all(|s| s.grievance == 0.0 && !s.hostile),
            "an unprovoked world starts at zero grievance"
        );
        assert_eq!(
            snap.narration.prose.trim(),
            opening.trim(),
            "narration.prose IS the opening text"
        );
    }

    #[test]
    fn the_turn_counter_advances_with_committed_turns() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        assert_eq!(session.snapshot().unwrap().turn, 0);
        session.handle("look");
        assert_eq!(session.snapshot().unwrap().turn, 1);
        session.handle("whoami");
        assert_eq!(session.snapshot().unwrap().turn, 2);
    }

    #[test]
    fn wait_populates_occupancy_for_every_derived_npc() {
        // The Threshold whole-branch review, Important 4: `wait` used to run
        // `tick()` alone and discard `DriveMovements::step_with_occupancy`'s
        // own within-room `Occupancy` — so `needs()` and the snapshot's
        // present-entry read always fell back to a co-located NPC's room-
        // landing anchor, regardless of where its own walk that tick
        // actually carried it. This pins the wiring directly, at the level
        // where the bug lived: before any `wait`, `self.occupancy` is
        // still its post-`start` empty default (nothing has ever run the
        // walk); after one `wait`, every derived npc must have a tracked
        // within-room anchor, because `wait` now captures
        // `step_with_occupancy`'s second element instead of throwing it
        // away. Without the fix this assertion is never reached — the
        // `before` check alone would still pass, since `self.occupancy`
        // would stay empty forever.
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        assert!(
            session
                .npcs
                .iter()
                .all(|n| session.occupancy.at(n.entity).is_none()),
            "before any `wait`, occupancy has never been populated"
        );
        session.wait("1");
        for npc in &session.npcs {
            assert!(
                session.occupancy.at(npc.entity).is_some(),
                "after `wait`, every derived npc must have a tracked within-room anchor: {}",
                npc.label
            );
        }
    }

    #[test]
    fn an_unprovoked_npcs_grievance_is_not_negative_zero() {
        // Names the invariant `grievance`'s own fold comment explains: a
        // revert to `.sum::<f64>()` (which folds from `-0.0`) would only show
        // up as a large fixture diff without this assertion (The Snapshot
        // chronicle).
        let world = seam_world();
        let (session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        for npc in &session.npcs {
            let g = grievance(&session.ledger, npc.entity);
            assert_eq!(g, 0.0);
            assert!(
                !g.is_sign_negative(),
                "an unprovoked NPC's grievance must be plain 0.0, not -0.0"
            );
        }
    }

    #[test]
    fn a_blank_line_clobbers_neither_turn_nor_narration() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        session.handle("whoami");
        let before = session.snapshot().unwrap();
        session.handle("");
        let after = session.snapshot().unwrap();
        assert_eq!(before.turn, after.turn, "a blank line commits no turn");
        assert_eq!(
            before.narration.prose, after.narration.prose,
            "a blank line must not clobber the last verb's own narration"
        );
    }

    #[test]
    fn a_snapshot_is_pure_taken_twice() {
        let world = seam_world();
        let (session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let a = crate::snapshot_json(&session.snapshot().unwrap());
        let b = crate::snapshot_json(&session.snapshot().unwrap());
        assert_eq!(a, b, "the read is pure — no hidden state advances");
    }

    #[test]
    fn provoking_shows_up_in_the_social_channel() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let before = session.snapshot().unwrap();
        assert!(before.social.iter().all(|s| s.grievance == 0.0));
        session.handle("provoke");
        let after = session.snapshot().unwrap();
        assert!(
            after.social.iter().any(|s| s.grievance > 0.0),
            "a provoked NPC's grievance surfaces in `social`"
        );
    }

    #[test]
    fn narration_follows_the_verb_not_the_room() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        session.handle("whoami");
        let snap = session.snapshot().unwrap();
        assert!(
            snap.narration.prose.starts_with("A "),
            "after `whoami` the narration is the whoami answer, not the room block: {:?}",
            snap.narration.prose
        );
        assert!(
            !snap.narration.prose.starts_with("[room "),
            "the room block must NOT be substituted for a verb's own response"
        );
        session.handle("look");
        let snap = session.snapshot().unwrap();
        assert!(
            snap.narration.prose.starts_with("[room "),
            "after `look` the narration IS the room block"
        );
    }

    // ---- The Lintel: the chamber naming and resolution layer -------------
    //
    // These cover the layer BETWEEN `structure_at`'s link graph and the
    // player's typing. It is where a whole-branch review found multi-chamber
    // structures partly unnavigable: every chamber of a structure derives the
    // identical interior (terrain is read at their shared walk-band ancestor),
    // so noun-named apertures were indistinguishable, the ways-on list
    // deduplicated them to one, and `enter <noun>` from chamber 1 resolved
    // back to the threshold — stranding chambers 2 and 3 with no input that
    // could reach them.

    /// A walk-band address to hang a synthetic structure under. Which locale it
    /// is does not matter to the link graph; the resolution tests pass the
    /// session's OWN position instead, so the interiors they read are real.
    fn synthetic_locale() -> RoomAddr {
        RoomAddr {
            face: 3,
            path: (0..12).map(|i| (i % 4) as u8).collect(),
        }
    }

    /// A `Structure` of `count` chambers under `base`, linked as the path graph
    /// rooted at the threshold that `structure_at` builds. Synthetic because
    /// `structure_at`'s own count is a seed draw, and the naming layer must hold
    /// for every count — so these tests choose it rather than hoping for it.
    fn path_structure(base: &RoomAddr, count: usize) -> crate::structure::Structure {
        assert!(
            (1..=crate::structure::MAX_CHAMBERS).contains(&count),
            "a chamber index is one base-4 path digit"
        );
        let chambers: Vec<RoomAddr> = (0..count)
            .map(|i| {
                let mut path = base.path.clone();
                path.extend(std::iter::repeat_n(
                    0u8,
                    crate::band::CHAMBER_DEPTH_OFFSET as usize,
                ));
                let last = path.len() - 1;
                path[last] = i as u8;
                RoomAddr {
                    face: base.face,
                    path,
                }
            })
            .collect();
        crate::structure::Structure {
            threshold: chambers[0].clone(),
            links: (1..count).map(|i| (i - 1, i)).collect(),
            chambers,
        }
    }

    #[test]
    fn neighbours_reads_the_link_graph_in_both_directions() {
        let s = path_structure(&synthetic_locale(), 4);
        assert_eq!(
            Session::neighbours(&s, 0),
            vec![1],
            "the threshold has one aperture"
        );
        assert_eq!(
            Session::neighbours(&s, 1),
            vec![0, 2],
            "a middle chamber has two: back and further in"
        );
        assert_eq!(
            Session::neighbours(&s, 3),
            vec![2],
            "the innermost chamber has one"
        );
    }

    #[test]
    fn further_in_is_the_deeper_aperture_and_stops_at_the_last_chamber() {
        let s = path_structure(&synthetic_locale(), 3);
        assert_eq!(Session::further_in(&s, 0), Some(1));
        assert_eq!(
            Session::further_in(&s, 1),
            Some(2),
            "from a middle chamber, deeper is the HIGHER index, never the way back"
        );
        assert_eq!(
            Session::further_in(&s, 2),
            None,
            "nothing lies deeper than the last chamber"
        );
    }

    #[test]
    fn named_neighbour_walks_further_in_from_a_middle_chamber() {
        let world = seam_world();
        let (session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let s = path_structure(&session.agent.position, 4);
        for word in FURTHER_IN_WORDS {
            assert_eq!(
                session.named_neighbour(&s, 1, word),
                Some(2),
                "{word:?} must resolve deeper, never back toward the threshold"
            );
        }
        // Case and surrounding space are the player's, not the parser's.
        assert_eq!(session.named_neighbour(&s, 1, "  Further In  "), Some(2));
    }

    #[test]
    fn a_bare_noun_refuses_while_two_apertures_are_open() {
        let world = seam_world();
        let (session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let s = path_structure(&session.agent.position, 4);
        // Precondition: the noun really IS in the neighbouring chamber's prose,
        // so the refusal below is about ambiguity, not about an absent word.
        let terrain = session.terrain_here();
        let brief = session.brief_here();
        let nouns = crate::chamber_prose::chamber_nouns(&crate::interior::chamber_interior_of(
            &s.chambers[2],
            &terrain,
            session.walk_depth(),
            &brief,
            2,
        ));
        let noun = *nouns
            .first()
            .expect("a built chamber's prose names something");
        assert_eq!(
            session.named_neighbour(&s, 1, noun),
            None,
            "an ambiguous noun must refuse, not silently pick a direction"
        );
        assert_eq!(
            session.named_neighbour(&s, 0, noun),
            Some(1),
            "with exactly one aperture the same noun is unambiguous, and accepted"
        );
    }

    #[test]
    fn an_unmatched_name_refuses_rather_than_choosing_a_destination() {
        let world = seam_world();
        let (session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let s = path_structure(&session.agent.position, 2);
        assert_eq!(
            session.named_neighbour(&s, 0, "a-noun-no-chamber-holds"),
            None
        );
    }

    #[test]
    fn the_ways_on_inside_name_out_and_the_deeper_aperture() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let middle = path_structure(&session.agent.position, 3);
        session.descend(middle, 1).expect("a chamber to stand in");
        let text = session.describe_chamber_here().expect("a chamber renders");
        assert!(
            text.ends_with("Ways on: out, further in."),
            "a middle chamber must offer BOTH directions under distinct names: {text:?}"
        );
        let innermost = path_structure(&session.agent.position, 3);
        session
            .descend(innermost, 2)
            .expect("a chamber to stand in");
        let text = session.describe_chamber_here().expect("a chamber renders");
        assert!(
            text.ends_with("Ways on: out."),
            "the innermost chamber must not advertise a way that is not there: {text:?}"
        );
    }

    #[test]
    fn a_refusal_in_a_middle_chamber_names_the_tokens_that_work() {
        // The falsity this replaced: "There is no way to <noun> from here.",
        // said in a room with two ways, about a noun the room's own prose had
        // just listed. Both refusals that land there — bare, and an ambiguous
        // noun — must name the tokens that move instead of denying the ways.
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let middle = path_structure(&session.agent.position, 3);
        session.descend(middle, 1).expect("a chamber to stand in");
        for line in ["enter", "enter doorway"] {
            let reply = match session.handle(line) {
                Turn::Out(t) => t,
                Turn::Released(_) => panic!("enter must not release"),
            };
            assert!(
                reply.contains("two ways") && reply.contains("further in"),
                "{line:?} must name the tokens that work: {reply:?}"
            );
            assert!(
                !reply.contains("There is no way"),
                "a room with two ways must not deny that they exist: {reply:?}"
            );
        }
    }

    #[test]
    fn every_chamber_is_reachable_from_the_threshold_by_input() {
        // `Structure`'s doc promises "every chamber is reachable from
        // `threshold`" — a claim about `links`. This is the player-facing
        // version: reachable BY INPUT, which is the half that was false.
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let reply = match session.handle("enter") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("enter must not release"),
        };
        assert!(
            !reply.starts_with("Nothing here is built"),
            "the flagship's own locale is built: {reply:?}"
        );
        let total = session
            .inside
            .as_ref()
            .expect("a successful enter is inside something")
            .structure
            .chambers
            .len();
        // `total` is a seed draw over `1..=MAX_CHAMBERS`. At 1 the loop below
        // never runs and the visited-set assertion passes trivially — the exact
        // vacuity shape this round fixed elsewhere — so pin the fixture instead
        // of trusting today's draw.
        assert!(
            total > 1,
            "fixture must draw a multi-chamber structure for this test to mean anything"
        );
        let mut visited = std::collections::BTreeSet::new();
        visited.insert(session.inside.as_ref().unwrap().at);
        for _ in 1..total {
            session.handle("enter further in");
            visited.insert(session.inside.as_ref().unwrap().at);
        }
        assert_eq!(
            visited.len(),
            total,
            "every chamber must be reachable by input; visited {visited:?} of {total}"
        );
        let wall = match session.handle("enter further in") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("enter must not release"),
        };
        assert_eq!(
            wall, "This is as far in as the place goes.",
            "the far end names itself rather than reading as a parse failure"
        );
    }

    #[test]
    fn examine_indoors_answers_what_the_chamber_names_and_refuses_the_rest_in_one_voice() {
        // The Lintel's version of this test asserted the reply was
        // `INDOOR_EXAMINE_REFUSAL` — an honest statement of a real limit while
        // nothing authored a detail for a chamber's nouns. This campaign authors
        // them, so the test is REWRITTEN rather than deleted: the noun `look` just
        // named must be ACCEPTED, and an unknown noun must be refused in the
        // OUTDOOR wording, byte for byte. Two wordings for one question is the
        // drift §6 exists to prevent.
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let shown = match session.handle("enter") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("enter must not release"),
        };
        assert!(
            !shown.starts_with("Nothing here is built"),
            "the flagship's own locale is built: {shown:?}"
        );
        // Take a noun the chamber's prose has just named to the player.
        let terrain = session.terrain_here();
        let brief = session.brief_here();
        let interior = crate::interior::chamber_interior_of(
            &session.inside.as_ref().unwrap().structure.chambers
                [session.inside.as_ref().unwrap().at],
            &terrain,
            session.walk_depth(),
            &brief,
            session.inside.as_ref().unwrap().at,
        );
        let nouns = crate::chamber_prose::chamber_nouns(&interior);
        let noun = *nouns
            .first()
            .expect("a built chamber's prose names something");
        assert!(
            shown.contains(noun),
            "the precondition is that `look` NAMED this noun: {noun:?} not in {shown:?}"
        );
        let reply = match session.handle(&format!("examine {noun}")) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("examine must not release"),
        };
        assert!(
            !reply.starts_with("You see no"),
            "look must not name what examine denies, two turns apart: {reply:?}"
        );
        assert_eq!(
            reply,
            crate::chamber_prose::detail(
                interior
                    .ids()
                    .iter()
                    .map(|&id| interior.anchor(id).kind)
                    .find(|&k| crate::chamber_prose::noun(k) == Some(noun))
                    .expect("the noun came from this interior")
            ),
            "the reply must be the AUTHORED detail for the anchor the noun names, \
             not a generic acknowledgement"
        );
        // A noun the chamber does not hold is refused, and refused in the SAME
        // words the outdoor path uses — asserted by equality against that path
        // rather than by a shared prefix, which two drifting wordings would still
        // satisfy.
        let unknown = "a-noun-no-grain-surfaced";
        let refused_indoors = match session.handle(&format!("examine {unknown}")) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("examine must not release"),
        };
        assert_eq!(refused_indoors, format!("You see no {unknown} here."));
        // A BARE `examine` names nothing, so the band guard must not swallow it:
        // "Examine what?" is as true indoors as out, and the refusal above is an
        // answer to a question the player did not ask.
        let bare = match session.handle("examine") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("examine must not release"),
        };
        assert_eq!(
            bare, "Examine what?",
            "a bare `examine` keeps its usage hint indoors"
        );
        // Out of doors the ordinary path is untouched.
        session.handle("out");
        let outdoors = match session.handle("examine a-noun-no-grain-surfaced") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("examine must not release"),
        };
        assert!(
            outdoors.starts_with("You see no"),
            "the outdoor examine path is unchanged: {outdoors:?}"
        );
        // And the same equality out of doors, so the two paths are pinned to one
        // sentence rather than to one prefix.
        assert_eq!(outdoors, refused_indoors);
    }

    #[test]
    fn the_plan_is_keyed_to_the_locale_not_the_world() {
        // THE trap this helper exists to avoid. `allocate` reads only the chamber
        // count, the links and the seed, so two structures of the same shape
        // produce the same lattice unless the SEED differs. Keyed to
        // `self.world.seed`, every building in the world would get one identical
        // floor plan — self-consistent, all eight rules green, and uniformly
        // wrong. That makes this a real falsifier rather than the near-tautology
        // `structure.rs` flags at `a_different_locale_gives_a_different_structure`,
        // where the locale's path is inherited into the answer by construction.
        //
        // Eight locales rather than two, and the weaker claim rather than a
        // pairwise one: the legal cut band is only about seven positions wide at
        // this extent, so two locales agreeing is a coin flip and asserting they
        // differ would be flaky. Assert the property that actually matters — the
        // locale is read at all.
        let world = seam_world();
        let (session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let here = session.agent.position.clone();
        let mut plans = Vec::new();
        let mut locales = std::collections::BTreeSet::new();
        for i in 0..8u8 {
            let mut path = here.path.clone();
            path[0] = (path[0] + i % 4) % 4;
            path[1] = (path[1] + i / 4) % 4;
            let locale = RoomAddr {
                face: here.face,
                path,
            };
            assert!(locales.insert(locale.path.clone()), "locale {i} repeats");
            // Asked of the PLACE rather than of a session put inside it, which is
            // what `lattice_of` taking a structure buys: the derivation is pinned
            // without mutating the session at all.
            let structure = path_structure(&locale, 2);
            let plan = session.lattice_of(&structure);
            // Purity, at each locale, before the difference below means anything:
            // a plan that varied between two calls would make "eight differ" true
            // for the wrong reason.
            assert_eq!(
                plan,
                session.lattice_of(&structure),
                "the plan at locale {i} is not a pure function of the place"
            );
            plans.push(plan);
        }
        assert!(
            plans.iter().any(|p| *p != plans[0]),
            "eight different built locales derived the SAME floor plan, so the \
             lattice is keyed to the world's seed and every building in the world \
             looks alike"
        );
    }

    #[test]
    fn the_plan_does_not_change_as_the_possession_walks_deeper() {
        // The plan is a property of the STRUCTURE, so `lattice_of` keys it to the
        // threshold's locale rather than to the chamber stood in. Keyed to the
        // chamber instead, the building would redraw itself every time the player
        // stepped through a door.
        //
        // Asserted through the CARRIED copy as well as through the derivation, now
        // that `Inside` holds one: a plan that were re-derived per chamber would
        // fail the first assertion, and a carried copy that went stale as the
        // possession walked would fail the second.
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let s = path_structure(&session.agent.position, 3);
        let from_threshold = session.lattice_of(&s);
        for at in 0..3 {
            session
                .descend(s.clone(), at)
                .expect("a chamber to stand in");
            assert_eq!(
                session.lattice_of(&s),
                from_threshold,
                "the plan redrew itself on stepping into chamber {at}"
            );
            assert_eq!(
                session.inside.as_ref().unwrap().lattice,
                from_threshold,
                "the lattice carried in chamber {at} is not the one the place derives"
            );
        }
    }

    #[test]
    fn map_indoors_draws_the_plan_and_map_out_indoors_refuses() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        session
            .descend(path_structure(&session.agent.position, 2), 0)
            .expect("a chamber to stand in");
        let plan = match session.handle("map") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("map must not release"),
        };
        assert!(plan.starts_with("[plan: chamber "), "{plan}");
        assert!(plan.contains("legend: "), "{plan}");
        for line in ["map out", "map out 2"] {
            let refused = match session.handle(line) {
                Turn::Out(t) => t,
                Turn::Released(_) => panic!("map must not release"),
            };
            assert_eq!(
                refused, INDOOR_CHART_REFUSAL,
                "{line:?} indoors must refuse rather than ignore the argument"
            );
        }
        // Out of doors both paths are untouched. The default eyes are `Own`
        // (colour on), so the walk-band chart now draws the colour lens
        // (The Beholding, Task 5) — this assertion used to read "terrain"
        // and was wrong for the default path once that shipped.
        session.handle("out");
        let chart = match session.handle("map") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("map must not release"),
        };
        assert!(chart.contains("[lens: colour"), "{chart}");
        // `eyes off` falls all the way back to the plain terrain lens.
        session.handle("eyes off");
        let bare = match session.handle("map") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("map must not release"),
        };
        assert!(bare.contains("[lens: terrain"), "{bare}");
    }

    #[test]
    fn a_compass_step_indoors_never_touches_the_walk_band() {
        // The Lintel's `lateral_movement_indoors_is_refused_and_leaves_the_
        // possession_inside`, rewritten rather than deleted. Its `go` half asserted
        // the REFUSAL, which The Blocking reverses; the claim underneath it is what
        // actually mattered and it survives untouched: §1b.6 says lateral movement
        // never changes band, and a cell step stays inside the chamber band. So
        // `go n` indoors must move the CELL and nothing else — unguarded, it
        // rendered the neighbouring LOCALE and cleared `inside` on the way, and the
        // player left the building with no sentence saying so.
        //
        // `back` keeps its refusal, because it retraces a walk-band trail whatever
        // the interior looks like.
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let shown = match session.handle("enter") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("enter must not release"),
        };
        assert!(
            !shown.starts_with("Nothing here is built"),
            "the flagship's own locale is built: {shown:?}"
        );
        let structure = session
            .inside
            .as_ref()
            .expect("a successful enter is inside")
            .structure
            .clone();
        // Give `back` somewhere to retrace to (walking there first would have
        // left the built locale), so its refusal below cannot be the vacuous
        // "You have not walked anywhere yet.": the guard must refuse it even
        // when there IS a trail.
        let elsewhere = session
            .agent
            .position
            .neighbors()
            .into_iter()
            .next()
            .expect("a locale has neighbours");
        session.trail.push(elsewhere.clone());
        let here = session.agent.position.clone();
        for line in ["go n", "go north", "go ne", "back"] {
            let reply = match session.handle(line) {
                Turn::Out(t) => t,
                Turn::Released(_) => panic!("{line:?} must not release"),
            };
            assert!(
                !reply.starts_with("[room "),
                "{line:?} indoors must not render a LOCALE: {reply:?}"
            );
            assert_eq!(
                session.inside.as_ref().map(|i| &i.structure),
                Some(&structure),
                "{line:?} must leave the possession in the building it was in"
            );
            assert_eq!(
                session.agent.position, here,
                "{line:?} must not move the walk-band position"
            );
            assert_eq!(
                session.trail,
                vec![elsewhere.clone()],
                "{line:?} must not consume the walk-band trail"
            );
        }
        // And the two refusals that remain say what they are. A diagonal is
        // geometry (`HEADINGS` is orthogonal); `back` is a band.
        session.handle("out");
        session.handle("enter");
        assert_eq!(
            match session.handle("go ne") {
                Turn::Out(t) => t,
                Turn::Released(_) => panic!("go must not release"),
            },
            INDOOR_DIAGONAL_REFUSAL
        );
        assert_eq!(
            match session.handle("back") {
                Turn::Out(t) => t,
                Turn::Released(_) => panic!("back must not release"),
            },
            INDOOR_BACK_REFUSAL
        );
        // Out of doors `back` works exactly as before.
        session.handle("out");
        let retraced = match session.handle("back") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("back must not release"),
        };
        assert!(
            retraced.starts_with("[room "),
            "the outdoor `back` path is unchanged: {retraced:?}"
        );
        assert_eq!(
            session.agent.position, elsewhere,
            "and it still retraces the trail"
        );
    }

    #[test]
    fn a_step_north_moves_the_mark_up_the_picture() {
        // The one thing a table of deltas cannot check about itself: that `north`
        // agrees with the DRAWN plan. A sign flip here produces a world that is
        // internally consistent and vertically mirrored — every wall, doorway and
        // reachability claim still holds — so it is checked against the picture,
        // which is the only place the reader's north lives.
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        session
            .descend(path_structure(&session.agent.position, 2), 0)
            .expect("a chamber to stand in");
        let before = session.inside.as_ref().unwrap().cell;
        let row_of = |session: &Session| {
            let plan = session.plan_here().expect("inside, so a plan draws");
            plan.lines()
                .position(|l| l.contains(crate::lattice::render::YOU))
                .expect("the plan marks where you stand")
        };
        let first = row_of(&session);
        match session.handle("go n") {
            Turn::Out(t) => assert!(t.starts_with("You step north"), "{t}"),
            Turn::Released(_) => panic!("go must not release"),
        }
        let after = session.inside.as_ref().unwrap().cell;
        assert_eq!(
            (after.0, after.1),
            (before.0, before.1 - 1),
            "north is -y, matching the render's top-down rows"
        );
        assert_eq!(
            row_of(&session),
            first - 1,
            "a step north must draw the mark one row HIGHER in the picture"
        );
    }

    #[test]
    fn the_carried_lattice_is_the_one_the_place_derives() {
        // `Inside` carries the lattice rather than re-deriving it per turn, which
        // makes it a cache — and a cache that can disagree with its source is how
        // FRAME-tier state stops being derived. Asserted after a real descent and
        // after walking, because the walk is what could stale it.
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        session.handle("enter");
        for line in ["go n", "go e", "go s", "go w", "enter further in"] {
            session.handle(line);
            let inside = session.inside.as_ref().expect("still indoors");
            assert_eq!(
                inside.lattice,
                session.lattice_of(&inside.structure),
                "after {line:?} the carried plan is not the one the place derives"
            );
        }
    }

    #[test]
    fn the_marks_answer_is_the_sessions_own_self_description() {
        // §6's parity contract over the one legend noun whose answer is not a
        // static line. Two claims, and the second is the one that keeps this from
        // being a second description of the possessed agent: the mark answers, and
        // it answers with EXACTLY `whoami`'s words.
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        session.handle("enter");
        let noun = crate::lattice::render::YOU_NOUN;
        assert!(
            session.plan_legend_nouns().iter().any(|n| n == noun),
            "the plan draws the mark, so its legend must name it: {:?}",
            session.plan_legend_nouns()
        );
        let answered = match session.handle(&format!("examine {noun}")) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("examine must not release"),
        };
        assert_eq!(
            answered,
            session.whoami(),
            "the mark must resolve to the session's existing self-description, not \
             to a second one authored beside it"
        );
        // And `chamber_prose` must NOT answer for it, or there would be two.
        assert!(
            crate::chamber_prose::glyph_detail(noun).is_none(),
            "a static detail for the mark is a second description of the possessed \
             agent, which is the drift §6 exists to prevent"
        );
    }

    /// The first cave-bearing cell this seed's terrain places whose entrance
    /// address (`band = 0, slot = 0`) resolves to `want_open`. Scans the
    /// terrain directly (`GeneratedTerrain::cave_at`) rather than steering a
    /// walk there: a terrain cell spans many walk-band rooms (measured while
    /// developing this campaign — dozens to low hundreds of `go` steps per
    /// terrain-cell crossing), and even once ON the right cell, only 51.5% of
    /// caves have a chamber at their entrance at all (Task 3), so a walk
    /// cannot be relied on to land on either specific outcome. Direct
    /// scanning is what `windows/worldgen/tests/deep_realm_substrate.rs`
    /// (Task 0) and `deep_realm_chamber.rs` (Tasks 2-3) already do for the
    /// same reason.
    fn find_cave_cell(
        terrain: &hornvale_terrain::GeneratedTerrain,
        seed: Seed,
        want_open: bool,
    ) -> (hornvale_kernel::CellId, hornvale_terrain::Cave) {
        let overrides = hornvale_worldgen::chamber::ChamberOverrides::new();
        for cell in terrain.geosphere().cells() {
            if terrain.is_ocean(cell) {
                continue;
            }
            let Some(cave) = terrain.cave_at(cell) else {
                continue;
            };
            let addr = hornvale_worldgen::chamber::ChamberAddr {
                cell,
                entrance: 0,
                band: 0,
                slot: 0,
            };
            let is_open =
                hornvale_worldgen::chamber::chamber_at(seed, &cave, addr, &overrides).is_some();
            if is_open == want_open {
                return (cell, cave);
            }
        }
        panic!(
            "no {} cave found in seed 42's terrain — the fixture no longer has one \
             of the three outcomes this campaign's descent verb needs to distinguish",
            if want_open { "open" } else { "sealed" }
        );
    }

    /// The Deep Realm, Task 5's own hazard: `delve` needs THREE
    /// distinguishable outcomes, not the two the original plan sketch
    /// anticipated. Task 3 measured that even a cell WITH a cave resolves no
    /// chamber at its own entrance address 51.5% of the time (spec §3.4 rung
    /// 0, `Sealed` — "the void exists and is unreachable," a real fact a
    /// later dig could find, not a defect) — so "no cave" and "cave but
    /// sealed" are different facts about the world and must read as such,
    /// exactly the failure mode `dive`'s own doc warns a refusal that
    /// doesn't name what stopped you falls into.
    #[test]
    fn delve_has_three_distinguishable_outcomes() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session.terrain.clone().expect("seed 42 builds terrain");

        // Outcome 1: no cave at all. The flagship's own starting cell — no
        // walk needed, mirroring `there_is_nothing_to_dive_into_on_dry_land`.
        let no_cave = match session.handle("delve") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("delve must not release"),
        };
        assert!(no_cave.contains("no cave here"), "{no_cave}");
        assert!(
            session.underground.is_none(),
            "a refused delve must not change the underground state"
        );

        // Outcome 2: a cave, but the entrance resolves to nothing — SEALED,
        // named as such rather than read as "no cave here" again.
        let (sealed_cell, sealed_cave) = find_cave_cell(&terrain, world.seed, false);
        let sealed = match session.delve_at(sealed_cell, sealed_cave) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("delve must not release"),
        };
        assert!(sealed.contains("sealed"), "{sealed}");
        assert!(
            session.underground.is_none(),
            "a sealed entrance must not change the underground state"
        );

        // Outcome 3: a chamber — descend, and `climb` returns.
        let (open_cell, open_cave) = find_cave_cell(&terrain, world.seed, true);
        let open = match session.delve_at(open_cell, open_cave) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("delve must not release"),
        };
        assert!(
            session.underground.is_some(),
            "a resolved entrance chamber must set the underground state: {open}"
        );
        let up = match session.handle("climb") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("climb must not release"),
        };
        assert!(
            session.underground.is_none(),
            "climb must clear the underground state"
        );
        assert!(up.contains("You climb back into the light"), "{up}");

        // The whole point: each outcome must be told apart from the others.
        assert_ne!(no_cave, sealed, "no-cave and sealed read identically");
        assert_ne!(
            sealed, open,
            "sealed and a successful descent read identically"
        );
        assert_ne!(
            no_cave, open,
            "no-cave and a successful descent read identically"
        );
    }

    /// `delve` refuses while indoors, mirroring `dive`'s own "no water in
    /// here" guard one realm over — descending into rock through a
    /// building's own floor is not what either verb means.
    #[test]
    fn delve_refuses_while_inside_a_structure() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        session
            .descend(path_structure(&session.agent.position, 2), 0)
            .expect("a chamber to stand in");
        let out = match session.handle("delve") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("delve must not release"),
        };
        assert!(out.contains("no rock to delve into in here"), "{out}");
    }

    /// Lateral movement is refused while underground, and says so
    /// diegetically — mirroring `SUBMERGED_LATERAL_REFUSAL`'s own guard one
    /// realm over. Exercised directly against a hand-picked open cave
    /// (`delve_at`) rather than a walk, for the same reason
    /// `delve_has_three_distinguishable_outcomes` is.
    #[test]
    fn lateral_movement_is_refused_underground() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session.terrain.clone().expect("seed 42 builds terrain");
        let (cell, cave) = find_cave_cell(&terrain, world.seed, true);
        session.delve_at(cell, cave);
        assert!(
            session.underground.is_some(),
            "the fixture must have descended"
        );
        for line in ["go n", "back", "n"] {
            let out = match session.handle(line) {
                Turn::Out(t) => t,
                Turn::Released(_) => panic!("{line} must not release"),
            };
            assert!(!out.contains("No verb"), "{line}: {out}");
            assert!(
                out.contains("Climb out first"),
                "{line} must refuse underground with the underground reason: {out}"
            );
        }
    }

    /// The snapshot's spatial channel and the `map` verb must answer the
    /// SAME band question, including in a band neither was written against.
    ///
    /// Found at The Panes' merge, not during either campaign: The Deep Realm
    /// added `underground` while The Panes added the spatial channel, in
    /// parallel worktrees, and the textual merge was clean because they
    /// touched different lines of the same file. `SpatialChannel` enumerates
    /// bands; The Deep Realm added one; neither campaign's chronicle mentions
    /// the other's surface. That is precisely the semantic collision
    /// `make preflight` says it cannot score.
    ///
    /// What it asserts is a FOLD, not a correctness claim. Standing in a cave
    /// chamber, the pane shows a chart of the country overhead — which is
    /// odd, and is exactly what the `map` verb already does in the same
    /// state, because both guard on `inside` alone. So the invariant worth
    /// pinning is not "the pane is right here" but "the pane and the verb
    /// cannot drift apart here": whichever answer the sim settles on, one
    /// change must move both. Without this, adding a fourth band would fold
    /// silently into `walk` and no test would notice.
    #[test]
    fn the_underground_band_folds_into_walk_as_map_does() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session.terrain.clone().expect("seed 42 builds terrain");
        let (cell, cave) = find_cave_cell(&terrain, world.seed, true);
        session.delve_at(cell, cave);
        assert!(
            session.underground.is_some(),
            "the fixture must have descended"
        );

        // The pane: `walk`, carrying a chart rather than a plan.
        let snap = session.snapshot().expect("a descended session snapshots");
        match &snap.spatial {
            crate::snapshot::SpatialChannel::Walk { .. } => {}
            crate::snapshot::SpatialChannel::Chamber { .. } => panic!(
                "the underground band emitted `chamber` — if that is now intended, \
                 `SpatialChannel`'s doc and the `map` verb's band arms must change WITH it"
            ),
        }
        let json = crate::snapshot_json(&snap);
        assert!(
            json.contains(r#""band":"walk""#),
            "the wire tag must read `walk` underground: {json:.120}"
        );

        // The verb, in the same state: the surface chart, not a plan and not
        // a refusal. `plan_here` prints a legend; `map`'s chart prints a lens
        // header — so the two are told apart by content, not by length.
        let out = match session.handle("map") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("map must not release"),
        };
        assert!(
            out.contains("[lens:"),
            "map underground must draw the walk-band chart, as the pane does: {out}"
        );
        assert!(
            !out.contains(INDOOR_CHART_REFUSAL),
            "map underground must not take the indoor refusal: {out}"
        );
    }

    /// The Handle, Task 4: an underground `examine` must resolve against the
    /// band's OWN catalog, not fall through to the surface locale's — which is
    /// what `session.rs`'s dispatch did before this fix (the bare `"examine"`
    /// arm has no `self.underground` guard, so it ran `examine(rest)` against
    /// whatever the surface locale above the chamber names). This is the
    /// campaign's only instance never reproduced live before now: the
    /// controller could not reach a cave by walking (400 steps, none found),
    /// and `delve_at` is crate-private, so only an in-crate test can drive it
    /// directly at a hand-picked open cave the way
    /// `lateral_movement_is_refused_underground` does.
    #[test]
    fn underground_examine_answers_for_the_rock_it_names() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session.terrain.clone().expect("seed 42 builds terrain");
        let (cell, cave) = find_cave_cell(&terrain, world.seed, true);
        let shown = match session.delve_at(cell, cave) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("delve must not release"),
        };
        assert!(
            shown.contains("You worm down into the dark"),
            "not underground: {shown}"
        );
        let reply = match session.handle("examine rock") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("examine must not release"),
        };
        assert!(
            !reply.starts_with("You see no"),
            "the underworld names rock and then refuses it: {reply}"
        );
    }

    // ---- The Sighting -------------------------------------------------
    //
    // Tests 2-4 of the campaign's four live HERE rather than in
    // `tests/session_snapshot.rs`, where the plan filed them, because each
    // needs a lever the public surface deliberately does not offer: an NPC
    // put at a chosen anchor (`Session::occupancy`), a second creature made
    // co-located (`Session::ledger`), and — the negative control — the frame
    // seed the embedding is drawn from (`Inside::seed`). Adding a public
    // setter for any of those would ship a knob production never turns, which
    // is worse than a unit test.

    /// A session standing in `world`'s opening structure, one tick in — the
    /// shared fixture for the three tests below. The tick matters: `Occupancy`
    /// is populated by `DriveMovements::step_with_occupancy`, so before a
    /// `wait` no creature has a within-room anchor at all.
    fn possessed_inside(world: &World) -> Session<'_> {
        let (mut session, _) = Session::start(world, &PossessOpts::default()).unwrap();
        session.handle("wait");
        session.handle("enter");
        assert!(
            session.inside.is_some(),
            "this world's opening locale must be built and enterable, or nothing \
             below is tested"
        );
        session
    }

    /// The marks this session's snapshot draws.
    fn marks_of(session: &Session<'_>) -> Vec<crate::plan::PlanMark> {
        match session
            .snapshot()
            .expect("a live session snapshots")
            .spatial
        {
            SpatialChannel::Chamber { plan } => plan.marks,
            SpatialChannel::Walk { .. } => panic!("expected the chamber band"),
        }
    }

    #[test]
    fn two_creatures_cannot_be_drawn_in_one_cell() {
        // THE SIGHTING, TEST 2. `lattice::Occupancy::place`'s `Refusal` path
        // shipped with no caller at all — its own module doc says a test over
        // data that does not exist yet "reads as coverage". This is the caller,
        // and this is the test that makes the refusal non-vacuous.
        //
        // The collision is built out of the two facts that make it reachable:
        // `liveness::Occupancy` deliberately ALLOWS two creatures at one anchor
        // ("a hearth crowded with three NPCs is a legitimate occupancy"), and
        // `lattice::Occupancy` deliberately forbids two creatures in one cell.
        // One anchor resolves to one cell, so the second creature must be
        // refused and must not be drawn.
        //
        // THE WORLD IS SEARCHED FOR, NOT PINNED (see `world_where`), and the
        // predicate is both of this test's structural needs at once: one
        // creature already drawn, and a second derived creature to collide with
        // it. Asking for both up front is what keeps the assertions below about
        // the REFUSAL rather than about whether some seed happened to oblige.
        let (seed, world) = world_where(
            "exactly one creature is drawn and a second is available to collide with it",
            |s| {
                let drawn = marks_of(s).len();
                let others = s
                    .colocated_npcs()
                    .first()
                    .copied()
                    .map(|first| s.npcs.iter().any(|n| n.entity != first.entity));
                drawn == 1 && others == Some(true)
            },
        );
        let mut session = possessed_inside(&world);

        let room = session.agent.position.clone();
        let first = session
            .colocated_npcs()
            .first()
            .copied()
            .expect("the seed was chosen because a creature stands here")
            .entity;
        let anchor = session
            .occupancy
            .anchor_in(first, &room)
            .expect("the tick recorded where it stands");
        assert_eq!(
            marks_of(&session).len(),
            1,
            "precondition: seed {seed} was chosen because exactly one creature \
             is drawn before the second arrives"
        );

        // A second creature, made co-located the way the world makes one: an
        // `agent-at` fact, which is what `colocated_npcs` reads.
        let second = session
            .npcs
            .iter()
            .map(|n| n.entity)
            .find(|&e| e != first)
            .expect("the seed was chosen because a second NPC is derived");
        let fact = crate::liveness::place_agent(second, &room, session.day);
        session
            .ledger
            .commit(fact, &session.registry)
            .expect("agent-at is registered");
        session.occupancy.place(second, &room, anchor);

        assert_eq!(
            session.colocated_npcs().len(),
            2,
            "both creatures are now in the possession's room"
        );
        assert_eq!(
            session.occupancy.anchor_in(first, &room),
            session.occupancy.anchor_in(second, &room),
            "and both stand at the same anchor, which liveness permits"
        );

        let marks = marks_of(&session);
        assert_eq!(
            marks.len(),
            1,
            "one cell may hold one creature: the second must be REFUSED, not stacked — got {marks:?}"
        );
        // THE UNPLACED ROW (fix round 2), and this test is the only place that
        // constructs it. The refused creature is co-located, is NOT drawn, and
        // must nonetheless be present, examinable and readable by `needs` —
        // because the embedding declining to place it says nothing whatever
        // about whether the possession can perceive it, and presence must never
        // depend on the embedder's free draws (spec §2.1). "Present but
        // undrawable" is honest; "absent" would be a lie.
        let refused = session
            .npcs
            .iter()
            .find(|n| n.entity == second)
            .expect("the second creature is derived")
            .label
            .clone();
        let snap = session.snapshot().unwrap();
        assert_eq!(
            snap.sensed.present.len(),
            2,
            "a creature refused a cell must not vanish from `sensed.present`"
        );
        assert!(
            !marks.iter().any(|m| m.noun == refused),
            "precondition: the refused creature is genuinely UNDRAWN"
        );
        let answered = session.examine_chamber(&refused);
        assert!(
            !answered.starts_with("You see no"),
            "an unplaced but present creature must be examinable — `present` must \
             imply examinable, or the channel and the verb disagree: {answered}"
        );
        assert!(
            session.needs().contains(&refused),
            "and `needs` must read it too, for the same reason: {}",
            session.needs()
        );
        // ...and it stays ACTABLE-ON. The sight gate on `colocated_npc` (fix
        // round 3) must narrow on sight and on nothing else: an undrawable
        // creature is not an unseen one, so refusing to provoke it would make
        // the placement scan decide what the player may do.
        let acted = match session.handle(&format!("provoke {refused}")) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("provoke must not release"),
        };
        assert!(
            acted.contains(&refused),
            "an unplaced but present creature must stay provokable: {acted}"
        );
    }

    #[test]
    fn a_creature_beyond_sight_appears_neither_in_sensed_nor_in_marks() {
        // THE SIGHTING, TEST 3. The narrowing is structural and sim-side
        // (`CLIENT-redaction-panes`): the client is never handed a creature it
        // is trusted to hide.
        //
        // The creature is moved by putting it at a DIFFERENT anchor of its own
        // room's interior — the same `Occupancy::place` catch-up itself uses —
        // and the anchor is CHOSEN BY MEASUREMENT rather than by hand: the test
        // asks the embedding which of this chamber's cells lies outside the
        // shadowcast and then finds the room anchor that draws there. Hardcoding
        // an anchor id would pin a number that moves with the pattern
        // inventory.
        let world = seam_world();
        let mut session = possessed_inside(&world);
        let room = session.agent.position.clone();
        let who = session
            .colocated_npcs()
            .first()
            .copied()
            .expect("a creature is here")
            .entity;

        let (near, far) = {
            let inside = session.inside.as_ref().unwrap();
            let chamber = session.chamber_interior_here().unwrap();
            let cells =
                crate::lattice::anchor_cells(&chamber, &inside.lattice, inside.at, inside.seed);
            let lit = crate::lattice::shadowcast(&inside.lattice, inside.cell, SIGHT_RADIUS);
            let terrain = session.terrain_here();
            let interior = crate::interior::interior_of(&room, &terrain);
            // For each of the ROOM's anchors, which cell it would be drawn at
            // (joined by kind, exactly as `sighting` joins them), and whether
            // that cell is lit.
            let drawn = |a: crate::interior::AnchorId| {
                let kind = interior.anchor(a).kind;
                chamber
                    .ids()
                    .into_iter()
                    .find(|&c| chamber.anchor(c).kind == kind)
                    .and_then(|c| cells.get(&c).copied())
            };
            let mut near = None;
            let mut far = None;
            for a in interior.ids() {
                match drawn(a) {
                    Some(cell) if lit.contains(&cell) => near = near.or(Some(a)),
                    Some(_) => far = far.or(Some(a)),
                    None => {}
                }
            }
            (near, far)
        };
        let near = near.expect("some anchor of this room draws inside the possession's sight");
        let far = far.expect(
            "some anchor of this room draws OUTSIDE it — without one this test asserts nothing",
        );

        let label = session
            .npcs
            .iter()
            .find(|n| n.entity == who)
            .expect("the creature is derived")
            .label
            .clone();

        session.occupancy.place(who, &room, near);
        let snap = session.snapshot().unwrap();
        assert_eq!(
            snap.sensed.present.len(),
            1,
            "precondition: a creature in sight IS sent"
        );
        assert_eq!(marks_of(&session).len(), 1, "precondition: and IS drawn");
        assert!(
            !session.examine_chamber(&label).starts_with("You see no"),
            "precondition: and ANSWERS examine while it is depicted"
        );
        assert!(
            session.needs().contains(&label),
            "precondition: and `needs` reads it: {}",
            session.needs()
        );

        session.occupancy.place(who, &room, far);
        let snap = session.snapshot().unwrap();
        assert!(
            snap.sensed.present.is_empty(),
            "a creature out of sight must not be sent: {:?}",
            snap.sensed.present
        );
        assert!(
            marks_of(&session).is_empty(),
            "and must not be drawn either — one shadowcast decides both"
        );
        // THE SIDE CHANNEL, closed. `examine_chamber` answers a creature's noun
        // (fix round 1, so the noun does not stop answering at a doorway) — but
        // gated on SIGHT, not on co-location. Ungated it would hand back the
        // creature `snapshot` had just structurally redacted, one verb later.
        let refused = session.examine_chamber(&label);
        assert!(
            refused.starts_with("You see no"),
            "examine must refuse a creature sight withheld, or it is a side \
             channel around the redaction: {refused}"
        );
        // THE SECOND SIDE CHANNEL, closed one round later (fix round 2). `needs`
        // named the withheld creature by label AND by felt state — a strictly
        // richer leak than `examine`'s, since it also reports the creature's
        // interior. It is band-blind (`handle` does not gate it on `inside`), so
        // the gate lives in the verb rather than in the dispatch.
        let read = session.needs();
        assert!(
            !read.contains(&label),
            "`needs` must not read a creature sight withheld — it is the same \
             side channel `examine`'s gate closes, one verb over: {read}"
        );

        // THE THIRD (fix round 3), and the one that survived two rounds of
        // closing the other two. `provoke`/`soothe` resolve through
        // `colocated_npc`, and a SUCCESSFUL act narrates its target by name:
        // `You provoke <label>. They bristles.` The leak is not that the action
        // is permitted — that is a game question — but that the success line
        // discloses presence and disposition state, which is the identical shape
        // to the `needs` leak in a third location.
        //
        // Both forms are checked. The BARE form matters at least as much as the
        // named one: it selects the first sensed NPC, and unfiltered it would
        // silently pick the hidden creature without the player ever naming it.
        for arg in ["", &label] {
            let acted = match session.handle(&format!("provoke {arg}")) {
                Turn::Out(t) => t,
                Turn::Released(_) => panic!("provoke must not release"),
            };
            assert!(
                !acted.contains(&label),
                "`provoke {arg:?}` named a creature sight withheld — a verb's \
                 SUCCESS LINE is a disclosure channel: {acted}"
            );
        }
        assert!(
            !session.would_turn_hostile(&label),
            "`would_turn_hostile` rides the same resolution, so it must not \
             report a withheld creature's disposition either"
        );

        // THE FOURTH, and the richest (fix round 4). `wait`'s own narration
        // asserts presence UNPROMPTED, with identity — the player names nothing
        // and is simply told the creature is here. Fed directly rather than
        // reached through a tick, because it cannot be reached: a 200-turn
        // indoor sweep never fired either branch on seed 42, whose structure
        // produces only the `stirred` fallback. A latent branch still needs a
        // test; "I could not reach it" is not coverage.
        //
        // A real, packable room that is NOT this one: the last path digit
        // stepped one place. Built rather than invented so `RoomAddr::pack`
        // (which rejects any digit >= 4) still accepts it.
        let elsewhere = {
            let mut path = room.path.clone();
            let last = path.last_mut().expect("a walk-band address has a path");
            *last = (*last + 1) % 4;
            RoomAddr {
                face: room.face,
                path,
            }
        };
        let nowhere: std::collections::BTreeSet<EntityId> = Default::default();

        // THE ARRIVAL. `before` says the creature was elsewhere; the ledger
        // still says it is here; `moved` is nonzero so the early return does
        // not swallow the call.
        let arriving: Vec<RoomAddr> = session
            .npcs
            .iter()
            .map(|npc| {
                if npc.entity == who {
                    elsewhere.clone()
                } else {
                    agent_position(&session.ledger, npc, session.day)
                }
            })
            .collect();
        let narrated = session.narrate_motion(1, &arriving, &nowhere);
        assert!(
            !narrated.contains(&label),
            "`wait` must not announce the ARRIVAL of a creature sight withheld — \
             it is the only channel that names a creature the player never asked \
             about: {narrated}"
        );

        // ...AND ITS POSITIVE CONTROL, symmetric with the departure arm's below
        // (fix round 5). Without it, an arrival guard restricted to nothing at
        // all — `if false && sensed_now.contains(…)` — suppresses every arrival
        // line the game can print and every test in the crate stays green. The
        // review measured exactly that: 442 passed under that mutation. A gate
        // needs both halves pinned, or only one direction of breaking it is
        // visible.
        session.occupancy.place(who, &room, near);
        let seen_arriving = session.narrate_motion(1, &arriving, &nowhere);
        assert!(
            seen_arriving.contains(&label),
            "an arrival the player CAN see must still be narrated — without this \
             the gate above could be suppressing everything: {seen_arriving}"
        );
        // Back out of sight for the departure checks below, which are about the
        // creature the player could NOT see.
        session.occupancy.place(who, &room, far);

        // THE DEPARTURE, gated on a different moment and so checked separately:
        // `before` says the creature WAS here, the ledger now says it left, and
        // the sensed-before set says the player could not see it while it was.
        // Watching something go that you never saw arrive is the same
        // disclosure as watching it arrive.
        let was_here: Vec<RoomAddr> = session
            .npcs
            .iter()
            .map(|npc| agent_position(&session.ledger, npc, session.day))
            .collect();
        let fact = crate::liveness::place_agent(who, &elsewhere, session.day);
        session
            .ledger
            .commit(fact, &session.registry)
            .expect("agent-at is registered");
        assert!(
            !session.colocated_npcs().iter().any(|n| n.entity == who),
            "precondition: the creature really left the room"
        );
        let leaving = session.narrate_motion(1, &was_here, &nowhere);
        assert!(
            !leaving.contains(&label),
            "`wait` must not announce the DEPARTURE of a creature the player \
             could not see while it was here: {leaving}"
        );

        // THE POSITIVE CONTROL, and it is what stops both assertions above
        // being vacuous. The identical departure vector, with the creature in
        // the sensed-before set, MUST name it — otherwise the two negatives
        // would pass simply because this branch never narrates anything.
        let seen: std::collections::BTreeSet<EntityId> = [who].into_iter().collect();
        let announced = session.narrate_motion(1, &was_here, &seen);
        assert!(
            announced.contains(&label),
            "a departure the player COULD see must still be narrated — without \
             this the gate above could be suppressing everything: {announced}"
        );
    }

    #[test]
    fn perturbing_the_embedding_moves_what_is_drawn_and_not_what_is_known() {
        // THE SIGHTING'S CENTRAL INVARIANT, and spec §2.1 as a test.
        //
        // Decision 0069 lets the fine layer "regenerate differently forever
        // without corrupting a world" precisely because nothing stored points
        // into it. The moment sight-derived knowledge accumulated, an agent's
        // BELIEF would depend on the embedder's free draws — so the embedding
        // may decide what a client is SHOWN and may never decide what an agent
        // comes to BELIEVE.
        //
        // The experiment is the whole claim: change the placement seed and
        // NOTHING ELSE, then read both channels. `spatial` must move (the
        // embedding is load-bearing there, and a control that cannot see its
        // own positive is as empty as one that cannot see its own negative) and
        // `known` must be byte-identical.
        //
        // THE WORLD IS SEARCHED FOR, NOT PINNED (see `world_where`). The
        // predicate carries BOTH halves of the control, because both are
        // properties of the world and not of the code under test: something must
        // be drawn from the embedding at all, and every creature must stay in
        // sight under both placements — otherwise the `sensed.present`
        // assertion below is asserting a coincidence rather than the invariant.
        let (seed, world) = world_where(
            "the embedding draws a creature and every creature stays in sight under a perturbed placement",
            |s| {
                if marks_of(s).is_empty() {
                    return false;
                }
                let before = s.snapshot().unwrap();
                let inside = s.inside.as_ref().unwrap();
                let original = inside.seed;
                s.inside.as_mut().unwrap().seed = Seed(original.0 ^ PERTURBATION);
                let after = s.snapshot().unwrap();
                s.inside.as_mut().unwrap().seed = original;
                before.spatial != after.spatial && before.sensed.present == after.sensed.present
            },
        );
        let mut session = possessed_inside(&world);
        assert!(
            !marks_of(&session).is_empty(),
            "precondition: seed {seed} was chosen because something is drawn \
             from the embedding at all"
        );

        let before = session.snapshot().unwrap();
        let placement = session.inside.as_ref().unwrap().seed;
        // A different DRAW of the same placement, not a different world: only
        // `Inside::seed` moves, and `anchor_cells` is the only reader of it.
        // The SAME perturbation the search applied, named once so the two
        // cannot drift apart and leave the search filtering on a different
        // experiment than the one this test runs.
        session.inside.as_mut().unwrap().seed = Seed(placement.0 ^ PERTURBATION);
        let after = session.snapshot().unwrap();

        assert_ne!(
            before.spatial, after.spatial,
            "perturbing the embedding must MOVE what is drawn — if it does not, \
             the placement seed is not reaching the plan and this control is decoration"
        );
        assert_eq!(
            before.known, after.known,
            "perturbing the embedding must NOT move what is known (spec §2.1): \
             sight has leaked into belief"
        );
        assert_eq!(
            before.sensed.present, after.sensed.present,
            "nor may it move who is REPORTED here in seed {seed}, which was \
             chosen because every creature stays in sight under both placements"
        );
    }
}
