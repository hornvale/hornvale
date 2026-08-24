//! The possession session: a pure step function over a frozen world. Every
//! verb is read-only; possessing a world never changes it.

use crate::action::{Action, Mood};
use crate::agent::check_species_known;
use crate::body::Body;
use crate::clock::{climb_factor, cost_ticks, days_of, mass_for_species};
use crate::controller::PlayerController;
use crate::gate::{BodyState, Verdict, verdict};
use crate::liveness::{
    AGENT_AT, Affect, AffectLabel, DRANK, DriveKind, DriveMovements, EATEN, HomeNavCache,
    LocaleTerrain, Mode, Occupancy, PrimaryAfraidMemo, RESTED, SUSTENANCE, Terrain,
    affect_of_memo_occupied, agent_at_fact, agent_position, built_rooms, derive_npcs,
    derive_wild_npcs, next_awake_day, rested_fact, species_activity, village_or_fallback,
};
use crate::snapshot::{
    KnownChannel, KnownEntry, Narration, NounEntry, PresentEntry, SESSION_SCHEMA, SelfChannel,
    SensedChannel, SessionSnapshot, SocialEntry, SpatialChannel,
};
use crate::{
    Focalized, Focalizer, IdentityProjection, Knowledge, PossessOpts, PossessTarget, Projection,
    TemplateFocalizer, Turn, VesselError, absorb_common, most_populous_settlement, observable,
    reader_set,
};
use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, Ledger, RoomAddr, RoomId, Seed, TickSpan, Value, World,
    WorldTime, tick,
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

/// Spec §3.2's group D — session control, which is *not an act* and therefore
/// carries no [`Mood`] at all. A body you cannot let go of is a hang, not a
/// capability, and `exit`'s coarse-ward refusal is a statement about the
/// world's grain rather than about this body.
///
/// Disjoint from [`IN_CHARACTER_VERBS`] by construction, and asserted so:
/// `session_control_is_never_an_in_character_verb` in this file's own tests.
const SESSION_CONTROL: [&str; 3] = ["release", "quit", "exit"];

/// Spec §3.2's groups B and C, bare — every token [`Session::handle`]'s
/// in-character match resolves to an act, and therefore exactly the tokens the
/// body-state gate stands in front of. Bare compass directions belong here too
/// and are recognised separately, by [`parse_compass`], because they are an
/// open set rather than a list.
///
/// # Why the gate needs a roster at all (fix round 1)
///
/// Task 7 asked the gate about *every* bare token, before verb resolution. So
/// a sleeping body answered "You cannot — you are asleep." to `xyzzy`. A
/// nonsense token is not an act the body is too asleep to perform, and the
/// answer LEAKS: it told a player that `whoami` — a **retired** group-A bare
/// form (Task 5) — is a real in-character verb merely blocked by body state,
/// which is exactly what spec §3.2 says it must not be ("until [the hint path]
/// exists, a bare group-A verb is an ordinary unknown-verb refusal"). Awake it
/// was; asleep it was not.
///
/// # The drift this roster could have, and what holds it
///
/// A second list beside a match can fall out of step with it, silently, in the
/// direction that matters most: a new bare verb added to `handle` and not
/// added here would be UNGATED, and nothing about it would look wrong.
/// [`HELP`] is the third copy that closes the loop — it is the surface a
/// player reads, so it is already obliged to be complete — and
/// `every_bare_verb_help_lists_is_classified` asserts the two agree in **both**
/// directions: every verb `HELP` lists is in this roster or in
/// [`SESSION_CONTROL`], and every entry of this roster is listed by `HELP`.
const IN_CHARACTER_VERBS: [&str; 17] = [
    "back", "climb", "consult", "delve", "dive", "enter", "examine", "go", "knows", "look", "map",
    "needs", "out", "sleep", "surface", "wait", "write",
];

/// The provenance a walk-band step commits under (The Deed, Task 7).
///
/// **An in-world reason, deliberately, and this is the acceptance test's
/// hinge.** A creature's `agent-at` provenance names its errand ("went down to
/// the river it knew (thirst)"); the keystone requires that "nothing in the
/// trace may reveal that a different mind chose", so a possessed body's must
/// name an errand too. It must never name the driver — `provoke`/`soothe`
/// stamp `player: …` precisely because those ARE operator acts (spec §2.3),
/// and an in-character act is the opposite case.
const WALKED_PROVENANCE: &str = "walked on (its own errand)";

/// The provenance `back` commits under — the same in-world register as
/// [`WALKED_PROVENANCE`], naming the retrace rather than the retracer.
const RETRACED_PROVENANCE: &str = "turned back the way it came";

/// The provenance `sleep` commits its `rested` fact under, in the same
/// register `liveness.rs` uses for a creature's own Rest ("slept at home
/// (fatigue eased)").
const SLEPT_PROVENANCE: &str = "lay down and slept (fatigue eased)";

/// What `sleep` says. It names `!wait` on purpose: an in-character `wait` is
/// gated the moment the body goes under, so a player told nothing here would
/// have a body that refuses every verb and no way to learn which one still
/// works — precisely the "indistinguishable from the game having hung" state
/// spec §3.4 argues out-of-character exists to prevent.
const SLEEP_REPLY: &str = "You lie down and let go of the day. Time still passes for the world, and \
     '!wait' still passes it for you; the body wakes on its own.";

/// What `sleep <anything>` says. It names the two things a player who typed a
/// length actually wanted — that `sleep` takes none, and which verb does —
/// rather than refusing bare, which reads as a parse failure (the standard
/// `dive`'s own refusal set).
const SLEEP_ARGUMENT_REFUSAL: &str = "Sleep takes no length: you lie down until your own cycle wakes you. Say 'sleep' \
     on its own, or 'wait N' to let N days pass while you are awake.";

/// Advance an instant by a DURATION in standard days, on the tick lattice, and
/// report every failure as a caller-facing string rather than panicking.
///
/// **Two callers, one guard.** `charge` and `wait` both accumulate onto the
/// session clock, and the guard predates The Escapement: `days` is validated
/// finite and positive at its parse site, but the SUM is an accumulation the
/// parse-site guard cannot see, and a live `possess` stdin can reach it with
/// two `wait 1e308`s.
///
/// **What the flip changed is the failure MODE, not the guard.** The sum used
/// to be an `f64` addition that could reach infinity; it is now an `i64` tick
/// addition that can overflow, so it is `checked_add` rather than trusted. The
/// crossing from the continuous domain onto the lattice happens ONCE, on the
/// duration, before any arithmetic (spec §2.1) — never by re-deriving a float
/// day from the instant, adding, and re-rounding.
fn advanced_by(day: WorldTime, days: f64) -> Result<WorldTime, String> {
    let span = TickSpan::from_std_days(days).map_err(|e| format!("error: {e}"))?;
    day.ticks()
        .checked_add(span.ticks())
        .map(WorldTime::from_ticks)
        .ok_or_else(|| {
            // Names the SUM, and both of its operands, because that is what
            // failed: `days` was already accepted as representable by the
            // crossing above, so blaming it here would send a reader looking at
            // a valid number (fix round 1, code review Minor 7). This text is
            // deliberately DISTINCT from the crossing's own rejection, so a
            // test can tell the two arms apart — they were indistinguishable by
            // message, which is why the accumulation arm sat uncovered while a
            // test appeared to exercise it.
            format!(
                "error: advancing day {} by {days} standard days leaves the \
                 representable tick range",
                day.as_std_days()
            )
        })
}

/// What the body says when the gate refuses. The reason itself comes from
/// [`crate::gate::verdict`], so a new [`BodyState`] row cannot reach a player
/// without a sentence of its own.
fn body_refusal(reason: &str) -> String {
    format!("You cannot — {reason}.")
}

/// The `eyes` an out-of-character chart is drawn through (The Deed, Task 6,
/// spec §3.2): the observer step declined.
///
/// [`crate::eyes::resolve`] answers `None` for this, which is the permissive
/// limit of the chart's one gating parameter — the plain terrain, not the
/// colour this body's photoreceptors project onto it. Named rather than
/// written inline at each `!` arm so the objective view has exactly one
/// definition, the same discipline [`Perceiving::Objectively`] keeps for the
/// sight gate.
const OBJECTIVE_EYES: crate::eyes::Eyes = crate::eyes::Eyes::Off;

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
  sleep            lie down and sleep; the body stops obeying until its own
                   cycle wakes it, and only '!' verbs answer meanwhile
  knows            everything they have seen
  needs            read the felt state of anyone sharing this room
  write <sentence> speak a line of Common; you absorb what it says, written
                   into your own margin
  consult          read the Book's Reckoning at your own day, and whatever
                   your margin has initiated you into
  release          let go (quit works too)

operator instruments (out-of-character; bypass the body, never the world):
  !whoami          the one you possess
  !npcs            the derived NPCs sharing this world (label, number)
  !why <who>       recount an NPC's dated history (by label or number)
  !eyes [who]      whose eyes you see colour through (a species, 'own',
                   'standard', or 'off'); bare, it says what yours drop
  !provoke [who]   shift a co-located NPC's disposition, your own mark
  !soothe [who]    ease a co-located NPC's disposition, your own mark
  !help            this list

the out-of-character halves (bypass the body, never the world; four take a
renderer's gate to its limit — so where nothing is being withheld, each
answers exactly as its bare twin does — and two relax nothing at all, but
answer while the body cannot):
  !map [out N]     the chart drawn in plain terrain, not through your eyes;
                   indoors it differs only when a lens is on
  !examine <thing> as examine, but indoors a creature standing here in the
                   dark answers too
  !needs           the felt state of everyone here, seen or unseen; only
                   indoors is anyone unseen
  !wait [N]        as wait, and the clock moves the same; indoors, the comings
                   and goings are narrated whether you could see them or not
  !look            as look, and identical to it whenever your body can act;
                   the point is that it still answers when your body cannot
  !knows           as knows, on the same terms as '!look'
";

/// The world-scoped half of starting a possession: everything
/// [`Session::start`] used to derive per session that does not depend on WHO
/// is possessed. Build it once and start many sessions from it.
///
/// Splitting this out is what makes possession and release cheap (The Quire).
/// The derivation ORDER inside [`WorldContext::build`] is copied verbatim from
/// the old `Session::start` and is a save-format contract — a reorder changes
/// which seed draws are taken and is an epoch event, not a refactor.
///
/// Nothing here is agent-scoped, and nothing here is mutated after `build`
/// returns: a `&WorldContext` is shared by every session started from it
/// ([`Session::start_in`]), so a field that a turn could write would alias
/// across sessions. The per-session mutable state — the ledger clone, the
/// registry clone, the derived NPC roster — stays on [`Session`].
pub struct WorldContext<'w> {
    /// The world this context was derived from.
    pub(crate) world: &'w World,
    /// The world's terrain, sculpted once (`hornvale_worldgen::terrain_of`).
    ///
    /// Held as an `Option` — never `None` in practice, because `build`'s `?`
    /// already succeeded — so that every downstream `(terrain, climate)` match
    /// in this module reads exactly as it did before the hoist. The `Option`
    /// is this field's defensive posture, inherited from the `Session::terrain`
    /// field it replaced, and never a second derivation that could fail.
    pub(crate) terrain: Option<hornvale_terrain::GeneratedTerrain>,
    /// The world's climate, fit once from `terrain`
    /// (`hornvale_worldgen::climate_from`). `Option` for the same reason
    /// `terrain` is.
    pub(crate) climate: Option<hornvale_climate::GeneratedClimate>,
    /// The locale context every observation is taken through, built from the
    /// `terrain`/`climate` pair above rather than re-sculpting its own.
    pub(crate) ctx: LocaleContext,
    /// The assembled world components (the species/biosphere roster). `None`
    /// when assembly fails, exactly as the old `start` had it.
    pub(crate) wc: Option<hornvale_worldgen::WorldComponents>,
    /// The coexistence-stack demography fit, run ONCE over
    /// `(world, wc, terrain, climate)` and shared by the predator/prey
    /// pressures and the wild-NPC concentrations. `None` whenever `wc` or the
    /// fit itself fails.
    pub(crate) report: Option<hornvale_worldgen::DemographyReport>,
}

impl<'w> WorldContext<'w> {
    /// Derive the world-scoped half. Terrain/climate failure is a hard
    /// failure, exactly as it was in `Session::start`.
    // Named construction site (decision 0092): `WorldComponents::assemble` is
    // called here, once per context rather than once per session.
    #[allow(clippy::disallowed_methods)]
    pub fn build(world: &'w World) -> Result<WorldContext<'w>, VesselError> {
        // ONE derivation block (The Weir, Stage 2; hoisted out of
        // `Session::start` by The Quire): terrain, climate, the locale
        // context, the species roster and the demography report are each
        // derived EXACTLY ONCE here, then threaded into everything below —
        // `LocaleContext::build_from`, the predator/prey pressures, the
        // wild-NPC concentration fit — instead of a consumer quietly
        // re-sculpting or re-fitting its own copy. Terrain/climate failure
        // is a hard failure for `build`, exactly the failure
        // `LocaleContext::build` used to surface on this identical call
        // (`build` is still the right entry point for a caller that has not
        // already sculpted its own pair — see its doc).
        //
        // THE ORDER OF THESE FIVE DERIVATIONS IS A SAVE-FORMAT CONTRACT, and
        // it is worth being exact about what does and does not check it.
        //
        // The committed `possess --script` gallery transcripts
        // (`book/src/gallery/possession-*.md`) are the ONLY guard on the order
        // itself. Re-derive them with `make rebaseline` and diff.
        //
        // `windows/vessel/tests/world_context.rs` does NOT guard it, despite
        // living next door and looking like it should. Both arms of its
        // comparison — `Session::start` and `Session::start_in` — route
        // through THIS function, so a change made inside this block moves both
        // arms identically and cancels. That is measured, not assumed: The
        // Quire swapped two of these derivations and all 490 vessel tests
        // stayed green. What that test does guard is the OTHER direction —
        // that `start_in` agrees with `start`, and that a reused context does
        // not drift between sessions.
        //
        // So: change the order here and the vessel suite will not stop you.
        // The transcripts will.
        let terrain = hornvale_worldgen::terrain_of(world)
            .map_err(|e| VesselError::Locale(hornvale_locale::LocaleError::Build(e.to_string())))?;
        let climate = hornvale_worldgen::climate_from(world, &terrain)
            .map_err(|e| VesselError::Locale(hornvale_locale::LocaleError::Build(e.to_string())))?;
        let ctx = LocaleContext::build_from(world, &terrain, &climate);
        // The species roster and the demography report, assembled/fit ONCE
        // per CONTEXT (The Weir, Stage 1b/2; per session before The Quire):
        // shared below by `predator`/`prey` and by the wild-NPC derivation
        // instead of each independently re-running the coexistence-stack fit
        // over the same `(world, wc, terrain, climate)`. `None` whenever `wc`
        // or the fit itself fails — the same `Option` posture as
        // `calendar`/`predator`/`prey` on the session.
        //
        // `mint_flagship` used to sit between `ctx` and this fit, so that a
        // settlement-less or unspecied world failed before paying for the
        // fit. It cannot stay there: it is agent-scoped, and the whole point
        // of this type is that it is not. The fast-fail is therefore gone
        // from the SINGLE-possession path (`Session::start` now pays the fit
        // before it learns the world has no settlement) and irrelevant to the
        // reusing path, which pays the fit once for any number of sessions.
        // The error a caller sees is unchanged: `start` still returns
        // `VesselError::NoSettlement`/`NoSpecies`.
        let wc = hornvale_worldgen::WorldComponents::assemble().ok();
        let report = match wc.as_ref() {
            Some(wc) => {
                hornvale_worldgen::demography_report_from(world, wc, &terrain, &climate).ok()
            }
            None => None,
        };
        // Wrapped in `Some` from here on: both derivations above already
        // succeeded (the `?`s), so `terrain`/`climate` are `Option` only for
        // the field's own defensive posture (see its doc), never because a
        // second, independent derivation could fail where this one didn't.
        let terrain = Some(terrain);
        let climate = Some(climate);
        Ok(WorldContext {
            world,
            terrain,
            climate,
            ctx,
            wc,
            report,
        })
    }

    /// The locale context this world is observed through (read-only).
    pub fn context(&self) -> &LocaleContext {
        &self.ctx
    }
}

/// How a [`Session`] holds its [`WorldContext`]: BORROWED from a caller that
/// means to start many sessions over one world ([`Session::start_in`]), or
/// OWNED by a session that derived its own ([`Session::start`]).
///
/// A hand-rolled two-variant `Cow` rather than `std::borrow::Cow`, which would
/// require `WorldContext: Clone` — and cloning the derivation is precisely the
/// cost this campaign exists to remove. Deref rather than accessor methods so
/// that `self.wctx.ctx` stays a *place* expression: the borrow checker then
/// still sees it as disjoint from `self.ledger`, `self.bodies` and the rest,
/// exactly as the old `self.ctx` field was.
enum HeldContext<'w> {
    /// Derived by [`Session::start`] for this one session.
    Owned(Box<WorldContext<'w>>),
    /// Shared with every other session started from the same context.
    Borrowed(&'w WorldContext<'w>),
}

impl<'w> std::ops::Deref for HeldContext<'w> {
    type Target = WorldContext<'w>;

    fn deref(&self) -> &WorldContext<'w> {
        match self {
            HeldContext::Owned(owned) => owned,
            HeldContext::Borrowed(shared) => shared,
        }
    }
}

/// A live possession over a frozen world. The possessed agent's own senses
/// stay pinned to the frozen `world` (byte-identical, never mutated); only
/// the NPC layer evolves, in a session-owned ledger clone (the-quickening).
pub struct Session<'w> {
    world: &'w World,
    /// The world-scoped derivations — the locale context, the terrain/climate
    /// pair, the species roster and the demography fit — either owned by this
    /// session or shared with its siblings. See [`HeldContext`].
    wctx: HeldContext<'w>,
    /// Every body this session derived (The Hand, Task 3): ONE roster,
    /// re-derivable, never saved. The possessed body is a MEMBER of it —
    /// `bodies[driven]` — not a second, separately-minted representation of
    /// the same villager (Task 2 proved the two were always identical on
    /// every field that matters). What `self.agent`/`self.npcs` used to split
    /// into "the possessed one" and "the others" is now one list plus an
    /// index.
    bodies: Vec<Body>,
    /// Which element of `bodies` is being driven. `derive_npcs`'s
    /// `ordered_for_derivation` step hoists the settlement-anchored roster's
    /// own body to index `0`, and every `PossessTarget` before Task 4 drove
    /// exactly that element, so this was `0` for the whole of Tasks 1-3.
    /// `PossessTarget::Creature` (The Hand, Task 4) generalises it: the
    /// resolved roster index of the named entity, whatever it is — spec
    /// §3.2's own phrase for this is `driven = i`, naming a controller MAP
    /// (Arc III) as the reason this is a real field rather than a hardcoded
    /// `0`. `other_bodies` reads this field, not an assumption that it is
    /// `0`, so a non-zero `driven` narrates correctly everywhere that
    /// function is the single source of "every other body".
    driven: usize,
    knowledge: Knowledge,
    trail: Vec<RoomAddr>,
    /// The walk-band course, if the possession is mid-traverse.
    ///
    /// `None` before the first `go` and after any verb that invalidates a
    /// heading. Never serialized: a world is a seed plus a ledger, and a
    /// course is a fact about this session's walk, not about the world.
    course: Option<crate::course::Course>,
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
    /// The presentation lens the DRAWN chamber plan is filtered through (The
    /// Lantern, Task 8, spec §7), carried from `PossessOpts::lens`.
    ///
    /// Read in exactly one place — [`Session::plan_here`], the terminal draw.
    /// It must never reach [`crate::plan::plan_of`] or [`Session::snapshot`]:
    /// those produce committed artifacts, and lensed colour in one would make
    /// these constants a save-format-class contract for the sake of a look.
    lens: crate::lens::Lens,
    // The world's terrain and climate — sculpted once (The Shuttle), so every
    // book-reading verb (`write`, `consult`) shares one sculpt instead of
    // re-sculpting the globe per call — used to be owned HERE. They moved to
    // [`WorldContext`] (The Quire) because they are world-scoped, not
    // session-scoped: `self.wctx.terrain` / `self.wctx.climate` read them, and
    // their `Option` posture is unchanged. They are threaded into the
    // worldgen/book `_from` readout family (`reckoning_at_from`,
    // `esoteric_lines_from`, `hornvale_book::parse_context_from`) whenever
    // both are present; the unthreaded (`_of`/bare) form is the fallback on a
    // `None`, matching what those calls already did before this campaign.
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
    /// The moment the body's own cycle next wakes it, or `None` while it is
    /// awake (The Deed, Task 7).
    ///
    /// The body state is DERIVED from this rather than stored beside it
    /// ([`Session::body_state`]), so there is no second field to keep in step
    /// and no "settle" pass to forget: the clock advancing IS the waking.
    /// Computed by `liveness::next_awake_day` — the very function a creature's
    /// own `Rest` jumps by — so the possessed body sleeps by its species'
    /// cycle and not by a second rule invented for the player.
    wake_at: Option<WorldTime>,
    /// The possessed body's mass in kilograms, derived ONCE at `start` through
    /// [`crate::clock::mass_for_species`] — the same one derivation every
    /// creature reads, which is what makes the player's tariff the same tariff
    /// rather than a parallel one (The Tackle extracted it for exactly this).
    /// type-audit: bare-ok(ratio: body_mass_kg)
    body_mass_kg: f64,
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
    /// branch = 0, floor = 0`) when `Some`.
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
    /// The driven body's own commitment mode as of the most recent `!wait`
    /// (The Hand, Task 5 fix round 1, spec §2.3) — `None` before the first
    /// one. Set by [`Self::wait`], the only place the driven body's own
    /// arbitration runs; read back by [`Self::driven_mode`].
    driven_mode: Option<Mode>,
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

/// Whether a read of who is present is narrowed by what the possessed body
/// can perceive, or takes that gate to its permissive limit (The Deed, Task
/// 6, spec §3.2).
///
/// **This is the whole of group B's out-of-character halves, and it is one
/// enum rather than a second rendering path on purpose.** `!needs`,
/// `!examine` and `!wait` are the same renderers their bare twins are, called
/// with this parameter flipped — so a change to how a creature is described,
/// or to what counts as sensed, cannot land in one mood and miss the other.
/// The alternative (a parallel objective renderer) is exactly how `examine`
/// came to answer one creature with two different sentences across a band
/// boundary, which §6 exists to prevent.
///
/// [`Perceiving::Objectively`] is *not* a fourth row of
/// [`Session::sensed_npcs`]' table — it is that predicate with nothing to
/// narrow it, which is precisely what `sensed_npcs(None)` already means out
/// of doors.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Perceiving {
    /// Through the possessed body: the chamber band's shadowcast narrows who
    /// is sensed. What every bare verb has always done.
    Body,
    /// At the permissive limit: every co-located creature, whatever the body
    /// can see. The out-of-character view.
    Objectively,
}

/// Every derived body other than the one being driven — what `self.npcs`
/// meant before The Hand collapsed the two representations (Task 3): every
/// occupancy/social/perception/tick read that used to exclude the possessed
/// `Agent` by construction (it was never a member of that list) now excludes
/// it by this filter instead.
///
/// **A free function taking `bodies`/`driven` directly, not a `&self`
/// method.** [`HeldContext`]'s own doc explains why: a method call borrows
/// `self` as a whole, where a direct field expression borrows only that
/// field — and several callers (the `wait` tick's `turned-hostile` loop, in
/// particular) iterate this result while mutably borrowing `self.ledger` in
/// the same loop body, exactly as they iterated `self.npcs.iter()` before.
///
/// An owned `Vec` of borrows, not a slice (The Hand, Task 4 fix round 1):
/// `driven` can now name ANY roster index, not only `0`
/// (`PossessTarget::Creature`), so "every OTHER body" can no longer be the
/// contiguous `bodies[1..]` this used to slice — it is `bodies` with
/// exactly the `driven`'th element removed, ORDER PRESERVED. Order
/// preservation is load-bearing, not cosmetic: `list_npcs`/`why`/
/// `colocated_npc` number every other body by its 1-based POSITION in this
/// list, and a body uninvolved in the possession choice must keep the same
/// handle number regardless of which OTHER body is driven — an earlier
/// version of this fix swapped the driven body into slot `0` instead of
/// filtering, which kept `driven == 0` true but silently renumbered every
/// handle between the old and new driven slots, a user-visible regression
/// no test caught until spec review measured it directly
/// (`possessing_a_creature_does_not_renumber_other_bodies_handles`).
fn other_bodies(bodies: &[Body], driven: usize) -> Vec<&Body> {
    bodies
        .iter()
        .enumerate()
        .filter(|(i, _)| *i != driven)
        .map(|(_, npc)| npc)
        .collect()
}

impl<'w> Session<'w> {
    /// Begin a possession, deriving a fresh [`WorldContext`] for it: build the
    /// locale context, mint the flagship agent, absorb the first projection,
    /// and return the opening text.
    ///
    /// Kept so every existing caller compiles unchanged; prefer
    /// [`Session::start_in`] when starting more than one session over the same
    /// world, which is what this whole split exists for (The Quire).
    /// type-audit: bare-ok(prose: return)
    pub fn start(
        world: &'w World,
        opts: &PossessOpts,
    ) -> Result<(Session<'w>, String), VesselError> {
        let ctx = WorldContext::build(world)?;
        Self::start_held(HeldContext::Owned(Box::new(ctx)), opts)
    }

    /// Begin a possession from an already-derived [`WorldContext`], paying
    /// none of its cost. Every session started from one `ctx` is
    /// byte-identical to one started by [`Session::start`] over the same
    /// world, and independently drivable —
    /// `windows/vessel/tests/world_context.rs` holds both halves.
    /// type-audit: bare-ok(prose: return)
    pub fn start_in(
        ctx: &'w WorldContext<'w>,
        opts: &PossessOpts,
    ) -> Result<(Session<'w>, String), VesselError> {
        Self::start_held(HeldContext::Borrowed(ctx), opts)
    }

    /// The agent-scoped half of starting a possession, over a context this
    /// session either owns or shares. ONE body, so the two public entry points
    /// above cannot drift — which is the only reason `start`'s output can be
    /// claimed identical to `start_in`'s.
    fn start_held(
        held: HeldContext<'w>,
        opts: &PossessOpts,
    ) -> Result<(Session<'w>, String), VesselError> {
        let world = held.world;
        let ctx = &held.ctx;
        let terrain = &held.terrain;
        let wc = &held.wc;
        let report = &held.report;
        // The commanded settlement: a cheap failure path (a settlement/
        // species lookup). It used to sit between `ctx` and the
        // coexistence-stack fit, so that a settlement-less or unspecied
        // world failed before paying for the fit; the fit is world-scoped
        // and now lives in `WorldContext::build`, so this resolves after it.
        // See that function's own note. `opts.target` picks WHICH settlement
        // supplies the driven body (The Quire, Task 2) — **neither arm mints
        // any more (The Hand, Task 3)**: both SELECT the roster entry
        // `derive_npcs` below always hoists to index 0 for this settlement.
        // `Flagship` stays the exact lookup that predates the target, so
        // that path is byte-identical. `Creature` (The Hand, Task 4) picks
        // its driven body out of the roster by entity, below, once the
        // roster is fully derived — it does not change WHICH settlement
        // anchors that roster's derivation, so it shares `Flagship`'s
        // lookup here.
        let village = match opts.target {
            PossessTarget::Flagship | PossessTarget::Creature(_) => {
                hornvale_settlement::village_info(world).ok_or(VesselError::NoSettlement)?
            }
            PossessTarget::MostPopulousSettlement => {
                most_populous_settlement(world).ok_or(VesselError::NoSettlement)?
            }
        };
        // `mint_at`'s fail-loud species check, kept byte-for-byte even though
        // nothing mints any more: `liveness::body_at` (below, via
        // `derive_npcs`) is deliberately infallible on an unresolved
        // species — the fallback `derive_npcs` always wanted for every OTHER
        // settlement's NPC — but a player COMMANDING an unrecognized species
        // was always a loud error, not a silent fallback to "goblin".
        check_species_known(world, &village)?;
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
        // The Hand, Task 3: derive the roster ONCE and SELECT the driven body
        // from it, rather than minting a second representation of the same
        // villager. `ordered_for_derivation` (inside `derive_npcs`) always
        // hoists `village`'s own body to index 0 (guaranteeing it is
        // co-located with itself is what the old comment here — "otherwise
        // no NPC is ever co-located with the player" — used to guard;
        // driving that same element makes the guarantee trivial rather than
        // merely satisfied), so `driven` resolves to `0` for `Flagship`/
        // `MostPopulousSettlement` — see the field's own doc for why it is a
        // real field rather than a bare constant. `driven`'s actual value is
        // computed below, once the roster (settled + wild) is complete,
        // since `PossessTarget::Creature` (Task 4) may name a wild member.
        let mut bodies = derive_npcs(world, ctx, &mut ledger, NPC_COUNT, village.id);
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
            bodies.extend(derive_wild_npcs(world, ctx, &mut ledger, concentrations));
        }
        // The Hand, Task 4: `PossessTarget::Creature` selects any already-
        // derived roster member — settled OR wild (a wild creature's
        // `village` is `None`, and it is still a legitimate target) — by
        // resolving `driven` to its roster INDEX, exactly spec §3.2's own
        // phrase for this ("possessing any creature is `driven = i`"). Fix
        // round 1 replaced an earlier `bodies.swap(0, idx)` that kept
        // `driven == 0` by construction: the swap permanently exchanged two
        // roster slots, so `other_bodies`'s 1-based handles (`list_npcs`/
        // `why`/`colocated_npc`) silently renumbered every body between the
        // old and new front slots — a user-visible regression an index
        // assignment does not produce, since `other_bodies` now filters by
        // index rather than slicing from a fixed front (see its own doc).
        // An entity outside the full roster (settled + wild) fails loudly
        // rather than silently falling back to the flagship — generation
        // never guesses (spec §4.6).
        let driven = match opts.target {
            PossessTarget::Creature(entity) => bodies
                .iter()
                .position(|npc| npc.entity == entity)
                .ok_or(VesselError::NoSuchCreature(entity))?,
            PossessTarget::Flagship | PossessTarget::MostPopulousSettlement => 0,
        };
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
        let built = built_rooms(world, ctx);
        // The possessed body's own mass, through the ONE shared derivation
        // (The Tackle): read here, once, exactly as `derive_npcs` reads a
        // creature's. Bound before the struct literal because `bodies` is
        // moved into it.
        let species_for_mass = bodies[driven].species.clone();
        let biosphere_for_mass = hornvale_species::biosphere_registry();
        let mut session = Session {
            world,
            wctx: held,
            bodies,
            driven,
            knowledge: Knowledge::default(),
            trail: Vec::new(),
            course: None,
            day: opts.day,
            focalizer: TemplateFocalizer,
            projection: IdentityProjection,
            ledger,
            registry,
            eyes: opts.eyes.clone(),
            lens: opts.lens,
            calendar,
            predator,
            prey,
            built,
            occupancy: Occupancy::default(),
            wake_at: None,
            body_mass_kg: mass_for_species(&species_for_mass, Some(&biosphere_for_mass)),
            turn: 0,
            last_text: String::new(),
            inside: None,
            submerged: None,
            underground: None,
            mesh_memo: hornvale_kernel::RoomMeshMemo::new(),
            home_nav_cache: HomeNavCache::new(),
            driven_mode: None,
        };
        session.absorb_here()?;
        let opening = session.describe_here()?;
        session.last_text = opening.clone();
        Ok((session, opening))
    }

    /// The body being driven (read-only) — a member of [`Self::bodies`], not
    /// a second, separately-minted representation of the same villager (The
    /// Hand, Task 3). Replaces the pre-Hand `Session::agent()`.
    pub fn driven_body(&self) -> &Body {
        &self.bodies[self.driven]
    }

    /// Every body this session derived, the driven one included. Task 5
    /// asserts a specific body committed by reading this alongside
    /// [`Self::agent_entity`]; `windows/vessel/tests/suite/one_roster.rs`
    /// asserts the driven body appears exactly once in it.
    pub fn bodies(&self) -> &[Body] {
        &self.bodies
    }

    /// The driven body's current position: a ledger-derived read
    /// (`liveness::agent_position`), the same one a creature's own position
    /// uses — spec §3.1 says committing the `agent-at` fact **is** the
    /// position update, so there is no separate mutable field to go stale
    /// against it. Every `.agent().position` reader from before The Hand
    /// reads through here now.
    pub fn position(&self) -> RoomAddr {
        agent_position(&self.ledger, self.driven_body(), self.day)
    }

    /// The driven body's own commitment mode, as of the most recent `!wait`
    /// tick — arbitration run for the possessed body itself (The Hand, Task 5
    /// fix round 1, spec §2.3: the co-present decision made mechanical). A
    /// possession is not displacement; the host has its own drives and its
    /// own felt state while you ride it, and this is that computation's route
    /// out, alongside [`Self::committed_agent_at_count_for`] for what
    /// actually happened.
    ///
    /// **This is the tick's own answer, not a separate re-derivation.**
    /// `Session::wait` runs the driven body through
    /// [`DriveMovements::step_one_with_controller`] — the SAME `advance_one`
    /// every other body's walk calls, with a fresh [`PlayerController`] so
    /// nothing here ever double-moves a body the player drives through the
    /// verb loop (`go`, `drink`, …) — and stores the mode that call's own
    /// arbitration reached. `None` before the first `!wait` (there is no tick
    /// to report on yet).
    pub fn driven_mode(&self) -> Option<Mode> {
        self.driven_mode
    }

    /// The accumulated knowledge (read-only).
    pub fn knowledge(&self) -> &Knowledge {
        &self.knowledge
    }

    /// This session's walk-band course, if it is mid-traverse.
    ///
    /// An accessor rather than a `pub` field: the course is session-private
    /// state whose invariant is that `reckoned` advances only through
    /// `rhumb_advance`. A `pub` field invites a consumer that assigns to it,
    /// which is exactly the memoryless walk this campaign exists to avoid.
    pub fn course(&self) -> Option<&crate::course::Course> {
        self.course.as_ref()
    }

    /// The locale context this session walks (for the battery's checks).
    pub fn context(&self) -> &LocaleContext {
        &self.wctx.ctx
    }

    /// This turn as `vessel/session/v2` — a pure read, grouped by epistemic
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
        let vantage = observable(
            self.world,
            &self.wctx.ctx,
            self.driven_body(),
            &self.position(),
            self.day,
        )?;
        // The noun catalog comes from the focalizer; the PROSE comes from
        // `last_text` (this turn's real response), not from here.
        let focalized = self.focalizer.render(&vantage);

        // `&self`-only: can read whatever `self.mesh_memo` already holds
        // (Finding 1's cache field is a shared borrow, not a mutation) but
        // cannot prefill it fresh — `wait`'s tick is where that happens.
        let terrain = LocaleTerrain::with_fields(
            &self.wctx.ctx,
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
        //
        // `band` is cloned ONCE here, outside the `.map()` below, rather than
        // re-derived per creature: `other_bodies` (The Hand, Task 4 fix round
        // 1) now filters and allocates rather than slicing a fixed front, and
        // `affect_of_memo_occupied`'s `band: &[Body]` — shared with
        // `windows/lab`'s health metric, so not a signature this scope can
        // narrow to `&[&Body]` alone — needs owned data to borrow from.
        let band: Vec<Body> = other_bodies(&self.bodies, self.driven)
            .into_iter()
            .cloned()
            .collect();
        let here: Vec<(EntityId, String, PresentEntry)> = self
            .sensed_npcs(sighting.as_ref())
            .iter()
            .map(|npc| {
                let affect = affect_of_memo_occupied(
                    &self.ledger,
                    npc,
                    &band,
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

        let social = other_bodies(&self.bodies, self.driven)
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
            Some(inside) => SpatialChannel::Chamber {
                plan: self.chamber_plan(inside, marks, &self.eyes)?,
            },
            // `purview(0)` is the same call `map` makes out of doors, at the
            // same zoom, so the pane shows what the verb would have shown.
            None => SpatialChannel::Walk {
                chart: Box::new(self.purview(0)?),
            },
        };

        Ok(SessionSnapshot {
            schema: SESSION_SCHEMA.to_string(),
            turn: self.turn,
            day: self.day.as_std_days(),
            me: SelfChannel {
                agent: self.driven_body().entity.0.get(),
                species: self.driven_body().species.clone(),
                settlement: village_or_fallback(self.driven_body()).name,
                population: village_or_fallback(self.driven_body()).population,
                room: self
                    .position()
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
                        kind: n.kind.tag().to_string(),
                    })
                    .collect(),
            },
            spatial,
        })
    }

    /// The day the possession stands on — the same `WorldTime` every fact this
    /// session commits is stamped with, and the number the walk-band room line
    /// prints.
    ///
    /// Typed rather than a bare `f64` (decision 0126): a day is a point on an
    /// axis, and handing a caller the primitive is exactly what 0126 supersedes
    /// 0014 to stop. It carries no `type-audit:` tag for the same reason —
    /// there is no primitive at this boundary to tag.
    pub fn day(&self) -> WorldTime {
        self.day
    }

    /// How many `agent-at` facts the session's owned ledger has committed.
    ///
    /// Zero at turn 0, and moved by two different things since The Deed's Task
    /// 7: the NPC layer's own walks on a `wait` tick (which is all it ever
    /// meant before — "zero until the first `wait`", T3's day-zero guard) and
    /// now the possession's own walk-band `go`/`back`, which commit through
    /// the same `liveness::agent_at_fact` constructor a creature's step uses.
    /// A caller reading this as "how many NPC steps have happened" would be
    /// wrong by exactly the player's trail, which is the point of spec §3.1.
    /// type-audit: bare-ok(count: return)
    pub fn committed_agent_at_count(&self) -> usize {
        self.ledger.find(AGENT_AT).count()
    }

    /// How many `agent-at` facts the session's owned ledger has committed
    /// FOR `who` specifically — unlike [`Self::committed_agent_at_count`],
    /// which sums every body's (the possessed one's own walk-band steps
    /// included since spec §3.1), this narrows to one subject. Added for
    /// Task 5's assertion that a SPECIFIC body committed, which the summed
    /// accessor cannot distinguish from any other body's commit.
    /// type-audit: bare-ok(count: return)
    pub fn committed_agent_at_count_for(&self, who: EntityId) -> usize {
        self.ledger
            .find(AGENT_AT)
            .filter(|f| f.subject == who)
            .count()
    }

    /// Commit `who` — another body from this session's own [`Self::bodies`]
    /// — into the ledger at the DRIVEN body's current position, through the
    /// same `agent-at` constructor ([`crate::liveness::place_agent`]) a real
    /// arrival commits with, so a manufactured co-location is
    /// indistinguishable from one the world produced.
    ///
    /// **A test seam, not a verb — `handle` never reaches this.** The Hand's
    /// Task 3 deleted the possessed body's duplicate representation, and
    /// with it went the only thing that ever guaranteed a fresh possession
    /// starts co-located with anything: every other settlement lives
    /// elsewhere, and wild concentrations are scattered independently
    /// (confirmed live: seed 42's flagship possession still finds nobody to
    /// provoke after sixty `wait`s). The fidelity ruling on that finding
    /// (idea registry: `SOC-one-creature-per-settlement`) is that changing
    /// world population to restore an incidental co-location guarantee is
    /// its own campaign — The Penstock already measured tick cost
    /// superlinear in exactly that dimension — so a test that needs a
    /// co-located body now asks for one explicitly, through here, rather
    /// than relying on a population side effect.
    /// Commit `who` — another body from this session's own [`Self::bodies`]
    /// — into the ledger at the DRIVEN body's current position, through the
    /// same `agent-at` constructor ([`crate::liveness::place_agent`]) a real
    /// arrival commits with, so a manufactured co-location is
    /// indistinguishable from one the world produced. If the possession is
    /// currently INDOORS, `who` also gets a fine-layer anchor at a cell the
    /// possession can actually see — the same `Occupancy`/lit-cell join
    /// [`Self::sighting`] reads back — so it is DRAWN on the chamber plan,
    /// not merely present-but-unplaced.
    ///
    /// **A test seam, not a verb — `handle` never reaches this.** The Hand's
    /// Task 3 deleted the possessed body's duplicate representation, and
    /// with it went the only thing that ever guaranteed a fresh possession
    /// starts co-located with anything: every other settlement lives
    /// elsewhere, and wild concentrations are scattered independently
    /// (confirmed live: seed 42's flagship possession still finds nobody to
    /// provoke after sixty `wait`s). The fidelity ruling on that finding
    /// (idea registry: `SOC-one-creature-per-settlement`) is that changing
    /// world population to restore an incidental co-location guarantee is
    /// its own campaign — The Penstock already measured tick cost
    /// superlinear in exactly that dimension — so a test that needs a
    /// co-located body now asks for one explicitly, through here, rather
    /// than relying on a population side effect.
    pub fn place_creature_at_me(&mut self, who: EntityId) {
        let room = self.position();
        let fact = crate::liveness::place_agent(who, &room, self.day);
        self.ledger
            .commit(fact, &self.registry)
            .expect("AGENT_AT is registered every session and non-functional");
        if let Some(inside) = self.inside.as_ref() {
            let terrain = self.terrain_here();
            let room_interior = crate::interior::interior_of(&room, &terrain);
            if let Some(chamber) = self.chamber_interior_here() {
                let cells =
                    crate::lattice::anchor_cells(&chamber, &inside.lattice, inside.at, inside.seed);
                let lit = crate::lattice::shadowcast(&inside.lattice, inside.cell, SIGHT_RADIUS);
                // A chamber anchor whose OWN placed cell is actually lit —
                // `sighting()` would draw a creature standing here, so this
                // is the only choice honest enough not to fabricate a
                // placement the fine layer could not itself have produced —
                // then the room-interior anchor of the SAME kind, which is
                // what `Occupancy::place`/`sighting()`'s join actually reads.
                let anchor = chamber
                    .ids()
                    .into_iter()
                    .find(|&a| cells.get(&a).is_some_and(|cell| lit.contains(cell)))
                    .and_then(|chamber_anchor| {
                        let kind = chamber.anchor(chamber_anchor).kind;
                        room_interior
                            .ids()
                            .into_iter()
                            .find(|&a| room_interior.anchor(a).kind == kind)
                    });
                if let Some(anchor) = anchor {
                    self.occupancy.place(who, &room, anchor);
                }
            }
        }
    }

    /// [`Self::place_creature_at_me`]'s complement: place `who` co-located
    /// but specifically OUT OF SIGHT — a fine-layer anchor at a chamber cell
    /// the current shadowcast does NOT light, so [`Self::sighting`]'s own
    /// join places it on a cell that exists but is not drawn, which is
    /// exactly the "present but unsensed" state the sight-gated verbs
    /// (`!needs`, `!examine`, `!wait`'s narration) discriminate on.
    ///
    /// `false`, and nothing placed beyond the room-level `agent-at`, if the
    /// possession is not indoors or the entered chamber has no cell outside
    /// its own shadowcast — this seam refuses to fabricate an occlusion the
    /// fine layer could not itself produce, the same discipline
    /// [`Self::place_creature_at_me`] applies to a lit one.
    /// type-audit: bare-ok(flag: return)
    pub fn place_creature_out_of_my_sight(&mut self, who: EntityId) -> bool {
        let room = self.position();
        let fact = crate::liveness::place_agent(who, &room, self.day);
        self.ledger
            .commit(fact, &self.registry)
            .expect("AGENT_AT is registered every session and non-functional");
        let Some(inside) = self.inside.as_ref() else {
            return false;
        };
        let terrain = self.terrain_here();
        let room_interior = crate::interior::interior_of(&room, &terrain);
        let Some(chamber) = self.chamber_interior_here() else {
            return false;
        };
        let cells = crate::lattice::anchor_cells(&chamber, &inside.lattice, inside.at, inside.seed);
        let lit = crate::lattice::shadowcast(&inside.lattice, inside.cell, SIGHT_RADIUS);
        let anchor = chamber
            .ids()
            .into_iter()
            .find(|&a| cells.get(&a).is_some_and(|cell| !lit.contains(cell)))
            .and_then(|chamber_anchor| {
                let kind = chamber.anchor(chamber_anchor).kind;
                room_interior
                    .ids()
                    .into_iter()
                    .find(|&a| room_interior.anchor(a).kind == kind)
            });
        match anchor {
            Some(anchor) => {
                self.occupancy.place(who, &room, anchor);
                true
            }
            None => false,
        }
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

    /// How many facts the session's owned ledger has committed with `who` as
    /// SUBJECT, across every predicate — the per-entity counterpart to
    /// [`Self::committed_fact_count`], the same way
    /// [`Self::committed_agent_at_count_for`] narrows
    /// [`Self::committed_agent_at_count`]. Added for The Hand, Task 5 fix
    /// round 1: proving a driven body's own arbitration commits NOTHING while
    /// the player says nothing needs every predicate a creature's own drives
    /// can emit (`agent-at`, `drank`, `rested`, `eaten`), not only position —
    /// a body pursuing Fatigue that reaches `Rest` without ever moving would
    /// slip past `committed_agent_at_count_for` alone.
    /// type-audit: bare-ok(count: return)
    pub fn committed_fact_count_for(&self, who: EntityId) -> usize {
        self.ledger.iter().filter(|f| f.subject == who).count()
    }

    /// The driven body's own stable identity as a ledger `EntityId` — the
    /// object a hostile NPC's `turned-hostile` fact points at.
    ///
    /// **The Hand, Task 3: this IS the creature's own entity now, not a
    /// separately-minted id.** Before this task, the possessed `Agent` was
    /// never committed to the ledger (spec's reversibility rule) and carried
    /// its own seed-derived `AgentId` instead; that split is exactly what
    /// this campaign removes — the driven body is a member of
    /// [`Self::bodies`], and its `entity` is the same `reuse_or_mint_entity`
    /// lineage id `derive_npcs` gives every other settlement's NPC (stable
    /// across a reload, unlike the old per-session `AgentId` draw).
    pub fn agent_entity(&self) -> EntityId {
        self.driven_body().entity
    }

    /// Would the named co-located NPC be hostile to the player right now
    /// (their grievance fold at or past `HOSTILITY_THRESHOLD`)? A pure read
    /// — never commits anything. `who` resolves exactly as `provoke`/
    /// `soothe` do (`colocated_npc`): empty selects the sole co-located
    /// NPC, else the `npcs` listing's 1-based handle or a case-insensitive
    /// label substring; an
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
    /// undiscoverable: `SessionSnapshot::social` folds `other_bodies` *unfiltered*
    /// (every derived body except the one being driven) and ships `label` +
    /// `grievance` + this very `hostile` bool for every
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
    /// the `npcs` listing's 1-based handle or case-insensitive label
    /// substring; `None` if no derived NPC matches `who`.
    /// type-audit: bare-ok(identifier-text: who), bare-ok(diagnostic-value: return)
    pub fn npc_grievance(&self, who: &str) -> Option<f64> {
        let others = other_bodies(&self.bodies, self.driven);
        who.parse::<usize>()
            .ok()
            .filter(|n| *n >= 1)
            .and_then(|n| others.get(n - 1))
            .or_else(|| {
                let needle = who.to_lowercase();
                others
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
        other_bodies(&self.bodies, self.driven)
            .iter()
            .map(|n| n.label.as_str())
            .collect()
    }

    /// The current room, focalized (for the battery's checks).
    pub fn focalized(&self) -> Result<Focalized, VesselError> {
        let v = observable(
            self.world,
            &self.wctx.ctx,
            self.driven_body(),
            &self.position(),
            self.day,
        )?;
        Ok(self.focalizer.render(&v))
    }

    /// The lateral exits from here: each compass bearing paired with its
    /// destination's packed room id (three per room, always — the mesh's
    /// base-edge neighbors). For the walker battery's deterministic pick.
    /// type-audit: bare-ok(index: return)
    pub fn ways(&self) -> Vec<(Compass, u64)> {
        let v = observable(
            self.world,
            &self.wctx.ctx,
            self.driven_body(),
            &self.position(),
            self.day,
        )
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
        self.purview_through(zoom_out, &self.eyes)
    }

    /// [`Self::purview`], drawn through `eyes` rather than the session's own
    /// (The Deed, Task 6).
    ///
    /// **`eyes` is the chart's one gating parameter, and this is where its
    /// permissive limit is taken.** `purview_scene` receives `knowledge` and
    /// `eyes`; only the second has an objective limit. `eyes` resolving to
    /// `None` declines the observer step entirely and yields the uncoloured
    /// terrain — the world as it is rather than as this body's photoreceptors
    /// project it. `knowledge` does not: it marks which cells have been
    /// WALKED, and rendering every cell as remembered would be a lie rather
    /// than an objective view, so `!map` leaves the fog exactly as `map` draws
    /// it. Deciding per parameter is the point; "both parameters at their
    /// permissive limit" is a half-truth.
    ///
    /// Private, unlike its caller: the objective chart is reached through the
    /// `!map` verb, and a `pub` second charting entry point would be one more
    /// way for a caller who has not read `handle` to draw a chamber-band
    /// chart, which is exactly what the assertion below exists to stop.
    fn purview_through(
        &self,
        zoom_out: u32,
        eyes: &crate::eyes::Eyes,
    ) -> Result<hornvale_scene::SurroundsScene, VesselError> {
        debug_assert!(
            self.inside.is_none(),
            "the walk-band chart marks every derived NPC ungated, so drawing it \
             from inside a chamber would disclose a creature sight withheld"
        );
        crate::purview_scene(
            self.world,
            &self.wctx.ctx,
            &self.position(),
            &self.knowledge,
            &other_bodies(&self.bodies, self.driven),
            &self.ledger,
            self.day,
            zoom_out,
            self.driven_body(),
            eyes,
            self.calendar.as_ref(),
        )
    }

    // ---- The Deed, Task 7: the gate, the clock, and the ledger -----------
    //
    // Three questions a possessed body's act must answer, kept apart on
    // purpose because the plan text that collapsed them violated a ratified
    // decision. MAY it act (the gate)? What does it COST (the clock)? Does it
    // change the WORLD (the ledger)? Decision 0069 answers the third with a
    // flat NO for every band change and every within-room step — "entering a
    // room, moving within it, and leaving *cannot* alter the world" — while
    // saying in the same breath that "the only thing spent is turns", which is
    // the second question answered YES. `clock::base_ticks` already prices a
    // within-room step at a tenth of a room-to-room move, so the cost model
    // has contemplated this all along.

    /// The body's state, as the gate reads it — DERIVED from [`Self::wake_at`]
    /// rather than stored, so the clock advancing past the next waking IS the
    /// waking and there is no second field to fall out of step.
    fn body_state(&self) -> BodyState {
        match self.wake_at {
            Some(wake) if self.day < wake => BodyState::Asleep,
            _ => BodyState::Awake,
        }
    }

    /// The refusal the body owes this bare verb, or `None` to let it through.
    ///
    /// Asked ONCE for the whole bare match rather than per arm, because the
    /// bare namespace *is* the in-character namespace (spec §3.2) — the `!`
    /// namespace is the other one, and [`Self::handle_ooc`] never consults the
    /// gate at all (spec §2.2: an out-of-character act bypasses the body's
    /// state entirely).
    ///
    /// A refusal charges nothing and commits nothing: it is not an act. The
    /// gate is therefore consulted BEFORE the match, never inside a handler
    /// that has already moved the clock.
    ///
    /// **The gate stands in front of ACTS, not in front of verb RESOLUTION**
    /// (fix round 1), which is why it consults [`IN_CHARACTER_VERBS`] rather
    /// than refusing every bare token: a token that resolves to no verb is not
    /// something a body could be too asleep to do, and answering the body's
    /// refusal to one told a player that a retired group-A bare form was a real
    /// in-character verb. That roster's own doc carries the argument and the
    /// guard against it going stale.
    fn refused_by_the_body(&self, verb: &str) -> Option<String> {
        if !Self::gated_by_the_body(verb) {
            return None;
        }
        match verdict(self.body_state(), Mood::InCharacter) {
            Verdict::Permitted => None,
            Verdict::Refused(reason) => Some(body_refusal(&reason)),
        }
    }

    /// Whether the body-state gate stands in front of this bare token at all.
    ///
    /// Ordered, and the order is the rule rather than an implementation
    /// detail: [`SESSION_CONTROL`] is never an act whatever else it is (spec
    /// §3.2 group D — a body you cannot let go of is a hang), and everything
    /// else is gated iff it RESOLVES to an act. `IN_CHARACTER_VERBS` is the
    /// closed half of that; a bare compass direction is the open half, and
    /// [`parse_compass`] is the same recogniser `handle`'s own trailing arms
    /// use, so the two cannot disagree about which tokens are directions.
    ///
    /// The two rosters are disjoint today —
    /// `session_control_is_never_an_in_character_verb` asserts it — so the
    /// first branch changes no behaviour now. It states the precedence anyway,
    /// because the failure it prevents (a group-D verb gated by a sleeping
    /// body) is a hang, and a rule that only holds while a list happens to be
    /// right is the shape this whole fix is about.
    fn gated_by_the_body(verb: &str) -> bool {
        if SESSION_CONTROL.contains(&verb) {
            return false;
        }
        IN_CHARACTER_VERBS.contains(&verb) || parse_compass(verb).is_some()
    }

    /// The planet's rotation period in standard days, as the action clock
    /// needs it — `None` on a tidally-locked world, which the rotation pin
    /// admits. Extracted from `wait`'s own inline read so the player's charge
    /// and the NPC layer's cannot disagree about the tick rate.
    /// type-audit: bare-ok(ratio: return)
    fn day_length_std(&self) -> Option<f64> {
        self.calendar
            .as_ref()
            .and_then(|c| c.day_length())
            .map(|d| d.get())
    }

    /// Charge `action` against THIS BODY'S OWN MASS and advance the day.
    ///
    /// The creature path's two lines, verbatim in shape
    /// (`liveness.rs`'s `DriveMovements` walk): `cost_ticks` keyed on the
    /// action and the body, `days_of` to convert at the commit boundary.
    /// `cost_ticks` takes no driver parameter — it never did — which is the
    /// whole reason routing the player through it makes the tariff the same
    /// tariff rather than a parallel one.
    ///
    /// **Out-of-character acts must never reach here**, and that is enforced
    /// by the caller rather than by a branch inside: `cost_ticks` floors its
    /// result at `Ticks(1)`, so an out-of-character action — every one of
    /// which `base_ticks` deliberately prices at `Ticks(0)` — would be charged
    /// one tick it must not pay, re-creating the second silent clock movement
    /// `base_ticks`'s own comment exists to avoid. See [`Self::handle_ooc`],
    /// which charges nothing.
    ///
    /// Errs on a clock overflow rather than saturating: `wait` already routes
    /// that through its own error channel (a live `possess` stdin can reach
    /// `wait 1e308` twice), and a move that cannot be timed must not happen.
    fn charge(&mut self, action: &Action, terrain_factor: f64) -> Result<(), String> {
        debug_assert_eq!(
            action.mood(),
            Mood::InCharacter,
            "an out-of-character act must not reach the clock: cost_ticks \
             floors at one tick, which base_ticks prices at zero on purpose"
        );
        let ticks = cost_ticks(action, self.body_mass_kg, terrain_factor);
        let days = days_of(ticks, self.day_length_std());
        match advanced_by(self.day, days) {
            Ok(d) => {
                self.day = d;
                Ok(())
            }
            Err(e) => Err(e),
        }
    }

    /// Charge one WITHIN-ROOM step (The Deed, Task 7): `Action::MoveWithin`'s
    /// own dial — a tenth of a room-to-room move, the ratio The Threshold
    /// authored — scaled by this body's mass like every other act. No terrain
    /// factor: `climb_factor` is a `MoveTo` modifier alone (spec §3.1), and a
    /// step inside a room changes no room and so has no elevation pair.
    ///
    /// **It commits nothing, and that is decision 0069, not an omission.**
    /// Fine position is never serialized, so "moving within a room *cannot*
    /// alter the world" — while "the only thing spent is turns", which is this
    /// charge. `the_blocking::walking_a_chamber_commits_nothing` holds the
    /// other half.
    ///
    /// The `AnchorId(0)` payload is a placeholder, exactly as
    /// [`Action::all`](crate::action::Action::all)'s representative is: a
    /// chamber lattice cell is not an interior anchor, and neither
    /// [`crate::clock::cost_ticks`] nor [`Action::mood`] reads the payload.
    /// Passing a real-looking anchor from another graph would be the worse
    /// lie.
    ///
    /// # Three callers, not one (fix round 1)
    ///
    /// [`Self::step`] (one cell), [`Self::enter`] (in off the street, and
    /// through an aperture) and [`Self::leave`]. Task 7 charged only the
    /// first, on the stated grounds that no authored dial existed for the
    /// other two — which was wrong on the tree: `Action::MoveWithin` **is**
    /// that dial, and it was already being read one dispatch arm away for the
    /// strictly FINER act. The result was that stepping one cell cost time
    /// while walking through a doorway into a different chamber was free, and
    /// `out` → `enter` was an unbounded free loop — which the very consequence
    /// paragraph of 0069 relied on not being true ("cheesing by re-entry is
    /// not defended against because it does not exist — the only thing spent
    /// is turns").
    ///
    /// **`dive`/`surface`/`delve`/`climb` are deliberately still free**, and
    /// that is a DIFFERENT case rather than the same one left half-done: those
    /// four change the vertical band — water column, cave — and no dial in
    /// [`crate::clock::base_ticks`] prices a descent. `MoveWithin` would be a
    /// guess dressed as a tariff (a swim down a hundred metres of water column
    /// is not a tenth of a walk between rooms), and inventing one is a cost
    /// model, which spec §3.4 forbids this arc from building. See their own
    /// dispatch arms.
    fn charge_within_room(&mut self) -> Result<(), String> {
        self.charge(&Action::MoveWithin(crate::interior::AnchorId(0)), 1.0)
    }

    /// The uphill factor a walk-band step pays, read exactly as the creature
    /// path reads it: `climb_factor` over the two rooms' elevations, before
    /// the position moves. A `MoveTo` modifier alone (spec §3.1).
    /// type-audit: bare-ok(ratio: return)
    fn climb_to(&self, dest: &RoomAddr) -> f64 {
        let terrain = self.terrain_here();
        climb_factor(terrain.elevation(&self.position()), terrain.elevation(dest))
    }

    /// Commit the possessed body's `agent-at` for `position` — its NEW
    /// position, through the very constructor the NPC layer commits a
    /// creature's with (`liveness::agent_at_fact`), which is what makes the
    /// two indistinguishable BY CONSTRUCTION rather than by two
    /// implementations agreeing today. The caller passes the destination
    /// explicitly (rather than this method reading it off a field) because
    /// The Hand removed the mutable `Agent.position` field a pre-Hand caller
    /// mutated in place before calling this: position is read-model now
    /// ([`Self::position`]), and committing this fact **is** the update
    /// (spec §3.1).
    fn commit_agent_at(&mut self, position: &RoomAddr, provenance: &str) {
        let fact = agent_at_fact(
            self.agent_entity(),
            position,
            self.day.as_std_days(),
            provenance,
        );
        self.ledger
            .commit(fact, &self.registry)
            .expect("AGENT_AT is registered every session and non-functional");
    }

    /// Lie down and sleep (The Deed, Task 7). A new verb, and the only one
    /// this arc adds: the acceptance test needs a body that can stop obeying,
    /// and none of the 26 existing verbs could produce one.
    ///
    /// **It mints nothing.** It routes to the existing [`Action::Rest`] — so
    /// it costs what lying down costs, and commits the same `rested` fact a
    /// creature's own Rest commits — and `rest`/`sleep` are both already
    /// registered concepts, so no concept, cohort or accession entry moves.
    ///
    /// **When it wakes** is `liveness::next_awake_day`, the same scan a
    /// creature's Rest jumps by. That function answers "the next moment this
    /// species is awake", which is at least one scan step away, so the body is
    /// genuinely under until the clock advances. It is honest but coarse: a
    /// body that lies down *during* its own waking phase wakes at the next
    /// scan step rather than sleeping through to the following night. Naming
    /// a finer rule would be inventing a second sleep model beside the
    /// creature layer's, which this task declines to do.
    ///
    /// **An argument is refused, not swallowed** (fix round 1). `sleep 5`
    /// reads as "sleep five days" and cannot be honoured — the body wakes on
    /// its own cycle, which is the whole point of the verb — so it is refused
    /// with a sentence naming what `sleep` *does* do and which verb takes a
    /// length. `map`'s indoor arm states the principle this follows: "an
    /// ignored argument is how a player comes to believe they asked for
    /// something and got it."
    fn sleep(&mut self, arg: &str) -> Turn {
        if !arg.is_empty() {
            return Turn::Out(SLEEP_ARGUMENT_REFUSAL.to_string());
        }
        if let Err(e) = self.charge(&Action::Rest, 1.0) {
            return Turn::Out(e);
        }
        let fact = rested_fact(
            self.agent_entity(),
            self.day.as_std_days(),
            SLEPT_PROVENANCE,
        );
        self.ledger
            .commit(fact, &self.registry)
            .expect("RESTED is registered every session and non-functional");
        let wake = {
            let activity = species_activity(self.world, &self.driven_body().species);
            let terrain = self.terrain_here();
            next_awake_day(activity, &terrain, &self.position(), self.day.as_std_days())
        };
        self.wake_at = WorldTime::from_std_days(wake).ok();
        Turn::Out(SLEEP_REPLY.to_string())
    }

    /// The out-of-character namespace's own dispatch: every verb reachable
    /// behind a leading `!` (The Deed, spec §2.1/§3.2).
    ///
    /// **A named handler, not a second inline match.** [`Self::handle`]'s
    /// bare-verb match was re-indented one level into an `else` at Task 5 and
    /// this file is ~5,800 lines; growing a second match inside it would put
    /// two dispatch tables in one screenful of `git blame`. The split is also
    /// semantic: `!` selects a NAMESPACE, and these are different acts from
    /// their bare spellings rather than the same acts invoked with extra
    /// authority (spec §2.1 reverses the metaplan on exactly this point).
    ///
    /// # Two groups, and what distinguishes them
    ///
    /// **Group A** — `why`/`npcs`/`help`/`eyes`/`whoami`/`provoke`/`soothe` —
    /// are operator instruments with no in-character counterpart, so this
    /// namespace is their only entry point (Task 5 retired the bare forms).
    ///
    /// **Group B's objective halves** — `map`/`examine`/`needs`/`wait` — each
    /// render the same thing their bare twin does, through the same renderer,
    /// with that renderer's own gating parameter taken to its permissive
    /// limit: [`OBJECTIVE_EYES`] for the chart, [`Perceiving::Objectively`]
    /// for the sight gate. There is no second rendering path anywhere in this
    /// arm, deliberately.
    ///
    /// # THE LIMIT IS INERT IN SOME BANDS, AND EACH ARM SAYS WHERE
    ///
    /// Every one of the four discriminates *somewhere* — that is the STOP rule
    /// Task 6 applied per verb, and it is why these four shipped then and
    /// `!look`/`!knows` did not. It is not a promise that each differs from its
    /// bare twin *everywhere*, and three of them are exact aliases in a
    /// nameable band:
    ///
    /// | arm | where it discriminates | where it is an alias, and why |
    /// |---|---|---|
    /// | `!map` | out of doors, and indoors under a lens | indoors under [`crate::lens::Lens::Off`] — the plan carries no colour to decline, so the bytes match `map` exactly |
    /// | `!examine` | the chamber band, on a creature sight withheld | the walk band (the objective eyes reach [`Self::purview_through`] but the legend carries nouns and datums, never colour) and underground (no creature arm at all) |
    /// | `!needs` | the chamber band | out of doors, where `sighting()` is `None` and [`Perceiving::Body`] already *is* the limit |
    /// | `!wait` | the chamber band, on both halves of the motion narration | out of doors, for the same reason as `!needs` |
    ///
    /// An alias here is the honest outcome, not a defect: the objective view of
    /// a place with nothing withheld is the subjective view of it. What would
    /// be a defect is advertising otherwise, so [`HELP`] states the band each
    /// arm is *for* rather than promising a difference it cannot always make.
    ///
    /// # `!look` and `!knows`: shipped in Task 7's fix round, on a DIFFERENT
    /// discriminator
    ///
    /// The table above does not cover them, and that is exact rather than an
    /// omission. Neither relaxes anything: `Session::knows` prints every entry
    /// of `self.knowledge` (perception-filtered upstream at `absorb_here`, not
    /// by this renderer), and `look`'s three band arms
    /// ([`Self::describe_here`], [`Self::describe_chamber_here`],
    /// [`Self::describe_underground_here`]) consult no sight, eyes, lens or
    /// knowledge at all — they render the place, and the place is objective
    /// already. Task 6 read that as "no gate to relax, therefore an alias,
    /// therefore do not ship", and it was right about the renderer.
    ///
    /// **Task 7 supplied the gate they were missing, and it is not a
    /// renderer's.** It is the BODY's, and spec §2.2 has an out-of-character
    /// act bypass it. So `look`/`knows` now discriminate in the one place spec
    /// §3.4 says this namespace exists for — "observing a state you cannot act
    /// in requires a clock you can still advance" — and the most basic
    /// observational verb there is was unavailable in exactly that state.
    /// These two arms therefore relax no parameter and fork no renderer: they
    /// call the same functions their bare twins call, and differ only in that
    /// the body's state cannot refuse them.
    /// `tests/suite/ooc_objective.rs` holds both halves — equality awake, and
    /// divergence asleep.
    fn handle_ooc(&mut self, verb: &str, rest: &str) -> Turn {
        match verb {
            // Group A: the operator instruments (The Deed, spec §3.2).
            // Bare forms are retired — this namespace is their only entry
            // point now, and it carries no in-character counterpart for
            // any of the seven, by design.
            "why" => Turn::Out(self.why(rest)),
            "npcs" => Turn::Out(self.list_npcs()),
            "help" => Turn::Out(HELP.to_string()),
            // Bare `!eyes` reports whose eyes the chart is coloured
            // through and what their projection drops; `!eyes <name>`
            // switches them.
            "eyes" if rest.is_empty() => Turn::Out(self.eyes_report()),
            "eyes" => self.set_eyes(rest),
            "whoami" => Turn::Out(self.whoami()),
            "provoke" => self.act_on_disposition(rest, 1),
            "soothe" => self.act_on_disposition(rest, -1),
            // Group B's objective halves (Task 6). BAND-AWARE IN EXACTLY THE
            // SAME SHAPE their bare twins are, arm for arm: the objective
            // view of a chamber is still a chamber, and an out-of-character
            // form that silently drew the walk band from indoors would
            // disclose the surrounding locale rather than the room the
            // possession stands in. A band guard is not a perception gate,
            // so none of it relaxes here.
            "map" if self.inside.is_some() && rest.is_empty() => {
                self.out(self.plan_here(&OBJECTIVE_EYES))
            }
            "map" if self.inside.is_some() => Turn::Out(INDOOR_CHART_REFUSAL.to_string()),
            "map" => self.map(rest, &OBJECTIVE_EYES),
            "examine" if self.inside.is_some() && !rest.is_empty() => {
                Turn::Out(self.examine_chamber(rest, Perceiving::Objectively))
            }
            "examine" if self.underground.is_some() && !rest.is_empty() => {
                Turn::Out(self.examine_underground(rest))
            }
            "examine" => self.examine(rest, &OBJECTIVE_EYES),
            "needs" => Turn::Out(self.needs(Perceiving::Objectively)),
            // `!look` and `!knows` (Task 7's fix round). BAND-AWARE ARM FOR
            // ARM with the bare spellings, and deliberately calling the very
            // same methods: there is no objective variant of either to reach
            // for, so a second rendering path here would be inventing the
            // difference rather than exposing one. What this namespace buys is
            // that a sleeping — later dominated, unconscious — body can still
            // be looked out of.
            "look" if self.inside.is_some() => self.out(self.describe_chamber_here()),
            "look" if self.submerged.is_some() => self.out(self.describe_here()),
            "look" if self.underground.is_some() => Turn::Out(self.describe_underground_here()),
            "look" => self.out(self.describe_here()),
            "knows" => Turn::Out(self.knows()),
            // The one out-of-character act that MOVES THE CLOCK (spec §3.4).
            // Its objective half is the departure/arrival narration, not the
            // advance: the day advances identically under both moods, which
            // is what makes observing a state you cannot act in possible at
            // all rather than indistinguishable from the game having hung.
            "wait" => self.wait(rest, Perceiving::Objectively),
            other => Turn::Out(format!("No verb '!{other}' ('!help' lists them).")),
        }
    }

    /// One verb, one response. `Turn::Released` ends the possession.
    /// type-audit: bare-ok(prose: line)
    pub fn handle(&mut self, line: &str) -> Turn {
        let line = line.trim();
        let (verb, rest) = match line.split_once(' ') {
            Some((v, r)) => (v, r.trim()),
            None => (line, ""),
        };
        let verb_present = !verb.is_empty();
        if verb_present {
            self.turn += 1;
        }
        // `!` selects the out-of-character namespace (The Deed, spec
        // §2.1/§3.2): stripped here, before verb lookup, so the sigil
        // routes to a SEPARATE lookup table below rather than becoming part
        // of the verb string itself. That separation is load-bearing, not
        // cosmetic — it is what keeps an unclassified "!<in-character
        // verb>" (group B's own `!`-forms are Task 6's job, not this one's)
        // an ordinary unknown-verb refusal instead of silently falling
        // through and aliasing to the bare form's behaviour.
        let ooc = verb.starts_with('!');
        let verb = verb.strip_prefix('!').unwrap_or(verb);
        let turn = if ooc {
            self.handle_ooc(verb, rest)
        } else if let Some(refusal) = self.refused_by_the_body(verb) {
            // The body-state gate (The Deed, Task 7, spec §3.3), consulted
            // ONCE for the whole in-character namespace and BEFORE any
            // handler runs, so a refusal charges no time and commits no fact.
            Turn::Out(refusal)
        } else {
            match verb {
                "" => Turn::Out(String::new()),
                // `look` is the one existing verb that must become band-aware:
                // inside a structure it renders the chamber, out of doors the
                // locale. Everything else reads `self.position()`, which never
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
                "map" if self.inside.is_some() && rest.is_empty() => {
                    self.out(self.plan_here(&self.eyes))
                }
                "map" if self.inside.is_some() => Turn::Out(INDOOR_CHART_REFUSAL.to_string()),
                "map" => self.map(rest, &self.eyes),
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
                "go" if self.submerged.is_some() => {
                    Turn::Out(SUBMERGED_LATERAL_REFUSAL.to_string())
                }
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
                    Turn::Out(self.examine_chamber(rest, Perceiving::Body))
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
                "examine" => self.examine(rest, &self.eyes),
                // `back` retraces the WALK-band trail, so it stays refused where `go`
                // no longer is: the capability this campaign built is intra-chamber
                // GEOMETRY, and a walk-band trail is not geometry.
                "back" if self.inside.is_some() => Turn::Out(INDOOR_BACK_REFUSAL.to_string()),
                "back" if self.submerged.is_some() => {
                    Turn::Out(SUBMERGED_LATERAL_REFUSAL.to_string())
                }
                "back" if self.underground.is_some() => {
                    Turn::Out(UNDERGROUND_LATERAL_REFUSAL.to_string())
                }
                "back" => self.back(),
                "wait" => self.wait(rest, Perceiving::Body),
                "knows" => Turn::Out(self.knows()),
                "needs" => Turn::Out(self.needs(Perceiving::Body)),
                // The one verb this arc adds (The Deed, Task 7): the
                // acceptance test needs a body that can stop obeying, and
                // none of spec §3.2's 26 could produce one. Routed to the
                // existing `Action::Rest` machinery — no new concept, no new
                // cost dial, no new predicate.
                "sleep" => self.sleep(rest),
                "write" => Turn::Out(self.write(rest)),
                "consult" => Turn::Out(self.consult()),
                // THE FOUR VERTICAL BAND CHANGES, AND THE ONE THING THEY DO
                // NOT DO (fix round 1). Each is an in-character act — the gate
                // above stands in front of all four — and each still charges
                // NOTHING, unlike `enter`/`out`/`step` beside them. The
                // difference is not band-vs-band: it is that
                // `clock::base_ticks` prices a within-room step and prices no
                // descent at all, so charging these would mean minting a cost
                // dial, which spec §3.4 forbids ("no new cost model"). Deferred
                // openly rather than folded in with `enter`/`out`, whose dial
                // already existed. See `Self::charge_within_room`'s own doc.
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
                "release" | "quit" => Turn::Released("You let go.".to_string()),
                // A bare compass token IS a movement command. The room names the
                // three nearest bearings — "the nearest ground lies SE, N, SW" —
                // and every one of those tokens must be typeable; `parse_compass`
                // already accepted them, and only this dispatch arm was missing.
                // (The Rhumb: those three no longer bound what `go` accepts, but
                // they are still real destinations the prose promises, so the
                // invariant they were written to satisfy is unchanged.)
                //
                // It carries `go`'s own band guards, and must: this arm dispatches
                // to `self.go` directly, so without them repeated here a bare `n`
                // typed inside a structure would slip past the band split that
                // `"go" if self.inside.is_some()` exists to make, and silently
                // render the neighbouring locale from indoors. Indoors it therefore
                // means what `go n` means indoors — one CELL of the floor plan, and
                // `step` refuses a bare diagonal with the geometry as the reason.
                other if self.inside.is_some() && parse_compass(other).is_some() => {
                    self.step(other)
                }
                other if self.submerged.is_some() && parse_compass(other).is_some() => {
                    Turn::Out(SUBMERGED_LATERAL_REFUSAL.to_string())
                }
                other if self.underground.is_some() && parse_compass(other).is_some() => {
                    Turn::Out(UNDERGROUND_LATERAL_REFUSAL.to_string())
                }
                other if parse_compass(other).is_some() => self.go(other),
                other => Turn::Out(format!("No verb '{other}' ('!help' lists them).")),
            }
        };
        if verb_present {
            self.last_text = match &turn {
                Turn::Out(s) | Turn::Released(s) => s.clone(),
            };
        }
        turn
    }

    /// The water column at the room the possession stands on, shallowest
    /// first; empty on land.
    fn column_here(&self) -> Vec<hornvale_climate::Stratum> {
        let Ok(v) = crate::vantage::observable_at(
            self.world,
            &self.wctx.ctx,
            self.driven_body(),
            &self.position(),
            self.day,
            None,
        ) else {
            return Vec::new();
        };
        let Some(cw) = v.locale.corners.iter().max_by_key(|c| c.weight) else {
            return Vec::new();
        };
        self.wctx
            .ctx
            .water_column_at(hornvale_kernel::CellId(cw.cell))
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
        let terrain = self.wctx.terrain.as_ref()?;
        let v = crate::vantage::observable_at(
            self.world,
            &self.wctx.ctx,
            self.driven_body(),
            &self.position(),
            self.day,
            None,
        )
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
    /// Mirrors `dive`, but with an extra outcome `dive` never needed — TWO
    /// today, and it was THREE until The Drift. `dive`'s own doc warns what
    /// happens when a refusal doesn't name what stopped you: it reads as a
    /// parse failure rather than a fact about the world. So each outcome
    /// below is named:
    ///   1. no cave at this cell at all — say so;
    ///   2. a chamber — descend, and say what the rock here is.
    ///
    /// # THE THIRD OUTCOME WAS REMOVED, AND THE CODE STILL CARRIES ITS ARM
    ///
    /// A cave used to be **SEALED** when its own entrance address
    /// (`branch = 0, band = Undercroft, level = 0`) resolved to no chamber —
    /// spec §3.4 rung 0, *"the void exists and is unreachable"*, a real
    /// chamber a later dig could find rather than a defect. The Deep Realm's
    /// Task 3 measured that a cave's entrance resolved to an actual chamber
    /// only **51.5%** of the time, which is the 0.5 per-address existence
    /// coin showing through.
    ///
    /// **The Drift deleted that coin** (spec §4.1), and a sealed cave is now
    /// **impossible rather than rare**: every cave in shape realizes
    /// chambers, measured `systems_with_open_mouth == systems` on all three
    /// panel seeds (874/874, 1681/1681, 1266/1266) and 0 of 48,316 caves
    /// sealed over thirty worlds. Nathan's ruling (spec amendment B) was to
    /// accept two outcomes and build **restricted passage** later — locked
    /// doors, collapses magic can clear, boss encounters, and the rare
    /// chamber that stays lost with something worth finding
    /// (`MAP-restricted-passage`).
    ///
    /// The sealed branch below is therefore **live code on an unreachable
    /// path**, kept deliberately: it is what restricted passage will speak
    /// through, and `delve_has_two_distinguishable_outcomes` reddens the
    /// moment a sealed cave becomes possible again while that test still
    /// claims two.
    fn delve(&mut self) -> Turn {
        if self.inside.is_some() {
            return Turn::Out("There is no rock to delve into in here.".to_string());
        }
        if self.underground.is_some() {
            return Turn::Out(
                "You are already below; 'climb' brings you back up first.".to_string(),
            );
        }
        self.delve_column(self.chamber_column_here())
    }

    /// [`Self::delve`]'s outcome for an ALREADY-RESOLVED column — the
    /// no-cave refusal plus the sealed/open decision below it.
    ///
    /// Split out for the same reason [`Self::delve_at`] was, one level up: the
    /// no-cave branch used to be reachable from a test only by the flagship's
    /// own starting cell happening to be cave-free, and decision 0134's
    /// terrain epoch put a sealed cave under that cell and falsified the
    /// contingency. Production still reaches this exactly one way, through
    /// `delve` with `chamber_column_here()`, so nothing about the verb's
    /// behaviour moved.
    fn delve_column(
        &mut self,
        column: Option<(hornvale_kernel::CellId, hornvale_terrain::Cave)>,
    ) -> Turn {
        let Some((cell, cave)) = column else {
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
    /// walk-band rooms, so hitting one particular cell by walking is not
    /// something a test should depend on landing.
    ///
    /// **The parenthesis this used to carry — "let alone one with a SEALED
    /// cave specifically, ~48.5% of caves per Task 3's measurement" — is
    /// dead twice over.** The Drift deleted the existence coin that produced
    /// the 48.5%, so the sealed population is now 0 of 48,316 caves over
    /// thirty worlds; and there is consequently no sealed cell to steer to at
    /// all. The seam is still worth having for the reason its first sentence
    /// gives, and it is what restricted passage will be tested through.
    fn delve_at(&mut self, cell: hornvale_kernel::CellId, cave: hornvale_terrain::Cave) -> Turn {
        let addr = hornvale_worldgen::chamber::ChamberAddr {
            cell,
            band: hornvale_kernel::Band::Undercroft,
            branch: 0,
            level: 0,
        };
        let overrides = hornvale_worldgen::chamber::ChamberOverrides::new();
        // The chamber lattice is placed by HEAT since `chamber/v2` (spec
        // §4.1), so the same cave reaches a different distance down it
        // depending on the cell's gradient, and a chamber's stratum is read
        // off that cell's own column. Both come from the same terrain handle
        // `chamber_column_here` already resolved the cave through, so no
        // second, independently-chosen lookup is introduced here.
        let Some(terrain) = self.wctx.terrain.as_ref() else {
            return Turn::Out("There is no cave here to delve into.".to_string());
        };
        let gradient = terrain.geothermal_gradient_at(cell);
        let column = terrain.column_at(cell);
        match hornvale_worldgen::chamber::chamber_at(
            self.world.seed,
            &cave,
            gradient,
            &column,
            addr,
            &overrides,
        ) {
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
        let v = observable(
            self.world,
            &self.wctx.ctx,
            self.driven_body(),
            &self.position(),
            self.day,
        )?;
        self.knowledge
            .absorb(self.projection.project(&v, &self.driven_body().perception));
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
        let v = crate::vantage::observable_at(
            self.world,
            &self.wctx.ctx,
            self.driven_body(),
            &self.position(),
            self.day,
            vantage,
        )?;
        let f = self.focalizer.render(&v);
        // F1 (The Rhumb, final review): this render doubles as the SUBMERGED
        // vantage's (see the `"look"`/`dive`/`surface` arms above), and while
        // under, `go` and a bare compass token both refuse EVERY lateral
        // direction (`SUBMERGED_LATERAL_REFUSAL`) — the walk-band mesh's own
        // laterals do not reach a submerged cell at all. Claiming "no
        // direction here is closed" there would be false the instant the
        // player tried one, which is exactly the class of defect decision
        // 0141 exists to remove. `Ways on: surface.` mirrors
        // `describe_underground_here`'s `Ways on: out.` — the one way on this
        // band actually leads anywhere.
        let closing = if self.submerged.is_some() {
            "Ways on: surface.".to_string()
        } else {
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
            format!(
                "No direction here is closed; the nearest ground lies {}.",
                ways.join(", ")
            )
        };
        Ok(format!(
            "[room {}, day {}]\n{}\n{closing}",
            v.locale.id,
            self.day.as_std_days(),
            f.prose,
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
        // The locale itself is no longer consulted for exit matching — a rhumb
        // course resolves against pure geometry (the current position and its
        // neighbours), not `v.locale.exits` — but the current position must
        // still be observable before a step is taken from it, so the call
        // stays for its error-detection side effect alone.
        let here = self.position();
        if let Err(e) = observable(
            self.world,
            &self.wctx.ctx,
            self.driven_body(),
            &here,
            self.day,
        ) {
            return Turn::Out(format!("error: {e}"));
        }
        let bearing = crate::course::bearing_of(wanted);
        // Continue an existing course only when the bearing is unchanged;
        // any other direction starts a fresh one from where we stand.
        let mut course = match self.course.take() {
            Some(c) if c.bearing_deg == bearing => c,
            _ => crate::course::Course {
                bearing_deg: bearing,
                reckoned: here.coord(),
            },
        };
        let delta = crate::course::step_length_rad(&here);
        course.reckoned = crate::course::rhumb_advance(course.reckoned, bearing, delta);
        let dest = crate::course::nearest_neighbour(&here, course.reckoned);
        // The Deed, Task 7: a walk-band step is an in-character act, so it
        // pays the action clock against this body's own mass and posts the
        // `agent-at` a creature's step posts. Charged BEFORE the position
        // moves — the climb factor is a pair of elevations and the second one
        // is where we are going — and the whole act is abandoned if the clock
        // cannot represent the result, exactly as `wait` abandons an overflow.
        let ground = self.climb_to(&dest);
        if let Err(e) = self.charge(&Action::MoveTo(dest.clone()), ground) {
            return Turn::Out(e);
        }
        self.course = Some(course);
        self.trail.push(here);
        // Decision 0069's committed tier: the WALK band is an entity's
        // persisted position, so this one commits. The fine layer below it —
        // `step`, `enter`/`out`, `dive`/`delve` — does not, and must not.
        // Committing IS the position update now (spec §3.1) — there is no
        // mutable field left to assign `dest` to.
        self.commit_agent_at(&dest, WALKED_PROVENANCE);
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
        // A retrace is a walk-band step like any other (The Deed, Task 7), and
        // the plan's own table omitted it — it charges and commits exactly as
        // `go` does, because decision 0069's committed tier is the ROOM, and
        // `back` changes the room. Charged before the move, and the trail entry
        // is pushed back if the clock refuses, so a failed retrace loses
        // nothing.
        let ground = self.climb_to(&prev);
        if let Err(e) = self.charge(&Action::MoveTo(prev.clone()), ground) {
            self.trail.push(prev);
            return Turn::Out(e);
        }
        // Committing IS the position update now (spec §3.1) — there is no
        // mutable field left to assign `prev` to.
        self.commit_agent_at(&prev, RETRACED_PROVENANCE);
        // A retrace is not a continuation of any heading.
        self.course = None;
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
            // Charged HERE — after every refusal and after the no-floor check,
            // before the position moves — exactly where `step` charges, and for
            // the same reason: a refusal is not an act and must cost nothing,
            // while an act that happens must cost something.
            if let Err(e) = self.charge_within_room() {
                return Turn::Out(e);
            }
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
            &crate::band::truncate_to_walk(&self.position(), self.walk_depth()),
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
        //
        // Charged before `descend`, which is the one place in this function the
        // charge does not sit strictly after every failure: `descend`'s only
        // `None` is the §7 rule 1 no-floor DEFECT, reported as `error:` rather
        // than refused as a move. Reordering to charge after it would mean
        // mutating the position and then discovering the clock cannot represent
        // the result — the inconsistency `go` and `step` both avoid by charging
        // first — so a tick spent on the way to an internal defect report is the
        // cheaper of the two wrongs.
        if let Err(e) = self.charge_within_room() {
            return Turn::Out(e);
        }
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
                if let Err(e) = self.charge_within_room() {
                    return Turn::Out(e);
                }
                let inside = self.inside.as_mut().expect("checked above");
                inside.at = next;
                inside.cell = cell;
                return self.out(self.describe_chamber_here());
            }
        }
        if let Err(e) = self.charge_within_room() {
            return Turn::Out(e);
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
        if self.inside.is_none() {
            return Turn::Out("You are already out of doors.".to_string());
        }
        // Charged, and committing nothing — decision 0069 forbids the commit,
        // not the charge ("the only thing spent is turns"). Without this,
        // `out` → `enter` was an unbounded free loop and a doorway crossing
        // cost less than the single-cell step `step` already priced.
        if let Err(e) = self.charge_within_room() {
            return Turn::Out(e);
        }
        self.inside = None;
        self.out(self.describe_here())
    }

    /// The world's walk depth, as this session's locale context defines it.
    /// A free function in `agent`, wrapped here so the handlers read as
    /// session state rather than as a module path.
    /// type-audit: bare-ok(count: return)
    fn walk_depth(&self) -> u32 {
        crate::agent::walk_depth(&self.wctx.ctx)
    }

    /// The terrain provider, built exactly as every other reader in this module
    /// builds it. NOT `LocaleTerrain::new`: that leaves `built: None`, which
    /// reads as *everything unbuilt*, and `enter` would then report nothing
    /// built anywhere.
    fn terrain_here(&self) -> LocaleTerrain<'_> {
        LocaleTerrain::with_fields(
            &self.wctx.ctx,
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
            self.wctx.ctx.climate().geosphere(),
            self.wctx.ctx.nearest_index(),
            &self.position(),
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
            self.day.as_std_days(),
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
    /// Read through `self.wctx.ctx` rather than `self.wctx.terrain`/`self.wctx.climate` for the
    /// same reason — the locale context is what described this room, so the
    /// fabric and the prose read one world.
    fn fabric_here(&self) -> Option<crate::fabric::FabricContext> {
        // The possession's own position is already walk-band (`Inside` records
        // descent, `Agent::position` does not move), so this truncation is a
        // no-op today. Stated anyway, because `brief_of` truncates identically
        // before its own `containing_cell` call and two readings of one cell
        // that agree only by accident are what this method exists not to be.
        let locale = crate::band::truncate_to_walk(&self.position(), self.walk_depth());
        let cell = crate::brief::containing_cell(
            &locale,
            self.wctx.ctx.climate().geosphere(),
            self.wctx.ctx.nearest_index(),
        )?;
        Some(crate::fabric::FabricContext::at(
            self.wctx.ctx.terrain(),
            self.wctx.ctx.climate(),
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
            // The Wick (spec §2.1): the implicit torch burns four times
            // brighter — "carry more candles". The falloff shape is
            // untouched and pinned by the fence test; hearth and doorway
            // sources keep their own levels.
            illuminant: crate::light::scaled(
                &hornvale_kernel::color::blackbody(crate::light::TORCH_KELVIN),
                4.0, // The Wick, spec §2.1
            ),
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

        for &(_, _, at) in &inside.lattice.doorways {
            sources.push(crate::light::Source {
                at,
                illuminant: self.daylight(),
                radius: SIGHT_RADIUS,
            });
        }
        sources
    }

    /// The world's daylight at the possession's position and day — one call,
    /// shared by the doorway sources and the skyglow ambient so the two can
    /// never disagree about which sky they are under.
    fn daylight(&self) -> hornvale_kernel::color::Illuminant {
        let (day, _altitude) = crate::eyes::daylight_at(
            self.world,
            self.calendar.as_ref(),
            self.day,
            self.position().coord().latitude,
        );
        day
    }

    /// The chamber plan for the room stood in — **the one derivation of the
    /// colour seam**, shared by the wire snapshot and the terminal draw.
    ///
    /// The Lantern's seam, in the order it runs: what the building is made of,
    /// what light reaches each cell, and whose eyes are looking. Any one of the
    /// three missing is a WITHHOLDING (see [`crate::plan::Shading`]) — the plan
    /// comes back exactly as it did before this campaign rather than carrying an
    /// invented colour.
    ///
    /// **It is one method rather than two on purpose.** `snapshot` and
    /// `plan_here` are the same room seen through two grains, and a second copy
    /// of this derivation is exactly how a pane and a picture end up disagreeing
    /// about what colour a wall is — the failure `chart_centre` and
    /// `eyes::daylight_at` each already carry a comment about. Note what is
    /// *not* here: the lens. This function produces the model's own bytes, and
    /// only [`Session::plan_here`] filters them.
    fn chamber_plan(
        &self,
        inside: &Inside,
        marks: Vec<crate::plan::PlanMark>,
        eyes: &crate::eyes::Eyes,
    ) -> Result<crate::plan::SessionPlan, VesselError> {
        let chamber = chamber_id(&inside.structure.chambers[inside.at])?;
        let fabric = self.fabric_here();
        let light = crate::light::light_field(&inside.lattice, &self.chamber_sources(inside));
        let observer = crate::eyes::resolve(eyes, self.driven_body()).map(|(o, _)| o);
        let shading = match (observer.as_ref(), fabric.as_ref()) {
            (Some(observer), Some(fabric)) => Some(crate::plan::Shading {
                observer,
                fabric,
                light: &light,
                ambient: crate::light::scaled(&self.daylight(), crate::light::SKYGLOW_SCALE),
            }),
            _ => None,
        };
        Ok(crate::plan::plan_of(
            &inside.lattice,
            inside.at,
            inside.structure.chambers.len(),
            chamber,
            inside.cell,
            marks,
            shading.as_ref(),
        ))
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
    ///
    /// # Where the lens lands, and why here
    ///
    /// This is the **only** place `Session` filters colour (The Lantern, Task 8,
    /// spec §7). It is the one seam in this repository where an emitted triple
    /// becomes something a person looks at directly and the result is *not*
    /// committed: the wire snapshot is a client fixture, the gallery transcript
    /// is a book page, and both must carry the model's own bytes. A drawn plan
    /// in somebody's terminal is neither.
    ///
    /// Under [`crate::lens::Lens::Off`] — which is what `PossessOpts::default`
    /// and the CLI's `--script` path both select — this function returns exactly
    /// what it returned before the lens existed, byte for byte: no tint, no
    /// escape sequence, no caption. That is not a convenience, it is how the
    /// committed transcripts stay unlensed by construction rather than by
    /// remembering a flag.
    ///
    /// **A plan with no observer takes that same path even under a lens**, and
    /// the arm below records why: with `eyes` resolving to `None` there is no
    /// colour for the lens to filter, so tinting is already the identity and
    /// only the caption would have differed. `!map` reaches it through
    /// [`OBJECTIVE_EYES`]; a bare `map` reaches it after `!eyes off`.
    fn plan_here(&self, eyes: &crate::eyes::Eyes) -> Result<String, VesselError> {
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
        let (picture, disclosure) = match self.lens {
            crate::lens::Lens::Off => (plan.picture, String::new()),
            // NO OBSERVER MEANS NO COLOUR, SO THERE IS NOTHING FOR A LENS TO
            // FILTER (The Deed, Task 6 fix round 1). `eyes::resolve` returns
            // `None` for `Eyes::Off` — which is [`OBJECTIVE_EYES`], what
            // `!map` passes, and equally what a player selects by typing
            // `!eyes off` before a bare `map`. `chamber_plan` then builds its
            // `Shading` as `None`, every palette entry carries `color: None`,
            // and `tint` returns the picture unchanged glyph for glyph. The
            // caption exists so a render through one lens cannot present
            // itself as another; captioning an untinted plan `— lens: lantern`
            // is that same false disclosure with the lens count at zero. So an
            // unobserved plan falls to the `Lens::Off` shape it is already
            // byte-identical to, rather than announcing a filter that did
            // nothing.
            _ if crate::eyes::resolve(eyes, self.driven_body()).is_none() => {
                (plan.picture, String::new())
            }
            lens => {
                let coloured = self.chamber_plan(inside, Vec::new(), eyes)?;
                (
                    tint(&plan.picture, &coloured, &lens),
                    format!(" — lens: {}", lens.label()),
                )
            }
        };
        Ok(format!(
            "[plan: chamber {}, {} of {}{}]\n{}  legend: {}",
            id,
            inside.at + 1,
            inside.structure.chambers.len(),
            disclosure,
            picture,
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
    /// taken (by the possession, or by a creature earlier in `other_bodies`'
    /// own derivation order) is left unplaced rather than stacked.
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
        let room = crate::interior::interior_of(&self.position(), &terrain);
        let mut placed = std::collections::BTreeMap::new();
        for npc in self.colocated_npcs() {
            // Room-CHECKED (`anchor_in`, not `at`): a creature whose recorded
            // anchor belongs to some other room is not standing anywhere here,
            // and reading it against this room's graph is what that method exists
            // to prevent.
            let Some(anchor) = self.occupancy.anchor_in(npc.entity, &self.position()) else {
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
    fn examine_chamber(&self, noun: &str, how: Perceiving) -> String {
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
        //
        // 4. **`how` is the only thing `!examine` changes** (The Deed, Task 6).
        //    Under `Perceiving::Objectively` the label match still resolves
        //    against the co-located roster and still answers with
        //    `creature_datum` — the SAME sentence, for the reason point 2
        //    gives; only the narrowing below is taken to its permissive limit.
        //    Everything above this arm (the anchors, the `you` mark, the glyph
        //    legend) is objective already, which is why the parameter reaches
        //    no further than this.
        if let Some(npc) = self
            .colocated_npcs()
            .into_iter()
            .find(|npc| npc.label.to_lowercase() == wanted)
        {
            let sensed = self.perceived_npcs(how);
            if sensed.iter().any(|n| n.entity == npc.entity) {
                return crate::purview::creature_datum(&npc.label, &npc.species);
            }
        }
        format!("You see no {noun} here.")
    }

    fn wait(&mut self, arg: &str, how: Perceiving) -> Turn {
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
        let before: Vec<RoomAddr> = other_bodies(&self.bodies, self.driven)
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
        //
        // `how` is what `!wait` moves (The Deed, Task 6): at the permissive
        // limit this roster is every creature standing here, so the departure of
        // one the possession could not see is narrated rather than dropped. That,
        // and not the clock, is `!wait`'s discriminator — the clock advances
        // identically under both moods (spec §3.4).
        let sensed_before: std::collections::BTreeSet<EntityId> = self
            .perceived_npcs(how)
            .iter()
            .map(|npc| npc.entity)
            .collect();
        let from = self.day;
        // `days` was validated as finite and positive above, but the SUM can
        // still leave the representable range — an accumulation, not a parse,
        // so the parse-site guard above cannot see it (fix round 1, The Ell
        // Task 2 review: reachable live from `possess` stdin via two `wait
        // 1e308`s). Route it through `wait`'s own error channel rather than
        // expecting.
        self.day = match advanced_by(self.day, days) {
            Ok(d) => d,
            Err(e) => return Turn::Out(e),
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
            let geo = self.wctx.ctx.climate().geosphere();
            let index = self.wctx.ctx.nearest_index();
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
            &self.wctx.ctx,
            self.calendar.as_ref(),
            self.predator.as_ref(),
            self.prey.as_ref(),
            Some(&self.built),
            Some(&mesh_snapshot),
        );
        let sys = DriveMovements {
            // `DriveMovements.npcs: Vec<Body>` is a widely-shared field
            // (28+ construction sites across `windows/vessel`/`windows/lab`),
            // so this clones out of `other_bodies`'s borrows rather than
            // widening that struct.
            npcs: other_bodies(&self.bodies, self.driven)
                .into_iter()
                .cloned()
                .collect(),
            from,
            to: self.day,
            params: SUSTENANCE,
            // The planet's rotation period, so the action clock's tick divides
            // the local day exactly (The Action Clock, spec §4.1). `None` on a
            // tidally-locked world, which the rotation pin admits.
            day_length_std: self.day_length_std(),
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
        // The driven body's OWN arbitration (The Hand, Task 5 fix round 1,
        // spec §2.3/§3.3): the SAME `advance_one` every other body's walk
        // just called, in a solo band-of-one walk (`step_one_with_controller`'s
        // own doc says why it is not folded into `sys.npcs` above), asked
        // through a FRESH `PlayerController` — nothing queues an action on
        // it yet, so its intent is unconditionally `Hold`.
        //
        // **What actually keeps the ledger clean is the next line, not the
        // `Hold` (fix round 3, N4): `_driven_facts` is discarded
        // UNCONDITIONALLY, regardless of what `step_one_with_controller`
        // returns.** An earlier version of this comment claimed this was
        // "the commits on `Do`, nothing on `Hold` argument spec §5.2
        // makes" — checked directly (fix round 2) and that claim is false:
        // forcing the intent to `Do` here still leaves the ledger untouched,
        // because the facts never reach `tick()` either way. The player's
        // verbs (`go`, `drink`, …) are what the body DOES; this walk only
        // ever supplies what the host WANTS (`self.driven_mode`) — spec
        // §5.2 is being corrected at Task 8 to say so. Cloned out of
        // `self.bodies` first: `driven_body()` borrows all of `self`, which
        // cannot coexist with the `&mut self.mesh_memo`/`&mut
        // self.home_nav_cache` borrows this call needs.
        let driven_npc = self.driven_body().clone();
        let (_driven_facts, driven_mode) = sys.step_one_with_controller(
            &self.ledger,
            &driven_npc,
            &mut self.mesh_memo,
            &mut self.home_nav_cache,
            &mut PlayerController::new(),
        );
        self.driven_mode = Some(driven_mode);
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
                // `other_bodies` in its existing (derivation) order keeps the
                // commit sequence deterministic. A free function, not a
                // `self.npcs.iter()` field read, but the same disjoint-field
                // borrow: it borrows only `self.bodies`, leaving `self.ledger`
                // (mutated below, inside this very loop) free.
                let player = self.agent_entity();
                for npc in other_bodies(&self.bodies, self.driven) {
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
                            day: Some(self.day),
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
                Turn::Out(self.narrate_motion(moved, &before, &sensed_before, how))
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
        how: Perceiving,
    ) -> String {
        if moved == 0 {
            return "Time passes; the world keeps its shape.".to_string();
        }
        // The arrival half's gate, and it takes `how` for the same reason
        // `sensed_before` (the departure half's) does: an ARRIVAL is judged
        // against who can be seen NOW, a departure against who could be seen
        // THEN, and an out-of-character `wait` must relax both or it would
        // narrate half the traffic. One parameter, both moments (The Deed,
        // Task 6).
        let sensed_now: std::collections::BTreeSet<EntityId> = self
            .perceived_npcs(how)
            .iter()
            .map(|npc| npc.entity)
            .collect();
        let mut arrived: Vec<&str> = Vec::new();
        let mut departed: Vec<&str> = Vec::new();
        for (npc, prior) in other_bodies(&self.bodies, self.driven).iter().zip(before) {
            let was_here = *prior == self.position();
            let is_here = agent_position(&self.ledger, npc, self.day) == self.position();
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
    fn map(&self, rest: &str, eyes: &crate::eyes::Eyes) -> Turn {
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
        let depth = self.position().depth();
        let max_zoom = depth.saturating_sub(self.wctx.ctx.globe_level());
        if zoom > max_zoom {
            return Turn::Out(
                "There is no coarser rung to show; the chart already draws at the coarsest \
                 the world allows."
                    .to_string(),
            );
        }
        let scene = match self.purview_through(zoom, eyes) {
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
        let centre = crate::chart_centre(&self.position(), zoom);
        let ways: Vec<String> = match self.wctx.ctx.describe(&centre, self.day) {
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
        let lens = if eyes == &crate::eyes::Eyes::Off {
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
        let Some((observer, name)) = crate::eyes::resolve(&self.eyes, self.driven_body()) else {
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
    fn examine(&self, noun: &str, eyes: &crate::eyes::Eyes) -> Turn {
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
        // Drawn through the CALLER's eyes (The Deed, Task 6). Nothing
        // observable turns on it here — the legend carries nouns and datums,
        // not colour — but `!examine` claiming an objective read while
        // building the scene through this body's photoreceptors would be a
        // small lie, and the objective draw is the cheaper of the two anyway.
        let scene = match self.purview_through(0, eyes) {
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
        let npc = self.driven_body();
        format!(
            "A {} of {} (agent {}), day {}, room {}.",
            npc.species,
            village_or_fallback(npc).name,
            npc.entity.0,
            self.day.as_std_days(),
            self.position()
                .pack()
                .map(|r| r.0.to_string())
                .unwrap_or_else(|_| "?".to_string()),
        )
    }

    /// List every derived NPC this session knows about, with a short,
    /// typeable handle `why` (and `provoke`/`soothe`/`npc_grievance`) accept:
    /// the NPC's 1-based position in `other_bodies` (every derived body except
    /// the one being driven), not its `EntityId`. The
    /// entity id is a wide, lineage-derived value (The Signet) that a player
    /// cannot reasonably type back; the handle is a display/input affordance
    /// only, scoped to this listing within this session — it is never stored
    /// and never crosses into a committed fact.
    fn list_npcs(&self) -> String {
        let others = other_bodies(&self.bodies, self.driven);
        let mut lines = vec![format!("{} NPC(s) derived this session:", others.len())];
        for (i, npc) in others.iter().enumerate() {
            lines.push(format!("  [{}] {}", i + 1, npc.label));
        }
        lines.join("\n")
    }

    /// Recount an NPC's dated history — the provenance read (the-quickening
    /// T4): the world remembers, so `why` over an NPC that has moved names
    /// each committed `agent-at` with the day it was asserted (`recount` in
    /// `windows/historiography` renders the day suffix). `who` is matched
    /// first as the `npcs` listing's 1-based handle, else as a
    /// case-insensitive substring of an NPC's label — this mirrors the CLI
    /// repl's `why <id>` (see `cli/src/repl.rs`) over the one kind of subject
    /// a possess session actually has on hand without a prior listing step:
    /// a name. The handle is deliberately NOT the NPC's `EntityId` (The
    /// Signet) — it is a short-lived, session-local position a player can
    /// type back, resolved fresh from `other_bodies` on every call.
    fn why(&self, who: &str) -> String {
        let who = who.trim();
        if who.is_empty() {
            return "Why what? Name an NPC (label or number — see 'npcs').".to_string();
        }
        let others = other_bodies(&self.bodies, self.driven);
        let target = who
            .parse::<usize>()
            .ok()
            .filter(|n| *n >= 1)
            .and_then(|n| others.get(n - 1))
            .or_else(|| {
                let needle = who.to_lowercase();
                others
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
    ///
    /// **THE HAND, TASK 6: THIS IS A PLACEHOLDER, LABELLED AS ONE.** The
    /// exclusion of the driven body is a single hardcoded index
    /// (`other_bodies(&self.bodies, self.driven)`, not a fixed `0` — see that
    /// function's own docs) applied by a linear scan over a roster that is
    /// always tiny. It covers exactly one thing: keeping the driven body out
    /// of its own "who else is here" answer. It does not cover, and is not
    /// meant to cover, an area query, a scenery/exclusion component, or a
    /// cached index — none of that exists yet. The shape that supersedes
    /// this is a component-style exclusion: creatures in the area lacking an
    /// `ExcludeFromWhoElse`-like marker, served by an indexed query and
    /// iterated as an array (the Infocom/Inform scenery-flag pattern). That
    /// is Penstock-lineage work, not this task's.
    fn colocated_npcs(&self) -> Vec<&Body> {
        // `.into_iter()`, not `.iter()`: `other_bodies` returns an owned
        // `Vec<&Body>` now (The Hand, Task 4 fix round 1), so `.into_iter()`
        // yields `&Body` directly — `.iter()` would yield `&&Body` and this
        // could no longer collect into `Vec<&Body>`.
        other_bodies(&self.bodies, self.driven)
            .into_iter()
            .filter(|npc| agent_position(&self.ledger, npc, self.day) == self.position())
            .collect()
    }

    /// Who else is here, as stable entity identities — [`Self::colocated_npcs`]
    /// with the borrow stripped off, so a caller (or a test) can ask the
    /// question without going through prose. Carries the same placeholder
    /// scope [`Self::colocated_npcs`] documents: a linear scan excluding one
    /// index, not an area query.
    pub fn colocated_entities(&self) -> Vec<EntityId> {
        self.colocated_npcs()
            .into_iter()
            .map(|npc| npc.entity)
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
    ///
    /// **One caller was added and no fourth row** (The Deed, Task 6):
    /// [`Self::perceived_npcs`] chooses between this predicate narrowed by
    /// `sighting()` and this predicate called with `None`. The
    /// out-of-character halves take exactly the limit the sentence above
    /// already describes, so every row of the table holds under both moods.
    fn sensed_npcs(&self, sighting: Option<&Sighting>) -> Vec<&Body> {
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

    /// Who is here, as `how` decides: [`Perceiving::Body`] narrows by sight,
    /// [`Perceiving::Objectively`] does not.
    ///
    /// **The single definition of group B's permissive limit** (The Deed,
    /// Task 6). Every out-of-character half routes through here rather than
    /// each writing `sensed_npcs(None)` for itself, for the same reason
    /// `sensed_npcs` exists at all: three copies of a perception rule is how
    /// a verb and a channel come to disagree about who the possession can
    /// see, which is the defect The Sighting spent four fix rounds closing.
    fn perceived_npcs(&self, how: Perceiving) -> Vec<&Body> {
        match how {
            Perceiving::Body => self.sensed_npcs(self.sighting().as_ref()),
            // `sensed_npcs`' own filter with nothing to filter ON — exactly
            // what it already computes out of doors, where `sighting` is
            // `None`. Not a bypass of the predicate: the predicate itself,
            // at its limit.
            Perceiving::Objectively => self.sensed_npcs(None),
        }
    }

    /// Resolve `who` to one **sensed** co-located NPC (The First Mark): an empty
    /// argument selects the first such NPC (the common case — a lone co-located
    /// NPC needs no name), otherwise `who` is matched as the `npcs` listing's
    /// 1-based handle or a case-insensitive substring of an NPC's label,
    /// mirroring `why`'s resolution but restricted to NPCs actually here.
    /// The handle is resolved against `other_bodies` (so it means the same
    /// number `npcs` printed) and then re-checked against `here` — resolving
    /// it directly against `here`'s own positions would let a handle's
    /// meaning shift with who happens to be sensed, and silently answer for
    /// an NPC the player typed a stale number for.
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
    fn colocated_npc(&self, who: &str) -> Option<&Body> {
        let here = self.sensed_npcs(self.sighting().as_ref());
        let who = who.trim();
        if who.is_empty() {
            return here.into_iter().next();
        }
        who.parse::<usize>()
            .ok()
            .filter(|n| *n >= 1)
            // `.copied()`: `other_bodies` (now an owned `Vec<&Body>`, The Hand,
            // Task 4 fix round 1) is a temporary here, and `.get()` on it
            // borrows from that temporary — `.copied()` copies the `&Body` it
            // holds out before the temporary Vec drops, rather than trying to
            // return a reference into it.
            .and_then(|n| other_bodies(&self.bodies, self.driven).get(n - 1).copied())
            .filter(|npc| here.iter().any(|h| h.entity == npc.entity))
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
            day: Some(self.day),
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
    fn needs(&self, how: Perceiving) -> String {
        // GATED ON SIGHT, through the same predicate `sensed.present` and
        // `examine` use (The Sighting, fix round 2). Ungated this verb was a
        // side channel straight around the structural redaction `snapshot` had
        // just performed: it named — by label AND by felt state — a creature the
        // pane had withheld one verb earlier. `sensed_npcs` is `colocated_npcs`
        // out of doors, so nothing outside the chamber band changes.
        //
        // `how` is the ONLY difference between `needs` and `!needs` (The Deed,
        // Task 6). The bare verb passes `Perceiving::Body` and is byte-identical
        // to what it was; the out-of-character half passes
        // `Perceiving::Objectively` and reads the same felt state through the
        // same arbitration for every creature actually standing here. Note that
        // this leaves the gate above intact rather than removing it: the side
        // channel The Sighting closed was the BARE verb walking around a
        // redaction, and the bare verb still cannot.
        let here = self.perceived_npcs(how);
        if here.is_empty() {
            return "No one else is here to read.".to_string();
        }
        // Read each co-located NPC's felt state through the SAME arbitration
        // that drives it (spec §7) — the affect label coloured by what the
        // feeling is about (its intentional object), not a bare thirst scalar.
        // `&self`-only: shares whatever `self.mesh_memo` already holds
        // (free — no mutation), same posture as `snapshot`.
        let terrain = LocaleTerrain::with_fields(
            &self.wctx.ctx,
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
        // Cloned once, outside the `.map()` below — see `snapshot`'s
        // identical comment on why (`other_bodies` now allocates, and
        // `affect_of_memo_occupied`'s shared `band: &[Body]` needs owned data
        // to borrow from).
        let band: Vec<Body> = other_bodies(&self.bodies, self.driven)
            .into_iter()
            .cloned()
            .collect();
        here.iter()
            .map(|npc| {
                let affect = affect_of_memo_occupied(
                    &self.ledger,
                    npc,
                    &band,
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
    /// `self.wctx.terrain`/`self.wctx.climate` when both are `Some`, so a session's
    /// repeated `write` calls share `start`'s one sculpt instead of
    /// re-sculpting the globe every turn, the same posture as `consult`.
    fn write(&mut self, line: &str) -> String {
        if line.is_empty() {
            return "Write what? Speak a line of Common.".to_string();
        }
        let ctx = match (self.wctx.terrain.as_ref(), self.wctx.climate.as_ref()) {
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
    /// calls the `_from` twin of each with `self.wctx.terrain`/`self.wctx.climate`
    /// when both are `Some`, so a session's repeated `consult`/`write`
    /// calls share one sculpt instead of re-sculpting the globe every turn;
    /// falls back to the re-sculpting bare form on the `None` a failed
    /// build at `start` would leave.
    fn consult(&self) -> String {
        // `whole_days()` FLOORS where `trunc() as u64` truncated toward zero
        // and then saturated (The Escapement, decision 0186). A session's day
        // is non-negative in practice, so the two agree here — but "day -1
        // reads as day 0" is not a property this line should depend on.
        let day = self.day.whole_days();
        let mut lines = vec![format!("The Reckoning, at day {day}.")];
        let at = hornvale_astronomy::StdDays::new(self.day.as_std_days())
            .expect("a session's day is always finite and non-negative");
        let epoch = match (self.wctx.terrain.as_ref(), self.wctx.climate.as_ref()) {
            (Some(t), Some(c)) => hornvale_book::reckoning_at_from(self.world, at, t, c),
            _ => hornvale_book::reckoning_at(self.world, at),
        };
        lines.extend(epoch.lines);
        lines.extend(epoch.margin);
        let reader = reader_set(&self.knowledge);
        let initiated = match (self.wctx.terrain.as_ref(), self.wctx.climate.as_ref()) {
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

/// One glyph wrapped in a 24-bit foreground colour and a reset.
///
/// Truecolor rather than the 256-colour cube, for the reason
/// `hornvale_scene::surrounds_ascii` already gives at its own escape: a terminal
/// that does not understand truecolor degrades to an uncoloured glyph rather
/// than to a *wrong* one.
fn ansi(glyph: char, rgb: [u8; 3]) -> String {
    format!(
        "\u{1b}[38;2;{};{};{}m{glyph}\u{1b}[0m",
        rgb[0], rgb[1], rgb[2]
    )
}

/// The drawn plan's picture with each glyph tinted by its own cell's colour,
/// seen through `lens`.
///
/// `plan.cells` is row-major over exactly the extent `picture`'s rows were drawn
/// from, so the two are indexed by the same arithmetic rather than by re-reading
/// the lattice — a second traversal is how a picture and a palette start
/// disagreeing about which cell is which.
///
/// **Two glyphs keep no tint**, and both absences are deliberate:
///
/// - the `@` mark, because it draws *you*, not the ground under you — the same
///   withholding `hornvale_scene`'s colour lens makes for marks and for the
///   standing cell;
/// - any cell whose palette entry carries no colour, because absence there means
///   "no colour is claimed here", never black (a threshold has no fabric, an
///   unlit cell is absent from the light field, and a declined observer emits
///   nothing) — see [`crate::plan::PaletteEntry::color`].
fn tint(picture: &str, plan: &crate::plan::SessionPlan, lens: &crate::lens::Lens) -> String {
    let stride = plan.extent.w.max(0) as usize;
    let mut out = String::with_capacity(picture.len() * 4);
    for (row, line) in picture.lines().enumerate() {
        for (col, glyph) in line.chars().enumerate() {
            let colour = (glyph != crate::lattice::render::YOU)
                .then(|| plan.cells.get(row * stride + col))
                .flatten()
                .and_then(|&ix| plan.palette.get(ix as usize))
                .and_then(|entry| entry.color);
            match colour {
                Some(rgb) => out.push_str(&ansi(glyph, crate::lens::apply(lens, rgb))),
                None => out.push(glyph),
            }
        }
        out.push('\n');
    }
    out
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

/// A bearing abbreviated, for a list: `N`. The SAME spelling the outdoor
/// nearest-ground sentence uses (`describe_here` uppercases the debug name)
/// and the indoor `Ways on:` footer below uses too, so one player habit
/// reads both bands.
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

    /// The verbs [`HELP`]'s own `verbs:` block lists, one per line at exactly
    /// two spaces of indent (a continuation line is indented far deeper, and
    /// the out-of-character sections come after the first blank line).
    fn help_verbs() -> Vec<&'static str> {
        HELP.lines()
            .take_while(|l| !l.is_empty())
            .filter(|l| l.starts_with("  ") && !l.starts_with("   "))
            .filter_map(|l| l.split_whitespace().next())
            .collect()
    }

    /// The body-state gate consults [`IN_CHARACTER_VERBS`], which is a second
    /// copy of what [`Session::handle`]'s bare match resolves — so it can drift
    /// from that match silently, and in the dangerous direction (a new verb
    /// nobody gated). [`HELP`] is the third copy and the one a player reads, so
    /// it is already obliged to be complete; agreement in BOTH directions is
    /// what makes the drift detectable rather than merely unlikely.
    ///
    /// FIRES WHEN: a bare verb is added to the dispatch and to `HELP` but not
    /// to the roster (it would be ungated), or removed from the dispatch and
    /// `HELP` but left in the roster (it would gate a token that resolves to
    /// nothing — the very defect the roster exists to fix).
    #[test]
    fn every_bare_verb_help_lists_is_classified() {
        let listed = help_verbs();
        assert!(
            listed.len() > 10,
            "the HELP parse found only {listed:?} — it has stopped finding the \
             verb block, and every assertion below would be vacuous"
        );
        for verb in &listed {
            assert!(
                IN_CHARACTER_VERBS.contains(verb) || SESSION_CONTROL.contains(verb),
                "`{verb}` is offered to players by HELP but classified by neither \
                 IN_CHARACTER_VERBS nor SESSION_CONTROL, so the body-state gate \
                 does not know whether it is an act"
            );
        }
        for verb in IN_CHARACTER_VERBS {
            assert!(
                listed.contains(&verb),
                "`{verb}` is gated as an in-character verb but HELP does not \
                 list it: either it is not a verb at all (and the gate now \
                 refuses a token that resolves to nothing) or players cannot \
                 discover it"
            );
        }
    }

    /// Group D is not an act and carries no [`Mood`] (spec §3.2), so it must
    /// never appear in the in-character roster: a body you cannot let go of is
    /// a hang, not a capability.
    ///
    /// FIRES WHEN: `release`/`quit`/`exit` is added to [`IN_CHARACTER_VERBS`].
    #[test]
    fn session_control_is_never_an_in_character_verb() {
        for verb in SESSION_CONTROL {
            assert!(
                !IN_CHARACTER_VERBS.contains(&verb),
                "`{verb}` is session control and must never be gated by the body"
            );
        }
    }

    /// H2. Every one of the eight compass points moves the possession from a
    /// walk-band cell. This is the campaign's central claim and the whole of
    /// the availability half of the defect.
    ///
    /// FIRES WHEN: `go` reverts to exact-matching one of the three exits.
    #[test]
    fn every_compass_point_moves_the_possession() {
        // ONE world, eight sessions. Each direction must resolve from the same
        // starting cell, so the session is fresh per direction — but genesis is
        // far too expensive to repeat eight times, so the world is not.
        let world = world_at(42).expect("seed 42 builds");
        for dir in ["n", "ne", "e", "se", "s", "sw", "w", "nw"] {
            let (mut s, _) =
                Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
            let before = s.position();
            let turn = s.handle(&format!("go {dir}"));
            let text = match turn {
                Turn::Out(t) => t,
                Turn::Released(t) => panic!("go {dir} released the possession: {t}"),
            };
            assert!(!text.contains("No way"), "go {dir} refused with: {text}");
            assert_ne!(s.position(), before, "go {dir} did not move");
        }
    }

    /// `back` clears the course, so a subsequent `go e` starts fresh rather
    /// than continuing a reckoning from before the retrace.
    #[test]
    fn back_clears_the_course() {
        let world = world_at(42).expect("seed 42 builds");
        let (mut s, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
        s.handle("go e");
        assert!(s.course().is_some());
        s.handle("back");
        assert!(s.course().is_none(), "back left a stale course");
    }

    /// The XOR applied to `Inside::seed` by
    /// [`perturbing_the_embedding_moves_what_is_drawn_and_not_what_is_known`].
    /// An arbitrary constant — its only job is to be a DIFFERENT draw of the
    /// same placement.
    /// type-audit: bare-ok(constructor-edge)
    const PERTURBATION: u64 = 0x5169_4741_u64;

    /// The regression this pins: `examine` must be able to tell "the lens
    /// itself failed" from "no grain surfaced that noun" — before this fix,
    /// `lens_nouns` swallowed both `focalized()`'s and `purview(0)`'s errors
    /// into a bare empty `Vec`, so a genuine lens failure rendered as the
    /// same "You see no <noun> here." as an honest absence. We force
    /// `focalized()` to fail by corrupting the possessed agent's own
    /// position with an out-of-range path digit (`RoomAddr::pack` rejects
    /// any digit >= 4 — see `kernel/src/room.rs`), which `LocaleContext::
    /// describe` hits on its very first line, well before any geometry
    /// runs. This mutates the session's private state directly (this test
    /// lives inside the `session` module for exactly that access) rather
    /// than reaching for a public setter that would let ordinary callers
    /// corrupt a session's position too.
    ///
    /// **The Hand, Task 3: position is a ledger-derived read, not a mutable
    /// field.** At turn 0 (before any `go`/`back` has committed an
    /// `agent-at`), [`Session::position`] falls back to the driven body's
    /// `home` (`liveness::agent_position`), so corrupting `home` in place
    /// achieves the same "position now describes nowhere real" effect
    /// `session.agent.position.path.push(99)` used to — mutating the
    /// TEMPORARY `session.position()` now returns would compile but corrupt
    /// nothing, since nothing holds onto it past this statement.
    #[test]
    fn examine_reports_a_genuine_lens_failure_loudly_not_as_an_absence() {
        let w = seam_world();
        let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        // Sanity: examine must work normally before we break anything.
        assert!(
            session.focalized().is_ok(),
            "the fixture session must start in a healthy state"
        );
        session.bodies[session.driven].home.path.push(99);
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
        // Skip entries whose first word is an ARTICLE. The head word is what
        // this test hands to `examine`, and nobody examines "a" — an entry
        // like "a karst cave" makes the assertion below vacuous while looking
        // like it passed, or (as at decision 0134, when the terrain epoch
        // reordered the legend and floated exactly that entry to the front)
        // fails on a word no matcher should ever have resolved. The property
        // under test is that a real noun in a multi-word name resolves; the
        // selection has to actually deliver one.
        let mark = scene
            .legend
            .iter()
            .find(|e| {
                let mut w = e.noun.split_whitespace();
                let head = w.next().unwrap_or_default().to_lowercase();
                w.next().is_some() && !matches!(head.as_str(), "a" | "an" | "the")
            })
            .expect("some legend entry is a multi-word name headed by a noun");
        let head = mark
            .noun
            .split_whitespace()
            .next()
            .expect("a multi-word name has a first word")
            .to_lowercase();
        let reply = match session.examine(&head, &session.eyes) {
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
        let reply = match session.examine("a-noun-no-grain-surfaced", &session.eyes) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("examine must not release"),
        };
        assert!(
            reply.starts_with("You see no"),
            "a healthy lens with no matching noun must refuse plainly: \
             got {reply:?}"
        );
    }

    /// claim: structural(seed: none — seam_world() fixture) — false-positive
    /// seed-loop flag; `s` binds an NPC social status
    #[test]
    fn the_opening_snapshot_carries_every_channel() {
        let world = seam_world();
        let (session, opening) = Session::start(&world, &PossessOpts::default()).unwrap();
        let snap = session.snapshot().expect("a live session snapshots");

        assert_eq!(snap.schema, crate::SESSION_SCHEMA);
        assert_eq!(snap.turn, 0, "the opening is turn 0");
        assert_eq!(snap.day, 0.5, "PossessOpts::default() is noon");
        assert!(!snap.me.species.is_empty());
        assert_eq!(snap.me.room, session.position().pack().unwrap().0);
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
        // Incidental choice of a second verb — this test pins turn
        // bookkeeping, not `!whoami`'s own answer, so any recognised verb
        // would do. Sigilled anyway (The Deed, Task 5) rather than leaning
        // on the now-retired bare form's unknown-verb refusal, which would
        // still pass here (a refusal is a non-empty, turn-consuming `Turn`
        // too — see `handle`'s own `verb_present` bookkeeping) but for a
        // reason this test does not intend to exercise.
        session.handle("!whoami");
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
            other_bodies(&session.bodies, session.driven)
                .iter()
                .all(|n| session.occupancy.at(n.entity).is_none()),
            "before any `wait`, occupancy has never been populated"
        );
        session.wait("1", Perceiving::Body);
        for npc in other_bodies(&session.bodies, session.driven) {
            assert!(
                session.occupancy.at(npc.entity).is_some(),
                "after `wait`, every derived npc must have a tracked within-room anchor: {}",
                npc.label
            );
        }
    }

    #[test]
    fn wait_routes_a_construction_failure_instead_of_panicking() {
        // Fix round 1 (The Ell, Task 2 review, Important finding): `wait`
        // validates its PARSED argument (`d.is_finite() && d > 0.0`), but the
        // day it feeds `WorldTime::from_std_days` is an ACCUMULATION
        // (`self.day.as_std_days() + days`), so a single adversarial `wait
        // 1e308` driven live from `possess`'s stdin can build a value
        // `from_std_days` rejects even though every individual operand it
        // saw was finite. That used to `.expect()`, so it panicked the
        // whole process instead of failing one verb.
        //
        // The Escapement's Ruling 9 (kernel/src/field.rs) changed WHICH
        // rejection is reachable here, and this test used to assert on the
        // wrong one. `from_std_days` now checks the representable tick
        // range as well as finiteness, and that range is roughly 294
        // orders of magnitude narrower than f64's own range
        // (`i64::MAX / TICKS_PER_STD_DAY` is ~9.22e13 days; `f64::MAX` is
        // ~1.80e308) — so two constructible `WorldTime`s can never sum to
        // `f64::MAX`, let alone overflow it to infinity: overflow-to-infinity
        // is no longer reachable through this accumulation at all.
        //
        // **WHICH ARM THIS TEST REACHES, stated exactly** (fix round 1, code
        // review Important 2). An earlier version of this comment claimed the
        // rejection came "from the exact same `Err` arm of the exact same
        // `from_std_days` call" as before. That is no longer true, and the
        // claim was hiding a coverage hole: post-flip the INCREMENT and the SUM
        // are checked by two different calls in `advanced_by` —
        // `TickSpan::from_std_days`'s range check, then `i64::checked_add` —
        // and waiting `f64::MAX` from genesis is rejected by the FIRST, before
        // any accumulation happens. So this test covers the crossing, not the
        // accumulation; `wait_routes_a_tick_overflow_in_the_accumulation`
        // covers the other arm. What this one still proves, and what it exists
        // for, is the ROUTING: a construction failure inside `wait` returns
        // through `wait`'s own error channel, never a panic and never a
        // release. Starting from `WorldTime::GENESIS` (day 0.0) and
        // waiting `f64::MAX` days is now the adversarial case itself,
        // closer to The Ell's original "a single `wait 1e308`" than the
        // old setup's hand-built `f64::MAX` starting day, and it needs no
        // unconstructible state to get there.
        let world = seam_world();
        let (mut session, _) = Session::start(
            &world,
            &PossessOpts {
                day: WorldTime::GENESIS,
                ..PossessOpts::default()
            },
        )
        .unwrap();
        match session.wait(&f64::MAX.to_string(), Perceiving::Body) {
            Turn::Out(msg) => assert!(
                msg.contains("outside the representable tick range"),
                "expected an error naming the tick-range rejection, got: {msg}"
            ),
            Turn::Released(_) => panic!("a rejected wait must not release the session"),
        }
    }

    #[test]
    fn wait_routes_a_tick_overflow_in_the_accumulation() {
        // THE ARM THE TEST ABOVE DOES NOT REACH (fix round 1, code review
        // Important 2). `advanced_by` checks the increment and the sum
        // separately, and only an increment that PASSES the range check can
        // exercise the sum's `checked_add`. Here the increment is tiny and
        // obviously valid — 20 ticks — while the session's clock already sits
        // ten ticks below `i64::MAX`, so the SUM is what fails. That is the
        // accumulation The Ell's finding was about, now with an integer failure
        // mode instead of an `f64` one, and it asserts the half the unit test
        // below cannot: that the overflow routes through `wait`'s OWN error
        // channel rather than panicking or releasing the session.
        //
        // **Starting near the ceiling rather than accumulating up to it is a
        // COST decision, and it was measured, not guessed.** The obvious
        // spelling — successive `wait`s until the sum overflows — costs a full
        // drive tick per wait over an enormous window, and the cost tracks the
        // total simulated span rather than the number of waits: ten `wait
        // 1e13`s measured 49.2 s and two `wait 9e13`s measured 41.8 s, against
        // 4.7 s for `Session::start` plus one rejected wait. This spelling pays
        // only that 4.7 s floor, because the window it simulates is 20 ticks
        // wide. Asserted on the SUM's own message, which is deliberately
        // distinct from the increment's: while both arms read "outside the
        // representable tick range" no test could tell which one it had
        // reached, and this arm had zero coverage while appearing to have some.
        let world = seam_world();
        let near_ceiling = WorldTime::from_ticks(i64::MAX - 10);
        let (mut session, _) = Session::start(
            &world,
            &PossessOpts {
                day: near_ceiling,
                ..PossessOpts::default()
            },
        )
        .unwrap();

        // The increment is valid on its own — so what fails below is the sum.
        let increment = WorldTime::from_ticks(20).as_std_days();
        assert!(
            advanced_by(WorldTime::GENESIS, increment).is_ok(),
            "a 20-tick increment must be representable, or this test proves nothing"
        );

        match session.wait(&increment.to_string(), Perceiving::Body) {
            Turn::Out(msg) => assert!(
                msg.contains("leaves the representable tick range"),
                "expected the ACCUMULATION arm's message, got: {msg}"
            ),
            Turn::Released(_) => panic!("a rejected wait must not release the session"),
        }
    }

    /// The accumulation guard at the unit level: both arms of [`advanced_by`]
    /// and the boundary between them. The two `wait` tests above prove the
    /// ROUTING; this proves the arithmetic — in particular that an overflow is
    /// an `Err`, not a debug panic and not a silent release-build wraparound.
    #[test]
    fn advanced_by_checks_the_increment_and_the_sum_separately() {
        // An ordinary advance is exact on the lattice.
        let day = WorldTime::from_std_days(1.5).expect("finite");
        assert_eq!(
            advanced_by(day, 0.25).expect("representable").ticks(),
            175_000
        );

        // Arm one: the INCREMENT is unrepresentable, named by the crossing.
        let e = advanced_by(WorldTime::GENESIS, f64::MAX).expect_err("f64::MAX is out of range");
        assert!(e.contains("outside the representable tick range"), "{e}");
        assert!(advanced_by(WorldTime::GENESIS, f64::NAN).is_err(), "NaN");

        // Arm two: the increment is fine and the SUM overflows — named
        // distinctly, so the two arms are not confusable by message.
        let near = WorldTime::from_ticks(i64::MAX - 10);
        let e = advanced_by(near, 1.0).expect_err("the sum must overflow i64");
        assert!(e.contains("leaves the representable tick range"), "{e}");
        assert!(
            !e.contains("is not a valid quantity"),
            "the overflow arm must not borrow the increment arm's wording: {e}"
        );

        // The boundary itself: landing exactly on `i64::MAX` still succeeds, so
        // the guard rejects overflow rather than merely being conservative.
        let one_short = WorldTime::from_ticks(i64::MAX - 1);
        assert_eq!(
            advanced_by(one_short, WorldTime::from_ticks(1).as_std_days())
                .expect("exactly reaching i64::MAX is representable")
                .ticks(),
            i64::MAX
        );
    }

    #[test]
    fn an_unprovoked_npcs_grievance_is_not_negative_zero() {
        // Names the invariant `grievance`'s own fold comment explains: a
        // revert to `.sum::<f64>()` (which folds from `-0.0`) would only show
        // up as a large fixture diff without this assertion (The Snapshot
        // chronicle).
        let world = seam_world();
        let (session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        for npc in other_bodies(&session.bodies, session.driven) {
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
        // Incidental choice of a verb — this test pins that a blank line
        // clobbers neither counter, not `!whoami`'s own answer.
        session.handle("!whoami");
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

    /// claim: structural(seed: none — seam_world() fixture) — false-positive
    /// seed-loop flag; `s` binds an NPC social status
    ///
    /// **The Hand, Task 3: manufactures its own co-located NPC.** Before this
    /// task, `derive_npcs`'s home-settlement body was a SEPARATE `Agent`
    /// twin that always started in the possessed body's own room — the very
    /// duplicate Task 2 proved and this task deletes. Deleting it means
    /// nothing is co-located with a fresh flagship possession by default any
    /// more (confirmed live: `!provoke` at seed 42 turn 0 now answers "There
    /// is no one here to provoke or soothe", and even sixty `wait`s never
    /// bring another derived body into the flagship's own room — every other
    /// settlement lives elsewhere, and wild concentrations are scattered
    /// independently). That is a real, structural consequence of the
    /// campaign's premise, reported in the Task 3 report, not a bug in this
    /// test. This test's actual subject is the SOCIAL CHANNEL wiring
    /// (`SessionSnapshot::social` surfaces a provoked NPC's grievance), which
    /// does not care WHY a body is co-located — so it places one itself,
    /// through the same `agent-at` fact the world places one with.
    #[test]
    fn provoking_shows_up_in_the_social_channel() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let room = session.position();
        let target = other_bodies(&session.bodies, session.driven)[0].entity;
        place_agent_now(&mut session, target, &room);
        let before = session.snapshot().unwrap();
        assert!(before.social.iter().all(|s| s.grievance == 0.0));
        session.handle("!provoke");
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
        session.handle("!whoami");
        let snap = session.snapshot().unwrap();
        assert!(
            snap.narration.prose.starts_with("A "),
            "after `!whoami` the narration is the whoami answer, not the room block: {:?}",
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
        let s = path_structure(&session.position(), 4);
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
        let s = path_structure(&session.position(), 4);
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
        let s = path_structure(&session.position(), 2);
        assert_eq!(
            session.named_neighbour(&s, 0, "a-noun-no-chamber-holds"),
            None
        );
    }

    #[test]
    fn the_ways_on_inside_name_out_and_the_deeper_aperture() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let middle = path_structure(&session.position(), 3);
        session.descend(middle, 1).expect("a chamber to stand in");
        let text = session.describe_chamber_here().expect("a chamber renders");
        assert!(
            text.ends_with("Ways on: out, further in."),
            "a middle chamber must offer BOTH directions under distinct names: {text:?}"
        );
        let innermost = path_structure(&session.position(), 3);
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
        let middle = path_structure(&session.position(), 3);
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
        let here = session.position();
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
        let s = path_structure(&session.position(), 3);
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
            .descend(path_structure(&session.position(), 2), 0)
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
        // `!eyes off` falls all the way back to the plain terrain lens.
        session.handle("!eyes off");
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
            .position()
            .neighbors()
            .into_iter()
            .next()
            .expect("a locale has neighbours");
        session.trail.push(elsewhere.clone());
        let here = session.position();
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
                session.position(),
                here,
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
            session.position(),
            elsewhere,
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
            .descend(path_structure(&session.position(), 2), 0)
            .expect("a chamber to stand in");
        let before = session.inside.as_ref().unwrap().cell;
        let row_of = |session: &Session| {
            let plan = session
                .plan_here(&session.eyes)
                .expect("inside, so a plan draws");
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

    /// Every cave-bearing cell in `terrain`, paired with whether its entrance
    /// address (`branch = 0, band = 0, floor = 0`) resolves to a chamber.
    /// Scans the terrain directly (`GeneratedTerrain::cave_at`) rather than
    /// steering a walk there: a terrain cell spans many walk-band rooms
    /// (measured while developing The Deep Realm — dozens to low hundreds of
    /// `go` steps per terrain-cell crossing), so a walk cannot be relied on
    /// to land on a chosen cell. Direct scanning is what
    /// `windows/worldgen/tests/deep_realm_substrate.rs` (Task 0) and
    /// `deep_realm_chamber.rs` (Tasks 2-3) already do for the same reason.
    ///
    /// Shared by [`find_open_cave_cell`] (which stops at the first open hit)
    /// and `delve_has_two_distinguishable_outcomes`'s exhaustive sealed-cave
    /// scan (The Drift, Task 3b), which does not stop early — one derivation
    /// for both, so the two can never quietly disagree about what "sealed"
    /// means.
    fn cave_entrance_states<'a>(
        terrain: &'a hornvale_terrain::GeneratedTerrain,
        seed: Seed,
    ) -> impl Iterator<Item = (hornvale_kernel::CellId, hornvale_terrain::Cave, bool)> + 'a {
        let overrides = hornvale_worldgen::chamber::ChamberOverrides::new();
        terrain.geosphere().cells().filter_map(move |cell| {
            if terrain.is_ocean(cell) {
                return None;
            }
            let cave = terrain.cave_at(cell)?;
            let addr = hornvale_worldgen::chamber::ChamberAddr {
                cell,
                band: hornvale_kernel::Band::Undercroft,
                branch: 0,
                level: 0,
            };
            let is_open = hornvale_worldgen::chamber::chamber_at(
                seed,
                &cave,
                terrain.geothermal_gradient_at(cell),
                &terrain.column_at(cell),
                addr,
                &overrides,
            )
            .is_some();
            Some((cell, cave, is_open))
        })
    }

    /// The first cave-bearing cell this seed's terrain places whose entrance
    /// chamber is realized. Until The Drift (Task 1) deleted
    /// `chamber_exists`'s 50% existence coin, this function also took a
    /// `want_open` flag and could be asked for the SEALED counterpart
    /// instead; that outcome is no longer reachable
    /// (`delve_has_two_distinguishable_outcomes`'s doc comment records why),
    /// so the flag is gone rather than kept as a parameter nothing ever
    /// satisfies.
    fn find_open_cave_cell(
        terrain: &hornvale_terrain::GeneratedTerrain,
        seed: Seed,
    ) -> (hornvale_kernel::CellId, hornvale_terrain::Cave) {
        cave_entrance_states(terrain, seed)
            .find_map(|(cell, cave, is_open)| is_open.then_some((cell, cave)))
            .unwrap_or_else(|| {
                panic!(
                    "no open cave found in seed 42's terrain — the fixture no longer has \
                     one of the two outcomes this campaign's descent verb needs to \
                     distinguish"
                )
            })
    }

    /// The Deep Realm, Task 5 shipped `delve` with THREE distinguishable
    /// outcomes: no cave, a cave whose entrance chamber resolves to nothing
    /// (**sealed** — spec §3.4 rung 0, "the void exists and is unreachable,"
    /// a real fact a later dig could find, not a defect), and a cave with a
    /// resolved chamber. This test carried that name and asserted all three
    /// until The Drift.
    ///
    /// **What removed the third outcome.** The Drift's §4.1 deleted the 50%
    /// existence coin `chamber_exists` gated on — the thing that made a
    /// realized chamber a coin flip rather than a certainty. With the coin
    /// gone, every cave inside the lattice's structural shape realizes a
    /// chamber at every address the five remaining gates admit. Task 1's own
    /// probe measured this directly rather than assuming it:
    /// `systems_with_open_mouth == systems` on all three panel seeds —
    /// 874/874, 1681/1681, 1266/1266. Sealed did not become rare. It became
    /// **impossible**, and this test's failure (it passed at `69d1f5469`) is
    /// what caught that a real behaviour change had happened, not a fixture
    /// going stale on its own.
    ///
    /// **What would restore it.** Restricted passage — locked doors,
    /// collapses that magic can clear, boss encounters, or the rare chamber
    /// that stays lost with something worth finding in it (spec §7's
    /// non-goal, promoted to owed work by AMENDMENT B). That is later
    /// campaign work, filed in `book/src/frontier/idea-registry.md`; this
    /// task does not build it.
    ///
    /// Nathan's ruling (spec AMENDMENT B, B.2): **accept two outcomes**
    /// until that later campaign lands. Deleting this test instead would
    /// have removed a permanent guard; renaming it without more would only
    /// have recorded a fact that never gets checked again. So this
    /// assertion is two-directional, the discipline `seam-guard`'s
    /// STALE-DECL verdict names: a one-directional acknowledgement
    /// ("sealed doesn't happen") can only ever be satisfied, so it rots.
    /// The scan below re-checks every cave-bearing cell in the fixture on
    /// every run and FAILS the moment a sealed cave becomes possible again
    /// while this test still claims two outcomes — forcing whoever ships
    /// restricted passage to come rename this test back, rather than
    /// leaving a stale two-outcome claim sitting here looking satisfied.
    #[test]
    fn delve_has_two_distinguishable_outcomes() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");

        // Outcome 1: no cave at all — asserted through `delve_column(None)`,
        // the branch production reaches when `chamber_column_here` finds
        // nothing. This read the flagship's own STARTING CELL until decision
        // 0131, a convenience resting on the contingency that that one cell
        // happened to be cave-free; the terrain epoch put a cave under it and
        // falsified that. The other outcome is found by scanning rather than
        // assumed, so this brings outcome 1 into line with it and leaves the
        // test independent of where the flagship happens to stand.
        let no_cave = match session.delve_column(None) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("delve must not release"),
        };
        assert!(no_cave.contains("no cave here"), "{no_cave}");
        assert!(
            session.underground.is_none(),
            "a refused delve must not change the underground state"
        );

        // Outcome 2: a chamber — descend, and `climb` returns.
        let (open_cell, open_cave) = find_open_cave_cell(&terrain, world.seed);
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

        // The whole point: the two live outcomes must be told apart.
        assert_ne!(
            no_cave, open,
            "no-cave and a successful descent read identically"
        );

        // The retired third outcome must STAY retired, loudly. Scan every
        // cave-bearing cell's entrance address in the fixture terrain and
        // assert none of them resolves SEALED. Scoped to this one seed
        // rather than a multi-seed panel: the exhaustive scan already
        // touches every cave-bearing cell this fixture has, and building
        // further whole worlds to widen it would push this test toward the
        // heavy tier `the_drift_reachability_baseline`
        // (`windows/worldgen/tests/suite/drift_reach_probe.rs`) already
        // measured `systems_with_open_mouth == systems` on — the workspace
        // gate never runs that tier, which would defeat the point of
        // pinning this guard where it actually runs.
        //
        // Non-vacuous by construction: `caves_examined` must itself be
        // nonzero, or "zero of zero sealed" would satisfy this assertion by
        // finding nothing rather than by finding the world genuinely
        // connected.
        let mut caves_examined = 0usize;
        let mut sealed: Vec<hornvale_kernel::CellId> = Vec::new();
        for (cell, _cave, is_open) in cave_entrance_states(&terrain, world.seed) {
            caves_examined += 1;
            if !is_open {
                sealed.push(cell);
            }
        }
        assert!(
            caves_examined > 0,
            "non-vacuous guard: seed 42's terrain must contain at least one \
             cave-bearing cell, or the sealed-cave scan below would pass by \
             finding nothing rather than by finding the world connected"
        );
        assert!(
            sealed.is_empty(),
            "a SEALED cave exists again ({} of {caves_examined} cave-bearing cells \
             examined, e.g. cell {:?}) — restricted passage has landed. Restore the \
             third `delve_at` outcome this test used to assert, rename it back to \
             `delve_has_three_distinguishable_outcomes`, and update its doc comment; \
             do not leave a two-outcome claim standing once a sealed cave is possible \
             again",
            sealed.len(),
            sealed[0],
        );
    }

    /// **Tripwire for `clients/game/bin/src/driver.rs`'s
    /// `DELVE_SUCCESS_PREFIX` constant.** That client (outside this
    /// workspace, outside this crate's reach to import from) detects a
    /// successful delve by matching this EXACT literal prefix — including
    /// the trailing period — against the turn's own narration text via
    /// `str::starts_with`. It is the ONLY signal available to it:
    /// `Spatial` deliberately folds underground into `Walk`
    /// (`the_underground_band_folds_into_walk_as_map_does`, this module),
    /// so there is no typed alternative to read instead.
    ///
    /// The nearest existing coverage before this test
    /// (`underground_examine_answers_for_the_rock_it_names`, below) only
    /// ever asserts `.contains("You worm down into the dark")` — no
    /// trailing period, substring rather than prefix — so a reword that
    /// inserted a word after "down" or dropped the period would silently
    /// break the client's cave-discovery gate while every test in THIS
    /// crate stayed green. Nothing else pins the literal the client
    /// actually depends on; this does. If this string ever needs to
    /// change, `driver.rs`'s `DELVE_SUCCESS_PREFIX` (and its own H6b
    /// cave-discovery test) must change with it, in the SAME commit.
    #[test]
    fn delve_success_narration_matches_the_clients_own_literal() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (open_cell, open_cave) = find_open_cave_cell(&terrain, world.seed);
        let out = match session.delve_at(open_cell, open_cave) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("delve must not release"),
        };
        assert!(
            out.starts_with("You worm down into the dark."),
            "the delve success prefix drifted -- update \
             `clients/game/bin/src/driver.rs`'s `DELVE_SUCCESS_PREFIX` (and its \
             own H6b cave-discovery test) in the SAME commit: got {out:?}"
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
            .descend(path_structure(&session.position(), 2), 0)
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
    /// `delve_has_two_distinguishable_outcomes` is.
    #[test]
    fn lateral_movement_is_refused_underground() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (cell, cave) = find_open_cave_cell(&terrain, world.seed);
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
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (cell, cave) = find_open_cave_cell(&terrain, world.seed);
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
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (cell, cave) = find_open_cave_cell(&terrain, world.seed);
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

    /// Commit an `agent-at` putting `who` in `room` as of the session's current
    /// day.
    ///
    /// **This helper used to carry a clock-nudging second half, and The
    /// Escapement (decision 0186) deleted the defect it compensated for.**
    /// `Ledger::commit` quantized a fact's day to 8 significant digits and that
    /// rounding could go UP — measured then,
    /// `quantize(1.5117199997382882) == 1.5117200000000000`, strictly LATER
    /// than the day handed in. `latest_committed_position` selects on
    /// `f.day <= t`, so a fact committed at `now` was invisible when read back
    /// at `now`, and the fixture silently described a creature that never
    /// moved. The repair was to advance the session to whatever the ledger
    /// actually stored.
    ///
    /// A `WorldTime` is an exact `i64` tick count now and `commit` canonicalizes
    /// no day at all, so what is stored IS what was handed in: the comparison
    /// `stored > session.day` could never again be true, and the compensation
    /// was deleted rather than left as a permanent no-op with a live-sounding
    /// rationale. This is the paired deletion spec §1 asks the second merger to
    /// make — a workaround outliving its cause, the same shape as The Hand's
    /// `quantize(t.day())`.
    fn place_agent_now(session: &mut Session<'_>, who: EntityId, room: &RoomAddr) {
        let fact = crate::liveness::place_agent(who, room, session.day);
        session
            .ledger
            .commit(fact, &session.registry)
            .expect("agent-at is registered");
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
        // The Hand, Task 3: constructed directly through the test seam
        // (`place_creature_at_me`, see docs/retrospectives/the-hand.md) rather than searched
        // for. Both `bodies()[1]` and `bodies()[2]` are placed through it in
        // turn: the seam's own anchor choice is deterministic (the first LIT
        // chamber anchor), so both land at the SAME room-interior anchor —
        // exactly the collision this test needs, and `colocated_npcs`'s
        // derivation-order iteration (bodies()[1] before [2]) is what makes
        // the FIRST one placed win the cell in `sighting()`'s own scan.
        let world = seam_world();
        let mut session = possessed_inside(&world);
        let room = session.position();
        let first = session.bodies[1].entity;
        session.place_creature_at_me(first);
        assert_eq!(
            marks_of(&session).len(),
            1,
            "precondition: the first placement alone is drawn"
        );

        let second = session.bodies[2].entity;
        session.place_creature_at_me(second);

        assert_eq!(
            session.colocated_npcs().len(),
            2,
            "both creatures are now in the possession's room"
        );
        assert_eq!(
            session.occupancy.anchor_in(first, &room),
            session.occupancy.anchor_in(second, &room),
            "and both stand at the same anchor, which liveness permits — and is \
             what the seam's deterministic choice guarantees here"
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
        let refused = other_bodies(&session.bodies, session.driven)
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
        let answered = session.examine_chamber(&refused, Perceiving::Body);
        assert!(
            !answered.starts_with("You see no"),
            "an unplaced but present creature must be examinable — `present` must \
             imply examinable, or the channel and the verb disagree: {answered}"
        );
        assert!(
            session.needs(Perceiving::Body).contains(&refused),
            "and `needs` must read it too, for the same reason: {}",
            session.needs(Perceiving::Body)
        );
        // ...and it stays ACTABLE-ON. The sight gate on `colocated_npc` (fix
        // round 3) must narrow on sight and on nothing else: an undrawable
        // creature is not an unseen one, so refusing to provoke it would make
        // the placement scan decide what the player may do.
        let acted = match session.handle(&format!("!provoke {refused}")) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("!provoke must not release"),
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
        //
        // The Hand, Task 3: `seam_world()` (a plain seed-42 build) rather than
        // a search, since nothing here needs a naturally co-located creature
        // any more (see docs/retrospectives/the-hand.md) — `who` is placed explicitly, and
        // the near/far anchor derivation below is a pure read of the entered
        // chamber's own geometry, unrelated to who (if anyone) stands there.
        // seed 42's chamber is confirmed (empirically, `place_creature_out_of_
        // my_sight`) to have both a lit and an unlit anchor.
        let world = seam_world();
        let mut session = possessed_inside(&world);
        let room = session.position();
        let who = session.bodies[1].entity;
        place_agent_now(&mut session, who, &room);

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

        let label = other_bodies(&session.bodies, session.driven)
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
            !session
                .examine_chamber(&label, Perceiving::Body)
                .starts_with("You see no"),
            "precondition: and ANSWERS examine while it is depicted"
        );
        assert!(
            session.needs(Perceiving::Body).contains(&label),
            "precondition: and `needs` reads it: {}",
            session.needs(Perceiving::Body)
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
        let refused = session.examine_chamber(&label, Perceiving::Body);
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
        let read = session.needs(Perceiving::Body);
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
            let acted = match session.handle(&format!("!provoke {arg}")) {
                Turn::Out(t) => t,
                Turn::Released(_) => panic!("!provoke must not release"),
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
        let arriving: Vec<RoomAddr> = other_bodies(&session.bodies, session.driven)
            .iter()
            .map(|npc| {
                if npc.entity == who {
                    elsewhere.clone()
                } else {
                    agent_position(&session.ledger, npc, session.day)
                }
            })
            .collect();
        let narrated = session.narrate_motion(1, &arriving, &nowhere, Perceiving::Body);
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
        let seen_arriving = session.narrate_motion(1, &arriving, &nowhere, Perceiving::Body);
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
        let was_here: Vec<RoomAddr> = other_bodies(&session.bodies, session.driven)
            .iter()
            .map(|npc| agent_position(&session.ledger, npc, session.day))
            .collect();
        place_agent_now(&mut session, who, &elsewhere);
        assert!(
            !session.colocated_npcs().iter().any(|n| n.entity == who),
            "precondition: the creature really left the room"
        );
        let leaving = session.narrate_motion(1, &was_here, &nowhere, Perceiving::Body);
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
        let announced = session.narrate_motion(1, &was_here, &seen, Perceiving::Body);
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
        // The Hand, Task 3: constructed directly through the test seam
        // (`place_creature_at_me`, see docs/retrospectives/the-hand.md) rather than searched
        // for. `bodies()[1]` is placed once at a LIT cell (the seam's own
        // choice), and stays in sight under the perturbed placement too —
        // confirmed by the assertion below, which is this test's OWN positive
        // control now that nothing is naturally drawn to search a world for.
        let world = seam_world();
        let mut session = possessed_inside(&world);
        session.place_creature_at_me(session.bodies[1].entity);
        assert!(
            !marks_of(&session).is_empty(),
            "precondition: the placed companion is drawn from the embedding"
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
            "nor may it move who is REPORTED here — the placed companion must \
             stay in sight under both placements"
        );
    }
}
