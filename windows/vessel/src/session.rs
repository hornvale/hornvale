//! The possession session: a pure step function over a frozen world. Every
//! verb is read-only; possessing a world never changes it.

use crate::action::{Action, Mood};
use crate::agent::check_species_known;
use crate::body::Body;
use crate::clock::{climb_factor, cost_of, mass_for_species};
use crate::controller::{Controller, ImposedController, PlayerController};
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
use crate::testimony::{FeltStateWord, Testimony, testify_with_stance};
use crate::{
    Focalized, Focalizer, IdentityProjection, Knowledge, PossessOpts, PossessTarget, Projection,
    TemplateFocalizer, Turn, VesselError, absorb_common, most_populous_settlement, observable,
    reader_set,
};
use hornvale_kernel::{
    ConceptRegistry, EntityId, Facet, FacetId, Fact, Ledger, Seed, TickSpan, Value, World,
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
/// [`HELP`] is a second copy that closes the loop — it is the surface a
/// player reads, so it is already obliged to be complete — and
/// `every_bare_verb_help_lists_is_classified` asserts the two agree in **both**
/// directions: every verb `HELP` lists is in this roster or in
/// [`SESSION_CONTROL`], and every entry of this roster is listed by `HELP`.
///
/// **Not "the third copy" — the final whole-branch review's own count (M-f)
/// found the verb-roster count hardcoded in EIGHT places across three
/// crates** (this roster, `HELP`, and — outside this file entirely —
/// `action_mood.rs`'s partition assertion, `windows/lab/tests/suite/
/// reticence_calibration.rs`'s prose, and others each of this campaign's
/// task reports found in turn). This doc used to claim three; it was wrong
/// the day a fourth copy existed and nobody had reason to re-check it.
/// Nothing here mechanizes the count — the two runtime rosters above are
/// the only pair `every_bare_verb_help_lists_is_classified` actually holds
/// together.
///
/// **THE DRIFT ABOVE IS NOT HYPOTHETICAL — IT HAPPENED, ONE CAMPAIGN AFTER
/// THIS DOC WARNED ABOUT IT.** The Latch added `clear` to
/// [`Session::handle`]'s match and to neither roster, so
/// `gated_by_the_body("clear")` answered `false` and a SLEEPING body could
/// clear a barred passage — and `clear` is the only verb in the free band
/// that WRITES TO THE LEDGER, so the ungated one was the consequential one.
/// The paired test above could not see it: it holds `HELP` and this roster
/// together, and `clear` was in neither, so both directions were satisfied
/// by its absence. That is the shape of this guard's blind zone — it catches
/// a verb listed in one place and missing from the other, never a verb
/// missing from both — and it is why `clear_is_refused_while_asleep` exists
/// beside `warm_is_refused_while_asleep` rather than in place of it.
///
/// **`carrying` is in this roster and not in [`SESSION_CONTROL`], and the
/// choice was made from the code rather than from taste (The Chattel, Task
/// 12).** It reads state and writes none, which makes it look like an
/// operator instrument; `knows` and `needs` are the standing counterexample —
/// both read-only, both write nothing, both in-character — and what they read
/// is *the body's own* state. `carrying` reads the body's custody, which is
/// as body-relative as a reading gets: an operator instrument bypasses the
/// body (spec §2.2), and a question whose entire subject IS the body has
/// nothing left to answer once it does.
///
/// **The mechanical half is that the disjointness check would NOT have caught
/// the other choice.** `session_control_is_never_an_in_character_verb` sweeps
/// [`SESSION_CONTROL`] and asserts no entry is in this roster; a `carrying`
/// placed in `SESSION_CONTROL` alone and listed in `HELP` satisfies it
/// (it is in only one roster), satisfies
/// `every_bare_verb_help_lists_is_classified`'s first loop (it IS classified),
/// and is never reached by its second (which iterates this roster). It would
/// have shipped ungated — The Latch's `clear` failure arriving through the
/// other door — so `carrying_is_refused_while_asleep` is the witness, exactly
/// as it is for `open`/`close`/`take`/`drop`/`put`.
const IN_CHARACTER_VERBS: [&str; 28] = [
    "ask", "back", "carrying", "clear", "climb", "close", "consult", "delve", "dive", "down",
    "drop", "enter", "examine", "go", "knows", "look", "map", "needs", "open", "out", "put",
    "sleep", "surface", "take", "up", "wait", "warm", "write",
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

/// What `back` says underground (The Gallery, Task 4). `go` is reversed —
/// `UNDERGROUND_LATERAL_REFUSAL` claimed "there is nowhere down here for a
/// bearing to mean", which Task 3's real generated level made false, and
/// Task 4 deletes the constant along with that claim. `back` is NOT
/// reversed, for the same reason it stays refused indoors after The
/// Blocking's own reversal of `go`: this campaign built intra-level
/// GEOMETRY, not a retraceable trail, so there is nothing for `back` to
/// retrace in either band.
const UNDERGROUND_BACK_REFUSAL: &str =
    "There is no trail to retrace down here; walk it again, cell by cell.";

/// What `map <anything>` says underground (The Gallery, Task 8). A level has
/// no coarser rung to zoom to the way the walk band's chart does — `map out
/// N` is a question about the country overhead, and there is no overhead to
/// draw from inside the rock — so any argument is refused the same way an
/// indoor `map <arg>` is refused by [`INDOOR_CHART_REFUSAL`]: for the
/// geometry, not as a parse complaint.
const UNDERGROUND_CHART_REFUSAL: &str =
    "Down here the chart is the rock around you; there is no coarser rung to draw.";

/// `climb`'s refusal from any rung but the entrance (The Gallery, Task 3):
/// there is no one-step way out of a multi-rung descent, only the stairs
/// up (Task 5).
const CLIMB_FROM_DEPTH_REFUSAL: &str =
    "You are too far down to climb straight out; find the stairs up.";

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

/// The entity currently holding this body (The Coercion). **NOT functional**,
/// unlike [`TURNED_HOSTILE`]: a body may be possessed, released, and possessed
/// again over its life, so the live state is [`possessor_of`]'s open/close fold
/// and never a single latest value. Committed only for an IMPOSED possession —
/// the player's own possession is the session's premise, not a world fact, so
/// an open `possessed-by` always means someone other than the player holds this
/// body (spec §3.1).
/// type-audit: bare-ok(identifier-text)
pub const POSSESSED_BY: &str = "possessed-by";

/// Closes the possession opened by the most recent [`POSSESSED_BY`] (The
/// Coercion). The object is the reason: `"released"` today, and `"died"` once
/// mortality exists — see the spec §6, which asserts the second is unreachable.
/// type-audit: bare-ok(identifier-text)
pub const POSSESSION_ENDED: &str = "possession-ended";

/// Who currently holds `body`, if anyone (The Coercion).
///
/// A single pass in LEDGER ORDER over the body's own facts: a
/// [`POSSESSED_BY`] opens, a [`POSSESSION_ENDED`] closes, and the last word
/// wins. Ledger order is the ordering — never a sort on `day`, which is
/// `Option<WorldTime>` and absent on facts that carry no instant.
pub(crate) fn possessor_of(ledger: &Ledger, body: EntityId) -> Option<EntityId> {
    let mut held = None;
    for fact in ledger.facts_about(body) {
        match fact.predicate.as_str() {
            POSSESSED_BY => {
                if let Value::Entity(who) = fact.object {
                    held = Some(who);
                }
            }
            POSSESSION_ENDED => held = None,
            _ => {}
        }
    }
    held
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
  delve            descend into the cave here, if the rock admits
                   one; 'climb' comes back
  climb            return to the surface from underground
  down             descend a rung by the stairs underfoot, if there is one;
                   'up' comes back
  up               ascend a rung by the stairs underfoot, if there is one
  clear            clear the way down at a barred cave mouth; only a thin
                   fall of rubble gives, and once cleared it stays cleared
  enter [way]      step inside what is built here; once inside, 'enter further
                   in' goes deeper and 'out' leaves
  out              step back out of doors
  examine <thing>  anything look or the floor plan names
  open <thing>     open what has a lid, indoors; a locked one wants a key
                   you are already carrying
  close <thing>    shut it again; what was inside goes out of sight
  take <thing>     pick it up, if it is a thing a body can carry; it comes
                   with you until you set it down
  drop <thing>     set down here something you are carrying
  put <thing> in <where>
                   stow something you are carrying inside something that
                   holds things and is not shut
  carrying         what you have in hand
  warm             warm yourself at a hearth, where one burns
  back             retrace your last step, out of doors
  wait [N]         let N days pass overhead (default 1); the world moves too
  sleep            lie down and sleep; the body stops obeying until its own
                   cycle wakes it, and only '!' verbs answer meanwhile
  knows            everything they have seen
  needs            read the felt state of anyone sharing this room
  ask              ask the body you are wearing how it feels; it may answer
                   truthfully, or in words its own tongue and mind allow
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
  !possess         another will takes this body; your own acts refuse until
                   it lets go
  !unpossess       the possessor's own option: let go of this body again
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
    trail: Vec<Facet>,
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
    /// A clone of the world's registry, extended with the live-play
    /// predicates — `AGENT_AT`, `LOCATED_IN`, `OPENNESS` and `LOCKEDNESS` (The Chattel,
    /// which also RETIRED `PASSAGE_CLEARED` from this list), and the
    /// drive/needs predicates beside
    /// them — every one registered per-session, never at genesis (spec §3).
    /// The roster is `Session::start`'s own `register_predicate` block, which
    /// is where a reader should look rather than trusting this list to stay
    /// exhaustive; it has already gone stale three predicates in a row.
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
    predator: Option<hornvale_kernel::VertexMap<f64>>,
    /// The world's prey-pressure field (The Teeth), computed once at `start`, so
    /// a carnivore's hunger senses prey territory; `None` if the demography fit
    /// fails.
    prey: Option<hornvale_kernel::VertexMap<f64>>,
    /// The world's settlement-territory set (The Threshold, task 5b —
    /// `built_rooms`), computed once at `start`, so a room a settlement
    /// actually occupies reads as built and can draw a real hearth.
    /// `Session::start` requires `mint_flagship` to resolve a settlement
    /// first, so in practice this always carries at least the possessed
    /// agent's own home room by the time a session exists.
    built: std::collections::BTreeSet<FacetId>,
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
    /// The possession's position within a generated underworld descent, if
    /// any (The Deep Realm, Task 5; made a real place by The Gallery, Task
    /// 3). `None` is the surface. Mirrors `submerged`: the whole resolved
    /// value is carried rather than just an address, so `climb` and a later
    /// `look` never need to re-derive it.
    ///
    /// **Before this task this was a single entrance
    /// `hornvale_worldgen::chamber::Chamber` — a bucket, not a place.** It
    /// is now a [`crate::underground::Underground`]: a whole descent
    /// (`Vec<Level>`, one per habitation rung) plus which rung and which
    /// cell of it the possession stands on. `delve_at` builds it once,
    /// through [`crate::underground::Underground::enter`]; nothing here
    /// re-derives it.
    underground: Option<crate::underground::Underground>,
    /// The session-lived geometry memo (the-waymark fix round, Finding 2):
    /// `RoomMeshMemo` is fixed for this session's whole lifetime (`neighbors`
    /// is world-independent; `corner_weights` is fixed once `ctx`'s
    /// `(Geosphere, NearestVertexIndex)` pair is built at `start` and never
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
    /// The driven body's own felt state as of the most recent `!wait` (The
    /// Confidant, Task 2) — `None` before the first one. Set alongside
    /// `driven_mode`, by the SAME [`Self::wait`] call into
    /// [`DriveMovements::step_one_with_controller`], from the SAME
    /// resolution — never a second, drift-prone derivation. Read back by
    /// [`Self::driven_affect`].
    driven_affect: Option<Affect>,
    /// The driven body's own arbitration's discarded ranks as of the most
    /// recent `!wait` (The Confidant, Task 5) — the OTHER drives that were
    /// active but not pursued, empty before the first one. Set alongside
    /// `driven_affect`, by the SAME [`Self::wait`] call into
    /// [`DriveMovements::step_one_with_controller`], from the SAME
    /// resolution — never a second, drift-prone derivation. This is the
    /// residue [`Self::driven_affect`] itself never carries: read back by
    /// [`Self::suppressed_drives`].
    driven_suppressed: Vec<DriveKind>,
    /// Every drive this body's own arbitration wanted and did not pursue,
    /// counted across the WHOLE possession (The Reticence, Task 2) — unlike
    /// `driven_suppressed`, which is a per-decision read overwritten by every
    /// `advance_one` iteration. When the rider is driving, this is the record
    /// of what the rider made this body ignore, and it is the only conduct
    /// input the host's willingness to speak reads.
    driven_overrides: std::collections::BTreeMap<DriveKind, u32>,
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
        // PASSAGE_CLEARED USED TO BE REGISTERED HERE, and its registration is
        // gone rather than kept as a compatibility stub (The Chattel, Task 8;
        // decision 0396 supersedes 0367). A cave mouth is a thing now, and the
        // predicate it writes is OPENNESS, registered a few lines below beside
        // LOCATED_IN — so this block would have registered a predicate nothing
        // in the tree can write and nothing reads. "Per-session" always named
        // WHERE a predicate is registered, not how long it lasts, and that has
        // not changed: `Session::into_played_world` moves `self.registry` AND
        // `self.ledger` into the saved `World`, and `possess --out` writes it
        // (decision 0368; decision 0171 already ruled a player's acts are not
        // filtered on the way out). A saved world written BEFORE this flip
        // still carries `passage-cleared` in its own registry and its own
        // facts; nothing reads them, so a passage it recorded as cleared is
        // barred again on reload. That break was measured, not estimated — no
        // committed fixture carries the predicate — and decision 0189 is the
        // precedent for taking one deliberately.
        //
        // The Chattel's two live-play predicates, registered on exactly the
        // same terms as AGENT_AT above: a predicate is registered by whoever
        // builds the registry that will hold its facts. Nothing registers
        // these at genesis, so `world-seed-42.json` does not move for them;
        // an out-of-session reader registers them into its own registry the
        // way `windows/lab` already does for AGENT_AT (`health.rs`,
        // `synthetic.rs`), which is a no-op for an identical definition
        // (`ConceptRegistry::register_predicate` is documented idempotent).
        //
        // Both are NON-FUNCTIONAL: a thing moves more than once, and a chest —
        // or, since Task 8, a cave mouth —
        // opens, closes and opens again. Each change is one dated fact and
        // the read is the as-of-day fold in `thing.rs`, never a latest-value
        // read.
        registry
            .register_predicate(
                crate::thing::LOCATED_IN,
                false,
                crate::thing::LOCATED_IN_DOC,
            )
            .expect("LOCATED_IN registers identically every session");
        registry
            .register_predicate(crate::thing::OPENNESS, false, crate::thing::OPENNESS_DOC)
            .expect("OPENNESS registers identically every session");
        // The third, on the same terms (decision 0399). LOCKEDNESS is a
        // separate state from OPENNESS, not a second spelling of it: `close`
        // shuts a lid and never turns a key, so a chest a player shut on their
        // own key can still be opened again.
        registry
            .register_predicate(
                crate::thing::LOCKEDNESS,
                false,
                crate::thing::LOCKEDNESS_DOC,
            )
            .expect("LOCKEDNESS registers identically every session");
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
        // The Coercion: an imposed possession is an open/close fact pair, not
        // a single latest value (a body may be possessed, released, and
        // possessed again) — both non-functional, matching DISPOSITION_SHIFT's
        // shape rather than TURNED_HOSTILE's.
        registry
            .register_predicate(POSSESSED_BY, false, "the entity holding this body")
            .expect("POSSESSED_BY registers identically every session");
        registry
            .register_predicate(
                POSSESSION_ENDED,
                false,
                "a possession ended, with its reason",
            )
            .expect("POSSESSION_ENDED registers identically every session");
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
            driven_affect: None,
            driven_suppressed: Vec::new(),
            driven_overrides: std::collections::BTreeMap::new(),
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
    pub fn position(&self) -> Facet {
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

    /// The driven body's own felt state, as of the most recent `!wait` tick
    /// (The Confidant, Task 2) — the same co-present computation
    /// [`Self::driven_mode`] documents, read back via its
    /// [`AffectLabel`] rather than the full [`Affect`]: later tasks turn the
    /// label into speech, and the richer `arousal`/`valence`/`object` fields
    /// stay internal to arbitration until a caller actually needs them.
    /// `None` before the first `!wait`.
    pub fn driven_affect(&self) -> Option<AffectLabel> {
        self.driven_affect.map(|affect| affect.label)
    }

    /// Which drive the driven body's most recent felt state is ABOUT (The
    /// Reticence, Task 5) — the affect's own object, so a topic-scoped refusal
    /// names the axis the rider actually overrode. `None` before the first
    /// `!wait`, and for a state with no object.
    pub fn driven_affect_object(&self) -> Option<DriveKind> {
        self.driven_affect.and_then(|affect| affect.object)
    }

    /// The driven body's own arbitration's discarded ranks, as of the most
    /// recent `!wait` tick (The Confidant, Task 5) — the SAME resolution
    /// [`Self::driven_affect`] reads, its OTHER active drives rather than
    /// its winner. This is the residue the creature cannot introspect: a
    /// host-facing utterance must draw from [`Self::driven_affect`] alone,
    /// and nothing routes this accessor's contents into one, by
    /// construction. Empty before the first `!wait`, and also whenever no
    /// other drive was active alongside the pursued one.
    pub fn suppressed_drives(&self) -> &[DriveKind] {
        &self.driven_suppressed
    }

    /// How many decisions this possession has overridden `drive` — the count
    /// of ticks on which arbitration found it active and did not pursue it
    /// (The Reticence, Task 2). Zero before the first `!wait`, and zero for a
    /// drive that has never lost.
    /// type-audit: bare-ok(count)
    pub fn overrides_of(&self, drive: DriveKind) -> u32 {
        self.driven_overrides.get(&drive).copied().unwrap_or(0)
    }

    /// The whole override record, drive-ordered (The Reticence, Task 2).
    /// type-audit: bare-ok(count)
    pub fn override_record(&self) -> &std::collections::BTreeMap<DriveKind, u32> {
        &self.driven_overrides
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

        // The band the possession is in decides the channel. `inside` and
        // `underground` each get their own arm; `submerged` is the one
        // remaining state that still falls through to the surface chart,
        // exactly as the `map` VERB's own arms do (they guard on `inside`
        // alone, so a submerged `map` draws the surface chart too — pane and
        // verb must keep agreeing there, or the pane would start showing
        // something the verb refuses to). Adding a band to the session
        // without deciding what the pane shows there is the failure this
        // comment exists to catch: see `SpatialChannel`'s own doc, which
        // also records why `underground` is no longer one of the ones that
        // folds (The Gallery, Task 7).
        let spatial = if let Some(inside) = self.inside.as_ref() {
            SpatialChannel::Chamber {
                plan: self.chamber_plan(inside, marks, &self.eyes)?,
            }
        } else if let Some(ug) = self.underground.as_ref() {
            SpatialChannel::Underground {
                level: Box::new(self.underground_level(ug, marks)),
            }
        } else {
            // `purview(0)` is the same call `map` makes out of doors, at the
            // same zoom, so the pane shows what the verb would have shown.
            SpatialChannel::Walk {
                chart: Box::new(self.purview(0)?),
            }
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
                    // `FacetError` implements `Debug` but not `Display`, so
                    // `{e:?}` is the only rendering available here — the same
                    // choice `windows/locale`'s `LocaleError::Unaddressable`
                    // makes for the identical error type.
                    .map_err(|e| VesselError::Build(format!("{e:?}")))?
                    .0,
                // The SAME fold `carrying`, `drop` and `put` resolve against
                // (`Self::carried`), not a second read of the ledger: a pane
                // that disagreed with the verb about what is in hand would be
                // a worse defect than an absent field, and there is exactly
                // one function here to disagree with.
                carrying: self
                    .carried()
                    .into_iter()
                    .map(|(entity, noun)| crate::snapshot::CarriedEntry {
                        entity: entity.0.get(),
                        noun: noun.to_string(),
                    })
                    .collect(),
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

    /// Who currently holds the driven body, if anyone (The Coercion) — the
    /// session-level read over [`possessor_of`]'s fold. `None` for a free
    /// body, which is every body until an imposition seam opens one.
    pub fn possessor(&self) -> Option<EntityId> {
        possessor_of(&self.ledger, self.agent_entity())
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

    /// The body's state, as the gate reads it — DERIVED from
    /// [`possessor_of`] and [`Self::wake_at`] rather than stored, so the
    /// clock advancing past the next waking IS the waking and there is no
    /// second field to fall out of step.
    ///
    /// **Possession is checked FIRST, and the order is a real decision**
    /// (The Coercion, Task 3): a body held by another is refused
    /// in-character whether or not it also happens to be asleep, and
    /// reporting "you are asleep" to a player whose body has been taken
    /// names the wrong condition.
    fn body_state(&self) -> BodyState {
        if possessor_of(&self.ledger, self.agent_entity()).is_some() {
            return BodyState::PossessedByAnother;
        }
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

    /// The planet's rotation period as an exact tick span — `None` on a
    /// tidally locked world, which the rotation pin admits.
    ///
    /// Reads the stored tick count rather than the continuous view (The
    /// Foliot): a day is a whole number of ticks now, so there is no reason to
    /// route the scheduler's own question through `f64` days. Extracted from
    /// `wait`'s own inline read so the player's charge and the NPC layer's
    /// cannot disagree about the rate.
    fn day_ticks(&self) -> Option<TickSpan> {
        self.calendar.as_ref().and_then(|c| c.day_ticks())
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
        // Integer addition, end to end (The Foliot). This used to convert the
        // cost to `f64` days and re-enter the lattice through
        // `WorldTime::from_std_days`; with a lattice-aligned day the cost IS a
        // kernel span, so there is no crossing left to make.
        let span = cost_of(action, self.body_mass_kg, terrain_factor);
        match self
            .day
            .ticks()
            .checked_add(span.ticks())
            .map(WorldTime::from_ticks)
        {
            Some(d) => {
                self.day = d;
                Ok(())
            }
            None => Err(format!(
                "error: advancing day {} by {} ticks leaves the representable \
                 tick range",
                self.day.ticks(),
                span.ticks()
            )),
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
    fn climb_to(&self, dest: &Facet) -> f64 {
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
    fn commit_agent_at(&mut self, position: &Facet, provenance: &str) {
        // `self.day` is already the instant the constructor wants (The
        // Precedence): the `f64` round-trip that stood here is gone, not
        // converted more carefully.
        let fact = agent_at_fact(self.agent_entity(), position, self.day, provenance);
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
        let fact = rested_fact(self.agent_entity(), self.day, SLEPT_PROVENANCE);
        self.ledger
            .commit(fact, &self.registry)
            .expect("RESTED is registered every session and non-functional");
        let wake = {
            let activity = species_activity(self.world, &self.driven_body().species);
            let terrain = self.terrain_here();
            next_awake_day(activity, &terrain, &self.position(), self.day)
        };
        // `next_awake_day` answers with the instant itself now, so the wake
        // time needs no reconstruction from a float day — and cannot fail.
        self.wake_at = Some(wake);
        Turn::Out(SLEEP_REPLY.to_string())
    }

    /// Warm oneself at a heat source (The Offer, Task 5, spec §3.3/§6): the
    /// one wholly new verb this campaign adds, and the session-side half of
    /// acceptance test (2)'s live witness — `warm_appears_on_hearth_with_no_
    /// object_table_edit` (`tests/suite/affordance.rs`, Task 2) proves the
    /// QUERY offers `warm` on `Hearth` and not on `Bed`; this method proves
    /// the VERB actually dispatches, gated the same way every other
    /// in-character act is.
    ///
    /// **Reads only DERIVED interior state, never a committed fact.**
    /// Whether a heat source is here is [`Self::chamber_interior_here`]'s own
    /// anchor catalogue, checked through [`crate::affordance::
    /// offered_to_observer`] exactly the way [`Self::examine_chamber`]
    /// already gates its own anchor detail — a per-room DERIVATION, not a
    /// ledger read. `Interior` is never serialized (decision 0069), so this
    /// precondition reads position and derived state only, the same property
    /// every other `Action`'s precondition holds
    /// (`action::precondition_reads_committed_state`). Standing at a hearth
    /// out of doors is impossible in the first place — no
    /// [`crate::interior::AnchorKind::Hearth`] exists outside a chamber's
    /// `Interior` — so `self.chamber_interior_here()` returning `None` out
    /// of doors doubles as that refusal.
    ///
    /// **Fixed post-review (I1): gates on the OFFER, not on a hardcoded
    /// `AnchorKind::Hearth` literal.** This method used to compare
    /// `interior.anchor(a).kind == AnchorKind::Hearth` directly — the exact
    /// per-kind coupling spec §3.2 says a new verb must never need
    /// ("neither edits a dispatcher"), reintroduced by the one verb this
    /// campaign actually adds. A future `RadiatesHeat` carrier (a cauldron
    /// of coals on `AnchorKind::Vessel`, say) would have needed an edit
    /// HERE as well as an `object_registry` entry — exactly the M×N
    /// dispatcher-edit this campaign exists to abolish. Now this reads
    /// [`crate::affordance::OfferedVerb::Warm`] off whichever anchor is
    /// actually here, so a future carrier needs only the registry entry.
    /// **Behaviour is unchanged today**: `Hearth` is still the only
    /// `RadiatesHeat` carrier, so `offered_to_observer` agrees with the old
    /// literal check on every anchor kind that exists —
    /// `warm_succeeds_at_a_real_hearth_through_a_real_session` (below) is
    /// the proof, unmoved by this fix. Routed through `offered_to_observer`
    /// rather than the narrower `offered_by`, for the same reason
    /// [`Self::examine_chamber`] is: one knowledge gate for every surface
    /// that reads an offer (Nathan's #9 ruling), not two.
    ///
    /// **The [`crate::affordance::thing_kind_of`] conversion is at this call
    /// site since Task 9, not inside the query** (spec §3.6, decision 0397).
    /// It is here because the anchor is here: this method holds an
    /// [`crate::interior::AnchorKind`] and the query speaks thing-kind, so
    /// the conversion belongs where the anchor is — the same rule
    /// `crate::affordance::encloses` already follows. Moving it out of
    /// `offered_to_observer` is what lets a caller who holds NO anchor kind
    /// (a cave mouth, addressed by a `Vertex`/`ChamberAddr`) reach the gate
    /// at all; nothing about warming changes.
    ///
    /// **Commits nothing, and mints no `Action` variant.** Warming is not a
    /// GOAP-planned creature act — no successor in `action.rs`'s search
    /// spaces ever proposes it — and `dive`/`surface`/`delve`/`climb` beside
    /// it in [`Self::handle`]'s match are the standing precedent that an
    /// in-character verb with no authored cost dial stays free rather than
    /// inventing one (their own doc: minting a tariff here would be "a cost
    /// model," which this arc does not add). A fifth `Action` variant would
    /// also break the three-way partition
    /// `tests/suite/action_mood.rs::every_rostered_action_is_classified`
    /// asserts over `Action::all()` (creature actions + group A + group B),
    /// since warm fits none of those three categories — reason enough on its
    /// own to leave the action layer untouched here. Its effect is narration
    /// alone: nothing durable changes, on the ledger or on this session,
    /// which is the IV.a/IV.b line this task was told to stop at.
    fn warm(&self) -> Turn {
        let can_warm = self.chamber_interior_here().is_some_and(|interior| {
            interior.ids().iter().any(|&a| {
                crate::affordance::offered_to_observer(
                    crate::affordance::thing_kind_of(interior.anchor(a).kind),
                    self.driven_body(),
                    &self.knowledge,
                )
                .contains(&crate::affordance::OfferedVerb::Warm)
            })
        });
        if !can_warm {
            return Turn::Out("There is no fire here to warm yourself at.".to_string());
        }
        Turn::Out("You warm yourself at the fire.".to_string())
    }

    /// The [`Facet`] of the chamber the possession is standing in — the ROOM
    /// key a thing promoted here is lineage-keyed on, and `None` out of doors.
    ///
    /// **Not [`Self::position`], and the difference is an entity-id
    /// collision.** `position()` is the outdoor LOCALE facet and does not move
    /// while descending into a structure — every chamber of one structure
    /// shares it, which is exactly why `offered_to_observer`'s knowledge gate
    /// can never fail indoors. Keying a thing on it would give the strongbox
    /// of a structure's third chamber and the strongbox of its fourth the
    /// SAME `EntityId`: `role_for` returns `Role::Store` for chamber index 2
    /// and for every index past it, so a four-chamber dwelling composes two
    /// strongboxes and two keys, in two different rooms, that
    /// `thing::thing_id` could not tell apart. `Structure::chambers` is a
    /// `Vec<Facet>` — a chamber IS a facet, drawn from the locale's own seed
    /// under `room/chambers/v1` and asserted distinct by
    /// `structure::tests::chambers_are_distinct` — so the right key was
    /// already in hand and needed only to be reached for.
    ///
    /// The per-room census that licenses `ordinal: 0`
    /// (`interior::pattern::tests::no_production_room_composes_two_anchors_of_
    /// one_kind`) is a statement about ONE composed interior, so it says
    /// nothing about this hazard; the two guards are complementary and neither
    /// substitutes for the other.
    fn chamber_facet_here(&self) -> Option<Facet> {
        let inside = self.inside.as_ref()?;
        Some(inside.structure.chambers[inside.at].clone())
    }

    /// Whether the container of `kind` in `room` stands open as of now.
    ///
    /// **This is where a container's absent-fact default is authored, and it
    /// is authored HERE rather than in [`crate::thing::is_open`] on purpose.**
    /// `is_open` returns `Option<bool>` because an absent [`crate::thing::
    /// OPENNESS`] fact means "whatever the seed drew" (spec §3.3), never
    /// "shut" — collapsing the `None` inside that fold would state that every
    /// untouched door in the world is closed, and its own doc forbids exactly
    /// that. A cave mouth's draw is `hornvale_worldgen::barrier_of`, which
    /// `passage::effective_state` falls back to. **No generator draws an
    /// openness for a container**, so the container's fallback is an authored
    /// constant, and it is `false`: a strongbox is a banded chest whose own
    /// `detail` line already says "its lid seated flush".
    ///
    /// The two fallbacks therefore live in the two readers, in the shape §3.7
    /// pins — `effective_state` for the passage angle, this for the container
    /// angle — over one shared fold.
    fn container_is_open(&self, room: &Facet, kind: hornvale_kernel::KindId) -> bool {
        crate::thing::thing_id(room, kind.0, 0)
            .ok()
            .and_then(|id| crate::thing::is_open(&self.ledger, id, self.day))
            .unwrap_or(false)
    }

    /// Whether the thing of `kind` in `room` is locked as of now (decision
    /// 0399) — [`Self::container_is_open`]'s sibling over
    /// [`crate::thing::LOCKEDNESS`], and the authored home of THAT fold's
    /// absent-fact default.
    ///
    /// **The default is keyed on the KIND, and that is the difference from
    /// the openness reader beside it.** A kind carrying
    /// [`crate::affordance::ObjectProperty::Lockable`] with no fact about it
    /// is locked — that is what makes a seeded strongbox worth finding a key
    /// for, and it is the state every world starts in, since no generator
    /// draws a lockedness any more than it draws an openness. A kind with no
    /// lock is never locked, at any ordinal, under any fact: an alcove has
    /// nothing to turn.
    ///
    /// **Nothing in this campaign ever writes `true`.** The only writer is
    /// [`Self::open_or_close`]'s unlock, so the fold has exactly two reachable
    /// states — the authored default, and unlocked-for-good. Re-locking would
    /// need the key put IN THE LOCK, which is a location no verb can reach
    /// (decision 0399, clause 4).
    fn container_is_locked(&self, room: &Facet, kind: hornvale_kernel::KindId) -> bool {
        if !crate::affordance::carries(kind, crate::affordance::ObjectProperty::Lockable) {
            return false;
        }
        crate::thing::thing_id(room, kind.0, 0)
            .ok()
            .and_then(|id| crate::thing::is_locked(&self.ledger, id, self.day))
            .unwrap_or(true)
    }

    /// Whether the driven body is carrying anything that carries `property`.
    ///
    /// **The first precondition in Hornvale that reads a SECOND object** (spec
    /// §3.8). Custody is a [`crate::thing::LOCATED_IN`] fact whose object is
    /// the body itself, so this is `held_by` over the body's entity, each held
    /// thing's kind read back off its own `instance-of`, and the property
    /// table asked about that label.
    ///
    /// **M+N in the KINDS, not in the property — and this doc used to claim
    /// both.** It read: *"The lock declares what it requires (a property) and
    /// the key declares what it carries."* Only the second clause holds.
    /// Nothing here mentions `strongbox` or `key`, and a second lockable kind
    /// or a second portable kind arrives with no edit to this function or to
    /// the dispatcher — that much is real, and it is what M+N means here. But
    /// [`crate::affordance::ObjectProperty::Lockable`] carries no payload, so
    /// the lock declares nothing about its opener; the wanted property is the
    /// literal `ObjectProperty::Portable` at [`Self::open_or_close`]'s one
    /// call to this function. **A second portable kind therefore arrives with
    /// no edit AND opens every lock in the world** — see
    /// [`LOCKED_WITHOUT_A_KEY_REFUSAL`]'s doc for the hazard and the test that
    /// reddens on it.
    fn carrying_something_that(&self, property: crate::affordance::ObjectProperty) -> bool {
        crate::thing::held_by(&self.ledger, self.agent_entity(), self.day)
            .into_iter()
            .filter_map(|thing| self.ledger.kind_of(thing))
            .any(|label| crate::affordance::label_carries(label, property))
    }

    /// `open <thing>` and `close <thing>` — The Chattel's headline verbs
    /// (Task 11, spec §3.7/§3.8), and the first pair in this file to commit a
    /// fact about a thing that is not the body.
    ///
    /// # One handler, both directions
    ///
    /// The two verbs differ in one boolean: which way the [`crate::thing::
    /// OPENNESS`] fact points, and which sentences say so. Splitting them
    /// would be two functions whose lock check, band guard, noun resolution
    /// and promotion could each drift, and the drift that matters is
    /// silent — a `close` that forgot the offer query would shut a thing the
    /// grammar never gave a lid.
    ///
    /// # What it consults, in order, and why that order
    ///
    /// 1. **A named object.** Bare `open` is a hint, not a refusal: the player
    ///    typed a real verb (`examine`'s own bare arm sets the precedent).
    /// 2. **A chamber.** Only a chamber composes an [`crate::interior::
    ///    Interior`], so only a chamber holds anything with a lid.
    /// 3. **The noun, against this chamber's own anchors**, through
    ///    `chamber_prose::noun` — the SAME matcher [`Self::examine_chamber`]
    ///    uses, so a word that examines here also opens here or is refused
    ///    with the same sentence.
    /// 4. **The derived offer**, `offered_to_observer(...)`. Not
    ///    `object_registry` directly and never an `AnchorKind` literal: a new
    ///    thing-kind carrying `Openable` becomes openable with no edit here
    ///    (acceptance 7), and the knowledge gate stands in front of this verb
    ///    exactly as it stands in front of `warm` and `examine`.
    /// 5. **The lock**, and only for `open` — [`Self::container_is_locked`]
    ///    over the thing's own [`crate::thing::LOCKEDNESS`] fold, and only
    ///    then a read of the body's custody
    ///    ([`Self::carrying_something_that`]). `close` never consults either:
    ///    it shuts a lid and turns nothing (decision 0399).
    /// 6. **The no-op**, before the write. A second `open` on an open thing
    ///    reports the state and commits nothing, which is `clear`'s own
    ///    behaviour at an already-open mouth and is what keeps
    ///    `thing::set_openness`'s across-days `instance-of` duplication
    ///    unreachable through the verb.
    /// 7. **The charge**, after every refusal and before both writes.
    ///
    /// # CLOSED AND LOCKED ARE DIFFERENT STATES (decision 0399, fix round 1)
    ///
    /// This verb shipped conflating them, and the conflation was reachable in
    /// three moves. `openness` was the only state a container had, and the
    /// lock arm re-derived "locked" from the PLAYER'S POCKETS at every ask —
    /// so a chest was locked whenever the body happened not to be holding
    /// something `Portable`, whatever the chest's own history. Shutting a lid
    /// therefore re-locked it, and shutting it on the very key that opened it
    /// was a permanent soft-lock: `open` refused for want of a key, and
    /// `take a key` refused because the key was shut away. Measured through
    /// the shipped CLI on seed 1, unrecoverable at any day by any verb:
    ///
    /// ```text
    /// > take a key                -> You take the key.
    /// > open a strongbox          -> You open the strongbox. Within it: a key.
    /// > put a key in a strongbox  -> You put the key in the strongbox.
    /// > close a strongbox         -> You close the strongbox.
    /// > open a strongbox          -> It is locked, and you are carrying nothing that would open it.
    /// > take a key                -> The key is shut away in something closed.
    /// ```
    ///
    /// **The repair is a second state, not a special case.** `openness` is
    /// untouched — it is still Task 5's open/shut fold — and lockedness is its
    /// own predicate with its own authored default (`Lockable` and no fact
    /// means locked, which is what makes a seeded strongbox worth finding a
    /// key for). `open` with a key in custody turns the lock ONCE and commits
    /// the unlocking; nothing re-locks it, because locking needs the key put
    /// in the lock and this campaign ships no `lock` verb. The dead end
    /// disappears with no precondition anywhere reading a container's
    /// contents, which is what made every alternative remedy worse.
    ///
    /// # IT CHARGES, AND USED NOT TO (fix round 1)
    ///
    /// [`Self::charge_within_room`], the same `Action::MoveWithin` dial
    /// `take`/`drop`/`put` and the `down`/`up` stairs already read — **no new
    /// cost model**, which spec §3.4 forbids. `take`'s own doc recorded the
    /// hazard from the other side while these two verbs still stayed free, and
    /// understated it: a free verb leaves the clock where it found it, so
    /// `open`, `close`, `open` in one instant commits `open`, `shut`, and then
    /// a third fact byte-identical to the first, which `Ledger::commit` dedups
    /// away. The last posting at that instant is `shut` while every reply was
    /// computed before its commit. The result was not one wrong reply but a
    /// STUCK INSTANT: every later `open` said it opened and did not, `close`
    /// answered "already shut", `put` refused the shut chest, and only `wait`
    /// escaped. Measured on seed 1 before the fix:
    ///
    /// ```text
    /// > open a strongbox   -> You open the strongbox. Within it: a key.
    /// > close a strongbox  -> You close the strongbox.
    /// > open a strongbox   -> You open the strongbox. Within it: a key.
    /// > open a strongbox   -> You open the strongbox. Within it: a key.
    /// > close a strongbox  -> The strongbox is already shut.
    /// > put a key in a strongbox -> The strongbox is shut.
    /// ```
    ///
    /// The no-op arm (clause 6) returns BEFORE the charge, so reporting a
    /// state still costs nothing — a refusal is not an act.
    ///
    /// # What it does NOT do
    ///
    /// **It does not open a passage, and `open` at a barred cave mouth
    /// refuses.** `clear` is that act and Task 8 already folded it through
    /// this very predicate, so the two angles share a FOLD (spec §3.7's
    /// deliverable) without sharing a verb. Merging them is a real question
    /// and a bigger one than this task: `clear`'s prose is about rubble
    /// ("only a thin fall gives"), its refusals are seeded barrier states
    /// rather than properties, and a passage has no `Interior` to resolve a
    /// noun against. Whoever takes it up should read `clear_response` first.
    fn open_or_close(&mut self, rest: &str, open: bool) -> Turn {
        let wanted = rest.trim().to_lowercase();
        if wanted.is_empty() {
            return Turn::Out(
                if open {
                    OPEN_WHAT_HINT
                } else {
                    CLOSE_WHAT_HINT
                }
                .to_string(),
            );
        }
        let (Some(interior), Some(room)) =
            (self.chamber_interior_here(), self.chamber_facet_here())
        else {
            return Turn::Out(NOTHING_HERE_OPENS_REFUSAL.to_string());
        };
        let Some(id) = interior.ids().into_iter().find(|&id| {
            crate::chamber_prose::noun(interior.anchor(id).kind)
                .is_some_and(|n| n.to_lowercase() == wanted)
        }) else {
            return Turn::Out(format!("You see no {} here.", rest.trim()));
        };

        let thing_kind = crate::affordance::thing_kind_of(interior.anchor(id).kind);
        let offer =
            crate::affordance::offered_to_observer(thing_kind, self.driven_body(), &self.knowledge);
        let verb = if open {
            crate::affordance::OfferedVerb::Open
        } else {
            crate::affordance::OfferedVerb::Close
        };
        if !offer.contains(&verb) {
            return Turn::Out(format!(
                "The {} does not {}.",
                crate::chamber_prose::without_article(
                    crate::chamber_prose::noun(interior.anchor(id).kind)
                        .expect("a noun matched above")
                ),
                verb.word()
            ));
        }

        // Locked is a state of the THING, read off its own fold — not a state
        // of the player's pockets re-derived at every ask (decision 0399).
        // `close` is absent from this expression by construction: `open` gates
        // it, so shutting a lid can never turn a key.
        let locked = open && self.container_is_locked(&room, thing_kind);
        if locked && !self.carrying_something_that(crate::affordance::ObjectProperty::Portable) {
            return Turn::Out(LOCKED_WITHOUT_A_KEY_REFUSAL.to_string());
        }

        let bare = crate::chamber_prose::without_article(
            crate::chamber_prose::noun(interior.anchor(id).kind).expect("a noun matched above"),
        );
        if self.container_is_open(&room, thing_kind) == open {
            return Turn::Out(format!(
                "The {bare} is already {}.",
                if open { "open" } else { "shut" }
            ));
        }

        // Charged BEFORE the writes and AFTER every refusal, exactly where
        // `take`/`drop`/`put` charge, and for the representability reason
        // their docs give rather than for a tariff: two openness facts at one
        // instant are one fact, and `Ledger::commit`'s dedup of the third
        // posting in `open`, `close`, `open` used to leave the chest shut
        // while the reply said it opened. The no-op arm above returns first,
        // so reporting a state costs nothing.
        if let Err(e) = self.charge_within_room() {
            return Turn::Out(e);
        }

        // The key turns before the lid lifts. One posting, once: `locked` is
        // false unless the fold said the lock was still shut, so a chest
        // opened twice writes this once and a chest with no lock never writes
        // it at all.
        if locked {
            crate::thing::set_lockedness(
                &mut self.ledger,
                &self.registry,
                &room,
                thing_kind.0,
                0,
                false,
                self.day,
            )
            .expect(
                "a chamber facet packs, and LOCKEDNESS/instance-of are registered by Session::start",
            );
        }

        crate::thing::set_openness(
            &mut self.ledger,
            &self.registry,
            &room,
            thing_kind.0,
            0,
            open,
            self.day,
        )
        .expect("a chamber facet packs, and OPENNESS/instance-of are registered by Session::start");

        if !open {
            return Turn::Out(format!("You close the {bare}."));
        }
        match crate::chamber_prose::contents_of(&interior, id) {
            None => Turn::Out(format!("You open the {bare}. It is empty.")),
            Some(listed) => Turn::Out(format!("You open the {bare}. Within it: {listed}.")),
        }
    }

    /// Every thing in the driven body's custody, paired with the noun prose
    /// says it by — the one read `carrying`, `drop` and `put` all resolve
    /// against, so the three cannot disagree about what a body is holding.
    ///
    /// **Label-keyed, because custody is.** A carried thing reaches this
    /// method as an [`EntityId`] whose kind is an `instance-of` object — a
    /// runtime string — and the room it was promoted from may be two rooms
    /// behind with its interior no longer composed. So the noun comes from
    /// [`crate::chamber_prose::noun_for_label`] and never from an
    /// `AnchorKind`, which nothing here holds.
    ///
    /// A held thing whose label no anchor kind spells (`cave-mouth`, which
    /// `passage.rs` mints) is dropped from this list rather than named by its
    /// bare label: nothing can put one in a hand, and inventing prose for the
    /// unreachable case would be prose no test could ever read back.
    /// [`crate::thing::held_by`]'s `EntityId` order is preserved, which is
    /// deterministic and never ledger order.
    fn carried(&self) -> Vec<(EntityId, &'static str)> {
        crate::thing::held_by(&self.ledger, self.agent_entity(), self.day)
            .into_iter()
            .filter_map(|thing| {
                let noun = crate::chamber_prose::noun_for_label(self.ledger.kind_of(thing)?)?;
                Some((thing, noun))
            })
            .collect()
    }

    /// The carried thing a player's word names, matched exactly as
    /// [`Self::open_or_close`] and [`Self::examine_chamber`] match an
    /// anchor's: against the full noun, article and all. One convention for
    /// every verb that takes a thing, rather than a second, kinder one that
    /// would make `drop key` work where `open strongbox` does not.
    fn carried_named(&self, wanted: &str) -> Option<(EntityId, &'static str)> {
        self.carried()
            .into_iter()
            .find(|(_, noun)| noun.to_lowercase() == wanted)
    }

    /// The thing-kind of `holder` if it is one of THIS chamber's own anchors,
    /// and `None` if it is something standing somewhere else — asked by
    /// [`Self::take`] to tell "in the chest here" from "in a chest two rooms
    /// back".
    ///
    /// **It returns the KIND rather than a yes/no, and that is not
    /// convenience.** The caller's next question is whether the holder is
    /// open, and the only honest source for "does this holder have a lid" is
    /// the anchor whose identity just matched — not
    /// [`hornvale_kernel::Ledger::kind_of`], which answers `None` for a
    /// container nothing has promoted yet and would send an untouched
    /// strongbox down [`Self::holder_admits`]'s lidless arm.
    ///
    /// **The red that hazard names was a `put`, and `put` does not call this
    /// function** — the doc said "it is the red this signature was written in
    /// response to", which credited one verb's defect to the other verb's
    /// helper (fix round 1). The function BOTH verbs share is
    /// [`Self::holder_admits`], and what it needs from either caller is a
    /// kind derived from an ANCHOR rather than from the ledger.
    /// [`Self::put_in`] gets one inline — it already holds the anchor it
    /// matched the container's noun against — and [`Self::take`] cannot,
    /// because it starts from a holder [`crate::thing::location_of`] handed
    /// it and has to find the anchor again. That search is what this function
    /// is, and returning the kind rather than a bool is what keeps the ledger
    /// out of the lid question on the path that has no anchor in hand.
    ///
    /// Derived, never minted: [`crate::thing::thing_id`] is a pure function
    /// of `(room, kind, ordinal)`, so this compares ids for anchors that may
    /// never have been promoted at all. `ordinal` is `0` for the reason
    /// `interior::pattern::tests::no_production_room_composes_two_anchors_of_
    /// one_kind` licenses everywhere else in this campaign.
    fn holder_anchored_here(
        &self,
        interior: &crate::interior::Interior,
        room: &Facet,
        holder: EntityId,
    ) -> Option<hornvale_kernel::KindId> {
        interior.ids().into_iter().find_map(|id| {
            let kind = crate::affordance::thing_kind_of(interior.anchor(id).kind);
            crate::thing::thing_id(room, kind.0, 0)
                .is_ok_and(|e| e == holder)
                .then_some(kind)
        })
    }

    /// Whether a container of `kind`, carrying entity `holder`, will let
    /// things through right now — the precondition `take` reads before
    /// pulling a thing out and `put` reads before pushing one in. **One
    /// function for both directions**, because a lid a player can reach past
    /// in one direction and not the other would be a rule nobody wrote.
    ///
    /// **Two arms, and the second is the alcove.** A kind carrying
    /// [`crate::affordance::ObjectProperty::Openable`] has a lid, so its
    /// [`crate::thing::OPENNESS`] fold decides — with the same authored
    /// default [`Self::container_is_open`] states, an absent fact reading
    /// *shut*. A kind with no lid (a recess cut into a wall) admits
    /// unconditionally: there is nothing on it to be closed.
    ///
    /// M+N: it names no kind literal, so a second lidded container arrives
    /// with no edit here, and a container that is `Encloses` and not
    /// `Openable` needs none either.
    fn holder_admits(&self, kind: hornvale_kernel::KindId, holder: EntityId) -> bool {
        !crate::affordance::carries(kind, crate::affordance::ObjectProperty::Openable)
            || crate::thing::is_open(&self.ledger, holder, self.day).unwrap_or(false)
    }

    /// `take <thing>` — the verb the whole campaign is named for (The
    /// Chattel, Task 12, spec §3.8 and acceptance 1).
    ///
    /// # Custody is a `located-in` fact naming the BODY
    ///
    /// Nothing is copied, moved between stores, or held in a session field: a
    /// take is one dated [`crate::thing::LOCATED_IN`] posting whose object is
    /// [`hornvale_kernel::Value::Entity`] of the driven body
    /// ([`crate::thing::located_in_holder_fact`]). That is what makes custody
    /// survive `possess --out` for free — a saved world carries the ledger
    /// (decision 0368) and the fold that reads it is the same fold either
    /// side of the save.
    ///
    /// # The three ledger states a takeable slot can be in
    ///
    /// The grammar's half of the offer — *does this room compose a key at
    /// all* — is `interior.ids()`, and the noun match against it is the same
    /// one `open`/`examine` use. The LEDGER's half is
    /// [`crate::thing::is_latent`], and this is its first production caller:
    ///
    /// | state | answer |
    /// |---|---|
    /// | never placed, and the GRAMMAR puts it inside a shut container | "shut away in something closed" |
    /// | latent otherwise (never placed and reachable, or placed here) | taken |
    /// | already located on the body | "you are already carrying it" |
    /// | located in a container standing here | taken, if that container admits ([`Self::holder_admits`]) |
    /// | anywhere else | the LEDGER is asked whether anything of that name is here ([`Self::take_from_the_ledger`]) |
    ///
    /// The fourth row is what makes `put` reversible: a key stowed in a chest
    /// stops being latent, so without it the room would refuse to give back
    /// what the player had just put down.
    ///
    /// # THE LID GATE RUNS ON BOTH PATHS NOW, AND THIS DOC USED TO RECORD THE
    /// DEFECT INSTEAD OF FIXING IT
    ///
    /// The first row is new (Task 13, fix round 1). It used to be absent, and
    /// the absence was **asymmetric in a way no reader would guess**: the
    /// container check sat behind `if !is_latent(…)`, so a key the LEDGER had
    /// put in a chest was refused (*"The key is shut away in something
    /// closed"*) while a key the GRAMMAR had composed inside the same chest —
    /// the untouched, as-generated case, which is every world on its first
    /// turn — came out through a locked lid. Measured through the shipped CLI
    /// on seed 1, standing in the storeroom:
    ///
    /// ```text
    /// > open a strongbox   It is locked, and you are carrying nothing that would open it.
    /// > take a key         You take the key.
    /// > open a strongbox   You open the strongbox. Within it: a key.
    /// ```
    ///
    /// The only lock in the game fell in one move. This doc RECORDED that —
    /// "the consequence is a real defect and it is NOT in this function" —
    /// and deferred it to an authored-grammar fix, on the grounds that
    /// gating the verb alone "would trade a lock nobody can defend for a
    /// verb nobody can reach". That reasoning was sound and its conclusion
    /// was still wrong, because it treated the two halves as alternatives.
    /// They are one change: `interior::pattern`'s `the-key-on-the-ledge`
    /// puts a key in the hearthroom — a room `role_for` guarantees is
    /// shallower than any `Role::Store` — so the lid can close without the
    /// strongbox becoming unopenable. Neither half is safe alone.
    ///
    /// **It asks `Anchor.within`, and only on the latent path.** The
    /// grammar's containment is the truth exactly while the ledger has no
    /// opinion; once a thing has been placed, its `located-in` fold is
    /// authoritative and the anchor graph is stale. Applying the gate
    /// unconditionally would refuse a key the player had already taken out
    /// and set down on that room's own floor, because the grammar still says
    /// it lives in the chest.
    ///
    /// **The lid, not the lock**, and the two are separate states (decision
    /// 0399). This asks [`Self::holder_admits`] — the same question `put`
    /// and [`Self::take_from_the_ledger`] ask about the same lid, so one
    /// container cannot be reachable one way and not the other. A locked box
    /// is refused here because it is also shut, not because it is locked;
    /// unlock it, open it, and `take` proceeds.
    ///
    /// **What still is NOT gated, stated so the asymmetry that remains is a
    /// choice.** `chamber_prose::chamber_nouns` still names a key inside a
    /// shut strongbox, `describe_chamber` still says so on entry, and
    /// `examine a key` still answers in full. Those belong to
    /// `PLAY-closed-container-conceals-nothing`, whose bill decision 0398
    /// priced. A verb that MOVES a thing is the one place the lid has to
    /// mean something, and it does now.
    ///
    /// # IT CHARGES, UNLIKE `open`/`close`/`warm`, AND THE REASON IS
    /// REPRESENTABILITY RATHER THAN TARIFF
    ///
    /// It reuses [`Self::charge_within_room`]'s existing
    /// [`crate::action::Action::MoveWithin`] dial — the same reuse The
    /// Gallery's `down`/`up` argued for, on the same grounds (one flight of
    /// stairs, or one thing lifted, is the scale of act that dial already
    /// prices). **No new cost model is minted**, which is what spec §3.4
    /// forbids; a dial that already exists is not one.
    ///
    /// The first draft made all three free, on `warm`'s precedent, and it
    /// was WRONG in a way only running it showed. Custody is a fold over
    /// dated postings, [`hornvale_kernel::Ledger::commit`] returns `false`
    /// for a fact identical to one already held, and a free verb leaves the
    /// clock where it found it — so `take`, `put`, `take` in one instant
    /// commits `(key -> body)`, `(key -> chest)`, and then a THIRD fact
    /// byte-identical to the first, which is deduped away. The postings at
    /// that instant are `[body, chest]`, "last posting at one instant wins"
    /// (`thing::latest_object_at_or_before`) resolves to the chest, and the
    /// player is told they picked up a key that the world says is still in
    /// the box. Measured, not reasoned:
    ///
    /// ```text
    /// > take a key                  -> You take the key.
    /// > carrying                    -> You are carrying nothing.
    /// ```
    ///
    /// Charging gives each act its own instant, so the fold has an order to
    /// read. **The same hazard was live in `open`/`close`, which shipped
    /// free, and fix round 1 closed it there too**: `open`, `close`, `open` at
    /// one instant dedups the third fact and leaves the chest SHUT while the
    /// reply says it opened, because every reply in [`Self::open_or_close`] is
    /// computed before its commit. That verb pair now takes the same
    /// `MoveWithin` charge, before its writes and after its refusals; see its
    /// own doc for the measured stuck-instant transcript.
    ///
    /// **The sentence this replaces deferred the repair to a witness that
    /// does not exist**, and that is the part worth keeping. It read: *"those
    /// two verbs are Task 11's and their transcripts are in the galleries."*
    /// No gallery contains `open`, `close`, `take`, `drop`, `put` or
    /// `carrying` at all — `scripts/possession-walk.txt` and
    /// `scripts/possession-over-time-walk.txt` are the only inputs
    /// `book/src/gallery/possession-*.md` is generated from, and neither
    /// types any of the six. A deferral that names a witness nobody checked
    /// reads as coverage; there was none, and the defect survived a task
    /// boundary because of it.
    fn take(&mut self, rest: &str) -> Turn {
        let wanted = rest.trim().to_lowercase();
        if wanted.is_empty() {
            return Turn::Out(TAKE_WHAT_HINT.to_string());
        }
        let (Some(interior), Some(room)) =
            (self.chamber_interior_here(), self.chamber_facet_here())
        else {
            return Turn::Out(NOTHING_HERE_TO_TAKE_REFUSAL.to_string());
        };
        let Some(id) = interior.ids().into_iter().find(|&id| {
            crate::chamber_prose::noun(interior.anchor(id).kind)
                .is_some_and(|n| n.to_lowercase() == wanted)
        }) else {
            return self.take_from_the_ledger(&interior, &room, &wanted, rest.trim());
        };

        let thing_kind = crate::affordance::thing_kind_of(interior.anchor(id).kind);
        let bare = crate::chamber_prose::without_article(
            crate::chamber_prose::noun(interior.anchor(id).kind).expect("a noun matched above"),
        );
        if !crate::affordance::offered_to_observer(thing_kind, self.driven_body(), &self.knowledge)
            .contains(&crate::affordance::OfferedVerb::Take)
        {
            return Turn::Out(format!("The {bare} is not yours to carry off."));
        }

        let thing = crate::thing::thing_id(&room, thing_kind.0, 0)
            .expect("a chamber facet packs, or the interior above could not have composed");
        let body = self.agent_entity();
        // The GRAMMAR's containment, authoritative exactly while the ledger
        // holds NO opinion about where this thing is.
        //
        // **Asked of the location fold's `None`, and not of
        // [`crate::thing::is_latent`], and the difference is a real state a
        // player can produce.** `is_latent` is true for two things: an anchor
        // nothing ever placed, and a thing set down on THIS room's own floor.
        // Only the first has no ledger answer to defer to. Gating on
        // `is_latent` would refuse a key the player had taken out of the chest
        // and dropped at their feet the moment they shut the lid — the anchor
        // graph still says that key lives in the chest, forever, because a
        // composed interior is a pure function of the room and never moves.
        if crate::thing::location_of(&self.ledger, thing, self.day).is_none()
            && let Some(container) = interior.anchor(id).within
        {
            let container_kind = crate::affordance::thing_kind_of(interior.anchor(container).kind);
            let container_entity = crate::thing::thing_id(&room, container_kind.0, 0)
                .expect("a chamber facet packs, or the interior could not have composed");
            if !self.holder_admits(container_kind, container_entity) {
                return Turn::Out(format!("The {bare} is shut away in something closed."));
            }
        }
        if !crate::thing::is_latent(&self.ledger, &room, thing_kind.0, 0, self.day)
            .expect("a chamber facet packs")
        {
            // THIS ROOM'S OWN ANCHOR IS SOMEWHERE ELSE — so ask the ledger
            // whether something of that NAME is here anyway, rather than
            // refusing on the strength of the one thing the grammar happens
            // to spell. The two arms below used to answer "You see no … here"
            // outright, which was harmless while the only key anchor in the
            // world sat in the deepest room a walk could reach and nothing
            // could be carried to it. `the-key-by-the-door` ends that: the
            // threshold chamber composes a key anchor, so a key carried in
            // from elsewhere and set down at the front door would be shadowed
            // by an anchor whose own key is three rooms away in a chest, and
            // a player would be told there was no key while standing on one.
            let holder = match crate::thing::location_of(&self.ledger, thing, self.day) {
                Some(Value::Entity(h)) if h == body => {
                    return Turn::Out(format!("You are already carrying the {bare}."));
                }
                Some(Value::Entity(h)) => h,
                _ => return self.take_from_the_ledger(&interior, &room, &wanted, rest.trim()),
            };
            let Some(holder_kind) = self.holder_anchored_here(&interior, &room, holder) else {
                return self.take_from_the_ledger(&interior, &room, &wanted, rest.trim());
            };
            if !self.holder_admits(holder_kind, holder) {
                return Turn::Out(format!("The {bare} is shut away in something closed."));
            }
        }

        if let Err(e) = self.charge_within_room() {
            return Turn::Out(e);
        }
        crate::thing::promote(
            &mut self.ledger,
            &self.registry,
            &room,
            thing_kind.0,
            0,
            self.day,
        )
        .expect("a chamber facet packs, and instance-of is a kernel-core predicate");
        let fact = crate::thing::located_in_holder_fact(thing, body, self.day);
        self.ledger
            .commit(fact, &self.registry)
            .expect("LOCATED_IN is registered by Session::start and is non-functional");
        Turn::Out(format!("You take the {bare}."))
    }

    /// [`Self::take`]'s second source: a thing the LEDGER says is lying on
    /// this chamber's floor, rather than one the grammar composes here.
    ///
    /// **A dropped thing has no anchor, and that is the whole reason this
    /// exists.** `interior_of` composes what a room's *pattern grammar* puts
    /// in it; a key carried in from two rooms away is composed by nothing
    /// here, so the noun match one frame up cannot see it and a player who set
    /// it down would never pick it up again. [`crate::thing::lying_in`] is
    /// [`crate::thing::held_by`]'s room-keyed sibling and answers exactly the
    /// question that gap poses.
    ///
    /// **No property gate, deliberately.** A thing on this floor is here
    /// because a body carried it here, and `take` already asked whether it was
    /// [`crate::affordance::ObjectProperty::Portable`] on the way in. Asking
    /// again would let a registry edit strand a thing in a room forever, with
    /// no verb able to lift it.
    ///
    /// **The room's PROSE does not name it, and this doc says so rather than
    /// implying otherwise.** `chamber_prose::describe_chamber` renders from
    /// the composed interior alone, so a dropped key is takeable here and
    /// unmentioned here. Putting the ledger's own things into the narration is
    /// the wire's half of this campaign (Task 13, "carried things on the
    /// snapshot"), not this verb's.
    ///
    /// **THE DIVERGENCE RUNS BOTH WAYS, AND ONLY THIS DIRECTION WAS EVER
    /// WRITTEN DOWN** (fix round 1). The paragraph above says the prose omits
    /// what the ledger holds. The prose also ASSERTS what the ledger denies:
    /// after `take a key`, `look` still lists *"a key"* among the room's
    /// contents, and `open a strongbox` still answers *"Within it: a key"*
    /// while the key is in the player's hand — because
    /// [`crate::chamber_prose::contents_of`] reads `nouns_within`, the
    /// GRAMMAR's containment, with no latency filter and no ledger at all.
    /// Neither direction is repaired here: both belong to the deferred
    /// containment work registered as
    /// `PLAY-closed-container-conceals-nothing`, whose bill (threading day and
    /// ledger into a pure, published `describe_chamber`) decision 0398 already
    /// priced.
    ///
    /// # A SECOND SOURCE: A CONTAINER STANDING HERE (fix round 1)
    ///
    /// The floor is not the only place the grammar cannot see. `put a key in
    /// an alcove`, in any room whose grammar composes no key of its own, used
    /// to lose the key **permanently and silently**: [`Self::take`]'s
    /// container arm sits behind the noun match against `interior.ids()`, and
    /// a room composing no key anchor never reaches it, so control arrived
    /// here — and [`crate::thing::lying_in`] is room-keyed and DIRECT, so it
    /// cannot see a thing whose location is a container entity. Measured
    /// through the shipped CLI on seed 1, in the alcove room two doors in:
    ///
    /// ```text
    /// > put a key in an alcove  -> You put the key in the alcove.
    /// > take a key              -> You see no a key here.
    /// > drop a key              -> You are not carrying a key.
    /// > look                    -> A small room, holding a doorway and an alcove.
    /// ```
    ///
    /// That is the exact loss [`NOWHERE_TO_SET_DOWN_REFUSAL`] refuses `drop`
    /// out of doors to prevent ("it would be lost"), arriving through the
    /// sibling verb with no refusal and no record. So this function asks the
    /// ledger twice: the floor first, then every container this room's
    /// grammar stands ([`Self::stowed_in_a_container_here`]). A shut container
    /// answers with the same sentence `take`'s own container arm gives, since
    /// it is the same [`Self::holder_admits`] question about the same lid.
    fn take_from_the_ledger(
        &mut self,
        interior: &crate::interior::Interior,
        room: &Facet,
        wanted: &str,
        typed: &str,
    ) -> Turn {
        let here = crate::thing::lying_in(&self.ledger, room, self.day)
            .expect("a chamber facet packs, or the interior could not have composed");
        let on_the_floor = here.into_iter().find_map(|thing| {
            let noun = crate::chamber_prose::noun_for_label(self.ledger.kind_of(thing)?)?;
            (noun.to_lowercase() == wanted).then_some((thing, noun))
        });
        // The floor first, then the containers standing on it — a thing set
        // down loose is the commoner case and the cheaper read, and a thing
        // cannot be in both places at once, so the order is a preference and
        // never an ambiguity.
        let found = match on_the_floor {
            Some((thing, noun)) => Some((thing, noun, None)),
            None => self
                .stowed_in_a_container_here(interior, room, wanted)
                .map(|(thing, noun, kind, holder)| (thing, noun, Some((kind, holder)))),
        };
        let Some((thing, noun, stowed)) = found else {
            return Turn::Out(format!("You see no {typed} here."));
        };
        let bare = crate::chamber_prose::without_article(noun);
        if let Some((holder_kind, holder)) = stowed
            && !self.holder_admits(holder_kind, holder)
        {
            return Turn::Out(format!("The {bare} is shut away in something closed."));
        }
        if let Err(e) = self.charge_within_room() {
            return Turn::Out(e);
        }
        let fact = crate::thing::located_in_holder_fact(thing, self.agent_entity(), self.day);
        self.ledger
            .commit(fact, &self.registry)
            .expect("LOCATED_IN is registered by Session::start and is non-functional");
        Turn::Out(format!("You take the {bare}."))
    }

    /// The thing named `wanted` sitting inside one of THIS room's containers,
    /// with the container's kind and entity — [`Self::take_from_the_ledger`]'s
    /// second source (fix round 1), and the half that closes `put`'s one-way
    /// trapdoor.
    ///
    /// **It starts from the GRAMMAR and asks the LEDGER, which is the only
    /// order that terminates.** A room's containers are its anchors, so the
    /// enumeration is `interior.ids()` filtered to
    /// [`crate::affordance::ObjectProperty::Encloses`]; each one's entity is
    /// derived by [`crate::thing::thing_id`] (a pure function, so a container
    /// nothing has ever promoted still resolves) and asked what it holds
    /// through [`crate::thing::held_by`]. The reverse order — enumerate every
    /// thing in the ledger and ask where it is — would answer about containers
    /// in other rooms, which is the question [`Self::holder_anchored_here`]
    /// exists to say no to.
    ///
    /// **DIRECT containment, one level, deliberately.** A chest inside a chest
    /// is not reachable here, and nothing in this world composes one:
    /// `Anchor.within` nests exactly one deep and no `Encloses` kind is
    /// `Within` another. A transitive walk would be a fold nothing could
    /// exercise, and the honest place for it is the day something nests.
    ///
    /// **No openness check**, and that is not an omission: the caller needs
    /// the pair to ask [`Self::holder_admits`] and give the shut lid its own
    /// sentence, which a `None` here could not be told apart from "no such
    /// thing in this room". Deterministic by construction: `interior.ids()`
    /// and `held_by` both answer in `EntityId` order, never ledger order.
    fn stowed_in_a_container_here(
        &self,
        interior: &crate::interior::Interior,
        room: &Facet,
        wanted: &str,
    ) -> Option<(EntityId, &'static str, hornvale_kernel::KindId, EntityId)> {
        interior.ids().into_iter().find_map(|id| {
            let kind = crate::affordance::thing_kind_of(interior.anchor(id).kind);
            if !crate::affordance::carries(kind, crate::affordance::ObjectProperty::Encloses) {
                return None;
            }
            let holder = crate::thing::thing_id(room, kind.0, 0).ok()?;
            crate::thing::held_by(&self.ledger, holder, self.day)
                .into_iter()
                .find_map(|thing| {
                    let noun = crate::chamber_prose::noun_for_label(self.ledger.kind_of(thing)?)?;
                    (noun.to_lowercase() == wanted).then_some((thing, noun, kind, holder))
                })
        })
    }

    /// `drop <thing>` — custody's other direction: one
    /// [`crate::thing::LOCATED_IN`] posting naming the ROOM instead of the
    /// body ([`crate::thing::located_in_room_fact`]).
    ///
    /// **It resolves the thing against CUSTODY, not against the room**, and
    /// that is the whole difference from [`Self::take`]. A carried thing may
    /// be of a kind this chamber's grammar never composes — it came from a
    /// room two doors back — so there is no anchor here to match a word
    /// against and [`crate::chamber_prose::noun`] cannot be reached from an
    /// `AnchorKind` this interior does not hold.
    ///
    /// **The order of its refusals is deliberate**: the custody lookup comes
    /// BEFORE the band check, so a player carrying nothing is told so
    /// wherever they are standing, rather than being told about the floor.
    ///
    /// **Chamber-only, and the reason is mechanical rather than physical.** A
    /// body out of doors could obviously set something down; what it could
    /// not do is ever find it again. [`Self::chamber_facet_here`] is the only
    /// room key a thing's identity is derived under, and no locale-band
    /// interior composes a portable anchor, so a thing dropped outside would
    /// have a location no offer list ever reads — lost, silently, with a
    /// cheerful reply. Refusing with the reason is the honest half.
    fn drop_carried(&mut self, rest: &str) -> Turn {
        let wanted = rest.trim().to_lowercase();
        if wanted.is_empty() {
            return Turn::Out(DROP_WHAT_HINT.to_string());
        }
        let Some((thing, noun)) = self.carried_named(&wanted) else {
            return Turn::Out(format!("You are not carrying {}.", rest.trim()));
        };
        let Some(room) = self.chamber_facet_here() else {
            return Turn::Out(NOWHERE_TO_SET_DOWN_REFUSAL.to_string());
        };
        let bare = crate::chamber_prose::without_article(noun);
        if let Err(e) = self.charge_within_room() {
            return Turn::Out(e);
        }
        let fact = crate::thing::located_in_room_fact(thing, &room, self.day)
            .expect("a chamber facet packs");
        self.ledger
            .commit(fact, &self.registry)
            .expect("LOCATED_IN is registered by Session::start and is non-functional");
        Turn::Out(format!("You set the {bare} down."))
    }

    /// `put <thing> in <container>` — the third direction custody can go, and
    /// the one that makes [`crate::thing::room_of`]'s transitive walk a thing
    /// a player can produce rather than a thing a test can construct.
    ///
    /// **Two objects, resolved in two different places**, which is why this
    /// is not folded into [`Self::drop_carried`]: the thing comes from
    /// custody and the container from this chamber's anchors. Both halves of
    /// the container's precondition are act-level rather than offer-level —
    /// [`crate::affordance::ObjectProperty::Encloses`] is what makes it a
    /// container at all, and [`Self::holder_admits`] is whether it is open —
    /// so neither is expressible in `offered_to_observer`, which holds one
    /// object. `Lockable`'s omission from `required_properties` is the same
    /// argument, and this is its second instance.
    ///
    /// **The preposition is `" in "`, matched literally.** No noun this
    /// module authors contains it, so a split on the first occurrence
    /// partitions the line exactly. A bare `put` and a `put` with no `in`
    /// both answer with the shape rather than a refusal — `examine`'s own
    /// bare arm is the precedent: the player typed a real verb.
    fn put_in(&mut self, rest: &str) -> Turn {
        let line = rest.trim().to_lowercase();
        if line.is_empty() {
            return Turn::Out(PUT_WHAT_HINT.to_string());
        }
        let Some((what, holder_word)) = line.split_once(" in ") else {
            return Turn::Out(PUT_WHAT_HINT.to_string());
        };
        let (what, holder_word) = (what.trim(), holder_word.trim());
        let Some((thing, noun)) = self.carried_named(what) else {
            return Turn::Out(format!("You are not carrying {what}."));
        };
        let (Some(interior), Some(room)) =
            (self.chamber_interior_here(), self.chamber_facet_here())
        else {
            return Turn::Out(format!("You see no {holder_word} here."));
        };
        let Some(id) = interior.ids().into_iter().find(|&id| {
            crate::chamber_prose::noun(interior.anchor(id).kind)
                .is_some_and(|n| n.to_lowercase() == holder_word)
        }) else {
            return Turn::Out(format!("You see no {holder_word} here."));
        };

        let holder_kind = crate::affordance::thing_kind_of(interior.anchor(id).kind);
        let holder_bare = crate::chamber_prose::without_article(
            crate::chamber_prose::noun(interior.anchor(id).kind).expect("a noun matched above"),
        );
        let bare = crate::chamber_prose::without_article(noun);
        if !crate::affordance::carries(holder_kind, crate::affordance::ObjectProperty::Encloses) {
            return Turn::Out(format!("The {holder_bare} does not hold things."));
        }
        // Derived first and promoted only once the act is certain, so a
        // refused `put` mints nothing: `promote` commits an `instance-of`
        // dated `self.day`, and promoting before the charge would date that
        // fact one instant earlier than the posting beside it — the
        // across-days duplication `thing::set_openness`'s own doc names.
        let holder = crate::thing::thing_id(&room, holder_kind.0, 0)
            .expect("a chamber facet packs, or the interior could not have composed");
        if !self.holder_admits(holder_kind, holder) {
            return Turn::Out(format!("The {holder_bare} is shut."));
        }
        if let Err(e) = self.charge_within_room() {
            return Turn::Out(e);
        }
        crate::thing::promote(
            &mut self.ledger,
            &self.registry,
            &room,
            holder_kind.0,
            0,
            self.day,
        )
        .expect("a chamber facet packs, and instance-of is a kernel-core predicate");
        let fact = crate::thing::located_in_holder_fact(thing, holder, self.day);
        self.ledger
            .commit(fact, &self.registry)
            .expect("LOCATED_IN is registered by Session::start and is non-functional");
        Turn::Out(format!("You put the {bare} in the {holder_bare}."))
    }

    /// `carrying` — what the driven body has in hand, read off the same fold
    /// every other custody question uses ([`Self::carried`]).
    ///
    /// **In-character, and gated by the body.** See [`IN_CHARACTER_VERBS`]'s
    /// own doc for the argument and for the mechanical half — the
    /// disjointness check could not have caught the other placement, which is
    /// why `carrying_is_refused_while_asleep` exists.
    ///
    /// Takes no argument, like `knows` and `needs` beside it, and ignores one
    /// rather than refusing: the two verbs it is modelled on do the same, and
    /// a body's whole custody is a short list.
    ///
    /// The list is formatted by [`crate::chamber_prose::listed`], which is
    /// literally the function `open`'s "Within it: …" clause is made of, so a
    /// chest and a pair of hands cannot spell one list two ways.
    fn carrying(&self) -> String {
        let held = self.carried();
        let nouns: Vec<&str> = held.iter().map(|(_, noun)| *noun).collect();
        match crate::chamber_prose::listed(&nouns) {
            None => "You are carrying nothing.".to_string(),
            Some(list) => format!("You are carrying {list}."),
        }
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
    /// **Group A** — `why`/`npcs`/`help`/`eyes`/`whoami`/`provoke`/`soothe`,
    /// and (The Coercion, Task 4) `possess`/`unpossess` — are operator
    /// instruments with no in-character counterpart. The first seven have
    /// none because Task 5 retired their bare forms; `possess`/`unpossess`
    /// have none because no creature can perform the act yet at all (spec §5
    /// defers the biology behind an imposed possession to a species-domain
    /// campaign) — this namespace is their only entry point either way.
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
    /// | `!examine` | the chamber band, on a creature sight withheld | the walk band (the objective eyes reach [`Self::purview_through`] but the legend carries nouns and datums, never colour) and underground (`examine_underground` takes no `Perceiving`/`Eyes` parameter at all, and reads `sight_reach()` — a constant unaffected by objective mode — for its own creature arm too) |
    /// | `!needs` | the chamber band | out of doors, where `sighting()` is `None` and [`Perceiving::Body`] already *is* the limit |
    /// | `!wait` | the chamber band, on both halves of the motion narration | out of doors, for the same reason as `!needs` |
    ///
    /// An alias here is the honest outcome, not a defect: the objective view of
    /// a place with nothing withheld is the subjective view of it. What would
    /// be a defect is advertising otherwise, so [`HELP`] states the band each
    /// arm is *for* rather than promising a difference it cannot always make.
    ///
    /// **Underground's cell in that row used to read "(no creature arm at
    /// all)", and the reason — not the conclusion — is what The Gallery's
    /// Task 11 falsified.** Underground has a creature arm now:
    /// [`Self::underground_nouns`] gains the chamber's own derived resident
    /// (spec §3.6) whenever [`Self::underground_resident`] finds one, lit.
    /// The row's CONCLUSION survives unchanged — `examine_underground` still
    /// takes no `Perceiving`/`Eyes` parameter, and the arm's own visibility
    /// test (`sight_reach()`) is still a constant no objective mode moves —
    /// so the bare and `!`-prefixed calls still agree byte-for-byte; only the
    /// STATED REASON was stale, the same "fix the reason, keep the
    /// conclusion" shape a driver.rs correction took one task earlier.
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
            // any of them, by design.
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
            // The Coercion, Task 4: the imposition seam. No creature can
            // possess another yet (spec §5), so this operator instrument
            // stands in for one — the same "the operator authors the world
            // event a creature cannot yet cause" shape `provoke`/`soothe`
            // already established. Named after `Controller`/`ImposedController`,
            // the abstraction this pair actually opens and closes, in the
            // same spirit game engines pair `Possess`/`UnPossess` on a
            // controller.
            "possess" => self.possess(),
            "unpossess" => self.unpossess(),
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
            // The underground half of Task 8, objective mood: same rule as
            // the bare form just below — a level, not the country overhead.
            "map" if self.underground.is_some() && rest.is_empty() => self.out(self.level_here()),
            "map" if self.underground.is_some() => Turn::Out(UNDERGROUND_CHART_REFUSAL.to_string()),
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
            // that a sleeping — later possessed, unconscious — body can still
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
                // The underground band (The Gallery, Task 8, spec §5): the
                // pane and the verb agree now — both answer `underground`,
                // never the walk band's chart of the country overhead. No
                // `eyes` parameter here, matching `level_here`'s own reason
                // (`vessel/level/v1` carries an explicit visibility state
                // per cell, never a colour, so there is nothing for a lens
                // to filter).
                "map" if self.underground.is_some() && rest.is_empty() => {
                    self.out(self.level_here())
                }
                "map" if self.underground.is_some() => {
                    Turn::Out(UNDERGROUND_CHART_REFUSAL.to_string())
                }
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
                // The cave level, by contrast, IS reversed (The Gallery, Task
                // 4): Task 3 gave it real cells to walk, which is what makes
                // `UNDERGROUND_LATERAL_REFUSAL`'s own claim false.
                // `step_underground` is `Self::step`'s reversal one band
                // over, following the same three rules its own doc names —
                // a diagonal refused before any lookup, passability asked as
                // a predicate, and lateral movement that never changes band.
                "go" if self.underground.is_some() => self.step_underground(rest),
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
                    Turn::Out(UNDERGROUND_BACK_REFUSAL.to_string())
                }
                "back" => self.back(),
                "wait" => self.wait(rest, Perceiving::Body),
                "knows" => Turn::Out(self.knows()),
                "needs" => Turn::Out(self.needs(Perceiving::Body)),
                // The verb this campaign adds (The Confidant, Task 6): ask
                // the possessed body itself, rather than reading its felt
                // state through the arbitration `needs` uses. `Self::ask`
                // is the whole of the tongue and gap machinery; this arm is
                // only dispatch.
                "ask" => Turn::Out(self.ask()),
                // The one verb this arc adds (The Deed, Task 7): the
                // acceptance test needs a body that can stop obeying, and
                // none of spec §3.2's 26 could produce one. Routed to the
                // existing `Action::Rest` machinery — no new concept, no new
                // cost dial, no new predicate.
                "sleep" => self.sleep(rest),
                "write" => Turn::Out(self.write(rest)),
                "consult" => Turn::Out(self.consult()),
                // The one wholly new verb The Offer adds (spec §3.3/§6): the
                // live witness for acceptance test (2) — `Hearth` offers it
                // with no edit to `affordance::object_registry`. Free, like
                // `dive`/`surface`/`delve`/`climb` just below: no authored
                // cost dial exists for warming oneself, and inventing one
                // here would be the same "new cost model" spec §3.4 forbids
                // those four from minting. See `Self::warm`'s own doc for
                // why it needs no `Action` variant either.
                "warm" => self.warm(),
                // The two verbs The Chattel adds (Task 11, spec §3.7/§3.8).
                // Unlike `warm` just above they COMMIT — an `openness` fact
                // about a promoted thing — which is why `open`/`close` are
                // the second and third entries in `IN_CHARACTER_VERBS` that
                // write to the ledger at all, and why each carries its own
                // `<verb>_is_refused_while_asleep` test rather than trusting
                // the roster-agreement check (which cannot see a verb missing
                // from both rosters — The Latch shipped `clear` that way).
                //
                // Free, like `warm`: `clock::base_ticks` prices no lid, and
                // minting a cost dial here is the "new cost model" spec §3.4
                // forbids. One handler for both directions, for the reason
                // `thing::set_openness`'s doc gives about its own halves.
                "open" => self.open_or_close(rest, true),
                "close" => self.open_or_close(rest, false),
                // The four verbs The Chattel adds last (Task 12, spec §3.8,
                // acceptance 1): the campaign's thesis — an identity that
                // moves — becomes playable here. Three of them WRITE (a
                // `located-in` posting naming a body, a room, or a chest),
                // which puts `take`/`drop`/`put` in the same consequential
                // class as `clear` and `open`/`close`, and is why each has its
                // own `<verb>_is_refused_while_asleep` rather than trusting
                // the roster-agreement check.
                //
                // `carrying` writes nothing and is in-character all the same:
                // what it reads IS the body. See `IN_CHARACTER_VERBS`'s doc
                // for that argument and for the blind zone in
                // `session_control_is_never_an_in_character_verb` that would
                // have let the other choice ship ungated.
                //
                // The three writers CHARGE, unlike `open`/`close`/`warm`
                // just above, and they reuse `Action::MoveWithin` rather
                // than minting a dial — The Gallery's `down`/`up`
                // precedent, so spec §3.4's "no new cost model" is intact.
                // The reason is not tariff but REPRESENTABILITY: two moves
                // of one thing at one instant are indistinguishable to an
                // as-of-day fold, and a repeat posting is deduped outright.
                // See `Session::take`'s own doc for the measured failure.
                // `carrying` charges nothing, like `knows` and `needs`.
                "take" => self.take(rest),
                "drop" => self.drop_carried(rest),
                "put" => self.put_in(rest),
                "carrying" => Turn::Out(self.carrying()),
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
                // The Gallery, Task 5: unlike the four vertical band changes
                // just above, a stairs move BETWEEN RUNGS charges — it
                // reuses the very `Action::MoveWithin` dial
                // `step_underground` already prices one lateral cell with,
                // rather than inventing a new one (spec §3.4 forbids that).
                // One flight of stairs is the same scale of act as one cell,
                // not a whole band's worth of descent the way `dive`/`delve`
                // are, which is what makes reusing that dial the right call
                // rather than the free ride the four above get.
                "down" => self.take_stairs(true),
                "up" => self.take_stairs(false),
                // The Latch, Task 5: the act that clears a barred passage.
                // Gated by the same band guards `delve` carries just above
                // (`clear_passage`'s own doc explains why), so it is dispatched
                // beside it rather than beside `enter`/`out`, whose guards
                // differ.
                "clear" => self.clear_passage(),
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
                    self.step_underground(other)
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
            .water_column_at(hornvale_kernel::Vertex(cw.vertex))
    }

    /// The cave at the vertex the possession stands on, if the terrain places
    /// one there — mirrors `column_here`: both resolve the same fuzzy
    /// corner-weighted vertex under the possession and ask "is there a medium
    /// here to descend into," one for water, one for rock. `None` on a vertex
    /// with no cave, or before terrain built at all.
    ///
    /// Returns the resolved [`hornvale_kernel::Vertex`] alongside the cave
    /// rather than the bare `Cave` `column_here` analogy would suggest:
    /// addressing a chamber (`ChamberAddr`) needs the vertex, where a water
    /// stratum needs no address at all, so the caller needs both.
    fn chamber_column_here(&self) -> Option<(hornvale_kernel::Vertex, hornvale_terrain::Cave)> {
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
        let vertex = hornvale_kernel::Vertex(cw.vertex);
        terrain.cave_at(vertex).map(|cave| (vertex, cave))
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

    /// Descend into the cave at this vertex's entrance chamber (The Deep
    /// Realm, Task 5).
    ///
    /// Mirrors `dive`, but with an extra outcome `dive` never needed — THREE
    /// again as of The Latch, after a stretch as two following The Drift.
    /// `dive`'s own doc warns what happens when a refusal doesn't name what
    /// stopped you: it reads as a parse failure rather than a fact about the
    /// world. So each outcome below is named:
    ///   1. no cave at this vertex at all — say so;
    ///   2. a barred passage — say WHICH barrier turned the body back
    ///      (`barred_refusal`);
    ///   3. an open, unbarred chamber — descend, and say what the rock here
    ///      is.
    ///
    /// # THE CHAMBER-UNREALIZED OUTCOME WAS REMOVED, AND THE CODE STILL
    /// # CARRIES ITS ARM — THIS IS A SEPARATE FACT FROM THE BARRIER GATE
    /// # BELOW, WHICH IS WHAT ACTUALLY RESTORES THE THIRD OUTCOME
    ///
    /// A cave used to be **SEALED** when its own entrance address
    /// (`branch = 0, band = Undercroft, level = 0`) resolved to no chamber —
    /// spec §3.4 rung 0, *"the void exists and is unreachable"*, a real
    /// chamber a later dig could find rather than a defect. The Deep Realm's
    /// Task 3 measured that a cave's entrance resolved to an actual chamber
    /// only **51.5%** of the time, which is the 0.5 per-address existence
    /// coin showing through.
    ///
    /// **The Drift deleted that coin** (spec §4.1), and this chamber-
    /// unrealized outcome is **impossible rather than rare**: every cave in
    /// shape realizes chambers, measured `systems_with_open_mouth ==
    /// systems` on all three panel seeds (874/874, 1681/1681, 1266/1266) and
    /// 0 of 48,316 caves sealed over thirty worlds. Nothing in The Latch
    /// touches `chamber_exists`, so this remains true and the sealed branch
    /// below stays **live code on an unreachable path**, kept deliberately.
    ///
    /// **What actually restores the third outcome is a different gate,
    /// consulted BEFORE this one is ever reached.** `delve_at` now checks
    /// `crate::passage::effective_state` against the address's seeded
    /// `BarrierState` and refuses descent for anything short of `Open` — see
    /// `delve_at`'s own doc comment, and
    /// `delve_has_three_distinguishable_outcomes` (renamed from
    /// `delve_has_two_distinguishable_outcomes`), which reddens the moment
    /// EITHER outcome this comment tracks goes missing.
    fn delve(&mut self) -> Turn {
        if self.inside.is_some() {
            return Turn::Out(NO_ROCK_INSIDE_REFUSAL.to_string());
        }
        if self.underground.is_some() {
            return Turn::Out(ALREADY_BELOW_DELVE_REFUSAL.to_string());
        }
        self.delve_column(self.chamber_column_here())
    }

    /// [`Self::delve`]'s outcome for an ALREADY-RESOLVED column — the
    /// no-cave refusal plus the sealed/open decision below it.
    ///
    /// Split out for the same reason [`Self::delve_at`] was, one level up: the
    /// no-cave branch used to be reachable from a test only by the flagship's
    /// own starting vertex happening to be cave-free, and decision 0134's
    /// terrain epoch put a sealed cave under that vertex and falsified the
    /// contingency. Production still reaches this exactly one way, through
    /// `delve` with `chamber_column_here()`, so nothing about the verb's
    /// behaviour moved.
    fn delve_column(
        &mut self,
        column: Option<(hornvale_kernel::Vertex, hornvale_terrain::Cave)>,
    ) -> Turn {
        let Some((vertex, cave)) = column else {
            return Turn::Out(NO_CAVE_TO_DELVE_REFUSAL.to_string());
        };
        self.delve_at(vertex, cave)
    }

    /// The outcome of delving at a KNOWN vertex and cave — split out of
    /// [`Self::delve`] so the sealed-vs-open decision can be exercised
    /// directly against a hand-picked vertex (this campaign's own unit
    /// coverage) without steering the possession there first. Steering is
    /// impractical to do from a test: `chamber_column_here` resolves the
    /// possession's terrain vertex through the same fuzzy corner-weighted walk-
    /// band lookup `column_here` uses, and a terrain vertex spans many, many
    /// walk-band rooms, so hitting one particular vertex by walking is not
    /// something a test should depend on landing.
    ///
    /// **The parenthesis this used to carry — "let alone one with a SEALED
    /// cave specifically, ~48.5% of caves per Task 3's measurement" — is
    /// dead twice over.** The Drift deleted the existence coin that produced
    /// the 48.5%, so the sealed population is now 0 of 48,316 caves over
    /// thirty worlds; and there is consequently no sealed vertex to steer to at
    /// all. The seam is still worth having for the reason its first sentence
    /// gives, and it is what restricted passage will be tested through.
    fn delve_at(&mut self, vertex: hornvale_kernel::Vertex, cave: hornvale_terrain::Cave) -> Turn {
        let addr = cave_entrance_addr(vertex);
        let overrides = hornvale_worldgen::chamber::ChamberOverrides::new();
        // The chamber lattice is placed by HEAT since `chamber/v2` (spec
        // §4.1), so the same cave reaches a different distance down it
        // depending on the vertex's gradient, and a chamber's stratum is read
        // off that vertex's own column. Both come from the same terrain handle
        // `chamber_column_here` already resolved the cave through, so no
        // second, independently-chosen lookup is introduced here.
        let Some(terrain) = self.wctx.terrain.as_ref() else {
            return Turn::Out(NO_CAVE_TO_DELVE_REFUSAL.to_string());
        };
        // The Latch: a barred passage refuses descent even where the lattice
        // realizes a chamber. This is the first precondition in the tree that
        // reads committed state — the fold consults the session's own ledger
        // for a clearing fact, and falls back to the seeded barrier.
        let barrier = crate::passage::effective_state(
            &self.ledger,
            self.world.seed,
            &addr,
            self.day,
            &hornvale_worldgen::BarrierPins::default(),
        );
        if barrier != hornvale_worldgen::BarrierState::Open {
            return Turn::Out(barred_refusal(barrier));
        }
        let gradient = terrain.geothermal_gradient_at(vertex);
        let column = terrain.column_at(vertex);
        match hornvale_worldgen::chamber::chamber_at(
            self.world.seed,
            &cave,
            gradient,
            &column,
            addr,
            &overrides,
        ) {
            None => Turn::Out(UNREALIZED_CHAMBER_REFUSAL.to_string()),
            Some(chamber) => {
                // The Gallery, Task 3: the entrance chamber above only
                // gates whether this cave mouth leads anywhere at all (the
                // sealed refusal above) — it is not itself where the
                // possession stands. `Underground::enter` builds the real
                // descent and places the possession on the entrance rung's
                // first standable cell, from the SAME terrain handle and
                // vertex the sealed-check above already resolved.
                self.underground = Some(crate::underground::Underground::enter(
                    terrain,
                    vertex,
                    cave,
                    self.world.seed,
                ));
                // Fix round 1: every ARRIVAL marks, not just a lateral step
                // (spec §3.5, amended in commit f6051a9c3) — the entrance
                // cell is the first of the three landing paths, and the one
                // a player sees before ever taking a single step.
                self.mark_underground_seen();
                Turn::Out(format!(
                    "{DESCENT_PREFIX} {}.",
                    stratum_word(chamber.stratum)
                ))
            }
        }
    }

    /// Return to the surface from the chamber lattice — `delve`'s inverse,
    /// mirroring `surface`.
    ///
    /// **Only clears `underground` from the entrance rung (The Gallery,
    /// Task 3).** Before this task the lattice reached only the entrance
    /// address, so any descent climbed out in one step; now a descent has
    /// several rungs, and stairs (Task 5) are the only way down or up
    /// between them. Climbing from a deeper rung refuses and says to take
    /// the stairs up — this task's own scope stops at that refusal; the
    /// stairs verb it names is Task 5's.
    fn climb(&mut self) -> Turn {
        match &self.underground {
            None => {
                return Turn::Out(
                    "You are not underground; there is nothing to climb out of.".to_string(),
                );
            }
            Some(ug) if ug.rung != 0 => {
                return Turn::Out(CLIMB_FROM_DEPTH_REFUSAL.to_string());
            }
            Some(_) => {}
        }
        self.underground = None;
        match self.describe_here() {
            Ok(d) => Turn::Out(format!("You climb back into the light.\n{d}")),
            other => self.out(other),
        }
    }

    /// A compass step UNDERGROUND: one cell of the current rung's real
    /// generated level (The Gallery, Task 4).
    ///
    /// The geometry itself — diagonal refusal, the passability predicate,
    /// and the law that lateral movement never changes band (metaplan
    /// §1b.6) — lives entirely in [`crate::underground::Underground::peek`]/
    /// [`crate::underground::Underground::commit_step`]; this wrapper parses
    /// the bearing, charges the move, and narrates the outcome, the same
    /// division `Self::step` (the indoor precedent, `session.rs:3413`) draws
    /// between its own geometry and this method's `go`/bare-compass
    /// callers.
    ///
    /// **Fix round 1 (review finding 2): charges exactly the way the indoor
    /// step does, and in the same order.** `Self::step`'s own precedent
    /// calls `charge_within_room` AFTER confirming the target is passable
    /// but BEFORE mutating `inside.cell`, so a refused clock (a body too
    /// spent to move) leaves the possession exactly where it stood and
    /// reports the charge's own error as the turn's output. This mirrors
    /// that: `peek` validates and returns the target WITHOUT moving,
    /// `charge_within_room` runs next, and only once that succeeds does
    /// `commit_step` move the possession. No new cost model — the same
    /// `Action::MoveWithin(AnchorId(0))` dial the indoor step already
    /// charges, reused rather than invented.
    ///
    /// **Part 1 of spec §3.2's water rule reads here, not in the geometry.**
    /// `Flooded` is passable (you wade), so the narration — not the
    /// refusal — is where "wet" becomes visible: the verb is `wade` when
    /// [`crate::underworld_level::movement_mode`] answers `Wade` for the
    /// cell just entered, `step` otherwise. Querying the mode is part 3 of
    /// the same rule: one seam answers "how", asked here rather than a
    /// boolean re-derived from the cell kind directly.
    fn step_underground(&mut self, dir: &str) -> Turn {
        let Some(wanted) = parse_compass(dir) else {
            return Turn::Out(format!("Go where? '{dir}' is no direction I know."));
        };
        let Some(ug) = self.underground.as_ref() else {
            // Unreachable through `handle` (every call site guards on
            // `self.underground.is_some()` first), the same shape `step`'s
            // own unreachable guard takes one band over.
            return Turn::Out("error: no cave floor to step across: not below".to_string());
        };
        let target = match ug.peek(wanted) {
            Err(reason) => return Turn::Out(reason.to_string()),
            Ok(target) => target,
        };
        // The charge runs BEFORE the move lands (review finding 2): a
        // refused clock must not move the possession, the same order
        // `Self::step`'s own `charge_within_room` call keeps indoors.
        if let Err(e) = self.charge_within_room() {
            return Turn::Out(e);
        }
        let ug = self
            .underground
            .as_mut()
            .expect("checked Some above; nothing between then and now clears it");
        let outcome = ug.commit_step(target);
        // The Gallery, Task 6: fog marks after the move actually lands, on
        // the path that moved it — never for a step `commit_step` would
        // refuse (it never does; see the unreachable arm just below), and
        // never before `charge_within_room` has already succeeded above.
        if !matches!(outcome, crate::underground::StepOutcome::Blocked(_)) {
            self.mark_underground_seen();
        }
        match outcome {
            crate::underground::StepOutcome::Blocked(reason) => {
                // Unreachable: `commit_step` only ever runs on a target
                // `peek` already validated, and never itself refuses. Kept
                // as a real arm (not `unreachable!()`) so a future change to
                // `commit_step` fails loudly with a message rather than a
                // panic with no context.
                Turn::Out(reason.to_string())
            }
            crate::underground::StepOutcome::Moved => {
                let ug = self.underground.as_ref().expect("just stepped");
                let kind = ug
                    .level()
                    .cells
                    .get(ug.cell)
                    .expect("the possession's own cell is always in the level's extent");
                let mode = crate::underworld_level::movement_mode(kind)
                    .expect("a cell just stepped onto is passable");
                let verb = match mode {
                    crate::underworld_level::MovementMode::Wade => "wade",
                    _ => "step",
                };
                Turn::Out(format!("You {verb} {}.", bearing_word(wanted)))
            }
            crate::underground::StepOutcome::NeedsStairs => {
                let ug = self.underground.as_ref().expect("just stepped");
                let word = match ug.level().cells.get(ug.cell) {
                    Some(crate::underworld_level::LevelCellKind::StairsDown) => "down",
                    Some(crate::underworld_level::LevelCellKind::StairsUp) => "up",
                    _ => "on",
                };
                Turn::Out(format!(
                    "You step {}, onto a stairway leading {word}.",
                    bearing_word(wanted)
                ))
            }
        }
    }

    /// Fold the current cell's shadowcast into the current rung's own
    /// remembered set (The Gallery, Task 6; spec §3.5, amended in Fix round
    /// 1 by commit f6051a9c3: "every arrival marks", not merely a lateral
    /// step). Called from all three ways a possession comes to occupy an
    /// underground cell, each AFTER the possession has actually landed
    /// there — never before, and never for a refused or blocked move, so
    /// what gets remembered is always somewhere the possession genuinely
    /// stood:
    ///
    /// - [`Self::delve_at`], right after `self.underground` is built, for
    ///   the entrance cell.
    /// - [`Self::step_underground`], after [`crate::underground::
    ///   Underground::commit_step`] has moved the possession one cell.
    /// - [`Self::take_stairs`], after [`crate::underground::Underground::
    ///   take_stairs`] has landed the possession on the connecting rung.
    ///
    /// **One shared call rather than three copies of the same logic** — Fix
    /// round 1's own instruction, and the reason a fourth arrival path
    /// (should one ever exist) only has one call site to remember to add.
    ///
    /// **The reach comes from [`Self::sight_reach`], never a literal** —
    /// the same seam `chamber_sources`'s implicit-torch source reads
    /// through, so a future carried-light model changes every call site by
    /// changing one function.
    ///
    /// A no-op if the possession is not underground. A private helper that
    /// assumes its own caller's precondition instead of checking it is a
    /// private helper waiting to be misused by the next one.
    fn mark_underground_seen(&mut self) {
        let reach = self.sight_reach();
        let Some(ug) = self.underground.as_ref() else {
            return;
        };
        let level = ug.level();
        let lit = crate::lattice::shadowcast_with(
            |cell| {
                level
                    .cells
                    .get(cell)
                    .and_then(crate::underworld_level::movement_mode)
                    .is_some()
            },
            |cell| level.extent.contains(cell),
            ug.cell,
            reach,
        );
        let rung = ug.rung;
        self.underground
            .as_mut()
            .expect("checked Some above; nothing between then and now clears it")
            .seen[rung]
            .mark_all(&lit);
    }

    /// Moving between rungs by way of the stairs (The Gallery, Task 5) — the
    /// verb-level wrapper around
    /// [`crate::underground::Underground::peek_stairs`]/
    /// [`crate::underground::Underground::take_stairs`], one level up the
    /// same way [`Self::step_underground`] wraps
    /// [`crate::underground::Underground::peek`]/
    /// [`crate::underground::Underground::commit_step`].
    ///
    /// **New verbs, not a re-point of `delve`/`climb`.** Both already carry
    /// an established meaning at this band boundary — `delve` opens a
    /// descent from the surface, `climb` closes one back to it, with its own
    /// pinned refusals ("You are already below; 'climb' brings you back up
    /// first." — [`Self::delve`]; [`CLIMB_FROM_DEPTH_REFUSAL`], which
    /// already tells the player to "find the stairs up", naming a DIFFERENT
    /// mechanism rather than itself). Re-pointing either would mean
    /// rewriting both of those pinned sentences into stairs-aware dispatch
    /// for no evidence they need to move; two new verbs cost one
    /// [`IN_CHARACTER_VERBS`] array-length bump and two `HELP` lines
    /// instead, with neither existing verb's behaviour touched. See this
    /// task's own report for the fuller argument.
    ///
    /// **The direction is checked against the current cell BEFORE
    /// `peek_stairs` is ever asked**, because `peek_stairs` itself is
    /// direction-agnostic (it reads whichever kind the current cell already
    /// is) — asking it while standing on the WRONG kind of stairs would
    /// silently take a player the opposite way from the word they typed.
    /// Refusing here, with a physical reason naming neither verb, is what
    /// keeps `down` always meaning down.
    ///
    /// **Charges exactly the way [`Self::step_underground`] does, and in the
    /// same order**: `peek_stairs` validates that the move can succeed and
    /// returns WITHOUT moving, `charge_within_room` runs next, and only once
    /// that succeeds does
    /// [`crate::underground::Underground::take_stairs`] itself move the
    /// possession — so a refused clock, or a structurally-impossible move
    /// (the descent's own deepest rung has no further rung to land on),
    /// never moves the possession and never spends a tick. One flight of
    /// stairs is priced the same as one lateral cell
    /// (`Action::MoveWithin(AnchorId(0))`, the same dial
    /// `charge_within_room` already charges) rather than a new cost model —
    /// spec §3.4 forbids minting one, and nothing here does.
    fn take_stairs(&mut self, want_down: bool) -> Turn {
        let Some(ug) = self.underground.as_ref() else {
            return Turn::Out("You are not underground; there are no stairs to take.".to_string());
        };
        let wanted_kind = if want_down {
            crate::underworld_level::LevelCellKind::StairsDown
        } else {
            crate::underworld_level::LevelCellKind::StairsUp
        };
        if ug.level().cells.get(ug.cell) != Some(wanted_kind) {
            return Turn::Out(if want_down {
                "There is no stairway down from here.".to_string()
            } else {
                "There is no stairway up from here.".to_string()
            });
        }
        if let Err(reason) = ug.peek_stairs() {
            return Turn::Out(reason.to_string());
        }
        // The charge runs BEFORE the move lands, the same order
        // `step_underground`'s own `charge_within_room` call keeps: a
        // refused clock must not move the possession.
        if let Err(e) = self.charge_within_room() {
            return Turn::Out(e);
        }
        let ug = self
            .underground
            .as_mut()
            .expect("checked Some above; charge_within_room never touches underground");
        ug.take_stairs()
            .expect("peek_stairs just confirmed this succeeds");
        // Fix round 1: the stairs are the second of the three arrival paths
        // that mark fog (spec §3.5, amended in commit f6051a9c3) — a rung
        // entered and left entirely by stairs must still remember the
        // landing's own surroundings, not just whatever a lateral step
        // happened to add.
        self.mark_underground_seen();
        let word = if want_down { "down" } else { "up" };
        Turn::Out(format!(
            "You take the stairs {word}.\n{}",
            self.describe_underground_here()
        ))
    }

    /// Clear the barred passage at the cave mouth here (The Latch, Task 5) —
    /// the campaign's headline act. Only reachable at a resolvable cave
    /// mouth on the surface, the same footing `delve` requires, so it shares
    /// `delve`'s own two early refusals rather than inventing new prose for
    /// the same two facts about where a body is standing.
    fn clear_passage(&mut self) -> Turn {
        if self.inside.is_some() {
            return Turn::Out(NOTHING_TO_CLEAR_INSIDE_REFUSAL.to_string());
        }
        if self.underground.is_some() {
            return Turn::Out(ALREADY_BELOW_CLEAR_REFUSAL.to_string());
        }
        self.clear_passage_column(self.chamber_column_here())
    }

    /// [`Self::clear_passage`]'s outcome for an ALREADY-RESOLVED column —
    /// split out for the same reason [`Self::delve_column`] is: production
    /// reaches this exactly one way, through `clear_passage` with
    /// `chamber_column_here()`.
    fn clear_passage_column(
        &mut self,
        column: Option<(hornvale_kernel::Vertex, hornvale_terrain::Cave)>,
    ) -> Turn {
        let Some((vertex, _cave)) = column else {
            return Turn::Out(NO_CAVE_MOUTH_TO_CLEAR_REFUSAL.to_string());
        };
        self.clear_passage_at(vertex)
    }

    /// The outcome of clearing at a KNOWN vertex — split out of
    /// [`Self::clear_passage`] for the same reason [`Self::delve_at`] is:
    /// the sealed/warded/thin/open decision can then be exercised directly
    /// against a hand-picked vertex without steering the possession there
    /// first, which `chamber_column_here`'s own doc explains is impractical.
    ///
    /// Reads the barrier through [`crate::passage::effective_state`] — the
    /// SAME fold `delve_at` consults, against the SAME address
    /// ([`cave_entrance_addr`]), so a passage this clears is a passage
    /// `delve_at` then finds open. Writes through
    /// [`crate::passage::set_openness`] only for [`hornvale_worldgen::
    /// BarrierState::Thin`] — [`clear_response`]'s own doc explains why
    /// `Sealed` and `Warded` do not yield to this act.
    ///
    /// **What is written changed with The Chattel, and what is NOT written
    /// changed with it.** The Latch committed a `passage-cleared` fact whose
    /// SUBJECT was the clearing body and whose object carried the address.
    /// The subject is now the cave mouth itself — a promoted
    /// [`crate::thing`] — and the predicate is `openness`, the one a
    /// strongbox already uses. So the body that did the clearing is no longer
    /// recorded anywhere by this act. That is a real loss of information and
    /// it is deliberate: The Latch's own fold never consulted the subject
    /// ("any body's clearing fact opens the passage for everyone"), so
    /// nothing INTERPRETED the subject and no assertion HELD it, and a
    /// predicate about a thing whose subject is a different thing is
    /// precisely what joining the object model removes. Whoever wants "who
    /// opened this" back wants an agentive predicate, not this one's subject
    /// slot.
    ///
    /// **"Write-only" is what this paragraph said first, and it overstated
    /// the case** (fix round 1, m1). `windows/historiography::recount`
    /// iterates `Ledger::facts_about` with no predicate filter, and
    /// `cli/src/repl.rs`'s `why <id>` calls it, so a pre-flip `possess --out`
    /// world reloaded into `repl` DID render the clearing against the body
    /// that did it, under the predicate's registered doc. Unread by any
    /// interpreter is not the same as unreadable by any reader, and the fold
    /// claim — the one that licenses the loss — is the former.
    fn clear_passage_at(&mut self, vertex: hornvale_kernel::Vertex) -> Turn {
        let addr = cave_entrance_addr(vertex);
        let barrier = crate::passage::effective_state(
            &self.ledger,
            self.world.seed,
            &addr,
            self.day,
            &hornvale_worldgen::BarrierPins::default(),
        );
        if barrier == hornvale_worldgen::BarrierState::Thin {
            crate::passage::set_openness(&mut self.ledger, &self.registry, &addr, true, self.day)
                .expect("OPENNESS and instance-of are registered and non-functional");
        }
        Turn::Out(clear_response(barrier))
    }

    /// The chamber rendering while underground (The Deep Realm, Task 5) —
    /// deliberately minimal, in `describe_chamber_here`'s spirit one realm
    /// over: no floor plan or anchor catalogue is drawn, only the footing
    /// underfoot. Read straight off `self.underground`'s own generated
    /// level rather than re-deriving anything — the level is already in
    /// hand, and there is nothing to gain by paying for a second lookup.
    ///
    /// **The Gallery, Task 3: reads the generated level's own cell, not a
    /// stratigraphic `stratum` word.** Before this task `self.underground`
    /// carried the entrance chamber's rock stratum (e.g. "sandstone");
    /// that chamber is now consulted only to gate whether the cave mouth
    /// leads anywhere at all (`delve_at`'s sealed check), and a descent's
    /// rungs are not stratum-addressed the way that single entrance bucket
    /// was. `underground_footing_word` reports the one thing the real
    /// generated level actually says about the cell the possession stands
    /// on: whether it is dry or `Flooded`.
    fn describe_underground_here(&self) -> String {
        let ug = self
            .underground
            .as_ref()
            .expect("guarded by self.underground.is_some() at the call site");
        format!(
            "[underground]\nThe rock here is {}. {}",
            underground_footing_word(ug),
            self.underground_ways_from_cell()
        )
    }

    /// The underworld's own "ways on" report (Fix round 1, review finding
    /// 1): the passable orthogonal neighbours of the cell stood on, in the
    /// same bearing vocabulary `Self::ways_from_cell` (indoors) already
    /// uses, plus `out` whenever the current rung is the entrance rung —
    /// `climb`'s own guard is `ug.rung != 0`, not "stood on the entrance
    /// cell", so `out` is a way on from anywhere on rung 0, not only the
    /// cell `Underground::enter` placed the possession on.
    ///
    /// **Before this fix the sentence was a fixed `"Ways on: out."`**
    /// regardless of what `go` could actually do from here — true only
    /// while `go` refused every lateral bearing (Task 3); Task 4 made `go`
    /// walk the level, which made the fixed sentence a one-turn observable
    /// contradiction the moment it landed (`look` says the only way on is
    /// out, `go n` immediately proves that false).
    fn underground_ways_from_cell(&self) -> String {
        let Some(ug) = self.underground.as_ref() else {
            return String::new();
        };
        let mut open = Vec::new();
        if ug.rung == 0 {
            open.push("out".to_string());
        }
        for wanted in COMPASS_SQUARE {
            let delta = cell_delta(wanted).expect("COMPASS_SQUARE is orthogonal");
            let target = crate::lattice::Cell(ug.cell.0 + delta.0, ug.cell.1 + delta.1);
            if ug
                .level()
                .cells
                .get(target)
                .and_then(crate::underworld_level::movement_mode)
                .is_some()
            {
                open.push(bearing_letter(wanted));
            }
        }
        if open.is_empty() {
            "No way on.".to_string()
        } else {
            format!("Ways on: {}.", open.join(", "))
        }
    }

    /// The underworld's examinable catalog. The band has its own because you
    /// cannot see the forest from inside the rock — resolving an underground
    /// `examine` against the surface locale's nouns is the defect this fixes
    /// (The Handle, Task 4).
    ///
    /// See [`Self::describe_underground_here`]'s doc for why this reads the
    /// cell's footing rather than a stratum word (The Gallery, Task 3).
    ///
    /// **Gains a third entry for the chamber's derived resident (Fix round
    /// 1, spec §3.6/§4.1.2).** Task 11 drew the resident on the pane
    /// (`Self::underground_level`) but left this catalog untouched, which is
    /// exactly the "prose contradicting behaviour a player can observe in
    /// one turn" shape this campaign's Task 4 already fixed once for
    /// `look`/`go`: a creature on the plate that `examine` calls nonexistent
    /// teaches a player the game is lying, with no way to tell which half.
    /// [`Self::underground_resident`] is the one predicate both this method
    /// and the pane read, so an unlit-but-remembered resident is absent from
    /// BOTH or neither — never examinable without being drawn, and never
    /// drawn without being examinable.
    fn underground_nouns(&self) -> Vec<crate::focalize::Noun> {
        let ug = self
            .underground
            .as_ref()
            .expect("guarded by self.underground.is_some() at the call site");
        let footing = underground_footing_word(ug);
        let mut nouns = vec![
            crate::focalize::Noun::new("the rock", "rock", &format!("The rock here is {footing}.")),
            crate::focalize::Noun::new(
                footing,
                footing,
                &format!("{footing} rock — the footing of this passage."),
            ),
        ];
        if let Some((kind, source, _cell)) = self.underground_resident(ug) {
            nouns.push(
                crate::focalize::Noun::new(
                    kind.0,
                    kind.0,
                    &crate::underground::inhabitant_datum(kind, source),
                )
                .with_kind(crate::focalize::NounKind::Creature),
            );
        }
        nouns
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
        // not down among whatever lives on the floor. Rendering the room's own
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
        // laterals do not reach a submerged room at all. Claiming "no
        // direction here is closed" there would be false the instant the
        // player tried one, which is exactly the class of defect decision
        // 0141 exists to remove. `Ways on: surface.` is fixed for the same
        // reason `describe_underground_here`'s report used to be, before Fix
        // round 1 gave it real geometry to report on: the submerged band
        // has no lateral mesh at all, so `surface` really is the one way on
        // this band actually leads anywhere. Underground no longer shares
        // that excuse — it has real cells now, and `underground_ways_from_
        // cell` reports them.
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
            &crate::depth::truncate_to_walk(&self.position(), self.walk_depth()),
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
        crate::depth::truncate_to_walk(&structure.threshold, self.walk_depth())
            .seed(self.world.seed)
    }

    /// The ground the building the possession stands in is built from (The
    /// Lantern, spec §3), or `None` above the canonical grid.
    ///
    /// **One context for the whole structure**: a building sits on one vertex of
    /// the geosphere, so its stone comes from one bedrock however many chambers
    /// it has.
    ///
    /// The vertex is `brief::containing_vertex`'s — greatest blend weight, tie-broken
    /// to the lowest `Vertex` — which is the SAME rule `brief_of` selects the
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
        // before its own `containing_vertex` call and two readings of one vertex
        // that agree only by accident are what this method exists not to be.
        let locale = crate::depth::truncate_to_walk(&self.position(), self.walk_depth());
        let vertex = crate::brief::containing_vertex(
            &locale,
            self.wctx.ctx.climate().geosphere(),
            self.wctx.ctx.nearest_index(),
        )?;
        Some(crate::fabric::FabricContext::at(
            self.wctx.ctx.terrain(),
            self.wctx.ctx.climate(),
            vertex,
        ))
    }

    /// The possession's own reach — how far it can see, and how far its
    /// implicit torch throws light (The Gallery, Task 6; spec §3.4's seam:
    /// "one named function answers 'how', rather than a constant or a
    /// boolean re-derived at every call site"). Today this simply returns
    /// [`SIGHT_RADIUS`]; a future carried-light model replaces this body
    /// alone, with every caller already reading through it rather than a
    /// second copy of the number — `chamber_sources`'s implicit-torch
    /// [`crate::light::Source`] above, and the underground fog mark
    /// ([`Self::mark_underground_seen`]) below.
    fn sight_reach(&self) -> i32 {
        SIGHT_RADIUS
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
            // The Gallery, Task 6: this is THE body's own reach, read
            // through `sight_reach()` rather than `SIGHT_RADIUS` directly —
            // the seam a future carried-light model fills. The hearth and
            // doorway sources below keep `SIGHT_RADIUS` on purpose: they are
            // the FIRE's and the OPENING's own reach, not the body's, so
            // routing them through the same seam would mean picking up a
            // lantern brightens every hearth and doorway in the building.
            radius: self.sight_reach(),
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

    /// The chamber's derived resident, if it is visible RIGHT NOW — present
    /// (`crate::underground::chamber_resident`, spec §3.6) AND standing on
    /// a cell the CURRENT shadowcast actually lights
    /// (`crate::underground::resident_cell`), never merely remembered.
    ///
    /// **The single source both [`Self::underground_level`] (the pane's own
    /// mark) and [`Self::underground_nouns`] (`examine`'s own catalog)
    /// read** (Fix round 1) — a creature the pane draws that `examine` says
    /// does not exist is this campaign's signature defect shape (Task 4 hit
    /// the identical thing for `look`/`go`), and the fix is not "teach
    /// `examine` the same rule," which two independent copies of a fog rule
    /// will eventually disagree about, but "give both callers one rule to
    /// read." Recomputing the shadowcast here rather than threading it in
    /// from a caller matches this module's own existing precedent
    /// ([`Self::mark_underground_seen`] and [`Self::underground_level`]
    /// already each compute it independently); a third independent
    /// computation of the SAME shadowcast predicate is consistent with that
    /// shape, while a third independent computation of the RESIDENT
    /// visibility rule is exactly what this method exists to prevent.
    fn underground_resident(
        &self,
        ug: &crate::underground::Underground,
    ) -> Option<(
        hornvale_kernel::KindId,
        hornvale_worldgen::energy::EnergySource,
        crate::lattice::Cell,
    )> {
        let terrain = self.wctx.terrain.as_ref()?;
        let climate = self.wctx.climate.as_ref()?;
        let (kind, source) = crate::underground::chamber_resident(ug, terrain, climate)?;
        let level = ug.level();
        let cell = crate::underground::resident_cell(level)?;
        let lit = crate::lattice::shadowcast_with(
            |c| {
                level
                    .cells
                    .get(c)
                    .and_then(crate::underworld_level::movement_mode)
                    .is_some()
            },
            |c| level.extent.contains(c),
            ug.cell,
            self.sight_reach(),
        );
        lit.contains(&cell).then_some((kind, source, cell))
    }

    /// The underground band's own floor plan (The Gallery, Task 7; spec §4)
    /// — [`Self::chamber_plan`] one band down, minus the colour seam: a
    /// level carries an explicit visibility STATE per cell, never a shade
    /// (spec §4.1), so there is no `Shading` to resolve and no observer to
    /// ask for.
    ///
    /// The shadowcast computed here is the SAME predicate
    /// [`Self::mark_underground_seen`] folds into `ug.seen` on every
    /// arrival: this method never mutates that bitset, only reads it (via
    /// [`crate::underground::SeenBits::saw`]), so a cell this call marks
    /// `"lit"` is always already `"remembered"` too by the time this runs —
    /// nothing here can show the possession a cell its own arrival did not
    /// already commit to memory.
    ///
    /// **Since The Gallery, Task 11 (spec §3.6), `marks` may grow a chamber
    /// resident here even when the caller passed an empty `Vec`.** The
    /// resident is derived fresh from `ug`'s own substrate and energy
    /// (`crate::underground::chamber_resident`) whenever `self.wctx.terrain`
    /// and `self.wctx.climate` are both available, and appears only if its
    /// fixed cell (`crate::underground::resident_cell`) is genuinely `lit`
    /// this turn — a chamber that is out of sight shows nobody, exactly as
    /// the chamber band's own NPC marks already require.
    fn underground_level(
        &self,
        ug: &crate::underground::Underground,
        mut marks: Vec<crate::plan::PlanMark>,
    ) -> crate::level_doc::SessionLevel {
        let level = ug.level();
        let lit = crate::lattice::shadowcast_with(
            |cell| {
                level
                    .cells
                    .get(cell)
                    .and_then(crate::underworld_level::movement_mode)
                    .is_some()
            },
            |cell| level.extent.contains(cell),
            ug.cell,
            self.sight_reach(),
        );
        // The Gallery, Task 11 (spec §3.6): who lives here is derived from
        // THIS chamber's own substrate and energy, never a spawn table and
        // never the surface roster. Fix round 1: this reads
        // `Self::underground_resident` rather than re-deriving the
        // present/lit test inline, so the pane's own mark and `examine`'s
        // catalog (`Self::underground_nouns`) cannot independently drift on
        // which cell counts as lit.
        if let Some((kind, source, cell)) = self.underground_resident(ug) {
            marks.push(crate::plan::PlanMark {
                x: cell.0,
                y: cell.1,
                noun: kind.0.to_string(),
                kind: crate::purview::AGENT_MARK_KIND.to_string(),
                datum: crate::underground::inhabitant_datum(kind, source),
                salience: crate::purview::AGENT_SALIENCE,
            });
        }
        crate::level_doc::level_of(
            level,
            ug.rung_band(),
            ug.depths_m[ug.rung],
            ug.cell,
            &lit,
            |c| ug.seen[ug.rung].saw(c),
            marks,
        )
    }

    /// `map` underground (The Gallery, Task 8; spec §5) — the verb's own
    /// answer, one band down, drawn from the exact `SessionLevel` document
    /// the pane emits ([`Self::underground_level`]). The pane and the verb
    /// read one source now, which is what keeps them from drifting apart
    /// the way the retired fold worried they might (see this module's
    /// `the_pane_and_the_verb_agree_underground`, formerly
    /// `the_underground_band_folds_into_walk_as_map_does`).
    ///
    /// Fog-of-war-respecting by construction, not by a filter added here:
    /// `doc.cells` already omits every never-seen cell (spec §4.1.1), so a
    /// cell this picture never marks is simply left blank rather than
    /// checked and skipped.
    ///
    /// No `eyes` parameter and no lens, unlike [`Self::plan_here`]:
    /// `vessel/level/v1` carries an explicit visibility STATE per cell,
    /// never a shade (`level_doc`'s own module doc, spec §4.1), so there is
    /// no colour here for a lens to filter.
    ///
    /// **This picture never draws a creature mark, and that stays true after
    /// The Gallery, Task 11** — not because `doc.marks` is empty (as of
    /// Task 11 it may carry a derived chamber resident, exactly as the pane
    /// does: [`Self::underground_level`] builds it whether this verb's own
    /// `Vec::new()` argument is empty or not), but because the render loop
    /// below only ever walks `doc.cells`, `doc.palette` and `doc.you` — it
    /// never reads `doc.marks` at all. [`Self::plan_here`] draws the same
    /// way, for the same reason (it too always passes `Vec::new()` for
    /// `marks`, and its own picture ignores whatever a plan's `marks` would
    /// have carried).
    fn level_here(&self) -> Result<String, VesselError> {
        let Some(ug) = self.underground.as_ref() else {
            // Unreachable through `handle` (the arm checks first), the same
            // guard and the same reason `plan_here` gives for its own.
            return Err(VesselError::Build(
                "no level to draw: the possession is not underground".to_string(),
            ));
        };
        let doc = self.underground_level(ug, Vec::new());
        let e = doc.extent;
        let mut rows = vec![vec![' '; e.w as usize]; e.h as usize];
        for cell in &doc.cells {
            let glyph = level_kind_glyph(&doc.palette[cell.ix as usize].kind);
            rows[(cell.y - e.y) as usize][(cell.x - e.x) as usize] = glyph;
        }
        rows[(doc.you.y - e.y) as usize][(doc.you.x - e.x) as usize] = '@';
        let picture = rows
            .into_iter()
            .map(|r| r.into_iter().collect::<String>())
            .collect::<Vec<_>>()
            .join("\n");
        Ok(format!(
            "[level: rung {} of {}, {} — {:.1} m]\n{}\n  legend: # wall, . floor, ~ flooded, \
             > stairs down, < stairs up, @ you",
            ug.rung + 1,
            ug.descent.len(),
            doc.rung,
            doc.depth_m,
            picture
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
    /// records `(Facet, AnchorId)` — the anchor a creature stands at in its
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
                let thing_kind_here = crate::affordance::thing_kind_of(kind);
                if crate::chamber_prose::noun(kind).is_some_and(|n| n.to_lowercase() == wanted) {
                    // The Offer, Task 7 (spec §3.5/§4): routed through the
                    // derived offer query, not the anchor's bare kind, so
                    // the knowledge gate is LIVE CODE with an unreachable
                    // branch (Nathan's #9 ruling) rather than dead code.
                    // `Examine` is universal — `required_properties` is the
                    // empty set, which is a subset of any kind's properties
                    // — so the ONLY way this can ever be denied is
                    // `offered_to_observer`'s own knowledge check. Per Task
                    // 4's finding, no live `Session` can fail it today (this
                    // method's caller always absorbs the current room before
                    // the first turn runs), so this is byte-identical to the
                    // pre-Task-7 behaviour for every reachable input —
                    // `examine_chamber_anchor_reply_is_pinned_before_the_
                    // offer_gate` pins exactly that, and `examine_chamber_
                    // anchor_is_refused_when_the_observer_has_no_recorded_
                    // knowledge` proves the branch is real by manufacturing
                    // the one `Knowledge` state a live session cannot reach
                    // on its own.
                    //
                    // The Chattel, Task 9 (spec §3.6, decision 0397): the
                    // `thing_kind_of` conversion is HERE now, not inside
                    // `offered_to_observer`, which is keyed on `KindId`.
                    // `kind` is an `AnchorKind` read off this chamber's own
                    // anchor, so the conversion sits where the anchor is;
                    // the offer this consults is unchanged, because
                    // `thing_kind_of` is exactly what the query used to
                    // apply to the same value one frame in.
                    if !crate::affordance::offered_to_observer(
                        crate::affordance::thing_kind_of(kind),
                        self.driven_body(),
                        &self.knowledge,
                    )
                    .contains(&crate::affordance::OfferedVerb::Examine)
                    {
                        return format!("You see no {noun} here.");
                    }
                    // The Offer, Task 6 (spec §3.6, amended): what lies
                    // `within` an `Encloses` anchor is read here, not just
                    // its authored `detail` line — see `examine_detail`'s
                    // own doc for the property gate and the mutation it
                    // guards against.
                    // The Chattel, Task 11: the "or transparent" arm of the
                    // interactive-fiction rule goes live. `examine_detail`
                    // holds no ledger and no day, so the openness fold is
                    // asked HERE and handed in — see its doc for the two-arm
                    // rule and for why a container nobody has touched reads
                    // shut. Ignored for a kind carrying no `Openable`.
                    let opened = self
                        .chamber_facet_here()
                        .is_some_and(|room| self.container_is_open(&room, thing_kind_here));
                    return crate::chamber_prose::examine_detail(&interior, id, opened);
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
        let before: Vec<Facet> = other_bodies(&self.bodies, self.driven)
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
            day_ticks: self.day_ticks(),
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
        // own doc says why it is not folded into `sys.npcs` above).
        //
        // **Which controller answers now depends on possession (The
        // Coercion, Task 4 fix round).** Free, it is asked through a FRESH
        // `PlayerController` — nothing queues an action on it yet, so its
        // intent is unconditionally `Hold`, exactly as before this task.
        // Possessed, it is asked through an `ImposedController` instead —
        // semantically correct (a held body should not be asked what the
        // PLAYER wants), and a REAL swap: `ImposedController::intend`
        // delegates to `DefaultController`, which returns
        // `resolution.intent` unchanged rather than forcing `Hold`, so a
        // possessed body's solo walk actually ACTS on its own arbitration
        // during `wait` (moves, drinks, rests, eats) instead of sitting
        // frozen. Constructed fresh every call, same as `PlayerController`
        // always was — there is no controller state to carry between ticks
        // for either.
        //
        // **What actually keeps the LEDGER clean either way is the next
        // line, not which controller answered (fix round 3, N4, reconfirmed
        // by this task): `_driven_facts` is discarded UNCONDITIONALLY,
        // regardless of what `step_one_with_controller` returns.** An
        // earlier version of this comment claimed this was "the commits on
        // `Do`, nothing on `Hold` argument spec §5.2 makes" — checked
        // directly (fix round 2) and that claim is false: forcing the
        // intent to `Do` here still leaves the ledger untouched, because the
        // facts never reach `tick()` either way. The player's verbs (`go`,
        // `drink`, …) are what the body DOES; this walk only ever supplies
        // what the host WANTS (`self.driven_mode`) — spec §5.2 is being
        // corrected at Task 8 to say so.
        //
        // **The ledger is inert to this swap; `driven_mode`/`driven_affect`/
        // `driven_suppressed` are NOT (The Coercion, Task 4 fix round,
        // checked directly rather than assumed).** Those three are read
        // back from `st.mode`/`st.affect`/`st.suppressed` on the LAST
        // `advance_one` iteration of this call, and while each iteration
        // sets them from that iteration's OWN `resolution` — before
        // `controller.intend` is even invoked, so intent cannot change what
        // a single iteration reports — a multi-iteration `wait` (`from` to
        // `to` spans more than one decision point) lets an ACTING
        // controller's intent move `st.pos` between iterations, which
        // changes what the NEXT iteration's `resolution` is a resolution
        // OF. `Hold` never moves `st.pos` (`HoldStep` only ever advances
        // `st.day`), so under `PlayerController` every iteration re-judges
        // the same frozen position and this was never observable; under
        // `ImposedController` the body can walk to water and drink mid-wait,
        // which can leave it in a calmer felt state than a position-frozen
        // walk would have reported. See
        // `driven_felt_state_can_move_under_an_imposed_controller_during_wait`
        // for a direct, seed-42 demonstration — this is a real behavioural
        // consequence for `!ask`'s narration while possessed, not merely an
        // internal bookkeeping detail, even though no committed fact ever
        // differs.
        //
        // Cloned out of `self.bodies` first: `driven_body()` borrows all of
        // `self`, which cannot coexist with the `&mut self.mesh_memo`/`&mut
        // self.home_nav_cache` borrows this call needs.
        let driven_npc = self.driven_body().clone();
        let mut player_controller = PlayerController::new();
        let mut imposed_controller = ImposedController::new();
        let driven_controller: &mut dyn Controller = if self.possessor().is_some() {
            &mut imposed_controller
        } else {
            &mut player_controller
        };
        let (_driven_facts, driven_mode, driven_affect, driven_suppressed) = sys
            .step_one_with_controller(
                &self.ledger,
                &driven_npc,
                &mut self.mesh_memo,
                &mut self.home_nav_cache,
                driven_controller,
            );
        self.driven_mode = Some(driven_mode);
        self.driven_affect = Some(driven_affect);
        self.driven_suppressed = driven_suppressed;
        for drive in &self.driven_suppressed {
            *self.driven_overrides.entry(*drive).or_insert(0) += 1;
        }
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
        before: &[Facet],
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
    ///
    /// **The Offer, Task 7 (spec §4): the "chart legend" surface named
    /// there, investigated and found NOT re-pointable at
    /// [`crate::affordance::offered_to_observer`] without either a
    /// structural violation or a scope-widening feature addition.** The
    /// second matcher below (`scene.legend`, this method's own doc's "the
    /// chart's legend") synthesizes `Noun`s from `hornvale_scene::
    /// SurroundsScene` — WALK-band terrain marks (biome regions, sky), never
    /// a chamber `AnchorKind`. Worse, this method cannot even be reached
    /// while the possession is indoors: it calls [`Self::purview`] →
    /// [`Self::purview_through`], whose own `debug_assert!` requires
    /// `self.inside.is_none()`, because the walk-band chart marks every
    /// derived NPC ungated and drawing it from inside a chamber would
    /// disclose a creature sight withheld (see that assertion's doc).
    /// Routing chamber anchors through here would mean lifting that
    /// invariant, which is exactly the class of behaviour change spec
    /// §10.1 and this task's brief say to stop for rather than push
    /// through. See the Task 7 report for the full investigation.
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

    /// Opens an imposed possession of the driven body (The Coercion, Task 4):
    /// the out-of-character seam standing in for the creature capability spec
    /// §5 defers to a species-domain campaign. No creature can choose to do
    /// this yet, so the operator instrument does — the same shape
    /// `act_on_disposition` already established for an act no world system
    /// can currently cause on its own.
    ///
    /// The holder named in the committed [`POSSESSED_BY`] fact is the first
    /// other derived body in the roster (`other_bodies`), chosen
    /// deterministically because nothing yet exists that could choose one
    /// itself. A world with no other derived body has no entity to name, so
    /// this refuses rather than fabricating one or panicking (decision 0007).
    fn possess(&mut self) -> Turn {
        let Some(holder) = other_bodies(&self.bodies, self.driven).first().copied() else {
            return Turn::Out("There is no other will in this world to take you.".to_string());
        };
        let holder_entity = holder.entity;
        let holder_label = holder.label.clone();
        let body = self.agent_entity();
        // The provenance carries `self.turn` (incremented once per non-empty
        // `handle` call, including this one — see `Session::handle`) so a
        // same-day possess -> unpossess -> possess sequence never commits two
        // BYTE-IDENTICAL `possessed-by` facts. `Ledger::commit`'s idempotent
        // dedup compares the whole envelope including provenance (Task 1's own
        // finding), and `day` alone does not vary within a day — a static
        // provenance string here would make the second `possess` in such a
        // sequence a silent no-op: `possessor()` would read `None` right after
        // a verb that reported success. See
        // `reopening_a_possession_on_the_same_day_is_not_a_silent_no_op`.
        let fact = Fact {
            subject: body,
            predicate: POSSESSED_BY.to_string(),
            object: Value::Entity(holder_entity),
            place: None,
            day: Some(self.day),
            provenance: format!("player: possess (turn {})", self.turn),
        };
        self.ledger
            .commit(fact, &self.registry)
            .expect("possessed-by is registered and non-functional");
        Turn::Out(format!(
            "Another will settles into you — {holder_label} holds this body now. Your own \
             acts refuse; only '!' verbs still answer."
        ))
    }

    /// Closes an imposed possession at the possessor's own option (The
    /// Coercion, Task 4, spec §6: "release is reachable"). Idempotent by a
    /// guard on [`Self::possessor`], not on `Ledger::commit`'s own dedup —
    /// mirrors `TURNED_HOSTILE`'s pattern (`Ledger::value_of`), adapted
    /// because `POSSESSED_BY`/[`POSSESSION_ENDED`] are non-functional, so
    /// `value_of`'s single-latest-fact read is the wrong query here; the
    /// live state is [`possessor_of`]'s open/close fold, which
    /// [`Self::possessor`] already exposes.
    fn unpossess(&mut self) -> Turn {
        if self.possessor().is_none() {
            return Turn::Out("No other will holds this body.".to_string());
        }
        let body = self.agent_entity();
        let fact = Fact {
            subject: body,
            predicate: POSSESSION_ENDED.to_string(),
            object: Value::Text("released".to_string()),
            place: None,
            day: Some(self.day),
            provenance: format!("player: unpossess (turn {})", self.turn),
        };
        self.ledger
            .commit(fact, &self.registry)
            .expect("possession-ended is registered and non-functional");
        Turn::Out("The will withdraws. You are your own again.".to_string())
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

    /// Ask the possessed body how it feels (The Confidant, Task 6): the
    /// pipeline's visible end. `Self::driven_affect` is the arbitration's
    /// TRUE answer (Task 2); this asks the driven body's own culture
    /// (`hornvale_worldgen::lexicon_from_in`) what it can say about that
    /// state at all (`crate::testimony::testify`, Task 4/4b) — its own word
    /// if the lexicon has one, the nearest state it CAN name otherwise, or
    /// nothing if it has no felt-state word whatsoever. Only the answer
    /// reaches the player; the arbitration itself never does
    /// ([`render_testimony`] holds that invariant, not this method).
    ///
    /// Lands the reported concept in `self.knowledge` under the SAME
    /// `"{subject}::{predicate}"` heard shape [`absorb_common`] writes
    /// (`windows/vessel/src/knowledge.rs`'s own contract: heard is not
    /// verified, and a listener may already hold a false belief there — this
    /// adds no second store and no truth flag). A body with nothing to say
    /// (no `!wait` yet, or a lexicon with no felt-state word at all) lands
    /// nothing.
    ///
    /// **The tongue decision (spec §5.3), made rather than deferred:**
    /// `absorb_common` parses Common, and Common has no speakers at all
    /// (`hornvale_language::common_vocab`'s own doc: "the author's register,
    /// not a people's tongue") — so "scope to hosts speaking Common" would
    /// scope to the empty set; no host of any species ever qualifies. The
    /// campaign is instead scoped the OTHER way: the word actually spoken
    /// (`FeltStateWord`'s `WordViews.roman`, a real conlang string) is
    /// untranslatable by construction and is used for display ONLY, never
    /// landed in `self.knowledge`. What lands is the CONCEPT the utterance
    /// reports (`testimony::concept_id`), the same short id
    /// `hornvale_language`'s Common vocabulary already derives a word from
    /// for every registered concept (a total map — no gap is possible on
    /// this half). That keeps a later `misreport_distance` reading heard
    /// knowledge comparing concept ids to concept ids, never a concept id to
    /// an untranslated foreign string — the exact conflation spec §5.3 warns
    /// against.
    fn ask(&mut self) -> String {
        let Some(label) = self.driven_affect() else {
            return "It has not settled into anything yet; wait, then ask.".to_string();
        };
        let lexicon = match (
            self.wctx.terrain.as_ref(),
            self.wctx.climate.as_ref(),
            self.wctx.wc.as_ref(),
        ) {
            (Some(terrain), Some(climate), Some(wc)) => hornvale_worldgen::lexicon_from_in(
                self.world,
                wc,
                &self.driven_body().species,
                terrain,
                climate,
            )
            .ok(),
            _ => None,
        };
        let testimony = match lexicon {
            Some(lexicon) => {
                // The Reticence: the host's willingness, between arbitration and
                // the lexicon. The drive the answer is ABOUT is the pursued one —
                // the affect's own object — so a host goes quiet on the axis it
                // was overridden on, not globally.
                let topic = self.driven_affect_object();
                let prior = {
                    let name = crate::doctrine::improvised_name(
                        self.world,
                        &lexicon,
                        &self.driven_body().species,
                    );
                    crate::doctrine::openness(&name)
                };
                let overrides = topic.map(|d| self.overrides_of(d)).unwrap_or(0);
                let stance = crate::stance::stance_for(prior, overrides);
                let residue: Vec<_> = self.suppressed_drives().to_vec();
                testify_with_stance(&lexicon, label, stance, &residue)
            }
            None => None,
        };
        let body_label = self.driven_body().label.clone();
        let (turn, heard_value) = render_testimony(&body_label, label, testimony);
        if let Some(value) = heard_value {
            self.knowledge
                .0
                .insert(format!("{body_label}::feels"), value);
        }
        turn
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
        let at = hornvale_astronomy::StdInstant::new(self.day.as_std_days())
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

/// The pure rendering half of `ask` (The Confidant, Task 6; widened to the
/// four-arm [`Testimony`] at The Reticence, Task 5), split out of
/// [`Session::ask`] so it can be pinned directly against hand-built
/// [`Testimony`] values rather than only through a real, world-generated
/// culture that may or may not exercise the divergent arm — the same
/// rationale `windows/vessel/tests/suite/testimony.rs`'s own doc gives for
/// hand-supplying `ExposureClass`es instead of a real exposure pipeline.
///
/// Returns the player-facing turn text and, when the body said anything at
/// all, the concept id to land in `Knowledge` under `"{body_label}::feels"`
/// (`Session::ask` does the landing; this function only decides what to
/// land). [`Testimony::Withheld`] lands nothing — a refusal that recorded a
/// felt state would make silence informative.
///
/// **THE INVARIANT THIS FUNCTION EXISTS TO HOLD (Task 6 Step 5, extended by
/// The Reticence Task 5):** `label` — the arbitration's TRUE answer — is
/// read only to gloss the [`Direct`] arm (via [`render_felt_state_word`]),
/// where reporting it is CORRECT (a culture that has the word for its own
/// true state is, truthfully, using it). The [`Nearest`] arm never reads
/// `label` at all — only `reported_as`, which [`testify`](crate::testimony::testify) guarantees differs
/// from whatever it was asked about (`nearest` skips its own query
/// candidate) — so a divergent testimony can never carry the true label into
/// the returned text. This module's OWN test
/// `the_arbitration_never_reaches_a_divergent_utterance` (in the `tests`
/// module at the foot of this file — NOT in `tests/suite/ask_verb.rs`, which
/// an earlier version of this doc misnamed) mutation-proves it by
/// substituting `label` for `reported_as` in that arm and watching the test
/// catch it. [`Testimony::Falsehood`] holds the same discipline for a
/// deliberate lie — it glosses from `claimed`, never `label` — and
/// `a_deliberate_lie_carries_the_claim_and_never_the_truth` beside it
/// mutation-proves THAT arm the same way.
///
/// **All four arms are pinned in-module, and the fourth was not always.**
/// `Withheld`, `Falsehood` and `Costly` shipped with no test constructing
/// them at all; the `Falsehood` mutation above passed the entire 653-test
/// vessel crate, silently reporting a lying host's true state. The three
/// tests that close that gap sit beside the two named above.
///
/// [`Direct`]: FeltStateWord::Direct
/// [`Nearest`]: FeltStateWord::Nearest
fn render_testimony(
    body_label: &str,
    label: AffectLabel,
    testimony: Option<Testimony>,
) -> (String, Option<String>) {
    match testimony {
        None => (
            format!("{body_label} has no word for how it feels, and says nothing at all."),
            None,
        ),
        Some(Testimony::Withheld) => (format!("{body_label} will not say how it feels."), None),
        Some(Testimony::Spoken(word)) => render_felt_state_word(body_label, label, word),
        Some(Testimony::Falsehood { word, claimed }) => {
            // The Reticence, Task 5: the rendered gloss and the heard value
            // both come from `claimed`, the state the word actually names —
            // never `label`, the arbitration's true answer. That is the same
            // discipline the `Nearest` arm below already holds for a lexical
            // gap; here it holds for a deliberate lie instead.
            let concept = crate::testimony::concept_id(claimed);
            (
                format!("{body_label} says, \"{}\": {concept}.", word.roman),
                Some(concept.to_string()),
            )
        }
        Some(Testimony::Costly { word, revealed }) => {
            let (line, heard) = render_felt_state_word(body_label, label, word);
            if revealed.is_empty() {
                (line, heard)
            } else {
                let residue = revealed
                    .iter()
                    .map(|drive| format!("{drive:?}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                (
                    format!("{line} It costs something to say: {residue}."),
                    heard,
                )
            }
        }
    }
}

/// The ordinary lexical report shared by [`Testimony::Spoken`] and
/// [`Testimony::Costly`] (The Reticence, Task 5) — split out of
/// `render_testimony` so both arms hold the SAME true-label discipline
/// [`render_testimony`]'s own doc invariant states: `label` glosses only the
/// [`FeltStateWord::Direct`] arm, and [`FeltStateWord::Nearest`] never reads
/// it at all.
fn render_felt_state_word(
    body_label: &str,
    label: AffectLabel,
    word: FeltStateWord,
) -> (String, Option<String>) {
    match word {
        FeltStateWord::Direct(word) => {
            let concept = crate::testimony::concept_id(label);
            (
                format!("{body_label} says, \"{}\": {concept}.", word.roman),
                Some(concept.to_string()),
            )
        }
        FeltStateWord::Nearest {
            word, reported_as, ..
        } => {
            let concept = crate::testimony::concept_id(reported_as);
            (
                format!(
                    "{body_label} says, \"{}\": {concept}, near enough.",
                    word.roman
                ),
                Some(concept.to_string()),
            )
        }
    }
}

/// A chamber's packed room id, for the blocks that print one.
///
/// One place rather than two: `FacetError` implements `Debug` but not
/// `Display` (the constraint `snapshot` documents at its own `pack` call), so the
/// mapping has a shape worth stating once — and the chamber block and the plan
/// block must print the same id for the same chamber.
fn chamber_id(chamber: &Facet) -> Result<u64, VesselError> {
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

/// The reader-facing word for the footing under the possession's feet
/// underground (The Gallery, Task 3) — whether `ug`'s current cell is
/// `Flooded` or dry `Floor`. `describe_underground_here` and
/// `underground_nouns` share this rather than each reading `ug.level()`
/// and matching on the cell kind separately.
///
/// Matches on anything other than `Flooded` as dry rather than listing
/// `Floor` alone: `Underground::enter`'s own invariant guarantees the
/// possession's cell is `Floor` or `Flooded` at the moment a descent
/// begins, and a later task's stairs (Task 5) or fog (Task 6) adding a
/// third live kind at this position should read as dry footing, not panic
/// a description verb that has nothing to do with either.
/// type-audit: bare-ok(prose: return)
fn underground_footing_word(ug: &crate::underground::Underground) -> &'static str {
    match ug.level().cells.get(ug.cell) {
        Some(crate::underworld_level::LevelCellKind::Flooded) => "flooded",
        _ => "dry",
    }
}

/// The glyph [`Session::level_here`] draws for one `LevelPaletteEntry::kind`
/// wire string. Chosen fresh for this verb's own text render: `level_doc`'s
/// own module doc is explicit that the wire `kind` field is a name, never a
/// glyph (decision 0022 leaves glyph selection to the client), so this
/// mapping is `map`'s own and binds nothing on the wire.
///
/// `_ => '?'` is unreachable for any kind [`crate::level_doc::level_of`]
/// actually emits — every [`crate::underworld_level::LevelCellKind`]
/// variant has a named arm above it — kept rather than a hard panic because
/// a display glyph is not worth crashing a player's turn over should a
/// future variant land here first.
fn level_kind_glyph(kind: &str) -> char {
    match kind {
        "wall" => '#',
        "floor" => '.',
        "flooded" => '~',
        "stairs_down" => '>',
        "stairs_up" => '<',
        _ => '?',
    }
}

/// The chamber address a cave's entrance names, for whichever verb needs to
/// key a barrier read or a clearing fact against it (The Latch). Shared by
/// [`Session::delve_at`] and [`Session::clear_passage_at`] so the two can
/// never independently drift on what address one vertex resolves to — a
/// drift here is silent (`addr_key`'s injectivity guarantee only holds
/// between addresses that are actually equal), and would mean a passage this
/// campaign clears never opens for the verb that reads it, or opens a
/// passage nothing barred.
fn cave_entrance_addr(vertex: hornvale_kernel::Vertex) -> hornvale_worldgen::chamber::ChamberAddr {
    hornvale_worldgen::chamber::ChamberAddr {
        vertex,
        band: hornvale_kernel::Band::Undercroft,
        branch: 0,
        level: 0,
    }
}

/// What [`Session::delve_at`] says when the cave mouth is unbarred but the
/// entrance chamber the lattice would realize does not exist — spec §3.4
/// rung 0, *"the void exists and is unreachable"*.
///
/// **A const rather than an inline literal, and the promotion is the point.**
/// It is the ninth string in the vocabulary [`barred_refusal`] and
/// [`clear_response`] between them own, and the one a reader is most likely
/// to collide with by accident, because it is the only one that is about
/// stone the way three of `barred_refusal`'s arms are and is written a whole
/// function away from them. `every_passage_outcome_reads_distinctly` sweeps
/// all sixteen of the two verbs' strings pairwise, which it could not do
/// while this one was spelled inside a `match` arm — and fix round 1 promoted
/// the other seven for the same reason, after the review showed a collision
/// between two of them passing the whole vessel suite.
///
/// Unreachable since The Drift deleted the existence coin
/// (`delve_has_three_distinguishable_outcomes`'s doc records the measurement:
/// 0 of 48,316 caves over thirty worlds), and kept as live code for the reason
/// that doc gives.
/// type-audit: bare-ok(prose)
const UNREALIZED_CHAMBER_REFUSAL: &str =
    "The cave mouth is here, but the rock beyond is sealed; there is no way down.";

/// The seven strings the two passage verbs say OUTSIDE the barrier tables —
/// the footing refusals a body gets for standing in the wrong place, plus the
/// line a descent that succeeds actually prints.
///
/// **Consts rather than inline literals, and fix round 1's review is the
/// reason.** `every_passage_outcome_reads_distinctly` shipped sweeping nine
/// strings while the two verbs could say sixteen, and the review demonstrated
/// what the gap cost: making `clear_passage_column`'s no-cave refusal
/// byte-identical to `delve_column`'s left the whole vessel suite green. A
/// sweep widened over hand-COPIED literals would have stayed green against
/// that same edit, because the copy and the call site are two objects and
/// only one of them moved. The sweep reads these, production reads these, so
/// there is one object and a collision is reachable.
///
/// `delve`'s refusal for a body that is indoors: there is rock under a
/// chamber, but not a cave mouth you can reach from inside one.
/// type-audit: bare-ok(prose)
const NO_ROCK_INSIDE_REFUSAL: &str = "There is no rock to delve into in here.";

/// `delve`'s refusal for a body already underground — it names the verb that
/// undoes the state rather than merely saying no.
/// type-audit: bare-ok(prose)
const ALREADY_BELOW_DELVE_REFUSAL: &str =
    "You are already below; 'climb' brings you back up first.";

/// `delve`'s refusal where the terrain places no cave at all — the outcome
/// [`barred_refusal`]'s doc argues every OTHER refusal must be distinguishable
/// from. Said at two sites ([`Session::delve_column`] with no cave, and
/// [`Session::delve_at`] with no terrain handle at all): one string, because
/// both are the same fact about the world from the player's side.
/// type-audit: bare-ok(prose)
const NO_CAVE_TO_DELVE_REFUSAL: &str = "There is no cave here to delve into.";

/// `clear`'s refusal for a body that is indoors — [`Session::clear_passage`]
/// shares `delve`'s footing but not its prose, which is the thing the sweep
/// below exists to keep true.
/// type-audit: bare-ok(prose)
const NOTHING_TO_CLEAR_INSIDE_REFUSAL: &str = "There is nothing to clear from in here.";

/// `clear`'s refusal for a body already underground.
/// type-audit: bare-ok(prose)
const ALREADY_BELOW_CLEAR_REFUSAL: &str =
    "You are already below; there is nothing left to clear from down here.";

/// `clear`'s refusal where the terrain places no cave mouth at all.
/// type-audit: bare-ok(prose)
const NO_CAVE_MOUTH_TO_CLEAR_REFUSAL: &str = "There is no cave mouth here to clear.";

/// What `open`/`close` say when the player named nothing. A hint rather than
/// a parse error, the shape `examine`'s own bare arm already uses: the player
/// typed a real verb and left out its object, which is a different mistake
/// from typing a word that is not a verb.
/// type-audit: bare-ok(prose)
const OPEN_WHAT_HINT: &str = "Open what?";

/// [`OPEN_WHAT_HINT`]'s other half.
/// type-audit: bare-ok(prose)
const CLOSE_WHAT_HINT: &str = "Close what?";

/// `open`/`close`'s refusal outside a chamber.
///
/// **One string for out of doors, underwater and underground, and that is
/// deliberate rather than lazy.** The only things in this world with a lid
/// are anchors of a chamber's interior; nowhere else composes an `Interior`
/// at all, so "there is nothing here that opens" is the same true sentence in
/// all three bands. It is NOT true of a barred cave mouth, which does open —
/// but `clear` is that act and this task did not merge the two (see
/// [`Session::open_or_close`]'s doc for what that would take).
/// type-audit: bare-ok(prose)
const NOTHING_HERE_OPENS_REFUSAL: &str = "There is nothing here that opens.";

/// `take`'s bare-verb hint — a hint, not a refusal, on
/// [`OPEN_WHAT_HINT`]'s own terms: the player typed a real verb.
/// type-audit: bare-ok(prose)
const TAKE_WHAT_HINT: &str = "Take what?";

/// [`TAKE_WHAT_HINT`]'s counterpart for `drop`.
/// type-audit: bare-ok(prose)
const DROP_WHAT_HINT: &str = "Drop what?";

/// `put`'s hint, which answers for BOTH the bare verb and a line with no
/// `in` — one sentence, because the two are the same mistake (a player who
/// knows what they want to stow and not where) and a second sentence would be
/// two spellings of one correction.
/// type-audit: bare-ok(prose)
const PUT_WHAT_HINT: &str = "Put what, and in what? Say it as 'put a key in a strongbox'.";

/// `take`'s refusal outside a chamber, on exactly [`NOTHING_HERE_OPENS_REFUSAL`]'s
/// terms: only a chamber composes an `Interior`, so nowhere else in the world
/// holds a thing a body could pick up, and one true sentence covers out of
/// doors, underwater and underground alike.
/// type-audit: bare-ok(prose)
const NOTHING_HERE_TO_TAKE_REFUSAL: &str = "There is nothing here you could pick up.";

/// `drop`'s refusal outside a chamber. It names the CONSEQUENCE rather than
/// pretending a body cannot let go of something in the open air: a thing's
/// identity is derived under a chamber facet and no locale-band interior
/// composes a portable anchor, so a thing set down outdoors would have a
/// location nothing ever reads again. See [`Session::drop_carried`].
/// type-audit: bare-ok(prose)
const NOWHERE_TO_SET_DOWN_REFUSAL: &str =
    "Not here — put down out of doors, it would be lost. Carry it inside first.";

/// `open`'s refusal at a lockable thing with no key in custody — the arc's
/// own acceptance test, spoken (spec §3.8, acceptance 2).
///
/// It names the lock and the want, never the KIND of thing that would satisfy
/// it. A sentence saying "you need the iron key" would put the naming back.
///
/// **What M+N this actually buys, corrected.** This doc read: *"the lock
/// declares what it requires and the key declares what it carries, and
/// neither names the other."* The second half is true — `key` declares
/// [`crate::affordance::ObjectProperty::Portable`] on its own
/// `object_registry` row and names no lock. **The first half is false.**
/// `ObjectProperty::Lockable` is a bare enum variant with no payload: it
/// declares only THAT there is a lock, never what opens it. The required
/// property is a LITERAL in one place — `ObjectProperty::Portable`, written
/// inside [`Session::open_or_close`]'s lock arm — so the honest statement is
/// that the lock names no KIND (a second lockable kind costs no edit), while
/// what it wants is a constant, not a declaration.
///
/// **The hazard that hides in the difference — Task 12 came and went without
/// tripping it, so it now belongs to whoever next edits the property table.**
/// The
/// literal is correct today only because `Portable` went to `key` and nowhere
/// else. The day a second kind carries it, EVERY portable thing opens EVERY
/// lock — a lantern, a coin, a loaf. Nothing about this const or that arm
/// would change, no test would notice from the lock's side, and the failure
/// would be a played one. Task 12 shipped `take`/`drop`/`put` — three verbs
/// gating on `Portable` — and gave the property to no new kind, so the
/// roster is still exactly `["key"]`. That is why
/// `the_lock_wants_a_property_and_exactly_one_kind_supplies_it` asserts the
/// `Portable` roster by name rather than trusting a sentence: it reddens on
/// the second carrier, and its message states the two ways out (give the lock
/// a payload naming its opener, or add a narrower property the key alone
/// carries).
/// type-audit: bare-ok(prose)
const LOCKED_WITHOUT_A_KEY_REFUSAL: &str =
    "It is locked, and you are carrying nothing that would open it.";

/// The fixed half of the one passage outcome that is not a fixed string:
/// [`Session::delve_at`]'s success line, completed with [`stratum_word`] for
/// the entrance chamber's rock.
///
/// **Split at the format hole deliberately.** The sweep needs an object it can
/// compare, and the whole rendered line is not one — it is eleven lines, one
/// per [`hornvale_climate::Stratum`]. What decides this outcome's
/// distinctness from the other fifteen is the prefix, which no other outcome
/// shares any word of, so the prefix is what is pinned and the sweep says so
/// rather than quietly comparing one arbitrary filling as though it were the
/// whole.
/// type-audit: bare-ok(prose)
const DESCENT_PREFIX: &str = "You worm down into the dark. The rock here is";

/// The refusal a barred passage gives, naming WHICH barrier turned the body
/// back — a refusal that named no reason would be indistinguishable from the
/// no-cave one, which is the thing `delve`'s third outcome exists to be.
///
/// **Deliberately distinct from [`UNREALIZED_CHAMBER_REFUSAL`]** ("the rock
/// beyond is sealed") even for `BarrierState::Sealed`: that arm answers a
/// different question — an entrance chamber the lattice never realizes at
/// all, currently unreachable since The Drift deleted the existence coin,
/// but still live code this refusal must not collide with. Sharing wording
/// between the two would leave the unrealized-chamber outcome and the barred
/// outcome reading identically, which is exactly the vacuous-tripwire risk
/// the spec calls out.
/// type-audit: bare-ok(prose: return)
fn barred_refusal(barrier: hornvale_worldgen::BarrierState) -> String {
    match barrier {
        hornvale_worldgen::BarrierState::Sealed => {
            "The cave mouth is here, but the passage beyond is barred: choked \
             with unbroken stone, with no way through and no way down."
        }
        hornvale_worldgen::BarrierState::Warded => {
            "The cave mouth is here, but something set against passage holds \
             the dark shut, and you cannot force it."
        }
        hornvale_worldgen::BarrierState::Thin => {
            "The cave mouth is here, but a thin fall of rubble blocks the way \
             down; it looks like it would not take much to clear."
        }
        hornvale_worldgen::BarrierState::Open => "The way down is open.",
    }
    .to_string()
}

/// What [`Session::clear_passage_at`] reports for each barrier state — the
/// full outcome table for `clear`, mirroring [`barred_refusal`]'s shape so
/// the two verbs read consistently. Whether the state ALSO writes an
/// [`crate::thing::OPENNESS`] fact through [`crate::passage::set_openness`]
/// is the caller's own decision
/// (exactly one arm, `Thin`, is paired with a write); this function only
/// answers what the player reads.
///
/// **Only [`hornvale_worldgen::BarrierState::Thin`] yields to `clear`,** and
/// that choice is this task's own: `BarrierState`'s doc reads `Sealed` as
/// "nothing passes" (there is no rubble here for an act of clearing to move)
/// and `Warded` as passage that "costs something specific and standing" —
/// and `barred_refusal`'s own `Warded` prose already tells the player
/// outright "you cannot force it," which a clearing verb that then forced it
/// open would directly contradict. `Thin`'s own refusal, by contrast, already
/// promises the outcome this verb delivers ("it looks like it would not take
/// much to clear").
/// type-audit: bare-ok(prose: return)
fn clear_response(barrier: hornvale_worldgen::BarrierState) -> String {
    match barrier {
        hornvale_worldgen::BarrierState::Sealed => {
            "You throw your weight against it, but there is no rubble here to \
             clear — the stone beyond is unbroken, and clearing has nothing \
             to move."
        }
        hornvale_worldgen::BarrierState::Warded => {
            "You throw your weight against it, but it is not stone or fall \
             that holds this dark shut, and no amount of clearing moves it."
        }
        hornvale_worldgen::BarrierState::Thin => {
            "You wedge your shoulder into the rubble and heave; the loose \
             fall gives way, and the passage down stands clear."
        }
        hornvale_worldgen::BarrierState::Open => {
            "The way down is already clear; there is nothing here to clear."
        }
    }
    .to_string()
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
///
/// **`pub(crate)`, not private (The Gallery, Task 4):**
/// [`crate::underground::Underground::step`] reuses this exact table rather
/// than a second copy of it — a diagonal is a diagonal whether the walls
/// around it are built or natural rock.
pub(crate) fn cell_delta(c: Compass) -> Option<(i32, i32)> {
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

    /// The Offer, Task 7 (spec §4): `HELP` is the fourth surface spec §4
    /// names, and the one that resists a runtime `offered_to_observer` call
    /// for a structural reason rather than an oversight — it lists every
    /// verb unconditionally, for a body that may be standing anywhere at
    /// all, so there is no single `(KindId, Body, Knowledge)` triple to
    /// route it through (see the Task 7 report for the fuller finding). What
    /// IS mechanizable is the text-level agreement this test holds: the
    /// `warm` line's own word must be [`crate::affordance::OfferedVerb::
    /// Warm`]'s canonical spelling, it must name the SAME carrier
    /// [`crate::affordance::object_registry`] assigns `RadiatesHeat`
    /// (`chamber_prose::noun(AnchorKind::Hearth)`), and that carrier must
    /// actually offer `Warm` per the derived query — so a rename in either
    /// place, or a reassignment of the carrier, reddens here rather than
    /// drifting silently, the same discipline
    /// `every_bare_verb_help_lists_is_classified` already holds between
    /// `HELP` and the verb-gating rosters.
    #[test]
    fn help_names_warm_with_the_offer_modules_own_word_and_carrier() {
        let warm_line = HELP
            .lines()
            .find(|l| l.trim_start().starts_with("warm "))
            .expect("HELP must list warm");
        assert!(
            warm_line
                .trim_start()
                .starts_with(crate::affordance::OfferedVerb::Warm.word()),
            "HELP's warm line must open with OfferedVerb::Warm's own word: {warm_line:?}"
        );
        let hearth_noun = crate::chamber_prose::noun(crate::interior::AnchorKind::Hearth)
            .expect("Hearth always names a noun");
        assert!(
            warm_line.contains(hearth_noun),
            "HELP's warm line must name the same carrier object_registry \
             assigns RadiatesHeat to ({hearth_noun:?}): {warm_line:?}"
        );
        assert!(
            crate::affordance::offered_by(crate::affordance::thing_kind_of(
                crate::interior::AnchorKind::Hearth,
            ))
            .contains(&crate::affordance::OfferedVerb::Warm),
            "the carrier HELP names must actually offer Warm, or the two \
             texts would agree with each other while disagreeing with the \
             derived query"
        );
    }

    /// `warm` (The Offer, Task 5, spec §3.3/§6) is the LIVE half of
    /// acceptance test (2): `warm_appears_on_hearth_with_no_object_table_edit`
    /// (`tests/suite/affordance.rs`) proves the QUERY offers `warm` on
    /// `Hearth` and not `Bed`, entirely without a `Session`; this proves the
    /// VERB actually dispatches and reads only derived interior state.
    ///
    /// A freshly-possessed seed-42 session starts out of doors
    /// (`self.inside` is `None`), where `AnchorKind::Hearth` cannot exist at
    /// all — there is no chamber `Interior` to carry one — so `warm` must
    /// refuse for want of a fire.
    ///
    /// Mutation this is written to catch: replacing `Self::warm`'s
    /// `has_hearth` check with an unconditional `true` (which would make
    /// `warm` always succeed, indoors or out, hearth or none) — this test
    /// reddens on that mutation while `warm_appears_on_hearth_with_no_
    /// object_table_edit` stays green, since that test never calls
    /// `Session::handle` at all. Confirmed by actually applying the
    /// mutation and re-running (task-5 report carries the transcript).
    #[test]
    fn warm_refuses_with_no_hearth_in_reach() {
        let world = seam_world();
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
        assert!(
            session.inside.is_none(),
            "sanity check: a fresh possession starts out of doors"
        );
        let out = match session.handle("warm") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("warm must not release: {t}"),
        };
        assert!(
            out.contains("no fire"),
            "warm out of doors must refuse for lack of a hearth, got: {out}"
        );
    }

    /// `warm` is gated by the body like every other in-character verb (spec
    /// §2.1/§3.2): a sleeping body cannot warm itself any more than it can
    /// walk. This is the BEHAVIOURAL half of the three-roster proof —
    /// `every_bare_verb_help_lists_is_classified` catches a roster drift
    /// structurally (by scanning `HELP`/`IN_CHARACTER_VERBS`); this drives an
    /// actual sleeping body at `warm` and checks the refusal itself.
    ///
    /// Mutation this is written to catch: dropping `"warm"` from
    /// [`IN_CHARACTER_VERBS`] while leaving its dispatch arm in
    /// [`Session::handle`]'s match — exactly the "ungated new verb" hazard
    /// that roster's own doc warns about. Confirmed by actually removing the
    /// entry, re-running (this test reddened, asserting
    /// `warmed != "You cannot — you are asleep."` since the dispatch ran
    /// unrefused), and restoring it (task-5 report carries the transcript).
    #[test]
    fn warm_is_refused_while_asleep() {
        let world = seam_world();
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
        let slept = match session.handle("sleep") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("sleep must not release: {t}"),
        };
        assert!(
            !slept.starts_with("No verb"),
            "`sleep` must be a verb for this test to mean anything: {slept}"
        );
        assert_eq!(
            session.body_state(),
            BodyState::Asleep,
            "sanity check: asleep alone must gate as asleep"
        );
        let warmed = match session.handle("warm") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("warm must not release: {t}"),
        };
        assert_eq!(warmed, "You cannot — you are asleep.");
    }

    /// `clear` is gated by the body like every other in-character verb (spec
    /// §2.1/§3.2), and it is the verb for which that mattered most: it is
    /// the only one in the free band that WRITES TO THE LEDGER, so an
    /// ungated `clear` meant a sleeping body could commit an
    /// `openness` fact (a `PASSAGE_CLEARED` one, before Task 8 retired that
    /// predicate) — contradicting `HELP`'s own `sleep` line
    /// ("the body stops obeying until its own cycle wakes it, and only '!'
    /// verbs answer meanwhile").
    ///
    /// **This is the fix, not a precaution.** The Latch shipped `clear` into
    /// [`Session::handle`]'s match and into neither
    /// [`IN_CHARACTER_VERBS`] nor [`HELP`], which is exactly the blind zone
    /// `every_bare_verb_help_lists_is_classified` cannot see (it holds the
    /// two lists in agreement; a verb absent from both agrees). The final
    /// whole-branch review found it; the structural test could not.
    /// Deliberately placed beside `warm_is_refused_while_asleep` rather than
    /// among the passage tests below, because the pair — one gated verb that
    /// only speaks, one that commits — is the argument.
    ///
    /// MUTATION this must fail against: drop `"clear"` from
    /// [`IN_CHARACTER_VERBS`] (and its `HELP` line, so
    /// `every_bare_verb_help_lists_is_classified` stays green and this test
    /// is the only thing objecting) — precisely the state the branch shipped
    /// in. Confirmed 2026-08-28 by making that edit and re-running: this
    /// test reddened BEHAVIOURALLY, not on a compile error, with
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: "There is no cave mouth here to clear."
    ///  right: "You cannot — you are asleep."
    /// ```
    ///
    /// — the dispatch ran `clear_passage` unrefused and answered from inside
    /// the handler. Restored, and re-run on a fresh binary: green.
    ///
    /// **The ledger assertion names its SUBJECT** (fix round 1, m5). It
    /// shipped as "no OPENNESS fact exists anywhere in this ledger", which is
    /// a true statement about a session today and a fragile one: Task 11's
    /// `open`/`close` writes `openness` in a session, and this test would then
    /// have reddened for a reason with nothing to do with the sleep gate. It
    /// now checks the only population a `clear` could touch — cave mouths —
    /// and commits an unrelated `openness` fact first, so the narrowing is
    /// demonstrated rather than asserted: the old assertion fires on that
    /// control, the new one does not. Confirmed 2026-08-29 by re-inserting
    /// the shipped form beside the new one:
    ///
    /// ```text
    /// thread 'session::tests::clear_is_refused_while_asleep' panicked at
    /// windows/vessel/src/session.rs: a refused `clear` must commit nothing:
    /// the gate stands in front of the act, not inside it
    /// ```
    ///
    /// Removed again, and the subject-named pair re-run green.
    ///
    /// **The refusal string is the mutation witness; the ledger assertion is
    /// a standing invariant.** Seed 42's flagship vertex bears no cave, so an
    /// ungated `clear` refuses with `NO_CAVE_MOUTH_TO_CLEAR_REFUSAL` and
    /// writes nothing either — which is exactly what the red above shows.
    /// That is stated plainly rather than left for a reader to discover: the
    /// ledger half cannot be reddened by the mutation this test names, and a
    /// check whose strength is overstated is the shape fix round 1 found twice
    /// in this file.
    /// The subject of `clear_is_refused_while_asleep`'s control fact — an
    /// entity that is deliberately not a cave mouth and not anything else the
    /// session mints, so an `openness` fact about it can only have come from
    /// that test.
    const UNRELATED_OPENABLE: hornvale_kernel::EntityId =
        hornvale_kernel::EntityId(match std::num::NonZeroU64::new(0x00C0_FFEE) {
            Some(n) => n,
            None => unreachable!(),
        });

    #[test]
    fn clear_is_refused_while_asleep() {
        let world = seam_world();
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
        let slept = match session.handle("sleep") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("sleep must not release: {t}"),
        };
        assert!(
            !slept.starts_with("No verb"),
            "`sleep` must be a verb for this test to mean anything: {slept}"
        );
        assert_eq!(
            session.body_state(),
            BodyState::Asleep,
            "sanity check: asleep alone must gate as asleep"
        );
        // A POSITIVE CONTROL for the subject-named assertion below: an
        // openness fact about something that is not a cave mouth. The
        // assertion this test used to carry ("no thing anywhere was opened")
        // fires on this; the one it carries now must not.
        session
            .ledger
            .commit(
                crate::thing::openness_fact(UNRELATED_OPENABLE, true, session.day),
                &session.registry,
            )
            .expect("OPENNESS is registered by Session::start and is non-functional");

        let cleared = match session.handle("clear") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("clear must not release: {t}"),
        };
        assert_eq!(cleared, "You cannot — you are asleep.");

        // NAME THE SUBJECT (fix round 1, m5). This assertion first read "no
        // OPENNESS fact exists anywhere", which is true today only because
        // nothing else in a session writes one — Task 11's `open`/`close`
        // will, and this test would then have gone red for a reason with
        // nothing to do with the sleep gate. The only subject a `clear` can
        // ever open is a CAVE MOUTH, so that is the population checked, and
        // the control fact above proves the filter discriminates rather than
        // passing because the ledger happens to be bare.
        let cave_mouths: std::collections::BTreeSet<hornvale_kernel::EntityId> = session
            .ledger
            .find(hornvale_kernel::INSTANCE_OF)
            .filter(|f| f.object == Value::Text(crate::passage::CAVE_MOUTH.to_string()))
            .map(|f| f.subject)
            .collect();
        let opened: Vec<hornvale_kernel::EntityId> = session
            .ledger
            .find(crate::thing::OPENNESS)
            .map(|f| f.subject)
            .filter(|subject| cave_mouths.contains(subject))
            .collect();
        assert!(
            cave_mouths.is_empty() && opened.is_empty(),
            "a refused `clear` must neither promote a cave mouth nor open \
             one — the gate stands in front of the act, not inside it: \
             promoted {cave_mouths:?}, opened {opened:?}"
        );
        assert!(
            session
                .ledger
                .find(crate::thing::OPENNESS)
                .any(|f| f.subject == UNRELATED_OPENABLE),
            "the control fact must still be in the ledger, or the check above \
             is passing on an empty population rather than a filtered one"
        );
    }

    // --- The Chattel, Task 11: `open` and `close` (spec §3.7/§3.8) ---

    /// A body asleep cannot open anything. The body-state gate stands in
    /// front of every in-character verb, and a verb absent from
    /// [`IN_CHARACTER_VERBS`] silently bypasses it — the blind zone
    /// `every_bare_verb_help_lists_is_classified` cannot see, because a verb
    /// in NEITHER roster satisfies both of its directions by its absence.
    /// The Latch shipped `clear` exactly that way, and `open` is in the same
    /// consequential class: it WRITES to the ledger.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: remove `"open"` from
    /// [`IN_CHARACTER_VERBS`] (and its `HELP` line, so the roster-agreement
    /// test stays green and this test is the only thing objecting) — exactly
    /// the state The Latch shipped in. The verb keeps working and the
    /// dispatch is unchanged. Confirmed 2026-08-29, unfiltered over the whole
    /// crate (`cargo nextest run -p hornvale-vessel --no-fail-fast`, `847
    /// tests run: 845 passed, 2 failed`) — BEHAVIOURALLY, not on a compile
    /// error: the dispatch ran `open_or_close` unrefused and answered from
    /// inside the handler.
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: "Open what?"
    ///  right: "You cannot — you are asleep."
    /// ```
    ///
    /// The second failure in that run was
    /// `h2_no_shipped_verb_can_end_a_possession_by_death` (`left: 35 right:
    /// 36`), which is a roster-SIZE witness rather than a behavioural one and
    /// is named here so the "2 failed" is accounted for rather than left to
    /// look like this test firing twice.
    #[test]
    fn open_is_refused_while_asleep() {
        assert_eq!(
            asleep_then(crate::affordance::OfferedVerb::Open.word()),
            BODY_ASLEEP_REFUSAL
        );
    }

    /// Same, for `close`. Written as its own test rather than a loop over the
    /// two words: the point of the pair is that EACH verb has its own
    /// behavioural witness, and a loop would let one mutation redden a test
    /// whose name blames the other.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: remove `"close"` from
    /// [`IN_CHARACTER_VERBS`] and its `HELP` line. Confirmed 2026-08-29,
    /// unfiltered (`847 tests run: 845 passed, 2 failed` — the second is the
    /// same roster-size witness the test above names):
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: "Close what?"
    ///  right: "You cannot — you are asleep."
    /// ```
    #[test]
    fn close_is_refused_while_asleep() {
        assert_eq!(
            asleep_then(crate::affordance::OfferedVerb::Close.word()),
            BODY_ASLEEP_REFUSAL
        );
    }

    /// What the body-state gate says, as one constant, so the two tests above
    /// and the sanity checks in [`asleep_then`] cannot drift into two
    /// spellings of one refusal.
    const BODY_ASLEEP_REFUSAL: &str = "You cannot — you are asleep.";

    /// Put a fresh seed-42 possession to sleep and hand it `line`, returning
    /// what it answered — the shape `warm_is_refused_while_asleep` and
    /// `clear_is_refused_while_asleep` each write out inline, factored so a
    /// third and fourth copy do not accumulate.
    ///
    /// The two sanity checks are load-bearing and are why this is not merely
    /// two lines: without the first, a `sleep` that stopped being a verb
    /// would make every caller assert against a body that never went under;
    /// without the second, a change to what `sleep` does to `wake_at` would
    /// do the same more quietly.
    fn asleep_then(line: &str) -> String {
        let world = seam_world();
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
        let slept = match session.handle("sleep") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("sleep must not release: {t}"),
        };
        assert!(
            !slept.starts_with("No verb"),
            "`sleep` must be a verb for this to mean anything: {slept}"
        );
        assert_eq!(
            session.body_state(),
            BodyState::Asleep,
            "sanity check: asleep alone must gate as asleep"
        );
        match session.handle(line) {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("`{line}` must not release: {t}"),
        }
    }

    /// `open`/`close` dispatch for real, against a real chamber anchor, and
    /// are refused by the DERIVED offer rather than by a kind literal — an
    /// alcove is a recess, and a recess has no lid.
    ///
    /// **This is the end-to-end witness the pair otherwise lacks, and the
    /// reason it names an alcove rather than a strongbox is measured, not
    /// chosen** — see `a_lockable_thing_opens_only_with_the_key_in_custody`
    /// for the measurement. What it proves is the whole dispatch path: the
    /// arm in `Session::handle`, the band guard, the noun resolution against
    /// this chamber's own anchors, and `offered_to_observer`'s verdict.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: make `open_or_close` skip the offer
    /// check — `if !offer.contains(&verb)` -> `if false && !offer.contains(
    /// &verb)` (kept as an `&&` so `verb` stays bound and the red is
    /// behavioural rather than an unused-variable compile error). Confirmed
    /// 2026-08-29, unfiltered (`847 tests run: 846 passed, 1 failed` — this
    /// one and nothing else):
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: "You open the alcove. Within it: a hearth."
    ///  right: "The alcove does not open."
    /// ```
    ///
    /// The `left` side is worth reading twice: without the offer gate the
    /// verb does not merely answer wrongly, it COMMITS an openness fact about
    /// a recess in a wall and then reports its contents — which is why the
    /// ledger assertion below is part of this test rather than a separate
    /// one.
    #[test]
    fn a_kind_with_no_lid_is_refused_by_the_offer_not_by_a_kind_literal() {
        let world = world_at(13).expect("seed 13 builds");
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 13 possesses");
        session.handle("enter");
        session.handle("enter further in");
        let interior = session
            .chamber_interior_here()
            .expect("still indoors after stepping further in");
        let alcove = interior
            .ids()
            .into_iter()
            .find(|&a| crate::chamber_prose::noun(interior.anchor(a).kind) == Some("an alcove"))
            .expect(
                "precondition: seed 13's second chamber is the hearthroom, which draws an alcove",
            );
        let kind = crate::affordance::thing_kind_of(interior.anchor(alcove).kind);
        assert!(
            crate::affordance::carries(kind, crate::affordance::ObjectProperty::Encloses),
            "precondition: the alcove must ENCLOSE, or this test could not \
             tell 'no lid' apart from 'not a container'"
        );
        assert!(
            !crate::affordance::carries(kind, crate::affordance::ObjectProperty::Openable),
            "precondition: the alcove must carry no Openable"
        );

        let refused = match session.handle("open an alcove") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("open must not release: {t}"),
        };
        assert_eq!(refused, "The alcove does not open.");
        let refused = match session.handle("close an alcove") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("close must not release: {t}"),
        };
        assert_eq!(refused, "The alcove does not close.");
        assert!(
            session.ledger.find(crate::thing::OPENNESS).next().is_none(),
            "a refused open must commit nothing: the offer stands in front of \
             the act, not inside it"
        );
    }

    /// The two arms that need no container at all: a bare `open` is a HINT
    /// (the player typed a real verb and left out its object), and out of
    /// doors there is nothing with a lid.
    #[test]
    fn open_and_close_hint_when_bare_and_refuse_out_of_doors() {
        let world = seam_world();
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
        assert!(
            session.inside.is_none(),
            "sanity check: a fresh possession starts out of doors"
        );
        let say = |s: &mut Session<'_>, line: &str| match s.handle(line) {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("`{line}` must not release: {t}"),
        };
        assert_eq!(say(&mut session, "open"), OPEN_WHAT_HINT);
        assert_eq!(say(&mut session, "close"), CLOSE_WHAT_HINT);
        assert_eq!(
            say(&mut session, "open a strongbox"),
            NOTHING_HERE_OPENS_REFUSAL
        );
        assert_eq!(
            say(&mut session, "close a strongbox"),
            NOTHING_HERE_OPENS_REFUSAL
        );
    }

    /// A lockable thing refuses without the key and yields with it — the
    /// arc's own acceptance test (spec §6.2), and the first precondition in
    /// Hornvale that reads a SECOND object.
    ///
    /// # Why this drives the precondition rather than the sentence
    ///
    /// **THE PREMISE BELOW WAS TRUE WHEN WRITTEN AND IS NOW FALSE (decision
    /// 0398), and the correction is loud rather than a quiet edit because a
    /// reader reasons FROM a premise.** This paragraph read: *"No `Session` in
    /// any world can stand in front of a strongbox today, and that is a
    /// MEASUREMENT rather than an assumption"* — `the-strongbox` carried
    /// `needs_populous: true` (The Blocking: "a hamlet has nothing worth
    /// locking up"), which reads `Brief::is_populous`, i.e. `peak_population >
    /// hornvale_history::flesh::HAMLET_POPULATION_CEILING`, > 150. Probed on
    /// this tree, and the numbers stand:
    ///
    /// ```text
    /// seed 42: ceiling=150 occupations=1240 alive=389 populous_alive=0 max_alive_peak=84
    /// seed 13: ceiling=150 occupations=1094 alive=305 populous_alive=0 max_alive_peak=87
    /// seed  1: ceiling=150 occupations=1163 alive=310 populous_alive=0 max_alive_peak=85
    /// ```
    ///
    /// Not one living occupation in three whole worlds clears the ceiling, and
    /// a 48-seed sweep of the flagship a possession actually starts at found
    /// `populous = true` zero times. **What changed is the gate, not the
    /// demography**: 0398 dropped `needs_populous` from the strongbox and its
    /// key, on the ground that a capability nothing can reach is not a
    /// capability. The same 48-seed sweep now finds 8 seeds where a possession
    /// walks into a room holding a strongbox with a key inside it
    /// (`tests/suite/strongbox_reachability.rs`), so
    /// [`LOCKED_WITHOUT_A_KEY_REFUSAL`] IS reachable through `Session::handle`
    /// and is asserted there against a played reply.
    ///
    /// This test is unchanged all the same, and deliberately so. What it
    /// drives is both halves the lock joins, each against real state — a
    /// custody FOLD that an end-to-end walk exercises only one arm of
    /// (Task 12 shipped `take`, so the *true* arm is reachable in play now;
    /// the third assertion below — false again once the key is set down in a
    /// room — still is not, because `drop` places it in the room the player
    /// is standing in and this test places it in another):
    ///
    /// 1. **The lock's declaration** — `strongbox` carries `Lockable` and the
    ///    alcove does not, read from `object_registry` through the same
    ///    `carries` query `open_or_close` calls.
    /// 2. **The custody read** — [`Session::carrying_something_that`] over a
    ///    real session's real ledger: false before, true after a REAL
    ///    promotion of a key into the body's hands, and false again once the
    ///    key is put down in a room. The third assertion is the one that
    ///    makes this a fold rather than a search: `query_by_object` still
    ///    finds the superseded fact, and only `location_of`'s latest-posting
    ///    re-ask drops it.
    ///
    /// MUTATION THIS MUST FAIL AGAINST — the property is *that custody is
    /// consulted*: in [`crate::thing::held_by`], drop the second filter, i.e.
    /// `.filter(|&thing| location_of(ledger, thing, day) ==
    /// Some(Value::Entity(holder)))` -> `.filter(|&thing| { let _ = thing;
    /// true })`. Every candidate the O-index ever saw is then held forever,
    /// so a key put back down still opens the lock. Confirmed 2026-08-29,
    /// unfiltered (`847 tests run: 845 passed, 2 failed` — this test and
    /// `thing::a_holder_holds_what_was_last_put_in_it_and_nothing_it_has_
    /// given_up`, which is the same property asserted one layer down):
    ///
    /// ```text
    /// thread 'session::tests::a_lockable_thing_opens_only_with_the_key_in_custody'
    /// panicked at windows/vessel/src/session.rs:
    /// a key put back down is no longer carried — custody is a FOLD over the
    /// latest posting, not a search for any posting ever made
    /// ```
    #[test]
    fn a_lockable_thing_opens_only_with_the_key_in_custody() {
        use crate::affordance::{ObjectProperty, carries, thing_kind_of};
        let strongbox = thing_kind_of(crate::interior::AnchorKind::Strongbox);
        assert!(
            carries(strongbox, ObjectProperty::Lockable),
            "the lock must declare what it requires"
        );
        assert!(
            !carries(
                thing_kind_of(crate::interior::AnchorKind::Alcove),
                ObjectProperty::Lockable
            ),
            "a kind with no lock must not declare one, or the gate is universal"
        );

        let world = seam_world();
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
        let body = session.agent_entity();
        let day = session.day;
        assert!(
            !session.carrying_something_that(ObjectProperty::Portable),
            "a freshly possessed body carries nothing"
        );

        // A REAL promotion, in a real room, of the kind `object_registry`
        // marks Portable — never an invented EntityId, which would prove the
        // fold reads a value this test chose rather than one the ledger minted.
        let elsewhere = Facet {
            face: 0,
            path: vec![1],
        };
        let key = crate::thing::promote(
            &mut session.ledger,
            &session.registry,
            &elsewhere,
            thing_kind_of(crate::interior::AnchorKind::Key).0,
            0,
            day,
        )
        .expect("a shallow facet packs");
        session
            .ledger
            .commit(
                crate::thing::located_in_holder_fact(key, body, day),
                &session.registry,
            )
            .expect("LOCATED_IN is registered by Session::start and is non-functional");
        assert!(
            session.carrying_something_that(ObjectProperty::Portable),
            "a key placed in the body's own hands must be in its custody"
        );

        // Put it down again, a day later. The putting-IN fact is still in the
        // ledger forever; only the latest posting decides.
        let later = WorldTime::from_ticks(day.ticks() + 1);
        session
            .ledger
            .commit(
                crate::thing::located_in_room_fact(key, &elsewhere, later)
                    .expect("a shallow facet packs"),
                &session.registry,
            )
            .expect("LOCATED_IN is registered and non-functional");
        session.day = later;
        assert!(
            !session.carrying_something_that(ObjectProperty::Portable),
            "a key put back down is no longer carried — custody is a FOLD \
             over the latest posting, not a search for any posting ever made"
        );
    }

    /// **Acceptance 6.2's FIRST clause, driven through the verb** — *"a key
    /// opens a lockable strongbox"*, said by [`Session::handle`] to a body
    /// standing in the room the strongbox is in.
    ///
    /// # Why this test had to be written, and what was wrong before it
    ///
    /// Spec §6.2 is a conjunction: *"a key opens a lockable strongbox AND the
    /// same body without the key cannot."* The second clause was pinned twice
    /// over (here at the fold, and as a played reply in
    /// `tests/suite/strongbox_reachability.rs`). **The first clause was held
    /// by nothing at the verb level.**
    /// `a_lockable_thing_opens_only_with_the_key_in_custody` above never calls
    /// `open` at all — it pins that `strongbox` declares `Lockable` and that
    /// [`crate::thing::held_by`] is a fold, and leaves the JOIN between them
    /// unasserted. The consequence was measurable: making the lock's custody
    /// read vacuous, so the lock refuses even while carrying —
    ///
    /// ```text
    /// && !self.carrying_something_that(ObjectProperty::Portable)
    ///   ->  && (true || !self.carrying_something_that(ObjectProperty::Portable))
    /// ```
    ///
    /// — left the whole crate green (`850 tests run: 850 passed, 3 skipped`).
    /// So did the whole SUCCESS half of [`Session::open_or_close`] behind it:
    /// the `set_openness` call for a container, the `"Within it: …"` clause,
    /// the already-open no-op, and the empty case. This test kills the first
    /// three; the fourth is recorded as unreachable below.
    ///
    /// # A committed holding fact, because `held_by` is a FOLD
    ///
    /// Custody is a fold over [`crate::thing::LOCATED_IN`] postings, so this
    /// test commits the posting directly rather than typing `take`. It is
    /// kept that way after Task 12 rather than rewritten: the key is planted
    /// in a DIFFERENT facet (`face: 0, path: [1]`), which no walk can reach,
    /// so what is proved is that the lock reads CUSTODY and not proximity —
    /// a `take` here would put a key from this very room in the hand and
    /// could not tell the two apart. The key is a REAL promotion of the kind
    /// `object_registry` marks `Portable`, in a real facet, never an invented
    /// `EntityId`.
    ///
    /// **THE RESIDUAL THIS DOC RECORDED IS CLOSED, AND THE PARAGRAPH IS
    /// REPLACED RATHER THAN DELETED.** It read: *"The success arm is now
    /// HELD; it is still not REACHABLE by a player, because no shipped verb
    /// moves a key into custody. That is Task 12's, and it is the whole
    /// residual."* Task 12 shipped `take`, and the whole ladder is now
    /// played. Measured on seed 1's deepest chamber, 2026-08-29:
    ///
    /// ```text
    /// > open a strongbox   ->  It is locked, and you are carrying nothing that would open it.
    /// > take a key         ->  You take the key.
    /// > open a strongbox   ->  You open the strongbox. Within it: a key.
    /// ```
    ///
    /// **AND THE THING THAT MAKES IT REACHABLE IS ALSO A DEFECT, WHICH IS
    /// WHY THE FABRICATED-CUSTODY TEST IS KEPT.** Look at where the key came
    /// from: `the-key-in-the-strongbox` is the only pattern placing the only
    /// `Portable` kind, and it places it INSIDE the very lockable chest it
    /// opens — "a key is inside a strongbox or it is nowhere", in its own
    /// words. No verb in this tree respects grammar containment (the room's
    /// prose names the key, `examine a key` answers in full, both shipped
    /// before Task 12), so the played sequence above is a lock defeated by
    /// its own contents. That is an authored-grammar problem needing an
    /// authored-grammar fix — a pattern putting a key somewhere a strongbox
    /// is not, which is Task 11's Step 0 shape and owes its own census
    /// re-run — and `Session::take`'s doc records it at the verb. This test,
    /// which brings its key from a room the player never stood in, is the
    /// one that holds acceptance 2 honestly meanwhile. One arm is narrower
    /// still:
    /// `"You open the {bare}. It is empty."` is unreachable AND unheld, and
    /// the reason is structural rather than pending.
    /// `the-key-in-the-strongbox` and `the-strongbox` carry identical gates
    /// in `INVENTORY` (`built: true`, no cold, no populous, `roles:
    /// &[Role::Store]`, `at_locale: false`) and `draw_from` is a pure filter
    /// with no seeded selection, so **every composed strongbox has a key in
    /// it** and `contents_of` can never return `None` for one. Reaching that
    /// branch needs a second `Openable` container the grammar puts nothing
    /// inside — a change to the pattern language, not a test.
    ///
    /// MUTATION THIS MUST FAIL AGAINST — the property is *that custody
    /// UNLOCKS, not merely that it is consulted*: the vacuous-lock mutation
    /// quoted above. Confirmed 2026-08-29, unfiltered over the whole crate.
    #[test]
    fn a_key_in_custody_opens_the_strongbox_a_player_walked_to() {
        use crate::affordance::{ObjectProperty, thing_kind_of};
        let world = world_at(1).expect("seed 1 builds");
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 1 possesses");
        let say = |s: &mut Session<'_>, line: &str| match s.handle(line) {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("`{line}` must not release: {t}"),
        };

        // Walk in and as far in as the place goes — the same route
        // `tests/suite/strongbox_reachability.rs` walks, for the same reason:
        // `Role::Store` is only ever chamber index >= 2 (`pattern::role_for`).
        assert!(
            say(&mut session, "enter").starts_with("[chamber "),
            "the possession never got indoors, so nothing below is tested"
        );
        for _ in 0..4 {
            if !say(&mut session, "enter further in").starts_with("[chamber ") {
                break;
            }
        }
        let nouns = session.chamber_nouns_here();
        assert!(
            nouns.iter().any(|n| n == "a strongbox") && nouns.iter().any(|n| n == "a key"),
            "precondition: seed 1's deepest chamber must hold a strongbox with \
             a key in it, or this test drives nothing: {nouns:?}"
        );

        // Before: the played refusal. Asserted here as well as in the suite
        // test, because a success assertion with no paired refusal cannot
        // tell "the key opened it" from "the lock was never consulted".
        assert_eq!(
            say(&mut session, "open a strongbox"),
            LOCKED_WITHOUT_A_KEY_REFUSAL
        );

        let body = session.agent_entity();
        let day = session.day;
        let elsewhere = Facet {
            face: 0,
            path: vec![1],
        };
        let key = crate::thing::promote(
            &mut session.ledger,
            &session.registry,
            &elsewhere,
            thing_kind_of(crate::interior::AnchorKind::Key).0,
            0,
            day,
        )
        .expect("a shallow facet packs");
        session
            .ledger
            .commit(
                crate::thing::located_in_holder_fact(key, body, day),
                &session.registry,
            )
            .expect("LOCATED_IN is registered by Session::start and is non-functional");
        assert!(
            session.carrying_something_that(ObjectProperty::Portable),
            "sanity check: the committed holding fact must reach the fold, or \
             the assertions below would pass for the wrong reason"
        );

        // The success arm, its contents clause, its no-op, and the round trip.
        assert_eq!(
            say(&mut session, "open a strongbox"),
            "You open the strongbox. Within it: a key.",
            "a key in custody must open the lock, and the reply must name what \
             the lid was hiding"
        );
        assert_eq!(
            say(&mut session, "open a strongbox"),
            "The strongbox is already open.",
            "a second open reports the state and commits nothing"
        );
        assert_eq!(
            say(&mut session, "close a strongbox"),
            "You close the strongbox.",
            "close is unlocked in both senses — a lid you could open, you may shut"
        );
        assert_eq!(
            say(&mut session, "open a strongbox"),
            "You open the strongbox. Within it: a key.",
            "and it re-opens: openness is a fold, not a latch"
        );
        assert!(
            session.ledger.find(crate::thing::OPENNESS).next().is_some(),
            "the verb's success arm must WRITE, not merely answer"
        );
    }

    // --- The Chattel, Task 12: `take`/`drop`/`put`/`carrying` (spec §3.8) ---

    /// A body asleep cannot pick anything up. Same argument, same blind zone,
    /// same consequential class as `open`: `take` WRITES — a
    /// [`crate::thing::LOCATED_IN`] posting naming the body — and a verb
    /// absent from [`IN_CHARACTER_VERBS`] bypasses the gate without either
    /// direction of `every_bare_verb_help_lists_is_classified` noticing.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: remove `"take"` from
    /// [`IN_CHARACTER_VERBS`] and its `HELP` line (so the roster-agreement
    /// test stays green and this test is the only thing objecting).
    /// Confirmed 2026-08-29, unfiltered over the whole crate
    /// (`860 tests run: 859 passed, 1 failed`) — BEHAVIOURALLY, not on a
    /// compile error: the dispatch ran `Session::take` unrefused and answered
    /// from inside the handler. It is the ONLY test that objects, which is
    /// the blind zone stated as a measurement.
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: "Take what?"
    ///  right: "You cannot — you are asleep."
    /// ```
    #[test]
    fn take_is_refused_while_asleep() {
        assert_eq!(
            asleep_then(crate::affordance::OfferedVerb::Take.word()),
            BODY_ASLEEP_REFUSAL
        );
    }

    /// Same, for `drop`. Its own test rather than a loop over the four, for
    /// the reason `close_is_refused_while_asleep` states: the point of the
    /// set is that EACH verb has a behavioural witness, and a loop would let
    /// one mutation redden a test whose name blames another.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: remove `"drop"` from
    /// [`IN_CHARACTER_VERBS`] and its `HELP` line. Confirmed 2026-08-29,
    /// unfiltered over the whole crate, in one run that also carried `put`'s
    /// and `carrying`'s mutations — `860 tests run: 857 passed, 3 failed`,
    /// the three being exactly the three tests those three mutations name,
    /// which is what makes the attribution unambiguous rather than merely
    /// plausible:
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: "Drop what?"
    ///  right: "You cannot — you are asleep."
    /// ```
    #[test]
    fn drop_is_refused_while_asleep() {
        assert_eq!(
            asleep_then(crate::affordance::OfferedVerb::Drop.word()),
            BODY_ASLEEP_REFUSAL
        );
    }

    /// Same, for `put`.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: remove `"put"` from
    /// [`IN_CHARACTER_VERBS`] and its `HELP` line. Confirmed 2026-08-29,
    /// unfiltered (the same three-mutation run
    /// `drop_is_refused_while_asleep` cites):
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: "Put what, and in what? Say it as 'put a key in a strongbox'."
    ///  right: "You cannot — you are asleep."
    /// ```
    #[test]
    fn put_is_refused_while_asleep() {
        assert_eq!(
            asleep_then(crate::affordance::OfferedVerb::Put.word()),
            BODY_ASLEEP_REFUSAL
        );
    }

    /// **`carrying` is refused while asleep, and this is the test the
    /// disjointness check could never have stood in for.**
    ///
    /// `carrying` writes nothing, which makes it the one verb of the four
    /// that could plausibly have been classified as an operator instrument.
    /// Had it gone into [`SESSION_CONTROL`] alone, every roster test in this
    /// file would still have passed:
    /// `session_control_is_never_an_in_character_verb` asserts only that no
    /// control verb is ALSO in-character (it would have been in one roster,
    /// not both), and `every_bare_verb_help_lists_is_classified`'s first loop
    /// accepts a `HELP` verb classified by EITHER roster while its second
    /// loop iterates [`IN_CHARACTER_VERBS`], which would not have contained
    /// it. A sleeping body would have answered a question about its own
    /// hands. That is The Latch's `clear` failure arriving through the other
    /// door, and this assertion is the only thing in the tree standing in
    /// front of it.
    ///
    /// It is written as a spelled literal rather than through
    /// `OfferedVerb::_.word()` because `carrying` has no `OfferedVerb`: it is
    /// body-scoped, and the offer query answers about an OBJECT.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: move `"carrying"` out of
    /// [`IN_CHARACTER_VERBS`] and into [`SESSION_CONTROL`], leaving its
    /// `HELP` line in place — the exact miscategorisation above. Confirmed
    /// 2026-08-29, unfiltered. **Every roster test stayed green under it**,
    /// which is the finding rather than a footnote:
    /// `every_bare_verb_help_lists_is_classified` and
    /// `session_control_is_never_an_in_character_verb` both passed while a
    /// sleeping body answered a question about its own hands, and this
    /// assertion was the only thing in `860 tests run: 857 passed, 3 failed`
    /// that objected on `carrying`'s behalf.
    ///
    /// ```text
    /// assertion `left == right` failed
    ///   left: "You are carrying nothing."
    ///  right: "You cannot — you are asleep."
    /// ```
    #[test]
    fn carrying_is_refused_while_asleep() {
        assert_eq!(asleep_then("carrying"), BODY_ASLEEP_REFUSAL);
    }

    /// Seed 1's flagship possession, one `enter` in — the threshold chamber,
    /// which since fix round 1 is where `the-key-by-the-door` composes the
    /// key a player can actually pick up.
    ///
    /// **This is the shallow half of the pair, and the pair exists because a
    /// lid means something now.** Before the lid gate, every custody test
    /// walked to the deepest chamber and lifted the key straight out of a
    /// locked strongbox; that route is closed, so a test that needs a
    /// `Portable` thing in hand starts HERE and a test that needs a container
    /// walks on with the key.
    fn at_the_door_of_seed_one(world: &World) -> Session<'_> {
        let (mut session, _) =
            Session::start(world, &PossessOpts::default()).expect("seed 1 possesses");
        assert!(
            say(&mut session, "enter").starts_with("[chamber "),
            "the possession never got indoors, so nothing below is tested"
        );
        let nouns = session.chamber_nouns_here();
        assert!(
            nouns.iter().any(|n| n == "a key"),
            "precondition: seed 1's threshold chamber must hold a key, or this \
             test drives nothing: {nouns:?}"
        );
        assert!(
            !nouns.iter().any(|n| n == "a strongbox"),
            "precondition: the room the key is taken in must stand no chest, \
             or the lid gate is not what lets this take through: {nouns:?}"
        );
        session
    }

    /// Walk seed 1's flagship possession as far into its dwelling as the
    /// place goes — the route
    /// `a_key_in_custody_opens_the_strongbox_a_player_walked_to` takes, for
    /// the same reason (`Role::Store` is only ever chamber index >= 2), and
    /// the only route in the tree that reaches a container with a LID.
    ///
    /// Returns the session and the say-helper's own preconditions already
    /// checked, so a caller's assertions cannot pass against a possession
    /// that never got indoors. The body is empty-handed and the strongbox is
    /// as the seed drew it: shut and locked.
    fn in_the_store_room_of_seed_one(world: &World) -> Session<'_> {
        let (mut session, _) =
            Session::start(world, &PossessOpts::default()).expect("seed 1 possesses");
        assert!(
            say(&mut session, "enter").starts_with("[chamber "),
            "the possession never got indoors, so nothing below is tested"
        );
        for _ in 0..4 {
            if !say(&mut session, "enter further in").starts_with("[chamber ") {
                break;
            }
        }
        let nouns = session.chamber_nouns_here();
        assert!(
            nouns.iter().any(|n| n == "a key"),
            "precondition: seed 1's deepest chamber must hold a key, or this \
             test drives nothing: {nouns:?}"
        );
        session
    }

    /// [`in_the_store_room_of_seed_one`] with the door key already in hand —
    /// the walk the campaign's own thesis describes, and the only way to
    /// reach the strongbox with something that opens it since the lid gate
    /// closed.
    ///
    /// It takes the key in the threshold chamber and then walks in, so a
    /// caller arrives standing in front of a shut, LOCKED chest carrying the
    /// one thing that turns it. The two facets are asserted distinct, because
    /// a walk that never left the front room would satisfy every downstream
    /// assertion for the wrong reason.
    fn in_the_store_room_of_seed_one_with_a_key(world: &World) -> Session<'_> {
        let mut session = at_the_door_of_seed_one(world);
        let door = session
            .chamber_facet_here()
            .expect("`enter` lands in a chamber");
        assert_eq!(say(&mut session, "take a key"), "You take the key.");
        for _ in 0..4 {
            if !say(&mut session, "enter further in").starts_with("[chamber ") {
                break;
            }
        }
        let store = session
            .chamber_facet_here()
            .expect("the walk ended in a chamber");
        assert_ne!(
            store, door,
            "precondition: the key must have been carried into a DIFFERENT \
             room, or nothing here crosses a threshold"
        );
        let nouns = session.chamber_nouns_here();
        assert!(
            nouns.iter().any(|n| n == "a strongbox"),
            "precondition: the walk must end in front of a chest: {nouns:?}"
        );
        assert_eq!(
            say(&mut session, "carrying"),
            "You are carrying a key.",
            "precondition: the key must survive the walk, or the open below \
             is not being paid for"
        );
        session
    }

    /// One line at a possession, with the release arm turned into a panic —
    /// the closure four tests above spell inline, factored once the fifth
    /// wanted it.
    fn say(session: &mut Session<'_>, line: &str) -> String {
        match session.handle(line) {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("`{line}` must not release: {t}"),
        }
    }

    /// **A thing taken in one room is carried into another and is still held
    /// there — the identity-that-travels claim, which is the whole
    /// campaign** (spec §6 acceptance 1, the first half; the saved-world half
    /// is `custody_survives_a_save_and_a_re_possession` in
    /// `tests/suite/session.rs`).
    ///
    /// The route crosses three rooms, not two: the threshold chamber the key
    /// is taken in, the chamber behind it, and the open air `out` returns
    /// to — and the two chamber facets are asserted DISTINCT, because a walk
    /// that never left the room it started in would satisfy every other
    /// assertion here.
    ///
    /// **It starts at the door rather than in the storeroom since fix round
    /// 1**, because the storeroom's key is inside a shut, locked chest and
    /// `take` no longer reaches through one. Nothing about the claim moved:
    /// the key is still taken in one room, still carried into another, and
    /// the entity held is still the one the room it came FROM derives.
    ///
    /// MUTATION THIS MUST FAIL AGAINST — the property is *that custody is
    /// held by the BODY and therefore travels with it*, not merely that
    /// `take` writes something: point `Session::take`'s commit at
    /// `located_in_room_fact(thing, &room, self.day)` instead of
    /// `located_in_holder_fact(thing, body, self.day)`. The take still
    /// answers "You take the key.", the room still stops offering it, and
    /// every latency assertion in the campaign stays green — the key is
    /// simply left on the floor of the room it was picked up in. Confirmed
    /// 2026-08-29, unfiltered over the whole crate. **It reds at the
    /// PRECONDITION, not at the assertion the mutation was aimed at**, and
    /// that is recorded rather than tidied: the first `carrying` after the
    /// take already disagrees, so the walk never runs. The later assertion is
    /// still worth its lines — it is what would catch a custody that reached
    /// the hand and did not survive the room change — but nothing has yet
    /// demonstrated it, and saying otherwise would be a mutation credited to
    /// the wrong line.
    ///
    /// ```text
    /// assertion `left == right` failed: the take must reach custody, or the
    /// walk below tests nothing
    ///   left: "You are carrying nothing."
    ///  right: "You are carrying a key."
    /// ```
    ///
    /// `860 tests run: 856 passed, 4 failed` — this test, both tests below,
    /// and `custody_survives_a_save_and_a_re_possession`. `take` is upstream
    /// of every custody assertion in the campaign, so a mutation of its
    /// commit is deliberately NOT a discriminating one; the three that follow
    /// are.
    #[test]
    fn a_thing_taken_is_carried_between_rooms() {
        let world = world_at(1).expect("seed 1 builds");
        let mut session = at_the_door_of_seed_one(&world);
        let door = session
            .chamber_facet_here()
            .expect("`enter` lands in a chamber");

        assert_eq!(say(&mut session, "carrying"), "You are carrying nothing.");
        assert_eq!(say(&mut session, "take a key"), "You take the key.");
        assert_eq!(
            say(&mut session, "carrying"),
            "You are carrying a key.",
            "the take must reach custody, or the walk below tests nothing"
        );

        assert!(say(&mut session, "enter further in").starts_with("[chamber "));
        let deeper = session
            .chamber_facet_here()
            .expect("`enter further in` lands in a chamber");
        assert_ne!(
            deeper, door,
            "precondition: the walk must have changed rooms, or 'carried \
             between rooms' is not what was tested"
        );
        assert_eq!(
            say(&mut session, "carrying"),
            "You are carrying a key.",
            "the key must still be in hand one room further in"
        );

        assert!(say(&mut session, "out").starts_with("[room "));
        assert_eq!(
            say(&mut session, "carrying"),
            "You are carrying a key.",
            "custody must survive leaving the building"
        );
        assert_eq!(
            crate::thing::held_by(&session.ledger, session.agent_entity(), session.day),
            vec![crate::thing::thing_id(&door, "key", 0).expect("a chamber facet packs")],
            "and the entity held must be the one the THRESHOLD room derives — \
             custody carries an identity, not a copy minted where it was \
             last seen"
        );
    }

    /// **A dropped thing joins the room it was dropped in, and is offered
    /// there on the next entry** (spec §3.4's negative fold, read forwards).
    ///
    /// The room it is dropped in is NOT the room it came from and composes no
    /// key anchor of its own, which is the case that makes this more than a
    /// restatement of latency: the chamber behind the threshold has never
    /// heard of a key, so the only thing that can offer it back is
    /// [`crate::thing::lying_in`] — the ledger's own answer. A `take` that
    /// consulted the grammar alone would refuse here.
    ///
    /// MUTATION THIS MUST FAIL AGAINST — the property is *that a dropped
    /// thing is placed in THIS room rather than merely released*: change
    /// `Session::drop_carried`'s commit to
    /// `located_in_room_fact(thing, &Facet { face: 0, path: vec![1] }, ...)`,
    /// a room that is not here. The reply is unchanged, `carrying` correctly
    /// reports empty hands, and the key is simply gone. Confirmed 2026-08-29,
    /// unfiltered, in one run carrying this mutation and the two below —
    /// `860 tests run: 857 passed, 3 failed`, one per mutation, so each is
    /// attributed by name rather than by argument.
    ///
    /// **The red is the ROOM KEY, one assertion earlier than the retake**,
    /// which is the sharper witness: `160` is the packed foreign facet the
    /// mutation names and `141819825979456` is the chamber the player is
    /// actually standing in.
    ///
    /// ```text
    /// assertion `left == right` failed: the drop must post THIS room's key
    ///   left: Some(Text("160"))
    ///  right: Some(Text("141819825979456"))
    /// ```
    #[test]
    fn a_dropped_thing_joins_the_room_it_was_dropped_in() {
        let world = world_at(1).expect("seed 1 builds");
        let mut session = at_the_door_of_seed_one(&world);
        let door = session
            .chamber_facet_here()
            .expect("`enter` lands in a chamber");
        assert_eq!(say(&mut session, "take a key"), "You take the key.");

        assert!(say(&mut session, "enter further in").starts_with("[chamber "));
        let entrance = session
            .chamber_facet_here()
            .expect("`enter further in` lands in a chamber");
        assert_ne!(entrance, door, "precondition: a different room");
        assert!(
            !session.chamber_nouns_here().iter().any(|n| n == "a key"),
            "precondition: this room's GRAMMAR must not compose a key, or the \
             ledger's own answer is not what is being tested"
        );

        assert_eq!(say(&mut session, "drop a key"), "You set the key down.");
        assert_eq!(say(&mut session, "carrying"), "You are carrying nothing.");
        assert_eq!(
            crate::thing::location_of(
                &session.ledger,
                crate::thing::thing_id(&door, "key", 0).expect("a chamber facet packs"),
                session.day
            ),
            Some(Value::Text(
                crate::thing::room_key(&entrance).expect("a chamber facet packs")
            )),
            "the drop must post THIS room's key"
        );
        assert_eq!(
            say(&mut session, "take a key"),
            "You take the key.",
            "the dropped key must be lying in THIS room, and takeable again"
        );
    }

    /// `put` stows a carried thing in a container standing here, and `take`
    /// gets it back out — the third direction custody can go, and the one
    /// that would silently strand a thing if [`Session::take`]'s container
    /// arm did not exist.
    ///
    /// It also pins the shut arm from both sides: a closed strongbox refuses
    /// the `put`, and the same strongbox opened accepts it.
    ///
    /// **IT COVERS ONE ROOM SHAPE OF TWO, AND ITS DOC USED TO CLAIM THE
    /// HOLE WAS CLOSED** (fix round 1). `take`'s container arm sits behind the
    /// noun match against `interior.ids()`, so it is reachable only in a room
    /// whose grammar composes the stowed thing's OWN anchor — which the
    /// storeroom below does, because the key is `Within(Strongbox)` there. In
    /// any other room the same `put` used to lose the thing for good;
    /// `a_thing_put_into_a_container_the_grammar_never_composes_comes_back_out`
    /// is the other shape, and [`Session::take_from_the_ledger`]'s second
    /// source is what answers it. Neither test substitutes for the other.
    ///
    /// MUTATION THIS MUST FAIL AGAINST — the property is *that `take` can
    /// reach into a container standing here*, which is what makes `put`
    /// reversible rather than a hole: in `Session::take`'s container arm,
    /// replace `Some(Value::Entity(h)) => h,` with
    /// `Some(Value::Entity(_)) => return Turn::Out(format!("You see no {} here.", rest.trim())),`
    /// — the already-carrying arm above it survives, so the match still
    /// compiles and every other take still works.
    ///
    /// **The mutation this replaces could not be applied at all.** It named
    /// `Some(Value::Entity(h)) if self.holder_stands_here(...)`, and there is
    /// no `holder_stands_here` in the tree and no guard on that arm: the real
    /// function is [`Session::holder_anchored_here`] and it is called AFTER
    /// the match, not inside it. A mutation nobody can apply is a claim that
    /// reads exactly like evidence. Re-taken against the real code, confirmed
    /// 2026-08-30, unfiltered over the whole crate —
    /// `864 tests run: 862 passed, 2 failed, 3 skipped`:
    ///
    /// ```text
    /// assertion `left == right` failed: a thing put into a chest standing
    /// here must be takeable back out of it
    ///   left: "You see no a key here."
    ///  right: "You take the key."
    /// ```
    ///
    /// **The second red is `closing_a_container_does_not_lock_it`, and it is
    /// recorded rather than tidied**: that test ends by taking the key back
    /// out of the same strongbox in the same room, so it rides this arm too.
    /// It is a weaker witness for THIS property (it reaches the take through
    /// four earlier verbs) and a sharper one for its own, which is why both
    /// exist.
    #[test]
    fn a_thing_put_into_an_open_chest_can_be_taken_back_out() {
        let world = world_at(1).expect("seed 1 builds");
        let mut session = in_the_store_room_of_seed_one_with_a_key(&world);
        assert_eq!(
            say(&mut session, "put a key in a strongbox"),
            "The strongbox is shut.",
            "a shut lid refuses what a player tries to stow behind it"
        );
        assert_eq!(
            say(&mut session, "open a strongbox"),
            "You open the strongbox. Within it: a key.",
            "the door key must turn the seeded lock, or nothing below is \
             reachable at all"
        );
        assert_eq!(
            say(&mut session, "put a key in a strongbox"),
            "You put the key in the strongbox."
        );
        assert_eq!(say(&mut session, "carrying"), "You are carrying nothing.");

        // THE STORE ROOM'S OWN KEY, out and back in — and it has to be this
        // one rather than the door key, because only a thing the ledger has
        // already placed reaches `take`'s container arm at all. The door key
        // stowed above is still in the chest and unreachable by noun: the
        // grammar's match resolves "a key" to this room's own anchor, so
        // every take below is about the store key.
        assert_eq!(
            say(&mut session, "take a key"),
            "You take the key.",
            "with the lid up, the room's own key comes out — the GRAMMAR arm"
        );
        assert_eq!(
            say(&mut session, "put a key in a strongbox"),
            "You put the key in the strongbox."
        );
        assert_eq!(say(&mut session, "carrying"), "You are carrying nothing.");
        assert_eq!(
            say(&mut session, "take a key"),
            "You take the key.",
            "a thing put into a chest standing here must be takeable back out \
             of it"
        );
        assert_eq!(say(&mut session, "carrying"), "You are carrying a key.");
    }

    /// **A thing stowed in a container the room's GRAMMAR never composes
    /// comes back out** — the other room shape, and the one the shipped verb
    /// lost the thing in permanently (fix round 1).
    ///
    /// The sibling test above stows the key in the strongbox of the room that
    /// composes both, where `take`'s own container arm answers. This one
    /// carries the key out of the building and back into the chamber whose
    /// grammar knows only a doorway and an alcove, so the noun match at the
    /// top of
    /// [`Session::take`] cannot fire and control reaches
    /// [`Session::take_from_the_ledger`] — where [`crate::thing::lying_in`] is
    /// room-keyed and direct and cannot see a thing held by a container
    /// entity. Before the fix the key was gone from the world for the rest of
    /// play, with a cheerful reply and no refusal: exactly the loss
    /// [`NOWHERE_TO_SET_DOWN_REFUSAL`] exists to prevent, through the sibling
    /// verb.
    ///
    /// The `an alcove` precondition is asserted rather than assumed, because
    /// an alcove carries `Encloses` and NOT `Openable` — it is the lidless
    /// arm of [`Session::holder_admits`], so this test also pins that a
    /// container with nothing to shut never refuses.
    ///
    /// MUTATION THIS MUST FAIL AGAINST — the property is *that the ledger's
    /// second source is consulted at all*: in
    /// `Session::take_from_the_ledger`, replace the
    /// `None => self.stowed_in_a_container_here(interior, room, wanted).map(...)`
    /// arm with `None => None`. Every floor take still works and the reply
    /// for a stowed thing reverts to "You see no a key here.". Confirmed
    /// 2026-08-30, unfiltered over the whole crate —
    /// `864 tests run: 863 passed, 1 failed, 3 skipped`, this test alone, so
    /// the second source is held by nothing else in the tree:
    ///
    /// ```text
    /// assertion `left == right` failed: a thing stowed in a container this
    /// room's grammar never composed must still be reachable, or `put` is a
    /// one-way trapdoor
    ///   left: "You see no a key here."
    ///  right: "You take the key."
    /// ```
    #[test]
    fn a_thing_put_into_a_container_the_grammar_never_composes_comes_back_out() {
        let world = world_at(1).expect("seed 1 builds");
        let mut session = in_the_store_room_of_seed_one_with_a_key(&world);
        let store = session
            .chamber_facet_here()
            .expect("the walk above ended in a chamber");

        assert!(say(&mut session, "out").starts_with("[room "));
        assert!(say(&mut session, "enter").starts_with("[chamber "));
        assert!(say(&mut session, "enter further in").starts_with("[chamber "));
        let alcove_room = session
            .chamber_facet_here()
            .expect("`enter further in` lands in a chamber");
        assert_ne!(alcove_room, store, "precondition: a different room");
        let nouns = session.chamber_nouns_here();
        assert!(
            nouns.iter().any(|n| n == "an alcove"),
            "precondition: this room must stand a lidless container: {nouns:?}"
        );
        assert!(
            !nouns.iter().any(|n| n == "a key"),
            "precondition: this room's GRAMMAR must not compose a key, or \
             `take`'s own container arm answers and the ledger's second \
             source is not what is being tested: {nouns:?}"
        );

        assert_eq!(
            say(&mut session, "put a key in an alcove"),
            "You put the key in the alcove."
        );
        assert_eq!(say(&mut session, "carrying"), "You are carrying nothing.");
        assert_eq!(
            say(&mut session, "take a key"),
            "You take the key.",
            "a thing stowed in a container this room's grammar never composed \
             must still be reachable, or `put` is a one-way trapdoor"
        );
        assert_eq!(say(&mut session, "carrying"), "You are carrying a key.");
    }

    /// **A locked lid refuses `take`, and lifting it lets the same take
    /// through** — the bypass fix's own witness (Task 13, fix round 1), both
    /// directions in one session.
    ///
    /// Before this, `take` asked about a container only on the LEDGER path,
    /// so a key the grammar composed inside a locked chest — the untouched
    /// state every world starts in — came straight out and the only lock in
    /// the game fell in one move:
    ///
    /// ```text
    /// > open a strongbox   It is locked, and you are carrying nothing that would open it.
    /// > take a key         You take the key.
    /// ```
    ///
    /// **The third beat is the half that keeps this from being a
    /// one-directional assertion**, and it is why the gate reads the location
    /// fold's `None` rather than [`crate::thing::is_latent`]: a key taken out
    /// and set down on this room's own floor is `is_latent`, and the anchor
    /// graph still says it lives in the chest — forever, because a composed
    /// interior is a pure function of the room. Shut the lid on that key and
    /// the naive gate refuses a thing lying in plain sight at the player's
    /// feet.
    ///
    /// MUTATION THIS MUST FAIL AGAINST — two, because the gate has two ways
    /// to be wrong and each has its own red. Both confirmed 2026-08-30,
    /// unfiltered over the whole crate (`--no-fail-fast`), and each was the
    /// ONLY failure of its run — `868 tests run: 867 passed, 1 failed, 3
    /// skipped`, twice:
    ///
    /// 1. Neutralise the gate — `if false && let Some(container) =
    ///    interior.anchor(id).within`. Beat one goes green and the lock is
    ///    defeated again:
    ///
    ///    ```text
    ///    assertion `left == right` failed: a shut lid must refuse the verb
    ///    that would move what is behind it
    ///      left: "You take the key."
    ///     right: "The key is shut away in something closed."
    ///    ```
    ///
    /// 2. Widen its condition from
    ///    `crate::thing::location_of(&self.ledger, thing, self.day).is_none()`
    ///    to `crate::thing::is_latent(…)`. Beats one and two stay green and
    ///    beat three reds with the key lying at the player's feet:
    ///
    ///    ```text
    ///    assertion `left == right` failed: a key lying on this floor must be
    ///    takeable however the grammar composed it
    ///      left: "The key is shut away in something closed."
    ///     right: "You take the key."
    ///    ```
    ///
    /// **The two mutations are not redundant and the second is the one worth
    /// having.** Mutation 1 is the defect this campaign fixed; mutation 2 is
    /// the defect the FIX would have introduced, and it is the plausible
    /// version of the gate — `is_latent` is the predicate the function was
    /// already calling one line down, so writing it there is the natural
    /// move rather than an exotic one.
    #[test]
    fn a_shut_lid_refuses_take_and_an_open_one_does_not() {
        let world = world_at(1).expect("seed 1 builds");
        let mut session = in_the_store_room_of_seed_one_with_a_key(&world);

        // 1. The grammar puts a key inside this chest and the chest is shut.
        assert_eq!(
            say(&mut session, "take a key"),
            "The key is shut away in something closed.",
            "a shut lid must refuse the verb that would move what is behind it"
        );
        assert_eq!(
            say(&mut session, "carrying"),
            "You are carrying a key.",
            "the refusal must not have quietly taken it anyway"
        );

        // 2. The same take, through a lid the player has lifted.
        assert_eq!(
            say(&mut session, "open a strongbox"),
            "You open the strongbox. Within it: a key."
        );
        assert_eq!(
            say(&mut session, "take a key"),
            "You take the key.",
            "an open lid must not refuse: a gate that never lets go is a \
             capability removed, not a lock"
        );

        // 3. That key, set down on this room's floor with the lid shut again.
        // The grammar still composes it inside the chest; the ledger knows
        // better, and the ledger wins.
        assert_eq!(say(&mut session, "drop a key"), "You set the key down.");
        assert_eq!(
            say(&mut session, "close a strongbox"),
            "You close the strongbox."
        );
        assert_eq!(
            say(&mut session, "take a key"),
            "You take the key.",
            "a key lying on this floor must be takeable however the grammar \
             composed it, or shutting a lid strands what is outside it"
        );
    }

    /// **A key set down where the grammar composes a DIFFERENT one is still
    /// found** — the shadowing the second key pattern made reachable, closed
    /// in the same change (Task 13, fix round 1).
    ///
    /// [`Session::take`] resolves a typed noun against this room's anchors
    /// FIRST, so the anchor decides which entity the verb is about. While
    /// `the-key-in-the-strongbox` was the only key pattern that was harmless:
    /// the one room with a key anchor was the deepest room a walk could
    /// reach, and nothing could be carried into it that the anchor did not
    /// already name. `the-key-by-the-door` puts an anchor in the room every
    /// possession passes through, so the shadow became a state a player can
    /// produce in six moves — stow the door key in the chest, carry the
    /// chest's key to the front room, set it down, and ask for it back.
    /// Before the fix the answer was *"You see no a key here"* with a key
    /// lying at the player's feet.
    ///
    /// The two verbs that made the old behaviour safe are still in the tree
    /// unchanged: this is a fall-through to
    /// [`Self::take_from_the_ledger`], not a second search, so the sentence a
    /// genuinely absent thing gets is the same sentence it always got.
    ///
    /// MUTATION THIS MUST FAIL AGAINST — the property is *that the ledger is
    /// consulted when the room's own anchor is elsewhere*: revert either
    /// fall-through in `Session::take`'s ledger branch to
    /// `return Turn::Out(format!("You see no {} here.", rest.trim()))`. Both
    /// compile; the second (`holder_anchored_here`'s `else`) is the arm this
    /// walk takes. Confirmed 2026-08-30, unfiltered over the whole crate,
    /// the only failure — `868 tests run: 867 passed, 1 failed, 3 skipped`:
    ///
    /// ```text
    /// assertion `left == right` failed: a key lying in this room must be
    /// takeable even though the room's own key anchor is somewhere else
    ///   left: "You see no a key here."
    ///  right: "You take the key."
    /// ```
    #[test]
    fn a_key_set_down_where_another_is_composed_is_still_taken() {
        let world = world_at(1).expect("seed 1 builds");
        let mut session = in_the_store_room_of_seed_one_with_a_key(&world);

        // Put the DOOR key beyond the front room's reach, so the anchor that
        // room composes is answering about something three chambers away.
        assert_eq!(
            say(&mut session, "open a strongbox"),
            "You open the strongbox. Within it: a key."
        );
        assert_eq!(
            say(&mut session, "put a key in a strongbox"),
            "You put the key in the strongbox."
        );
        // And bring the STORE room's key back to the front room instead.
        assert_eq!(say(&mut session, "take a key"), "You take the key.");
        assert!(say(&mut session, "out").starts_with("[room "));
        assert!(say(&mut session, "enter").starts_with("[chamber "));
        let door = session
            .chamber_facet_here()
            .expect("`enter` lands in a chamber");
        assert!(
            session.chamber_nouns_here().iter().any(|n| n == "a key"),
            "precondition: this room must compose a key ANCHOR of its own, or \
             there is no shadow to cast"
        );
        assert_eq!(say(&mut session, "drop a key"), "You set the key down.");

        assert_eq!(
            say(&mut session, "take a key"),
            "You take the key.",
            "a key lying in this room must be takeable even though the room's \
             own key anchor is somewhere else"
        );
        assert_eq!(
            crate::thing::held_by(&session.ledger, session.agent_entity(), session.day)
                .into_iter()
                .filter(|t| *t == crate::thing::thing_id(&door, "key", 0).expect("packs"))
                .count(),
            0,
            "and the thing taken must be the one on the floor, not the anchor \
             the room composes"
        );
    }

    /// **Closing a container does not lock it** (decision 0399) — the
    /// permanent soft-lock the shipped verbs could reach in three moves, and
    /// the model correction that removed it.
    ///
    /// The sequence is the reviewer's, verbatim: open the chest with the key,
    /// put the key inside it, shut the lid. Under the shipped conflation the
    /// world ended there — `open` refused for want of a key that was inside
    /// the chest, and `take a key` refused because the key was shut away, at
    /// any day, by any verb. Under decision 0399 the chest is shut and
    /// UNLOCKED, so the lid lifts again with empty hands and the key comes
    /// back out.
    ///
    /// **It asserts the recovery, not the absence of a refusal**, which is
    /// the distinction that makes it a regression test rather than a
    /// re-spelling of the bug: the last two lines are the two verbs that were
    /// stuck, each answering the way it would if nothing had ever been locked.
    ///
    /// MUTATION THIS MUST FAIL AGAINST — the property is *that lockedness is
    /// a state of the THING and not of the player's pockets*, which is
    /// decision 0399 itself put back: in `Session::open_or_close`, replace
    /// `let locked = open && self.container_is_locked(&room, thing_kind);`
    /// with
    /// `let locked = open && crate::affordance::carries(thing_kind, crate::affordance::ObjectProperty::Lockable);`
    /// — the pre-0399 expression. It compiles and every first-open test stays
    /// green, which is the whole reason the defect shipped. Confirmed
    /// 2026-08-30, unfiltered over the whole crate —
    /// `864 tests run: 863 passed, 1 failed, 3 skipped`, this test alone:
    ///
    /// ```text
    /// assertion `left == right` failed: a shut lid is not a turned key: the
    /// chest a player closed on their own key must open again with empty
    /// hands
    ///   left: "It is locked, and you are carrying nothing that would open it."
    ///  right: "You open the strongbox. Within it: a key."
    /// ```
    #[test]
    fn closing_a_container_does_not_lock_it() {
        let world = world_at(1).expect("seed 1 builds");
        let mut session = in_the_store_room_of_seed_one_with_a_key(&world);
        assert_eq!(
            say(&mut session, "open a strongbox"),
            "You open the strongbox. Within it: a key.",
            "precondition: the key in custody must turn the seeded lock, or \
             nothing below is about closing"
        );
        assert_eq!(
            say(&mut session, "put a key in a strongbox"),
            "You put the key in the strongbox."
        );
        assert_eq!(
            say(&mut session, "close a strongbox"),
            "You close the strongbox."
        );

        assert_eq!(
            say(&mut session, "open a strongbox"),
            "You open the strongbox. Within it: a key.",
            "a shut lid is not a turned key: the chest a player closed on \
             their own key must open again with empty hands"
        );
        assert_eq!(
            say(&mut session, "take a key"),
            "You take the key.",
            "and the key must come back out, or the soft-lock is only one \
             verb further along"
        );
    }

    /// **A chest that was never unlocked stays locked across a close** — the
    /// other half of decision 0399, and the half a reader is likeliest to
    /// suspect the fix of breaking.
    ///
    /// Clause 3: a seeded strongbox starts locked, and the default is
    /// authored on the KIND rather than drawn, so a chest nobody has opened
    /// refuses empty hands however many times its lid is worked. The walk
    /// below never takes the key, so `open` never turns the lock, so
    /// `container_is_locked` reads its absent-fact default every time.
    ///
    /// MUTATION THIS MUST FAIL AGAINST — the property is *that a REFUSED
    /// `open` writes nothing*, which is what the last two lines hold and what
    /// the first line alone could not: in `Session::open_or_close`, hoist the
    /// `if locked { crate::thing::set_lockedness(…, false, …) }` block ABOVE
    /// the `LOCKED_WITHOUT_A_KEY_REFUSAL` return, so the lock turns on the
    /// way to being told it did not. Confirmed 2026-08-30, unfiltered over
    /// the whole crate — `864 tests run: 863 passed, 1 failed, 3 skipped`,
    /// this test alone, and it reds on the THIRD assertion rather than the
    /// precondition:
    ///
    /// ```text
    /// assertion `left == right` failed: and the lock is still a lock
    ///   left: "You open the strongbox. Within it: a key."
    ///  right: "It is locked, and you are carrying nothing that would open it."
    /// ```
    ///
    /// **A second, blunter mutation is recorded because its RED IS IN THE
    /// WRONG PLACE and that is worth knowing before anyone credits it**:
    /// flipping `Session::container_is_locked`'s `.unwrap_or(true)` to
    /// `.unwrap_or(false)` reds three tests
    /// (`864 tests run: 861 passed, 3 failed`) — this one plus
    /// `a_key_in_custody_opens_the_strongbox_a_player_walked_to` and
    /// `strongbox_reachability::a_possession_walks_to_a_strongbox_and_finds_
    /// it_locked` — and all three object at "a seeded strongbox starts
    /// locked", which was already held before this test existed. It proves
    /// the default, not the close, so it is not this test's mutation.
    #[test]
    fn a_container_nobody_unlocked_is_still_locked_after_a_close() {
        let world = world_at(1).expect("seed 1 builds");
        let mut session = in_the_store_room_of_seed_one(&world);
        assert_eq!(
            say(&mut session, "open a strongbox"),
            LOCKED_WITHOUT_A_KEY_REFUSAL,
            "precondition: a seeded strongbox starts locked"
        );
        assert_eq!(
            say(&mut session, "close a strongbox"),
            "The strongbox is already shut.",
            "the refused open must have written nothing"
        );
        assert_eq!(
            say(&mut session, "open a strongbox"),
            LOCKED_WITHOUT_A_KEY_REFUSAL,
            "and the lock is still a lock"
        );
    }

    /// **`open` and `close` each take an instant, so a chest's state is
    /// representable** (fix round 1) — the same representability argument
    /// [`Session::take`]'s doc makes for custody, applied to the verb pair
    /// that shipped free.
    ///
    /// `Ledger::commit` dedups a fact identical to one already held and a
    /// free verb leaves the clock where it found it, so `open`, `close`,
    /// `open` at ONE instant committed `open`, `shut`, and then a third fact
    /// byte-identical to the first — dropped. The last posting at that
    /// instant was `shut`, every reply having been computed before its
    /// commit. The result was not one wrong reply but a stuck instant that
    /// only `wait` escaped.
    ///
    /// **The assertion is on the LEDGER's answer and on the next verb, never
    /// on `open`'s own reply**, because `open`'s reply is exactly what lied:
    /// a test reading it back would have passed against the defect.
    ///
    /// MUTATION THIS MUST FAIL AGAINST — the property is *that the charge is
    /// what makes three acts three instants*: delete the
    /// `if let Err(e) = self.charge_within_room() { return Turn::Out(e); }`
    /// block from `Session::open_or_close`. Every reply in the walk below is
    /// unchanged and the chest silently stays shut. Confirmed 2026-08-30,
    /// unfiltered over the whole crate —
    /// `864 tests run: 863 passed, 1 failed, 3 skipped`, this test alone, so
    /// nothing else in the tree notices `open`/`close` going free again:
    ///
    /// ```text
    /// opening a lid must cost an instant, or the two facts below cannot be
    /// told apart
    /// ```
    ///
    /// **It reds at the CLOCK assertion, one line in, not at the two
    /// assertions this test was written for**, and that is recorded rather
    /// than tidied. The clock line is a precondition — it is what makes the
    /// three postings distinguishable at all — so the mutation never reaches
    /// the ledger read or the `put`. Those two are still worth their lines
    /// (they are what would catch a charge that advanced the clock while the
    /// commits stayed on the old day), but nothing has demonstrated them, and
    /// saying otherwise would credit a mutation to the wrong assertion.
    #[test]
    fn open_and_close_each_take_an_instant() {
        let world = world_at(1).expect("seed 1 builds");
        let mut session = in_the_store_room_of_seed_one_with_a_key(&world);
        let room = session
            .chamber_facet_here()
            .expect("the walk above ended in a chamber");

        let before = session.day;
        assert_eq!(
            say(&mut session, "open a strongbox"),
            "You open the strongbox. Within it: a key."
        );
        assert!(
            session.day > before,
            "opening a lid must cost an instant, or the two facts below \
             cannot be told apart"
        );
        assert_eq!(
            say(&mut session, "close a strongbox"),
            "You close the strongbox."
        );
        assert_eq!(
            say(&mut session, "open a strongbox"),
            "You open the strongbox. Within it: a key."
        );

        assert!(
            session.container_is_open(&room, hornvale_kernel::KindId("strongbox")),
            "the ledger must agree with the reply: a third posting deduped \
             away leaves the chest shut while `open` says it opened"
        );
        assert_eq!(
            say(&mut session, "put a key in a strongbox"),
            "You put the key in the strongbox.",
            "and the next verb must see the open chest the player is looking \
             at"
        );
    }

    /// **The lock wants a PROPERTY and exactly one kind supplies it** — the
    /// hazard [`LOCKED_WITHOUT_A_KEY_REFUSAL`]'s doc names, made
    /// unmissable from Task 12 rather than left as a sentence.
    ///
    /// [`Session::open_or_close`] refuses a `Lockable` thing unless the body
    /// carries something marked [`crate::affordance::ObjectProperty::
    /// Portable`]. `Lockable` has no payload, so that property is a literal at
    /// the call site, and it is the right literal only while `key` is the
    /// sole carrier. This test asserts that roster by NAME. **Task 12 has
    /// shipped and did not fire it**: it added three verbs that GATE on
    /// `Portable` and no kind that CARRIES it, so the roster is unmoved. The
    /// day a lantern or a coin becomes portable, this reddens before the
    /// world ships a lock every pocket opens.
    ///
    /// **Deliberately not a "count is 1" assertion.** The failure message has
    /// to arrive with the two ways out, because the person who trips it is
    /// mid-way through a different task and the correct fix is not "revert":
    /// give `Lockable` a payload naming the property (or kind) it wants, or
    /// coin a narrower property the key alone carries and read THAT in the
    /// lock arm. Either is a design act; neither is obvious from a bare count.
    ///
    /// MUTATION THIS MUST FAIL AGAINST — the property is *that the roster is
    /// pinned, not merely non-empty*: add `ObjectProperty::Portable` to any
    /// other row of `affordance::object_registry` (`log` and `vessel` are the
    /// plausible ones). Confirmed 2026-08-29, unfiltered; re-confirmed
    /// unfiltered at Task 12, where the same mutation now also reddens
    /// `the_re_key_preserves_every_anchor_kinds_offer` — because `Portable`
    /// gates three verbs since this test was written, so a second carrier
    /// moves that frozen table's row for the newly-portable kind as well.
    #[test]
    fn the_lock_wants_a_property_and_exactly_one_kind_supplies_it() {
        use crate::affordance::{ObjectProperty, object_registry};
        let portable: Vec<&str> = object_registry()
            .iter()
            .filter(|(_, traits)| traits.properties.contains(&ObjectProperty::Portable))
            .map(|(kind, _)| kind.0)
            .collect();
        assert_eq!(
            portable,
            vec!["key"],
            "`open`'s lock arm asks for ObjectProperty::Portable and nothing \
             narrower, so every kind on this roster opens every lock in the \
             world. It was written when the roster was exactly [\"key\"]. It \
             is now {portable:?}, so the lock is no longer a lock. Two ways \
             out, both design acts: give ObjectProperty::Lockable a payload \
             naming what it wants, or coin a narrower property (an opener) \
             that the key alone carries and read that in `open_or_close`."
        );
    }

    /// Opening is durable for the session, and closing undoes it — the
    /// re-closability decision 0367 deferred, now shipped (spec §3.7,
    /// acceptance 3).
    ///
    /// **Driven through the real writer and the real fold**,
    /// [`crate::thing::set_openness`] and [`Session::container_is_open`], on
    /// a live session's own ledger and registry — the same pair
    /// `Session::open_or_close` calls, one frame in. It is written this way
    /// for a reason that has since lapsed and is recorded rather than
    /// silently dropped: when this was written, no world composed a strongbox
    /// into a chamber a possession could stand in, so `handle("open a
    /// strongbox")` could not reach the writer at all. Decision 0398 relaxed
    /// that gate and eight of 48 swept seeds now do
    /// (`tests/suite/strongbox_reachability.rs`).
    ///
    /// **THE SENTENCE THAT USED TO CLOSE THAT PARAGRAPH WAS FALSE, AND ITS
    /// OWN SECOND CLAUSE IS WHY.** It read: *"still the sharper instrument
    /// for the FOUR states below — a played walk reaches only the first two,
    /// since `close` needs a chest the session has already opened and the key
    /// is behind Task 12's `take`."* If the key is behind `take`, a played
    /// walk cannot open anything, so it cannot reach the second state either.
    /// Counted rather than estimated, seed 1's deepest chamber, 2026-08-29:
    ///
    /// ```text
    /// > close a strongbox  ->  The strongbox is already shut.
    /// > open a strongbox   ->  It is locked, and you are carrying nothing that would open it.
    /// ```
    ///
    /// **One state, not two** — shut, reported by the already-shut no-op. A
    /// correction that miscounts the thing it is correcting is the failure
    /// 0398 exists to name, so it is fixed loudly rather than quietly.
    ///
    /// **What actually reaches all four is
    /// `a_key_in_custody_opens_the_strongbox_a_player_walked_to` above**,
    /// which commits a `LOCATED_IN` posting from a room no walk reaches and
    /// then drives every state through [`Session::handle`]. (Since Task 12 a
    /// PURELY played walk reaches them too — `take a key` first — but from
    /// the strongbox's own contents, which is the grammar defect that test's
    /// doc records.) So this test is
    /// no longer the only thing holding the ladder, and it is kept for two
    /// things that one cannot do: it evaluates the fold at an instant EARLIER
    /// than any posting (the last assertion — a verb can only ever ask about
    /// now), and it needs no world to compose a strongbox, so it still holds
    /// if worldgen stops drawing one.
    ///
    /// Four states, not two, and the third is the deliverable: 0367's latch
    /// was MONOTONE — it short-circuited on any clearing fact ever committed
    /// — so under that rule the `close` below could not have lowered
    /// anything and the fourth state would be unreachable.
    ///
    /// The FIRST assertion is the other half: a container nobody has touched
    /// reads shut. That is an authored default living in
    /// `Session::container_is_open`, not in `thing::is_open` (whose `None`
    /// means "whatever the seed drew"), and nothing else in the tree pins it.
    ///
    /// MUTATION THIS MUST FAIL AGAINST — the property is *that the latest
    /// posting wins, not the first*: in
    /// `crate::thing::latest_object_at_or_before`, invert the fold's
    /// comparison, `best.is_none_or(|(seen, _)| seen <= d)` ->
    /// `best.is_none_or(|(seen, _)| seen >= d)`. The first posting then wins
    /// and the chest never closes. Confirmed 2026-08-29, unfiltered:
    ///
    /// ```text
    /// assertion `left == right` failed: after setting open=false the fold
    /// must report false
    ///   left: true
    ///  right: false
    /// ```
    ///
    /// **That run failed SIX tests, not one, and the blast radius is the
    /// honest shape of this evidence rather than a footnote.**
    /// `latest_object_at_or_before` is the one fold `location_of` and
    /// `is_open` are both made of, so the same mutation also reddened
    /// `thing::tests::a_thing_put_back_is_still_here`,
    /// `thing::tests::location_is_read_as_of_the_day_asked_about`,
    /// `thing::tests::openness_is_absent_until_a_fact_says_otherwise`,
    /// `passage::a_cave_mouth_closed_after_it_was_opened_bars_again` and this
    /// task's own `thing::a_holder_holds_what_was_last_put_in_it_...`. A
    /// mutation this broadly caught proves the fold is held; what it does NOT
    /// prove on its own is that anything holds `open`/`close` SPECIFICALLY,
    /// which is what the four-state sequence below is for and what the three
    /// narrower mutations above establish.
    #[test]
    fn a_container_opens_closes_and_re_opens() {
        let world = seam_world();
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
        let room = Facet {
            face: 0,
            path: vec![2],
        };
        let kind = crate::affordance::thing_kind_of(crate::interior::AnchorKind::Strongbox);

        assert!(
            !session.container_is_open(&room, kind),
            "a container nobody has touched reads shut — no generator draws \
             an openness for one, so the fallback is authored at the reader"
        );

        let mut at = session.day;
        for (open, expect) in [(true, true), (false, false), (true, true)] {
            at = WorldTime::from_ticks(at.ticks() + 1);
            crate::thing::set_openness(
                &mut session.ledger,
                &session.registry,
                &room,
                kind.0,
                0,
                open,
                at,
            )
            .expect("a shallow facet packs, and the predicates are registered");
            session.day = at;
            assert_eq!(
                session.container_is_open(&room, kind),
                expect,
                "after setting open={open} the fold must report {expect}"
            );
        }

        // TIME-CORRECT, not a flag: the day before anything happened still
        // reads shut, however many postings came later.
        session.day = WorldTime::from_ticks(at.ticks() - 4);
        assert!(
            !session.container_is_open(&room, kind),
            "a fold evaluated at an earlier instant must not see later postings"
        );
    }

    /// The final whole-branch review's C1: neither test above ever drives
    /// `warm` to SUCCESS. `warm_refuses_with_no_hearth_in_reach` stays out
    /// of doors, where no chamber (and so no `Hearth`) can exist at all;
    /// `warm_is_refused_while_asleep` is turned away by the body-state gate
    /// before [`Self::warm`] ever runs. The success string "You warm
    /// yourself at the fire." occurs exactly once in the whole workspace —
    /// in production code — and was asserted by nothing until this test.
    ///
    /// **Seed 13, not seed 42.** `Terrain::is_cold` is read at a canonical
    /// reference day (`liveness.rs`), so whether a location ever draws the
    /// fire pattern is a fixed fact about that PLACE, not about a session's
    /// current day. Seed 42's flagship sits somewhere that reads warm at
    /// that reference day — `enter; enter further in` there composes an
    /// `Alcove` with no `Hearth` inside it — so it cannot witness this test
    /// at all. Seed 13 was found by probing seeds 1-19 for one whose
    /// flagship's hearthroom actually draws the fire; the precondition
    /// assertion below is what makes that a checked fact of this test rather
    /// than a silent assumption the next campaign could break by changing
    /// worldgen.
    ///
    /// Reaches a real hearth in a real session: `enter` lands at the
    /// threshold chamber (`chamber_index` 0, `Role::Threshold`), which never
    /// carries an alcove or a hearth (`interior/pattern.rs`'s `the-alcove`
    /// pattern is gated `roles: &[Role::Hearthroom]`); `enter further in`
    /// steps to `chamber_index` 1, `Role::Hearthroom`, the one role the fire
    /// pattern's own `Attach::Within(AnchorKind::Alcove)` can ever reach.
    ///
    /// Mutation this must fail against: repoint `Self::warm`'s success gate
    /// at `AnchorKind::Vessel` instead of `AnchorKind::Hearth` — this
    /// reddens (confirmed below) while `warm_refuses_with_no_hearth_in_reach`
    /// and `warm_is_refused_while_asleep` stay green, matching the final
    /// review's own point: nothing but this test can tell the success path
    /// apart from a refusal.
    ///
    /// **Not `AnchorKind::Bed`, and this is a real, checked finding, not an
    /// oversight.** The final review's own illustrative mutation repointed
    /// the gate at `Bed` ("warm... succeeds in bedrooms"). Run against seed
    /// 13's real hearthroom, that swap does NOT redden this test —
    /// `the-fireside-bed` (`interior/pattern.rs`) `requires:
    /// Some(AnchorKind::Hearth)` in the SAME chamber, and `Bed`'s own
    /// `needs_cold` is the same flag as `Hearth`'s, so every chamber that
    /// ever composes a `Hearth` also composes a `Bed`, and vice versa — the
    /// grammar makes the two anchor kinds perfectly co-located in every real
    /// interior. `Vessel` has no such correlation
    /// (`roles: STORING_ROLES` excludes `Role::Hearthroom` outright, so a
    /// `Vessel` anchor can never share a chamber with a `Hearth`), which is
    /// why it discriminates where `Bed` cannot. Verified by applying BOTH
    /// mutations by hand: `Hearth` → `Bed` leaves all ten `warm`-area tests
    /// green (including this one); `Hearth` → `Vessel` reddens exactly this
    /// test. Both runs are pasted in the fix wave's report.
    #[test]
    fn warm_succeeds_at_a_real_hearth_through_a_real_session() {
        let world = world_at(13).expect("seed 13 builds");
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 13 possesses");
        session.handle("enter");
        session.handle("enter further in");
        let inside = session
            .inside
            .as_ref()
            .expect("still indoors after stepping further in");
        let interior = session.chamber_interior_here().expect(
            "a chamber built at the position `enter` stood the possession in \
             must compose an interior",
        );
        assert!(
            interior
                .ids()
                .iter()
                .any(|&a| interior.anchor(a).kind == crate::interior::AnchorKind::Hearth),
            "precondition: chamber_index {} (expected the hearthroom) must \
             carry a real Hearth anchor, or this test proves nothing about \
             warm succeeding: {:?}",
            inside.at,
            interior
                .ids()
                .iter()
                .map(|&a| interior.anchor(a).kind)
                .collect::<Vec<_>>()
        );
        let warmed = match session.handle("warm") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("warm must not release: {t}"),
        };
        assert_eq!(warmed, "You warm yourself at the fire.");
    }

    /// H2. Every one of the eight compass points moves the possession from a
    /// walk-band room. This is the campaign's central claim and the whole of
    /// the availability half of the defect.
    ///
    /// FIRES WHEN: `go` reverts to exact-matching one of the three exits.
    #[test]
    fn every_compass_point_moves_the_possession() {
        // ONE world, eight sessions. Each direction must resolve from the same
        // starting room, so the session is fresh per direction — but genesis is
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
    /// position with an out-of-range path digit (`Facet::pack` rejects
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

    /// The Coercion, Task 1: `possessor_of`'s own coverage. Lives in-module
    /// (not in `windows/vessel/tests/`) because it reaches `session.ledger`
    /// and `session.registry` directly — `Session` gains no public ledger
    /// reader or test-only commit for this, per the task's resolved brief.
    #[test]
    fn a_body_with_no_facts_has_no_possessor() {
        let world = seam_world();
        let (session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        assert_eq!(possessor_of(&session.ledger, session.agent_entity()), None);
    }

    /// `possessed-by` is NOT functional (unlike `TURNED_HOSTILE`): a body may
    /// be possessed, released, and possessed again, so the live state must be
    /// the fold and never a single latest value. The `reopen` fact below is
    /// deliberately given different provenance from `open` — an identical
    /// `Fact` (same subject/predicate/object/place/day/provenance) is an
    /// idempotent no-op under `Ledger::commit`'s dedup, which would silently
    /// defeat exactly the reopen case this test exists to prove.
    #[test]
    fn possession_opens_closes_and_reopens() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let body = session.agent_entity();
        let holder = other_bodies(&session.bodies, session.driven)[0].entity;

        let open = Fact {
            subject: body,
            predicate: POSSESSED_BY.to_string(),
            object: Value::Entity(holder),
            place: None,
            day: None,
            provenance: "test: open".to_string(),
        };
        session
            .ledger
            .commit(open, &session.registry)
            .expect("possessed-by is registered");
        assert_eq!(possessor_of(&session.ledger, body), Some(holder));

        let close = Fact {
            subject: body,
            predicate: POSSESSION_ENDED.to_string(),
            object: Value::Text("released".to_string()),
            place: None,
            day: None,
            provenance: "test: close".to_string(),
        };
        session
            .ledger
            .commit(close, &session.registry)
            .expect("possession-ended is registered");
        assert_eq!(possessor_of(&session.ledger, body), None, "closed");

        let reopen = Fact {
            subject: body,
            predicate: POSSESSED_BY.to_string(),
            object: Value::Entity(holder),
            place: None,
            day: None,
            provenance: "test: reopen".to_string(),
        };
        session
            .ledger
            .commit(reopen, &session.registry)
            .expect("possessed-by is registered");
        assert_eq!(
            possessor_of(&session.ledger, body),
            Some(holder),
            "a body may be possessed again after release — this is why the state \
             is a fold and not a single latest value"
        );
    }

    /// The Coercion, Task 3: proves `body_state` actually consults
    /// `possessor_of` — Task 1 shipped the facts and Task 2 shipped the
    /// `BodyState::PossessedByAnother` row, but nothing derived it yet, so
    /// the gate never saw it. `look` is in-character and commits nothing,
    /// so the assertion lands on the gate rather than a side effect;
    /// `!whoami` is out-of-character and must still answer, or a possessed
    /// body would be indistinguishable from a hung game (spec §2.2).
    ///
    /// Committed in-module, not in `windows/vessel/tests/suite/
    /// possession_facts.rs`, for the same reason Task 1's fold coverage is:
    /// `mod session` is private, so an external test cannot reach
    /// `POSSESSED_BY`/`possessor_of`/`session.ledger` to construct the
    /// state at all — there is no public commit surface for an imposed
    /// possession yet (that is Task 4's OOC verb), and the brief for this
    /// task says plainly not to invent one.
    #[test]
    fn an_imposed_possession_refuses_in_character_and_permits_out_of_character() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let body = session.agent_entity();
        let holder = other_bodies(&session.bodies, session.driven)[0].entity;

        // Baseline: an in-character verb works before anyone takes the body.
        let before = match session.handle("look") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("look must not release: {t}"),
        };
        assert!(!before.is_empty());

        let open = Fact {
            subject: body,
            predicate: POSSESSED_BY.to_string(),
            object: Value::Entity(holder),
            place: None,
            day: None,
            provenance: "test: imposed possession".to_string(),
        };
        session
            .ledger
            .commit(open, &session.registry)
            .expect("possessed-by is registered");

        // In-character now refuses, and names possession specifically...
        let refused = match session.handle("look") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("possession must not release: {t}"),
        };
        assert_ne!(
            refused, before,
            "an in-character verb must not behave identically once the body is held"
        );
        assert!(
            refused.contains("another will holds this body"),
            "the refusal must name possession, not some other reason: {refused}"
        );

        // ...and out-of-character still works, which is the whole point:
        // without it, being possessed is indistinguishable from the game
        // having hung.
        let ooc = match session.handle("!whoami") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("!whoami must not release: {t}"),
        };
        assert!(!ooc.is_empty(), "OOC must still answer while possessed");
    }

    /// The Coercion, Task 3, Step 5: the ordering in `body_state` — possession
    /// checked BEFORE sleep — is a deliberate decision, not an accident of
    /// which `if` came first. A body that is both asleep and possessed must
    /// be refused for possession, not sleep: reporting sleep to a player
    /// whose body was taken names the wrong condition. Reachable through the
    /// shipped surface: `sleep` is a public in-character verb that sets
    /// `wake_at` into the future, and this module can commit the same
    /// `possessed-by` fact Task 1 shipped directly onto `session.ledger`.
    #[test]
    fn a_possessed_and_asleep_body_is_refused_for_possession_not_sleep() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let body = session.agent_entity();
        let holder = other_bodies(&session.bodies, session.driven)[0].entity;

        let slept = match session.handle("sleep") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("sleep must not release: {t}"),
        };
        assert!(
            !slept.starts_with("No verb"),
            "`sleep` must be a verb for this test to mean anything: {slept}"
        );
        assert_eq!(
            session.body_state(),
            BodyState::Asleep,
            "sanity check: asleep alone must gate as asleep"
        );

        let open = Fact {
            subject: body,
            predicate: POSSESSED_BY.to_string(),
            object: Value::Entity(holder),
            place: None,
            day: None,
            provenance: "test: imposed possession while asleep".to_string(),
        };
        session
            .ledger
            .commit(open, &session.registry)
            .expect("possessed-by is registered");

        // The body is now BOTH asleep (wake_at is still in the future) and
        // possessed. The gate must name possession.
        assert_eq!(
            session.body_state(),
            BodyState::PossessedByAnother,
            "a body that is both asleep and possessed must gate as possessed — \
             this is the ordering decision this test exists to pin"
        );

        let refused = match session.handle("look") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("possession must not release: {t}"),
        };
        assert!(
            refused.contains("another will holds this body"),
            "the refusal must name possession, not sleep: {refused}"
        );
        assert!(
            !refused.contains("asleep"),
            "reporting sleep to a player whose body was taken names the wrong \
             condition: {refused}"
        );
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
    fn synthetic_locale() -> Facet {
        Facet {
            face: 3,
            path: (0..12).map(|i| (i % 4) as u8).collect(),
        }
    }

    /// A `Structure` of `count` chambers under `base`, linked as the path graph
    /// rooted at the threshold that `structure_at` builds. Synthetic because
    /// `structure_at`'s own count is a seed draw, and the naming layer must hold
    /// for every count — so these tests choose it rather than hoping for it.
    fn path_structure(base: &Facet, count: usize) -> crate::structure::Structure {
        assert!(
            (1..=crate::structure::MAX_CHAMBERS).contains(&count),
            "a chamber index is one base-4 path digit"
        );
        let chambers: Vec<Facet> = (0..count)
            .map(|i| {
                let mut path = base.path.clone();
                path.extend(std::iter::repeat_n(
                    0u8,
                    crate::depth::CHAMBER_DEPTH_OFFSET as usize,
                ));
                let last = path.len() - 1;
                path[last] = i as u8;
                Facet {
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

    /// A shared helper for the two `examine`'s-datum pins below: a real
    /// seed-42 anchor (never a synthetic one), so both tests exercise the
    /// actual production interior rather than a hand-built stand-in.
    /// Returns the entered session, the anchor's [`crate::interior::AnchorId`]
    /// and its noun.
    fn entered_with_a_named_anchor(
        world: &World,
    ) -> (
        Session<'_>,
        crate::interior::Interior,
        crate::interior::AnchorId,
        &'static str,
    ) {
        let (mut session, _) = Session::start(world, &PossessOpts::default()).unwrap();
        session.handle("enter");
        let inside = session
            .inside
            .as_ref()
            .expect("the flagship's own locale is built");
        let terrain = session.terrain_here();
        let brief = session.brief_here();
        let interior = crate::interior::chamber_interior_of(
            &inside.structure.chambers[inside.at],
            &terrain,
            session.walk_depth(),
            &brief,
            inside.at,
        );
        let id = interior
            .ids()
            .into_iter()
            .find(|&id| crate::chamber_prose::noun(interior.anchor(id).kind).is_some())
            .expect("a built chamber names at least one anchor");
        let noun = crate::chamber_prose::noun(interior.anchor(id).kind)
            .expect("checked Some by the find above");
        (session, interior, id, noun)
    }

    /// The Offer, Task 7 (spec §4, §10.1): pins `examine_chamber`'s
    /// anchor-detail reply BEFORE it is routed through
    /// [`crate::affordance::offered_to_observer`] — the equality the spec's
    /// own risk note asks for: "the behaviour is already shipped and already
    /// tested; what changes is that one derivation feeds it." Must still
    /// hold, byte for byte, after the re-point below, because the knowledge
    /// gate can never fire through a live `Session` (Task 4's own finding).
    ///
    /// Mutation this must fail against: replacing `examine_chamber`'s anchor
    /// branch with a bare refusal.
    ///
    /// **Folded in from the final review's minor M-c.** A second test,
    /// `examine_chamber_anchor_reply_pin_can_fail`, used to run the SAME
    /// mutation by hand and assert only `assert_ne!` against the refusal
    /// sentence — a strict superset of the equality already asserted below
    /// (its own doc said as much: "this test itself asserts nothing new").
    /// Folded here rather than kept as a second function.
    #[test]
    fn examine_chamber_anchor_reply_is_pinned_before_the_offer_gate() {
        let world = seam_world();
        let (session, interior, id, noun) = entered_with_a_named_anchor(&world);
        let reply = session.examine_chamber(noun, Perceiving::Body);
        // A mutation that always refuses would make this equal the refusal
        // sentence instead of the real detail — the two must differ, or the
        // equality below could not tell the two apart. Confirmed by
        // temporarily editing the anchor branch to `return format!("You see
        // no {noun} here.");` and re-running this test, which failed with
        // the expected diff, then restoring the branch.
        assert_ne!(reply, format!("You see no {noun} here."));
        assert_eq!(
            reply,
            // `false` is the right expectation and not a guess: nothing in
            // this test opens anything, and `container_is_open`'s authored
            // default for a container with no `openness` fact is shut. It is
            // also inert for the anchor this fixture reaches — the threshold
            // chamber's first nouned anchor, which carries no `Openable`.
            crate::chamber_prose::examine_detail(&interior, id, false),
            "examine_chamber's anchor reply must equal chamber_prose's own \
             examine_detail — the pin this task's re-point must not move"
        );
    }

    /// The final whole-branch review's C2: the pin above is tautological
    /// (both sides route through [`crate::chamber_prose::examine_detail`])
    /// and it never reaches the one branch Task 6 added — `entered_with_a_
    /// named_anchor` takes the FIRST nouned anchor of the chamber `enter`
    /// lands in, which is always `chamber_index` 0 (`Role::Threshold`,
    /// `pattern.rs`:415); the alcove Task 6's `within` relation actually
    /// names is drawn only for `chamber_index` 1 (`Role::Hearthroom`,
    /// `pattern.rs`:178). So no test exercised the production seam at all.
    ///
    /// **Seed 13, not seed 42** — same reason as `warm_succeeds_at_a_real_
    /// hearth_through_a_real_session`: `Terrain::is_cold` is a fixed fact
    /// about a PLACE (read at a canonical day), and seed 42's flagship reads
    /// warm at that day, so its hearthroom draws an alcove with nothing in
    /// it. Seed 13's does not.
    ///
    /// Walks to the real hearthroom (`enter; enter further in`, exactly as
    /// the `warm` test does) and examines the alcove directly, asserting on
    /// the literal `"Within it:"` clause `chamber_prose::examine_detail`
    /// only emits when [`crate::affordance::ObjectProperty::Encloses`] is
    /// carried AND something composes `within` the anchor — never comparing
    /// against `examine_detail`'s own output, so this cannot be tautological
    /// the way the pin above is.
    ///
    /// Mutation this must fail against: revert `examine_chamber`'s anchor
    /// branch (session.rs) from `chamber_prose::examine_detail(&interior,
    /// id)` back to `chamber_prose::detail(kind)` — the exact mutation the
    /// final review ran, which left 700/700 tests green while `examine
    /// alcove` silently stopped naming the hearth within it. Confirmed
    /// below.
    #[test]
    fn examine_chamber_names_the_hearth_within_the_real_hearthroom_alcove() {
        let world = world_at(13).expect("seed 13 builds");
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 13 possesses");
        session.handle("enter");
        session.handle("enter further in");
        let interior = session.chamber_interior_here().expect(
            "a chamber built at the position `enter further in` stood the \
             possession in must compose an interior",
        );
        let alcove_id = interior
            .ids()
            .into_iter()
            .find(|&id| interior.anchor(id).kind == crate::interior::AnchorKind::Alcove)
            .expect(
                "precondition: seed 13's hearthroom must compose an Alcove, \
                 or this test proves nothing about the within-relation",
            );
        assert!(
            interior.anchor(alcove_id).within.is_none()
                && interior.ids().into_iter().any(|id| interior.anchor(id).kind
                    == crate::interior::AnchorKind::Hearth
                    && interior.anchor(id).within == Some(alcove_id)),
            "precondition: seed 13's alcove must have a real Hearth composed \
             WITHIN it (the-fire's own Attach::Within(AnchorKind::Alcove)), \
             or the within-relation this test checks does not exist here"
        );
        let alcove_noun = crate::chamber_prose::noun(crate::interior::AnchorKind::Alcove)
            .expect("Alcove always names a noun");
        let reply = match session.handle(&format!("examine {alcove_noun}")) {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("examine must not release: {t}"),
        };
        assert!(
            reply.contains("Within it:"),
            "examining the alcove must name what the grammar placed within \
             it (the hearth): {reply:?}"
        );
        assert!(
            reply.contains("a hearth"),
            "the within-clause must name the hearth specifically: {reply:?}"
        );
    }

    /// The Offer, Task 7 (spec §3.5/§4): [`crate::affordance::
    /// offered_to_observer`] can never deny through a LIVE `Session` (Task
    /// 4's own finding — knowledge absorption is unconditional before the
    /// first turn), so the only way to observe this gate firing at all is to
    /// manufacture the failing `Knowledge` directly — the same technique
    /// `affordance.rs`'s own `an_unencountered_object_offers_nothing` uses
    /// with a synthetic value.
    ///
    /// **Written and run RED against the pre-Task-7 `examine_chamber`**: it
    /// does not consult `self.knowledge` at all, so wiping it changes
    /// nothing and this assertion fails. The re-point below is what turns it
    /// green — the live proof that `examine`'s datum now derives from
    /// [`crate::affordance::offered_to_observer`], not merely from the
    /// anchor's own kind.
    #[test]
    fn examine_chamber_anchor_is_refused_when_the_observer_has_no_recorded_knowledge() {
        let world = seam_world();
        let (mut session, _interior, _id, noun) = entered_with_a_named_anchor(&world);
        // Manufacture the one state a live Session can never reach on its
        // own (Task 4's own finding): an observer who has recorded no room
        // at all.
        session.knowledge = Knowledge::default();
        let reply = session.examine_chamber(noun, Perceiving::Body);
        assert_eq!(reply, format!("You see no {noun} here."));
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
            let locale = Facet {
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

    /// Every cave-bearing vertex in `terrain`, paired with whether its entrance
    /// address (`branch = 0, band = 0, floor = 0`) resolves to a chamber.
    /// Scans the terrain directly (`GeneratedTerrain::cave_at`) rather than
    /// steering a walk there: a terrain vertex spans many walk-band rooms
    /// (measured while developing The Deep Realm — dozens to low hundreds of
    /// `go` steps per terrain-vertex crossing), so a walk cannot be relied on
    /// to land on a chosen vertex. Direct scanning is what
    /// `windows/worldgen/tests/deep_realm_substrate.rs` (Task 0) and
    /// `deep_realm_chamber.rs` (Tasks 2-3) already do for the same reason.
    ///
    /// Shared by [`find_open_cave_vertex`] (which stops at the first open hit)
    /// and `delve_has_three_distinguishable_outcomes`'s exhaustive
    /// chamber-realization scan (The Drift, Task 3b), which does not stop
    /// early — one derivation for both, so the two can never quietly
    /// disagree about what "realized" means.
    ///
    /// **Addresses come from [`cave_entrance_addr`], the same constructor
    /// production uses** (The Latch, Task 7). This helper built its own
    /// `ChamberAddr` literal until then. The two literals agreed field for
    /// field, so nothing was wrong — but `addr_key`'s injectivity guarantee
    /// only holds between addresses that are genuinely equal, so a test
    /// helper deriving the address independently of production is exactly
    /// how a later `ChamberAddr` field change would go unnoticed: the helper
    /// would keep answering about one address while the verb under test
    /// asked about another.
    fn cave_entrance_states<'a>(
        terrain: &'a hornvale_terrain::GeneratedTerrain,
        seed: Seed,
    ) -> impl Iterator<Item = (hornvale_kernel::Vertex, hornvale_terrain::Cave, bool)> + 'a {
        let overrides = hornvale_worldgen::chamber::ChamberOverrides::new();
        terrain.geosphere().vertices().filter_map(move |vertex| {
            if terrain.is_ocean(vertex) {
                return None;
            }
            let cave = terrain.cave_at(vertex)?;
            let addr = cave_entrance_addr(vertex);
            let is_open = hornvale_worldgen::chamber::chamber_at(
                seed,
                &cave,
                terrain.geothermal_gradient_at(vertex),
                &terrain.column_at(vertex),
                addr,
                &overrides,
            )
            .is_some();
            Some((vertex, cave, is_open))
        })
    }

    /// The SEEDED barrier at a vertex's cave-entrance address — the address
    /// coming from [`cave_entrance_addr`], the same constructor production
    /// uses, rather than from a second copy of its fields.
    ///
    /// **The Latch's fix wave, closing the other half of Task 7's own
    /// finding.** Task 7 repointed [`cave_entrance_states`] at
    /// `cave_entrance_addr` for exactly this reason and left four barrier
    /// reads in this module still spelling `Band::Undercroft, 0` inline;
    /// this collapses all four onto one derivation. The hazard is the same
    /// one that helper's doc names and it is quieter here, because
    /// [`hornvale_worldgen::barrier_of`] takes loose `(vertex, band,
    /// branch)` arguments rather than a `ChamberAddr`: a change to where a
    /// cave's entrance sits would leave these reads type-checking and
    /// silently answering about a DIFFERENT address than `delve_at` and
    /// `clear_passage_at` ask about, so every fixture would still be found
    /// and every assertion would still be about the wrong place.
    ///
    /// `barrier_of` consults `vertex`/`band`/`branch` and not `level`, so
    /// only three of the address's four fields reach it. That is the
    /// function's own business, not this helper's: passing the fields off
    /// the shared address is what keeps the two in step whichever fields
    /// `barrier_of` grows.
    fn seeded_entrance_barrier(
        seed: Seed,
        vertex: hornvale_kernel::Vertex,
        pins: &hornvale_worldgen::BarrierPins,
    ) -> hornvale_worldgen::BarrierState {
        let addr = cave_entrance_addr(vertex);
        hornvale_worldgen::barrier_of(seed, addr.vertex, addr.band, addr.branch, pins)
    }

    /// The first cave-bearing vertex this seed's terrain places whose entrance
    /// chamber is realized AND whose seeded barrier is `Open`. Until The
    /// Drift (Task 1) deleted `chamber_exists`'s 50% existence coin, this
    /// function also took a `want_open` flag and could be asked for the
    /// SEALED counterpart instead; that outcome is no longer reachable
    /// (`delve_has_three_distinguishable_outcomes`'s doc comment records
    /// why), so the flag is gone rather than kept as a parameter nothing
    /// ever satisfies.
    ///
    /// **The Latch (Task 4) added the barrier filter.** Before restricted
    /// passage landed, chamber realization was the whole story (every
    /// cave-bearing vertex resolves, post-Drift), so the first such vertex
    /// was as good as any for every caller that wants a WORKING descent.
    /// That stopped being true the moment `delve_at` started gating on
    /// `effective_state`: seed 42's terrain barred 639 of 874 cave mouths
    /// (Task 1's own measurement), so the first cave-bearing vertex in scan
    /// order is barred more often than not — confirmed directly, it is
    /// (`Vertex(30)`, `Warded`). Every caller of this finder wants a
    /// descent that actually succeeds, so "open" now means both facts, not
    /// just the one this function used to check alone.
    fn find_open_cave_vertex(
        terrain: &hornvale_terrain::GeneratedTerrain,
        seed: Seed,
    ) -> (hornvale_kernel::Vertex, hornvale_terrain::Cave) {
        let pins = hornvale_worldgen::BarrierPins::default();
        cave_entrance_states(terrain, seed)
            .find_map(|(vertex, cave, is_open)| {
                let unbarred = seeded_entrance_barrier(seed, vertex, &pins)
                    == hornvale_worldgen::BarrierState::Open;
                (is_open && unbarred).then_some((vertex, cave))
            })
            .unwrap_or_else(|| {
                panic!(
                    "no open, unbarred cave found in seed 42's terrain — the fixture no \
                     longer has one of the outcomes this campaign's descent verb needs to \
                     distinguish"
                )
            })
    }

    /// The first cave-bearing vertex whose seeded barrier is not `Open` — the
    /// third outcome's own fixture-independent finder, mirroring
    /// [`Self::find_open_cave_vertex`]. Scans every cave-bearing vertex in
    /// the fixture terrain rather than assuming a particular one is barred
    /// — a hard-coded vertex is a contingency an epoch can falsify, the same
    /// reason [`Self::find_open_cave_vertex`]'s own doc comment gives for
    /// scanning rather than steering a walk there.
    ///
    /// Reuses `windows/vessel/tests/suite/passage.rs`'s Task 1 loop rather
    /// than [`cave_entrance_states`]: that helper answers whether an entrance
    /// CHAMBER resolves, a question The Drift made unconditionally true; this
    /// answers whether the BARRIER at the same address is `Open`, an
    /// independent draw `hornvale_worldgen::barrier_of` makes.
    fn find_barred_cave_vertex(
        terrain: &hornvale_terrain::GeneratedTerrain,
        seed: Seed,
    ) -> (hornvale_kernel::Vertex, hornvale_terrain::Cave) {
        let pins = hornvale_worldgen::BarrierPins::default();
        terrain
            .geosphere()
            .vertices()
            .filter_map(|vertex| {
                if terrain.is_ocean(vertex) {
                    return None;
                }
                let cave = terrain.cave_at(vertex)?;
                let barrier = seeded_entrance_barrier(seed, vertex, &pins);
                (barrier != hornvale_worldgen::BarrierState::Open).then_some((vertex, cave))
            })
            .next()
            .unwrap_or_else(|| {
                panic!(
                    "no barred cave found in seed 42's terrain — the fixture no longer has \
                     the barred outcome this campaign's descent verb needs to distinguish"
                )
            })
    }

    /// The first cave-bearing vertex whose seeded barrier is EXACTLY `want` —
    /// The Latch, Task 5's own finder, sharper than
    /// [`Self::find_barred_cave_vertex`] (which accepts any non-`Open`
    /// state). `clear_passage_at` distinguishes `Thin` from `Sealed`/
    /// `Warded` (only the former yields), so a test exercising that
    /// distinction needs a vertex known to carry ONE specific state, not
    /// merely "some barred state" — the same scanning discipline
    /// [`Self::find_barred_cave_vertex`]'s own doc gives for why a
    /// hard-coded vertex is unsafe against a future terrain epoch.
    fn find_cave_vertex_with_barrier(
        terrain: &hornvale_terrain::GeneratedTerrain,
        seed: Seed,
        want: hornvale_worldgen::BarrierState,
    ) -> (hornvale_kernel::Vertex, hornvale_terrain::Cave) {
        let pins = hornvale_worldgen::BarrierPins::default();
        terrain
            .geosphere()
            .vertices()
            .filter_map(|vertex| {
                if terrain.is_ocean(vertex) {
                    return None;
                }
                let cave = terrain.cave_at(vertex)?;
                let barrier = seeded_entrance_barrier(seed, vertex, &pins);
                (barrier == want).then_some((vertex, cave))
            })
            .next()
            .unwrap_or_else(|| {
                panic!(
                    "no cave found in seed 42's terrain with barrier {want:?} — the fixture \
                     no longer has the outcome this campaign's clearing verb needs to \
                     distinguish"
                )
            })
    }

    /// **The sixteen strings the two passage verbs can say must be pairwise
    /// distinct.** A refusal that reads like another refusal is a refusal that
    /// tells the player nothing — `barred_refusal`'s own doc gives that
    /// argument for one pair of them ("a refusal that named no reason would be
    /// indistinguishable from the no-cave one") and
    /// `clearing_a_warded_passage_names_the_ward_not_the_rubble` gives it for
    /// a second pair. This is the same argument applied to the whole
    /// vocabulary at once.
    ///
    /// **THIS CHECK DID NOT EXIST BEFORE TASK 8, AND THE BRIEF THAT ASKED FOR
    /// IT SAID IT DID.** The instruction was to extend a check that "verified
    /// the four refusal strings pairwise distinct once"; nothing in this crate
    /// did that — `delve_has_three_distinguishable_outcomes` compares three
    /// whole TURN OUTPUTS for one barrier state, and the two `clear` tests
    /// each assert on a substring. So this was written rather than extended,
    /// which is worth recording because a check believed to exist is weaker
    /// than one known not to: nobody re-derives it.
    ///
    /// **IT THEN SHIPPED SWEEPING NINE OF THE SIXTEEN AND CLAIMING ALL OF
    /// THEM, WHICH IS THE SAME LESSON ONE TURN LATER.** Task 8's own first
    /// line here read "the nine strings the two passage verbs can say";
    /// grepping the two verbs finds sixteen. Fix round 1's review DEMONSTRATED
    /// the cost instead of asserting it: making `clear_passage_column`'s
    /// no-cave refusal byte-identical to `delve_column`'s left all 831 vessel
    /// tests green — and `barred_refusal`'s doc argues distinctness
    /// specifically against the no-cave refusal, so the single string that
    /// argument names by hand was the one string the sweep did not hold. The
    /// seven that were missing are both verbs' three footing refusals and the
    /// descent line.
    ///
    /// **The sweep reads the production sites, not copies of them**, which is
    /// why those seven are consts ([`NO_ROCK_INSIDE_REFUSAL`] and its
    /// neighbours) rather than literals re-typed into this test. A widened
    /// sweep over hand-copied literals would have stayed green against the
    /// very collision that exposed the gap: the copy and the call site are two
    /// objects, and only one of them moves.
    ///
    /// **Fifteen exact strings and one PREFIX, which is all the sixteenth can
    /// honestly contribute.** `delve_at`'s success line carries a format hole
    /// filled by [`stratum_word`], so it is not one string but eleven.
    /// [`DESCENT_PREFIX`] — its fixed half — is what this sweep holds, plus a
    /// `starts_with` check below that no other outcome begins with it; the two
    /// together imply every filling is distinct from every other outcome.
    /// Sweeping all eleven fillings instead was rejected deliberately: it
    /// would redden this test the day two STRATA came to share a word, which
    /// is a different table doing a different job, and a passage test that
    /// fails for a stratigraphy reason is the failure mode fix round 1's own
    /// m5 finding names.
    ///
    /// **Sixteen, and the enumeration is by hand for the two tables and by
    /// const for the rest.** `barred_refusal` and `clear_response` each answer
    /// four [`hornvale_worldgen::BarrierState`]s, written out here because the
    /// enum is not `all()`-bearing; a fifth variant would not automatically
    /// appear, but it would redden both `match`es at compile time, which is
    /// the compiler doing the enumeration this test cannot.
    /// [`UNREALIZED_CHAMBER_REFUSAL`] is the ninth and was an inline `match`
    /// arm until Task 8 — unreachable for any assertion to name, which is
    /// exactly how a collision with it would have gone unnoticed.
    ///
    /// MUTATION this must fail against (fix round 1, reproducing the review's
    /// own collision): give [`NO_CAVE_MOUTH_TO_CLEAR_REFUSAL`] the text of
    /// [`NO_CAVE_TO_DELVE_REFUSAL`], so `clear` and `delve` refuse a
    /// cave-less vertex in identical words. Confirmed 2026-08-29:
    ///
    /// ```text
    /// assertion `left == right` failed: two passage outcomes read
    /// identically — a player cannot tell them apart. All sixteen: [
    ///     ...,
    ///     "There is no cave here to delve into.",
    ///     ...,
    ///     "There is no cave here to delve into.",
    ///     ...,
    /// ]
    ///   left: 15
    ///  right: 16
    /// ```
    ///
    /// The earlier, narrower mutation is kept as a second witness because it
    /// exercises the two TABLES rather than the consts: in `clear_response`,
    /// give the `Open` arm `barred_refusal`'s `Open` text ("The way down is
    /// open.") — the most plausible real collision, since the two tables
    /// mirror each other's shape by design and that pair is the only one whose
    /// meanings are genuinely close. Confirmed 2026-08-29 the same way, `left:
    /// 15  right: 16`.
    ///
    /// **The duplicate is printed, which is the reason the message carries
    /// the whole list rather than just the counts.** `15 != 16` alone names no
    /// culprit, and a reader who has to go re-derive which two collided is a
    /// reader who will not.
    ///
    /// Genuine behavioural reds, not compile errors; restored and re-run
    /// on a fresh binary, green.
    #[test]
    fn every_passage_outcome_reads_distinctly() {
        let states = [
            hornvale_worldgen::BarrierState::Sealed,
            hornvale_worldgen::BarrierState::Warded,
            hornvale_worldgen::BarrierState::Thin,
            hornvale_worldgen::BarrierState::Open,
        ];
        let mut said: Vec<String> = Vec::new();
        for state in states {
            said.push(barred_refusal(state));
            said.push(clear_response(state));
        }
        said.push(UNREALIZED_CHAMBER_REFUSAL.to_string());
        // The seven the sweep did not hold until fix round 1 — both verbs'
        // footing refusals, and the fixed half of the descent line.
        said.push(NO_ROCK_INSIDE_REFUSAL.to_string());
        said.push(ALREADY_BELOW_DELVE_REFUSAL.to_string());
        said.push(NO_CAVE_TO_DELVE_REFUSAL.to_string());
        said.push(NOTHING_TO_CLEAR_INSIDE_REFUSAL.to_string());
        said.push(ALREADY_BELOW_CLEAR_REFUSAL.to_string());
        said.push(NO_CAVE_MOUTH_TO_CLEAR_REFUSAL.to_string());
        said.push(DESCENT_PREFIX.to_string());

        assert_eq!(
            said.len(),
            16,
            "non-vacuous guard: the sweep must actually collect every string \
             the two verbs can say, or the uniqueness check below passes by \
             comparing nothing"
        );
        let unique: std::collections::BTreeSet<&String> = said.iter().collect();
        assert_eq!(
            unique.len(),
            said.len(),
            "two passage outcomes read identically — a player cannot tell \
             them apart. All sixteen: {said:#?}"
        );

        // Every one must also NAME something. An empty or whitespace-only
        // refusal is trivially distinct from the other fifteen and tells a
        // player nothing at all, so uniqueness alone would not catch it.
        for line in &said {
            assert!(
                line.len() > 20,
                "a passage outcome must say something: {line:?}"
            );
        }

        // The sixteenth is a PREFIX, so exact-string uniqueness above says
        // nothing about the eleven lines it actually renders into. This is
        // what carries the gap: no other outcome may BEGIN with it, or some
        // stratum filling could read as that outcome plus a trailing phrase.
        for line in &said {
            if line == DESCENT_PREFIX {
                continue;
            }
            assert!(
                !line.starts_with(DESCENT_PREFIX),
                "a passage outcome begins with the descent line's fixed half, \
                 so some stratum filling of it reads as this outcome: {line:?}"
            );
        }
    }

    /// The Deep Realm, Task 5 shipped `delve` with THREE distinguishable
    /// outcomes: no cave, a cave whose entrance chamber resolves to nothing
    /// (spec §3.4 rung 0, "the void exists and is unreachable," a real fact
    /// a later dig could find, not a defect), and a cave with a resolved
    /// chamber. The Drift's §4.1 (Task 1) then deleted the 50% existence
    /// coin `chamber_exists` gated on, so every cave-bearing vertex's
    /// entrance chamber realizes unconditionally — measured directly,
    /// `systems_with_open_mouth == systems` on all three panel seeds
    /// (874/874, 1681/1681, 1266/1266) — and that chamber-unrealized
    /// outcome went from rare to **impossible**. This test carried the name
    /// `delve_has_two_distinguishable_outcomes` from then until The Latch,
    /// with a two-directional tripwire (the discipline `seam-guard`'s
    /// STALE-DECL verdict names: a one-directional acknowledgement can only
    /// ever be satisfied, so it rots) that re-scanned every cave-bearing
    /// vertex on every run and would redden the moment a chamber-unrealized
    /// cave became possible again. **That specific outcome is still
    /// unreachable** — nothing in this campaign touches `chamber_exists`,
    /// and `delve_at`'s `None` arm stays live, unreachable code for the
    /// reason its own doc comment gives.
    ///
    /// **The Latch (Task 4) restored a third outcome through a different
    /// door: the barrier, not the chamber.** `delve_at` now consults
    /// `crate::passage::effective_state` before it ever calls `chamber_at`,
    /// so a cave whose seeded [`hornvale_worldgen::BarrierState`] is not
    /// `Open` refuses descent even though its entrance chamber would
    /// resolve fine. Task 1's own census
    /// (`windows/vessel/tests/suite/passage.rs`) measured 639 of seed 42's
    /// 874 cave-bearing vertices barred (sealed=215 warded=209 thin=215) —
    /// the third outcome is the MAJORITY case, not a corner: the very
    /// first cave-bearing vertex in scan order (`Vertex(30)`) is itself
    /// `Warded`, which is exactly what caught that
    /// [`Self::find_open_cave_vertex`] needed its own barrier filter (its
    /// doc comment records this).
    ///
    /// **Renamed from `delve_has_two_distinguishable_outcomes`,** whose own
    /// doc comment predicted this exact rename ("forcing whoever ships
    /// restricted passage to come rename this test back") — though what
    /// actually reddened first, once `delve_at`'s barrier gate landed, was
    /// not the chamber-realization scan (untouched, still zero) but this
    /// test's own "outcome 2" assertion, which had been silently relying on
    /// the first chamber-realized vertex also being unbarred. **Keeps the
    /// scan**, repointed at the fact that is now load-bearing: every
    /// cave-bearing vertex's barrier is re-derived from
    /// [`hornvale_worldgen::barrier_of`] on every run, never assumed from
    /// [`Self::find_barred_cave_vertex`]'s own panic alone, so a fixture
    /// accident — the barred population going to zero under some future
    /// terrain epoch — cannot leave this test looking satisfied while
    /// silently testing nothing.
    ///
    /// MUTATION this must fail against (added by the fix wave; the review's
    /// M4 noted this test alone among the campaign's new ones named none,
    /// and "its red was corroborated mechanically when the gate landed" is a
    /// weaker record than the neighbouring tests carry): delete
    /// [`Session::delve_at`]'s barrier gate — the
    /// `if barrier != BarrierState::Open { return Turn::Out(barred_refusal(
    /// barrier)) }` block — which is precisely the third outcome this test's
    /// name claims. The gate is what The Latch added; without it a barred
    /// mouth descends like an open one, because chamber realization is
    /// unconditional post-Drift.
    ///
    /// Confirmed 2026-08-28, with that block replaced by `let _ = barrier;`
    /// (deleting it outright leaves `barrier` unbound; the discard is the
    /// same behaviour and still compiles):
    ///
    /// ```text
    /// a barred passage must not set the underground state: You worm down
    /// into the dark. The rock here is the regolith.
    /// ```
    ///
    /// — outcome 2's own assertion, firing because the barred vertex
    /// descended and rendered its chamber. A genuine behavioural red, not a
    /// compile error; restored and re-run on a fresh binary, green.
    ///
    /// **What that mutation does NOT reach, stated so the record is not
    /// read as wider than it is:** the exhaustive scan at the end of this
    /// test is a pure read over `barrier_of` and `chamber_at` and is
    /// untouched by any change to `delve_at`. Its own guard against going
    /// vacuous is the `caves_examined > 0` assertion, not this mutation.
    #[test]
    fn delve_has_three_distinguishable_outcomes() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");

        // Outcome 1: no cave at all — asserted through `delve_column(None)`,
        // the branch production reaches when `chamber_column_here` finds
        // nothing. This read the flagship's own STARTING VERTEX until The
        // Glasshouse (commit 14aa6fbab, 2026-08-14), a convenience resting on
        // the contingency that that one vertex happened to be cave-free; that
        // campaign's terrain epoch put a sealed cave under it and falsified
        // that, and `delve_column` was split out of `delve` so the branch is
        // reached directly instead. The other outcomes are found by scanning
        // rather than assumed, so this leaves the test independent of where
        // the flagship happens to stand.
        //
        // NO DECISION COVERS THIS, and the line said "decision 0131" until The
        // Latch (Task 7) checked it. 0131 is "refuted is a seventh registry
        // status" and 0134 is "a partition statistic refuted by its own
        // mechanism is retired" — neither is about a terrain epoch or a
        // contingent test subject. The event was a test re-pin inside a
        // campaign, so it is cited as one; a commit is a fact, and inventing a
        // nearer-looking number would only have moved the error.
        let no_cave = match session.delve_column(None) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("delve must not release"),
        };
        assert!(no_cave.contains("no cave here"), "{no_cave}");
        assert!(
            session.underground.is_none(),
            "a refused delve must not change the underground state"
        );

        // Outcome 2: a barred passage — descent is refused, naming the
        // barrier, and the underground state must not move.
        let (barred_vertex, barred_cave) = find_barred_cave_vertex(&terrain, world.seed);
        let barred = match session.delve_at(barred_vertex, barred_cave) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("delve must not release"),
        };
        assert!(
            session.underground.is_none(),
            "a barred passage must not set the underground state: {barred}"
        );

        // Outcome 3: an open, unbarred chamber — descend, and `climb`
        // returns.
        let (open_vertex, open_cave) = find_open_cave_vertex(&terrain, world.seed);
        let open = match session.delve_at(open_vertex, open_cave) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("delve must not release"),
        };
        assert!(
            session.underground.is_some(),
            "a resolved, unbarred entrance chamber must set the underground state: {open}"
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

        // The whole point: all three live outcomes must be told apart.
        assert_ne!(
            no_cave, barred,
            "no-cave and a barred refusal read identically"
        );
        assert_ne!(
            no_cave, open,
            "no-cave and a successful descent read identically"
        );
        assert_ne!(
            barred, open,
            "a barred refusal and a successful descent read identically"
        );

        // One exhaustive scan over every cave-bearing vertex, carrying BOTH
        // guards the prior two-outcome test carried separately — chamber
        // realization (unchanged by this task, still checked so a future
        // regression there is still caught here) and the barrier (new).
        // Non-vacuous by construction and re-derived on every run rather
        // than trusted from `find_barred_cave_vertex`'s own panic alone: at
        // least one cave-bearing vertex must carry a non-`Open` barrier, or
        // outcome 2 above would have exercised a vertex this scan never
        // independently confirmed is representative of anything. Scoped to
        // this one seed rather than a multi-seed panel for the same reason
        // the prior version of this test was: the exhaustive scan already
        // touches every cave-bearing vertex this fixture has, and building
        // further whole worlds to widen it would push this test toward the
        // heavy tier — the workspace gate never runs that tier, which would
        // defeat the point of pinning this guard where it actually runs.
        let pins = hornvale_worldgen::BarrierPins::default();
        let mut caves_examined = 0usize;
        let mut barred_count = 0usize;
        let mut chamber_unrealized: Vec<hornvale_kernel::Vertex> = Vec::new();
        for (vertex, _cave, is_open) in cave_entrance_states(&terrain, world.seed) {
            caves_examined += 1;
            if !is_open {
                chamber_unrealized.push(vertex);
            }
            let barrier = seeded_entrance_barrier(world.seed, vertex, &pins);
            if barrier != hornvale_worldgen::BarrierState::Open {
                barred_count += 1;
            }
        }
        assert!(
            caves_examined > 0,
            "non-vacuous guard: seed 42's terrain must contain at least one \
             cave-bearing vertex, or the scans below would pass by finding \
             nothing rather than by finding the world genuinely connected and \
             genuinely barred somewhere"
        );
        assert!(
            chamber_unrealized.is_empty(),
            "a chamber-unrealized cave exists again ({} of {caves_examined} \
             cave-bearing vertices examined, e.g. vertex {:?}) — `delve_at`'s `None` \
             arm is reachable once more and its doc comment (and this test's) needs \
             updating to say so",
            chamber_unrealized.len(),
            chamber_unrealized[0],
        );
        assert!(
            barred_count > 0,
            "no barred cave mouth exists in seed 42's terrain of {caves_examined} \
             cave-bearing vertices examined — restricted passage's third outcome has \
             gone unreachable again"
        );
    }

    /// The Gallery, Task 12 (spec §6 acceptance criterion 9): the campaign's
    /// constitutional property, stated once at the end rather than assumed
    /// throughout. Every prior task in this campaign asserted a behaviour
    /// (a refusal reads correctly, fog is monotone, an inhabitant is
    /// derived); none asserted that two INDEPENDENT builds of the same seed
    /// and pins, driven through the identical script, stay byte-identical
    /// at every turn — not merely at the end, which would pass even if the
    /// two diverged mid-descent and reconverged by coincidence.
    ///
    /// **Two separately-built `World`s**, not one `World` shared by two
    /// `Session`s: sharing a `World` would prove the session-level fold is
    /// deterministic given identical inputs, but would leave any
    /// nondeterminism inside world genesis itself (terrain, the cave
    /// lattice) unexercised, since both sessions would be reading the exact
    /// same in-memory value rather than two independently-derived ones.
    ///
    /// `find_open_cave_vertex` is called on EACH terrain independently and
    /// the two answers are asserted equal before either session delves —
    /// a determinism check on the finder itself, and the reason a later
    /// snapshot mismatch could not be blamed on picking two different
    /// entrances.
    ///
    /// The script is concrete to seed 42's fixture rather than a generic
    /// blind sweep, and was found by running it and reading what the
    /// narration actually said (`Underground::enter` places the possession
    /// at the first standable cell in `Cell` order, which is not guaranteed
    /// to be a stairs cell — unlike a rung's connectivity PROBE, which
    /// measures from the entrance by convention, see the plan's F3):
    /// stepping `n` from the entrance happens to land on a stairs-down cell
    /// here, so `down` actually descends; the walk on rung 1 happens to
    /// cross a stairs-up cell, so `up` actually returns. The script therefore
    /// exercises a real multi-rung round trip — delve, a lateral step, a
    /// real descent, three more lateral steps, a real ascent, climb — not
    /// merely a sequence of blind attempts, and the two sessions' snapshot
    /// bytes are compared after every single line.
    #[test]
    fn the_same_seed_and_pins_produce_a_byte_identical_descent_and_pane() {
        fn world_at_seed_42() -> World {
            build_world(
                Seed(42),
                &SkyPins::default(),
                SkyChoice::Generated,
                &TerrainPins::default(),
                &SettlementPins::default(),
            )
            .expect("seed 42 builds")
        }

        let world_a = world_at_seed_42();
        let world_b = world_at_seed_42();

        let (mut session_a, _) = Session::start(&world_a, &PossessOpts::default()).unwrap();
        let (mut session_b, _) = Session::start(&world_b, &PossessOpts::default()).unwrap();

        let terrain_a = session_a
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let terrain_b = session_b
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex_a, cave_a) = find_open_cave_vertex(&terrain_a, world_a.seed);
        let (vertex_b, cave_b) = find_open_cave_vertex(&terrain_b, world_b.seed);
        assert_eq!(
            vertex_a, vertex_b,
            "two independent builds of the same seed must find the same open cave vertex"
        );

        let assert_same_snapshot = |a: &Session, b: &Session, label: &str| {
            let json_a = crate::snapshot_json(&a.snapshot().unwrap());
            let json_b = crate::snapshot_json(&b.snapshot().unwrap());
            assert_eq!(
                json_a, json_b,
                "snapshots diverged at {label} — the same seed and pins must \
                 produce a byte-identical descent and pane at every turn, not \
                 merely at the end"
            );
        };

        match session_a.delve_at(vertex_a, cave_a) {
            Turn::Out(_) => {}
            Turn::Released(_) => panic!("delve must not release"),
        };
        match session_b.delve_at(vertex_b, cave_b) {
            Turn::Out(_) => {}
            Turn::Released(_) => panic!("delve must not release"),
        };
        assert!(
            session_a.underground.is_some() && session_b.underground.is_some(),
            "an open, unbarred vertex found by find_open_cave_vertex must delve successfully"
        );
        assert_same_snapshot(&session_a, &session_b, "delve");

        const SCRIPT: &[&str] = &[
            "look", "go n", "down", "go n", "go e", "go s", "go w", "up", "climb",
        ];
        for line in SCRIPT {
            session_a.handle(line);
            session_b.handle(line);
            assert_same_snapshot(&session_a, &session_b, line);
        }
    }

    /// The Latch's own headline, end to end (Task 5, acceptance criteria 1
    /// and 2): a barred mouth refuses, `clear` clears it, and it stays clear
    /// for the REST OF THE SESSION — across several turns, including a
    /// `wait` tick and the NPC activity it drives.
    ///
    /// Exercised through [`Session::delve_at`] and
    /// [`Session::clear_passage_at`] directly — the identical test seam
    /// `delve_has_three_distinguishable_outcomes` uses, for the identical
    /// reason: [`Session::chamber_column_here`]'s own doc explains why
    /// steering the possession to one hand-picked vertex by walking is
    /// impractical for a test to depend on.
    ///
    /// **Session-scoped BY CHOICE OF SCOPE, not because persistence is
    /// impossible** — this paragraph claimed the opposite until the final
    /// whole-branch review, reading "session lifetime is the only lifetime
    /// the engine has (spec section 3.1): the session ledger is never
    /// written back". It is written back:
    /// [`Session::into_played_world`] moves the evolved ledger AND registry
    /// into a new `World`, and `possess --out` saves it (decision 0368,
    /// which retired the spec section this used to cite; decision 0171 rules
    /// that a player's acts are not filtered on the way out).
    ///
    /// What this test asserts is therefore the narrower of two true claims:
    /// the clearing survives across turns WITHIN one session. The save round
    /// trip — clear, `--out`, re-possess, delve — is a different test that
    /// nobody has written, so `passage.rs`'s module doc states it as an
    /// inference from `agent-at`'s identical mechanism rather than as a
    /// proven fact, and this test does not reach it either.
    ///
    /// MUTATION this must fail against: delete `clear_passage_at`'s
    /// `crate::passage::set_openness(...)` call (the whole `if barrier ==
    /// Thin` body). The post-clear delve then reports the refusal again and
    /// the final assertion fires.
    ///
    /// Confirmed 2026-08-29 against the openness write (Task 8); confirmed
    /// 2026-08-28 against the `passage-cleared` commit it replaced. In both
    /// cases the panic read
    /// `a cleared passage must let descent through after several turns and a
    /// wait: The cave mouth is here, but a thin fall of rubble blocks the
    /// way down; it looks like it would not take much to clear.` — the
    /// post-clear delve fell straight back to `barred_refusal`'s own `Thin`
    /// text because `effective_state` never found an `openness` fact about
    /// the cave mouth to fold over. A genuine behavioural red, not a compile
    /// error.
    #[test]
    fn a_cleared_passage_stays_open_for_the_rest_of_the_session() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");

        // 1/2. A vertex whose seeded barrier is specifically `Thin` — the
        // only state this campaign's clearing act yields to
        // (`clear_response`'s own doc explains why `Sealed`/`Warded` do
        // not).
        let (vertex, cave) = find_cave_vertex_with_barrier(
            &terrain,
            world.seed,
            hornvale_worldgen::BarrierState::Thin,
        );

        // 3. A pre-clear delve refuses, naming the barrier — acceptance
        // criterion 1.
        let refused = match session.delve_at(vertex, cave) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("delve must not release"),
        };
        assert!(
            refused.contains("thin fall of rubble"),
            "a Thin barrier's refusal must name it: {refused}"
        );
        assert!(
            session.underground.is_none(),
            "a barred passage must not set the underground state"
        );

        // 4. Clear it.
        let cleared = match session.clear_passage_at(vertex) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("clear must not release"),
        };
        assert!(
            cleared.contains("gives way"),
            "clearing a Thin barrier must say the rubble gave way: {cleared}"
        );

        // 5. Take several turns, including one `wait` — the NPC activity a
        // wait tick drives, plus ordinary looking around, must not touch the
        // fact just committed.
        session.handle("look");
        session.handle("wait");
        session.handle("look");
        session.handle("wait");

        // 6. The SAME vertex now lets a delve through — acceptance
        // criterion 2, and the whole point of the test's own name.
        let after = match session.delve_at(vertex, cave) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("delve must not release"),
        };
        assert!(
            session.underground.is_some(),
            "a cleared passage must let descent through after several turns and a \
             wait: {after}"
        );
        assert!(after.contains("You worm down into the dark"), "{after}");
    }

    /// **Which barriers yield to `clear` is a design choice, and this test
    /// is what pins it (Task 5's own brief calls this out by name).** Only
    /// `Thin` yields — `clear_response`'s doc comment gives the physical
    /// reasoning — so a `Sealed` mouth must refuse both the clearing act
    /// itself AND every delve after it, exactly as it did before `clear`
    /// was ever called.
    ///
    /// MUTATION this must fail against: widen `clear_passage_at`'s gate from
    /// `barrier == BarrierState::Thin` to `barrier != BarrierState::Open`
    /// (i.e. "everything barred yields"). `clear_passage_at` then opens the
    /// cave mouth at the `Sealed` vertex below, and the
    /// post-clear delve at the end of this test succeeds where it must
    /// still refuse.
    ///
    /// Confirmed 2026-08-29 (Task 8) and 2026-08-28 (The Latch): under that
    /// mutation, the panic read `a Sealed
    /// passage must still refuse descent after a clear attempt: You worm
    /// down into the dark. The rock here is the basement rock.` — the
    /// widened gate committed a clearing fact for the Sealed vertex, and the
    /// post-clear delve descended where it must still refuse. A genuine
    /// behavioural red, not a compile error.
    #[test]
    fn clearing_a_sealed_passage_does_not_open_it() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");

        let (vertex, cave) = find_cave_vertex_with_barrier(
            &terrain,
            world.seed,
            hornvale_worldgen::BarrierState::Sealed,
        );

        let attempt = match session.clear_passage_at(vertex) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("clear must not release"),
        };
        assert!(
            attempt.contains("unbroken"),
            "clearing a Sealed barrier must say there is nothing to clear: {attempt}"
        );

        let after = match session.delve_at(vertex, cave) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("delve must not release"),
        };
        assert!(
            session.underground.is_none(),
            "a Sealed passage must still refuse descent after a clear attempt: {after}"
        );
        assert!(
            after.contains("barred: choked"),
            "the post-attempt refusal must still name the Sealed barrier: {after}"
        );
    }

    /// The `Warded` half of the same split, and it pins **prose**, not the
    /// boolean gate. `clearing_a_sealed_passage_does_not_open_it` (above)
    /// already holds the LOGIC — one `barrier == Thin` test guards both
    /// non-yielding arms — so this test exists for the thing that test cannot
    /// see: that `Warded` gets its own words rather than falling back on
    /// `Sealed`'s.
    ///
    /// **That is not a stylistic preference, it is the campaign's own
    /// consistency argument.** `clear_response`'s doc justifies refusing
    /// `Warded` by pointing at `barred_refusal`'s `Warded` prose, which tells
    /// the player outright "you cannot force it" — a clearing verb that then
    /// forced it would contradict shipped text. A `Warded` clear attempt
    /// answering in `Sealed`'s vocabulary ("there is no rubble here", "the
    /// stone beyond is unbroken") would be a *different* contradiction: it
    /// would tell the player the ward is a rockfall.
    ///
    /// MUTATION this must fail against: in `clear_response`, merge the
    /// `Warded` arm into the `Sealed` arm (`BarrierState::Sealed |
    /// BarrierState::Warded => "You throw your weight against it, but there
    /// is no rubble here to clear — ..."`). This type-checks and leaves every
    /// other test in this module green, because no other assertion reads the
    /// `Warded` wording.
    ///
    /// Confirmed 2026-08-28: under that mutation the panic read `a Warded
    /// clear attempt must name what actually holds the dark shut: You throw
    /// your weight against it, but there is no rubble here to clear — the
    /// stone beyond is unbroken, and clearing has nothing to move.` A genuine
    /// behavioural red, not a compile error.
    ///
    /// **It is the FIRST assertion that fires, not the second, and this note
    /// is here because the prediction written above the run said otherwise.**
    /// Both assertions are genuinely load-bearing and they fail in order: the
    /// positive one ("names the ward") is strictly stronger, since prose could
    /// avoid `no rubble here` while still failing to name the ward. The
    /// negative one is kept as the guard against a *reworded* Sealed arm being
    /// merged in — a mutation the positive assertion alone would still catch,
    /// but which the pair localises faster.
    #[test]
    fn clearing_a_warded_passage_names_the_ward_not_the_rubble() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");

        let (vertex, cave) = find_cave_vertex_with_barrier(
            &terrain,
            world.seed,
            hornvale_worldgen::BarrierState::Warded,
        );

        let attempt = match session.clear_passage_at(vertex) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("clear must not release"),
        };
        assert!(
            attempt.contains("not stone or fall"),
            "a Warded clear attempt must name what actually holds the dark shut: {attempt}"
        );
        assert!(
            !attempt.contains("no rubble here"),
            "a Warded clear attempt must not answer in Sealed's vocabulary: {attempt}"
        );

        // And, like `Sealed`, it stays shut: the attempt commits nothing, so
        // the passage is exactly as barred afterwards as before.
        let after = match session.delve_at(vertex, cave) {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("delve must not release"),
        };
        assert!(
            session.underground.is_none(),
            "a Warded passage must still refuse descent after a clear attempt: {after}"
        );
        assert!(
            after.contains("holds the dark shut"),
            "the post-attempt refusal must still name the Warded barrier: {after}"
        );
    }

    /// **Tripwire for `clients/game/bin/src/driver.rs`'s
    /// `DELVE_SUCCESS_PREFIX` constant.** That client (outside this
    /// workspace, outside this crate's reach to import from) detects a
    /// successful delve by matching this EXACT literal prefix — including
    /// the trailing period — against the turn's own narration text via
    /// `str::starts_with`. As of this test's writing that was the ONLY
    /// signal available to it: `Spatial` folded underground into `Walk`, so
    /// there was no typed alternative to read instead. **The Gallery, Task 7
    /// changed the sim-side half of that**: the pane now carries its own
    /// `band: "underground"` with `vessel/level/v1`
    /// (`the_underground_pane_reads_band_underground`, this module) rather
    /// than folding. `clients/game` itself has not been re-pointed at it
    /// yet — that is Task 9's job — so `DELVE_SUCCESS_PREFIX`'s narration
    /// scan remains the client's actual, live cave-discovery signal until
    /// then, and this tripwire's own job is unchanged.
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
        let (open_vertex, open_cave) = find_open_cave_vertex(&terrain, world.seed);
        let out = match session.delve_at(open_vertex, open_cave) {
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

    /// `clear`'s two early refusals — the same two facts about where a body
    /// is standing that [`Session::delve`] guards on, which is why
    /// [`Session::clear_passage`] reuses their shape rather than inventing
    /// new ones.
    ///
    /// **Both arms shipped untested** (the final whole-branch review's M3):
    /// `delve`'s indoors twin is pinned by
    /// `delve_refuses_while_inside_a_structure` just above, and `clear`'s two
    /// had nothing. They are `Turn::Out` early returns, so a regression in
    /// either would be silent — the verb would fall through to
    /// `clear_passage_column`, find no cave mouth in a chamber or below
    /// ground, and answer "There is no cave mouth here to clear.": a
    /// plausible refusal for the wrong reason, which no other assertion
    /// would object to. Asserting on the DISTINGUISHING half of each string
    /// is what makes that substitution visible.
    #[test]
    fn clear_refuses_from_inside_a_structure_and_from_underground() {
        let world = seam_world();

        // Indoors: `descend` into a chamber, the same seam
        // `delve_refuses_while_inside_a_structure` uses.
        let (mut inside, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        inside
            .descend(path_structure(&inside.position(), 2), 0)
            .expect("a chamber to stand in");
        let out = match inside.handle("clear") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("clear must not release"),
        };
        assert_eq!(out, "There is nothing to clear from in here.");

        // Below ground: reached through `delve_at` against a hand-picked open
        // cave rather than a walk, for the reason
        // `lateral_movement_is_refused_underground` gives.
        let (mut below, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = below.wctx.terrain.clone().expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        below.delve_at(vertex, cave);
        assert!(
            below.underground.is_some(),
            "the fixture must have descended, or the arm below is unreached"
        );
        let out = match below.handle("clear") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("clear must not release"),
        };
        assert_eq!(
            out,
            "You are already below; there is nothing left to clear from down here."
        );
    }

    /// `back` alone is still refused while underground (The Gallery, Task
    /// 4 narrows what was `lateral_movement_is_refused_underground`):
    /// `go`/a bare compass token are reversed now that a cave level has real
    /// geometry to step through, but `back` is not, for the same reason it
    /// stays refused indoors after The Blocking's own reversal of `go` —
    /// this campaign built intra-level GEOMETRY, not a retraceable trail.
    /// Exercised directly against a hand-picked open cave (`delve_at`)
    /// rather than a walk, for the same reason
    /// `delve_has_three_distinguishable_outcomes` is.
    #[test]
    fn back_is_still_refused_underground() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        assert!(
            session.underground.is_some(),
            "the fixture must have descended"
        );
        let out = match session.handle("back") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("back must not release"),
        };
        assert!(!out.contains("No verb"), "{out}");
        assert!(
            out.contains("no trail to retrace"),
            "back must refuse underground with its own reason: {out}"
        );
    }

    /// The Gallery, Task 4's headline: a compass step underground moves the
    /// possession one cell, the same shape `handle`'s given test template
    /// specifies. Tries every orthogonal bearing from the entrance cell
    /// rather than a fixed one — Task 3's own connectivity invariant
    /// guarantees at least one is walkable from any standable cell, but not
    /// which one, so trying all four is what makes this robust against a
    /// terrain epoch that moves the entrance cell's local shape.
    #[test]
    fn a_compass_step_underground_moves_one_cell() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);

        let before = session.underground.as_ref().expect("descended").cell;
        // Try each bearing until one is not rock; at least one must be,
        // because the possession was placed on a cell of a connected level.
        let moved = ["n", "s", "e", "w"].iter().any(|d| {
            session.handle(&format!("go {d}"));
            session.underground.as_ref().expect("still below").cell != before
        });
        assert!(
            moved,
            "at least one bearing from a standable cell must be walkable"
        );
    }

    /// Rock refuses a lateral step with a PHYSICAL reason: not a parse
    /// complaint, and not a sentence naming a verb or a movement mode.
    /// Exercised at the `Underground::step` seam directly rather than
    /// through `Session::handle` — the entrance cell's own neighbours are
    /// generated content, not guaranteed to include a rock face, so this
    /// scans the level for a standable cell known to sit beside `Wall`
    /// before stepping toward it, the same scanning discipline
    /// `find_open_cave_vertex`'s own doc argues for over a hand-picked
    /// coordinate.
    #[test]
    fn rock_refuses_a_step_with_a_physical_reason() {
        let world = seam_world();
        let (session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        let mut ug = crate::underground::Underground::enter(&terrain, vertex, cave, world.seed);
        let level = ug.level().clone();
        let mut rock_adjacent = None;
        'search: for (cell, kind) in level.cells.iter() {
            if !matches!(
                kind,
                crate::underworld_level::LevelCellKind::Floor
                    | crate::underworld_level::LevelCellKind::Flooded
            ) {
                continue;
            }
            for wanted in [Compass::N, Compass::E, Compass::S, Compass::W] {
                let delta = cell_delta(wanted).expect("orthogonal");
                let neighbour = crate::lattice::Cell(cell.0 + delta.0, cell.1 + delta.1);
                if level.cells.get(neighbour) == Some(crate::underworld_level::LevelCellKind::Wall)
                {
                    rock_adjacent = Some((cell, wanted));
                    break 'search;
                }
            }
        }
        let (cell, wanted) =
            rock_adjacent.expect("a generated level has at least one standable cell beside rock");
        ug.cell = cell;
        match ug.step(wanted) {
            crate::underground::StepOutcome::Blocked(reason) => {
                let lower = reason.to_lowercase();
                assert!(!lower.contains("verb"), "not a parse complaint: {reason}");
                assert!(!lower.contains("mode"), "must not name a mode: {reason}");
                assert!(!lower.contains("wade"), "must not name a mode: {reason}");
                assert!(!lower.contains("walk"), "must not name a mode: {reason}");
            }
            other => panic!("expected a Blocked outcome, got {other:?}"),
        }
    }

    /// `Underground::peek_stairs`'s own catch-all arm — refusing when the
    /// current cell is not a stairs cell at all — is unreachable through any
    /// shipped path: `Session::take_stairs` always checks the current
    /// cell's kind against the direction it wants BEFORE ever calling
    /// `peek_stairs`, so a live session can never hand it a non-stairs
    /// cell. `peek_stairs`'s own doc calls it "a real seam a test... can
    /// reach directly" — this is that test, exercised the same way
    /// `rock_refuses_a_step_with_a_physical_reason` (just above) reaches
    /// `Underground::step`'s own seam: build a real `Underground` via
    /// `enter`, place it on a scanned, known-non-stairs cell, and call the
    /// method directly rather than through `Session::handle`.
    #[test]
    fn peek_stairs_refuses_off_any_stairs_cell() {
        let world = seam_world();
        let (session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        let mut ug = crate::underground::Underground::enter(&terrain, vertex, cave, world.seed);
        let level = ug.level().clone();
        let floor_cell = level
            .cells
            .iter()
            .find(|(_, k)| {
                matches!(
                    k,
                    crate::underworld_level::LevelCellKind::Floor
                        | crate::underworld_level::LevelCellKind::Flooded
                )
            })
            .map(|(c, _)| c)
            .expect("a generated level has at least one standable, non-stairs cell");
        ug.cell = floor_cell;
        match ug.peek_stairs() {
            Err(reason) => {
                assert!(
                    reason.to_lowercase().contains("stairway"),
                    "the refusal must name the physical reason: {reason}"
                );
            }
            Ok(_) => panic!("a plain Floor/Flooded cell must not offer stairs"),
        }
    }

    /// `UNDERGROUND_LATERAL_REFUSAL`'s own doc said "there is nowhere down
    /// here for a bearing to mean" — false the moment Task 3 gave the cave
    /// real cells to walk. The Gallery, Task 4 deletes the constant
    /// outright; this pins that its text is truly gone from a walking
    /// session's output, not merely unreachable from one call site.
    #[test]
    fn the_lateral_refusal_is_gone() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        assert!(
            session.underground.is_some(),
            "the fixture must have descended"
        );
        for line in ["go n", "go s", "go e", "go w", "n", "s", "e", "w", "back"] {
            let out = match session.handle(line) {
                Turn::Out(t) => t,
                Turn::Released(_) => panic!("{line} must not release"),
            };
            assert!(
                !out.contains("Not down here") && !out.contains("Climb out first"),
                "{line}: the retired underground lateral refusal must never print: {out}"
            );
        }
    }

    /// Fix round 1, finding 1: `look`'s underground "Ways on" must agree
    /// with the level's own real passable neighbours, in BOTH directions —
    /// a way that exists is listed, and a way that does not is absent — the
    /// agreement shape a one-directional check cannot catch (The Latch's own
    /// retrospective). Before this fix the sentence was a fixed `"Ways on:
    /// out."` regardless of the level, so a bare "some bearing is listed"
    /// check would have passed against the old, wrong text too; this test
    /// picks a cell with a KNOWN mixed neighbourhood (at least one open
    /// bearing and at least one blocked one) so both directions are
    /// actually exercised rather than a degenerate all-open or all-blocked
    /// cell.
    #[test]
    fn underground_ways_on_agrees_with_the_levels_real_neighbours() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        let ug = crate::underground::Underground::enter(&terrain, vertex, cave, world.seed);
        let level = ug.level().clone();

        let mut mixed = None;
        let mut open_wanted = None;
        let mut blocked_wanted = None;
        'search: for (cell, kind) in level.cells.iter() {
            if crate::underworld_level::movement_mode(kind).is_none() {
                continue;
            }
            let mut open = None;
            let mut blocked = None;
            for wanted in [Compass::N, Compass::E, Compass::S, Compass::W] {
                let delta = cell_delta(wanted).expect("orthogonal");
                let neighbour = crate::lattice::Cell(cell.0 + delta.0, cell.1 + delta.1);
                match level
                    .cells
                    .get(neighbour)
                    .and_then(crate::underworld_level::movement_mode)
                {
                    Some(_) => open = open.or(Some(wanted)),
                    None => blocked = blocked.or(Some(wanted)),
                }
            }
            if let (Some(o), Some(b)) = (open, blocked) {
                mixed = Some(cell);
                open_wanted = Some(o);
                blocked_wanted = Some(b);
                break 'search;
            }
        }
        let cell = mixed.expect(
            "a generated level has a cell with both an open and a blocked orthogonal bearing",
        );
        let open_wanted = open_wanted.expect("set alongside mixed");
        let blocked_wanted = blocked_wanted.expect("set alongside mixed");

        let mut expected: Vec<String> = vec!["out".to_string()]; // rung 0
        for wanted in [Compass::N, Compass::E, Compass::S, Compass::W] {
            let delta = cell_delta(wanted).expect("orthogonal");
            let neighbour = crate::lattice::Cell(cell.0 + delta.0, cell.1 + delta.1);
            if level
                .cells
                .get(neighbour)
                .and_then(crate::underworld_level::movement_mode)
                .is_some()
            {
                expected.push(bearing_letter(wanted));
            }
        }

        session.underground = Some(ug);
        session.underground.as_mut().expect("just set").cell = cell;

        let out = match session.handle("look") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("look must not release"),
        };
        let marker = "Ways on: ";
        let start = out
            .find(marker)
            .unwrap_or_else(|| panic!("no {marker:?} substring in {out:?}"));
        let ways_body = &out[start + marker.len()..];
        let ways_body = ways_body.trim_end_matches('\n');
        let ways_body = ways_body.strip_suffix('.').unwrap_or(ways_body);
        let listed: Vec<String> = ways_body.split(", ").map(|part| part.to_string()).collect();
        let ways_line = format!("{marker}{ways_body}.");

        // Direction 1: a way that EXISTS is listed.
        assert!(
            listed.contains(&bearing_letter(open_wanted)),
            "an open bearing {open_wanted:?} must be listed: {ways_line:?}"
        );
        // Direction 2: a way that does NOT exist is absent.
        assert!(
            !listed.contains(&bearing_letter(blocked_wanted)),
            "a blocked bearing {blocked_wanted:?} must not be listed: {ways_line:?}"
        );
        // And the full set, both ways: nothing extra, nothing missing.
        let mut listed_sorted = listed.clone();
        listed_sorted.sort();
        let mut expected_sorted = expected.clone();
        expected_sorted.sort();
        assert_eq!(
            listed_sorted, expected_sorted,
            "ways-on must match the level's real neighbours exactly: {ways_line:?}"
        );
    }

    /// The property that matters (The Gallery, Task 5's own brief):
    /// descending from rung `n` and climbing back arrives on rung `n` —
    /// not necessarily the same CELL, since `place_connections` sites the
    /// down-stairs and the up-stairs independently (first leaf vs last
    /// leaf), so this asserts on the RUNG alone, never the cell.
    ///
    /// Reaching a stairs cell by walking is impractical from a test — the
    /// same reason `delve_at` exists as a seam
    /// (`underground_ways_on_agrees_with_the_levels_real_neighbours` above
    /// already uses it the same way): this scans the entrance rung's own
    /// level for its one `StairsDown` cell and places the possession there
    /// directly.
    #[test]
    fn stairs_connect_adjacent_rungs_in_both_directions() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        assert!(
            session.underground.is_some(),
            "the fixture must have descended"
        );
        assert_eq!(
            session.underground.as_ref().expect("descended").rung,
            0,
            "sanity check: `enter` always starts a descent at rung 0"
        );

        let down_cell = {
            let ug = session.underground.as_ref().expect("descended");
            ug.level()
                .cells
                .iter()
                .find(|(_, k)| *k == crate::underworld_level::LevelCellKind::StairsDown)
                .map(|(c, _)| c)
                .expect("every rung has exactly one StairsDown cell")
        };
        session.underground.as_mut().expect("descended").cell = down_cell;

        let down_out = match session.handle("down") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("down must not release"),
        };
        assert_eq!(
            session.underground.as_ref().expect("still below").rung,
            1,
            "descending from rung 0 must land on rung 1: {down_out}"
        );

        // Round trip: `up` from here must arrive back on rung 0 — the
        // possession already stands on rung 1's own `StairsUp` cell
        // (`Underground::peek_stairs`'s own landing rule), so no further
        // scan or placement is needed before taking the stairs back up.
        let up_out = match session.handle("up") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("up must not release"),
        };
        assert_eq!(
            session.underground.as_ref().expect("still below").rung,
            0,
            "ascending back must land on rung 0: {up_out}"
        );
    }

    /// The direction is checked against the CURRENT cell, not merely
    /// "is this any stairs cell" — typing `up` while standing on a
    /// `StairsDown` cell must refuse rather than silently taking the
    /// possession down (`Self::take_stairs`'s own doc explains why: asking
    /// `peek_stairs` with no direction check first would do exactly that,
    /// since it reads whichever kind the current cell already is).
    #[test]
    fn down_refuses_on_an_up_stairs_cell_and_vice_versa() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        let down_cell = {
            let ug = session.underground.as_ref().expect("descended");
            ug.level()
                .cells
                .iter()
                .find(|(_, k)| *k == crate::underworld_level::LevelCellKind::StairsDown)
                .map(|(c, _)| c)
                .expect("every rung has exactly one StairsDown cell")
        };
        session.underground.as_mut().expect("descended").cell = down_cell;
        let before = session.underground.as_ref().expect("descended").rung;

        let out = match session.handle("up") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("up must not release"),
        };
        assert_eq!(
            session.underground.as_ref().expect("still below").rung,
            before,
            "`up` on a StairsDown cell must not move the possession: {out}"
        );
        assert!(
            out.to_lowercase().contains("no stairway up"),
            "the refusal must name the physical mismatch: {out}"
        );
    }

    /// Fix round 1's first covering test: `delve_at` itself is one of the
    /// three arrival paths that marks fog (spec §3.5, amended by commit
    /// f6051a9c3 — "every arrival marks", not merely a lateral step). This
    /// is the test that failed before the fix: taking no step at all, the
    /// entrance cell's own surroundings must already be remembered the
    /// instant the descent is built.
    #[test]
    fn delving_marks_the_arrival_cell_without_a_step() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        let ug = session
            .underground
            .as_ref()
            .expect("the fixture must have descended");
        assert!(
            ug.seen[ug.rung].saw(ug.cell),
            "the entrance cell itself must be remembered immediately after \
             delve_at, before any step is ever taken"
        );
    }

    /// Fix round 1's second covering test: `take_stairs`' own landing is
    /// the second of the three arrival paths (spec §3.5, amended by commit
    /// f6051a9c3). Descend to a fresh rung by the stairs alone — no lateral
    /// step on the new rung at all — and the landing cell must already be
    /// remembered there.
    #[test]
    fn taking_stairs_marks_the_landing_without_a_step() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        assert!(
            session.underground.is_some(),
            "the fixture must have descended"
        );

        // Reaching the StairsDown cell by walking is impractical from a
        // test — the same reason
        // `stairs_connect_adjacent_rungs_in_both_directions` places it
        // directly.
        let down_cell = {
            let ug = session.underground.as_ref().expect("descended");
            ug.level()
                .cells
                .iter()
                .find(|(_, k)| *k == crate::underworld_level::LevelCellKind::StairsDown)
                .map(|(c, _)| c)
                .expect("every rung has exactly one StairsDown cell")
        };
        session.underground.as_mut().expect("descended").cell = down_cell;

        match session.handle("down") {
            Turn::Out(_) => {}
            Turn::Released(_) => panic!("down must not release"),
        }
        let ug = session.underground.as_ref().expect("still below");
        assert_eq!(ug.rung, 1, "descending from rung 0 must land on rung 1");
        assert!(
            ug.seen[ug.rung].saw(ug.cell),
            "the landing cell on rung 1 must be remembered immediately after \
             taking the stairs down, before any lateral step there"
        );
    }

    /// Every cell of `ug.rung`'s own remembered set — a `BTreeSet` snapshot
    /// taken by scanning the level, used only by this task's own fog tests
    /// to compare "before" against "after" with an ordinary set-comparison
    /// method rather than poking at `SeenBits`'s packed bits directly.
    fn seen_snapshot(
        ug: &crate::underground::Underground,
    ) -> std::collections::BTreeSet<crate::lattice::Cell> {
        let level = ug.level();
        level
            .cells
            .iter()
            .filter(|(cell, _)| ug.seen[ug.rung].saw(*cell))
            .map(|(cell, _)| cell)
            .collect()
    }

    /// The Gallery, Task 6, Step 1's `walking_only_ever_adds_to_what_is_
    /// remembered`: monotonicity (spec §4.1.2's acceptance 4b). After every
    /// real "go <bearing>" turn — whether it actually moves the possession
    /// or refuses — the rung's own remembered set must be a superset of
    /// what it was one turn before, never smaller.
    ///
    /// **A superset check alone cannot fail, and this test used to carry
    /// only that one.** A set that never shrinks satisfies
    /// `after.is_superset(&before)` trivially, and the non-vacuity guard
    /// that shipped beside it (`assert!(!before.is_empty())`) was already
    /// satisfied at turn zero — `delve_at` marks the entrance before the
    /// loop even starts — so deleting `Session::mark_underground_seen`
    /// from `step_underground` entirely left this test green. The fix is
    /// `ever_grew` below: it asserts the set actually GROWS at least once
    /// across the walk, which is the property this doc's own opening
    /// sentence claims ("only ever adds") and the one a dropped-marking
    /// regression would actually violate.
    ///
    /// Exercised through `Session::handle`, not `Underground::step`
    /// directly: the wiring under test is `Session::mark_underground_seen`,
    /// which sits one level above `Underground` and is what a bare
    /// `Underground::step` call would bypass entirely.
    #[test]
    fn walking_only_ever_adds_to_what_is_remembered() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        assert!(
            session.underground.is_some(),
            "the fixture must have descended"
        );

        let mut before = seen_snapshot(session.underground.as_ref().expect("descended"));
        let mut ever_grew = false;
        for _ in 0..8 {
            for d in ["n", "s", "e", "w"] {
                match session.handle(&format!("go {d}")) {
                    Turn::Out(_) => {}
                    Turn::Released(_) => panic!("go {d} must not release"),
                }
                let after = seen_snapshot(session.underground.as_ref().expect("still below"));
                assert!(
                    after.is_superset(&before),
                    "remembered cells must never shrink after `go {d}`: \
                     before had {} cells, after has {}, and after is missing \
                     {:?}",
                    before.len(),
                    after.len(),
                    before.difference(&after).collect::<Vec<_>>()
                );
                if after.len() > before.len() {
                    ever_grew = true;
                }
                before = after;
            }
        }
        assert!(
            ever_grew,
            "32 attempted steps from a connected level must have GROWN what \
             is remembered at least once — a set that starts non-empty and \
             never shrinks would satisfy every assertion above even if \
             nothing were ever marked seen again"
        );
    }

    /// A shortest path of bearings from `from` to some passable cell whose
    /// Chebyshev distance from `from` is strictly greater than
    /// `min_chebyshev` — used by this task's own fog tests to reach a cell
    /// genuinely OUTSIDE a given reach, rather than one that merely happens
    /// to be a single lateral step away (which Fix round 1 made ambiguous:
    /// after commit f6051a9c3, `delve_at` itself marks everything within
    /// `sight_reach()` of the entrance, so a nearby cell cannot distinguish
    /// "still remembered from before" from "freshly visible from here").
    /// Every non-Surface rung is at least 40x24 (`generate_level_extent`),
    /// and Task 9's own connectivity invariant
    /// (`every_walkable_cell_is_reachable_from_every_other`) guarantees a
    /// path exists to any passable cell, so a cell beyond `min_chebyshev`
    /// is always reachable from an interior point this many cells wide.
    fn path_beyond_reach(
        level: &crate::underworld_level::Level,
        from: crate::lattice::Cell,
        min_chebyshev: i32,
    ) -> Vec<Compass> {
        use std::collections::{BTreeMap, BTreeSet, VecDeque};
        let mut visited: BTreeSet<crate::lattice::Cell> = BTreeSet::new();
        let mut parent: BTreeMap<crate::lattice::Cell, (crate::lattice::Cell, Compass)> =
            BTreeMap::new();
        let mut queue = VecDeque::new();
        queue.push_back(from);
        visited.insert(from);
        let mut target = None;
        while let Some(cur) = queue.pop_front() {
            let dist = (cur.0 - from.0).abs().max((cur.1 - from.1).abs());
            if dist > min_chebyshev {
                target = Some(cur);
                break;
            }
            for d in [Compass::N, Compass::E, Compass::S, Compass::W] {
                let delta = cell_delta(d).expect("orthogonal");
                let next = crate::lattice::Cell(cur.0 + delta.0, cur.1 + delta.1);
                if visited.contains(&next) {
                    continue;
                }
                if level
                    .cells
                    .get(next)
                    .and_then(crate::underworld_level::movement_mode)
                    .is_none()
                {
                    continue;
                }
                visited.insert(next);
                parent.insert(next, (cur, d));
                queue.push_back(next);
            }
        }
        let target = target.expect(
            "a level at least 40x24 must have a passable cell beyond min_chebyshev \
             reachable from an interior point (Task 9's connectivity invariant)",
        );
        let mut path = Vec::new();
        let mut cur = target;
        while cur != from {
            let (prev, d) = parent[&cur];
            path.push(d);
            cur = prev;
        }
        path.reverse();
        path
    }

    /// The Gallery, Task 6, Step 4's `fog_survives_moving_between_rungs_
    /// and_dies_on_climbing_out`: spec acceptance criterion 5, in both
    /// directions. Descend, walk to accumulate seen cells on rung 0, take
    /// the stairs down and back up — rung 0's own remembered set must still
    /// hold what was walked. Then climb out and delve again: the fresh
    /// descent's rung 0 must remember nothing, because fog is session-lived
    /// (spec §3.5), not derived from `(seed, address)`.
    ///
    /// **Fix round 1 (commit f6051a9c3) walks to a cell OUTSIDE
    /// `sight_reach()` of the entrance**, not merely one lateral step away.
    /// Before this fix `delve_at` marked nothing, so any walked-to cell was
    /// a valid witness; now `delve_at` itself marks the entrance's own
    /// surroundings, so a fresh descent legitimately remembers a NEARBY
    /// cell again — that is correct behaviour, not a lifetime leak. Only a
    /// cell beyond the fresh entrance's own reach can distinguish "the old
    /// descent's fog leaked through" from "the new descent can see this
    /// far on its own".
    #[test]
    fn fog_survives_moving_between_rungs_and_dies_on_climbing_out() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        assert!(
            session.underground.is_some(),
            "the fixture must have descended"
        );

        let entrance_cell = session.underground.as_ref().expect("descended").cell;
        let level = session
            .underground
            .as_ref()
            .expect("descended")
            .level()
            .clone();
        let path = path_beyond_reach(&level, entrance_cell, SIGHT_RADIUS);
        assert!(
            !path.is_empty(),
            "the entrance cell itself must already be beyond its own reach \
             from itself, which is impossible — path must be non-empty"
        );
        for bearing in &path {
            let letter = bearing_letter(*bearing).to_lowercase();
            match session.handle(&format!("go {letter}")) {
                Turn::Out(_) => {}
                Turn::Released(_) => panic!("go {letter} must not release"),
            }
        }
        let remote_cell = session.underground.as_ref().expect("still below").cell;
        let dist = (remote_cell.0 - entrance_cell.0)
            .abs()
            .max((remote_cell.1 - entrance_cell.1).abs());
        assert!(
            dist > SIGHT_RADIUS,
            "the walked-to cell must be strictly beyond sight_reach() of the \
             entrance: distance {dist}, SIGHT_RADIUS {SIGHT_RADIUS}"
        );
        assert!(
            session.underground.as_ref().expect("still below").seen[0].saw(remote_cell),
            "the remote cell just walked to must be marked seen on rung 0"
        );

        // Place the possession on the entrance rung's own StairsDown cell —
        // reaching it by walking is impractical from a test, the same
        // reason `stairs_connect_adjacent_rungs_in_both_directions` places
        // it directly.
        let down_cell = {
            let ug = session.underground.as_ref().expect("descended");
            ug.level()
                .cells
                .iter()
                .find(|(_, k)| *k == crate::underworld_level::LevelCellKind::StairsDown)
                .map(|(c, _)| c)
                .expect("every rung has exactly one StairsDown cell")
        };
        session.underground.as_mut().expect("descended").cell = down_cell;

        match session.handle("down") {
            Turn::Out(_) => {}
            Turn::Released(_) => panic!("down must not release"),
        }
        assert_eq!(
            session.underground.as_ref().expect("still below").rung,
            1,
            "descending from rung 0 must land on rung 1"
        );
        match session.handle("up") {
            Turn::Out(_) => {}
            Turn::Released(_) => panic!("up must not release"),
        }
        assert_eq!(
            session.underground.as_ref().expect("still below").rung,
            0,
            "ascending back must land on rung 0"
        );
        assert!(
            session.underground.as_ref().expect("still below").seen[0].saw(remote_cell),
            "rung 0's remembered set must survive a round trip through rung 1"
        );

        // Climb out (rung is 0, so `climb` succeeds) and delve the SAME cave
        // again: `Underground::enter` re-derives an identical descent from
        // the same seed/vertex/cave (nothing here consumes any per-session
        // state), so `remote_cell` names the same physical cell in the new
        // descent — but the new descent's own `seen` starts all-unseen
        // except for what `delve_at`'s OWN arrival mark reaches, which
        // `remote_cell` is, by construction, outside of.
        match session.handle("climb") {
            Turn::Out(_) => {}
            Turn::Released(_) => panic!("climb must not release"),
        }
        assert!(
            session.underground.is_none(),
            "climb must clear the descent entirely"
        );
        session.delve_at(vertex, cave);
        assert!(
            session.underground.is_some(),
            "the fixture must have descended again"
        );
        assert!(
            !session.underground.as_ref().expect("descended again").seen[0].saw(remote_cell),
            "fog is session-lived: a fresh descent must remember nothing \
             beyond its own entrance's reach, even at a cell the previous \
             descent had marked"
        );
    }

    /// The descent's own deepest rung still carries a `StairsDown` cell
    /// (`place_connections` never special-cases the last rung), but nothing
    /// generated lies beneath it — `Underground::peek_stairs`'s own
    /// boundary check. Reached here by forcing `rung` to the bottom rather
    /// than walking a full descent down: this task's own scope is the
    /// stairs verb, not a fifth end-to-end walk of the ladder.
    #[test]
    fn the_deepest_rungs_stairs_down_refuses_without_moving() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        let bottom = session
            .underground
            .as_ref()
            .expect("descended")
            .descent
            .len()
            - 1;
        let down_cell = {
            let ug = session.underground.as_mut().expect("descended");
            ug.rung = bottom;
            ug.level()
                .cells
                .iter()
                .find(|(_, k)| *k == crate::underworld_level::LevelCellKind::StairsDown)
                .map(|(c, _)| c)
                .expect("every rung, the last included, has exactly one StairsDown cell")
        };
        session.underground.as_mut().expect("descended").cell = down_cell;
        let before = session.day;

        let out = match session.handle("down") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("down must not release"),
        };
        assert_eq!(
            session.underground.as_ref().expect("still below").rung,
            bottom,
            "the deepest rung's own down-stairs must not move the possession: {out}"
        );
        assert_eq!(
            session.day, before,
            "a refused stairs move must not spend a tick: {out}"
        );
        assert!(
            !out.to_lowercase().contains("verb"),
            "not a parse complaint: {out}"
        );
    }

    /// A stairs move charges time, the same dial `an_underground_step_
    /// advances_the_clock` (just below) already pins for a lateral cell —
    /// one flight of stairs is priced the same scale of act as one cell,
    /// not the free ride `dive`/`surface`/`delve`/`climb` get for a whole
    /// band's worth of vertical movement.
    #[test]
    fn taking_the_stairs_advances_the_clock() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        let down_cell = {
            let ug = session.underground.as_ref().expect("descended");
            ug.level()
                .cells
                .iter()
                .find(|(_, k)| *k == crate::underworld_level::LevelCellKind::StairsDown)
                .map(|(c, _)| c)
                .expect("every rung has exactly one StairsDown cell")
        };
        session.underground.as_mut().expect("descended").cell = down_cell;
        let before = session.day;

        let out = match session.handle("down") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("down must not release"),
        };
        assert!(
            session.day > before,
            "taking the stairs must advance the clock: before={before:?}, \
             after={:?}, out={out}",
            session.day
        );
    }

    /// `down` is gated by the body like every other in-character verb (spec
    /// §2.1/§3.2) — the behavioural half of the three-roster proof
    /// (`every_bare_verb_help_lists_is_classified` catches a roster drift
    /// structurally; this drives an actual sleeping body at `down` and
    /// checks the refusal itself). Copied from `warm_is_refused_while_
    /// asleep`, the pattern that pin names: The Latch shipped a verb
    /// (`clear`) that was in NEITHER `IN_CHARACTER_VERBS` nor `HELP`, which
    /// satisfies every STRUCTURAL agreement check in both directions
    /// (absence agrees with absence) — only a live sleeping body at the verb
    /// itself catches that shape of drift.
    #[test]
    fn down_is_refused_while_asleep() {
        let world = seam_world();
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
        let slept = match session.handle("sleep") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("sleep must not release: {t}"),
        };
        assert!(
            !slept.starts_with("No verb"),
            "`sleep` must be a verb for this test to mean anything: {slept}"
        );
        assert_eq!(
            session.body_state(),
            BodyState::Asleep,
            "sanity check: asleep alone must gate as asleep"
        );
        let out = match session.handle("down") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("down must not release: {t}"),
        };
        assert_eq!(out, "You cannot — you are asleep.");
    }

    /// `up`'s own half of the same proof `down_is_refused_while_asleep`
    /// carries — both verbs were added together, so both get the
    /// behavioural check, not just the one this doc happened to name first.
    #[test]
    fn up_is_refused_while_asleep() {
        let world = seam_world();
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
        let slept = match session.handle("sleep") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("sleep must not release: {t}"),
        };
        assert!(
            !slept.starts_with("No verb"),
            "`sleep` must be a verb for this test to mean anything: {slept}"
        );
        let out = match session.handle("up") {
            Turn::Out(t) => t,
            Turn::Released(t) => panic!("up must not release: {t}"),
        };
        assert_eq!(out, "You cannot — you are asleep.");
    }

    /// Fix round 1, finding 2: an underground step must advance the clock,
    /// the same way an indoor cell step does (`charge_within_room`) —
    /// without this, walking underground costs no time at all, and the
    /// world's own clock (`WorldTime`) stands still while the possession
    /// crosses a cave.
    #[test]
    fn an_underground_step_advances_the_clock() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        assert!(
            session.underground.is_some(),
            "the fixture must have descended"
        );
        let before = session.day;
        let moved = ["n", "s", "e", "w"].iter().any(|d| {
            let cell_before = session.underground.as_ref().expect("descended").cell;
            session.handle(&format!("go {d}"));
            session.underground.as_ref().expect("still below").cell != cell_before
        });
        assert!(
            moved,
            "at least one bearing must be walkable to exercise the charge"
        );
        assert!(
            session.day > before,
            "an underground step must advance the clock: before={before:?}, after={:?}",
            session.day
        );
    }

    /// The Gallery, Task 7's own headline assertion (spec §4, §6 acceptance
    /// 3): the pane answers `band: "underground"` the moment the possession
    /// descends, carrying `vessel/level/v1` rather than folding into the
    /// walk-band chart. See `SpatialChannel`'s own doc and
    /// `the_pane_and_the_verb_agree_underground` (below) for the history
    /// this replaces.
    #[test]
    fn the_underground_pane_reads_band_underground() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        let snap = session.snapshot().expect("a descended session snapshots");
        let json = crate::snapshot_json(&snap);
        assert!(
            json.contains(r#""band":"underground""#),
            "the pane must read `underground` immediately after a descent: {json:.200}"
        );
    }

    /// Spec §4.1.1: the document carries only cells the possession has
    /// seen, so on the very first turn after descending — before a single
    /// lateral step — the level document's own cell count must be strictly
    /// less than the rung's full extent area. A dense, TOTAL grid (as
    /// `vessel/plan/v1`'s `cells` is) could never satisfy this: it always
    /// carries exactly `w * h` entries, seen or not.
    #[test]
    fn a_never_seen_cell_is_absent_from_the_document() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        let snap = session.snapshot().expect("a descended session snapshots");
        let level = match &snap.spatial {
            crate::snapshot::SpatialChannel::Underground { level } => level,
            other => panic!("expected the underground band, got {other:?}"),
        };
        let area = (level.extent.w * level.extent.h) as usize;
        assert!(
            level.cells.len() < area,
            "a freshly-entered level must not carry every cell of its extent: \
             {} of {area} cells present",
            level.cells.len()
        );
    }

    /// Spec §4.1, §4.1.2: `here`, `lit` and `remembered` must all be
    /// reachable in one document, distinguished by the `state` field alone —
    /// never by colour. Walking far enough from the entrance that it falls
    /// outside the current shadowcast is what manufactures the third state:
    /// the entrance is marked the instant `delve_at` lands
    /// (`Session::mark_underground_seen`'s "every arrival marks" rule), so
    /// once the possession is more than `sight_reach()` Chebyshev cells away
    /// from it, the entrance is guaranteed `remembered`, not `lit` —
    /// [`crate::lattice::shadowcast_with`] cannot light a cell farther than
    /// the radius it was given, regardless of the level's own geometry.
    #[test]
    fn the_three_visibility_states_are_distinguishable_without_colour() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        let start = session.underground.as_ref().expect("descended").cell;
        let reach = session.sight_reach();

        // Rather than walking there through `handle("go ...")` — which risks
        // an unlucky maze shape sending a fixed-priority walk back and forth
        // near the entrance instead of away from it — find a reachable cell
        // more than `reach` Chebyshev cells from the entrance by breadth-
        // first search over the level's own walkable graph, then place the
        // possession there directly and fold its shadowcast the same way a
        // real arrival would (`Session::mark_underground_seen`). Every
        // generated level is connected well beyond `reach` cells from its
        // own entrance (Task 9's connectivity invariant), so the search
        // always finds one.
        let far_cell = {
            let ug = session.underground.as_ref().expect("descended");
            let level = ug.level();
            let mut visited = std::collections::BTreeSet::new();
            let mut queue = std::collections::VecDeque::new();
            visited.insert(start);
            queue.push_back(start);
            let mut found = None;
            while let Some(cur) = queue.pop_front() {
                let dist = (cur.0 - start.0).abs().max((cur.1 - start.1).abs());
                if dist > reach {
                    found = Some(cur);
                    break;
                }
                for (dx, dy) in [(0, 1), (0, -1), (1, 0), (-1, 0)] {
                    let next = crate::lattice::Cell(cur.0 + dx, cur.1 + dy);
                    if visited.insert(next)
                        && level
                            .cells
                            .get(next)
                            .and_then(crate::underworld_level::movement_mode)
                            .is_some()
                    {
                        queue.push_back(next);
                    }
                }
            }
            found.expect(
                "a generated level is connected well beyond sight_reach() from \
                 its own entrance (Task 9's connectivity invariant)",
            )
        };
        session.underground.as_mut().expect("descended").cell = far_cell;
        session.mark_underground_seen();

        let snap = session.snapshot().expect("a descended session snapshots");
        let json = crate::snapshot_json(&snap);
        for needle in [
            r#""state":"here""#,
            r#""state":"lit""#,
            r#""state":"remembered""#,
        ] {
            assert!(json.contains(needle), "missing {needle} in {json}");
        }
        assert!(
            !json.contains("\"color\"") && !json.contains("\"colour\""),
            "the level document must distinguish visibility by state alone, \
             never by colour: {json}"
        );
    }

    /// The Gallery, Task 11 (spec §4.1.2, §3.6): a chamber's derived
    /// resident is drawn only while its own cell is `lit` — never merely
    /// `remembered`. The chamber band's own marks already follow this rule
    /// (`a_creature_beyond_sight_appears_neither_in_sensed_nor_in_marks`,
    /// below); this is the same assertion one band down, for a mark this
    /// module derives fresh on every snapshot rather than reading off a
    /// placed body.
    ///
    /// Stands the possession directly on the resident's own cell (folding
    /// the shadowcast there via [`Session::mark_underground_seen`], exactly
    /// as a real arrival would) so that cell is genuinely `seen`, then walks
    /// far enough away — by BFS over the level's own walkable graph, the
    /// same technique
    /// [`the_three_visibility_states_are_distinguishable_without_colour`]
    /// uses, rather than steering a walk there and risking an unlucky maze
    /// shape — that the resident's cell falls outside the current
    /// shadowcast: `remembered`, not `lit`. The mark must vanish there and
    /// reappear back at the resident's own cell.
    #[test]
    fn a_creature_underground_is_drawn_only_while_lit() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let climate = session
            .wctx
            .climate
            .clone()
            .expect("seed 42 builds climate");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);

        let (kind, _) = {
            let ug = session.underground.as_ref().expect("descended");
            crate::underground::chamber_resident(ug, &terrain, &climate).unwrap_or_else(|| {
                panic!(
                    "the fixture's entrance chamber must support a resident for this \
                     test to exercise anything — if a terrain/species change moved \
                     this, pick a different fixture vertex rather than deleting the \
                     assertion"
                )
            })
        };
        let resident_cell = {
            let ug = session.underground.as_ref().expect("descended");
            crate::underground::resident_cell(ug.level())
                .expect("Task 9's connectivity invariant guarantees a standable cell")
        };
        let reach = session.sight_reach();

        // Stand on the resident's own cell and fold the shadowcast there, the
        // same way a real arrival marks its surroundings seen.
        session.underground.as_mut().expect("descended").cell = resident_cell;
        session.mark_underground_seen();

        let has_resident_mark = |session: &Session| -> bool {
            let snap = session.snapshot().expect("a descended session snapshots");
            match snap.spatial {
                crate::snapshot::SpatialChannel::Underground { level } => level
                    .marks
                    .iter()
                    .any(|m| m.noun == kind.0 && m.x == resident_cell.0 && m.y == resident_cell.1),
                other => panic!("expected the underground band, got {other:?}"),
            }
        };

        assert!(
            has_resident_mark(&session),
            "standing on the resident's own (lit) cell must show its mark"
        );

        // BFS over the level's own walkable graph for a cell more than
        // `reach` Chebyshev cells from the resident's cell — guaranteed to
        // exist by Task 9's connectivity invariant.
        let far_cell = {
            let ug = session.underground.as_ref().expect("descended");
            let level = ug.level();
            let mut visited = std::collections::BTreeSet::new();
            let mut queue = std::collections::VecDeque::new();
            visited.insert(resident_cell);
            queue.push_back(resident_cell);
            let mut found = None;
            while let Some(cur) = queue.pop_front() {
                let dist = (cur.0 - resident_cell.0)
                    .abs()
                    .max((cur.1 - resident_cell.1).abs());
                if dist > reach {
                    found = Some(cur);
                    break;
                }
                for (dx, dy) in [(0, 1), (0, -1), (1, 0), (-1, 0)] {
                    let next = crate::lattice::Cell(cur.0 + dx, cur.1 + dy);
                    if visited.insert(next)
                        && level
                            .cells
                            .get(next)
                            .and_then(crate::underworld_level::movement_mode)
                            .is_some()
                    {
                        queue.push_back(next);
                    }
                }
            }
            found.expect(
                "Task 9's connectivity invariant: every level is connected well beyond \
                 `reach` cells from any of its own standable cells",
            )
        };

        session.underground.as_mut().expect("descended").cell = far_cell;
        assert!(
            !has_resident_mark(&session),
            "the resident's cell is now merely REMEMBERED (seen earlier, not lit \
             from here) — its mark must not appear, exactly as an entity's own \
             mark never appears from mere memory (spec §4.1.2)"
        );

        // Walk back: the resident's own cell is lit again, so the mark must
        // reappear — this is not a one-shot "seen once, gone forever" bug.
        session.underground.as_mut().expect("descended").cell = resident_cell;
        assert!(
            has_resident_mark(&session),
            "the mark must reappear once its cell is lit again"
        );
    }

    /// Fix round 1 (spec §3.6/§4.1.2): the pane and `examine` must AGREE
    /// about the chamber's derived resident — a creature the plate draws
    /// that `examine` calls nonexistent is this campaign's signature defect
    /// shape (Task 4 fixed the identical thing for `look`/`go`). Standing on
    /// the resident's own (lit) cell, the pane's `marks` carries it AND
    /// `examine <its noun>` answers about it, with the SAME text
    /// (`crate::underground::inhabitant_datum`) the mark's own `datum`
    /// carries — one noun, one datum, the same discipline
    /// `a_noun_at_both_grains_resolves_to_one_datum` already pins one band
    /// up.
    #[test]
    fn a_creature_underground_is_examinable_exactly_when_the_pane_draws_it() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let climate = session
            .wctx
            .climate
            .clone()
            .expect("seed 42 builds climate");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);

        let (kind, _source) = {
            let ug = session.underground.as_ref().expect("descended");
            crate::underground::chamber_resident(ug, &terrain, &climate).unwrap_or_else(|| {
                panic!(
                    "the fixture's entrance chamber must support a resident for this                      test to exercise anything"
                )
            })
        };
        let resident_cell = {
            let ug = session.underground.as_ref().expect("descended");
            crate::underground::resident_cell(ug.level())
                .expect("Task 9's connectivity invariant guarantees a standable cell")
        };

        session.underground.as_mut().expect("descended").cell = resident_cell;
        session.mark_underground_seen();

        let snap = session.snapshot().expect("a descended session snapshots");
        let mark_datum = match snap.spatial {
            crate::snapshot::SpatialChannel::Underground { level } => level
                .marks
                .iter()
                .find(|m| m.noun == kind.0)
                .unwrap_or_else(|| {
                    panic!(
                        "the pane must draw {} while standing on its own cell",
                        kind.0
                    )
                })
                .datum
                .clone(),
            other => panic!("expected the underground band, got {other:?}"),
        };

        let examine_reply = session.examine_underground(kind.0);
        assert_ne!(
            examine_reply,
            format!("You see no {} here.", kind.0),
            "examine must not deny a creature the SAME turn's pane draws it"
        );
        assert_eq!(
            examine_reply, mark_datum,
            "the mark's own datum and examine's reply must be the SAME text —              one noun, one datum, across the pane and the prose"
        );
    }

    /// The negative half of the agreement above: where the resident is NOT
    /// drawn (its cell merely remembered, not lit), `examine` must still
    /// refuse it — otherwise the fog `underground_level` correctly respects
    /// on the pane would leak straight through `examine`, which the
    /// coordinator's own fix-round note calls a worse bug than the one this
    /// round fixes. A test that only checked the positive half would pass on
    /// a chamber that happens to hold nobody at all; this one is only
    /// meaningful because the fixture DOES have a resident, pinned by the
    /// same panic-if-absent guard the positive test uses.
    #[test]
    fn an_unlit_resident_stays_unexaminable() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let climate = session
            .wctx
            .climate
            .clone()
            .expect("seed 42 builds climate");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);

        let (kind, _source) = {
            let ug = session.underground.as_ref().expect("descended");
            crate::underground::chamber_resident(ug, &terrain, &climate).unwrap_or_else(|| {
                panic!(
                    "the fixture's entrance chamber must support a resident for this                      test to exercise anything"
                )
            })
        };
        let resident_cell = {
            let ug = session.underground.as_ref().expect("descended");
            crate::underground::resident_cell(ug.level())
                .expect("Task 9's connectivity invariant guarantees a standable cell")
        };
        let reach = session.sight_reach();

        // Fold the shadowcast from the resident's own cell first, so it is
        // genuinely SEEN (remembered), then walk far enough away — by BFS,
        // the same technique
        // `a_creature_underground_is_drawn_only_while_lit` uses — that it is
        // no longer LIT. Remembered-but-unlit is the one state that could
        // leak through a naive `examine` fix.
        session.underground.as_mut().expect("descended").cell = resident_cell;
        session.mark_underground_seen();

        let far_cell = {
            let ug = session.underground.as_ref().expect("descended");
            let level = ug.level();
            let mut visited = std::collections::BTreeSet::new();
            let mut queue = std::collections::VecDeque::new();
            visited.insert(resident_cell);
            queue.push_back(resident_cell);
            let mut found = None;
            while let Some(cur) = queue.pop_front() {
                let dist = (cur.0 - resident_cell.0)
                    .abs()
                    .max((cur.1 - resident_cell.1).abs());
                if dist > reach {
                    found = Some(cur);
                    break;
                }
                for (dx, dy) in [(0, 1), (0, -1), (1, 0), (-1, 0)] {
                    let next = crate::lattice::Cell(cur.0 + dx, cur.1 + dy);
                    if visited.insert(next)
                        && level
                            .cells
                            .get(next)
                            .and_then(crate::underworld_level::movement_mode)
                            .is_some()
                    {
                        queue.push_back(next);
                    }
                }
            }
            found.expect(
                "Task 9's connectivity invariant: every level is connected well beyond                  `reach` cells from any of its own standable cells",
            )
        };
        session.underground.as_mut().expect("descended").cell = far_cell;

        let snap = session.snapshot().expect("a descended session snapshots");
        let still_drawn = match snap.spatial {
            crate::snapshot::SpatialChannel::Underground { level } => {
                level.marks.iter().any(|m| m.noun == kind.0)
            }
            other => panic!("expected the underground band, got {other:?}"),
        };
        assert!(
            !still_drawn,
            "sanity check: the pane must not draw a merely-remembered resident"
        );

        assert_eq!(
            session.examine_underground(kind.0),
            format!("You see no {} here.", kind.0),
            "a remembered-but-unlit resident must stay unexaminable, exactly as              it stays undrawn — the fog must not leak through examine"
        );
    }

    /// The snapshot's spatial channel and the `map` verb, underground —
    /// **the fold, retired (The Gallery, Task 8; spec §5).**
    ///
    /// Found at The Panes' merge, not during either campaign: The Deep Realm
    /// added `underground` while The Panes added the spatial channel, in
    /// parallel worktrees, and the textual merge was clean because they
    /// touched different lines of the same file. `SpatialChannel` enumerates
    /// bands; The Deep Realm added one; neither campaign's chronicle mentions
    /// the other's surface. That is precisely the semantic collision
    /// `make preflight` says it cannot score. **This test's own name and
    /// body have changed twice, and the invariant it pins has not.** Its
    /// first form (`the_underground_band_folds_into_walk_as_map_does`)
    /// asserted a FOLD — the pane and the `map` verb both answering `walk`
    /// underground — under the argument that whichever answer the sim
    /// settles on, one change must move both, so a silent fold could never
    /// hide behind a passing test. Task 7 answered the PANE half (its own
    /// `band: "underground"`, carrying `vessel/level/v1`) while deliberately
    /// leaving the verb one turn behind, so that test's second form pinned
    /// the two answering DIFFERENTLY on purpose — a transitional state, not
    /// a destination — for exactly as long as it took Task 8 to land the
    /// verb's own arm.
    ///
    /// **This is that landing.** Renamed again because a reader scanning the
    /// test list must not see a fold reported as intact when there is none
    /// left to report — the old name was half-false the moment Task 7
    /// landed and would have been wholly false the moment this task did, had
    /// it survived unrenamed. What survives, under the new name, is the
    /// original argument verbatim: the pane and the verb cannot drift apart
    /// here, so one change — this one — must move both, and this test now
    /// asserts them agreeing on the new answer rather than the old one.
    #[test]
    fn the_pane_and_the_verb_agree_underground() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        assert!(
            session.underground.is_some(),
            "the fixture must have descended"
        );

        // The pane: its own `underground` band (Task 7), carrying a level
        // rather than a chart or a plan.
        let snap = session.snapshot().expect("a descended session snapshots");
        match &snap.spatial {
            crate::snapshot::SpatialChannel::Underground { .. } => {}
            other => panic!(
                "the underground band no longer answers `underground` — if that \
                 changed again, `SpatialChannel`'s own doc and this test must \
                 change WITH it: {other:?}"
            ),
        }
        let json = crate::snapshot_json(&snap);
        assert!(
            json.contains(r#""band":"underground""#),
            "the wire tag must read `underground`: {json:.120}"
        );

        // The verb, in the same state: the level now, never the surface
        // chart and never the indoor refusal. `plan_here` prints a
        // ` legend: ` line; the walk-band chart prints a `[lens:` header —
        // so all three are told apart by content, not by length.
        let out = match session.handle("map") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("map must not release"),
        };
        assert!(
            out.starts_with("[level:"),
            "map underground must draw the level, not the country overhead: {out}"
        );
        assert!(
            !out.contains("[lens:"),
            "map underground must not fall through to the walk-band chart: {out}"
        );
        assert!(
            !out.contains(INDOOR_CHART_REFUSAL),
            "map underground must not take the indoor refusal: {out}"
        );
    }

    /// Fix round 1 finding 1: `handle_ooc`'s underground `map` arm and
    /// `handle`'s underground `map` arm are two copies of one rule
    /// (`self.underground.is_some() && rest.is_empty() => self.level_here()`)
    /// with nothing to distinguish them — `level_here` takes no `eyes`
    /// parameter, unlike the `inside` pair `!map`/`map` genuinely diverge on
    /// (`OBJECTIVE_EYES` vs `self.eyes`; see `ooc_objective.rs`'s own
    /// discriminators). `ooc_objective.rs`'s two `!map` tests cover only the
    /// walk band and the indoor chamber, so nothing exercised `!map`
    /// underground at all, and a future edit to only one of the two arms
    /// (say, the bare form's refusal text) would go undetected. This test
    /// pins the AGREEMENT the duplication depends on, not merely that each
    /// arm works in isolation — that is the property two unsynchronized
    /// copies threaten.
    #[test]
    fn bang_map_and_bare_map_agree_underground() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        assert!(
            session.underground.is_some(),
            "the fixture must have descended"
        );

        let bare = match session.handle("map") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("map must not release"),
        };
        let bang = match session.handle("!map") {
            Turn::Out(t) => t,
            Turn::Released(_) => panic!("!map must not release"),
        };
        assert!(
            bare.starts_with("[level:"),
            "the bare form must draw the level: {bare}"
        );
        assert_eq!(
            bare, bang,
            "handle_ooc's underground `map` arm and handle's underground `map` \
             arm must agree — both call `level_here`, which takes no `eyes` \
             parameter, so nothing legitimately distinguishes them"
        );
    }

    /// Fix round 1 finding 2: `UNDERGROUND_CHART_REFUSAL` was implemented
    /// and reachable (the guard ordering makes it fire only when `rest` is
    /// non-empty) but exercised by no test anywhere. The indoor precedent
    /// this copies, `map_indoors_draws_the_plan_and_map_out_indoors_refuses`,
    /// asserts both `"map out"` and `"map out 2"` for exactly the reason
    /// given here: the refusal arm is the one nobody drives by accident.
    #[test]
    fn map_out_is_refused_underground() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);
        assert!(
            session.underground.is_some(),
            "the fixture must have descended"
        );

        for line in ["map out", "map out 2"] {
            let refused = match session.handle(line) {
                Turn::Out(t) => t,
                Turn::Released(_) => panic!("map must not release"),
            };
            assert_eq!(
                refused, UNDERGROUND_CHART_REFUSAL,
                "{line:?} underground must refuse rather than ignore the argument"
            );
            let refused_ooc = match session.handle(&format!("!{line}")) {
                Turn::Out(t) => t,
                Turn::Released(_) => panic!("map must not release"),
            };
            assert_eq!(
                refused_ooc, UNDERGROUND_CHART_REFUSAL,
                "!{line:?} underground must refuse rather than ignore the argument"
            );
        }
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
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        let shown = match session.delve_at(vertex, cave) {
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

    /// The Gallery, Task 3: `delve` no longer sets a bucket — it stands the
    /// possession on a real cell of a real generated level.
    #[test]
    fn delve_places_the_possession_on_a_real_cell_of_a_generated_level() {
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session
            .wctx
            .terrain
            .clone()
            .expect("seed 42 builds terrain");
        let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
        session.delve_at(vertex, cave);

        let ug = session.underground.as_ref().expect("a resolved descent");
        assert!(!ug.descent.is_empty(), "a descent has at least one rung");
        assert_eq!(ug.rung, 0, "you enter at the top rung");
        assert!(
            matches!(
                ug.level().cells.get(ug.cell),
                Some(crate::underworld_level::LevelCellKind::Floor)
                    | Some(crate::underworld_level::LevelCellKind::Flooded)
            ),
            "the possession stands on a standable cell, not inside rock"
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
    fn place_agent_now(session: &mut Session<'_>, who: EntityId, room: &Facet) {
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
            SpatialChannel::Walk { .. } | SpatialChannel::Underground { .. } => {
                panic!("expected the chamber band")
            }
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
        // stepped one place. Built rather than invented so `Facet::pack`
        // (which rejects any digit >= 4) still accepts it.
        let elsewhere = {
            let mut path = room.path.clone();
            let last = path.last_mut().expect("a walk-band address has a path");
            *last = (*last + 1) % 4;
            Facet {
                face: room.face,
                path,
            }
        };
        let nowhere: std::collections::BTreeSet<EntityId> = Default::default();

        // THE ARRIVAL. `before` says the creature was elsewhere; the ledger
        // still says it is here; `moved` is nonzero so the early return does
        // not swallow the call.
        let arriving: Vec<Facet> = other_bodies(&session.bodies, session.driven)
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
        let was_here: Vec<Facet> = other_bodies(&session.bodies, session.driven)
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

    /// A hand-built [`WordViews`] for [`render_testimony`]'s tests — the
    /// three surface views are display-only for this fn, so a fixed dummy
    /// suffices, the same freedom `testimony.rs`'s own tests take with
    /// `ExposureClass`.
    fn dummy_word(roman: &str) -> hornvale_language::WordViews {
        hornvale_language::WordViews {
            roman: roman.to_string(),
            ipa: String::new(),
            espeak: String::new(),
        }
    }

    /// The DIRECT arm (The Confidant, Task 6): a culture that has the word
    /// for its own true state reports it, and the truth SHOULD be visible
    /// here — the invariant is about a DIVERGENT report, not about hiding
    /// truth that was correctly spoken.
    #[test]
    fn render_testimony_direct_reports_the_word_and_its_own_concept() {
        let (turn, heard) = render_testimony(
            "the herder",
            AffectLabel::Content,
            Some(Testimony::Spoken(FeltStateWord::Direct(dummy_word(
                "Vrenn",
            )))),
        );
        assert!(
            turn.contains("Vrenn"),
            "the turn must carry the actual spoken word, got: {turn}"
        );
        assert!(
            turn.contains("content"),
            "a Direct report's gloss is the true state's own concept, got: {turn}"
        );
        assert_eq!(
            heard,
            Some("content".to_string()),
            "the heard value must be the concept id, not the raw conlang word"
        );
    }

    /// A culture with no felt-state word at all says nothing, and lands
    /// nothing — `testify` returning `None` must not fabricate a report.
    #[test]
    fn render_testimony_none_says_nothing_and_lands_nothing() {
        let (turn, heard) = render_testimony("the herder", AffectLabel::Lost, None);
        assert!(
            !turn.is_empty(),
            "a body with no word at all must still answer SOMETHING"
        );
        assert_eq!(
            heard, None,
            "nothing was said, so nothing may land as heard"
        );
    }

    /// **THE DESIGN INVARIANT (Task 6 Step 5, spec §3.1): the player is
    /// NEVER shown the arbitration.** A divergent testimony (host truly
    /// `Helpless`, culture has no word for it, nearest known word is
    /// `Eager`'s) must carry `Eager`'s concept and word ONLY — `Helpless`
    /// must appear nowhere in the rendered turn or the heard value, in any
    /// casing. This is mutation-proved in the Task 6 report: substituting
    /// `label` for `reported_as` in `render_testimony`'s `Nearest` arm turns
    /// this test red.
    #[test]
    fn the_arbitration_never_reaches_a_divergent_utterance() {
        let true_label = AffectLabel::Helpless;
        let (turn, heard) = render_testimony(
            "the herder",
            true_label,
            Some(Testimony::Spoken(FeltStateWord::Nearest {
                word: dummy_word("Grenth"),
                reported_as: AffectLabel::Eager,
                reason: hornvale_language::GapReason::Experiential(
                    "this test culture never named it".to_string(),
                ),
            })),
        );
        let lowered = turn.to_lowercase();
        assert!(
            !lowered.contains("helpless"),
            "the true state must never appear in a divergent turn, got: {turn}"
        );
        assert!(
            turn.contains("Grenth"),
            "the actually-spoken word must still appear, got: {turn}"
        );
        assert!(
            turn.contains("eager"),
            "the REPORTED concept must appear (the lie, not the truth), got: {turn}"
        );
        assert_eq!(
            heard,
            Some("eager".to_string()),
            "the heard entry must record what was actually SAID (eager), never the \
             true state (helpless)"
        );
    }

    /// The WITHHELD arm (The Reticence, Task 5). A refusal is the one arm
    /// that lands nothing: a `heard` entry written on a refusal would make
    /// silence informative, which is the exact property
    /// [`render_testimony`]'s own doc names for this variant.
    ///
    /// FIRES WHEN: the refusal arm starts returning a concept id (from
    /// `label` or from anywhere else), or the refusal text starts naming the
    /// state the host declined to name.
    #[test]
    fn render_testimony_withheld_lands_nothing_and_names_no_state() {
        let (turn, heard) = render_testimony(
            "the herder",
            AffectLabel::Helpless,
            Some(Testimony::Withheld),
        );
        assert!(
            !turn.is_empty(),
            "a refusal is still a turn — the player must be told the host declined"
        );
        let lowered = turn.to_lowercase();
        assert!(
            !lowered.contains("helpless"),
            "a refusal must not name the state it refused to name, got: {turn}"
        );
        assert_eq!(
            heard, None,
            "a refusal lands NOTHING: knowledge that recorded a felt state here \
             would make silence informative"
        );
    }

    /// **THE CAMPAIGN'S CENTRAL INVARIANT FOR A DELIBERATE LIE (The
    /// Reticence, Task 5, spec §4.3): a host that lies is heard to have
    /// lied.** A dissembling host truly [`AffectLabel::Helpless`] claims
    /// [`AffectLabel::Content`] — so `content` must reach both the player's
    /// ear and the `heard` value, and `helpless` must reach neither, in any
    /// casing. This is the [`Testimony::Falsehood`] twin of
    /// [`the_arbitration_never_reaches_a_divergent_utterance`] above, which
    /// holds the same discipline for a lexical gap.
    ///
    /// MUTATION-PROVED (the final-fix wave): rewrite the argument of the
    /// `Falsehood` arm's `concept_id` call from `claimed` to `label` — the
    /// one-token change that makes a deliberate lie report the host's true
    /// state — and this test turns RED on both the `helpless`-leak assertion
    /// and the `heard` equality. Before this test existed the whole
    /// 653-test vessel crate passed under that mutation, which fed the TRUE
    /// state into the player's ear AND into
    /// `Knowledge["{body_label}::feels"]`, inverting the campaign's premise
    /// while every gate stayed green.
    ///
    /// (The repro command is deliberately NOT quoted verbatim here:
    /// `scripts/mutate.py` refuses a target it finds twice, and a doc
    /// comment holding the exact source line is the second occurrence.)
    ///
    /// FIRES WHEN: the lie's gloss is re-derived from the arbitration's true
    /// answer instead of from the claim the spoken word actually names.
    #[test]
    fn a_deliberate_lie_carries_the_claim_and_never_the_truth() {
        let true_label = AffectLabel::Helpless;
        let (turn, heard) = render_testimony(
            "the herder",
            true_label,
            Some(Testimony::Falsehood {
                word: dummy_word("Sallim"),
                claimed: AffectLabel::Content,
            }),
        );
        let lowered = turn.to_lowercase();
        assert!(
            !lowered.contains("helpless"),
            "the true state must never appear in a lie's turn text, got: {turn}"
        );
        assert!(
            turn.contains("Sallim"),
            "the word the host actually said must appear, got: {turn}"
        );
        assert!(
            turn.contains("content"),
            "the CLAIMED state's concept is what a listener hears, got: {turn}"
        );
        assert_eq!(
            heard,
            Some("content".to_string()),
            "the heard entry must record the lie the host told (content), never \
             the state it is actually in (helpless) — a knowledge store that \
             recorded the truth here would make lying free"
        );
    }

    /// The COSTLY arm (The Reticence, Task 5). Truth, plus the arbitration's
    /// discarded ranks a forthcoming host never mentions — so this arm is
    /// the ONE place the true label is both reported and accompanied by the
    /// residue, and the residue must actually reach the text.
    ///
    /// Both directions are pinned, because only the pair discriminates: a
    /// non-empty residue must be named, and an EMPTY one must render
    /// byte-identically to the ordinary [`Testimony::Spoken`] answer rather
    /// than emitting a dangling "it costs something to say:" with nothing
    /// after it.
    ///
    /// FIRES WHEN: the residue is dropped from the rendered line, or the
    /// empty-residue guard is removed.
    #[test]
    fn a_costly_truth_names_its_residue_and_an_empty_one_reads_as_ordinary() {
        let (turn, heard) = render_testimony(
            "the herder",
            AffectLabel::Content,
            Some(Testimony::Costly {
                word: FeltStateWord::Direct(dummy_word("Vrenn")),
                revealed: vec![DriveKind::Thirst, DriveKind::Fatigue],
            }),
        );
        assert!(
            turn.contains("Vrenn"),
            "a costly answer is still an ordinary lexical report, got: {turn}"
        );
        assert!(
            turn.contains("content"),
            "a costly answer is TRUTHFUL — the true state's concept is correct here, got: {turn}"
        );
        assert_eq!(
            heard,
            Some("content".to_string()),
            "a costly answer lands the true concept, exactly as a Spoken one does"
        );
        assert!(
            turn.contains("Thirst") && turn.contains("Fatigue"),
            "the residue is the WHOLE point of this arm and must reach the player, got: {turn}"
        );

        // The empty-residue half: identical to the plain Spoken rendering.
        let (plain, plain_heard) = render_testimony(
            "the herder",
            AffectLabel::Content,
            Some(Testimony::Spoken(FeltStateWord::Direct(dummy_word(
                "Vrenn",
            )))),
        );
        let (bare, bare_heard) = render_testimony(
            "the herder",
            AffectLabel::Content,
            Some(Testimony::Costly {
                word: FeltStateWord::Direct(dummy_word("Vrenn")),
                revealed: Vec::new(),
            }),
        );
        assert_eq!(
            bare, plain,
            "a costly answer with nothing to reveal must read exactly like an \
             ordinary one, not trail an empty cost clause"
        );
        assert_eq!(bare_heard, plain_heard, "and land exactly the same concept");
    }

    /// The Coercion, Task 4 fix round: `ImposedController` is now wired at
    /// the driven body's own `!wait` walk (`Session::wait`'s call into
    /// `step_one_with_controller`), selected exactly when the body is
    /// possessed. That swap is checked directly here rather than assumed
    /// inert: `PlayerController`'s intent is unconditionally `Hold`, so a
    /// FREE body's felt state after `!wait` is always read at whatever
    /// position it started the tick standing in; `ImposedController`
    /// delegates to `DefaultController`, whose intent is
    /// `resolution.intent` unchanged, so a POSSESSED body's own solo walk
    /// can actually act — move, drink, rest, eat — within the very same
    /// tick, landing in a felt state a frozen walk could not have reached.
    ///
    /// Seed 42, one `!wait 1` from a fresh session, is a reliable
    /// discriminator: the free body reads fatigue-pursuing and eager (it has
    /// not yet had the chance to rest), while the possessed body — free to
    /// act on its own arbitration under `ImposedController` — has already
    /// served that need this tick and reads idle and content. Two session
    /// instances from the same seed, differing only in whether `!possess`
    /// was called first, is how this isolates the controller swap from
    /// every other source of variation.
    ///
    /// **The ledger stays untouched by this test's own construction**: `!wait`
    /// (`Session::wait`) discards the driven body's own facts unconditionally
    /// regardless of which controller answered (see that call site's own
    /// doc), so this test asserts only on the felt-state trio, never on
    /// `committed_fact_count()` — a ledger assertion here would be asserting
    /// something this swap was never claimed to change.
    #[test]
    fn driven_felt_state_can_move_under_an_imposed_controller_during_wait() {
        let world = seam_world();
        let (mut free, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let (mut held, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let _ = held.handle("!possess");
        assert!(
            held.possessor().is_some(),
            "possession must actually be open for this to test the swap at all"
        );

        let _ = free.handle("!wait 1");
        let _ = held.handle("!wait 1");

        assert_eq!(
            free.driven_mode(),
            Some(Mode::Pursuing(DriveKind::Fatigue)),
            "seed 42's free body, held to Hold and so frozen at its starting \
             position, still pursues the fatigue it never got to act on"
        );
        assert_eq!(
            held.driven_mode(),
            Some(Mode::Idle),
            "seed 42's possessed body, free to act under ImposedController, \
             has already served that need this tick and reads idle"
        );
        assert_ne!(
            free.driven_mode(),
            held.driven_mode(),
            "the controller swap is a real behavioural difference, not an \
             identity — PlayerController's forced Hold and \
             ImposedController's unforced resolution.intent are genuinely \
             different intents"
        );
        assert_ne!(
            free.driven_affect(),
            held.driven_affect(),
            "the felt-state trio ask() draws from moves with the swap too"
        );
    }

    /// The Coercion, spec §7 H2 — "the death terminator is unreachable": no
    /// currently-shipped verb produces a [`POSSESSION_ENDED`] fact whose
    /// reason is `"died"`.
    ///
    /// **THE LIVE EVIDENCE FOR THAT CONCLUSION IS THE GREP, NOT THIS
    /// FIXTURE.** Spec §6: no live death state exists anywhere in
    /// `windows/vessel` today, and that predicate's own doc comment
    /// (`:341`) only NAMES `"died"` as the reason once mortality exists —
    /// grep-verified, the literal string `"died"` is CONSTRUCTED nowhere
    /// under `windows/vessel/src`, in no match arm, so nothing could build
    /// that fact today regardless of what a player types. The conclusion
    /// rests on that; the loop below corroborates it over a roster.
    ///
    /// **WHAT THIS FIXTURE ACTUALLY EXERCISES IS 12 OF THE 34, NOT 34 —
    /// state that plainly rather than let the roster count imply
    /// otherwise.** Every verb here runs against a session that has just
    /// been `!possess`ed, and a possessed body is exactly what
    /// `gated_by_the_body` refuses in front of: all 24
    /// [`IN_CHARACTER_VERBS`] are turned away by the body-state gate BEFORE
    /// their handlers run, so only the 3 [`SESSION_CONTROL`] verbs and the
    /// 9 Group-A operator instruments below reach any dispatch arm at all.
    /// The `assert_eq!` on `roster.len()` pins the ROSTER's size — the
    /// stated denominator — and must not be read as pinning the exercised
    /// population, which is 12. **Measured, not inferred**: a scratch probe
    /// of this exact loop, counting lines whose output carries the gate's own
    /// refusal ("another will holds this body"), reported `roster=30
    /// gate-refused=18` — `ask back climb consult delve dive enter examine go
    /// knows look map needs out sleep surface wait write` (The Offer, Task 5,
    /// added `warm` to [`IN_CHARACTER_VERBS`] afterwards, and The Latch's
    /// fix wave added `clear`; the count above is re-derived arithmetically
    /// from that measurement — same 12 ungated verbs, two more gated ones,
    /// 18→19→20 and 30→31→32 — rather than re-run, since
    /// `gated_by_the_body`'s own logic is untouched by either addition).
    /// The Gallery's Task 5 added two more gated verbs still, `down`/`up`,
    /// re-derived the identical way: 20→22 and 32→34, the 12 ungated verbs
    /// again untouched. The Chattel's Task 11 added `open`/`close` — both
    /// gated, both in-character — re-derived the same way once more: 22→24
    /// and 34→36, and the 12 ungated verbs untouched for the fourth time,
    /// since `gated_by_the_body`'s own logic is unchanged by an entry in the
    /// roster it reads.
    ///
    /// **So this is a weak tripwire, not the tripwire that turns red the
    /// day mortality ships.** If a death terminator ever arrives through an
    /// IN-CHARACTER verb — the likeliest route, since dying is something a
    /// body does — this construction would not catch it: the gate refuses
    /// that verb first and the loop sees nothing. It would catch a death
    /// arm reached through session control or the operator namespace. The
    /// durable check is the grep above; when mortality lands, re-derive
    /// this fixture rather than trusting it to have objected.
    ///
    /// The stated denominator (spec §7's own requirement): the full shipped
    /// verb roster this file itself classifies is the SUM of three groups —
    /// [`IN_CHARACTER_VERBS`] (28, since The Offer's Task 5 added `warm`,
    /// The Latch's fix wave added `clear`, The Gallery's Task 5 added
    /// `down`/`up`, The Chattel's Task 11 added `open`/`close` and its Task
    /// 12 added `take`/`drop`/`put`/`carrying`),
    /// [`SESSION_CONTROL`] (3: `release`/`quit`/`exit`), and the nine
    /// out-of-character-ONLY operator instruments `handle_ooc`'s Group A
    /// dispatches (`why`/`npcs`/`help`/`eyes`/`whoami`/`provoke`/`soothe`/
    /// `possess`/`unpossess`) — **40** total. Group B's six `!`-twins
    /// (`!map`/`!examine`/`!needs`/`!wait`/`!look`/`!knows`) are deliberately
    /// NOT counted a second time — [`HELP`]'s own text calls them "the
    /// out-of-character halves" of verbs already among the 24: the same verb
    /// under the other mood, not a distinct one.
    ///
    /// Each verb runs against its OWN fresh, freshly-possessed session
    /// (`!possess` first, so a possession is genuinely open for every verb
    /// to act against — including `!unpossess`, the only verb anywhere in
    /// the tree that can commit a [`POSSESSION_ENDED`] fact at all) rather
    /// than one long sequence through a single session: `release`/`quit`
    /// return `Turn::Released`, and a single shared session would let those
    /// two short-circuit — or at least complicate the provenance of — every
    /// verb tried after them in roster order. No verb is given a crafted
    /// argument to make it succeed: since no dispatch arm anywhere
    /// constructs the string `"died"` regardless of input, a bare
    /// invocation already covers the whole surface this loop can reach —
    /// which, per the paragraph above, is the 12 ungated verbs, not the 40
    /// the roster names.
    #[test]
    fn h2_no_shipped_verb_can_end_a_possession_by_death() {
        let world = seam_world();
        let operator_only: [&str; 9] = [
            "why",
            "npcs",
            "help",
            "eyes",
            "whoami",
            "provoke",
            "soothe",
            "possess",
            "unpossess",
        ];
        let roster: Vec<&str> = IN_CHARACTER_VERBS
            .into_iter()
            .chain(SESSION_CONTROL)
            .chain(operator_only)
            .collect();
        assert_eq!(
            roster.len(),
            40,
            "the stated denominator: 28 IN_CHARACTER_VERBS + 3 SESSION_CONTROL \
             + 9 Group-A operator instruments the OOC namespace alone \
             dispatches. This pins the ROSTER's size, NOT the exercised \
             population: under a possessed body the gate refuses all 28 \
             in-character verbs, so 12 reach a dispatch arm — see this \
             test's doc comment"
        );

        for verb in &roster {
            let (mut s, _) =
                Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
            let _ = s.handle("!possess");
            let line = if operator_only.contains(verb) {
                format!("!{verb}")
            } else {
                (*verb).to_string()
            };
            let _ = s.handle(&line);
            let died = s.ledger.iter().any(|f| {
                f.predicate == POSSESSION_ENDED && f.object == Value::Text("died".to_string())
            });
            assert!(
                !died,
                "`{line}` produced a possession-ended fact with reason \"died\" \
                 — mortality does not exist yet (spec §6); if this fires, the \
                 death arm is already correct and shipped, and spec §6 wants \
                 updating, not this test deleted"
            );
        }
    }
}
