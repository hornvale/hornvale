//! The action layer: what a body can do, and the search spaces that plan
//! sequences of it.
//!
//! Extracted from `liveness.rs` by The Tackle (Arc I.a of The Bridle) with
//! no behavioural change, so that The Deed can unify the player's verbs
//! with these without doing surgery inside a 14,700-line file.

use crate::interior::AnchorId;
use hornvale_kernel::{AStarSolver, Facet, RoomMeshMemo, SearchSpace, Solver, astar};

/// A GOAP action — a precondition/effect transformation over the plan state.
/// Minimal + heterogeneous (the precondition chain needs two kinds); the MAP-27
/// authored-verb DSL is a followup.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Action {
    /// Walk to an adjacent room (precondition: adjacency; effect: position).
    MoveTo(Facet),
    /// Drink (precondition: at the water room; effect: hydrated).
    Drink,
    /// Rest (precondition: none — a body rests where it stands; effect: some
    /// fatigue eased, the body staying conscious throughout) — The Slumber's
    /// discharge action, the fatigue analogue of `Drink`.
    ///
    /// **This doc used to read "Rest / sleep", and the slash is exactly what
    /// The Wicket's Task 8 split.** Resting leaves the body conscious and
    /// watchful and restores less per unit time; [`Action::Sleep`] renders it
    /// unconscious and restores more. One variant could not carry both, which
    /// is why unconsciousness had to live on the player's route instead.
    ///
    /// **It also used to read "(precondition: at home; effect: fatigue
    /// reset)", and no at-home precondition has ever been enforced
    /// anywhere**: [`precondition_reads_committed_state`] already answers
    /// `false` for `Rest`, and the creature layer's own fatigue drive
    /// (`liveness.rs`) says outright that a creature beds down *where it is*,
    /// so an explorer sleeps in the field at nightfall rather than trekking
    /// home. `home` survives as a reserved hook for a future rest-QUALITY
    /// refinement, never a gate; see
    /// `PSY-rest-quality-is-a-grade-not-a-gate` in the idea registry.
    Rest,
    /// Sleep (precondition: none — a body sleeps where it stands; effect:
    /// unconsciousness for the act's own span, and the larger of the two
    /// fatigue recoveries). Distinct from [`Action::Rest`], which leaves the
    /// body conscious and watchful and restores less per unit time. The
    /// Wicket, Task 8.
    ///
    /// **The distinction is the act's, not the caller's.** Before this variant
    /// existed, unconsciousness was a property of the PLAYER'S ROUTE —
    /// `Session::sleep` set `wake_at` itself, so a creature proposing
    /// `Action::Rest` never went under by that route even though it was
    /// running the same act. [`renders_unconscious`](crate::liveness::
    /// renders_unconscious) reads it off the action now, so both routes reach
    /// the same state.
    Sleep,
    /// Eat / graze (precondition: standing on a room rich enough to feed;
    /// effect: hunger reset) — The Provender's discharge action, the hunger
    /// analogue of `Drink`.
    Eat,
    /// Walk to another anchor inside the current room (The Threshold).
    /// Precondition: adjacency in the room's anchor graph. Effect: fine
    /// position, which is NEVER serialized (decision 0069) — which is what
    /// makes this the one action catch-up may replay.
    MoveWithin(AnchorId),
    /// Recount a co-located NPC's dated history (The Deed, group A). An
    /// operator instrument — no creature ever plans one.
    Why,
    /// List the derived NPCs sharing this world (The Deed, group A).
    Npcs,
    /// Print the verb list (The Deed, group A).
    Help,
    /// Report or switch whose eyes the chart is coloured through (The Deed,
    /// group A). One variant covers both the report and the set forms —
    /// they differ only in `rest`, which the dispatch layer reads, not this
    /// enum.
    Eyes,
    /// Name the one you possess (The Deed, group A).
    Whoami,
    /// Shift a co-located NPC's disposition upward, marked as the player's
    /// own act (The Deed, group A).
    Provoke,
    /// Ease a co-located NPC's disposition downward, marked as the player's
    /// own act (The Deed, group A).
    Soothe,
    /// Draw the OBJECTIVE chart — `!map` (The Deed, group B). The same
    /// renderer `map` draws, with the observer step declined, so it shows the
    /// terrain rather than this body's colour projection of it.
    ObjectiveMap,
    /// Examine a named thing objectively — `!examine` (The Deed, group B):
    /// the chamber band's sight narrowing taken to its permissive limit, so a
    /// creature standing here in the dark still answers.
    ObjectiveExamine,
    /// Read every co-located creature's felt state ungated — `!needs` (The
    /// Deed, group B), through the same affect arbitration `needs` reads.
    ObjectiveNeeds,
    /// Let the world move without you, out of character — `!wait` (The Deed,
    /// group B). The one out-of-character act that advances the clock (spec
    /// §3.4); its objective half is the motion narration, which names a
    /// creature the body could neither see arrive nor see go.
    ObjectiveWait,
    /// Read the place out of character — `!look` (The Deed, group B; shipped
    /// in Task 7's fix round). It relaxes NOTHING: `look`'s three band arms
    /// consult no sight, eyes, lens or knowledge, so this renders the same
    /// object through the same functions. What it bypasses is the BODY's gate
    /// (spec §2.2), which is a real difference only because Task 7 built one —
    /// asleep, the bare form is refused and this still answers.
    ObjectiveLook,
    /// Read your own knowledge out of character — `!knows` (The Deed, group B;
    /// shipped in Task 7's fix round). Same shape as [`Action::ObjectiveLook`]:
    /// the store is perception-filtered upstream at absorb time rather than by
    /// this renderer, so nothing here is relaxed — only the body's gate is
    /// bypassed.
    ObjectiveKnows,
}

/// Spec §3.2's group B, whose bare verbs are in character and whose `!` forms
/// are these — **all six**, one per group-B verb, since Task 7's fix round.
///
/// # The two shipped later, and why the reason changed rather than the verbs
///
/// Task 6 shipped four and withheld `!look`/`!knows` under a STOP rule that
/// an out-of-character form must DISCRIMINATE from its bare twin somewhere.
/// The four each relax a renderer's own gating parameter to its permissive
/// limit; neither `look` nor `knows` has such a parameter, so at Task 6 an
/// out-of-character half of either would have been an exact alias — a no-op
/// advertising a capability the surface did not have.
///
/// **Task 7 falsified the premise, not the rule.** The gate it built is the
/// BODY's, not a renderer's, and an out-of-character act bypasses it (spec
/// §2.2). So the two now discriminate exactly where spec §3.4 says the
/// namespace earns its keep — "observing a state you cannot act in requires a
/// clock you can still advance" — and withholding the most basic
/// observational verb from a body that cannot act was the sharpest case
/// against the exclusion. They are still not renderer forks: they render the
/// same object, through the same functions, with nothing relaxed.
pub const OBJECTIVE_HALVES: [Action; 6] = [
    Action::ObjectiveMap,
    Action::ObjectiveExamine,
    Action::ObjectiveNeeds,
    Action::ObjectiveWait,
    Action::ObjectiveLook,
    Action::ObjectiveKnows,
];

/// Whether an action's effect is position rather than a committed fact.
/// type-audit: bare-ok(flag: return)
pub fn is_movement(a: &Action) -> bool {
    matches!(a, Action::MoveTo(_) | Action::MoveWithin(_))
}

/// Whether an action's precondition reads committed state rather than position
/// alone. Today nothing does — every precondition in this file is adjacency or
/// standing-here — and catch-up (The Threshold) depends on that: it replays a
/// creature's movement while suppressing the actions that commit facts, which
/// reconstructs a past that could actually have happened only while no
/// movement is gated by a committed effect. A barred door needing unbarring
/// would end it.
///
/// **The guard is the exhaustive match, not the test.** An earlier draft took
/// `_a` and returned a bare `false`, with a test asserting the answer is
/// `false` for movement actions — which reduces to `assert!(!false)` and
/// cannot fail for any input. It would have caught a new action only if
/// whoever added it *remembered* to come here and flip the answer, i.e.
/// exactly when the guard was not needed. Matching every variant by name
/// instead means adding one to [`Action`] fails to COMPILE here, which is this
/// project's usual preference for structural enforcement over discipline.
/// type-audit: bare-ok(flag: return)
pub fn precondition_reads_committed_state(a: &Action) -> bool {
    match a {
        // Adjacency in the room graph; nothing committed is read.
        Action::MoveTo(_) => false,
        // Adjacency in the anchor graph; likewise.
        Action::MoveWithin(_) => false,
        // Standing at the water / at home / on forage — all positional.
        // `Sleep` (The Wicket, Task 8) has no precondition at all: a body
        // sleeps where it stands, which is the same answer for the same
        // reason as `Rest` beside it.
        Action::Drink | Action::Rest | Action::Sleep | Action::Eat => false,
        // The group-A operator instruments (The Deed) have no GOAP
        // precondition at all — no creature ever plans one, so catch-up
        // never asks this question about them. `false` for the same reason
        // as the arms above: nothing here reads position OR committed state
        // in the sense this function means.
        Action::Why
        | Action::Npcs
        | Action::Help
        | Action::Eyes
        | Action::Whoami
        | Action::Provoke
        | Action::Soothe => false,
        // Group B's objective halves (The Deed, Task 6), for the same reason:
        // no creature plans one, and none of the four reads committed state
        // as a PRECONDITION. `!wait` and `!needs` read the ledger to produce
        // their ANSWER, which is a different question — this one asks what
        // must already be true before the act may happen, and for these four
        // the answer is nothing.
        Action::ObjectiveMap
        | Action::ObjectiveExamine
        | Action::ObjectiveNeeds
        | Action::ObjectiveWait
        | Action::ObjectiveLook
        | Action::ObjectiveKnows => false,
    }
}

/// Whether catch-up (spec §5) may replay this action. Exactly the actions
/// whose effects are ephemeral: coarse `MoveTo` writes `agent-at`, and
/// `Drink`/`Rest`/`Sleep`/`Eat` each commit a fact, so only fine movement
/// qualifies. The partition is "does it commit", not "is it movement".
/// type-audit: bare-ok(flag: return)
pub fn is_replayable_in_catch_up(a: &Action) -> bool {
    matches!(a, Action::MoveWithin(_))
}

impl Action {
    /// Every action kind the planner can emit, one representative per variant
    /// — the roster the correspondence audit reconciles against the concept
    /// registry (The Actants). `MoveTo` carries an address, so its
    /// representative uses a placeholder: the audit reads only which VARIANTS
    /// exist, never their payloads.
    ///
    /// Kept exhaustive by [`action_variants_must_all_be_rostered`]: a new
    /// variant fails to compile until it is listed here, so an act can never
    /// enter the world without the audit noticing it has no word.
    pub fn all() -> Vec<Action> {
        vec![
            Action::MoveTo(Facet {
                face: 0,
                path: Vec::new(),
            }),
            Action::Drink,
            Action::Rest,
            Action::Sleep,
            Action::Eat,
            Action::MoveWithin(crate::interior::AnchorId(0)),
            Action::Why,
            Action::Npcs,
            Action::Help,
            Action::Eyes,
            Action::Whoami,
            Action::Provoke,
            Action::Soothe,
            Action::ObjectiveMap,
            Action::ObjectiveExamine,
            Action::ObjectiveNeeds,
            Action::ObjectiveWait,
            Action::ObjectiveLook,
            Action::ObjectiveKnows,
        ]
    }

    /// The concept name that would name this act, whether or not it is
    /// registered. The audit reports the ones that are not.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn concept_name(&self) -> &'static str {
        match self {
            // Both movement variants answer to one concept. They differ in
            // SCALE — between rooms, and between anchors inside a room (The
            // Threshold) — which is a mechanism distinction, not a vocabulary
            // one: a language has a word for going, not two words separated by
            // how far. If a people ever needs to say `approach` distinctly from
            // `move`, that is a new concept and a deliberate one, not a second
            // name minted here by accident.
            Action::MoveTo(_) | Action::MoveWithin(_) => "move",
            Action::Drink => "drink",
            Action::Rest => "rest",
            // Already a registered language concept before this variant
            // existed (`domains/language/src/accession.rs`, `packs.rs`), so
            // naming it here mints nothing and the orphan-acts audit stays
            // quiet — verified rather than assumed (The Wicket, Task 8).
            Action::Sleep => "sleep",
            Action::Eat => "eat",
            // Group A's Task 2 concepts (The Deed), verbatim from the
            // derived roster: `why`->`recount`, `npcs`->`survey`,
            // `help`->`help`, `eyes`->`lens`, `whoami`->`identify`,
            // `provoke`->`provoke`, `soothe`->`soothe`.
            Action::Why => "recount",
            Action::Npcs => "survey",
            Action::Help => "help",
            Action::Eyes => "lens",
            Action::Whoami => "identify",
            Action::Provoke => "provoke",
            Action::Soothe => "soothe",
            // Group B's Task 2 concepts (The Deed), read VERBATIM from the
            // cohort-11 comment in `domains/language/src/accession.rs`:
            // `map`->`chart`, `examine`->`look`, `needs`->`sense`,
            // `wait`->`wait`. Taken from that comment rather than derived
            // positionally, precisely because a positional reading of "these
            // verbs onto those concepts" is the silent mispairing the roster
            // cannot afford — and because two of the six group-B verbs ship
            // no variant at all, so the positions do not line up.
            //
            // `examine` and `look` fold onto the ONE concept `look` by SCOPE
            // — a focused look at a named thing against a survey of the
            // surroundings — the same shape `MoveTo`/`MoveWithin` fold onto
            // `move` by SCALE.
            Action::ObjectiveMap => "chart",
            Action::ObjectiveExamine => "look",
            Action::ObjectiveNeeds => "sense",
            Action::ObjectiveWait => "wait",
            // The two shipped in Task 7's fix round, read from the SAME
            // cohort-11 comment: `look` -> `look` (the comment spells this
            // pair out on its own line, beside `examine` -> `look`), and
            // `knows` -> `know`. Both concepts already exist, so neither
            // variant mints one and neither reaches the orphan-acts audit.
            Action::ObjectiveLook => "look",
            Action::ObjectiveKnows => "know",
        }
    }
}

/// Whether an act is subject to the body's state or bypasses it. A property
/// of the ACTION, not of the invocation: `examine` and `!examine` are
/// different acts, so a verb never carries both moods (spec §2.1).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mood {
    /// Subject to the body's state; charges time; may post facts.
    InCharacter,
    /// Bypasses the body's state, never the world's rules. May still commit,
    /// stamped with operator provenance (spec §2.2).
    OutOfCharacter,
}

impl Action {
    /// This act's mood. Exhaustive by variant with NO wildcard arm, so a new
    /// action fails to compile until it is classified — the same discipline
    /// as `action_variants_must_all_be_rostered`.
    pub fn mood(&self) -> Mood {
        match self {
            Action::MoveTo(_) | Action::MoveWithin(_) => Mood::InCharacter,
            Action::Drink | Action::Rest | Action::Sleep | Action::Eat => Mood::InCharacter,
            // Group A: operator instruments, out-of-character only (The
            // Deed, spec §3.2) — no in-character counterpart is meaningful.
            Action::Why
            | Action::Npcs
            | Action::Help
            | Action::Eyes
            | Action::Whoami
            | Action::Provoke
            | Action::Soothe => Mood::OutOfCharacter,
            // Group B's objective halves (The Deed, spec §3.2): the `!`
            // spelling IS the mood, and mood is a property of the ACTION
            // rather than of the invocation (spec §2.1) — `examine` and
            // `!examine` are different acts, which is why no variant here
            // ever answers both.
            Action::ObjectiveMap
            | Action::ObjectiveExamine
            | Action::ObjectiveNeeds
            | Action::ObjectiveWait
            | Action::ObjectiveLook
            | Action::ObjectiveKnows => Mood::OutOfCharacter,
        }
    }
}

/// Compile-time tripwire: a new [`Action`] variant breaks this match — every
/// variant is named and there is no `_` arm — forcing [`Action::all`] and
/// [`Action::concept_name`] to be revisited. The `manifest.rs` destructure
/// tripwire applied to an enum. Never remove, never add a wildcard arm.
#[allow(dead_code)]
fn action_variants_must_all_be_rostered(a: &Action) -> &'static str {
    match a {
        Action::MoveTo(_) => "move",
        Action::MoveWithin(_) => "move",
        Action::Drink => "drink",
        Action::Rest => "rest",
        Action::Sleep => "sleep",
        Action::Eat => "eat",
        Action::Why => "recount",
        Action::Npcs => "survey",
        Action::Help => "help",
        Action::Eyes => "lens",
        Action::Whoami => "identify",
        Action::Provoke => "provoke",
        Action::Soothe => "soothe",
        Action::ObjectiveMap => "chart",
        Action::ObjectiveExamine => "look",
        Action::ObjectiveNeeds => "sense",
        Action::ObjectiveWait => "wait",
        Action::ObjectiveLook => "look",
        Action::ObjectiveKnows => "know",
    }
}

/// The GOAP planning state A* searches: where the agent is and whether it has
/// drunk. `Ord` for the deterministic search.
/// type-audit: bare-ok(flag: hydrated)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PlanState {
    /// The agent's room.
    pub position: Facet,
    /// Whether the sustenance goal is met (has drunk this plan).
    pub hydrated: bool,
}

/// How many of a room's [`Facet::neighbors`] are EDGE-adjacent: the pinned
/// prefix `[..4]`. See [`crate::clock::step_factor`]'s own copy of this
/// reliance — the kernel's [`Facet::neighbor_steps`] states the invariant and
/// `the_first_four_neighbours_are_always_the_four_edge_neighbours` (in
/// `kernel/tests/suite/cube_adjacency.rs`) pins it, including that the step a
/// cube corner drops always lands in the diagonal tail, so the prefix survives
/// the short arity.
/// type-audit: bare-ok(count)
const EDGE_ADJACENT_NEIGHBOURS: usize = 4;

/// The planner's edge cost for one ORTHOGONAL (edge-adjacent) step: the unit
/// every other planner cost is expressed in.
///
/// # Why the baseline is no longer `1`
///
/// It was `1`, uniformly, for all eight neighbours — and once The Pavement made
/// the mesh 8-connected that was the clock's own 41% exploit in the PLANNER: a
/// diagonal covers `√2 ≈ 1.414` times the ground, so a flat edge cost made a
/// diagonal-heavy route strictly cheaper per unit distance and A* preferred it.
/// A physics falsehood, not a preference. (Measured on the lattice actually
/// walked, not assumed: the mean diagonal/edge centroid separation at walk
/// depth is 1.411786, so the exploit was 41.18% against the ideal 41.42% — see
/// [`crate::clock::DIAGONAL_STEP_FACTOR`].)
///
/// A* needs integer costs (`kernel/src/astar.rs`, for an exact total order), so
/// a literal `√2` is unavailable and the ratio has to be a rational. `12`/`17`
/// is the pair, chosen and not tuned:
///
/// - `17/12 = 1.416667` is the **third continued-fraction convergent of `√2`**
///   (the sequence is `3/2`, `7/5`, `17/12`, `41/29`), so it is the best
///   rational approximation at its denominator, not an authored number.
/// - It is the **smallest** convergent whose error — `+0.173%` against `√2`,
///   `+0.346%` against the lattice's own measured 1.411786 — lands inside the
///   **0.5%** tolerance the campaign preregistered for octile cost (spec §7,
///   H2). The two cheaper convergents miss it: `3/2` (orthogonal 2, diagonal 3)
///   is `+6.07%`, and `7/5` (5, 7) is `-1.01%`.
/// - Scaling further buys nothing measurable. The planner's job is to not
///   prefer a route the clock will charge more for, and the clock's own ratio
///   is `√2`; a residual six times under the campaign's stated tolerance is
///   already below the projection distortion the lattice itself carries
///   (per-room ratios span 1.366086–1.434180).
///
/// The scale is uniform, so it changes no exploration ORDER by itself: A* runs
/// Dijkstra-mode here and multiplying every cost by 12 leaves the frontier's
/// order untouched. What changes behaviour is the diagonal's `17`, which is the
/// intended fix.
/// type-audit: bare-ok(count)
pub(crate) const ORTHOGONAL_STEP: u64 = 12;

/// The planner's edge cost for one DIAGONAL (corner-adjacent) step —
/// [`ORTHOGONAL_STEP`] times `17/12`, the octile ratio derived there.
/// type-audit: bare-ok(count)
pub(crate) const DIAGONAL_STEP: u64 = 17;

/// The extra `MoveTo` cost the planners charge for stepping INTO a
/// remembered-dangerous room (The Haunt): a finite detour budget over the
/// baseline edge cost of one [`ORTHOGONAL_STEP`], so the A* routes AROUND
/// remembered-bad ground
/// whenever a detour is cheaper than the penalty, yet still braves it when the
/// detour would exceed the penalty (survival-override for free — the finite cost
/// IS the override, never a wall). Deliberately SMALL (decision-ledger #4): the
/// planners run Dijkstra-mode (`heuristic() == 0`, budget `PLAN_BUDGET` node
/// expansions), so a LARGE penalty makes A* exhaust its budget exploring the
/// cost-radius around a chokepoint remembered room and return `None` — the
/// creature freezes instead of detouring (the over-avoidance failure; `20` froze
/// ~900 seed-42 fauna). `5` orthogonal steps keeps the cost-radius within budget
/// so avoidance is
/// graceful (the seed-42 possession `stirred` count barely moves — a handful of
/// beasts detour, none freeze). Decoupling magnitude from budget via an
/// admissible geometric heuristic (for STRONG avoidance) is reserved.
///
/// # RE-EXPRESSED, NOT RE-TUNED — AND THE BALL IT BUYS IS 40% SMALLER
///
/// This was the literal `5` against a baseline edge cost of `1`. The octile
/// rescale above moved the unit under it, so the literal is now written as the
/// quantity it always meant — **five orthogonal steps' worth of detour
/// budget**. Had it stayed the bare `5` it would silently have become a detour
/// budget of five TWELFTHS of a step, an avoidance penalty a single sidestep
/// beats, which is the "silently changes weight relative to distance" failure
/// the rescale had to answer for.
///
/// **What is preserved is the ratio, NOT the set of rooms, and an earlier
/// draft of this doc claimed otherwise.** It argued that "a uniform rescale of
/// every cost leaves the cost-radius untouched". The rescale is *not* uniform:
/// orthogonals went ×12 and diagonals ×14.1667, which is the whole point of the
/// change. The metric moved from **Chebyshev** (every one of eight neighbours
/// at cost 1, so graph distance is `max(|dx|, |dy|)`) to **octile**, so the
/// constant is now calibrated against a different SET:
///
/// | | reach along an axis | reach on the pure diagonal | rooms inside |
/// |---|---|---|---|
/// | before: Chebyshev radius 5 | 5 | 5 | **121** |
/// | after: octile cost ≤ 60 | 5 | 3 | **73** |
///
/// Forty percent fewer rooms. The quotient `60 / 12 = 5 / 1` is exact and the
/// axis reach is unchanged at five steps — that half really is preserved — but
/// the corners of the old square are gone, because reaching them was never
/// worth five steps of real ground and the old metric only said it was.
///
/// **The direction is safe, and arguably better, which is why it ships.** The
/// failure this constant's doc names is OVER-avoidance: a large penalty makes
/// A\* exhaust `PLAN_BUDGET` exploring the cost-radius and freeze the creature.
/// A smaller ball is strictly less budget pressure, never more. And "a fixed
/// amount of distance" is the more principled reading of what a detour budget
/// should buy than "a fixed hop count" — under Chebyshev, five diagonal hops
/// bought `5√2` of ground for the same price as five orthogonal ones, which is
/// the same falsehood the edge costs above exist to remove.
///
/// `the_remembered_penalty_still_buys_five_steps_along_an_axis` pins the
/// quotient (which was never the half in doubt) AND both ball sizes, so this
/// claim is enforceable rather than merely written down.
/// type-audit: bare-ok(count)
const REMEMBERED_PENALTY: u64 = 5 * ORTHOGONAL_STEP;

/// The `MoveTo` edge cost of the step from `from`'s neighbour at index
/// `neighbour_index` into `n`, given the remembered-danger set: the octile
/// baseline ([`ORTHOGONAL_STEP`] for `neighbour_index < 4`,
/// [`DIAGONAL_STEP`] beyond it), plus [`REMEMBERED_PENALTY`] when `n` is
/// remembered-dangerous. For an EMPTY `avoid` set every edge is its bare
/// geometric cost — the byte-identity property both planners share.
///
/// **`neighbour_index` is a position in the caller's [`Facet::neighbors`]
/// list, so the caller must pass that list UNFILTERED.** The pinned order is
/// what carries the geometry (see [`EDGE_ADJACENT_NEIGHBOURS`]); a caller that
/// filtered or reordered first would hand this function indices that mean
/// nothing and get an orthogonal price for a diagonal step, silently. Both
/// callers here pass `neighbors()`/`neighbors_memo()` straight through.
fn move_cost(neighbour_index: usize, n: &Facet, avoid: &std::collections::BTreeSet<Facet>) -> u64 {
    let base = if neighbour_index < EDGE_ADJACENT_NEIGHBOURS {
        ORTHOGONAL_STEP
    } else {
        DIAGONAL_STEP
    };
    if avoid.contains(n) {
        base + REMEMBERED_PENALTY
    } else {
        base
    }
}

/// The GOAP search space for the sustenance goal: reach water and drink.
pub struct GoapSpace<'a> {
    /// The water room the `Drink` action requires.
    pub water: Facet,
    /// The remembered-dangerous rooms to route around (The Haunt) — a `MoveTo`
    /// into one costs its octile baseline ([`ORTHOGONAL_STEP`] or
    /// [`DIAGONAL_STEP`]) plus [`REMEMBERED_PENALTY`]. Empty ⇒ byte-identical.
    pub avoid: &'a std::collections::BTreeSet<Facet>,
}
impl<'a> SearchSpace for GoapSpace<'a> {
    type State = PlanState;
    type Action = Action;
    fn successors(&self, s: &PlanState) -> Vec<(Action, PlanState, u64)> {
        if s.hydrated {
            return Vec::new(); // goal reached; no need to expand
        }
        let mut out: Vec<(Action, PlanState, u64)> = s
            .position
            .neighbors()
            .into_iter()
            .enumerate()
            .map(|(i, n)| {
                let cost = move_cost(i, &n, self.avoid);
                (
                    Action::MoveTo(n.clone()),
                    PlanState {
                        position: n,
                        hydrated: false,
                    },
                    cost,
                )
            })
            .collect();
        if s.position == self.water {
            out.push((
                Action::Drink,
                PlanState {
                    position: s.position.clone(),
                    hydrated: true,
                },
                // DELIBERATELY LEFT AT `1` BY THE OCTILE RESCALE, and said out
                // loud because it is a relative weight that changed. It used
                // to equal one step exactly (both were `1`); it is now a
                // twelfth of one. Three reasons that is the better answer
                // rather than an oversight:
                //
                // 1. It is not a DISTANCE. `ORTHOGONAL_STEP`/`DIAGONAL_STEP`
                //    exist to price ground covered; drinking covers none, so
                //    the octile scale has nothing to say about it.
                // 2. A twelfth of a move is far closer to what the CLOCK
                //    charges than parity ever was — `clock::base_cost` prices
                //    `Drink` at 150 ticks against `MoveTo`'s 10,000, i.e. 1.5%
                //    of a move, so 8.3% is a large improvement on 100%.
                // 3. It cannot change which route is chosen: this is the only
                //    goal-reaching edge and it is appended at one fixed state,
                //    so its cost adds a constant to the goal's `g`. Being
                //    SMALLER than before relative to a step, it makes the goal
                //    pop EARLIER, i.e. strictly fewer expansions against
                //    `PLAN_BUDGET`, never more.
                1,
            ));
        }
        out
    }
    fn goal(&self, s: &PlanState) -> bool {
        s.hydrated
    }
    fn heuristic(&self, _s: &PlanState) -> u64 {
        0 // Dijkstra-mode; a geometric heuristic is a followup
    }
}

/// Plan the `[move*, drink]` journey to satisfy the sustenance goal, or `None`
/// if water is unreachable within `budget`. `avoid` is the remembered-danger set
/// the A* routes around (The Haunt); pass an empty set for the memory-less path.
/// type-audit: bare-ok(count: budget)
pub fn plan_to_water(
    from: &Facet,
    water: &Facet,
    budget: usize,
    avoid: &std::collections::BTreeSet<Facet>,
) -> Option<Vec<Action>> {
    astar(
        &GoapSpace {
            water: water.clone(),
            avoid,
        },
        PlanState {
            position: from.clone(),
            hydrated: false,
        },
        budget,
    )
}

/// A navigation-only space (the home-return goal — no Drink): goal is arrival.
struct NavSpace<'a> {
    dest: Facet,
    /// The remembered-dangerous rooms to route around (The Haunt) — a `MoveTo`
    /// into one costs its octile baseline ([`ORTHOGONAL_STEP`] or
    /// [`DIAGONAL_STEP`]) plus [`REMEMBERED_PENALTY`]. Empty ⇒ byte-identical.
    avoid: &'a std::collections::BTreeSet<Facet>,
}
impl<'a> NavSpace<'a> {
    /// The `move_cost`/`avoid` edge-building rule, shared verbatim by
    /// [`SearchSpace::successors`] and [`SearchSpace::successors_memo`]
    /// below (the-waymark, Task 6) so memoizing the raw neighbor lookup can
    /// never accidentally also change how an edge's cost is computed — the
    /// memo boundary is `neighbors`/`neighbors_memo` ALONE, never the
    /// successor list this builds from it.
    fn edges_from(&self, neighbors: Vec<Facet>) -> Vec<(Action, Facet, u64)> {
        neighbors
            .into_iter()
            .enumerate()
            .map(|(i, n)| {
                let cost = move_cost(i, &n, self.avoid);
                (Action::MoveTo(n.clone()), n, cost)
            })
            .collect()
    }
}
impl<'a> SearchSpace for NavSpace<'a> {
    type State = Facet;
    type Action = Action;
    fn successors(&self, s: &Facet) -> Vec<(Action, Facet, u64)> {
        self.edges_from(s.neighbors())
    }
    /// Ledger #7's re-plan (the-waymark, Task 6): consults a caller-owned
    /// [`RoomMeshMemo`] for the neighbor lookup instead of recomputing the
    /// cube-lattice/seam arithmetic on every `astar` expansion — this is the
    /// specific hot path (`Facet::neighbors` inside `NavSpace::successors`
    /// → `astar` expansions) Task 3's memo was built for but could not reach,
    /// because `SearchSpace::successors(&self, ...)` alone had no way to
    /// thread a caller's memo down into it. Byte-identical to `successors`
    /// either way ([`Facet::neighbors_memo`] is a cache of the same pure
    /// function `neighbors` computes), and the `edges_from` cost rule is
    /// untouched — only which of `neighbors`/`neighbors_memo` supplies the
    /// rooms it costs. HOW MANY it costs is no longer fixed: The Pavement made
    /// the walk 8-connected, so it is eight rooms in the interior and seven at
    /// one of the cube's eight corners (see [`Facet::neighbors`]).
    ///
    /// **The memo now has to preserve ORDER as well as content**, and that is
    /// a strictly stronger demand than it carried before Task 7. `edges_from`
    /// reads a neighbour's POSITION to price it (the pinned edge prefix — see
    /// [`move_cost`]), so a memo that returned the same eight rooms in a
    /// different order would charge an orthogonal price for a diagonal step
    /// while every membership-based assertion stayed green.
    /// [`Facet::neighbors_memo`] caches the whole `Vec` as `neighbors`
    /// produced it and `neighbors_memo_bit_equals_recomputation` pins that,
    /// so the demand is met; it is stated here because nothing about the old
    /// cost rule required it.
    fn successors_memo(
        &self,
        s: &Facet,
        memo: Option<&mut RoomMeshMemo>,
    ) -> Vec<(Action, Facet, u64)> {
        let neighbors = match memo {
            Some(m) => s.neighbors_memo(m),
            None => s.neighbors(),
        };
        self.edges_from(neighbors)
    }
    fn goal(&self, s: &Facet) -> bool {
        *s == self.dest
    }
    fn heuristic(&self, _s: &Facet) -> u64 {
        0
    }
}

/// [`plan_to_room`], threading a caller-owned [`RoomMeshMemo`] through the
/// underlying [`AStarSolver`] search instead of recomputing `Facet::
/// neighbors` on every expansion (the-waymark, Task 6 — ledger #7's
/// re-plan). `mesh_memo: None` is exactly `plan_to_room`'s own behavior
/// (`NavSpace::successors_memo`'s default-free override still falls back to
/// plain `neighbors`); `Some(memo)` is byte-identical too, by construction —
/// see `NavSpace::successors_memo`'s own doc. `pub(crate)`: today's one
/// caller worth the memo is `HomeNavCache::home_nav` in [`crate::liveness`];
/// a future external caller can widen this if it ever needs to.
pub(crate) fn plan_to_room_memo(
    from: &Facet,
    dest: &Facet,
    budget: usize,
    avoid: &std::collections::BTreeSet<Facet>,
    mesh_memo: Option<&mut RoomMeshMemo>,
) -> Option<Vec<Action>> {
    AStarSolver.solve(
        &NavSpace {
            dest: dest.clone(),
            avoid,
        },
        from.clone(),
        budget,
        mesh_memo,
    )
}

/// Plan a pure navigation path to `dest` (the home-return goal), or `None`.
/// `avoid` is the remembered-danger set the A* routes around (The Haunt); pass
/// an empty set for the memory-less path. A thin delegator to
/// [`plan_to_room_memo`] with `mesh_memo: None` (the-waymark, Task 6) — every
/// existing caller (there is no session memo in scope at most of them) is
/// unchanged.
/// type-audit: bare-ok(count: budget)
pub fn plan_to_room(
    from: &Facet,
    dest: &Facet,
    budget: usize,
    avoid: &std::collections::BTreeSet<Facet>,
) -> Option<Vec<Action>> {
    plan_to_room_memo(from, dest, budget, avoid, None)
}
#[cfg(test)]
mod tests {
    use super::*;

    /// **The planner and the clock must not disagree about what a diagonal
    /// costs**, or A* will choose routes the clock then charges more for.
    ///
    /// A* takes integer costs, so the planner approximates the clock's exact
    /// `√2` with `DIAGONAL_STEP / ORTHOGONAL_STEP`. This holds the pair to the
    /// 0.5% the campaign preregistered for octile cost (spec §7, H2) — the
    /// same bar `ORTHOGONAL_STEP`'s doc uses to pick `12/17` over the two
    /// cheaper convergents of `√2`.
    #[test]
    fn the_planner_and_the_clock_price_a_diagonal_alike() {
        let planner = DIAGONAL_STEP as f64 / ORTHOGONAL_STEP as f64;
        let clock = crate::clock::DIAGONAL_STEP_FACTOR;
        let rel = (planner - clock).abs() / clock;
        assert!(
            rel < 0.005,
            "the planner prices a diagonal at {planner} and the clock at {clock} \
             — {:.4}% apart, past the preregistered 0.5%",
            rel * 100.0
        );
        // And the pair is the convergent it claims to be, not a number that
        // merely lands inside the band: 17/12 is the third continued-fraction
        // convergent of sqrt(2), so no smaller denominator does better.
        assert_eq!((ORTHOGONAL_STEP, DIAGONAL_STEP), (12, 17));
        for (o, d) in [(2u64, 3u64), (5, 7)] {
            let cheaper = (d as f64 / o as f64 - clock).abs() / clock;
            assert!(
                cheaper > rel,
                "{d}/{o} is not worse than 17/12; the choice's premise has moved"
            );
        }
    }

    /// **`REMEMBERED_PENALTY` was RE-EXPRESSED, and this pins BOTH halves of
    /// what that did** — the one that is preserved and the one that is not.
    ///
    /// PRESERVED: the quotient. Five orthogonal steps of detour budget,
    /// `60 / 12 = 5 / 1`, so the reach along an axis is unchanged.
    ///
    /// NOT PRESERVED: the SET. The metric moved from Chebyshev to octile, so
    /// the ball the constant is calibrated against shrinks from 121 rooms to
    /// 73. That is a real change of effect and the constant's doc argues why
    /// its direction is safe (less `PLAN_BUDGET` pressure, never more); this
    /// test exists so the claim cannot rot into an assertion nobody checks.
    #[test]
    fn the_remembered_penalty_still_buys_five_steps_along_an_axis() {
        assert_eq!(REMEMBERED_PENALTY, 5 * ORTHOGONAL_STEP);
        assert_eq!(REMEMBERED_PENALTY / ORTHOGONAL_STEP, 5, "five steps");
        assert_eq!(REMEMBERED_PENALTY % ORTHOGONAL_STEP, 0, "an exact radius");
        // Had it stayed the literal `5` the radius would have collapsed to
        // five twelfths of a step — an avoidance penalty one sidestep beats.
        // Stated as the arithmetic rather than `assert!(5 < ORTHOGONAL_STEP)`,
        // which clippy reads (correctly) as an assertion on a constant.
        assert_eq!(5 / ORTHOGONAL_STEP, 0, "the bare literal would be sub-step");

        // The two balls, counted rather than asserted from memory. `dx`/`dy`
        // range well past either reach so neither count can be clipped by the
        // window it is counted over.
        let span = -8i64..=8;
        let chebyshev = span
            .clone()
            .flat_map(|dx| span.clone().map(move |dy| (dx, dy)))
            .filter(|(dx, dy)| dx.abs().max(dy.abs()) <= 5)
            .count();
        let octile = span
            .clone()
            .flat_map(|dx| span.clone().map(move |dy| (dx, dy)))
            .filter(|(dx, dy)| {
                let (hi, lo) = (dx.abs().max(dy.abs()), dx.abs().min(dy.abs()));
                // The octile cost of reaching (dx, dy): `lo` diagonal steps
                // then `hi - lo` orthogonal ones.
                (DIAGONAL_STEP as i64) * lo + (ORTHOGONAL_STEP as i64) * (hi - lo)
                    <= REMEMBERED_PENALTY as i64
            })
            .count();
        assert_eq!(chebyshev, 121, "the pre-Task-7 detour ball");
        assert_eq!(octile, 73, "the octile detour ball");
        assert!(
            octile < chebyshev,
            "the octile ball must be the SMALLER one — a larger one would put \
             MORE pressure on PLAN_BUDGET, which is the over-avoidance freeze \
             this constant exists to avoid"
        );
    }

    /// **`NavSpace` must hand `move_cost` the neighbour list UNFILTERED and
    /// UNREORDERED**, because the cost is now keyed on a neighbour's INDEX.
    ///
    /// This is the composed claim nothing else holds. `move_cost` is pinned by
    /// index, and `Facet::neighbors`' order is pinned in the kernel — but a
    /// `successors` that filtered impassable rooms out before `.enumerate()`,
    /// or sorted them, would slide every diagonal into the edge prefix and
    /// charge it as an orthogonal. Every membership-based assertion in the
    /// crate would stay green, and the planner would quietly resume preferring
    /// diagonals.
    ///
    /// Both paths are checked: `successors` and `successors_memo`, the latter
    /// with a live [`RoomMeshMemo`], since the memo is the one place the list
    /// could be rebuilt differently.
    #[test]
    fn nav_space_costs_the_whole_unfiltered_neighbour_list_in_order() {
        // An interior room: path digits give lattice (13, 21) at scale 64, so
        // all eight neighbours exist and none of the arity cases are in play.
        let room = Facet {
            face: 0,
            path: vec![0, 1, 2, 3, 0, 1],
        };
        let ns = room.neighbors();
        assert_eq!(ns.len(), 8, "precondition: an interior room");
        let empty = std::collections::BTreeSet::new();
        let space = NavSpace {
            dest: room.clone(),
            avoid: &empty,
        };
        let mut memo = RoomMeshMemo::new();
        for (label, edges) in [
            ("successors", space.successors(&room)),
            (
                "successors_memo",
                space.successors_memo(&room, Some(&mut memo)),
            ),
        ] {
            assert_eq!(
                edges.len(),
                ns.len(),
                "{label} dropped or added a neighbour: the list must reach \
                 move_cost unfiltered, or an index means nothing"
            );
            for (i, (action, dest, cost)) in edges.iter().enumerate() {
                assert_eq!(dest, &ns[i], "{label}: edge {i} is not neighbour {i}");
                assert_eq!(action, &Action::MoveTo(ns[i].clone()), "{label}: edge {i}");
                let want = if i < EDGE_ADJACENT_NEIGHBOURS {
                    ORTHOGONAL_STEP
                } else {
                    DIAGONAL_STEP
                };
                assert_eq!(*cost, want, "{label}: edge {i} is priced by position");
            }
        }
    }

    /// The edge cost is octile in both directions, and the remembered penalty
    /// rides on top of whichever baseline the step's geometry names.
    #[test]
    fn move_cost_is_octile_and_the_penalty_rides_on_top() {
        let room = Facet {
            face: 0,
            path: vec![0, 1, 2],
        };
        let empty = std::collections::BTreeSet::new();
        let mut avoid = std::collections::BTreeSet::new();
        avoid.insert(room.clone());
        for i in 0..EDGE_ADJACENT_NEIGHBOURS {
            assert_eq!(move_cost(i, &room, &empty), ORTHOGONAL_STEP);
            assert_eq!(
                move_cost(i, &room, &avoid),
                ORTHOGONAL_STEP + REMEMBERED_PENALTY
            );
        }
        for i in EDGE_ADJACENT_NEIGHBOURS..8 {
            assert_eq!(move_cost(i, &room, &empty), DIAGONAL_STEP);
            assert_eq!(
                move_cost(i, &room, &avoid),
                DIAGONAL_STEP + REMEMBERED_PENALTY
            );
        }
    }

    /// The prefix this module indexes against is the kernel's own pinned one —
    /// asserted here, so a kernel reordering fails in `windows/vessel` too
    /// rather than only in `kernel/tests`.
    #[test]
    fn the_edge_prefix_is_the_kernels_pinned_prefix() {
        assert_eq!(EDGE_ADJACENT_NEIGHBOURS, 4);
        let steps = Facet::neighbor_steps();
        for (dx, dy) in &steps[..EDGE_ADJACENT_NEIGHBOURS] {
            assert!(
                (dx.abs() + dy.abs()) == 1,
                "({dx}, {dy}) is not an edge step but sits in the edge prefix"
            );
        }
        for (dx, dy) in &steps[EDGE_ADJACENT_NEIGHBOURS..] {
            assert!(
                dx.abs() == 1 && dy.abs() == 1,
                "({dx}, {dy}) is not a diagonal step but sits in the diagonal tail"
            );
        }
    }
}
