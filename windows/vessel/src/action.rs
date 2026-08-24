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
    /// Rest / sleep (precondition: at home; effect: fatigue reset) — The
    /// Slumber's discharge action, the fatigue analogue of `Drink`.
    Rest,
    /// Eat / graze (precondition: standing on a cell rich enough to feed;
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
        Action::Drink | Action::Rest | Action::Eat => false,
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
/// `Drink`/`Rest`/`Eat` each commit a fact, so only fine movement qualifies.
/// The partition is "does it commit", not "is it movement".
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
            Action::Drink | Action::Rest | Action::Eat => Mood::InCharacter,
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

/// The extra `MoveTo` cost the planners charge for stepping INTO a
/// remembered-dangerous cell (The Haunt): a finite detour budget over the
/// baseline edge cost of `1`, so the A* routes AROUND remembered-bad ground
/// whenever a detour is cheaper than the penalty, yet still braves it when the
/// detour would exceed the penalty (survival-override for free — the finite cost
/// IS the override, never a wall). Deliberately SMALL (decision-ledger #4): the
/// planners run Dijkstra-mode (`heuristic() == 0`, budget `PLAN_BUDGET` node
/// expansions), so a LARGE penalty makes A* exhaust its budget exploring the
/// cost-radius around a chokepoint remembered cell and return `None` — the
/// creature freezes instead of detouring (the over-avoidance failure; `20` froze
/// ~900 seed-42 fauna). `5` keeps the cost-radius within budget so avoidance is
/// graceful (the seed-42 possession `stirred` count barely moves — a handful of
/// beasts detour, none freeze). Decoupling magnitude from budget via an
/// admissible geometric heuristic (for STRONG avoidance) is reserved.
/// type-audit: bare-ok(count)
const REMEMBERED_PENALTY: u64 = 5;

/// The `MoveTo` edge cost into `n` given the remembered-danger set: the baseline
/// `1`, plus [`REMEMBERED_PENALTY`] when `n` is remembered-dangerous. For an
/// EMPTY `avoid` set every edge stays `1` — the byte-identity property both
/// planners share.
fn move_cost(n: &Facet, avoid: &std::collections::BTreeSet<Facet>) -> u64 {
    if avoid.contains(n) {
        1 + REMEMBERED_PENALTY
    } else {
        1
    }
}

/// The GOAP search space for the sustenance goal: reach water and drink.
pub struct GoapSpace<'a> {
    /// The water room the `Drink` action requires.
    pub water: Facet,
    /// The remembered-dangerous cells to route around (The Haunt) — a `MoveTo`
    /// into one costs `1 + REMEMBERED_PENALTY`. Empty ⇒ byte-identical.
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
            .map(|n| {
                let cost = move_cost(&n, self.avoid);
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
    /// The remembered-dangerous cells to route around (The Haunt) — a `MoveTo`
    /// into one costs `1 + REMEMBERED_PENALTY`. Empty ⇒ byte-identical.
    avoid: &'a std::collections::BTreeSet<Facet>,
}
impl<'a> NavSpace<'a> {
    /// The `move_cost`/`avoid` edge-building rule, shared verbatim by
    /// [`SearchSpace::successors`] and [`SearchSpace::successors_memo`]
    /// below (the-waymark, Task 6) so memoizing the raw neighbor lookup can
    /// never accidentally also change how an edge's cost is computed — the
    /// memo boundary is `neighbors`/`neighbors_memo` ALONE, never the
    /// successor list this builds from it.
    fn edges_from(&self, neighbors: [Facet; 3]) -> Vec<(Action, Facet, u64)> {
        neighbors
            .into_iter()
            .map(|n| {
                let cost = move_cost(&n, self.avoid);
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
    /// icosphere lattice arithmetic on every `astar` expansion — this is the
    /// specific hot path (`Facet::neighbors` inside `NavSpace::successors`
    /// → `astar` expansions) Task 3's memo was built for but could not reach,
    /// because `SearchSpace::successors(&self, ...)` alone had no way to
    /// thread a caller's memo down into it. Byte-identical to `successors`
    /// either way ([`Facet::neighbors_memo`] is a cache of the same pure
    /// function `neighbors` computes), and the `edges_from` cost rule is
    /// untouched — only which of `neighbors`/`neighbors_memo` supplies the
    /// three rooms it costs.
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
