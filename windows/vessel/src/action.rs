//! The action layer: what a body can do, and the search spaces that plan
//! sequences of it.
//!
//! Extracted from `liveness.rs` by The Tackle (Arc I.a of The Bridle) with
//! no behavioural change, so that The Deed can unify the player's verbs
//! with these without doing surgery inside a 14,700-line file.

use crate::interior::AnchorId;
use hornvale_kernel::{AStarSolver, RoomAddr, RoomMeshMemo, SearchSpace, Solver, astar};

/// A GOAP action — a precondition/effect transformation over the plan state.
/// Minimal + heterogeneous (the precondition chain needs two kinds); the MAP-27
/// authored-verb DSL is a followup.
/// type-audit: bare-ok(return)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Action {
    /// Walk to an adjacent room (precondition: adjacency; effect: position).
    MoveTo(RoomAddr),
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
}

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
            Action::MoveTo(RoomAddr {
                face: 0,
                path: Vec::new(),
            }),
            Action::Drink,
            Action::Rest,
            Action::Eat,
            Action::MoveWithin(crate::interior::AnchorId(0)),
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
    }
}

/// The GOAP planning state A* searches: where the agent is and whether it has
/// drunk. `Ord` for the deterministic search.
/// type-audit: bare-ok(flag: hydrated)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PlanState {
    /// The agent's room.
    pub position: RoomAddr,
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
fn move_cost(n: &RoomAddr, avoid: &std::collections::BTreeSet<RoomAddr>) -> u64 {
    if avoid.contains(n) {
        1 + REMEMBERED_PENALTY
    } else {
        1
    }
}

/// The GOAP search space for the sustenance goal: reach water and drink.
/// type-audit: bare-ok(return)
pub struct GoapSpace<'a> {
    /// The water room the `Drink` action requires.
    pub water: RoomAddr,
    /// The remembered-dangerous cells to route around (The Haunt) — a `MoveTo`
    /// into one costs `1 + REMEMBERED_PENALTY`. Empty ⇒ byte-identical.
    pub avoid: &'a std::collections::BTreeSet<RoomAddr>,
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
    from: &RoomAddr,
    water: &RoomAddr,
    budget: usize,
    avoid: &std::collections::BTreeSet<RoomAddr>,
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
    dest: RoomAddr,
    /// The remembered-dangerous cells to route around (The Haunt) — a `MoveTo`
    /// into one costs `1 + REMEMBERED_PENALTY`. Empty ⇒ byte-identical.
    avoid: &'a std::collections::BTreeSet<RoomAddr>,
}
impl<'a> NavSpace<'a> {
    /// The `move_cost`/`avoid` edge-building rule, shared verbatim by
    /// [`SearchSpace::successors`] and [`SearchSpace::successors_memo`]
    /// below (the-waymark, Task 6) so memoizing the raw neighbor lookup can
    /// never accidentally also change how an edge's cost is computed — the
    /// memo boundary is `neighbors`/`neighbors_memo` ALONE, never the
    /// successor list this builds from it.
    fn edges_from(&self, neighbors: [RoomAddr; 3]) -> Vec<(Action, RoomAddr, u64)> {
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
    type State = RoomAddr;
    type Action = Action;
    fn successors(&self, s: &RoomAddr) -> Vec<(Action, RoomAddr, u64)> {
        self.edges_from(s.neighbors())
    }
    /// Ledger #7's re-plan (the-waymark, Task 6): consults a caller-owned
    /// [`RoomMeshMemo`] for the neighbor lookup instead of recomputing the
    /// icosphere lattice arithmetic on every `astar` expansion — this is the
    /// specific hot path (`RoomAddr::neighbors` inside `NavSpace::successors`
    /// → `astar` expansions) Task 3's memo was built for but could not reach,
    /// because `SearchSpace::successors(&self, ...)` alone had no way to
    /// thread a caller's memo down into it. Byte-identical to `successors`
    /// either way ([`RoomAddr::neighbors_memo`] is a cache of the same pure
    /// function `neighbors` computes), and the `edges_from` cost rule is
    /// untouched — only which of `neighbors`/`neighbors_memo` supplies the
    /// three rooms it costs.
    fn successors_memo(
        &self,
        s: &RoomAddr,
        memo: Option<&mut RoomMeshMemo>,
    ) -> Vec<(Action, RoomAddr, u64)> {
        let neighbors = match memo {
            Some(m) => s.neighbors_memo(m),
            None => s.neighbors(),
        };
        self.edges_from(neighbors)
    }
    fn goal(&self, s: &RoomAddr) -> bool {
        *s == self.dest
    }
    fn heuristic(&self, _s: &RoomAddr) -> u64 {
        0
    }
}

/// [`plan_to_room`], threading a caller-owned [`RoomMeshMemo`] through the
/// underlying [`AStarSolver`] search instead of recomputing `RoomAddr::
/// neighbors` on every expansion (the-waymark, Task 6 — ledger #7's
/// re-plan). `mesh_memo: None` is exactly `plan_to_room`'s own behavior
/// (`NavSpace::successors_memo`'s default-free override still falls back to
/// plain `neighbors`); `Some(memo)` is byte-identical too, by construction —
/// see `NavSpace::successors_memo`'s own doc. `pub(crate)`: today's one
/// caller worth the memo is `HomeNavCache::home_nav` in [`crate::liveness`];
/// a future external caller can widen this if it ever needs to.
pub(crate) fn plan_to_room_memo(
    from: &RoomAddr,
    dest: &RoomAddr,
    budget: usize,
    avoid: &std::collections::BTreeSet<RoomAddr>,
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
    from: &RoomAddr,
    dest: &RoomAddr,
    budget: usize,
    avoid: &std::collections::BTreeSet<RoomAddr>,
) -> Option<Vec<Action>> {
    plan_to_room_memo(from, dest, budget, avoid, None)
}
