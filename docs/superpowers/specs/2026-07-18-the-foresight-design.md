# The Foresight (The Walk · Milestone 2 · the GOAP goal rung) — Design

> **STATUS: SHIPPED** — *The Foresight*, all four tasks landed on
> `worktree-the-foresight` 2026-07-18 (byte-identical; census-free; the
> planner runs only inside a possess session). PSY-6's goal rung: a
> general, tense-agnostic deterministic A* in the kernel (`SearchSpace` +
> `astar`), GOAP as its first instantiation over a minimal two-action space
> (`MoveTo`/`Drink`) whose precondition chain (`Drink` gated by at-water)
> makes the planner discover `[move*, drink]`, not just a path; `decide`'s
> body swaps in as The Wanting's reserved seam anticipated. Arbitration, the
> psychology-vector cost, and planning-over-belief stay reserved as the next
> three body-swaps (§9). Chronicle: `book/src/chronicle/the-foresight.md`.
> Retrospective: `docs/retrospectives/the-foresight.md`.

Program: **The Walk** (metaplan `docs/superpowers/specs/2026-07-11-the-walk-metaplan-design.md`,
Milestone 2 "the living world," Campaign V — the THIRD liveness slice: PSY-6's
**goal rung**, the A* planner). Base: main `@36cd84c`. Builds on **The Wanting**
(`windows/vessel/src/liveness.rs` — the drive rung + the reserved `decide` seam).
Registry: **UNI-19** (the planning engine, A* across the tenses) + **PSY-6**.
Chronicle-name candidate: **The Foresight** — the agent looks ahead: A* searches
forward to the goal; confirmed at close.

## 1. What this is

The Wanting gave NPCs a *want* — a drive that makes them *react* (thirsty → go to
the water one step away). But a reactive controller can only close a gap that is
*directly reachable*. This campaign gives the agent **foresight**: it can form a
**plan** — a sequence of actions whose preconditions interlock — to reach a goal
that is *not* directly reachable.

It lands PSY-6's **goal rung**: **GOAP** (Goal-Oriented Action Planning), which
is **A***. The agent's drive generates a *goal* (be hydrated); the planner
searches the space of actions for the least-cost sequence that achieves it. The
essence — what makes this GOAP and not mere pathfinding — is a **precondition
chain**: the only action that hydrates you, `Drink`, requires you to be *at* the
water, so the planner must *establish that precondition* with a run of `MoveTo`
actions first. The plan is `[move, move, …, drink]`, discovered by A*.

Two things make this more than a vessel feature:

- Per **UNI-19**, A* is *"the rare natively-deterministic game-AI primitive, so
  one small kernel planner serves navigation, history, prophecy, tech,
  reconstruction, each with its own state space + cost."* So the planner is a
  **general, tense-agnostic A* in the kernel**; GOAP is its first instantiation.
- It is the payoff of **The Wanting's reserved seam**: `decide` becomes the
  planner's body without changing the seam the tick depends on — exactly the
  body-swap that campaign was designed to receive.

Deliberately scoped: **one drive/goal** (no arbitration), **uniform cost** (no
psychology-vector yet), **ground-truth** world-state (no belief-reading yet).
Those are the next three body-swaps (§9), each of a seam this campaign leaves
reserved.

## 2. Scope

**In:**

1. **A general deterministic A* — a kernel primitive.** A pure graph search
   parameterized by a `State: Ord`, a `successors(&State) -> Vec<(Action, State,
   cost)>`, a `goal(&State) -> bool`, and an admissible `heuristic(&State) ->
   cost`; returns the least-cost `Vec<Action>` (or `None`). Tense-agnostic — it
   does not know forward vs backward (§4.1). **GOAP is its first and only
   instantiation this campaign.**
2. **The GOAP action model (vessel).** A minimal, heterogeneous action set —
   `MoveTo(room)` (precondition: adjacency; effect: position) and `Drink`
   (precondition: at the water room; effect: hydrated) — each a
   precondition/effect/cost over the planning state (§4.2). The full MAP-27
   verb-chemistry DSL is deferred.
3. **The planning state + goal.** The state A* searches is `(position: RoomAddr,
   hydrated: bool)`, `Ord`; the goal is `hydrated`, generated when the drive
   crosses `act` (§4.3).
4. **The drive refined to a `drank`-event fold.** The drive becomes *time since
   the last drink* — a fold over committed `drank` events (rise always, reset to
   0 on a `drank`), superseding The Wanting's proximity-satiety. This is what
   makes `Drink` a real, planned action rather than automatic proximity (§4.4).
5. **The planner replaces `decide`'s body (the reserved seam pays off).** `Intent`
   generalizes to `Intent::Do(Action)` / `Intent::Hold`; `decide`'s body runs the
   kernel A* and returns the plan's *first action*; the tick executes one action
   per step (a move commits an `agent-at`; a drink commits a `drank`), advancing
   time by the action's duration, re-planning each step (§4.5).
6. **Observation.** `why <npc>` recounts the planned journey and the drink; the
   possess transcript shows an agent walking a multi-room path to water and
   drinking (§4.6).

**Out (deferred; §9 followups — each a reserved seam's next body):**

- **Arbitration** (goal selection among ≥2 competing drives) — one drive here.
- **The psychology-vector cost function** — uniform cost here (`cost` is a
  `successors` parameter, so a body change later).
- **Belief-reading (UNI-16)** — the planner reads `view` (ground-truth); belief
  is a change to what fills `view`.
- **The MAP-27 verb-chemistry action DSL** — two hardcoded actions here.
- **Plan caching** — re-plan each decision here (deterministic, stable).
- **The other A* tenses** (navigation, confabulation, prophecy — UNI-19) — the
  engine ships; the instantiations are future.

## 3. Determinism contract — the save-format line (leads G3)

**Census-free, genesis byte-identical — unchanged from The Wanting. The planner
runs only in the possess session.**

- **The kernel A* is a pure function** (no serialized state) — the kernel gains
  an algorithm, byte-identical.
- **No plan/goal fact and no planned move at genesis.** `build_world` commits
  exactly what it commits today; a genesis world (census, almanacs,
  gallery/laboratory artifacts) is **byte-identical**; The Wanting's
  genesis-zero-`agent-at` pin still holds. **No census regen, no epoch.**
- **A* PATH DETERMINISM (the keystone, §5):** the search uses `BTreeMap`/
  `BTreeSet` (no `HashMap`) and a **total-order** open-set priority (f-cost
  ascending, ties broken by a deterministic `State` order), so the returned path
  is a pure deterministic function of the graph — same successors/goal/heuristic
  → byte-identical path, even with multiple optimal paths. **No RNG** (A* is
  natively deterministic). A saved session re-derives identically.
- **`drank` and any goal/plan predicate are game-layer**, registered by the
  session, never at genesis (as The Wanting did for `agent-at`).
- **Lorenz-safe:** the planning state is discrete; costs are integers/finite; no
  chaotic integrator.
- **Game-layer artifact update (not a determinism concern):** the possession
  transcripts update to show planned journeys + drinking — a legitimate
  game-behavior update; genesis artifacts stay byte-identical (verified at close).

## 4. Architecture

Kernel gains the domain-agnostic A* (the mechanism/population split — the kernel
defines, the game composes). All the game-specific parts (actions, state, the
planner-in-`decide`, the tick) live in `windows/vessel` (The Walk §11, domains
untouched).

### 4.1 The kernel A* (general, tense-agnostic)

```
// kernel/src/astar.rs — a pure deterministic shortest-path search.
pub trait SearchSpace {
    type State: Ord + Clone;
    type Action: Clone;
    fn successors(&self, s: &Self::State) -> Vec<(Self::Action, Self::State, u64)>; // (action, next, cost)
    fn goal(&self, s: &Self::State) -> bool;
    fn heuristic(&self, s: &Self::State) -> u64; // admissible; 0 = Dijkstra
}
pub fn astar<S: SearchSpace>(space: &S, start: S::State) -> Option<Vec<S::Action>>;
```

- Costs are `u64` (integer — no float non-determinism; a cost budget is finite).
- Open set: a `BTreeSet<(f_cost, g_cost, State)>` (or a `BTreeMap` keyed by that
  total order) — **the tie-break is the tuple order** `(f, g, State)`, fully
  deterministic; `State: Ord` supplies the final tie-break. Closed/`g`-so-far:
  `BTreeMap<State, u64>`. **No `HashMap`.**
- Returns the least-cost action sequence, or `None` (unreachable). A **step
  budget** (max expansions) bounds the search (§4.5 passes a budget via the
  space or an `astar_bounded` variant) so an unreachable goal fails fast, not
  forever.
- The engine is **tense-agnostic**: it knows nothing of "time" or "GOAP"; a
  `SearchSpace` supplies the semantics (GOAP now; confabulation/navigation later).

### 4.2 The GOAP action model (vessel)

```
enum Action { MoveTo(RoomAddr), Drink }
```

- `MoveTo(n)` — precondition: `n` is a mesh neighbor of the state's position;
  effect: `position = n`; cost: 1.
- `Drink` — precondition: `position == water_room`; effect: `hydrated = true`;
  cost: 1.

`successors` enumerates the ≤3 `MoveTo(neighbor)` plus `Drink` (if at water). The
minimal heterogeneous set; MAP-27's authored verb-chemistry is the followup.

### 4.3 The planning state + goal

```
struct PlanState { position: RoomAddr, hydrated: bool }  // derive Ord
// goal(s) = s.hydrated ;  heuristic(s) = mesh steps from s.position to water_room, 0 if hydrated
```

The goal (`hydrated`) is generated when the agent's drive crosses `act`. The
heuristic is the room-mesh step distance to the water room (admissible — you
cannot hydrate in fewer moves than the distance), which focuses A* toward the
water. Bounded to a local region (§4.5).

### 4.4 The drive, refined to a `drank`-event fold

The Wanting's drive folded `agent-at` (fell by proximity). The Foresight makes
drinking an **action**, so the drive becomes *time since the last drink*:

```
drive(npc, t) = min(1, rise * (t - last_drank_day(npc)))   // last_drank_day = 0 before any drank
```

a pure fold over committed `drank` events (DRIVE ≡ FOLD preserved, now over
`drank` not `agent-at`). Position no longer directly satiates — the agent must
*plan* to reach water and *drink*. `agent-at` still records position over time
(the moves); `drank` records satiety. Both are game-layer, session-registered.

### 4.5 The planner replaces `decide`'s body; the tick walks the plan

```
enum Intent { Do(Action), Hold }
fn decide(state, water_room, drive, p, budget) -> Intent:
    if drive < p.act -> Hold                 // no goal yet (the drive gate)
    plan = astar(GoapSpace{ water_room, budget }, PlanState{ state.position, hydrated:false })
    match plan.first() { Some(a) -> Do(a.clone()), None -> Hold }   // unreachable -> Hold
```

The tick executes **one action per step**, re-planning each step (deterministic,
so the plan is stable and the agent follows it): a `MoveTo` commits an `agent-at`
(day advanced by a move-duration `τ`); a `Drink` commits a `drank` (resetting the
drive), day unchanged or +ε. The step loop advances event-by-event over the
`wait` interval, with the strict-day-progress guard (The Wanting's fix) ensuring
termination. A* is bounded by a **step budget** (§4.1) so a far/unreachable water
room fails to `Hold`, never hangs.

`Intent` generalizes `{GoTo, Hold}` → `{Do(Action), Hold}` (GoTo becomes
`Do(MoveTo)`); the tick depends only on `Intent` — the reserved seam's body-swap,
exactly as The Wanting #9 designed.

### 4.6 Observation

`why <npc>` recounts the dated `agent-at` moves and the `drank` (its provenance
naming the goal, e.g. `"walked to water and drank (thirst)"`). The possess
transcript shows the agent walking a multi-room path and drinking. `needs` (from
The Wanting) still reads the felt drive.

### 4.7 What stays reserved (the next bodies)

Cost is uniform (the psychology-vector cost is a `successors` cost change);
`view`/state is ground-truth (belief is a change to what fills it); one goal (no
arbitration). All three are body changes to a seam this campaign fills or leaves,
never a re-architecture.

## 5. Correctness — the ladder

The load-bearing new property is **A* PATH DETERMINISM**, plus **the plan is
valid** and **the plan achieves the goal**.

1. **Type-level.** `State: Ord`; `BTreeMap`/`BTreeSet` only (no `HashMap`);
   integer costs; the tick's strict-progress guard (reused).
2. **Property — A* PATH DETERMINISM + OPTIMALITY (the keystone).** Over random
   seeded graphs (grid/mesh with random integer weights): (a) `astar` returns the
   same path on repeat and after a serialize→reload of any state it reads —
   byte-identical; (b) its path cost equals a brute-force/Dijkstra optimum (A* ≡
   optimal); (c) when multiple optimal paths exist, the tie-break selects one
   deterministically (a generator that *plants* two equal-cost paths, per the
   c4/c5 generator-coverage lesson). Plus admissibility of the GOAP heuristic
   (never overestimates → A* stays optimal).
3. **Property — the plan is valid and achieves the goal.** For the GOAP space:
   every action in a returned plan has its precondition satisfied in sequence
   (the precondition chain holds — `Drink` is only ever preceded by arrival at
   water); executing the plan reaches `hydrated`. A multi-room water placement
   yields a `[move+, drink]` plan (genuine multi-step, not one-step).
4. **Genesis byte-identity (master oracle).** The genesis-zero pin holds; every
   committed artifact byte-identical; no census regen.
5. **Session determinism + termination.** Same seed + same waits → byte-identical
   session ledger; the step loop terminates (strict-progress guard) even when the
   goal is unreachable (budget → `Hold`).
6. **Observation shadow.** Possess seed 42, `wait` through a drive cycle, `look`/
   `why` — the agent walked to water and drank; `why` recounts the journey.

Every new test is **mutation-verified**: break the tie-break (non-total order) →
path-determinism reds; break admissibility (inflate the heuristic) → optimality
reds; drop a precondition check → plan-validity reds; collapse the drink reset →
the drive never falls. Report each. **Generator coverage is part of the review**
(plant equal-cost paths; unreachable goals; a zero-heuristic Dijkstra case).

## 6. Perf budgets (rolled-our-own; no criterion)

- **Deterministic, gated:** the plan A* produces for a fixed GOAP problem (seed
  42, a fixed water placement) is a pinned, drift-checkable action sequence; the
  session's committed moves/drinks over a fixed `wait` script are a pinned set.
- **Wall-time (heavy tier if needed):** A* expansions vs graph size — but the
  search is budget-bounded and local, not a hot path. No new unbounded structure.

## 7. Stages (each independently testable; genesis byte-identical throughout)

1. **The kernel A* (`kernel/src/astar.rs`).** The `SearchSpace` trait +
   `astar`/`astar_bounded`; the PATH-DETERMINISM + OPTIMALITY + tie-break
   property battery over random graphs (generator plants equal-cost paths,
   unreachable goals, Dijkstra cases). Pure, reusable, no vessel dependency.
2. **The GOAP space (vessel):** `Action`, `PlanState`, the `SearchSpace` impl
   (successors/goal/heuristic), the precondition-chain + plan-validity +
   multi-step + admissibility tests. Pure.
3. **The drive refactor + the planner in `decide` + the tick walks the plan:**
   drive folds `drank`; `Intent::Do(Action)`; `decide` runs A* returns first
   action; the tick executes actions step-by-step (agent-at / drank), bounded,
   terminating; the genesis-zero pin green; session determinism.
4. **Observation + close:** `why` recounts the journey + drink; the possess
   transcript re-baselined (planned journeys); book (chronicle — the GOAP/A*
   story, the reserved-seam payoff, UNI-19 the-engine-across-tenses; PSY-6 +
   UNI-19 re-score) + retrospective + close.

## 8. Risks and open items

- **The step-executing tick is the integration complexity.** The Wanting's clean
  discrete-event crossings become a step loop that walks a plan; the
  strict-progress guard + the A* budget bound termination. The one design-judgment
  point (stage 3): the action-duration model (`τ` per move) — kept simple (a
  fixed per-move duration); richer durations are a followup.
- **A* determinism is subtle.** A non-total open-set order, or `HashMap`
  anywhere, breaks byte-identity — the keystone tests (tie-break, reload) guard
  it; the review audits generator coverage (planted equal-cost paths).
- **Heuristic admissibility** — the mesh-distance heuristic must never
  overestimate (else A* loses optimality); property-covered.
- **`type-audit` runs in `make gate`** — new kernel + vessel pub boundaries carry
  verdict tags from the first commit; run `type-audit check` before each commit.
- **Domains untouched** (Walk §11); the kernel A* is domain-agnostic (kernel is
  its correct home, like the tick). Verified by the architecture test.

## 9. Decisions (promoted from the campaign decision ledger #1–#9)

- **#8 — determinism/save-format (leads G3):** census-free, genesis
  byte-identical; the planner runs only in the possess session; the kernel A* is
  a pure fn; no epoch, no census regen.
- **#2 — the general kernel A* (leads G3 with #8):** a tense-agnostic pure graph
  search (UNI-19's one engine); GOAP its first instantiation; navigation/confab
  deferred; no speculative extra instantiations.
- **#3 — A* PATH DETERMINISM (keystone):** BTree + total-order frontier +
  seed-tiebroken state; no HashMap, no RNG.
- **#4/#5 — minimal heterogeneous actions + the precondition chain:** {MoveTo,
  Drink}; `Drink` gated by at-water forces the `[move*, drink]` plan (genuine
  GOAP, not pathfinding); MAP-27 DSL deferred.
- **#6 — the planner replaces `decide`'s body** (The Wanting's reserved seam);
  `Intent::Do(Action)`; the tick walks the plan a step at a time; re-plan each
  step, no caching.
- **the drive folds `drank` events** (§4.4): drinking is a *planned action*, not
  automatic proximity — the change that makes `Drink` a real precondition-gated
  action (DRIVE ≡ FOLD preserved, now over `drank`).
- **#7 — bounded local search** (game-as-lens; a step budget → `Hold` if
  unreachable).
- **#9 — cost/belief/arbitration stay reserved** (the next three body-swaps).

## 10. Definition of Done

- All four stages green under `make gate` (incl. type-audit); the full
  `cargo nextest run --workspace` before any FF; census fixtures untouched
  (byte-identical — genesis commits no plan/drank; verified vs the merge-base).
- A* PATH DETERMINISM + OPTIMALITY + tie-break + plan-validity + precondition-
  chain + session-determinism + termination, all green and mutation-verified;
  generator coverage audited (planted equal-cost paths, unreachable goals).
- Book: a chronicle entry (candidate *The Foresight*), a freshness sweep (the
  liveness chapters — movement is now planned, not reactive), the re-baselined
  transcript (planned journeys + drinking), and the frontier bookkeeping
  (**PSY-6** re-scored: the goal rung / A* planner shipped, arbitration/cost/
  belief deferred; **UNI-19** re-scored: the kernel A* engine shipped, the other
  tenses its instantiations; **MAP-27** noted as the action-DSL followup).
- A one-page retrospective in `docs/retrospectives/`.
- The followup register's rows promoted into the retrospective.
