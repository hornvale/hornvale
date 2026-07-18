# The Wanting (The Walk · Milestone 2 · the drive rung) — Design

> **STATUS: SHIPPED** — *The Wanting*, all four tasks landed on
> `worktree-the-wanting` 2026-07-18 (byte-identical; census-free; the drive
> lives only inside a possess session). PSY-6's drive rung: a homeostatic
> sustenance drive, a pure fold over an NPC's own `agent-at` history, with a
> hysteresis dead-band closing the gap it feels between its setpoint and its
> state; the meaningless destination becomes the resource that satisfies it;
> a discrete-event tick advances exactly to each closed-form threshold
> crossing; the GOAP goal rung's seam (`decide`/`Goal`/`view`) is reserved,
> interface-shape only. `needs` reads a co-located NPC's felt state as
> diegetic prose; `why` recounts the drive's own reason for each move.

Program: **The Walk** (metaplan `docs/superpowers/specs/2026-07-11-the-walk-metaplan-design.md`,
Milestone 2 "the living world," Campaign V "The World Moves" — the SECOND
liveness slice: the motivation engine's first rung, PSY-6). Base: main
`@67bc1a2`. Builds directly on **The Quickening** (`windows/vessel/src/liveness.rs`
— NPC movement on c6's tick, a stateless clock schedule). Chronicle-name
candidate: **The Wanting** — the agent's first *want*, the setpoint gap it acts
to close; confirmed at close.

## 1. What this is

The Quickening made the world move: NPCs walk a fixed daily route (home ⇌ a
neighbor room, on their species' activity-cycle). But the route has **no
reason** — the neighbor room is a meaningless "destination," and the NPC goes
there because a *clock* says so, not because it *wants* anything.

This campaign gives the routine a **why**. It lands PSY-6's first rung — the
**drive**: a homeostatic need with *memory* that accumulates over time, and an
agent that **acts to close the gap** between its setpoint and its state. One
drive (a "sustenance" need — thirst/foraging); the meaningless destination
becomes the **resource** that satisfies it; the NPC goes to the water *because
it is parched*, drinks, returns, and grows parched again. The move is committed
with provenance naming the drive, so the world *remembers why*: `why the herder`
→ "it went to the water on day 5 because its thirst had grown."

It is the difference between a puppet and an agent — the first **animate** NPC,
acting from an inner state rather than a clock. Deliberately tiny: **one** drive,
a **reactive** controller (no planning), no arbitration between competing needs —
that (and the GOAP/A* planner) is the *goal* rung, a later campaign.

## 2. Scope

**In:**

1. **The drive-state (derived).** `drive(npc, t)` — a scalar "sustenance" need,
   a pure deterministic **fold over the NPC's committed `agent-at` history**: it
   rises while the NPC is away from its resource and falls while at it (§4.2).
   Never committed — re-derivable from the position log (the seam rule).
2. **The resource anchor.** The NPC's destination becomes a **resource cell**
   with meaning — the lowest-elevation neighbor room ("toward water"), the
   semantics The Quickening's §4.2 already named but left inert (§4.1).
3. **The hysteresis controller.** Two thresholds — an **act** threshold (drive
   rises past it → seek the resource) and a **sated** threshold (drive falls past
   it → return home), with the drive accumulating between (§4.3). The dead-band
   prevents thrashing.
4. **The drive-driven movement tick.** A `TickSystem` (through c6's kernel
   `tick`) that, over a `wait` interval, computes the drive's threshold
   **crossings in closed form** (discrete-event, not fixed-timestep — §4.4) and
   commits a dated `agent-at` event at each crossing, with **provenance naming
   the drive**.
5. **The why + the felt state.** `why <npc>` recounts the drive-driven moves
   with their reason (via the enriched provenance, reusing the existing
   `recount`); and a `needs`/`self` read lets the possessed agent perceive its
   **own** drive as a *felt state* ("You are parched." / "sated"), never a raw
   number (§4.5).

**Out (deferred; captured in `followups.md`):**

- **The GOAP / A* planner (the goal rung of PSY-6)** — multi-step plans to reach
  a goal; this slice is a reactive drive, no planning.
- **Multiple competing drives + arbitration** — thirst vs fatigue vs hunger;
  competition is what *requires* prioritization/planning (the goal rung). One
  drive by construction needs none.
- **Fatigue / safety / social drives** — fatigue overlaps the existing
  activity-cycle; safety needs threats; social needs interacting agents. Later.
- **Emotion (PSY-7)** — the *appraisal* of the gap, the rung above drive.
- **Belief / inference (UNI-1)** — the drive reads ground-truth position here;
  reading the *projection* (a possibly-false belief about where the water is) is
  Milestone-2-later. (PSY-6's "the planner reads belief, not truth" applies at
  the goal rung.)
- **Player-acts-mutate (Campaign IV)** — the player satisfying its own drive by
  acting is a sibling slice.

## 3. Determinism contract — the save-format line (leads G3)

**Census-free, genesis byte-identical — unchanged from The Quickening. The drive
lives only in the possess session.**

- **No drive fact and no drive-driven `agent-at` at genesis.** `build_world`
  commits exactly what it commits today; a genesis world (census, almanacs,
  gallery/laboratory artifacts) is **byte-identical**. The Quickening's
  genesis-zero-`agent-at` pin still holds. **No census regen, no epoch.**
- The drive-state is **derived** (a fold over the committed `agent-at` history),
  so a saved, reloaded session re-derives the same drive and continues
  deterministically — no second source of truth.
- **No RNG:** the drive dynamics, the threshold crossings, and the resulting
  moves are pure functions of (seed, committed history, elapsed time).
- **Lorenz-safe (§11):** drive accumulation is monotone within a segment and
  non-chaotic; the crossings are solved in **closed form** (no integrator, no
  step-size error) at full precision.
- `agent-at` already exists (The Quickening). This campaign enriches its
  **provenance** (naming the drive) and may add the game-layer read verbs; it
  registers **no** genesis-reaching predicate. If it adds a game-layer
  predicate (e.g. a resource tag), that predicate is registered by the session,
  never at build (as The Quickening did for `agent-at`).
- **Game-layer artifact update (not a determinism concern):** the over-time
  possession transcript will show drive-driven movement + the "why" — a
  legitimate game-behavior update, exactly as The Quickening re-baselined its
  transcript. Genesis artifacts stay byte-identical (verified at close).

## 4. Architecture

The Walk §11: **new game-layer code in `windows/vessel`, domains untouched.** All
of this extends `windows/vessel/src/liveness.rs` (The Quickening's home), reusing
`Npc`, `AGENT_AT`, `agent_position`, and the movement tick; consuming the kernel
(`tick`, ledger, `WorldTime`) and the locale fields (elevation, for the resource
anchor). No domain crate is modified (`cli/tests/architecture.rs` enforces it).

### 4.1 The resource anchor

The NPC's `destination` (today the lowest-address neighbor, meaningless) becomes
its **resource** — the lowest-elevation of its home room's three mesh neighbors
("toward water"), read from the locale/terrain elevation field at derive time.
Deterministic, seed-independent, a pure pick. (Flavor — water vs forage — is a
plan detail; the *mechanism* is "the destination is the thing the drive seeks.")

### 4.2 The drive-state (a fold over history — a derived view, like belief)

`drive(npc, t)` is a scalar in `[0, 1]`, a pure fold over the NPC's committed
`agent-at` facts (each dated) up to `t`:

- While the NPC's latest committed position is **not** its resource, the drive
  **rises** linearly at rate `r` per day: `drive = min(1, drive_at_last_move +
  r · (t − day_of_last_move))`.
- While it **is** at its resource, the drive **falls** at rate `s` per day
  (satiety), floored at 0.
- Genesis anchor: before any `agent-at`, `drive = drive0` (an authored initial,
  e.g. 0). `r`, `s`, `drive0`, and the thresholds are authored constants.

This is structurally a **CQRS/event-sourced projection** — the drive is a
*derived view over the one ledger*, the same shape as belief (UNI-16) and the
query indexes (UNI-20). It is never stored; it is recomputed (a bounded fold —
the NPC has few committed moves) on demand.

### 4.3 The hysteresis controller (the thermostat dead-band)

The NPC's **intent** (where it wants to be) is a function of its drive and its
current position, with two thresholds `ACT` and `SATED` (`SATED < ACT`):

```
intent(npc, t):
  at_resource = (latest position == resource)
  if !at_resource and drive(npc, t) >= ACT   -> resource   (parched -> seek water)
  if  at_resource and drive(npc, t) <= SATED  -> home       (sated -> go home)
  else -> hold current position                             (dead-band: no move)
```

The dead-band (`SATED < ACT` with accumulation between) is what prevents the
memoryless-reflex thrashing (§decision #4). The activity-cycle from The
Quickening is retained as a **waking gate**: an NPC only moves during its active
phase (a diurnal NPC seeks water by day) — so intent is realized only while
awake; asleep, it holds. (Plan may simplify to drive-only if the gate proves
noise; the drive is the primary mover.)

### 4.4 The drive-driven tick (discrete-event, closed-form crossings)

Because the drive is **path-dependent** (it has memory), the NPC's position is
*not* a closed-form function of `t` (unlike The Quickening's clock schedule).
But the drive accumulates **linearly**, so the next threshold-crossing time is
**solvable in closed form**: from a known `(drive_last, day_last)`, the drive
reaches `ACT` at `day_last + (ACT − drive_last)/r`. So the tick is a
**discrete-event simulator**: over a `wait` interval `(t_old, t_new]`, it
advances NPC-by-NPC to the next crossing time, flips intent, commits a dated
`agent-at` at the crossing day, recomputes the next crossing, and repeats until
past `t_new` — exact, no integration error, trivially Lorenz-safe. Each commit
carries provenance naming the drive (`"sought water (thirst)"`).

This **extends** The Quickening's `AgentMovements::step` (which emitted ≤1 fact
per NPC at a single `at_time`) to emit a dated fact **per crossing** within the
interval. A `wait` that spans several drive cycles commits several dated moves,
each at its true crossing day — the history stays coherent (the read contract:
`agent_position` = latest committed, which after the tick equals the intent at
`t_new`).

### 4.5 The why and the felt state

- **Provenance → why.** The drive-driven `agent-at` carries provenance naming
  the drive; the existing `why <npc>`/`recount` (which replays dated facts)
  surfaces it: "day 5 — went to the water (thirst)."
- **The felt state.** A `needs` (or `self`) read renders the possessed agent's
  *own* drive as a diegetic felt state — thresholded to words, never a raw
  number: `drive >= ACT` → "You are parched."; `drive <= SATED` → "You are
  sated."; between → "You could drink." This is the first time the player has an
  **inner state** — a hook for later (the drive steering the player's options).

### 4.6 Reserved seams — the planner slots in as a body-swap (decision #9)

GOAP is the arc's throughline, and this slice is *goal-generation-from-a-drive*
without the planner (§4.6b explains why). To make the eventual planner a
**body-swap, not a seam-change**, the decision is shaped now as the **degenerate
one-goal case of goal-selection-then-planning** — at *zero* machinery cost. Three
minimal reservations, and nothing more:

1. **A named `Goal`** — a desired world-state. Here there is exactly one
   (`Goal::AtResource`), generated by the drive when it crosses `ACT`. Naming it
   makes "the drive *generates a goal*" explicit rather than implicit in a
   position comparison.
2. **`decide(npc, view, t) -> Intent`** — the single decision function. Its
   *signature* is the shape a future multi-drive arbitration + A\* planner will
   fill; today its *body* is trivial (one drive → one goal → the one-step
   "move toward the resource" intent). **The tick depends only on `Intent`**
   (an intended position), never on the drive internals — so the body can be
   replaced without touching the caller, the commit path, or the read.
3. **`view` is a perception abstraction** — "what the agent perceives" of the
   world. Today its body is ground-truth position; PSY-6's decisive move (the
   planner reads the *projection*/belief, UNI-16, so deception steers it for
   free) is later a change to what `view` returns — **a body change, not a seam
   change.**

**Discipline (the failure mode to avoid):** this is *interface shape only* — one
`Goal` enum with one variant, one `decide` fn, one `view` parameter. **No
planner, no action graph, no cost function, no arbitration is built here.**
Over-reserving (a speculative GOAP framework) would contradict the cardinality-1
argument; the reservation must read as an obvious minimal seam, not a half-built
engine. The map it preserves: world-state = the ledger (built); goal = the
drive's setpoint (built here); planner = A\* (deferred, trivial at cardinality-1);
cost = the psychology vector (deferred); belief-read = the `view` seam (reserved,
ground-truth body now). When the planner campaign lands, it changes `decide`'s
body and adds drive #2 — not the tick, the commit, or the ledger.

### 4.6b How much PSY-6

The **drive rung only**, cardinality-1. A single reactive controller closing a
single gap. No A* planner (the goal rung), no arbitration (needs ≥2 competing
drives), no emotion/appraisal (PSY-7), no belief-reading (the drive reads
ground-truth position, not a projection). The setpoint/gap-closing *core* of
PSY-6 is realized; its planning and multiplicity are deferred.

## 5. Correctness — the ladder

The load-bearing new properties are **the drive is a deterministic fold** and
**the controller does not thrash**.

1. **Type-level.** Reuse c6's `TickSystem`/`tick`, The Quickening's `Npc`/
   `agent-at`/`agent_position`; the drive is a pure `fn`; no `HashMap`.
2. **Property — DRIVE ≡ FOLD (determinism).** `drive(npc, t)` is a pure,
   deterministic function of the committed history — recomputing it twice, or
   after a serialize→reload of the session, yields the identical value
   (seeded-loop property tests over random histories/thresholds/rates). Bounded
   in `[0, 1]`; monotone within a segment.
3. **Property — NO THRASH (the hysteresis guard).** Over a long `wait`, the
   number of committed moves is bounded by the number of genuine drive cycles
   (≈ interval / cycle-length), NOT one-per-tick — a memoryless controller
   would commit ~one move per evaluation. A test asserts a multi-cycle `wait`
   commits the *expected small* number of crossings, and that consecutive moves
   alternate resource↔home (no resource→resource). **Mutation-verify:** collapse
   the two thresholds to one (`SATED = ACT`) → the no-thrash test reds (the
   controller oscillates).
4. **Property — CLOSED-FORM CROSSING EXACTNESS.** For any `wait` interval, each
   committed move's day equals the analytic crossing time, and the post-tick
   `agent_position` equals `intent(npc, t_new)` (the jump-coherence contract,
   extended from The Quickening). Cover a single `wait` that spans **multiple**
   crossings.
5. **Genesis byte-identity (the master oracle).** The Quickening's
   genesis-zero-`agent-at` pin still passes; every committed artifact is
   byte-identical (the drift-check); no census regen.
6. **Observation shadow.** A possess-session integration test: possess seed 42,
   `wait` long enough for a drive cycle, `look`/`needs`/`why` — the NPC sought
   its resource and returned, the felt state reads, and `why` recounts the dated
   reason.

Every new test is **mutation-verified** (the standing discipline): break the
fold (drop the satiety fall), break the hysteresis (collapse thresholds), break
the crossing math (off-by-one on the interval) — each reds its test.

## 6. Perf budgets (rolled-our-own; no criterion)

- **Deterministic, gated:** the `agent-at` moves committed by a fixed possess
  script (seed 42, a fixed `wait` sequence) are a pinned, drift-checkable set
  (count + dated resource/home moves + provenance). No wall-time hot path (the
  discrete-event tick is O(#npcs × #crossings-in-interval); crossings per NPC per
  `wait` are few).
- No new unbounded structure at genesis; session moves accrue bounded by
  #npcs × #drive-cycles.

## 7. Stages (each independently testable; genesis byte-identical throughout)

1. **The resource anchor + the drive-state fold (pure).** The resource-cell pick
   (lowest-elevation neighbor); `drive(npc, t)` as the history fold; property
   tests (deterministic, bounded, monotone-within-segment; DRIVE ≡ FOLD under
   reload). No ledger writes — pure functions over a given history.
2. **The hysteresis controller + closed-form crossings.** `intent(...)`; the
   next-crossing solver; the NO-THRASH property + the CLOSED-FORM EXACTNESS
   property; mutation-verify the dead-band.
3. **The drive-driven tick + provenance.** Extend `AgentMovements::step` (or a
   sibling `DriveMovements`) to emit a dated `agent-at` per crossing with
   drive-naming provenance; the multi-crossing `wait` coherence test; the
   genesis-zero pin still green.
4. **The felt state + why + close.** The `needs`/`self` felt-state read; `why`
   surfaces the drive provenance; the possess-session integration test; the
   over-time transcript re-baselined (game-layer artifact) with genesis
   byte-identical; book (chronicle, freshness sweep, PSY-6 registry re-score) +
   retrospective + close.

## 8. Risks and open items

- **The drive-vs-activity-cycle reconciliation** (§4.3): the drive is the primary
  mover, the activity-cycle a waking gate. If the two interact confusingly
  (e.g. an NPC parched at night that can't move), the plan may simplify to
  drive-only for the first slice — the drive is the point, the gate is optional.
  Named as the one design-judgment call for stage 2.
- **Closed-form crossing edge cases** (§4.4): a `wait` that lands exactly on a
  threshold, a drive already saturated at `1`/`0`, an interval with zero
  crossings — property-covered and mutation-verified.
- **The felt-state prose** must read diegetically (words, not numbers) and stay
  deterministic; it is rendered from the derived drive, so it re-derives.
- **`type-audit` runs in `make gate`:** new pub boundaries (the drive fn, the
  controller, the tick, any read verbs) carry verdict tags from the first commit.
- **Domains untouched** (Walk §11): all in `windows/vessel`, verified by the
  architecture test.

## 9. Decisions (promoted from the campaign decision ledger #1–#8)

- **#7 — determinism/save-format (leads G3):** census-free, genesis
  byte-identical; the drive lives only in the possess session; derived (re-
  derivable) drive-state; no epoch, no census regen.
- **#3 — the drive is a derived fold (leads G3 with #7):** a projection over the
  committed `agent-at` history (= belief = indexes, UNI-20); never committed.
- **#2 — the drive rung, cardinality-1:** one reactive drive, no A* planner, no
  arbitration (that is the goal rung).
- **#9 — reserved seams (interface shape only):** `decide(npc, view, t) ->
  Intent` is the degenerate one-goal case of goal-selection-then-planning; a
  named `Goal`; `view` abstracts perception (ground-truth now, belief later). The
  future GOAP planner is a body-swap of `decide`, not a seam-change — built at
  zero machinery cost, no speculative framework.
- **#4 — the hysteresis controller:** two thresholds + accumulation (the
  thermostat dead-band) prevents thrashing.
- **#6 — the discrete-event tick:** closed-form threshold crossings (exact, no
  integrator), extending The Quickening's `step` to emit a dated fact per
  crossing.
- **#5 — one universal homeostatic drive (sustenance) with a material anchor**
  (the destination becomes the resource); fatigue/safety/social deferred.
- **#8 — the drive is made visible through provenance (why) + a diegetic felt
  state** (the player perceives its own drive as words, not a number).

## 10. Definition of Done

- All four stages green under `make gate` (incl. type-audit); the full
  `cargo nextest run --workspace` before any FF; census fixtures untouched
  (byte-identical — genesis commits no drive fact; verified by diff against the
  merge-base).
- DRIVE ≡ FOLD (determinism under reload) + NO-THRASH + CLOSED-FORM EXACTNESS +
  the genesis-zero pin, all green and mutation-verified.
- Book: a chronicle entry (candidate *The Wanting*), a freshness sweep (the
  vessel/liveness chapters that call the NPC routine a fixed clock schedule —
  now it is drive-driven), the re-baselined over-time transcript (drive movement
  + why), and the frontier bookkeeping (PSY-6 re-scored: the drive rung shipped,
  the goal-rung planner deferred; UNI-20 gains the drive as a derived view).
- A one-page retrospective in `docs/retrospectives/`.
- The followup register's rows promoted into the retrospective.
