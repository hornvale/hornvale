# The Quickening (The Walk · Milestone 2 · first liveness) — Design

> **STATUS: SHIPPED** — *The Quickening*, all four tasks landed on
> `worktree-the-quickening` 2026-07-18 (byte-identical; census-free; the
> world moves only inside a possess session). The world's first autonomous
> motion: a derived NPC's deterministic daily route, committed as dated
> `agent-at` events via c6's tick, observed on `look`/`wait` and recounted
> by a new `why`/`npcs` provenance read. Plan-time pivot: the original
> demography-growth actor was measured inert (population is a conserved
> equilibrium, decision-ledger #8) and replaced with NPC movement before
> any code was written. FF to main is Nathan's G6 call. Chronicle:
> `book/src/chronicle/the-quickening.md`.

Program: **The Walk** (metaplan `docs/superpowers/specs/2026-07-11-the-walk-metaplan-design.md`,
Milestone 2 "the living world," Campaign V "The World Moves" — first minimal
slice). Base: main `@7305dfc`. This is **not** the ECS substrate program (that
closed at c6 *The Ordination*; ECS c7 spatial-partition is reserved per
decision 0037, no measured trigger). Chronicle-name candidate: **The
Quickening** — the world's first perceptible autonomous motion after six
campaigns of a frozen world; confirmed at close.

> **Revised 2026-07-18 (decision-ledger #8):** the original actor — settlement
> demography growing toward carrying capacity — was falsified against the
> substrate: genesis `population` is a *conserved equilibrium* readout (values
> 2–8, "conserved against total K by construction"), not a growing headcount,
> so growth-toward-K is flat and yields zero milestones. The world is a static
> equilibrium; liveness must come from **agent action**. The actor is now a
> **derived NPC's deterministic daily movement**. The seam-rule architecture,
> the census-free posture, the `wait` verb, and the use of c6's tick all carry
> over unchanged.

## 1. What this is

Hornvale's world is generated at genesis and then **frozen**. Milestone 1 of
The Walk shipped a walkable frozen slice — `possess --seed N` gives a
look/go/examine loop over "a pure step function over a frozen world" (The Seam,
The Casement). The physical layer (sky, climate) already advances as a *derived
field over time*. But the **social layer is a static equilibrium**: settlements,
populations, and every agent-layer fact are committed at genesis day 0 and never
change, and — as the substrate makes plain — there is no built-in forward-time
dynamic to surface (population is conserved; nothing grows). So the world can
only be made to move by an **agent acting** in it.

This campaign lands the smallest real "the world moves without you": while you
possess one agent and let time pass, **another agent goes about its day** — a
herder walks down to the river at dawn and back at dusk — and you perceive it on
re-query. It is the true shape the metaplan names ("NPCs are the same agent
spine, taking deterministic daily actions"), and the first place the whole
substrate program pays off at once:

- **c6's tick** (`TickSystem` + `tick`) — the mechanism, now actually *run*, to
  commit a dated event when time advances.
- **c5's kind-change-over-sim-time** (dated facts, latest-wins reads) — an
  agent's *position* is exactly the "state that changes over sim time" c5 built
  the read for.
- **Milestone 1's agent spine** — the possessed agent (`vessel::mint_flagship`,
  an `Agent` with a `RoomAddr` position) derived from the population; an NPC is
  the same derivation, minus the player.

It is deliberately tiny: one to a few NPCs, each a *deterministic* daily route,
one kind of committed event (a dated position). No GOAP planner, no needs, no
belief, no growth model — those are later Milestone-2 campaigns.

## 2. Scope

**In:**

1. **Derived NPCs.** One to a few NPCs derived the way the possessed agent is
   (`mint_flagship`'s pattern), each anchored to a home settlement and given a
   species (for its activity-cycle). Derived, not stored — re-derivable from the
   genesis world (§4.1).
2. **The derived daily route.** `scheduled_position(npc, t)` — a pure,
   deterministic function of the NPC's home, a nearby destination cell, and its
   species activity-cycle: at home by its rest phase, at the destination by its
   active phase (§4.2). Lazy, re-computed on query, **never committed** (the
   *routine* is reversible — the seam rule, §3).
3. **The movement `TickSystem`.** When the possess session advances time, a
   `TickSystem` (run through c6's kernel `tick`) evaluates each NPC's scheduled
   position at the new time and **commits a dated `agent-at` event whenever the
   NPC's position has changed** since its last committed one — the discrete,
   irreversible record of where each agent *was* on each day (§4.3). This is the
   campaign's use of c6's tick, and what makes it *liveness* (accumulating,
   remembered history) rather than merely another derived field.
4. **The `wait` verb.** The possess session's existing `wait [N]` stub (today:
   "the world stays still") is made real: it advances the session's `WorldTime`,
   runs the movement tick over the elapsed interval, and re-focalizes.
5. **Observation on re-query.** After `wait`, `look` reflects which NPCs the
   possessed agent would now perceive at or near its location, and their motion
   ("The herder has gone down to the river."). Read as latest-committed-position
   ELSE derived-scheduled-position (§4.3, reusing c5's latest-wins).
6. **Provenance.** A committed `agent-at` is a dated, provenanced fact, so the
   world *remembers*: an `explain`/history read can recount "the herder was at
   the river on day 5."

**Out (deferred; captured in `followups.md`):**

- **Needs / goals / the GOAP motivation engine (PSY-6)** — the routes are fixed
  deterministic schedules this slice, not planned; the full planner + autonomy
  ladder are later (§4.4).
- **Belief / inference (UNI-1)** — Milestone-2-later.
- **NPC interaction** (agents affecting each other, encounters, conversation) —
  a later slice; this one is parallel non-interacting routines.
- **A demographic time-model** (birth/death/growth) — the substrate has none
  (ledger #8); introducing one is its own campaign.
- **Player-acts-mutate (Campaign IV)** — the player's own verbs committing
  events is a sibling slice, not this one (this slice is *the world moves
  without you*).
- **Genesis-time liveness** — no `agent-at` event is ever committed at world
  build; the world moves only inside a possess session (§3).

## 3. Determinism contract — the save-format line (leads G3)

**Genesis worlds are byte-identical; liveness is census-free. The world moves
only inside a possess session, never at build.**

- **No `agent-at` event is committed at genesis.** `build_world` commits exactly
  the facts it commits today. A genesis world — what the census, the three
  seed-42 almanacs, and every gallery/laboratory artifact measure — is
  **byte-identical**; the existing drift-check is the proof. **No census regen,
  no epoch.**
- Movement events commit **only** when a possess session advances time. The
  session's evolving `World` registers the liveness predicate (`agent-at`) at
  session start and commits `agent-at` facts as `wait` runs — an *evolved*
  world, distinct from the frozen genesis world by construction.
- **Byte-deterministic session:** same seed + same sequence of `wait`/verbs →
  byte-identical committed events. No RNG anywhere; the routes and the positions
  are pure functions of (seed, elapsed time).
- **Lorenz-safe (§11):** a fixed daily schedule is periodic and non-chaotic; no
  chaotic integrator is seeded from quantized ledger floats.
- **The existing possession-transcript gallery artifact is at day 0 (frozen) →
  unchanged.** The `wait` verb is opt-in; the committed transcript does not
  advance time, so no committed artifact drifts. A **new** over-time transcript
  may be *added* (and drift-checked), never a diff of the old one — verified at
  plan time.
- `agent-at` is a **game-layer predicate**, owned and registered by the liveness
  window (The Walk §11: new windows only, domains untouched). It never enters a
  genesis world's registry.

## 4. Architecture

The Walk §11: **new window code, domains untouched.** The liveness logic lives
in the game-layer vessel window (`windows/vessel` — where the possess session
and the `wait` stub already are), consuming the kernel (`tick`, the ledger,
`WorldTime`, the room mesh) and the already-present agent spine. No domain crate
is modified (enforced by `cli/tests/architecture.rs`).

### 4.1 Deriving NPCs

An NPC is the possessed-agent derivation minus the player. `mint_flagship`
already derives one `Agent { position: RoomAddr, … }` from the flagship
settlement's argmax cell. NPCs are derived the same way from a small,
deterministic selection of settlements (e.g. the *k* most populous, k small and
authored), each yielding one NPC anchored at its home settlement's cell/room,
carrying its settlement's species (for the activity-cycle). Derived at session
start from the genesis world; re-derivable, never stored.

### 4.2 The daily route (derived, deterministic)

```
scheduled_position(npc, t) =
    if active_phase(npc.species, t)  -> npc.destination_room
    else                             -> npc.home_room
```

- `home_room` = the NPC's settlement room (§4.1). `destination_room` = a nearby
  room reached by the kernel room mesh's O(1) neighbor walk (`kernel::room`,
  MAP-28) — a fixed, deterministic pick (e.g. the lowest-elevation adjacent
  cell, standing in for "the river"; the exact rule is an authored, deterministic
  choice, plan-detail).
- `active_phase(species, t)` reuses the species **activity-cycle** (diurnal /
  nocturnal / crepuscular — already authored per species) against the day
  fraction of `t` (`WorldTime.day`'s fractional part). Diurnal → active by day.
- Pure, deterministic, periodic (Lorenz-safe). Re-computed on query; **never
  stored**. This is the *routine* — the reversible half.

**Fidelity note (Nathan's call at G3):** a two-point home⇌destination route on a
pure activity-cycle is a coarse first behavior — real agents have errands,
seasons, needs, and interaction. The first slice takes the fixed two-point
schedule because it is deterministic, observable, and needs no new substrate;
richer routines (multi-stop, need-driven, seasonal) are followups.

### 4.3 The movement system (committed via c6's tick)

The committed event is what makes this **liveness** (accumulating, remembered
history) rather than merely a derived field. A `TickSystem`:

```
struct AgentMovements { npcs: Vec<Npc>, at_time: WorldTime };
impl TickSystem for AgentMovements {
    fn label(&self) -> &'static str { "agent-movements" }
    fn step(&self, frozen: &Ledger) -> Vec<Fact> {
        // for each npc: p = scheduled_position(npc, at_time);
        // if p != latest committed agent-at for npc in `frozen`
        //    (else the npc's genesis home) -> emit a dated agent-at fact
        //    (subject = npc entity, object = RoomAddr, day = at_time).
    }
}
```

- `wait <interval>` advances the session time to `t_new` and runs c6's `tick`
  with `at_time = t_new`. For an interval that spans several activity phases
  (a multi-day `wait`), the tick commits the NPC's position **at t_new** plus
  any intermediate phase-transition it jumped over that the observer would
  perceive as history — the interval's crossings, not a per-instant loop (the
  schedule is O(1) to evaluate at any t; §5 covers the jump case).
- `agent-at` is **non-functional** (an agent's position changes over time — the
  same shape as c5's `instance-of`); the current position is the *latest*
  committed one (reuse c5's latest-wins read), else the derived scheduled
  position, else the NPC's genesis home.
- The read for rendering: `agent_position(ledger, npc, t)` = latest committed
  `agent-at` for `npc` ELSE `scheduled_position(npc, t)`.

The `agent-at` object is a `RoomAddr` — serialized how the existing agent/room
facts serialize (plan-detail: a `Value::Text` of the room address string, or the
existing room-fact encoding; must round-trip and quantize like every other
serialized value).

### 4.4 How much PSY-6

**None of the planner.** The route is a fixed deterministic schedule, not a
plan; there are no needs, goals, or costs. PSY-6's setpoint/gap-closing idea is
not yet exercised (the activity-cycle is a phase, not a setpoint). The first
slice proves "the social world moves observably and is remembered," not "agents
plan." The planner, needs, and the autonomy ladder are the next liveness
campaign.

### 4.5 The `wait` verb + observation

`windows/vessel/src/session.rs` already has a `wait` arm (today a no-op that
advances `self.day`). It is made real: advance the session's owned, evolving
`WorldTime` and ledger, run the movement tick, and return a focalized line
noting any motion the possessed agent would perceive. The session gains an
owned evolving `Ledger` + `ConceptRegistry` (cloned from the world at start,
with `agent-at` registered), because committing requires `&mut` state the
current `&'w World` borrow does not provide; c6's `tick` returns a *new* Ledger,
which fits this exactly. `look` renders co-located / nearby NPCs via §4.3.

## 5. Correctness — the ladder

The load-bearing new property is **the world moves only in-session, and
deterministically**.

1. **Type-level.** Reuse c6's `TickSystem`/`tick` and c5's latest-wins read; no
   `HashMap`; the route is a pure `fn`.
2. **Property — SESSION DETERMINISM.** Same seed + same `wait` sequence →
   byte-identical committed `agent-at` facts (serialize the session ledger
   twice, compare). Plus: **the route is a total, deterministic function** —
   `scheduled_position(npc, t)` is defined and equal on repeat for every t, and
   an NPC is at its home during rest phase and its destination during active
   phase over a full cycle (seeded-loop property tests over random t). Plus:
   **the jump case** — a single multi-day `wait` that skips over a phase change
   still commits the resulting position change (a wait from day 0.2 to day 1.9
   must reflect that the NPC went out and came back, i.e. the latest committed
   position matches `scheduled_position(npc, 1.9)` and the history is coherent —
   no missed or spurious `agent-at`).
3. **Genesis byte-identity (the master oracle).** A pinned test: a freshly built
   genesis world contains **zero** `agent-at` facts. Every committed artifact is
   byte-identical (the drift-check); no census regen.
4. **Observation shadow.** A possess-session integration test: possess seed 42,
   `wait` across an activity phase, `look` — the description reflects an NPC's
   motion; `explain`/history recounts the dated `agent-at`.

Every new test is **mutation-verified** before it counts (measure-don't-narrate):
break the schedule (swap active/rest phase), break the jump case (commit only
the final instant, dropping a skipped transition), break the genesis-zero pin
(commit an `agent-at` at build) — each must redden its test. **And** the actor
premise itself is measured, not assumed: a test asserts the derived NPCs
actually move (their `scheduled_position` differs between rest and active phase)
— the guard against the demography-style inert-actor failure (#8).

## 6. Perf budgets (rolled-our-own; no criterion)

- **Deterministic, gated:** the `agent-at` facts committed by a fixed possess
  script (seed 42, a fixed `wait` sequence) are a pinned, drift-checkable set
  (a count + the dated positions). No wall-time hot path (one O(#npcs) tick per
  `wait`; #npcs is small and authored).
- No new unbounded structure at genesis (events accrue only in a live session;
  bounded per session by #npcs × #waits).

## 7. Stages (each independently testable; genesis byte-identical throughout)

1. **NPC derivation + the daily route (derived, pure).** Derive the *k* NPCs
   (`Npc { entity, home_room, destination_room, species }`) and
   `scheduled_position(npc, t)` + `active_phase(species, t)`. Unit + property
   tests (total, deterministic, moves-between-phases — the anti-inert guard).
   No ledger writes, no session changes — pure functions.
2. **The movement `TickSystem` + `agent_position` read.** `AgentMovements::step`
   emitting dated `agent-at` facts for position changes; the latest-else-derived
   read; the jump-case property; the genesis-zero-`agent-at` pin.
3. **The `wait` verb + session time-advance.** Make `wait` real: the session's
   owned evolving ledger/registry; advance `WorldTime`; run the tick; the
   session-determinism property; the possess-session observation integration
   test (wait → look reflects an NPC's motion).
4. **Provenance + a gallery over-time transcript.** `explain`/history recounts a
   dated `agent-at`; add a NEW drift-checked "a possession over time" transcript
   artifact (the old frozen transcript unchanged). Book (chronicle, freshness
   sweep, registry) + retrospective + close.

## 8. Risks and open items

- **The jump-past-a-phase case** is the one subtle correctness point (§5.2): a
  multi-day `wait` must leave the ledger coherent (the NPC's latest committed
  position equals its scheduled position at `t_new`), not miss or duplicate a
  transition. Property-covered and mutation-verified.
- **The evolving-session state** (owned ledger + registry) is the one real
  structural change to the vessel session; it must not alter the frozen-world
  read paths when no `wait` has advanced time (a day-0 session behaves exactly
  as today — a pinned test).
- **The over-time transcript artifact** is the only committed artifact this
  campaign adds; it must be a NEW file (the frozen day-0 transcript stays
  byte-identical), verified by the drift-check at close.
- **`type-audit` runs in `make gate`** (The Presiding): the new pub boundaries
  (the NPC type, the route fn, the tick system, the `agent-at` predicate const)
  carry their verdict tags from the first commit.
- **Domains untouched** (The Walk §11): the NPCs, route, and movement live in the
  vessel window, consuming the kernel + the agent spine — verified by the
  architecture test.

## 9. Decisions (promoted from the campaign decision ledger #1–#8)

- **#5 — determinism/save-format (leads G3):** census-free, genesis
  byte-identical; the world moves only in a possess session; no epoch, no census
  regen; the over-time transcript is a new artifact, the frozen one unchanged.
- **#3 — the eager/lazy hybrid (leads G3 with #5):** derive the smooth routine
  (reversible — the seam rule), commit only the discrete position change via
  c6's tick (irreversible); overturned "commit a daily position fact
  unconditionally."
- **#8 — the actor pivot:** demography growth is inert against the substrate
  (conserved population, measured); the actor is a derived NPC's deterministic
  daily movement (the true metaplan-V shape). Supersedes #2/#6.
- **#4 — the time model:** `wait` jumps to the new time and commits the resulting
  position change(s) (not a per-instant loop); position read is
  latest-committed-else-derived (reuse c5).
- **#7 — PSY-6:** none of the planner; a fixed schedule, no needs.

## 10. Definition of Done

- All four stages green under `make gate` (incl. type-audit); the full
  `cargo nextest run --workspace` before any FF; census fixtures untouched
  (byte-identical — genesis commits no `agent-at`; verified by diff against the
  merge-base).
- Session determinism + jump-case + moves-between-phases + genesis-zero-`agent-at`
  green; mutation-verified.
- Book: a chronicle entry (candidate *The Quickening*), a freshness sweep (the
  vessel/possess chapters that describe the world as frozen — now it can move on
  `wait`), the new over-time transcript artifact, and the frontier bookkeeping
  (The Walk metaplan Milestone 2 opened; the relevant registry rows — the agent
  spine, UNI-20 derived-view, PSY-6 — re-pointed).
- A one-page retrospective in `docs/retrospectives/` (including the measured
  demography-inert-actor finding and the pivot — the measure-don't-narrate win).
- The followup register's rows promoted into the retrospective.
