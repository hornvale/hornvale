# The Rack — a turn reads arrays the tick wrote

**Date:** 2026-09-02 · **Registry rows:** `UNI-ecs-is-the-adaptive-cache`
(`raw`, high — this campaign's thesis in Nathan's own words),
`TOOL-tick-profile-2026-08` · **Program:** The Penstock metaplan
(`docs/superpowers/specs/2026-08-22-the-penstock-metaplan.md`), stage 3 in
spirit — the derived-component layer — entered for the turn path, not
the tick. · **Ledger:** `docs/superpowers/ledgers/2026-09-02-the-rack.md`

Decision block: 0596–0605.

A rack is a frame of slots. Everything the walk knows about a body — its
room, its felt state, whether it is on the roll — goes in the slot the body
was given when it joined the roster, and a turn reads across the slots
instead of asking the ledger about each body again. Nathan's brief: *"shift
the per-turn snapshot into the ECS-ish system we're building so that we can
iterate through an archetype-like structure and benefit from
cache-friendliness rather than doing cold-ish memos … we need to always be
on the lookout for these performance regressions."*

## 1. The problem, measured

After The Roll a possession stands among 67 residents, and every turn's
`Session::snapshot` (`windows/vessel/src/session.rs:2097`) re-derives each
present body's felt state from the ledger:

```rust
    let affect = affect_of_memo_occupied(
        &self.ledger, npc, &band, self.day, &terrain,
        &mut afraid_memo, Some(&self.occupancy), &mut mesh_memo, &mut home_nav_cache,
    );
```

with `afraid_memo`, `mesh_memo` and `home_nav_cache` built **cold on every
call** because `snapshot` is `&self` and cannot reach the session's own
memos (`session.rs:2110-2135`, the comments say exactly this). Each call is
a fold over the body's committed history (`facts_of(DRANK)`, the thirst
integral, `creature_fatigue`, `hazard_memory_memo`), a band-shared water
belief, and an A\* plan home (`home_nav_cache.home_nav`), then a full
arbitration. `Session::needs` (`:8095`) does the same, and re-derives the
shadowcast a second time. The presence line does not read affect.

Measured on ambrose, release, seed 42's flagship (`windows/vessel/examples/
move_cost.rs`, The Hone):

```text
  verb                 handle      snapshot()+json
  go n  (room scale)   0.5 ms      4–32 ms   (32 in the home room, 67 present)
  go n  (chamber)      0.007 ms    44 ms
  enter                34 ms       44 ms
  look  (chamber)      17 ms       45 ms
  needs                27 ms       31 ms
```

A chamber step costs seven microseconds to handle and forty-four
milliseconds to snapshot. The browser client (`clients/vessel/wasm/src/
lib.rs:173`) and the terminal client (`clients/game/bin/src/driver.rs:2595`)
snapshot after every turn, so the snapshot IS the turn. Under the client
workspace's unoptimised profile the same turn cost 300–630 ms; The Hone
fixed that profile, and this campaign removes the work.

**The tick already holds what the turn recomputes.** `step_with_occupancy`
keeps a `WalkState` per walked body, and `WalkState.affect` is *"the most
recent resolution's felt state … recorded alongside `mode` from the
identical computation, never a second derivation"* (`liveness.rs:6320-6328`,
`:6516`). It is dropped when the function returns; only the driven body's
survives (`step_one_with_controller` returns it; `Session.driven_affect`).

**The two quantities are not the same, and the read's own doc says so.**
`affect_of` is *"the same arbitration a walk step runs, but stateless"*
(`liveness.rs:4770`): it passes `alarm: None`, `Mode::Idle` (no hysteresis,
"no sticky Helpless"), an empty `visited` frontier, no pending facts, and
the session's `self.day` rather than the walk's own instant. So storing the
tick's resolution and reading it back is not a cache of today's answer. It
is a different answer, and §3.4 rules on which one is the creature's.

**Nothing watches this.** `cli/tests/suite/session_cost.rs` bounds a
pooled wall-clock median over ten verbs at 9 ms and records in its own doc
that 20 of the 50 samples exceed the ceiling while the gate passes; it is
`heavy:`-tiered and runs on the canonical box. No deterministic per-turn
budget exists anywhere: `HomeNavCache::searches()` and `Derived`'s hit and
miss counters are read only by examples. The Roll multiplied per-turn work
by an order of magnitude and every gate stayed green.

## 2. What must survive

**2.1 The ledger is the only stored truth** (Penstock §3.1). Nothing this
campaign builds is serialized; every array is re-derivable from the ledger
plus the seed, FRAME-tier like `Occupancy` (decision 0069).

**2.2 A view equals its scan.** Anything the rack holds that *is* a
projection of the ledger — a body's room — must equal the ledger fold at
every read (`position[slot] == agent_position(&ledger, &body, day)`), and
a test asserts it after every kind of turn. Anything the rack holds that
is *not* a projection — a felt state, which is a resolution — is content,
ruled on in §3.4 and pinned by its own test.

**2.3 The roster is append-only and position-stable** (The Roll, decision
0546): a body's slot never moves; `narrate_motion`'s positional zip and
every user-visible handle depend on it.

**2.4 Determinism.** Same seed, same script, byte-identical session goldens
across two runs; no wall clock anywhere in the session; `BTreeMap`/`Vec`
only.

**2.5 One body type** (0229), **possession selects a body** (0227): the
driven body is a slot like any other, which lets this campaign retire the
`driven_affect`/`driven_mode` side-fields into the rack.

## 3. Design

**3.1 The rack.** `windows/vessel/src/roster.rs` owns one struct of arrays:

```text
  pub struct Roster {
      bodies:   Vec<Body>,             // the roll's append-only roster (The Roll)
      keys:     Vec<RollKeyStatic>,    // the static half of the roll key
      on_roll:  Vec<bool>,             // the mask the tick reads
      position: Vec<Facet>,            // a VIEW: the body's room as of the last write
      felt:     Vec<Felt>,             // CONTENT: the body's last resolution (§3.4)
      driven:   usize,
  }
```

with exactly one append, `push(body, key) -> Slot`, which pushes every
array in the same statement so index-alignment is unrepresentable to
break; `Slot(usize)` is a newtype and the only way to index. Every
parallel `Vec` on `Session` today (`bodies`, `roll_keys`, `on_roll`) moves
in; `driven_affect`, `driven_mode` and `driven_suppressed` become the
driven slot's `felt`. `Felt` is `Affect` plus the `Mode` and the suppressed
drives the tick already computes for the driven body — one struct for
every body, the driven body no longer special.

**3.2 The tick writes.** `step_with_occupancy` returns, beside its facts and
occupancy, one `Written { entity, position: Facet, felt: Felt }` per body
it walked — the `st.pos` and `st.affect`/`st.mode`/`st.suppressed` it
already holds at the end of each walk. `wait` writes them into the slots
by entity → slot lookup (a `BTreeMap<EntityId, Slot>` kept beside the
arrays, filled at push). A body not walked this tick keeps its slot as it
was. `refresh_roll_at` (The Roll) appends new bodies with `position =
home` and a `felt` seeded once by the stateless read (§3.4's one surviving
use of `affect_of` on the session path), exactly as turn 0 must seed every
slot before any tick has run.

**3.3 The turn reads.** `snapshot`, `needs`, `colocated_npcs`,
`sensed_npcs`, `narrate_motion`'s before/after and `presence_line` read
`position[slot]` and `felt[slot]`. No `affect_of*` call and no
`agent_position` fold remains on any turn path; `sighting()` is derived
once per turn and shared by `snapshot` and `needs`. A turn's cost over
present bodies becomes a scan of two dense arrays.

**3.4 The felt-state ruling — a creature feels what its own last resolution
felt.** The array holds the tick's resolution: with the alarm field, with
mode hysteresis, with the walk's own belief and frontier, at the walk's own
instant. That is the arbitration that actually moved the creature; the
stateless read was a *re-imagining* of the same body without its own
history, adopted by The Confidant because the tick's value was dropped
before anyone could read it. Between ticks a creature does not re-feel: a
room hop that advances the day by a tenth does not make sixty-seven
bystanders re-integrate their thirst; they feel as of their last step,
which is what they would report if asked. **This is a fidelity choice and
it leads the G3 flagged section.** Consequences a reader must know: `needs`
and `sensed.present.felt` can change wording for the same world (the
session goldens move, which is the positive control for §5); a body that
has never been walked (turn 0, or a newly appended resident) carries the
stateless seed until its first tick; the stateless `affect_of` family stays
for tests, the lab and the seed, with its doc amended to say it is no
longer the session's read.

**3.5 The regression watch — a counted budget, not a clocked one.**
`Session` gains a `TurnWork` counter set, reset at the start of each
`handle` and readable after it: drive folds performed (calls into
`affect_of*`), plan searches (`HomeNavCache::searches` delta), ledger
position folds (`agent_position` calls), shadowcasts (`sighting`
derivations), and bodies scanned. A new commit-tier test,
`windows/vessel/tests/suite/turn_budget.rs`, asserts per verb class at
seed 42's flagship and at a chamber: **`snapshot()` performs 0 folds, 0
searches, 0 position folds**; a chamber `go` performs 0 folds; `look`
derives at most 1 shadowcast; `wait` performs at most `roll_len()` folds.
These are counts of work, deterministic on every box, so the test runs in
the commit gate rather than the heavy tier and cannot flap. The wall-clock
gate in `session_cost.rs` stays as the stage-tier instrument and its doc
gains a pointer to the counted one; its `TURN_BUDGET_MS` and
`INDOOR_SNAPSHOT_BUDGET_MS` are re-pinned from post-campaign readings on
the canonical box, downward.

**3.6 Out of scope.** Invalidation dispatched from the commit (Penstock
§5.7) and `Derived<_, _>`'s `Ledger` validity: the rack has one writer, the
tick, so it needs no dependency keys; if a second writer ever appears that
is the moment the `Ledger` class gets its tenant. The lab health battery's
own reads. Dense storage for anything keyed by `KindId` (already dense) or
for the ledger itself.

## 4. Preregistered measurement

- **P1 — the snapshot budget.** `move_cost` at seed 42's flagship, release,
  ambrose: `snapshot()+json` in the home room **≤ 3 ms** (from 32) and in a
  chamber **≤ 3 ms** (from 44); `needs` **≤ 2 ms** (from 27). A budget, not
  a ratio. Serialising 69 KB of JSON is inside it.
- **P2 — the counted budget.** `turn_budget.rs` green on the commit gate
  with the counts in §3.5, and RED on the pre-campaign tree for `snapshot`
  (its positive control: the counter must read 67 folds before the change).
- **P3 — VIEW ≡ SCAN for position.** Across 64 seeds, after each of
  `look`, `go n`, `wait`, `enter`, `go n` (chamber), every slot's
  `position` equals `agent_position(&ledger, &body, day)`. Zero
  disagreements is the only passing count.
- **P4 — the felt state is the tick's.** After a `wait`, every on-roll
  slot's `felt` equals the `Affect` the walk's `WalkState` held at the end
  of that body's walk (the tick returns both; the test compares them), and
  the driven slot equals what `driven_affect` used to hold.
- **P5 — nothing coarse moves.** No world artifact moves; the session
  goldens and game fixtures DO move (felt wording) — the positive control —
  and the census, queued at close, moves zero columns.
- **P6 — the client feels it.** `clients/game/bin/examples/move_cost.rs`
  under its (now optimised) default profile: every movement turn **≤ 15
  ms** (from 37–81 ms after The Hone).

Decision rules: `make rebaseline` moving anything under `book/src/
laboratory/`, `book/src/domesday/`, an almanac, or `cli/tests/fixtures/
world-seed-42.json` → STOP, an epoch; gallery possession transcripts and
session fixtures moving → expected, commit; a P3 disagreement → the rack's
position writer is wrong, not the fold — fix the writer.

## 5. Capture

- `UNI-ecs-is-the-adaptive-cache` → `elaborated`, Where → this spec; the
  row already says "tick-by-tick position and hysteresis state belong
  there" — this campaign is that sentence built.
- `TOOL-tick-profile-2026-08` Where gains the turn-path numbers above.
- New row `TOOL-turn-budget-counted` (raw → shipped at close): the counted
  per-turn budget as the shape every future performance tripwire takes.
- Decisions, from 0596: a creature's felt state is its own last resolution,
  tick-written; the roster is a struct of arrays with one append, and a
  turn reads arrays; per-turn work is a counted budget in the commit gate,
  the wall clock a stage-tier instrument.

## 6. Definition of Done

Chronicle, retrospective, freshness sweep (the Confidant's chronicle names
`affect_of` as the narration seam — amend), registry flips, the goldens
re-pinned in the commits that drift them, `make game-check` by hand, the
census queued at close, and `move_cost` readings in both clients pasted
into their Measured blocks.
