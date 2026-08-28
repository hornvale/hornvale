# The Latch — a passage that stays open

**Campaign:** The Latch · **Branch:** `campaign/the-latch` ·
**Decision block:** 0366–0375 (main ceiling 0356 at reservation) ·
**Drafted:** 2026-08-28 · **Status:** Draft, at G3.

**Arc IV.b of The Bridle.** Predecessor: The Offer (IV.a,
`docs/superpowers/specs/2026-08-27-the-offer-design.md`).

*An offer is a thing advertised. A latch is a thing thrown — and a latch, once
thrown, stays thrown. That last clause is a scope line, not a flourish.*

---

## 1. What occasioned it

The Offer put objects on the 30–50% band of its own ordinal axis — *how much
state an affordance's precondition reads*. It advertised verbs derived from
kind-level properties, filtered by the acting body and the observer's
knowledge, and it deliberately stopped short of anything durable. Its
retrospective files the rest as "the durable half of the arc."

This campaign takes the 90% rung: **a precondition that reads committed world
facts.** Scoped, by owner ruling, to exactly one thing — *restricted passage*.

Three pieces of machinery in the tree are already waiting for it, and two of
them say so in their own doc comments:

- **`barrier_of`** (`windows/worldgen/src/character.rs`) draws a
  `BarrierState` — `{Sealed, Warded, Thin, Open}` — per chamber address, seeded
  and pinnable. Its module doc says "dials only, no effects." It has **no
  production consumer at all** outside its own tests
  (`windows/worldgen/tests/suite/stope_variety_probe.rs:107`).
- **`delve_has_two_distinguishable_outcomes`** (`windows/vessel/src/session.rs:7737`)
  guards a verb that used to have three outcomes. The Drift deleted the
  existence coin `chamber_exists` gated on, and sealed caves did not become
  rare — they became **impossible** (874/874, 1681/1681, 1266/1266 open on the
  three panel seeds). Its doc names the restorer by name: *"Restricted passage
  — locked doors, collapses that magic can clear."* Nathan's AMENDMENT B
  accepted two outcomes **until that campaign lands.** This is that campaign.
  The assertion is deliberately two-directional, so it **reddens the moment
  sealed becomes possible again** — this campaign trips it on purpose and
  renames it back to three.
- **The Offer's §3.5 knowledge gate** is correct, tested, mutation-proven and
  wired, and it *cannot deny anything today* — `Session::new` absorbs
  unconditionally. Restricted passage gives it a firing case with no rewiring.

## 2. Where the cut falls, and why the collision is smaller than it looked

The Offer's §2 axis, with this campaign placed on it:

```
  reads     position                          status
  --------------------------------------------------------------
   0%   nothing                            shipped
  10%   position only                      shipped -- ALL of it
  30%   derived object properties          shipped (IV.a)
  50%   the observer's knowledge           shipped, unfired (IV.a)
  70%   the playthrough's daybook          shipped (I.b)
  90%   committed world facts              <- THIS CAMPAIGN, one slice
 100%   another agent's committed mind     far future
```

The handoff into this campaign carried a warning: `action.rs`'s
`precondition_reads_committed_state` is an exhaustive match with zero `true`
arms, its doc says *"A barred door needing unbarring would end it,"* catch-up
replay depends on it, and **redesigning that replay may be a campaign by
itself.** Four verified facts change that estimate.

**(a) The function has no production caller.** `git grep -n` returns the
definition (`action.rs:135`), a test import (`liveness.rs:6117`), one call
(`liveness.rs:14248`, inside `#[cfg(test)] mod tests`), and two doc mentions.
It is a **tripwire asserting an invariant**, not a runtime gate. Catch-up
depends on the invariant, never on the function.

**(b) Catch-up already reads committed state, time-correctly.**
`last_fact_day_at_or_before` re-filters facts to `<= day` at each replay
instant, and its doc explains precisely why a whole-history fold would be
wrong there. "Catch-up cannot read the ledger" is false as stated; it does it
today, for the drive facts.

**(c) Catch-up never replays a room transition.** `is_replayable_in_catch_up`
admits exactly `MoveWithin`; every other intent hits
`Intent::Do(_) => break 'catchup` (`liveness.rs:4662`) — stop rather than
fabricate.

**(d) Decision 0069 forbids durable state in the fine layer *by construction*:**
"entering a room, moving within it, and leaving **cannot** alter the world."

Put (d) beside (c). **0069 pushes durable passage state up to room
granularity, and room granularity is exactly where catch-up already refuses to
replay.** The barred door whose doc says it would end catch-up is only fatal at
*anchor* scale — and 0069 already prohibits that. The two constraints agree
instead of colliding. `barrier_of` is keyed `(vertex, band, branch)` — chamber
address, already room-granular, already 0069-compatible.

### 2.1 The tripwire is stricter than the invariant it protects

It guards `if is_movement(&a)`, which covers `MoveTo` as well as `MoveWithin`.
The invariant catch-up actually needs is over `is_replayable_in_catch_up`.
Narrowing that guard is the whole of the feared "replay redesign."

**Measured, not reasoned** (decision 0353: name the mutation, paste the red).
`action.rs:138`, `Action::MoveTo(_) => false` → `=> true`, match verified unique
by `grep -c` before substituting; `cargo nextest run -p hornvale-vessel
--no-fail-fast`:

```
FAIL [   0.020s] (291/709) hornvale-vessel liveness::tests::no_movement_precondition_depends_on_a_committing_effect
Summary [ 131.970s] 709 tests run: 708 passed, 1 failed, 3 skipped
```

The tripwire fired — a positive control on the guard itself — and exactly one
test in 709 encodes the assumption.

**What that does NOT establish, stated because the distinction is the point.**
Per (a) the function has no production caller, so flipping its return changes
no runtime behaviour at all. The mutation says nothing about whether **gating
movement at runtime** is safe; that is a different change to a different path
and it remains unmeasured. A mutation proves only what it perturbs. Task 1
below exists to measure the thing this one did not.

## 3. The mechanism: state is derived, never stored

Facts key on `EntityId` (`kernel/src/ledger.rs:71`), so "what entity is a
passage?" looks like the blocking question. Precedent dissolves it:
`agent_at_fact(entity, room, day, …)` commits every position in the world with
the **creature** as subject and the **place** as an object `Value`. Places are
values here; they are never entity subjects.

So a passage stores nothing. Its initial state is already a deterministic
function of seed and address, and only the *change* is committed:

```
effective_state(addr, day) =
    if any `passage-cleared` fact names addr with day' <= day  ->  Open
    else                                                        ->  barrier_of(seed, addr)
```

This is the project's own "a world is a seed plus a ledger" model applied to
passage, and it buys four things structurally rather than by discipline:

- **No minted entities.** `mint_instance_of_kind` is untouched; handoff item 4
  defers to IV.c intact.
- **No durable store.** Handoff item 3 defers intact. Nothing new is
  serialized, so there is no new save-format contract and no epoch.
- **No `WorldComponents` join**, which The Offer's §3.1 correctly identified as
  requiring an object domain that does not exist.
- **Monotone.** Open-only, by owner ruling (#5). A monotone fold over
  append-only facts has no latest-wins ordering subtlety, and a replay can only
  ever err in the conservative direction.

`find(PASSAGE_CLEARED)` (`ledger.rs:388`) returns every such fact regardless of
subject, which is what makes "a passage *someone else* cleared" work with no
extra machinery. The subject is therefore **agent-neutral by construction**,
though only the possessed body clears one in this campaign (#4) — NPC clearing
becomes a later flip with no schema change.

The `day' <= day` filter is the same shape `last_fact_day_at_or_before` already
uses. That is not a coincidence to be admired; it is the reason (b) above
matters.

### 3.1 The lifetime is the session, and that is not this campaign's to change

Found while writing the plan, not while reviewing the spec, and it corrects an
earlier draft of §6 that claimed durability across a save.

**Nothing a possession session commits is ever persisted.** The session's
ledger is a clone of the frozen world's, and its own field doc says so:
"a clone of the frozen world's ledger, mutated only by `wait`'s tick (NPC
`agent-at` facts). **Never written back**" (`session.rs:639`). `AGENT_AT` is
likewise "registered per-session, never at genesis" (`session.rs:642`). There
is no world-writing path after genesis anywhere in the CLI. So registry and
ledger are *both* session-scoped, coherently and by design.

The consequence for this campaign is exact and worth stating plainly rather
than burying: **the latch stays thrown for a playthrough, not for a world.**
Every durable-state rung on The Offer's axis inherits this, however correct
its mechanics.

It is not this campaign's to fix. Every fact live play commits already
evaporates the same way; a passage that persisted would be the anomaly, not
the norm, and building world persistence is a serialization change touching
every committed fact — what a saved playthrough contains, whether the
per-session registry must become permanent, and what reload does are all
unscoped. Registered as `MAP-playthrough-persistence` in the idea registry.

What this campaign still delivers is unchanged in mechanism: agent-neutral
facts, contradiction-checked against the registry, folded time-correctly with
`day' <= day`. That is the 90% rung's machinery. Only its lifetime is smaller
than the rung's name suggests.

## 4. Risks

1. **A time-varying room graph vs. pure-function caches.** `RoomMeshMemo` and
   `HomeNavCache` cache reachability. Restricted passage makes reachability
   change over time. `HomeNavCache` already carries "avoid-epoch bookkeeping"
   (`liveness.rs:3038`), so an invalidation pattern exists to follow. **This is
   unverified and is the campaign's likeliest real cost** — likelier than
   replay. Monotonicity helps: the graph only ever improves, so a stale cache
   under-reaches rather than routing through a barrier.
2. **The cap-reached teleport.** `preferred_anchor` -> `route_within` (a purely
   positional A*) -> `Occupancy::place`, which performs no reachability check
   of its own. This is *within*-room and so out of scope by §2's argument, but
   it is the exact shape that would break if a future campaign ever put a
   barrier at anchor scale. Worth a note in that code, not work here.
3. **Tripping a green tripwire on purpose.** Renaming
   `delve_has_two_distinguishable_outcomes` is intended. The risk is renaming
   it *without* restoring the third outcome for real, leaving a
   three-outcome claim that passes on a fixture accident. Its own doc already
   demands a scan of every cave-bearing vertex; keep that shape.
4. **A GENESIS registry change would move `cli/tests/fixtures/world-seed-42.json`.**
   That fixture is a byte-golden: `make rebaseline` never writes it, and
   neither guarding test is in the subfloor roster, so `gate-commit` compiles
   them and never runs them. Only `make rebaseline-goldens`. This bounced The
   Offer from the chamber. **This campaign most likely avoids it entirely** by
   registering `passage-cleared` per-session, following `AGENT_AT`'s own
   precedent (`session.rs:642` — "registered per-session, never at genesis").
   Task 2 must CHECK which it is rather than assume: if the fixture moves, the
   golden is refreshed in that same commit.

## 5. Non-goals

Objects as entities; containers; contents; inventory; take/drop/put; a
name→entity lookup; re-closable passages; NPCs that clear passages; anything at
anchor granularity. All of these are IV.c.

## 6. Acceptance

1. A passage whose `barrier_of` state is not `Open` **refuses** passage, with a
   refusal naming the physical reason, and that refusal is reachable in seed
   42's terrain (scanned, not assumed).
2. An act clears such a passage, commits one fact, and the passage stays open
   for the **rest of that session** — across many turns, including a `wait`
   tick and the NPC activity it drives. This is the durability claim at the
   only lifetime the engine has (§3.1); it is NOT tested across a save
   boundary, because no such boundary exists.
3. The same passage reads **closed** when the fold is evaluated at a day before
   the clearing fact — the time-correctness claim, which is what distinguishes
   this from a mutable flag.
4. `delve` has three distinguishable outcomes again, and the tripwire is renamed
   to say so, still scanning every cave-bearing vertex.
5. The Offer's §3.5 knowledge gate **denies something** — a firing case, shown
   by a test that fails if the gate is removed.
6. Every regression test names the **mutation** it must fail against, and the
   red is pasted in its doc comment (decision 0353). No test is specified by
   the property it should assert.

## 7. Task shape (detail belongs to the plan)

- **Task 1 — measure what §2.1's mutation did not.** Establish, on the real
  movement path, what breaks when a room transition is genuinely gated on
  committed state. This is the campaign's own falsification step and it runs
  **before** the design is built on §2's argument. If it comes back ugly, the
  cut moves and this spec is wrong in its cheapest place.
- Then: the predicate and registry entry; the fold; the refusal; the clearing
  act; the delve outcome restoration; the knowledge-gate firing case; artifacts.

Each task regenerates and commits its own artifacts in its own commit — never a
terminal sweep task (The Offer's retrospective; the drift check is
`git diff --exit-code` and nothing runs it for you). The final task is a sweep
that **asserts the diff is empty**, which is a finding about earlier tasks
rather than routine labour.

**Absorb main at every stage boundary** (`make sluice-stage`). The Offer
absorbed once, at close, 50 commits behind, and conflicted on an aggregate that
must never be text-merged. That was a controller failure, named in its own
retrospective, and it is the single cheapest thing to not repeat.
