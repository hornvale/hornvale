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

### 3.1 The lifetime is the session by default, and a played world is a fork

**THIS SECTION WAS WRONG FOR THE WHOLE CAMPAIGN AND IS CORRECTED HERE, AT THE
DEFINITION-OF-DONE SWEEP.** It is rewritten in place rather than quietly
edited, because the false version was load-bearing: it shaped acceptance
criterion 2, `passage.rs`'s module doc, an idea-registry row, and a decision
record, and every one of those had to be corrected with it. See decision 0368.

**What this section used to say.** *"Nothing a possession session commits is
ever persisted… There is no world-writing path after genesis anywhere in the
CLI. So registry and ledger are both session-scoped, coherently and by
design."*

**One command retired it.** `possess` takes a documented `--out <PATH>`.
`Session::into_played_world` folds the session's evolved ledger **and its
per-session registry** into a `World`, and `--out` saves it. That is The First
Mark's Task 4, and decision 0171 already rules that a player's acts are *not*
filtered on the way out. Measured here on seed 42:
`possess --script 'go n; go n' --out walked.json` writes a world carrying **2
`agent-at` facts** and the `agent-at` predicate in its registry, and
`possess --world walked.json` then starts cleanly (rc=0).

**How the error was made, which is the reusable part.** The `ledger` field's
doc says "a clone of the frozen world's ledger… **Never written back**." That
sentence is true and answers its author's question — *does a session mutate the
world it borrowed?* No; `--world` is read-only and `cli/src/main.rs` says so.
This spec read it as answering a different question — *can these facts ever be
saved at all?* — and the two questions have opposite answers. A doc comment
answers its author's question, not the one a later reader brings to it.

**What is actually true: a played world is a fork, not an update.** Live play
never mutates the world it possessed. Its facts are carried into a *new*
`World` and written only if the player asks. So the default (no `--out`) is
session lifetime, which is the original finding's surviving half and the common
case; with `--out`, the facts persist into a new world file that can be
possessed again.

**What this campaign therefore claims, and what it does not.** The latch is
claimed and proved **for the session** — many turns, a `wait` tick, and the NPC
activity it drives. Its behaviour across a save is **not claimed, because it
was not tested.** The carrying mechanism demonstrably works for `agent-at`, a
sibling predicate committed through the same `Ledger::commit` call on the same
session ledger, and nothing in `passage-cleared`'s handling differs — but no
test drives clear → `--out` → re-possess → delve, so that round trip is left as
work rather than asserted. It is the cheapest real thing this campaign leaves
behind.

The mechanism itself is unchanged by any of this: agent-neutral facts,
contradiction-checked against the registry, folded time-correctly with
`day' <= day`. That is the 90% rung's machinery in full.

**The axis's rungs name what a precondition READS, never how long that state
lives.** Conflating the two is what produced the original error, and any future
rung can make the same conflation.

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
   tick and the NPC activity it drives. **MET.**

   The reason clause attached to this criterion was wrong and is corrected:
   it read "it is NOT tested across a save boundary, because no such boundary
   exists," and a save boundary **does** exist (`possess --out`, §3.1,
   decision 0368). The criterion itself is unchanged and was always the right
   one to assert — the session is the lifetime this campaign built and tested.
   What changes is that not testing the save boundary is now a **scope
   choice**, which is honest, rather than an **impossibility**, which was
   false.
3. The same passage reads **closed** when the fold is evaluated at a day before
   the clearing fact — the time-correctness claim, which is what distinguishes
   this from a mutable flag.
4. `delve` has three distinguishable outcomes again, and the tripwire is renamed
   to say so, still scanning every cave-bearing vertex.
5. ~~The Offer's §3.5 knowledge gate **denies something** — a firing case, shown
   by a test that fails if the gate is removed.~~ **NOT MET, AND NOT MEETABLE
   BY THIS CAMPAIGN. This criterion is left standing with its reason rather
   than deleted** — a criterion quietly dropped tells a successor nothing,
   while an unreachable one tells them exactly where the wall is. Rewritten
   from the original after Task 6, which was dispatched as a task that might
   correctly produce nothing and did.

   **The reason is a type mismatch, not a missing wire.** `offered_to_observer`
   (`windows/vessel/src/affordance.rs:450`) takes an `AnchorKind`, and
   `AnchorKind` (`windows/vessel/src/interior/anchor.rs:20`) is an
   **interior-object** enum — Hearth, Threshold, Bed, Vessel, Screen, Pool,
   Log, Ground, Alcove, Strongbox, HighSeat, Loom, Anvil, Altar. There is no
   cave-mouth variant and no passage variant. A cave mouth is addressed by a
   `Vertex`/`ChamberAddr`, which is a different kind of thing entirely.
   `windows/vessel/src/passage.rs` accordingly contains **zero** references to
   `Knowledge`, `AnchorKind` or `offered_to_observer` — the barred-passage
   mechanism reads ledger facts through `effective_state` and never constructs
   or consults a `Knowledge` value. Nor could it borrow the chamber's own
   anchors: the underground chamber `delve` reaches has no anchor catalogue at
   all, `underground_nouns()` (`session.rs:3014`) returning two hardcoded
   strings.

   **This falsifies decision 0349's own closing prediction**, which said IV.b's
   durable objects would give the gate "a firing case with no rewiring." The
   Latch shipped durable-enough passage state and the gate did not move,
   because the prediction was about *durability* and the obstacle is
   *addressing*. See decision 0369.

   Reaching the gate needs one of two design decisions that belong to the
   owner, not to an implementation step: invent a new `AnchorKind` plus a
   design for what a cave mouth offers, or give cave chambers their own
   anchor/interior system. Registered as `PLAY-passage-has-no-anchor` in the
   idea registry. The Offer already reshaped this gate once on contact with the
   code; a second reshaping is not a thing to do in passing.
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

## 8. Decisions taken during execution

Promoted from the campaign's SDD ledger at the Definition-of-Done sweep.
`.superpowers/sdd/` is git-ignored and dies with the worktree, so the reasoning
lives here or nowhere. Ordered as they were taken.

1. **Imports are trimmed to what each task uses** (pre-flight). Task 2's brief
   listed four symbols only Task 3's code touches. Under
   `clippy --all-targets -D warnings` an unused import is a hard failure, so the
   brief as written could not have passed its own commit gate.
2. **A barred refusal must be textually distinct from the no-cave and
   unrealized-chamber refusals** (pre-flight). The plan's `Sealed` arm reused
   the unrealized-chamber string verbatim, which would have made two of three
   outcomes indistinguishable and could satisfy a renamed three-outcome tripwire
   vacuously — the exact failure §4 risk 3 names. All four refusal strings were
   later verified pairwise distinct by reading source.
3. **The lost registry enumeration is restored in a decision record, not the
   registry cell.** Trimming `MAP-playthrough-persistence` to fit the
   600-character budget discarded three concrete open questions. A registry cell
   is budgeted and a decision record is not, so the enumeration went to 0368 and
   the row keeps the pointer. (At the sweep, all three turned out to be answered
   already — by The First Mark, not by this campaign. See §3.1.)
4. **Only `BarrierState::Thin` yields to `clear`** (Task 5, made from inside the
   code). `Sealed` has no rubble to move; `Warded`'s shipped refusal already
   says "you cannot force it," which a verb that forced it would contradict. The
   split has its own test and its own mutation. Ratified as part of 0367.
5. **The implementer was right to override "do not restructure them"** (Task 4).
   Adding the barrier gate invalidated `find_open_cave_vertex`'s premise for
   five pre-existing tests — seed 42's first cave-bearing vertex is `Warded`.
   That instruction was written against gratuitous refactoring, not against a
   change the gate makes necessary. All five callers assert a *successful*
   descent, so requiring barrier-open as well as chamber-realized is correct for
   every one of them rather than an over-constraint.
6. **The acceptance test belongs in `session.rs`'s own `mod tests`** (Task 5).
   The brief named an integration-test file *and* required a seam reachable only
   from inside the private module. The seam instruction reflects what the code
   permits; the file path did not. No coverage is lost.
7. **Accept the null on acceptance criterion 5** (Task 6). See §6 and 0369.
8. **Do not guess a replacement for the wrong `decision 0131` citation** (Tasks
   4 and 7). Traced instead to `14aa6fbab` (The Glasshouse, 2026-08-14), which
   split `delve_column` out of `delve` when that campaign's terrain epoch put a
   sealed cave under the flagship's starting vertex. No decision covers it, and
   the code now says so.
9. **Two carried minors were fixed rather than deferred** (Task 7):
   `cave_entrance_states` now calls the shared `cave_entrance_addr` constructor,
   and `clear_response`'s `Warded` arm gained a prose test, mutation-checked.
10. **§3.1's premise was tested and found false** (Task 7). The correction is in
    §3.1 and 0368, written loudly rather than edited quietly, because the false
    version was load-bearing in four documents.
