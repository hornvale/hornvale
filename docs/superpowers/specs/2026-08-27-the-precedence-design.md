# The Precedence — the ledger's chronology is not the scheduler's pop order

**Campaign:** The Precedence · **Date:** 2026-08-27 · **Status:** DRAFT — awaiting G3

The Foliot deferred `TOOL-liveness-accumulates-f64-days` after three attempts.
Retyping `WalkState.day` from `f64` to `WorldTime` passed 442 of 444 vessel
tests and then broke the shared-clock monotonicity invariant by 75 ticks — an
ordering inversion, not rounding. Three attempts to isolate it failed.

This campaign began by refusing to retype anything, and instrumented the
scheduler instead. **The inversion is not caused by the retype. It is a
pre-existing defect that the published fixture happens not to witness.**

## 1. The diagnosis

`interleaving_fixture` mints entity ids in the order its masses are listed, so
the list order *is* the queue's tie-break order. On unmodified main, with the
`f64` accumulation fully intact and nothing retyped, swapping that order is
enough:

```
interleaving_fixture(&[4.375, 70.0])   worst backward jump =     0 ticks
interleaving_fixture(&[70.0, 4.375])   worst backward jump = 9,925 ticks
```

The invariant's tolerance is **one** tick.

The published fixture hands the *fast* creature the lower entity id, so at
every tie the cheaper action is emitted first and the inversion never shows.
Nothing about `f64` was protecting it.

### The mechanism

**The queue orders creatures by when an action *begins*. Every fact is stamped
and emitted at the instant that action *ends*.** Those are different orderings
whenever two creatures' action costs differ. Traced, two pops at one tick:

```
POP  t=566667  e=336  mass=70     -> EMIT agent-at at 576667   (MoveTo, cost 10000)
POP  t=566667  e=337  mass=4.375  -> EMIT rested   at 566742   (Rest,   cost    75)
```

Both correct individually; 9,925 ticks out of order on emission. The
discrepancy is bounded by the population's **cost spread**, never by a tick.
Nothing sorts the emitted vector.

The very first pair of pops in that same trace emits `rested` at 100150 then
100075 — **a 75-tick inversion, the exact magnitude The Foliot hit**,
reproducible today with no retype at all.

### It reaches real worlds

Run against the census's own population-health path (`simulate_world`, seed
42, real settlement NPCs plus wild beasts):

```
ticks carrying an inversion   62
worst backward jump           10,014 ticks  (~2.4 h of world time)
distribution                  34x 55, 12x 10014, 6x 23, 4x 3363, 2x 9861, 2x 5195, 2x 195
```

### What the invariant's comment claims, and why it is false

`a_faster_creature_acts_more_often_between_a_slower_ones_actions` tolerates
`d >= prev - tick`, justified in-comment as "creatures tied at the same rounded
tick are separated by entity id and their exact `f64` days then differ within
that tick."

The divergence is not sub-tick float noise inside a shared tick. It is the cost
spread between two *different actions*. **The tolerance was sized for a
phenomenon that is not the one occurring**, which is why making days exact did
not make it unnecessary — it had never been bounding that quantity.

## 2. What is actually wrong

The defect is a **purpose conflation**, not an arithmetic one.

Pop order exists to answer *who acts next*. The emitted fact stream exists to
be a ledger — *what happened, and when*. One data structure was silently
serving both, and the two orderings coincide only when every creature's
actions cost the same.

Two measurements bound the claim, both taken rather than reasoned:

**Pop order determines nothing observable except emission order.** Every
`occupancy` access in `liveness.rs` is keyed by `npc.entity` — the creature's
own entry; there is no cross-entity read. Perception (`alarm`,
`hazard_memory`, belief seeding) is built from `frozen` *before* anyone moves.
Verified empirically: reversing the queue's tie-break (`u64::MAX - id`, a
completely different pop order) leaves the seed-42 affect traces
**bit-identical**.

**The in-flight vector is read mid-tick, and that read is already
order-insensitive.** `decide_step` folds `frozen` plus this tick's own `out`
to re-derive the thirst integral — but it filters to `f.subject ==
npc.entity` and then calls `sightings.sort_by(day)`. **The codebase already
solves this exact problem, on this exact data, three functions up.** The
scheduler simply never did the same for its own output.

## 3. The design

**Make the emitted stream chronological where it is produced, by a stable sort
on day, and stop pretending pop order was ever the chronology.**

```rust
// in DriveMovements::step_with_occupancy, immediately before returning:
out.sort_by(|a, b| day_of(a).total_cmp(&day_of(b)));
```

Stable, and by day alone: per-entity order is already monotone and therefore
preserved, and cross-entity ties keep the entity-id order the queue chose. The
result is a total, deterministic order that is a pure function of the frozen
ledger — which is what `the_emission_order_is_independent_of_the_input_order`
already demands.

### Why this is the root-cause fix and not a patch

The symptom patch would be widening the invariant's tolerance to the cost
spread. That is explicitly rejected: it would enshrine the false rationale and
grow silently as the mass band widens.

Sorting is not "patching the output" — **the emitted order *is* the
scheduler's only cross-entity product** (measured, §2). Correcting the one
place a conflation is observable is correcting the conflation.

Named honestly: this is **windowed event-time reordering**. A tick is a closed
window and `to_ticks` is its watermark; reordering inside a window is correct
exactly when no later event can be dated before an already-emitted one. That
precondition is what makes the fix sound, so §5 asserts it as a test rather
than assuming it.

### Alternatives considered

| option | verdict |
| --- | --- |
| **Stamp facts at action start** | Rejected — semantically false (a creature has not arrived when it sets out), and it changes the meaning of every committed fact's day. A save-format event bought for nothing. |
| **Begin/complete event queue** (pop at completion time) | Deferred, with a named trigger. This is the discrete-event-simulation canonical structure and is the right architecture *once creatures observe each other mid-tick*. Today they provably do not, so it would move every committed trajectory to fix a property nothing can see. |
| **Emit into a day-keyed map** (sortedness structural, not a step) | Rejected as disproportionate — `out` is threaded as `&[Fact]` through `advance_one`, `catch_up` and `decide_step`, so changing its type is invasive for no measured gain over the sort. |
| **Enforce non-decreasing days in `Ledger::commit`** | Not the fix — but adopted as a *guard* (§5), scoped to a test rather than kernel runtime enforcement. |

### The trigger that retires this decision

The sort is correct because creatures are mutually blind within a tick. **The
moment any creature observes another's mid-tick state — shared occupancy reads,
collision, mid-tick perception — pop order becomes semantically load-bearing
and the begin/complete queue becomes necessary.** Recorded here so a future
campaign meets a stated condition rather than rediscovering it.

## 4. What this unblocks

With emissions chronological by construction, the monotonicity invariant
cannot fail *whatever type `st.day` has*. That removes The Foliot's blocker.

`TOOL-liveness-accumulates-f64-days` is then a genuine retype, and the seam
gets placed on a principle rather than where the compiler stopped complaining:

**Every *instant* in the walk becomes `WorldTime`; continuous physical
integrals keep `f64` and convert at their own call sites through the kernel's
named hatch.**

That means `WalkState.day` **and** its siblings `last_drank`, `last_rested`,
`last_ate` — all four are instants, and The Foliot's partial move (day only) is
precisely what created the boundary that made the retype feel unbounded. The
four fact helpers (`agent_at_fact`, `drank_fact`, `rested_fact`, `eaten_fact`)
take `WorldTime`, which is what `Fact.day` already is (decision 0126). The
thirst/fatigue integrals stay continuous and say so at the crossing.

## 5. Guards

Three, each stating the direction it enforces.

1. **Strict monotonicity, no tolerance.** The invariant becomes
   `d >= prev`, and its comment is rewritten to say why no tolerance is
   needed. The one-tick allowance is deleted, not widened.
2. **The window precondition.** A test asserting every fact emitted by a tick
   is dated within `[from, to]` — the watermark the sort's correctness rests
   on. Enforces *emitted ⊆ window*; it is blind to a fact the walk declined to
   emit, and says so.

   **This one is unverified and load-bearing, so it is written FIRST in stage
   1, before the sort.** The sort is justified by this property; I have read
   the interval guard (`st.day > self.to` returns false before emitting) but
   have not measured it, and `hold_step`'s and `next_awake_day`'s jumps are
   not obviously bounded by it. If the test goes red, the sort's justification
   is wrong and §3 must be re-spec'd — that is a stage-1 stop condition, not a
   test to be relaxed until it passes.
3. **The tie-break is not load-bearing.** A test that reversing the queue's
   entity-id tie-break leaves the emitted fact *multiset* and the resulting
   affect traces unchanged. This is the property §2 measured, promoted from a
   throwaway probe to a standing assertion — it is what will fail, loudly and
   on purpose, when the trigger in §3 arrives.

Guard 3 is the one worth the most: it converts the precondition of this whole
design into something that cannot rot silently.

## 6. Stages

**Stage 1 — the ordering fix.** The sort, the three guards, the corrected
invariant comment. Self-contained and independently valuable; lands
mid-campaign so the later stages do not pay its absorption toll three times
(The Foliot's own lesson).

**Stage 2 — prove the retype is unblocked, with a stop condition.** Apply the
retype as a throwaway spike on top of stage 1 and run the full vessel suite.
*If the monotonicity invariant still fails, STOP and re-spec* — the premise of
stage 3 is false and no amount of stage-3 work will find that out. A fix task
cannot audit its own premise (The Foliot, generalised).

**Stage 3 — the retype.** All four `WalkState` instants, the four fact
helpers, the call sites. Only if stage 2 is clean.

## 7. Measured cost, and what is NOT yet measured

Taken on 672edba22, one process, both arms:

```
                    ticks_with_inversion   worst_ticks   affect-trace hash
sort = false                62               10,014      adb21c8768eb26c5
sort = true                  0                    0      adb21c8768eb26c5
tie-break reversed           0                    0      adb21c8768eb26c5
```

Vessel suite 675 tests, 0 failures. Lab suite 490 tests, 0 failures.

**The census's affect traces are bit-identical** — same rendered length
(32,587) and same hash — so the fix removes every inversion and moves no
census value on this path.

**What that does not establish, stated rather than buried:** it is one seed and
one path. The authoritative checks are `make rebaseline` plus the
`docs/generated-paths.txt` drift list, and a census refresh on lefford. Stage 1
runs the first; the second is a close-of-campaign act on the canonical box and
is *not* claimed here. Committed session fixtures under
`windows/vessel/tests/fixtures/` pass unchanged, but only `REBASELINE=1`
writes them, so an unexpected move there is a finding and not a rebaseline.

## 8. Success criteria

Absolutes, phrased so they are checkable by counting rather than by
recollection:

1. Zero inverted ticks on the seed-42 population-health path (from 62).
2. The monotonicity invariant asserts `d >= prev` with **no** tolerance term
   anywhere in the file.
3. Both mass orders of `interleaving_fixture` are green — the fixture no
   longer depends on which creature holds the lower entity id.
4. `make rebaseline` plus the `docs/generated-paths.txt` drift check is run,
   and its result is **adjudicated by branch rather than predicted**. I have
   not run it, so this spec states responses, not an expectation:
   - **nothing moves** -> commit as-is;
   - **only `docs/audits/` moves** -> the type-audit report drifting on a
     `pub` signature change (stage 3); regenerate and commit in the SAME
     commit;
   - **`book/src/domesday/` or a census CSV moves** -> STOP. That is a census
     value moving, which §7 measured as bit-identical; a contradiction of a
     measurement is a finding, not a rebaseline;
   - **`windows/vessel/tests/fixtures/` moves** -> STOP. Only `REBASELINE=1`
     writes those, deliberately; an unexpected move is a finding;
   - **anything else moves** -> STOP and name it before regenerating.
5. Full vessel and lab suites green — run per crate, not inferred from
   `gate-commit`, which executes the sub-floor tier only.

## 9. Out of scope

- The begin/complete event queue (§3 trigger).
- `SubstrateField::at`'s bare float day — the same defect one layer along,
  already carried as a Foliot follow-up.
- `TOOL-hold-step-progress-lost-to-round-to-nearest` — independent, real, and
  worth fixing on its own; it is not on this path and folding it in would
  confuse two findings.
