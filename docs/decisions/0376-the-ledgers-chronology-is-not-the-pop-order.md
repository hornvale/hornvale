# 0376. The ledger's chronology is not the scheduler's pop order

**Status:** Accepted (2026-08-28) · **Decider:** Nathan (autopilot, spec §3,
G1 auto-adopted with one overturn) · **Relates:**
[0186](0186-an-instant-is-an-exact-tick-count.md) (`WorldTime` is an exact `i64`, so
the sort key is integer and no float enters the ordering);
[0126](0126-fact-day-is-a-typed-world-time.md) (`Fact.day` is already the
type this sorts on) · [The Precedence](../../book/src/chronicle/the-precedence.md)

In the context of a shared-clock scheduler whose emitted facts were not in
chronological order across creatures, we decided that **the emitted stream is
sorted by day at the point of production, because the queue's pop order is
dispatch order and was never the ledger's chronology — one structure had been
silently serving both purposes.**

## Context

`DriveMovements::step_with_occupancy` pops creatures from a `(ticks, EntityId)`
queue at the moment each one *begins* an action, then stamps and emits every
fact at the instant that action *ends*. Those are different orderings whenever
two creatures' action costs differ, and the discrepancy is bounded by the
population's **cost spread** — never by a tick.

Measured before any code changed:

| probe | result |
| --- | --- |
| `interleaving_fixture(&[4.375, 70.0])` | 0 ticks backward |
| `interleaving_fixture(&[70.0, 4.375])` | **9,925 ticks** backward |
| seed-42 population health (`simulate_world`) | **62 inverted ticks, worst 10,014** |

The fixture mints entity ids in the order its masses are listed, so the list
order *is* the queue's tie-break order. The published fixture happened to hand
the *fast* creature the lower id, so at every tie the cheaper action was
emitted first and the inversion could not appear. Swapping two numbers in a
test fixture was the whole reproduction.

**The invariant that should have caught this had a false rationale.**
`a_faster_creature_acts_more_often_between_a_slower_ones_actions` tolerated
`d >= prev - tick`, justified in-comment as "creatures tied at the same rounded
tick are separated by entity id and their exact `f64` days then differ within
that tick." The divergence is not sub-tick float noise inside a shared tick; it
is the cost spread between two *different actions*. The tolerance had never
been bounding the quantity that actually varies — which is why The Foliot's
attempt to make days exact did not make the tolerance unnecessary, and instead
surfaced a 75-tick violation it could not explain. That campaign deferred the
work after three attempts, attributing the inversion to its own retype.

## The rule

`step_with_occupancy` ends with `out.sort_by_key(|f| f.day);` — stable, and on
`Option<WorldTime>`, which is an exact `i64` comparison with a derived `Ord`
(0186). No float enters the ordering. The monotonicity invariant asserts
`d >= prev` with **no tolerance term**.

Stability is load-bearing twice over: a creature's own facts are already
monotone and stay in place, and cross-entity ties keep the entity-id order the
queue chose, which is what keeps the emitted sequence a pure function of the
frozen ledger rather than of the caller's input vector.

## Why sorting is the root-cause fix and not a patch

Because **the emitted order is the queue's only cross-entity product.** Every
`occupancy` access in `liveness.rs` is keyed by the creature's own entity;
perception (`alarm`, hazard memory, belief seeding) is built from `frozen`
before anyone moves. Verified rather than argued: reversing the queue's
tie-break — a completely different pop order — leaves the seed-42 affect traces
bit-identical, and leaves the emitted fact multiset (keyed on mass, predicate,
tick, object and provenance) unchanged.

Correcting the one place a conflation is observable is correcting the
conflation. The codebase already agreed: `decide_step` folds `frozen` plus this
tick's in-flight `out` and calls `sightings.sort_by(day)` for exactly this
reason, three functions up. The scheduler simply never did the same for its own
output.

Named honestly, this is **windowed event-time reordering**: a tick is a closed
window and `to` is its watermark. That precondition is asserted by
`every_emitted_fact_is_dated_inside_the_tick_that_emitted_it`, written *before*
the sort so it could refute the design.

## Alternatives rejected

- **Widen the tolerance to the cost spread.** Rejected: it would enshrine a
  false rationale and grow silently as the mass band widens.
- **Stamp facts at action start.** Rejected: semantically false — a creature
  has not arrived when it sets out — and it changes the meaning of every
  committed fact's day, a save-format event bought for nothing.
- **A begin/complete event queue popping at completion time.** Deferred, not
  dismissed. This is the discrete-event-simulation canonical structure and is
  the right architecture *once creatures observe each other mid-tick*. Today
  they provably do not, so it would move every committed trajectory to fix a
  property nothing can observe. G1's ideonomy pass overturned an initial
  recommendation for it on exactly this ground.
- **Emit into a day-keyed map**, making sortedness structural. Rejected as
  disproportionate: `out` is threaded as `&[Fact]` through three functions.

## What retires this decision

**The moment any creature observes another's mid-tick state** — a cross-entity
`occupancy` read, collision, mid-tick perception — pop order becomes
semantically load-bearing and sorting the output stops being sufficient. The
begin/complete queue is then required.

`the_queues_tie_break_decides_nothing_but_order` is the tripwire. It asserts
that reversing the queue's tie-break changes *what happened* not at all, and it
will fail, loudly and on purpose, on the first change that makes pop order
matter. Its key covers five of `Fact`'s six fields; the sixth, `place`, is
hard-coded `None` at all four emission sites, so the key is complete for
today's roster — a fifth predicate, a `Value::Number` object, or a populated
`place` would reopen it.

## Consequences

- The committed fact stream is chronological across creatures for the first
  time. No committed artifact moved: `make rebaseline` plus the full
  `docs/generated-paths.txt` drift check came back empty, and the census's
  affect traces are bit-identical.
- `TOOL-liveness-accumulates-f64-days` is unblocked. Its blocker was this
  defect, not the retype it was attributed to.
- A campaign-scale lesson worth more than the fix: **a fixture's own
  construction masked a live defect through a whole campaign.** Three attempts
  reasoned about the failure; one measurement in the other arrangement settled
  it in minutes.
