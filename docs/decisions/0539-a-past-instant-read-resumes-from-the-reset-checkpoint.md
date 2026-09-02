# 0539. A past-instant read resumes from the reset checkpoint

**Status:** Accepted (2026-09-02) · **Decider:** Nathan · **Relates:**
[0237](0237-the-reset-event-is-the-checkpoint.md);
[0537](0537-a-reader-never-observes-a-fold-behind-its-ledger.md);
[0540](0540-the-past-day-affect-path-preserves-the-unfiltered-reset.md);
[The Pawl spec](../superpowers/specs/2026-09-01-the-pawl-design.md) §2.4,
§3 rule 5, §12.0

In the context of a fold that only ever advances being asked for its value at
a **past** instant — which the fear path does on the hot path, evaluating each
emitter's affect at the observer's latest-visit day for every visited room —
we decided that such a read **resumes from the last reset at or before the
instant and advances over the trail's range**, making
[0237](0237-the-reset-event-is-the-checkpoint.md) concrete for the tenant that
needs it; and, after the campaign's first readout, that the resumed state is a
**per-reset-partition prefix accumulator** rather than a re-integration from
the reset.

## The two halves, and why the second was added

The first half is 0237's rule applied where the code actually asks for it. A
past read is not a corner case served by a rebuild: it sits inside the most
expensive fold in the stack, and without a checkpoint it is a whole-history
walk per emitter per visited room per tick.

The second half is a correction the campaign's own measurement forced. Stage 1
shipped the checkpoint list alone and re-integrated from the last reset on
every read. For an agent that never resets — 21 of 50 on the instrument, the
decisive probe among them — "the last reset" is genesis, so that read was
still `O(history)`, with a terrain sample per segment. The first readout
measured it as an elasticity of 1.01 and could not attribute it; the mechanism
was named there as a candidate and confirmed by building the accumulator.

The accumulator is a per-`(entity, drive, reset)` partition holding the running
sum after each sighting, restarting at zero at each reset. A read is one
lookup, one linear term to the instant, and one clamp.

## Bit-identity is preserved by preserving the summation order

The accumulator performs **the same additions in the same order** as the loop
it replaces, which is what makes it byte-identical rather than merely
equivalent to within rounding. Floating-point addition is not associative; a
faster summation order would have been a behaviour change wearing an
optimisation's clothes.

## The memo's invariant is one temperature FUNCTION per store, not one terrain

The partition memoises a value that depends on terrain, and the obvious
statement of its safety condition — "one terrain per store" — is **false**:
several call sites construct a fresh terrain view over one store. The
invariant that actually holds is that there is one temperature *function* per
store: the terrain view's temperature reads only the locale context's cached
sample, whose memo hit returns the same integer weights a miss recomputes. The
weaker true statement is the one recorded, because the stronger false one was
written into a doc comment and contradicted by five call sites.

## Consequence

The past read is bounded by the segments since the governing reset rather than
by history, at every instant. The ecological cost of the sustenance reads fell
by more than two orders of magnitude; the synthetic sweep's single-reset column
now sits on top of its periodic one, which is the accumulator's claim expressed
as a measurement.

The cost accepted is memory proportional to the sightings in the live
partitions, and an eviction bound on how many partitions a drive keeps — whose
rate in production is not yet measured.

**See also.** [The Pawl chronicle](../../book/src/chronicle/the-pawl.md);
[The Pawl spec](../superpowers/specs/2026-09-01-the-pawl-design.md) §12.
