# 0540. The past-day affect path preserves the unfiltered reset, knowingly

**Status:** Accepted (2026-09-02) · **Decider:** Nathan · **Relates:**
[0237](0237-the-reset-event-is-the-checkpoint.md);
[0539](0539-a-past-instant-read-resumes-from-the-reset-checkpoint.md);
[0016](0016-studies-preregister-hypotheses.md);
[The Pawl spec](../superpowers/specs/2026-09-01-the-pawl-design.md) §3 rule 1

In the context of migrating the drive integrals onto folds, facing a
divergence between what [0237](0237-the-reset-event-is-the-checkpoint.md)
says a past read *should* do (resume from the last reset **at or before** the
instant) and what the code *does* (take the latest reset in the ledger, with
no bound on the instant at all), we decided that the migration **preserves
today's unfiltered behaviour exactly**, and that the filtered rule is a
separate, behaviour-changing campaign.

## The divergence is live, and it was measured rather than assumed

The campaign's own witness first reported zero offenders — of zero lookups.
Adding a denominator turned it into a real reading, and then into a different
question: on the live shape the lab uses, **6 of 18** past-day reads have a
reset in their future, so the unfiltered lookup returns a reset the read's own
instant has not reached. That is not an edge case in a test; it is the
production fear path, on any world whose emitter scan finds an emitter.

## Why the physically sensible rule is not applied here

0237's filtered rule is the right one: a creature's thirst at a past day
should not be zeroed by a drink it has not yet taken. Applying it would change
a past-day reading on every emitter-bearing world, which moves derived
quantities the calibration batteries assert against, and this campaign's
headline constraint is byte-identity. A behaviour change smuggled inside a
performance migration is the one thing that would make the migration
unreviewable.

So the fold is given the unfiltered semantics deliberately, and this record
exists so that the next reader finds a **decision** rather than an inheritance.
The distinction matters: an unexamined behaviour carried forward looks exactly
like an examined one in a diff.

## Consequence

The filtered rule is owed, and it is owed as its own campaign with its own
reference refresh, not as a follow-on edit. Until then the past-day affect
reading is what it has always been, and the code says so at the call rather
than leaving a reader to derive it.

The cost accepted is a known physical infelicity, one function call away from
the corrected form, held in place so that a performance campaign can prove it
moved nothing.

**See also.** [The Pawl chronicle](../../book/src/chronicle/the-pawl.md);
[The Pawl retrospective](../retrospectives/the-pawl.md).
