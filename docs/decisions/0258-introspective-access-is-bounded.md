# 0258. Introspective access is bounded — a creature cannot perceive what its own arbitration suppressed

**Status:** Accepted (2026-08-25) · **Decider:** Nathan · **Relates:**
[0256](0256-a-hosts-testimony-is-fallible-by-construction.md) (the deliverable
this is half of),
[0226](0226-a-possessed-host-is-co-present-not-displaced.md),
[0228](0228-a-controller-is-a-parameter-of-the-tick.md);
[The Confidant](../../book/src/chronicle/the-confidant.md)

In the context of arbitration ranking every drive and keeping only the winner,
we decided that **a creature reports its dominant drive and is blind to the
ranks arbitration discarded** — accepting that the discarded ranks are now
retained and readable by the instrument while remaining unreadable by the
creature itself.

## Context

Arbitration already computes the whole ordering and throws away everything but
the winner. That discarded residue *is* the cognitive gap: the thirst a body is
also feeling while it walks toward warmth is real, computed, and — for the
creature — not there. Nothing had to be invented for the gap to exist; it had
to be **named as a stage** rather than left as an implementation detail, so
that the difference between "what the sim knows about you" and "what you know
about yourself" is a thing with a location in the pipeline.

The alternative shape was to model introspection as a *lexicon* gap — a fifth
`GapReason` variant for a state the creature cannot access. That was rejected.
The two residues have different causes and different remedies: a lexical gap is
a property of the culture and moves when the culture's exposures move; a
cognitive gap is a property of the mind and would not move if the culture
acquired every word in the world. Collapsing them into one vocabulary would
make the instrument unable to separate the two things it exists to separate.

## Consequences

- **The filter is a stage before the lexicon**, not a kind of lexical gap. No
  new `GapReason` variant was added, and that restraint had a second payoff:
  two sites re-derive the proto-root universe rule independently, and adding a
  variant would have silently diverged them.
- **`Resolution.suppressed: Vec<DriveKind>`** (`windows/vessel/src/liveness.rs`)
  and `Session::suppressed_drives` (`session.rs`) retain the residue as a
  parallel, additive read. Behaviour is untouched: intent, mode and affect are
  computed exactly as before, and artifact drift is clean.
- **It is a vector, not a scalar.** A real body suppresses more than one drive
  at once — `[Thermal, Fatigue]` was observed at a single checkpoint — so no
  consumer may assume a single suppressed drive.
- **Proved by widening, not by narrowing.** Dropping the exclusion of the
  pursued kind from the filter leaks the pursued drive into the suppressed list
  and reddens. A test that only reddens when the filter is *removed* would pass
  against a filter that excluded everything.
- **A limit recorded rather than papered over:** `suppressed` is threaded
  through the live `Session` path only, not the stateless affect snapshot the
  Laboratory's health metric and `Session::needs` use. Any consumer that needs
  the stateless shape must thread it, and that work was not in this campaign's
  plan.

## See also

Spec §3.1 and §3.3(3); the residue's first reader is the verb `ask`.
