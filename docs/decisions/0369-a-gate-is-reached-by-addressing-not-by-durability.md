# 0369. A gate is reached by addressing, not by durability — 0349's firing case does not arrive

**Status:** Accepted (2026-08-28) · **Decider:** Nathan (autopilot; a
well-evidenced null accepted at the Task 6 stop) · **Relates:**
[0349](0349-the-offer-passes-through-the-observers-knowledge.md) (the
prediction this falsifies),
[0347](0347-an-affordance-is-a-relation-not-a-property.md),
[0348](0348-object-properties-are-keyed-by-kind.md) (the kind-keying that
makes `AnchorKind` the gate's only currency),
[0069](0069-fine-position-is-never-serialized.md) (why the gate sits at room
granularity in the first place) ·
[The Latch](../../book/src/chronicle/the-latch.md)

In the context of The Offer shipping a knowledge gate with no reachable
denying branch, we decided that **restricted passage does not give that gate a
firing case and cannot be made to without a design decision the owner has not
taken**, accepting that acceptance criterion 5 is recorded as unmet rather
than satisfied by construction.

## Context

Decision 0349 shipped `offered_to_observer` wired rather than dormant, on the
explicit understanding that it was live code with an unreachable branch. Its
closing consequence predicted the remedy: *"IV.b's durable objects give it a
firing case with no rewiring."* The Latch is IV.b. It shipped state that
survives a session and reads committed world facts, and **the gate did not
move.**

The prediction was about *durability*. The obstacle is *addressing*, and the
two are unrelated:

- `offered_to_observer` takes an `AnchorKind`.
- `AnchorKind` (`windows/vessel/src/interior/anchor.rs`) is an
  **interior-object** enum: Hearth, Threshold, Bed, Vessel, Screen, Pool, Log,
  Ground, Alcove, Strongbox, HighSeat, Loom, Anvil, Altar. There is no
  cave-mouth variant, no passage variant, and the module it lives in is named
  `interior`.
- A cave mouth is addressed by a `Vertex`/`ChamberAddr`. It is not an anchor
  and never passes through anything that takes an `AnchorKind`.
- Accordingly `windows/vessel/src/passage.rs` contains **zero** references to
  `Knowledge`, `AnchorKind` or `offered_to_observer`. It reads ledger facts
  through `effective_state` and never constructs a `Knowledge` value.
- Nor can it borrow the chamber's anchors: the underground chamber `delve`
  reaches has no anchor catalogue at all — `underground_nouns()` returns two
  hardcoded strings.

## The rule

**A gate's reachability is a property of what it is keyed on, not of how
durable the state behind it is.** Predicting that a future campaign will
"give a gate a firing case" requires checking that the campaign's subject can
be *addressed* in the gate's own currency. 0349's prediction did not, and no
gate, test, or drift check could have caught that — it was a sentence in a
consequence list, and the code was correct throughout.

## Consequences

- **Acceptance criterion 5 is left standing in the spec, struck through, with
  its reason** — not deleted. A criterion quietly dropped tells a successor
  nothing; an unreachable one tells them exactly where the wall is. This
  follows the same instinct as 0349's own insistence that the unreachable
  branch be named in three places.
- **0349 is not superseded.** Its rule (every surface reads offers through the
  knowledge-gated query) is untouched and correct. Only its forecast is
  falsified, and a forecast is not a rule.
- **Reaching the gate needs one of two owner decisions**, neither of which is
  an implementation step: invent a new `AnchorKind` plus a design for what a
  cave mouth *offers*, or give cave chambers their own anchor/interior system.
  The Offer already reshaped this gate once on contact with the code; a second
  reshaping is not a thing to do in passing. Registered as
  `PLAY-passage-has-no-anchor`.
- **The null was produced by a task dispatched as one that might correctly
  produce nothing**, and the three facts bearing on it were handed over
  *without* the conclusion they suggested — so the implementer's agreement is
  evidence rather than an echo. That framing is the reusable part.
