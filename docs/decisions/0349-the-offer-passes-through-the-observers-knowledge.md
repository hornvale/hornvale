# 0349. The offer passes through the observer's knowledge before it is rendered

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (asked directly, at the
§3.5 stop) · **Relates:**
[0347](0347-an-affordance-is-a-relation-not-a-property.md) (the third argument
of the relation), [0069](0069-fine-position-is-never-serialized.md) (why the
gate is at ROOM granularity and not anchor granularity),
[0259](0259-conceptual-deficiency-is-derived-not-authored.md) ·
[The Offer](../../book/src/chronicle/the-offer.md)

In the context of an object advertising what can be done to it, we decided
that **the advertisement is filtered by what the observer knows, and that this
gate ships wired even though nothing in Arc IV.a can make it deny**, accepting
a live branch with no reachable caller.

## Context

A body that has not encountered a thing should be offered nothing by it. The
spec's first shape gated on knowledge *of the anchor*, and that could not be
built: `Knowledge`'s key shapes are `room/<packed FacetId>` and
`settlement/<id>/<field>`, and an anchor key would have to be an `AnchorId`,
which is positional, never serialized, and unstable across derivations
(decision 0069). So the gate is at **room granularity** — you are offered an
anchor's verbs if you have knowledge of the room it sits in — which uses the
existing key shape and is semantically honest.

**The gate cannot deny anything in IV.a, and this was established before it
shipped rather than discovered after.** `Session::new` calls `absorb_here`
unconditionally before returning; `IdentityProjection::project` takes
`_perception` and never reads it, so darkness cannot suppress absorption;
`enter` descends from an already-absorbed locale. There is no live path on
which a body is offered an object whose room it does not know.

Two alternatives were declined at the stop. Reverting the gate and deferring
§3.5 to IV.b was a clean, isolated undo with zero callers. Manufacturing a
firing case by making absorption conditional on light is a real behaviour
change that moves gallery transcripts — the class of scope IV.a was cut to
avoid.

## The rule

Every surface that reads an offer reads it through `offered_to_observer`,
never through the narrower `offered_by` or `offered_to`. Both production call
sites do: `Session::examine_chamber`'s anchor gate, and `Session::warm`. A
surface wired to a narrower query silently drops the gate, which is why the
choice is stated here rather than left to each caller.

## Consequences

- **This is live code with an unreachable branch, not dead code, and it must
  not be deleted as dead code.** The branch is real — making
  `offered_to_observer` ignore its `known` argument reddens
  `an_unencountered_object_offers_nothing` — and IV.b's durable objects give it
  a firing case with no rewiring.
- **§3.5 reads as delivered and cannot be observed working.** No seed, no
  session and no transcript in IV.a exercises the denying branch. The
  chronicle says so; so does the code.
- This is the seam a *lying* object would plug into. IV.a ships truthful
  advertisement through it and does not ship the lie; naming the seam now is
  what keeps that a one-file change rather than a redesign.
