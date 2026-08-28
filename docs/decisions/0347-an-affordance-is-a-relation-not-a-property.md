# 0347. An affordance is a relation between an object and a body, not a property of either

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (autopilot, spec §11,
§3.4) · **Relates:**
[0346](0346-an-affordance-is-derived-never-committed.md),
[0348](0348-object-properties-are-keyed-by-kind.md),
[0349](0349-the-offer-passes-through-the-observers-knowledge.md) (the third
argument the relation takes) · `MAP-19` (Gibson) ·
[The Offer](../../book/src/chronicle/the-offer.md)

In the context of deciding where a verb's availability lives, we decided that
**an affordance is a relation over (object, body, observer) and is stored on
none of the three**, accepting that the query must be re-derived per ask
rather than read off a field.

## Context

`MAP-19` states the Gibsonian half directly: *"a supporter to a sprite is not
one to a giant."* The tempting model is a field — `Anchor.affords:
Vec<Verb>` — and it is wrong in a way that is invisible until a second body
asks. A bed does not *have* rest; a bed affords rest **to a body it can
hold**. The same anchor, unchanged, answers differently to two bodies in the
same room on the same tick.

The Tackle gave the possessed body `mass_kg` for the clock's `tempo`. IV.a is
its second consumer, and the first that makes the object/body distinction
visible in play.

## The rule

```
offered_by(kind)                    = verbs whose required properties the kind carries
offered_to(kind, body)              = offered_by(kind) ∩ what this BODY can do
offered_to_observer(kind, body, kn) = offered_to(kind, body), or ∅ if unknown
```

`SupportsRest` is the property that carries body-relativity in IV.a, and it is
the only one: `body_can_use` is a private predicate in
`windows/vessel/src/affordance.rs` and its restriction to one property is
asserted, not assumed.

**Body-relativity is ADDITIVE in IV.a, never restrictive.** A bed offering
rest is a *new* place to rest alongside the existing at-home precondition;
nothing a body could do yesterday becomes refused today. The canonical
Gibsonian case that would violate this — a body-relative `AffordsPassage`,
Gibson's own aperture experiment, with `SeamKind::{Narrow, Broad}` already
shipped in `interior/seam.rs` — was named and declined, because newly
*blocking* traversal changes where creatures can go, moves gallery
transcripts, and perturbs NPC pathing. That belongs with IV.b's restricted
passage.

## Consequences

- No struct in the tree gains an affordance field, on either side of the
  relation. A reviewer seeing one proposed should cite this record.
- The additive-only constraint is a **property of IV.a**, not of affordances
  in general, and it is asserted by a test that mutation-proves the direction
  rather than by prose. A restrictive body gate is IV.b's to introduce
  deliberately, with the transcript movement priced.
- The one-property scope is a floor, not a ceiling: a second body-relative
  property is a data change, and the test that pins today's scope is the
  thing a future campaign edits on purpose.
