# 0348. Object properties are keyed by kind; instance state is Arc IV.b

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (autopilot, spec §11,
§3.1) · **Relates:**
[0346](0346-an-affordance-is-derived-never-committed.md) (what this makes
structural), [0347](0347-an-affordance-is-a-relation-not-a-property.md),
[0069](0069-fine-position-is-never-serialized.md) ·
[The Offer](../../book/src/chronicle/the-offer.md)

In the context of needing somewhere to put the property vocabulary a verb
gates on, we decided that **properties live in a vessel-local
`ComponentStore<AnchorKind, ObjectTraits>` keyed by KIND**, accepting that
IV.a cannot express "this particular chest is locked" at all.

## Context

Two shapes were on the table and one of them is disqualifying.

**Rejected: `fn affordances(AnchorKind) -> &[Verb]`.** Smaller, and it *is*
the verb×object table the acceptance test forbids — one refactor from being
written as such, and it would pass a grep while being the thing the grep
exists to catch.

**Rejected: ledger facts per object instance.** That is the 90% rung of the
precondition scale, and it is IV.b together with the catch-up replay redesign
it forces.

A correction the spec made to its own earlier draft is part of what this
record settles. That draft said the store is *"joined into `WorldComponents`
exactly as `MaterialTraits` is."* It is not, and should not be for IV.a:
joining requires a `KindId` key and a domain that owns object kinds, and no
object domain exists — so joining means *creating* one. `AnchorKind` already
derives `Ord`, so a vessel-local store needs no kernel, domain, or worldgen
change at all.

## The rule

Properties are **kind-level build-state**. The store's key type is
`AnchorKind`; there is nowhere in it to put per-instance state. That is what
makes decision 0346 structural rather than disciplinary: IV.a *cannot*
accidentally climb the precondition scale, because the data structure has no
slot to climb into.

## Consequences

- **The M+N economy is real and small:** seven of fourteen anchor kinds carry
  a property, five properties gate five verbs, and a new kind or a new verb
  edits exactly one table.
- **Two anchors of the same kind are indistinguishable to the offer.** Every
  alcove reveals its contents; no alcove can be the locked one. IV.b's
  mintable object entities (`mint_instance_of_kind`, whose only production
  caller today mints species collectives) are where that changes.
- `MaterialTraits` (`domains/terrain`) is the model for the *shape* — a thin,
  honest, kind-keyed trait table whose own doc calls its field set "thin and
  honest" — and not for the placement: it is joined into `WorldComponents`
  and this one deliberately is not.
