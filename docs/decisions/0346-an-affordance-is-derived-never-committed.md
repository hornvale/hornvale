# 0346. An affordance is derived, never committed

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (autopilot, spec §11) ·
**Relates:** [0069](0069-fine-position-is-never-serialized.md) (the same
posture, one layer down — `Interior` carries no serde derive at all),
[0348](0348-object-properties-are-keyed-by-kind.md) (the store that makes this
structural rather than disciplinary),
[0074](0074-capacities-are-a-chain-and-facts-gate-on-components.md) ·
[The Offer](../../book/src/chronicle/the-offer.md)

In the context of the first campaign to ask "what can I do to this?" of a
thing in the world, we decided that **the answer is computed at the moment it
is asked and never written to the ledger**, accepting that no saved world
records what any object afforded anyone.

## Context

Arc IV's useful frame is not "objects vs no objects" but *how much state an
affordance's precondition reads*. Every precondition in the tree today reads
position only, and that is enforced rather than incidental:
`action::precondition_reads_committed_state` (`windows/vessel/src/action.rs`)
is an exhaustive match with no wildcard arm returning `false` for every
variant, and its own doc names the case that would end it — a barred door
needing unbarring — because The Threshold's catch-up replay reconstructs a
past that could have happened only while no movement is gated by a committed
effect.

Committing affordances would climb that scale twice over. It would make the
offer a fact whose absence in an old save is indistinguishable from a `false`,
and it would give the replay a committed effect to trip on.

## The rule

`affordance::offered_to_observer(kind, body, knowledge)` is a pure function
over derived state. Nothing it reads is serialized, nothing it produces is
committed, and no world file gains a field. `UNI-21`'s registry row had
already specified this shape for the capability query — *"DERIVED at load …
never in the world save (build-state, not world-state; no cross-version
hazard)"* — before this scale was drawn; IV.a is that row's first real user.

## Consequences

- **A world file written before this campaign loads unchanged, and one written
  after it says nothing new.** The campaign's only save-adjacent motion is the
  language domain's appended accession cohort (decision 0352), which is a
  concept registration, not an affordance.
- **The cost is stated, not hidden:** an affordance cannot depend on anything
  a previous session did. A chest you opened does not stay open, because
  nothing records that you opened it. That is Arc IV.b's rung, together with
  the replay redesign it forces.
- A later campaign wanting affordances in the ledger for *performance* should
  read this record first: the derivation is a `BTreeSet` build over at most
  five properties, and the reason it is not stored is correctness, not cost.
