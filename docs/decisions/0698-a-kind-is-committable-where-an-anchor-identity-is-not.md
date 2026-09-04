# 0698. A kind is committable where an anchor identity is not

**Status:** Accepted (2026-09-03) · **Decider:** Nathan (autopilot) ·
**Campaign:** The Pallet · **Relates:**
[0069](0069-fine-position-is-never-serialized.md) (the boundary this reads),
[0010](0010-predicate-schema-value-kind-enforced.md) (why a second attribute
takes a second predicate),
[0558](0558-sleep-is-never-gated-the-place-grades-it.md) (the grade the record
makes observable)

**Recorded as its own decision rather than folded into
[0697](0697-what-an-afforded-site-is-worth-is-a-property-of-the-sleeper.md)
because it is a reusable reading of a constitutional boundary, not a detail of
one campaign.** Every future campaign that wants to record *what a body
interacted with inside a room* meets exactly this question, and 0069 alone
reads as a flat prohibition on answering it.

## Context

[0069](0069-fine-position-is-never-serialized.md) makes an entity's persisted
position its **room**; anything finer "exists only inside the presence bubble
and is never serialized", and `windows/vessel/src/interior/anchor.rs` says the
same in its own words — "an anchor has no coordinate, and its identity within a
room is positional, not persisted."

So *"the body slept at anchor 3"* is unrecordable by constitutional design. Read
as far as that, 0069 appears to forbid recording anything about the furniture a
body used, which would leave [0558](0558-sleep-is-never-gated-the-place-grades-it.md)'s
grade permanently unobservable in the ledger.

## Decision

**The kind is recordable; the anchor is not.** An `Anchor` is
`{ kind: KindId, within }`, and a `KindId` is a **registered concept and a
stable string** — not a position, not an index into a bubble that will not
exist after the tick. So `slept-on = bed` is fully 0069-legal, while
`slept-at = anchor 3` is not, and the line between them is *identity within a
room* versus *membership of a registered class*.

The Pallet exercises it with a new predicate, `SLEPT_ON`, carrying
`Value::Text(kind)`. It is **additive**: `SLEPT`'s own object (the span) is
untouched, so no save-format contract moves and no world file changes meaning.
Predicates here are single-valued, so a second attribute takes a second
predicate rather than a compound object.

## `place` is `None`, and that is the second half of the ruling

The spec asked for `place: Some(room)` and the codebase cannot express it.
**Rooms have no `EntityId`**: they are `Facet`s encoded as `Value::Text`, and
every committed `place: Some(...)` in the workspace names a settlement,
community or person — `Fact.place`'s own doc is "the entity where this fact was
observed", and a room is not one.

Filling it with a derived id (`thing_id(room, kind, 0)`, which *is* 0069-legal —
`thing_role`'s own doc says it encodes the room and the kind, never the anchor
index) was rejected on three grounds of increasing weight:

1. the room is already recoverable by the mechanism the code already uses —
   `rest_timeline` reads a bout's site off the `AGENT_AT` timeline by day, and a
   `SLEPT_ON` fact joins the same way;
2. the derived id is an opaque hash, so the room cannot be recovered **from
   it** — it is not merely redundant, it is inert;
3. decisively, `thing_role`'s spelling is a **declared save-format contract**
   ("a change is an epoch, not an edit"), and spending it on a field carrying
   nothing recoverable binds any future real place-encoding to an epoch.

**A locative-sounding fact with no `place` is a deliberate ruling and its
reasoning lives at the constant, not only here** — the next reader will assume
an oversight.

## Consequences

- **`windows/historiography` narrates it for free**, because it replays any
  entity's facts against the registry's predicate docs. Recording a *number*
  would have bought nothing on that surface; recording a kind buys it with no
  code.
- **A body that slept on bare ground commits nothing.** Absence is the record
  for the road, which is the world's normal case rather than an omission to
  backfill.
- **Nothing reads the predicate yet, deliberately.** Grading a body's outcome on
  *which* kind it found is the `per-people` rung
  [0697](0697-what-an-afforded-site-is-worth-is-a-property-of-the-sleeper.md)
  defers. A fact written and not yet read is correct here.
- **The grade in the recovery fold is still room-level**, and committing the
  kind is what makes changing that possible later without touching 0069.
- **A new registered predicate must be registered at every site that builds its
  own registry**, and the rule "does this scenario actually commit it" cannot
  decide a site whose answer is a function of the world — see the idea-registry
  row on `run_simulation`'s swallowed commit errors, which is how a missed
  fourth registration would have stayed invisible.

## See also

`windows/vessel/src/liveness.rs` (`SLEPT_ON`'s own doc and `slept_on_fact`);
`windows/vessel/src/sleep_site.rs` (`select_sleep_site`);
[The Pallet chronicle](../../book/src/chronicle/the-pallet.md).
