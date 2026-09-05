# 0729. Recovery grades the committed kind, not a room boolean

**Status:** Accepted (2026-09-04) · **Decider:** Nathan (autopilot) ·
**Campaign:** The Tenon · **Relates:**
[0698](0698-a-kind-is-committable-where-an-anchor-identity-is-not.md)
(the record this now reads),
[0069](0069-fine-position-is-never-serialized.md)
(the boundary it preserves),
[0558](0558-sleep-is-never-gated-the-place-grades-it.md)
(the grade rather than gate)

## Context

The Pallet made a body commit `SLEPT_ON = <kind>` but deliberately left the
recovery fold reading the older room-level `Afforded` boolean. Consequently a
bed and bracken were indistinguishable to recovery even though the ledger could
now say which one the body used.

Old histories and conscious `Action::Rest` bouts contain no `SLEPT_ON` fact.
Deleting the older path would reinterpret them as bare ground.

## Decision

**For a sleep bout, recovery reads the `SLEPT_ON` kind and grades that kind
against the sleeper's traits.** The fold resolves the fact by a second ordered
merge, parallel to its existing merge over position. It records and reads the
stable kind, never an anchor identity.

Where no kind fact exists, the existing room-level `Bare`/`Afforded` resolution
remains. The fallback covers conscious rest and pre-Pallet ledgers; it is a
compatibility rule, not an alternative authoring path.

## Consequences and costs

- The people half of 0558 is live: the place grades recovery by both object kind
  and species kind without serializing fine position.
- The fold builds one object roster and shares it between label resolution and
  grading. It does not rebuild a registry per bout or anchor.
- Locale granularity is narrowed, not abolished. A committed kind distinguishes
  bed from bracken, but not two anchors of the same kind, and conscious rest
  still uses the room-level fallback.
- The fallback is durable complexity. It cannot be removed until old ledgers
  and `Action::Rest` have another explicit record.

## See also

`windows/vessel/src/liveness.rs` (`rest_timeline`, `grade_of`);
`windows/vessel/src/sleep_site.rs` (`SLEPT_ON` producers);
[The Tenon chronicle](../../book/src/chronicle/the-tenon.md).
