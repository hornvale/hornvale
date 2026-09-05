# 0730. The chooser prefers; no-travel preserves the diagnostic

**Status:** Accepted (2026-09-04) · **Decider:** Nathan (autopilot) ·
**Campaign:** The Tenon · **Relates:**
[0698](0698-a-kind-is-committable-where-an-anchor-identity-is-not.md)
(the choice record),
[0558](0558-sleep-is-never-gated-the-place-grades-it.md)
(rough sleep remains possible)

## Context

`select_sleep_site` carried The Pallet's warning that a future chooser must
preserve the body's ability to choose a worse site, and that guaranteeing the
optimum would erase the tuning signal. The Tenon gives sites different values,
so leaving selection unrelated to those values would make the relation inert
at the point of choice.

Within one room, preference and the old warning now conflict literally: an
argmax guarantees the best site among that room's candidates.

## Decision

**The chooser takes the highest-graded sleepable anchor in its current room,
ties going to the lowest `AnchorId`. It never searches another room and never
proposes travel.** Bad sleep remains possible because the world may place no
good site in the room a body reached, not because the chooser ignores its own
preference.

**This explicitly amends `select_sleep_site`'s earlier warning.** The warning is
not reinterpreted as though within-room argmax always satisfied it. Its durable
purpose survives in the no-travel boundary; its literal prohibition on a local
optimum is retired.

## Consequences and costs

- Choice and recovery use the same derived grade, so preference is observable
  in `SLEPT_ON` rather than only in a diagnostic function.
- Determinism is explicit: candidates arrive in ascending `AnchorId` order and
  only a strictly greater `total_cmp` result replaces the incumbent. No float
  enters a sort comparator.
- The chooser can be locally optimal and globally poor. It will take bracken
  here while a bed stands next door, preserving the signal about movement,
  settlement layout, drive order, and surface reachability.
- The cost is deliberate myopia. This decision does not add planning, threat,
  safety, or travel to sleep selection.

## See also

`windows/vessel/src/sleep_site.rs` (`select_sleep_site` and its amended doc);
`windows/vessel/src/liveness.rs` (`sleep_candidates`);
[The Tenon chronicle](../../book/src/chronicle/the-tenon.md).
