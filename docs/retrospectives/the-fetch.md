# The Fetch — retrospective

Process, not product. The product is in
[the chronicle](../../book/src/chronicle/the-fetch.md); the campaign rulings and
measured outputs are in [the ledger](../superpowers/ledgers/2026-09-06-the-fetch.md).

## The smallest witness was enough to choose the boundary

The campaign began with a two-room witness rather than a general redesign. It
showed that home and current position each admitted one remembered room while
selecting different rooms, with exactly two direct searches from each anchor.
That separated admission from ranking: changing only the final planner could
not recover a candidate already discarded by the belief fold.

The resulting implementation stayed at the smallest demonstrated boundary.
Memory remains derived from committed visits; geometry is evaluated from the
creature's committed current position. The existing `Option<Facet>` surface
survived, avoiding a wider consumer and serialization change.

## A familiar cache was not evidence for a new cache

The home-keyed `RouteMemo` looked reusable, but the new fold varies by current
position. The probe supplied a deterministic route count — `2 home / 2
current` — not a population bound or a performance budget. Reusing the memo
would therefore have hidden a new position-keyed cache behind an old name.
Keeping direct searches made the ownership boundary explicit and leaves any
cache as a separately measurable follow-up.

## Observer verification constrained the close

The verification record (`38ea7618d`) reports ten belief tests, errand and
affect observers, and seed-42 snapshot observers passing after implementation
`dca20b44c`. The commit gate reported 1,298 passing sub-floor tests. The
canonical stage gate then exposed the intended value change that the local
sub-floor did not exercise: 20 of 410 affect-trace lines moved, all within one
creature's thirst/danger/social arbitration, while the fixture stayed 27,985
bytes. Rebaselining that focused fixture and changing the stranded calibration
to assert recovery made the semantic change explicit rather than hiding it.

## Close boundary

G6 was approved on 2026-09-06. The first main absorption happened at close,
not at a stage boundary; its only conflict was the shared root
`IMPLEMENTATION_PLAN.md`, resolved in favor of main's active Counterpart plan.
The outstanding technical follow-up is a separately designed, bounded
position-keyed route cache only if later measurement warrants one.
