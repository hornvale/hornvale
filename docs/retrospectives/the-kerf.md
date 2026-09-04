# The Kerf — retrospective

Process, not product. The product is in [the chronicle](../../book/src/chronicle/the-kerf.md), the rulings are in [the campaign ledger](../superpowers/ledgers/2026-09-04-the-kerf.md), and the three BEFORE plus three AFTER unpaired observations are in the spec's §11.

## A subtraction still needed a full measurement

Deleting a tenant made the deterministic result unusually crisp: at band 10
the removed `KnownWater` row was exactly 4,665 entries / 247,245 estimated
bytes, and the surviving Trail and LatestVisit rows did not move in three
runs. But K2 and K3 still required three readings with endpoint loads. The
advance probe clustered at 60.98–62.63 ns/fact while the belief probe varied
with a busy Mac. A subtraction is not a license to infer timing behaviour.

## A constant is an instrument with an expiry date

The two Kerf hash values, their reach control, and their sharpness control
were essential while `KnownWater` still existed. After its deletion they would
have frozen unrelated future behaviour to one old walk. The close removed the
asserted values and retained two fresh runs, all non-vacuity floors, and the
independent fold-equals-scan witnesses. The historical values remain beside
the test because provenance is useful; they no longer execute because the
comparison they represented no longer exists.

## Do not generalise a private helper from one occurrence

`rooms_at_where` states one first-visit rule for `rooms_at` and `water_at`.
That is a local repair to duplicated logic, not evidence for a repository-wide
shared-predicate law. The campaign minted decision 0756 for the proven
resident-index criterion and explicitly left 0757 unminted. A future second
site can establish whether a common abstraction has a real boundary.

## Deferred review minors

All four ledgered minors found a committed home. Task 4's two were repaired in
the close commit: `session_length_scaling.rs` now describes the two surviving
tenants and four statements, and `liveness.rs` says that the visit list's first
element supplies the first-visit instant. Task 5's two were repaired in the
final close fix: `the_kerf.rs` now describes its live two-fresh-walk guarantee
and reports both past-instant counts when that two-run floor fails.
