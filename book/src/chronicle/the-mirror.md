# The Mirror

A mirror repeats what stands before it, faithfully or not — and nothing in
this repository checked which. This campaign was one commit: an audit of a
deliberate duplicate, and the guard that makes the next drift loud.

## The duplicated crossing

The deep history speaks two units. The bake reasons in years; the ledger
stores standard days; and every read that turns a committed fact back into a
bake year crosses between them (`kernel::Years::DAYS_PER_YEAR`). The
composition root owns the crossing
(`windows/worldgen/src/history_emit.rs`), but `windows/almanac` renders
history prose and cannot depend on worldgen — windows read downward only — so
it keeps a private mirror: its own `bake_year_of_ledger_day`, its own
`present_year`, its own record decoder.

Duplicated on purpose, then. But purpose is not a guard. [The Ballast]
(./the-ballast.md) fixed a day-crossing defect on the *worldgen* side after a
seam-guard survivor exposed it there, and could not audit the mirror from
where it stood — different crate, different copy, outside the seam's scope.
Seam-guard itself could not be pointed at either half: it keys a seam by
function name, and both crates deliberately define functions of the same name,
so any single tag claims call sites its scoped tests cannot see (registry row
`TOOL-seam-guard-path-keyed`, still open). Duplicated-on-purpose logic drifted
silently, because nothing compared the two halves.

## The audit

The audit half of this campaign read every almanac crossing against its
worldgen original and found them all correct as written:

- `record_of` crosses `occ-founded` and `occ-ended` once each, at the read;
- `present_year` crosses in **both** arms — the trusted `history-now` fast
  path and the pre-T8 fallback that approximates the present from the latest
  occupation event (the map is monotone, so crossing once on the winner is
  exact);
- `conquest_victim` crosses the one comparison that needs it and leaves the
  tie-break raw, where the monotone map makes raw correct ([The Ell]
  (./the-ell.md) documented that distinction; the mirror preserves it).

No defect was found. That is a finding, not a filler: the Ballast-era worry
that the mirror might already disagree with its original is retired by
observation, not by argument.

## The guard

The missing comparison now exists, and it lives where both halves are
visible: three tests inside the almanac's own test module, which can see the
private fns, backed by a dev-only dependency on `hornvale-worldgen` — the
precedent [hearsay](./the-hearsay.md) set, dev-only so the runtime layering
edge never exists. On one freshly baked ledger they assert:

1. **Parity**: the mirror's `present_year` equals worldgen's, through both of
   the mirror's arms.
2. **The unit, directly**: parity alone cannot catch identical drift, so the
   present is pinned to its value — bake year 200, not the ledger day 73,050
   a dropped crossing would leak through.
3. **Decoder parity**: the mirror's `record_of` and worldgen's
   `occupation_records` reconstruct the same years from the same facts.

Mutation-checked before landing: dropping the crossing at either site turns
exactly the test that should object red, and no other.

One deliberate cost surfaced at the gate: the new dev-dependency appears in
the enforced layering graph's dev-only column, so the book's authored layering
page was rebaselined (`REBASELINE=1`, diff reviewed — one column, one crate).
That is the enforcer doing its job; a window reaching the composition root in
tests is exactly the kind of fact the page exists to show.
