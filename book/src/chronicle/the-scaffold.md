# The Scaffold

**August 2026 · outcome: merged — a bake-only handle stops wearing `EntityId`'s
type, and a site's stratigraphy stops resting on mint order**

## What was attempted

The history bake's occupation records carried scaffolding it never took
down: `Bake::mint`'s private counter handed out values typed as `EntityId` —
the ledger's permanent, saved-forever primary key — for handles that live
only for the duration of one bake call and are remapped at emit. A sibling
campaign had already shown what that costs: ninety new persons, appended
last specifically to avoid disturbing anything, still shifted a live NPC's
entity id and reddened a save-format-class fixture with no content change at
all. The Scaffold clears the ground for **The Signet**, the campaign that
will fix identity's derivation, by first removing the type confusion and the
mint-order tie-break that would otherwise mask what The Signet changes.

Three read helpers — `occupations_at`, `occupations_by_cell`, and the
almanac's independent duplicate decoder `layers_at` — broke same-site,
same-epoch ties on the reconstructed occupation's own entity id: a fact
about *write order*, not about the world. Founding days quantize to 25-year
epochs, so `(site, founded)` collides 239 times in seed 42 alone — about 13%
of occupations share a site and a day with another record.

## What shipped

**`BakeId`** is now its own newtype, counter-derived exactly as before (the
type changes; the derivation does not — that is The Signet's move to make,
deliberately deferred). `Bake::mint` returns `BakeId`; `bake_to_ledger`
becomes `BTreeMap<BakeId, EntityId>`, honest about what it translates.
`Occupation` holds the eleven fields both the bake and the ledger side
agree on; `BakeOccupation` adds `community`/`lineage`/`Founding<BakeId>`/
`Ended<BakeId>`; the ledger-reconstructed `OccupationRecord` — kept under its
existing public name, since `hornvale_worldgen::occupation_records` is
consumed outside the crate — drops both fields it never legitimately had and
adds `id: EntityId`. The workspace enforces the split as a type error:
passing one handle where the other belongs does not compile, verified by
deliberately trying it once.

**`layer_key`** replaces the mint-order tie-break with material facts:
`(founded ASC, ended ASC with a still-living occupation sorting LAST, peak
population DESC, founded_from)`. A layer that closed earlier lies deeper,
which is what a stratigraphy *is*. The fourth key is measured to be
necessary, not a defensive fallback: six same-site records in seed 42 (four
in seed 7, zero in seed 1000) tie on every other material fact — same
epoch, same fate, same size, same cause, same tech, same function, same
notability — and differ only in who founded them. `layer_key` lives in
`domains/history` once, and both `windows/worldgen` and the almanac's
independently-duplicated decoder now call the same function rather than
carrying their own copies — one less thing the two crates' documented
lockstep contract has to hold by hand.

**M1 — what fraction of multi-occupation sites change layer order —
reproduced its pre-implementation measurement exactly**, on both decoder
paths independently: 299 sites / 19 changed (6.4%) at seed 42, 341/6 (1.8%)
at seed 7, 302/13 (4.3%) at seed 1000. `occupations_by_cell` and the
almanac's `layers_at` agreed at every seed — the two decoders were not
already out of lockstep. Mint order, it turns out, was deciding far less
than 239 tied pairs implied: `founded` alone separates most layers, and the
tie-break only bites where a site saw two events in one epoch *and* those
events differ in end date or size. The palimpsest had been mostly right by
accident.

**No committed fact moved.** `cli/tests/fixtures/world-seed-42.json` is
byte-identical and `lens_purity` stayed green throughout — the comparators
live in read helpers, never in anything that commits. `make rebaseline`
moved exactly one artifact, `docs/audits/type-audit-report.md` (the expected
count deltas from the new `BakeId`/`Occupation` split), plus the timings
ledger's own record of the run.

**Three of the four gallery artifacts the spec's own verification section
predicted would move, did not — and that needed tracing, not acceptance.**
`history-seed-42.md`'s hardcoded showcase site (cell 28414) turned out to be
one of the 280 *unaffected* seed-42 sites, not one of the 19 reordered ones —
its twelve real layers happen to be the case where founding order and
material order coincide throughout. `vestige-seed-42.png`'s per-cell pixel
comes from `most_dread`, which picks the highest-`dread` layer and breaks
ties by first occurrence; a second probe rebuilt every one of seed 42's 19
reordered sites under both orderings and found the winning layer identical
at all 19 — order matters to that render only through an exact `dread` tie,
and none of the reordered sites produced one. `vestige-seed-42.md` is static
caption prose with no per-cell data. The spec had also predicted
`almanac-seed-42*.md` would move; it does not, and the spec was simply wrong
about the path. `layers_at` is private and reached only by `render_site`,
which only `hornvale history --site` calls — the almanac document itself
routes through `vestige_lines_from`, which is count-based and therefore
order-blind.

**No heavy-tier artifact moves either, and the campaign's first answer about
why was wrong twice over.** A draft of this chronicle called
`book/src/laboratory/generated/the-history/summary.md` a census-gated artifact
awaiting `scripts/census-run.sh`. It is neither census-gated nor awaiting
anything. It is written by the *heavy tier* —
`cli/tests/history_battery.rs`, as `scripts/heavy-run.sh`'s own header
states — so the refresh it would need is `make heavy-remote`, not a census;
the census path runs exactly two studies, `the-census` and
`census-of-the-meeting`, and never touches that directory. And it needs no
refresh at all: every value it prints is an order-independent aggregation.
`stratigraphy` reads `is-occupation` facts straight off the ledger into a map
keyed by cell and accumulates counts and summed peak population, never once
calling the comparator this campaign replaced. A reordering cannot move a
sum. The same holds for the other two heavy-tier authoring tests, which do
not consume the reordered helpers at all.

That correction is worth stating rather than quietly fixing, because the
wrong version was the *plausible* one: an artifact under
`book/src/laboratory/generated/` that a campaign touching history did not
regenerate looks exactly like a deferred census. Which mechanism authors a
given artifact is a fact about the build, checkable in one grep, and this
campaign twice preferred inference to the grep.

## What it leaves reserved

**The Signet** is next: it changes *how* a `BakeId`/community identity is
computed (plausibly an ancestry hash) without touching what this campaign
built. A community that founds a colony and immediately raids it in the
same epoch — the sole cause of the residual ancestry-key collisions this
campaign's measurement surfaced — stays an open idea for its own campaign
to resolve. The spec's own §8 flags
a partial order this campaign still returns as a total one: two layers tied
on every material fact but ancestry are described in sequence when
"simultaneous" would be truer, recorded there as a candidate for whichever
campaign next touches the almanac's history rendering rather than solved
here. A handle-confusion cousin survives at
`windows/vessel/src/session.rs:693` (an `AgentId`'s raw `u64` forced into an
`EntityId`) and a type-audit tagging convention gap on standard *days*
(tagged `bare-ok(count)` where decision 0028 wants `pending`) both move to
the idea registry rather than staying in this campaign's scratch.
