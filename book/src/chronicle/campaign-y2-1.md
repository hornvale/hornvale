# Campaign Y2-1: The Peoples

**July 2026 · 14 commits · outcome: complete, merged — Campaign 14 in the
new sequence (decision 0017), the first spine campaign of Year 2's second
act, gives the world a second people**

## What was attempted

Year 1 closed with five domains at tier 1 and Campaign Y2-0 re-baselined
every census on firm ground, but every one of those worlds still held
exactly one people. Goblin's psychology was never named because there was
only one psychology to name — culture's role vocabulary, settlement's name
syllables, and religion's priesthood check were all goblin by omission, not
by design. The Peoples set out to give the world a second species, kobolds,
and — the harder half of the brief — the substrate underneath both that
makes the difference *legible* rather than a second copy of the same code
with different strings pasted in. The deliverable was one almanac showing a
goblin village and a kobold warren on the same globe, visibly different in
where they sit and how they organize, for reasons a player could be told.

## What landed

**Species are data; the social grammar is code.** `domains/species` is a
new kernel-only crate holding nothing but authored definitions: a species is
a point in a closed six-dimension psychology vector — four `[0,1]` scalars
(threat response, deliberation latency, in-group radius, time horizon) and
two enumerations (sociality, status basis) — never a table of its own rules.
The alternative tried on paper and refused was a per-species
`(role, condition)` table living in data: configuration at first glance,
a bespoke interpreter for an undesigned language by the second or third
species. Culture's structure function stays exactly the one reviewed
function it was; it grows a `PsychSummary` parameter instead of forking.

**The identity trick.** Goblin sits at every scalar's exact midpoint and
every enum's goblin variant, and every downstream formula that reads the
vector is *constructed*, not merely tuned, to reproduce today's behavior at
that point — `0.45 × (0.5 + time_horizon)`, `0.50 × (1.5 − threat_response)`,
the same pattern throughout. This is the mechanism, not just the claim,
behind the campaign's keystone test: a world pinned to `--species goblin`
must be a byte-identical **superset** of the pre-species main — the same
almanac text, the same facts under every pre-existing predicate, new
information appearing only under new predicates. Kobold's six numbers are
authored, not measured: the D&D 5E System Reference Document stands in as
decades of playtested lore, translated by hand into the vector with a
one-line derivation per value — cowardly in the open field but entrenched
behind a warren's traps (threat response 0.8), insular packs with loyalty
that does not extend past them (in-group radius 0.2), trap-cunning esteemed
over raw dominance (status basis: knowledge, not rank). The same "models
author, dice roll" discipline that already governs the frontier map's other
authoring work.

**The superset contract caught a real bug.** Building the keystone test
(Task 8) found that species genesis, run *before* culture and religion
genesis, shifted every subsequent entity id by exactly the new species
count — the first mismatch was `EntityId(62)` against the fixture's
`EntityId(60)`, off by two. The fix was one reorder — species entities now
mint after settlement, culture, and religion have already claimed their ids
— not a change to the test. It is exactly the outcome a pre-committed
byte-identity fixture exists to produce: a real regression, caught before
merge, fixed by moving code instead of loosening the assertion. A second,
unrelated finding surfaced in the same task: `serde_json` 1.0.150 can
round-trip a float one ULP off its own parse (`...91ea` vs `...91eb`) —
Rust's own `str::parse` inverts it correctly — worked around by comparing
worlds through the same save/load path the CLI actually uses, not an
in-memory shortcut.

**Joint greedy placement, and the divergences it measures.** Every
habitable site is now scored once per species, with that species'
psychology-derived suitability weights, into one shared ranked list; a
single spacing pass accepts sites greedily regardless of which species
scored them, so goblin and kobold compete for literally the same land.
Over the 10,000-seed census (Study 006): goblin flagships are coastal in
100.0% of worlds that place one, kobold flagships in 85.3% — kobold's
wider in-group radius and steadier temperament tolerate inland sites
goblin's weights discount. The two role ladders diverge on shape as much
as location: goblin's dominant shape (a stratified `slave, farmer, artisan,
shaman, chief`) covers 68.7% of worlds; kobold's dominant shape
(`digger, shaper, keeper, elders`) covers 76.1%, and no kobold structure
ever grows a slave rung or tops out anywhere but `elders` — the communal,
knowledge-based status basis the vector authors makes stratification by
rank structurally unavailable, not merely rare.

**Competitive exclusion: the campaign's unauthored discovery.** Nothing in
the spec or the plan asked joint placement to produce worlds where one
species claims every viable site and the other places nothing. It does,
in both directions, at comparable rates: kobolds win all of a world's
settlements and goblins place zero in 25 of 10,000 seeds (0.25%); goblins
win all and kobolds place zero in 20 (0.20%); three worlds have no
habitable land for either. The existing pinned calibration only checked
the kobold-excluded direction — a world where goblins are excluded had
never once failed a test, because nothing had ever looked from that side.
Absorbed as a discovery about the mechanism (two independent suitability
rankings sharing one spacing pass can, on the right terrain, favor one
species almost everywhere), not chased as a defect to retune away. A
second, quieter reversal rode the same mechanism: Study 003's finding that
Campaign Y2-0's placement fix let 66 goblin flagships place inland — the
first appearance of herding and foraging in any census — fully undoes
itself once kobolds compete for those same marginal inland sites; the
two-species re-baseline shows zero goblin herding or foraging flagships,
not because the subsistence function changed, but because competition
moved which species stands where the function's inputs would have favored
either.

**The book, kept honest as it went.** `book/src/domains/species.md` opened
the campaign, written before any code existed, in the book-driven-
development pattern culture.md and settlement.md already used. The census
work (Study 006) found and fixed a real bug in `write_csv` itself — the
new comma-joined role-ladder columns were not RFC-4180 quoted, corrupting
row structure — the same "an instrument exposes what it measures" lesson
Year 1's calibration family already taught, now caught by a metric rather
than a formula.

## What was learned

- **A byte-identity fixture is worth what it catches, not what it
  guarantees.** The superset contract's value was never "the goblin path
  is unchanged" — that was the design's intent from the first paragraph of
  the spec — it was catching the one place intent and implementation
  diverged (entity-mint order) before it reached main, at the cost of one
  committed fixture pair and one dedicated test.
- **Independent suitability rankings sharing one spacing pass is a richer
  mechanism than "two villages, different sky."** Competitive exclusion was
  not designed, is not a bug, and was only visible because a census asked a
  symmetric question of an asymmetric-looking calibration. The Year-1
  lesson about censuses surfacing unauthored combinations (pantheon
  verticality and cult form, Campaign 5) generalizes past single-domain
  metrics into cross-species dynamics nobody wrote a formula for directly.
- **Report evidence needs independent verification, not implementer
  self-report, at every haiku task boundary** — the process rule this
  campaign ratified after its third fabricated gate claim. See the
  retrospective for the full account.

## Deferred, deliberately (spec §1)

Perception and observer-dependent salience (kobold nocturnality is banked
in the model card, in prose, with no vector slot yet — The Eyes' job); a
generated tongue to delete this campaign's vocabulary and syllable
stopgaps (The Tongues); per-species and per-individual drawn variation in
place of one authored point per people; a physiology dimension, so habitat
affinity and temperance stay shared rather than species-derived; religion
stays goblin-flagship-only — `SocietySummary`'s priesthood check is a
literal `"shaman"` string match, which will silently fail to recognize a
kobold `"keeper"` caste the day religion goes two-species, a debt named
here rather than discovered later; inter-species politics, trade, and
conflict; and, past two, however many further peoples the registry is
asked to hold.

## Artifacts

[Study 006: The Census of Peoples II, Two Peoples](../laboratory/study-006.md)
— the three preregistered calibrations confirmed at 10k scale, and
competitive exclusion measured in both directions. [The Species
chapter](../domains/species.md) — the closed vector, the SRD authoring
method, the kobold model card. The re-baselined seed-42 artifacts and the
`census-lands-drift` sample carrying ten new per-species and pantheon-shape
metrics (registry now 43). `cli/tests/species_identity.rs` — the superset
contract, CI-enforced on every build.
