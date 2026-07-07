# Campaign 4b: Emergent Society

**July 2026 · 8 commits · outcome: complete, merged — Campaign 4 (The
People) closes with a society that grows from the land it stands on**

## What was attempted

Campaign 4a placed people on the map — a scatter of named, populated
settlements sited by real fields — but left every one of them, flagship
included, wearing the same tier-0 furniture: a fixed caste ladder, copied
verbatim regardless of biome, coast, wealth, or danger. Campaign 4b asks the
question 4a deliberately deferred: what does a settlement's *society* look
like when it is derived from its environment instead of authored once for
every world? It closes with the fixed ladder gone, a subsistence mode and an
emergent role structure standing in its place on the flagship, a Census of
Peoples answering the questions nobody could answer before this campaign
existed, and the two residues 4a's own retrospective flagged — a dead
tier-0 Vale generator and a vestigial `located-in` predicate — cleaned up
before either could rot further.

## What landed

**Subsistence, an exact function of biome and coast.** A new `hornvale-culture`
module classifies every settlement's biome into one of five coarse classes —
forest, grassland, arid, cold, barren — mapped once at the composition root
so culture still imports no domain, and crosses that class with a single
boolean (does the cell touch the ocean) to produce one of four subsistence
modes: farming, herding, fishing, foraging. Forest and grassland farm
regardless of coast; arid, cold, and barren settlements forage or herd
inland, but a coast rescues a marginal hinterland into fishing. The mapping
is deliberately exact, not fit or tuned, so the Lab can calibrate it — the
sociology sibling of Campaign 3c's band-count calibration.

**Structure, grown from surplus, scale, and threat — not copied.** A second
function takes an `EnvSummary` (subsistence, surplus, population, threat,
all assembled at the root from terrain and climate fields already
computed) and returns an ordered role list: the subsistence worker and
`chief` always appear; `warrior` above a threat threshold, `artisan` and
`slave` above surplus-and-scale thresholds together, `shaman` above surplus
alone. A lean forager camp grows two rungs; a rich, populous farm town grows
the same five rungs tier 0 gave every settlement unconditionally — the
difference is that now they are earned. Tier-1 genesis commits one
`subsistence` fact and the resulting `has-caste` facts per settlement,
replacing the fixed-array genesis outright; the predicate vocabulary the
almanac and REPL already queried is unchanged, so nothing downstream needed
to learn a new shape, only a new source for the same shape.

**The enrichment thesis, proven end to end.** The astronomy-to-biome cascade
Campaign 3c demonstrated now reaches all the way to who a settlement's chief
rules over. Seed 7's flagship, under a spinning sky, nets out a shrubland
hinterland and a coast: **Ishugish**, population 463, subsistence fishing,
structure `[fisher, chief]` — a lean two-rung hamlet. Pin the same seed's
sky to tidally locked and the flagship relocates entirely, without a single
line of culture code changing: **Bolzagug**, population 397, subsistence
farming, in temperate forest, structure `[slave, farmer, artisan, shaman,
chief]` — the full stratified ladder. Nothing about culture's rules moved;
only the sky did, and subsistence *and* structure reorganized together. This
is the enrichment thesis (spec §7) at its most direct test yet: upstream
richness (a different rotation regime) produces legible downstream
difference (a different economy and a different society), verified by a
dedicated cascade test (`locked_rotation_changes_the_flagship_cascade`), not
asserted by prose alone.

**The Census of Peoples, the campaign's third census.** Seven new metrics —
settlement count, mean population, the flagship's subsistence, biome,
coastal access, and structure size, and endorheic coverage — join the Lab's
unified registry alongside the fourteen sky and seven land metrics already
there. Run once at author time over 10,000 worlds
(`studies/census-of-peoples.study.json`), the census confirms the
calibration holds exactly and surfaces three numbers nobody could answer
before it existed: mean settlement count (57.7, strongly correlated with
Study 002's habitable fraction at +0.89), mean population per world (a
tight 353.8 despite wide per-settlement variance), and — the most striking
finding the exactness itself produced — **not one flagship across 10,000
worlds forages or herds**, because the placement suitability formula always
sends the flagship to a coastal cell, and a coast always rescues a marginal
verdict into fishing. Full analysis: [Study 003, the Census of
Peoples](../laboratory/study-003.md). `census-lands-drift`, the 500-seed
CI-checked drift study, now carries all seven new metrics alongside the
sky and land halves it already tracked, unchanged in shape.

**Settlement pins, and two residues finally cleaned.** `--min-suitability F`
overrides the placement floor, persisted as a round-trippable
`settlement-pin` scenario fact, mirroring the terrain and sky pin pattern
even though settlements themselves are never reconstructed from it (they
persist as committed facts, not re-derived fields). Two minor items 4a's own
retrospective flagged as carried debt are gone: the dead tier-0 Vale
generator in `hornvale-terrain` (unreachable since 4a's scatter replaced it,
never deleted until now) and the vestigial `located-in` predicate/field in
`hornvale-settlement` (never load-bearing once each settlement became its
own place entity with its own `cell-id`/`latitude`/`longitude`).

## What was learned

- **An exact function can still be a real sociology.** Subsistence needed no
  simulation, no drawn parameters, and no external calibration data — a
  five-way biome lookup crossed with one boolean reproduces the intuitive
  pattern (farms in fertile hinterlands, fishing villages on marginal
  coasts) and stays checkable row by row against the census, exactly as
  climate's band-count step function did a campaign earlier.
- **Exactness surfaces findings nobody authored on purpose.** Nobody wrote
  "flagships never herd or forage" anywhere in `hornvale-culture` — it falls
  out of composing an exact subsistence function with an already-existing
  placement formula that happens to always prefer the coast. The census is
  what makes an emergent consequence like that visible instead of merely
  true.
- **A tier-0 residue left in place is a maintenance cost, not a neutral
  no-op.** The dead Vale generator and the vestigial `located-in` field cost
  nothing to leave alone in 4a, but every campaign that touched
  `hornvale-terrain` or `hornvale-settlement` afterward would have had to
  reason about code that could never run. Naming carried debt in a
  retrospective and then actually paying it down, task one of the very next
  campaign, kept it from ever compounding.

## Deferred, deliberately (spec §14)

Culture stays goblin-only: the role vocabulary is restructured, not
replaced, and a second species — with the psychology substrate a real
inter-species contrast would need — waits for Year 2. Only the flagship
settlement runs tier-1 culture genesis; every other placed settlement is
named, populated, and located but carries no committed society yet, a scope
limit of the composition root rather than a claim that the rest of the
scatter has no culture worth modeling. Inter-settlement politics, trade, and
cultural diffusion are untouched. Structure is static — computed once at
genesis, never revised as population or environment would later drift — and
the flagship-selection pin override (letting an experimenter choose which
settlement becomes the showpiece rather than accepting the suitability
argmax) is explicitly deferred as showpiece-only, not needed by the Census
or any exit criterion. Embark — a walk-around interior for a habitable cell
— remains the standing local-refinement seam terrain has kept open since
Campaign 3.

## Artifacts

[Study 003: The Census of Peoples](../laboratory/study-003.md) — the
subsistence-biome calibration and the settlement-count, population,
structure-size, and endorheic-coverage numbers, over 10,000 worlds. The
almanac's People section (`book/src/gallery/almanac-*.md`) now names the
flagship's subsistence and its role structure alongside the population and
biome lines Campaign 4a already added; the concept registry gains
`subsistence` and `settlement-pin`.
