# Campaign 4: The People — Design

**Date:** 2026-07-06
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-05-hornvale-longterm-plan-design.md` (Constitution §2 governs)
**Provenance:** The fourth Year-1 campaign (settlement + social structure). Follows
Campaign 3 (The Land), which shipped the coarse tectonic globe, a climate, and a
land+marine biome field with a habitability mask. C4 puts people on that globe.

---

## 1. Goal

Give the world **people**: settlements placed on the coarse globe by field logic,
a generated flagship settlement whose **social structure emerges from its
environment**, and the first genuine cross-domain enrichment — settlement reads
climate reads astronomy. The deliverable is a **populated, queryable globe**: a
settlement map plus a flagship whose subsistence and castes are legibly a
product of where it sits. The hand-placed Vale retires; the world now populates
itself.

Walkable interiors (embark), multi-species societies, and inter-settlement
politics are explicitly **later** campaigns. C4 builds the coarse social tier on
the coarse physical tier — "coarse constrains fine," one layer up.

## 2. Design principles

1. **People-first at cell scale (Constitution Principle 2).** A settlement is
   generated at the Geosphere cell scale (subcontinental, ~10k cells): a
   habitable cell hosts a settlement whose placement and character follow from
   the fields already computed. Walk-around interiors are a later refinement
   campaign; the habitability mask C3 built is the seam this consumes.
2. **Derive from the land wherever possible.** Placement, subsistence, and social
   structure draw almost nothing of their own: habitability, freshwater, coast,
   temperature, moisture, drainage, and unrest — all upstream — set where people
   live and how they organize. Upstream richness produces legible downstream
   difference (the enrichment thesis, tested here for the first time at depth).
3. **Cheap-real, verifiable.** Subsistence-from-biome and structure-from-surplus
   are rule tables with Earth as the answer key (ecology→society patterns), the
   same discipline that made astronomy and land trustworthy — not agent-based
   social simulation.
4. **Layering holds.** `domains/settlement` and `domains/culture` depend on
   `hornvale-kernel` and nothing else. All cross-domain values reach them as bare
   kernel types, assembled at the composition root. Adding tier-1 people edits no
   other domain (except terrain gains a drainage field — a refinement within its
   own tier).
5. **Balanced depth (Constitution structural rule).** With astronomy and land at
   tier 1, people rise to tier 1 too; no domain leads another by more than ~2
   tiers. This is itself the argument for taking People next rather than
   deepening the land further.

## 3. Architecture

**Two domains deepen to tier 1; terrain gains one field.**

- `domains/terrain` gains a **drainage field**: a deterministic flow-accumulation
  pass over the Geosphere (elevation-derived, no new seed streams), plus
  endorheic-basin detection. A refinement within terrain's existing tier-1
  interface; drift-checked like the rest of the globe.
- `domains/settlement` becomes the **placement provider**: given per-cell
  suitability inputs, it places a scatter of settlements (with minimum spacing)
  and names/populates each; it identifies the single highest-suitability cell as
  the **flagship**. Kernel-only: it receives bare per-cell data, never a terrain
  or climate type.
- `domains/culture` becomes the **social-structure provider**: given a
  settlement's environmental summary (subsistence inputs, scale, surplus,
  threat), it derives an emergent caste/role structure. Kernel-only.

**The composition root (`windows/worldgen`) is the only place they meet.** It
reconstructs terrain + climate, assembles each habitable cell's suitability
inputs and each settlement's environmental summary as bare data, and drives the
cascade: terrain + climate → `settlement::place` → flagship place entity →
`culture::structure(flagship)` → `religion` (tier-0, from phenomena at the
flagship). Dataflow stays acyclic; adding the land's people edits no existing
domain interface.

**The Vale retires.** The tier-0 hand-placed Vale genesis is removed. Each placed
settlement mints a **place entity tagged with its `CellId`** (and latitude/
longitude), so "a place to stand" is now a generated cell. `hornvale_terrain::
places` reports generated settlements' home cells. The tier-0 `biome`/`is-place`
predicates remain registered (a settlement's home cell still has a biome — read
from the field, not hand-authored).

## 4. The drainage model (`domains/terrain`, refinement)

- **Flow accumulation.** Each land cell drains to its lowest neighbor (ties break
  to the lower `CellId`, deterministic). Accumulate upstream cell count (unit
  area per cell) downhill → `drainage: CellMap<f64>`. Ocean cells are sinks.
- **Endorheic basins.** A land cell whose downhill path terminates at an interior
  local minimum (never reaching the sea) is flagged endorheic — the banked
  Dead-Sea/Great-Salt-Lake hook (spec C3 §15). Coarse flag only; no lake
  geometry.
- **Freshwater availability** (a derived convenience, computed at the root, not
  stored): high drainage (a major waterway cell) OR coastal adjacency OR high
  moisture.

**Model card.**
- *Drawn:* nothing (pure function of elevation + adjacency).
- *Derived:* per-cell drainage (flow accumulation), endorheic flags, freshwater
  availability.
- *Approximated (declared):* single lowest-neighbor drainage direction (no flow
  splitting), unit-area accumulation (no precipitation weighting — a later tier
  could weight by moisture), no sub-cell river geometry, no lake filling.

## 5. Settlement placement (`domains/settlement`, tier 1)

- **Suitability, per habitable cell.** A bounded score from habitability (gate) +
  freshwater availability + coastal access + climate temperance, minus hostility
  (extreme aridity, extreme unrest). Bare inputs, assembled at the root.
- **Placement.** Deterministic greedy pass: rank cells by suitability descending
  (ties by `CellId`); place a settlement at the top cell, then skip any cell
  within a **minimum spherical spacing** of an already-placed settlement; repeat
  down to a suitability floor. Yields tens of settlements on a typical globe,
  spread rather than clumped.
- **Per settlement.** A generated name (the existing goblin syllable generator,
  tier-0 mechanism retained) and a **population drawn against carrying capacity**
  (suitability-scaled, replacing the flat 40–80). Each mints a place entity
  tagged with its `CellId`.
- **The flagship.** The single highest-suitability cell — the Vale's generated
  successor and the subject of the deep culture/religion cascade.

**Model card.**
- *Drawn:* per-settlement name and population (against carrying capacity).
- *Derived:* suitability, the placement set, the flagship.
- *Approximated (declared):* greedy spacing (not a settlement-competition model),
  static present-day placement (no founding history — a later historiography
  tier), one flagship gets social depth (the rest are placed-but-shallow this
  campaign).

## 6. Social structure (`domains/culture`, tier 1)

Emergent, subsistence-anchored. Derived per (deep) settlement from its
environmental summary.

- **Subsistence mode** from biome/climate: **farming** (temperate/tropical
  forest, grassland, savanna, watered), **herding** (arid/steppe/tundra/marginal
  grassland), **fishing** (productive coast / upwelling / high drainage outlet),
  **foraging** (cold or arid margins with no surplus).
- **Surplus** = fertility (biome productivity) × water availability. **Scale** =
  population. **Threat** = frontier isolation + unrest.
- **Caste/role structure** falls out of (subsistence, surplus, scale, threat): a
  fertile, watered, populous farm town differentiates into priest/artisan/
  ruler/laborer strata; an arid herder camp stays lean and warrior-weighted; a
  coastal town grows fishers and traders. The fixed 5-caste ladder is replaced by
  a derived role list that **varies by environment**.

**Model card.**
- *Drawn:* minimal (optional small local-variation term).
- *Derived:* subsistence mode, surplus/scale/threat, the role structure.
- *Approximated (declared):* rule-table sociology (not agent-based), single
  species (goblin — multi-species is the Year-2 psychology-substrate campaign),
  no inter-settlement politics/trade networks, static structure (no social
  history).

## 7. The enrichment cascade (the thesis, tested at depth)

The first real cross-domain depth test: settlement **suitability** reads climate
(habitability, subsistence viability) which reads astronomy (insolation,
seasons); the flagship's **structure** reads its biome which reads the sky. A
**cascade test** perturbs astronomy — tidal-lock the world, or shift obliquity —
and asserts the settlement map and the flagship's subsistence and role structure
reorganize while staying internally consistent. This proves the enrichment thesis
one campaign before religion cashes it (C5).

## 8. Integration with the existing cascade

- The Vale genesis is removed; the flagship is the new "place to stand" for
  settlement/culture/religion. Religion stays tier-0 (consumes phenomena at the
  flagship) and is untouched by C4 — C5 deepens it. The Year-1 exit demo
  (astronomy → religion) now runs on a *generated, environmentally-shaped*
  flagship, strengthening it.
- Tiered providers coexist: the coarse people layer is generated for every world.
  A future embark campaign refines a chosen settlement's cell into a walkable
  interior.

## 9. Pins and the exit demo

- **Settlement pins** (join the astronomy and terrain pins): a settlement-density
  / suitability-floor pin, and a flagship-selection override (e.g. pick the
  flagship by latitude band) for showpiece worlds. Persisted as scenario facts,
  round-tripping like every pin; stream-consumption isolation tested.
- **Exit demo:** the same land+seed under different skies. Showpiece pair — seed
  42 spinning (a temperate farming flagship, settlements clustered in the wet
  bands) vs. tidally-locked (settlements ringing the habitable terminator, the
  flagship a fishing or herding society) — the settlement map and social
  structure reorganize from the same land.
- **Artifact:** a deterministic **settlement map** (the biome map overlaid with
  settlement marks; flagship distinguished), committed and drift-checked; and the
  almanac's generated settlement section.

## 10. The Lab: a Census of Peoples

New metrics registered (the runner never changes): settlement count, mean/median
population, subsistence-mode distribution, dominant caste-structure class,
endorheic-basin coverage, flagship subsistence. **Calibration (built in):**
subsistence mode must equal the known function of biome (farming↔forest/
grassland, fishing↔productive coast, etc.) — validated the day it ships, the
sibling of the band-count and belief-kind calibrations. Genuinely unknown
numbers: settlement count per globe, population distribution, subsistence mix,
how habitable-fraction converts to settled-fraction. A 10k `census-of-peoples`
study runs at author time; the CI `census-lands-drift` study grows the new
metrics (unified registry, as in C3).

## 11. CI

The artifact drift check gains the settlement-map render(s) for the exit-demo
seed(s); the `census-lands-drift` rerun picks up the new People metrics, all
folded into the existing `git diff --exit-code` net. The 10k census is
author-time only.

## 12. Testing

- **Determinism:** same seed + pins → byte-identical settlement set, flagship,
  social structure, and settlement map. `BTreeMap`/`Vec`, `total_cmp`, no
  wall-clock, no `HashMap`/`HashSet` (clippy-enforced).
- **Pin isolation** (the 2a lesson): settlement pins consume the same draws as
  the unpinned path — tested explicitly.
- **Drainage battery:** flow accumulation conserves mass (total = land-cell
  count); every cell's drainage ≥ 1; downhill monotonicity; endorheic cells never
  reach the sea; ocean cells are sinks.
- **Placement battery:** placed settlements respect minimum spacing; all lie on
  habitable cells above the suitability floor; the flagship is the global
  suitability argmax; determinism across a seed sweep.
- **Social structure:** subsistence equals the known function of biome (the
  calibration, as a test); a fertile watered populous cell yields a stratified
  structure and an arid marginal cell a lean one; structure varies across
  environments.
- **Enrichment cascade:** perturbing astronomy (tidal lock / obliquity) changes
  the settlement map and the flagship's subsistence/structure, and the result
  stays internally consistent.
- **End-to-end:** a world exposes a queryable settlement map; the almanac renders
  the generated settlement section; the REPL answers `settlements` and
  `settlement <lat> <lon>`; the exit-demo test asserts seed 42's people
  reorganize between spinning and tidally-locked from the same land seed.

## 13. Staging

Following the C3 rhythm (3a/3b/3c), C4 splits into two sub-plans, each shipping a
working increment + artifact:

- **Plan 4a — Placement & drainage.** The terrain drainage field + endorheic
  flags; `settlement::place` (suitability, spacing, scatter); the generated
  flagship replacing the Vale; composition-root rewiring; the settlement-map
  artifact; REPL/almanac surfacing of placement. Culture stays tier-0 (fixed
  ladder) through 4a.
- **Plan 4b — Emergent society.** Subsistence + surplus/scale/threat →
  `culture::structure`; the Census of Peoples metrics + subsistence calibration;
  the enrichment cascade test; the settlement pins; the C4 close-out book work.

Splitting is acyclic (4a's placement is 4b's input). Either may abort at a week
boundary with the tier-0 cascade intact.

## 14. Book / Definition of Done

Chronicle entry (folding 4a + 4b); settlement and culture chapters promoted to
tier 1 with their model cards; the settlement-map gallery artifact and its
tidally-locked twin; a Census-of-Peoples study chapter with comprehension-gated
analysis (the subsistence calibration + the settled-fraction unknown number);
regenerated stream manifest and concept registry; freshness sweep of the cascade
overview (the Vale's retirement, the People rows to tier 1).

## 15. Explicitly deferred

- **Walkable interiors / embark** (fine terrain, local fields, the place to
  stand inside a cell) → the local-refinement campaign that reads this skeleton.
- **Multi-species societies** (the Vale sketch's several nonhuman groups) → the
  Year-2 species-psychology substrate campaign, upstream of social/religion.
  C4 is goblin-only.
- **Inter-settlement politics, trade networks, borders, migration** → later
  social/historiography tiers. C4 places and structures settlements in isolation.
- **Founding history / settlement age** → the event-ledger / historiography tier.
- **Full river geometry, erosion-weighted drainage, lake filling** → the
  hydrology/erosion refinement of terrain. C4 builds only coarse flow
  accumulation.
- **Deep social depth for non-flagship settlements** → a later tier; C4 gives one
  flagship the full culture/religion cascade.

## 16. Risks

- **Scope breadth.** Two domains to tier 1 plus a terrain field is a large
  campaign. Mitigation: coarse cell scale, rule-table sociology, one deep
  flagship, aggressive deferral (§15), and the 4a/4b split.
- **"Convincing enough" society** is a taste gate, like chart legibility and the
  biome map — the settlement map and the flagship's structure, and Nathan's read
  of them, are the gate. Mitigation: high verification surface (subsistence⇔biome
  calibration, Earth's ecology→society patterns) covers correctness; aesthetics
  iterate.
- **Placement clumping or emptiness.** Greedy spacing must produce a legible,
  non-degenerate scatter across plausible globes. Mitigation: property tests on
  spacing and count across a seed sweep; the suitability floor and spacing are
  tunable pins.
- **The Vale's retirement** touches the existing cascade wiring and every test
  that assumed a hand-placed Vale. Mitigation: the composition root is the single
  rewire point; the place-entity-with-CellId interface preserves "a place to
  stand" for the downstream cascade.

## 17. Exit criteria

1. `hornvale new` produces a world with a queryable, field-placed scatter of
   settlements and a generated flagship; the REPL answers settlement queries; the
   almanac renders the generated settlement section; the Vale is gone.
2. The settlement-map artifact for the exit seed reproduces byte-identically on
   rerun, and the tidally-locked twin visibly reorganizes the people from the
   same land seed.
3. The Census of Peoples answers the settled-fraction question with a number, and
   the subsistence⇔biome calibration matches its known function exactly.
4. The flagship's social structure demonstrably differs between two worlds that
   differ only in astronomy (the enrichment thesis, proven).
5. The Laboratory and gallery additions are live in the published book,
   drift-checked; the full gate (test + fmt + clippy) and the artifact drift
   check are green.
