# Settlement

**Questions it answers:** Where do people live? What is the settlement
called, how many live there, and where does it stand?

**Tier 0 — the goblin village.** Genesis mints one village in whatever place
it is handed, with three generated properties: a name, a population between
forty and eighty, and a location fact tying it to its home. Seed 42 yields
**Gruugish, population 57**.

**How the name happens** — this small feature carries three load-bearing
ideas at once:

1. *Candidate generation.* Two or three syllables drawn from a goblin
   syllable pool, capitalized — the direct descendant of the goblin-mutter
   system in the vision book's language chapter, and the most primitive
   ancestor of the phonaesthetic name generation planned for the language
   domain.
2. *Refinement, used in anger.* The name is chosen through the kernel's
   consistency engine, which rejects any candidate whose commitment would
   contradict a committed fact. Today, with an empty world, nothing ever
   conflicts — but every future settlement will flow through this same
   pathway when the ledger is crowded enough for collisions to be real.
3. *Derivation labels as contracts.* The name draws from the seed chain
   `settlement → name`, the pick from `settlement → name-pick`, the
   population from `settlement → population`. These labels are permanent:
   changing any of them renames every village in every saved world.

**What it emits:** four facts per village (name, is-a-settlement, located-in,
population), all with settlement's provenance, all queryable by anything —
culture and religion take the village's entity id at genesis, and the almanac
and REPL read the rest back through ordinary queries.

**Tier 1 — a scatter placed by the land itself (Campaign 4a, the Vale
retired).** The single hand-fed village is gone; every world now scores
every habitable Geosphere cell for **suitability** in `[0, 1]` and places a
spaced scatter of settlements at the cells that win. Suitability is one
weighted sum, no simulation: freshwater (whichever is highest of drainage,
coastal adjacency, or ambient moisture) at weight 0.45, a coastal bonus at
weight 0.20, and a temperance term — a triangular preference peaking at
15 °C and falling to zero by 0 °C or 30 °C — at weight 0.35, minus hostility
(tectonic unrest or aridity, whichever is worse) at weight 0.5. Freshwater is
where Campaign 4a's other new terrain field pays for itself: a coarse
drainage skeleton (each land cell sent downhill to its lowest neighbor,
upstream area accumulated along the way, with cells whose downhill path
never reaches the sea flagged *endorheic*) gives suitability a real,
non-authored answer to "is there water here" everywhere the coast does not
already answer it.

**Placement is a greedy spaced scatter.** Every habitable site clearing a
suitability floor (0.25 by default) is ranked by score, descending, ties
broken by ascending cell id for determinism; sites are then accepted
greedily, skipping any candidate within 12° of angular separation from a
settlement already placed. The single global suitability **argmax** is
always the first settlement accepted, unconditionally — the *flagship* —
so the spacing rule can never cost the world its capital. Each placed site
is committed as its own place entity (cell, latitude, longitude, biome, a
generated name) with a population drawn against a carrying capacity implied
by its own suitability score (`40 + 460 × suitability`, jittered ±25%),
rather than the flat 40–80 range tier 0 drew for the one village it knew
about.

**A settlement pin joins the terrain pins.** `--min-suitability F` overrides
the placement floor and is persisted as a round-trippable
`settlement-pin` scenario fact for inspection, though — unlike terrain and
sky pins — settlements are never *reconstructed* from it: once placed, a
settlement is its own committed fact, not a re-derived field. Choosing which
placed site becomes the flagship is not yet pinnable (deferred, spec §9);
the flagship is always the suitability argmax.

**The model card.**

- **Drawn (from the seed, or pinned):** the placement suitability floor
  (`--min-suitability`, default 0.25); per-settlement name syllables and
  population jitter, salted by the settlement's own cell id (so a
  settlement's identity is independent of how many others happen to be
  placed around it).
- **Derived (a weighted sum and a greedy scatter, not a simulation):**
  per-cell suitability from freshwater, coast, and temperance minus
  hostility; the placement scatter itself (ranking, spacing, and the
  flagship argmax); each settlement's carrying-capacity population.
- **Approximated (declared):** freshwater from the coarse drainage
  skeleton — single lowest-neighbor flow direction, unit-area accumulation
  with no precipitation weighting, no sub-cell river geometry or lake
  filling (an epoch bump behind the same interface, never a silent change);
  a fixed 12° minimum spacing, not a settlement-density field; no
  inter-settlement relationships or trade routes yet (deferred to
  Campaign 4b's successor); no settlement history — every settlement is
  founded, fully formed, at genesis.

Seed 42 under a spinning sky places 58 settlements; the flagship, **Torgna**,
holds 506 souls in temperate rainforest. The gallery holds the exit-demo
pair: [The Peoples of Seed 42](../gallery/settlement-seed-42.md) against [its
tidally-locked twin](../gallery/settlement-seed-42-locked.md), where
habitability's collapse toward the terminator ring (Campaign 3c's biome map
already predicted it) thins the same globe to 29 settlements. Chronicle:
[4a, Placement & Drainage](../chronicle/campaign-4a.md).

**The tier ladder ahead:** multiple settlements with relationships and
trade, settlement histories as the fields-and-ledger model of deep time
arrives, and a pinnable flagship-selection override (spec §9, deferred as
showpiece-only).
