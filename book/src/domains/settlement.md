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
   ancestor of the real phonology-driven name generation Campaign Y2-3 (The
   Tongues) later built (this stage's syllable pool is long retired; see
   below).
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

**Tier 1 — settlements condense out of a carrying-capacity field (Campaign
4a, the Vale retired; re-founded on a field by *The Gathering*).** The
single hand-fed village is gone. Where every world once *scored* each
habitable Geosphere cell for suitability and greedily scattered spaced
sites across the winners, every world now derives a **carrying-capacity
field** `K` — an absolute people-density each cell can support — and reads
discrete settlements off it as **conserved attractors of a population
flow**. The suitability formula and its greedy spacing pass are retired
outright (decision 0048); nothing in this domain scores a cell any more,
because population is no longer a number a formula hands out — it is a
readout of a physical field.

`K` is closed-form and seed-free, built in the sibling `demography` domain
(decision 0047) from the same terrain and climate reads the retired
suitability formula used: a Miami-model net-primary-productivity proxy
(the Liebig minimum of a temperature response peaking near 22 °C and a
moisture response) scaled up by freshwater availability (still the
non-authored answer Campaign 4a's drainage skeleton gives to "is there
water here" — seawater is not freshwater, the same conflation Campaign
Y2-0 removed) and a coastal bonus, scaled down by hostility (tectonic
unrest or aridity, whichever is worse) toward exactly zero. Each species
reads its own copy of `K`, its psychology folding in much as it did under
the retired formula: a longer time horizon scales up the freshwater term,
a bolder threat response scales down the effective hostility — an identity
substitution at the goblin baseline, a real divergence at kobold's
authored values. The field's grounding is calibration-checked, not
asserted: measured against the real biomass-by-latitude gradient, the
tropical-and-temperate band supports roughly **27×** the capacity of the
polar band, decisively reproducing the pattern real biomes show.

**Condensation borrows drainage's own shape, comparator flipped.** People
climb the `K` gradient the way water descends elevation: every cell routes
toward its highest-`K` neighbor (ties broken the same strict, deterministic
way drainage breaks elevation ties), and a cell's accumulation is the sum
of `K` over every cell whose climbing path passes through it. A cell with
no higher-`K` neighbor is an *attractor* — a candidate settlement — and
every other cell joins the catchment of the attractor its path leads to.
An attractor becomes a committed settlement once its catchment accumulation
clears a concentration threshold (frozen once, calibrated to a manageable
seed-42 count — 182 settlements, average catchment about 22 people, down
from 998 at an untuned threshold); its population is that accumulation
exactly, a readout of the field rather than a draw. Conservation is
structural, not tuned: summed over every attractor at no threshold at all,
`Σ population == Σ K` holds exactly, per species, because settlements
*partition* the carrying-capacity budget rather than each sampling a local
value. Thresholding at the operational cutoff culls the weakest catchments
and leaves their mass dispersed — realistically, roughly half of a
pre-industrial world's supported population is rural, not gathered into a
named place, and a world-level guard bounds that remainder on both sides.
Each placed site is still committed as its own place entity (cell,
latitude, longitude, biome, a generated name), exactly as before; only the
population's provenance changed.

**The founder floor still guarantees every species a place on the map.**
Since The Branches, no species' placement is left to fall to zero purely
because it is outcompeted: the founder floor — migrated alongside the
field it now floors over (decision 0049) — reserves each species its
single strongest attractor even where that attractor's catchment falls
below the concentration threshold, and floors its committed population at
one rather than ever rounding a settlement down to nobody. The retired
`--min-suitability` scenario pin, which used to tune this same floor
against a suitability score, has no referent left to override — the
concentration threshold is calibrated once and frozen rather than exposed
as a pin (no world needs to choose its own rank-size slope yet), and old
saves carrying a `settlement-pin` fact still load, the fact simply ignored
like any other unknown pin. Choosing which placed site becomes the
flagship is not yet pinnable (deferred, spec §9); the flagship is always
the settlement with the highest population across every species that
settled — the same "argmax, first fact committed" contract the retired
mechanism kept, just read off the flow instead of a suitability score.

**Joint condensation, four peoples (Campaign Y2-1; The Branches).** Every
world places for the whole species registry by default — goblin,
hobgoblin, bugbear, and kobold — rather than one fixed people, each
condensing its own field independently. A `--species NAME` pin restricts
placement to one species (used by the superset-contract test to reproduce
a goblin-only world); the unpinned default places every species the
registry holds. Population for any non-goblin species draws its
psychology-folded field from the same per-species machinery, so goblin's
placement and population outcomes trace the same species substrate
described fully in [Species](./species.md). The settlement noun itself is
species-specific: a goblin settlement is a "village," a kobold one a
"warren." One deliberate, documented step back rides along: because each
species now condenses its field independently, two peoples may settle
overlapping ground — the old 12° cross-species spacing rule is not rebuilt
here. Restoring real multi-species exclusion, with footprint-scaled home
ranges and competitive sharing rather than a fixed separation distance, is
the coexistence-stack campaign's job, building on this field rather than
inside it.

**A generated voice, not a syllable pool (Campaign Y2-3, The Tongues).**
Settlement naming no longer lives in this domain at all. `settlement/name`
and `settlement/kobold/name` are retired — kept documented forever as
save-format contracts (ADR 0006) but never drawn from again — because a
domain crate cannot depend on another domain, and a real name needs a real
mouth. The composition root now draws every settlement's name from
`domains/language`'s `Namer`, built over that species' own drawn phonology
under its authored articulation envelope; settlement only ever receives the
finished name back and commits it under the same `name` predicate it always
used. Name generation is pin-isolated by construction — a pure function of
seed, species, name-kind, and the settlement's own cell salt, with no
shared "used names" set threading between settlements — so cross-world
uniqueness is measured as a calibration, not enforced by re-draw. The
Tongues-era free stem calibrated at 2.79% mean collision at 10k worlds
(Study 008); Campaign 27 (The Words) replaced that free stem with a
name that also glosses truthfully to a settlement's own site facts, which
narrowed the name space enough to raise the mean to **4.91%** at the
CI-guarded 500-seed population (4.94% at 10k) — measured, reported, and
left as a standing open question rather than tuned back down (Study 011).
See [Language](./language.md) for the phonology, the gloss, and the naming
grammar themselves.

**The model card.**

- **Drawn (from the seed, or pinned):** a settlement's name, drawn by
  `domains/language`'s `Namer` (see above) — the only draw left in this
  domain's own surface; a `--species NAME` scenario pin restricts which
  species place at all.
- **Derived (a closed-form field plus a deterministic flow, not a
  simulation):** the carrying-capacity field itself (freshwater, coast, and
  temperature minus hostility, in the sibling `demography` domain); the
  up-gradient flow accumulation over it; the attractor extraction, the
  concentration threshold, and the flagship (highest-population attractor);
  each settlement's population, an exact readout of its catchment.
- **Approximated (declared):** freshwater from the coarse drainage
  skeleton — single lowest-neighbor flow direction, unit-area accumulation
  with no precipitation weighting, no sub-cell river geometry or lake
  filling (an epoch bump behind the same interface, never a silent change);
  the concentration threshold is a frozen constant, not yet a per-world
  choice; no cross-species spacing (a documented, temporary regression); no
  inter-settlement relationships or trade routes yet (deferred to
  Campaign 4b's successor); no settlement history — every settlement is
  founded, fully formed, at genesis, its population an equilibrium
  snapshot rather than something that grew there.

Seed 42 under a spinning sky condenses 182 settlements across all four
peoples (goblin, hobgoblin, bugbear, and kobold, since [The
Branches](../chronicle/the-branches.md) gave the goblinoids two new
members); the world flagship, the kobold settlement **Roqrraxaxoqrrak**,
holds 71 souls in temperate forest, with each other species' own chief
settlement close behind (goblin's **Ngebvngadxnotnoaboo** and hobgoblin's
**Gbeoveenoenoanoagoo** both at 67, bugbear's **Qvoashshngoashdoodoa** at
64) — real,
phonology-drawn names (Campaign Y2-3, The Tongues; Campaign 27, The Words),
where earlier campaigns' editions of this same page named the pair from a
syllable pool instead. The gallery holds the exit-demo pair: [The Peoples
of Seed 42](../gallery/settlement-seed-42.md) against [its tidally-locked
twin](../gallery/settlement-seed-42-locked.md), where habitability's
collapse toward the terminator ring (Campaign 3c's biome map already
predicted it) thins the same globe to 172 settlements. Chronicle: [4a,
Placement & Drainage](../chronicle/campaign-4a.md) and [Campaign Y2-0, Firm
Ground](../chronicle/campaign-y2-0.md) for the drainage field and freshwater
fix this campaign's carrying-capacity term still reads, [Campaign Y2-1, The
Peoples](../chronicle/campaign-y2-1.md) for the joint placement that split
it across species, [Campaign 16, The Tongues](../chronicle/16-the-tongues.md)
for the generated names themselves, and [The
Gathering](../chronicle/the-gathering.md) for the move from a suitability
scatter to the carrying-capacity field and its condensation.

**The tier ladder ahead:** the multi-species exclusion this campaign
loosened, restored properly by a coexistence stack layered on the field;
settlement histories — founding, growth, fission, abandonment — as the
field gains a clock; inter-settlement relationships and trade; and a
pinnable flagship-selection override (spec §9, deferred as showpiece-only).
