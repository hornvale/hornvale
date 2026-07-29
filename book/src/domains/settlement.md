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

**Joint condensation, the settling peoples (Campaign Y2-1; The Branches;
recut by *The Menagerie*).** Every world places for the settling peoples —
goblin, hobgoblin, bugbear, and kobold — filtered from a registry that now
also holds a biosphere-only menagerie (fauna do not settle). Originally each
people condensed its own carrying field independently; since *The Menagerie*
cut genesis over onto the competitive niche-K coexistence stack, the peoples
are packed *together* and a settlement is peopled by whichever people locally
prevails, with the rest present in its composition. Which people prevails
where moved sharply under *The Tumult*'s elevation re-datum (see
[Species](./species.md)): with the condition niche's elevation axis finally
scored above sea level, seed 42's stack resolves 216 settlements with the
hobgoblin dominant in 212 and the **kobold** — previously dominant nowhere at
all, its highland optimum having sat above most worlds' highest land — holding
4 and present in every one of the 216. The goblin, which had shared dominance
under the broken frame, holds none. The bugbear remains the one people that
wins no settlement's dominance while being present throughout; its stronghold
axis is moisture rather than elevation, and the re-datum leaves it exactly
where it was. A `--species NAME` pin restricts the roster to one
people; because the stack is competitive, a pinned world is a deterministic
*restricted-roster* world (that people with no rivals), not an isolated slice
of the full world. Population for any people draws its
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
used. Cross-world uniqueness is measured as a calibration, never enforced by
re-draw. The Tongues-era free stem calibrated at 2.79% mean collision at 10k
worlds (Study 008); Campaign 27 (The Words) replaced that free stem with a
name that also glosses truthfully to a settlement's own site facts, which
narrowed the name space enough to raise the mean to **4.91%** at the
CI-guarded 500-seed population (4.94% at 10k) — measured, reported, and
left as a standing open question rather than tuned back down (Study 011).
See [Language](./language.md) for the phonology, the gloss, and the naming
grammar themselves.

*Two claims in this paragraph were true when it was written and are not
now, both moved by The Wearing.* Name generation is no longer **pin-isolated
by construction**: it is still a pure function of its arguments, with no
shared mutable "used names" set and no re-draw, but one of those arguments is
the culture's own name corpus — toponymic wear is keyed to how often a
morpheme recurs across a people's settlements — so a pin that moves a species'
scatter can move that species' names. Determinism is untouched; what is given
up is world-level pin isolation, deliberately, and only for glossed settlement
names. And the collision rate has risen a long way past 4.91%: the drawn stem
that bought the low rate has been retired, on decision 0024's own instruction
that no future work fix the collision rate by adding entropy, and the census
median now reads **0.65**.

**Ambiguity is a property of a reference (The Wearing).** Decision 0024 held
that settlement-name uniqueness belongs to a *reference*, not to a name, and
named the remedy it deferred: qualify a name where an actual ambiguity
appears, in a rendered document, never in the ledger. That remedy now exists.
A rendered listing — the almanac's land roster, a settlement's connections
document, the REPL's settlement list — groups the entries it is about to
print, and where two would render identically it walks a ladder of qualifiers
drawn from the entities' own site facts: the people, the biome, both, then the
coordinate. **The first rung that separates the whole group is taken.** A rung
that separates only part of the group is refused, and a group no rung can
separate is left bare rather than given a counter, which 0024 forbids. Scope
is one document at a time: a name shared with a settlement on the far side of
the world is not ambiguous to anyone reading about either.

It is a **view**, and that is checked rather than asserted — seed 42's
`world.json` is byte-identical with the feature present and absent. No fact,
no stream, no epoch. Two entries in a listing that already prints the biome
are ambiguous only when the *whole line* coincides, so grouping is by the
rendered line and not by the name alone; that also makes a
`- **Roa (taiga)** — taiga` impossible by construction, since the biome rung
can never separate a group that already shares a biome. At seed 42, 102 of
334 land entries need no qualifier at all, and all 334 rendered lines are
distinct.

The measurement that came out of building it is the durable part. Colliding
names **agree on their descriptors by construction** — the gloss *is* the
site-descriptor set the name was compounded from, so two settlements sharing a
name necessarily share the facts that named them (at seed 42, all 51 colliding
name-groups share a single gloss). Widening the descriptor vocabulary, the
obvious lever, therefore cannot separate a collision: any fact fed *into*
naming is a fact the collision already agrees on. Only facts *outside* the
gloss can, which is why the coordinate rung carries nearly every live
qualification.

**History places now; the field became substrate (The Living
Community).** The carrying-capacity field no longer *places* settlements. It
is demoted from placer to **substrate** — the fitness landscape a derived deep
history plays out across. Genesis seeds a handful of proto-communities on the
early world and runs a coarse forward history over roughly two thousand years:
each community, each era, resolves against *that era's* carrying capacity
(the field ticked by the paleoclimate the world already computes) — growing
under slack, founding a daughter into vacant favourable ground, retreating
toward refugia when its cell turns against it, or ending. The **present world
is the last frame** of that run. A living settlement is a community still alive
at the final year; a ruin is one that died; the grassy clearing is one that
died lifetimes ago. The condensation flow above still builds the capacity
field the history reads, but it no longer emits the present map — history is
the sole provider of both settlement and ruin facts, and each carries a
provenance pointer to the skeleton event that produced it.

The history lives as a **committed skeleton, locally-derived flesh**: a
compact, dated event log (foundings, migrations, endings, each with
role-handles) is committed to the ledger, and the flesh — the persona behind a
handle, the objects a ruin leaves, a site's rendered prose — is a pure
function of a committed fact plus the lossless seed, derived on demand and
never stored. The present frame is a *query* over the communities still alive,
not a separate tier of state. Because the world is finite and equilibrating,
displacement is kept perpetually alive without a floor: era-varying
habitability (glacial advance and retreat) drives sustained migration, and a
preregistered census gate *aborts the build* if displacement fails to fire at
volume. On the world that campaign measured, displacement resolved as climate
**migration**, not war — vacant land let a frozen-out community move rather
than raid — so organised conflict was deferred to a later campaign of the
program, which has since arrived (see the conflict section below). Two measured
findings shaped the result: the four near-identical goblinoids finally hold
**distinct territories**, separated by history rather than niche; and
re-occupation stratigraphy accretes on **marginal, climate-contested** land
(repeatedly abandoned and resettled), while prime land is held stably by one
long occupation — the deepest layers mark the *worst* ground, not the best. A
site's layers and their flesh render through a read-only surface (the `history`
CLI verb and the almanac); see [The Living
Community](../chronicle/the-living-community.md).

**Communities fight over value, not over room (The Tumult).** The history's
conflict rule was rewritten to drop density entirely. Each epoch, after growth,
a community scans the occupied cells it can reach across that era's connection
graph and raids the best one that is both **worth more than its own** (the
era-effective capacity field, the same one growth reads) and **held by someone
it can beat** (population scaled by technological horizon, by a margin). Two
vetoes inhibit it: a target already starving against its own capacity has
nothing worth taking, and a people whose authored threat response falls below a
threshold does not raid at all — which makes the aversion structure asymmetric
with no pairwise machinery, since each people gates on its own trait. On the
shipped roster the goblin never raids anyone. A raid is a **conquest**: the
raider takes the cell and the loser is driven off, war destroys a fraction of
the combined population on both sides rather than transferring it, and the
displaced people re-enters the same rule with its baseline substituted —
comparing the nearest ring of cells that offers anything admissible, vacant
ones at plain value and held ones at a premium because a rival's holding comes
already made to work. Below a viable minimum a broken remnant dies instead of
cascading further.

The result is conflict on a world that never crowds: seed 42 resolves **76
conquests** where every prior model resolved zero, and the map does not empty —
communities alive at the final year rise from 138 to 203, because conquest
re-seats and refounds. The campaign's headline, though, is a **falsification**:
the cascade-size distribution was measured against the project's standing wager
that emergent conflict would be scale-free, and it is not. Pooled over a hundred
seeds and 2974 conquests, nothing chains beyond size three; the branching ratio
is **σ ≈ 0.051** against a critical value of 1, stable across a 3.3× change of
sample. The distribution is geometric with a hard cutoff, deeply sub-critical.
The diagnosis is that this slice supplies dissipation without **accumulation** —
nothing is stored between relaxations whose release could make a large event —
which is precisely what the deferred dominance-hierarchy slice would add. See
[The Tumult](../chronicle/the-tumult.md).

**A raid may also end in subordination rather than eviction (The Tithe).** The
accumulation term above was then built, and what decides between the two
outcomes is the **mobility of the prize**. A cell is a perfectly rival immobile
good, takeable only by occupying it, so contact over it stays conquest. But a
community's people and their product are mobile, and a mobile prize can be
taken again next year without displacing anyone — so a raider that beats a
neighbour whose *land* is no better, but which still has growth headroom,
**subordinates** it instead: the loser keeps its ground and begins paying
tribute. That is motive the covetousness gate previously refused outright, and
it is the accumulation the falsification asked for, because the dominant now
grows **without moving**.

The rate is a guess on both sides, and the asymmetry that makes it one is
already structural: **the patron taxes what it can see, the vassal holds what it
has.** The demand is set from the vassal cell's visible capacity; the remittance
is paid out of the epoch's growth increment and out of stock above a floor, less
a **concealment** derived from the vassal's authored in-group radius, so an
insular people withholds more than an expansive one. What is remitted lands not
in the patron's population — which would drive its pressure to the collapse
threshold and starve a successful extractor — but in a per-community **store**
of wealth, which feeds strength, never enters the pressure term, decays slowly,
and is lost when its holder dies. How hard a patron extracts is set by its
authored **time horizon**, read as a discount rate: because a community grows
logistically, maximum sustainable yield sits at half of capacity, so a patron
maximising the discounted stream holds its vassal there while one maximising
this epoch strips it. The Danegeld, the protection racket, tax farming and the
bust-out are therefore the *same rule at different horizons* — a family
generated rather than enumerated — and extinction arrives as the classical
resource-economics result that extermination is optimal when the discount rate
exceeds intrinsic growth: rare, and concentrated among the shortest-sighted
patrons. Vassals are not passive: one whose burden grows too heavy **flees**,
and one that comes to out-muscle its patron by the dominance margin **revolts**.
A community that relocates keeps the relations it holds as patron — a lord's
claim travels — but drops the one it owes as vassal, so a vassal that moves,
for any reason, arrives free.

The headline was re-measured on the same instrument, and it is a **second
falsification**. Accumulation moved the branching ratio: σ rises from ≈ 0.051 to
**≈ 0.11** across thirty seeds and 7183 conquests, and by the same factor across
a hundred seeds and 22 255. But σ ≈ 0.1 is not σ ≈ 1, and the *shape* is
unchanged — the support still spans 0.48 decades, the per-octave decay is still
17.6-fold where a heavy tail falls two- to fourfold, and **not one cascade
exceeds three displacements in roughly twenty-two thousand conquests**.
Geometric with a hard cutoff, still deeply sub-critical. The diagnosis this
leaves is structural rather than a missing term: a revolt frees exactly one
vassal, and the relation graph is a set of **one-level stars** (a vassal may not
itself take a vassal), so a patron's failure has no medium along which to
propagate. An avalanche needs a medium. See [The
Tithe](../chronicle/the-tithe.md).

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
  inter-settlement relationships or trade routes yet (deferred to the
  connection-graph campaign of the living-community program).
- **Grown, not placed (The Living Community):** settlements are no longer
  founded fully formed at genesis. They are the last frame of a derived deep
  history run over the (now era-varying) capacity field — grown, migrated, and
  abandoned across ~2000 years — with standing ruins and separated territories
  falling out of the run. See the history-first section above.
- **Taken, not only grown (The Tumult):** a settlement may also change hands.
  Conquest is a deterministic function of frozen epoch state — coveted value,
  relative strength, era reachability, with total-ordered tie-breaks — reading
  state rather than integrating it, so no new draw and no chaotic forward
  integration enters the bake. It commits no new fact shape: a conquest is a
  chain of the occupation record's existing endings and foundings.
- **Milked, not only taken (The Tithe):** a settlement may instead be
  subordinated and left standing. Assessment, remittance, concealment,
  the patron's setpoint, flight and revolt are all total functions of frozen
  epoch state and authored species data — no new draw, no agent decision, no
  chaotic forward integration. It commits **one** new registered predicate
  (`pays-tribute-to`) over the ledger's existing entity-to-entity fact
  envelope, and no new fact shape, cause of ending, or stream label.

Seed 42 under a spinning sky now grows **329 settlements** as the present
frame of its derived history (the field-condensation model above supplies the
substrate the history plays out on; it no longer emits the map). Since [The
Menagerie](../chronicle/the-menagerie.md) cut genesis over onto the
competitive niche-K stack, five peoples share the landscape — the four
goblinoids (goblin, hobgoblin, bugbear, and kobold, since [The
Branches](../chronicle/the-branches.md) gave them two new members) and the
gnoll, whose desert niche [The Vacancy](../chronicle/the-vacancy.md) added —
but history now separates them into distinct territories and each holds its own
chief settlement: the hobgoblin **Feamjeafeoqoanoagoo** (127 souls,
tropical-rainforest) is the world flagship, with the gnoll
**Gzaadmzhooqdsootngsootqzhoof** (119, tropical-rainforest), the bugbear
**Qvooshtvoagootao** (118, tropical-seasonal-forest), the goblin
**Fneksvotngetnoaboo** (118, tropical-rainforest), and the kobold
**Roqrrarogxok** (110, temperate-forest) each holding their own — real,
phonology-drawn names
(Campaign Y2-3, The Tongues; Campaign 27, The Words). One selection rule is
worth stating because it is easy to misread: a *people's* chief settlement is
not its largest but its **oldest surviving occupation** — the first settlement
fact that people committed and still holds — while the *world* flagship above
it is the highest-population site of any people. *The Tumult* made the
distinction consequential, since a conquest closes the raider's own occupation
record and reopens it at the seized cell: a raiding people therefore hands its
chief settlement on far more often than a peaceful one, which is a re-selection
effect on identity rather than anyone moving house. Populations run larger than
the equilibrium snapshot's, because a settlement's headcount is now something
that *grew* there over the history rather than a single field readout. The
gallery holds the exit-demo pair: [The
Peoples of Seed 42](../gallery/settlement-seed-42.md) against [its
tidally-locked twin](../gallery/settlement-seed-42-locked.md) (250
settlements), where habitability's collapse toward the terminator ring
(Campaign 3c's biome map already predicted it) reshapes the same globe. Chronicle: [4a,
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
inter-settlement relationships, trade, and a real transport topology for
diaspora and conflict (the connection-graph campaign next in the
living-community program); and a pinnable flagship-selection override (spec
§9, deferred as showpiece-only). *Settlement histories — founding, growth,
migration, abandonment — have since landed:* the field gained a clock in [The
Living Community](../chronicle/the-living-community.md), which grows the
present world as the last frame of a derived deep history rather than placing
it fully formed at genesis. *The transport topology and the conflict have since
landed too* — the connection graph over a sea that moves with the ice ([The
Sundering](../chronicle/the-sundering.md)) and value-driven conquest over it
([The Tumult](../chronicle/the-tumult.md)). *The first **standing
relationship** between communities has since landed too* — tribute, in [The
Tithe](../chronicle/the-tithe.md) — and the reason it had been deferred turned
out to be wrong. The claim on record was that a persistent inter-community
relation "needs a save-format change and a real new subsystem", because the
occupation record cannot express one as a chain of endings and foundings. It
does not need to: the ledger's fact envelope is **already** a typed, directed,
dated entity-to-entity edge, and the bake already emitted two of them. Tribute
took **one registered predicate** and nothing else. What still stands open is
therefore not the shape but the rest of the space it opens — alliance, trade,
employment, and the down-flow of **protection**, a patron shielding its vassals
from third-party raids, which is the strongest deferred lever because it changes
the shipped raid rule itself; along with **chained** tribute, where a vassal's
vassal remits upward, and **collapse-release**, where a fallen patron frees its
whole network at once. The last two are what the criticality measurement now
argues for, since they are what would give a patron's failure a medium to
propagate along.
