# The Delvers

This campaign set out to put the first people underground, and its most useful
result is that it withdrew that claim as never having been true.

A settlement in Hornvale is keyed to a cell. A cell is a patch of the world's
surface. So a people declared to live in the subterranean realm does not live
under the rock; it lives on the *surface* of a cell that happens to have a void
beneath it. The realm gate — the best selection mechanism this model has, a hard
zero on the eighty-eight percent of land with no cave in it — places a kind at a
**cave mouth**. There is nothing on the far side of that mouth for a people to
occupy, because the model has no vocabulary for the inside of the world.

That absence has a shape worth naming. The **sea** solved this problem years of
work ago: epipelagic, mesopelagic, bathypelagic and abyssal are depth-named
*places*, each carrying its own supply multiplier, so in the ocean depth and
darkness are a kind of terrain rather than a coordinate pushed through a
tolerance curve. The **rock** got something different — an addressed graph of
chambers and passages, which is an excellent way to say where a room is and a
poor way to say what living in it is like. The sea got biomes and the rock got a
graph, and nobody noticed the asymmetry until a people needed to stand in it.

So the campaign shipped three dwarves instead of five, on the surface, and spent
what it saved on measuring what the model can actually distinguish.

## Three kinds, one family

`desert-dwarf`, `gully-dwarf` and `hill-dwarf` take the settling roster from six
peoples to nine and the biosphere from thirty kinds to thirty-three. They are the
first multi-member family the roster has gained since the goblinoids, which
matters more than it sounds: every piece of machinery that reasons about a
*family* — the proto-language seam, the monophyly measurements, the daughter
lexicons — had exactly one possible subject, and a mechanism with one instance
cannot be told apart from a mechanism hard-coded to that instance. Several of
them turned out to be the latter.

## The theorem that decides which axis is allowed to speak

A creature's fit in a cell is the **minimum** of four condition tolerances —
temperature, moisture, insolation, elevation — because a life is limited by its
scarcest requirement rather than by the average of its comforts. Three of those
four are floored by the creature's *sovereignty*, the buffer that mass and
potency buy against environmental constraint. The fourth, elevation, is not.

Each tolerance evaluates to `floor + (1 - floor) · devotion · exp(-z²/2)`,
clamped, where `z` measures how far the cell sits from the kind's authored
optimum. Two facts fall straight out of that expression: a floored axis can never
read below its floor, and an unfloored axis can never read above its devotion.
Therefore

> **elevation is the binding axis on every land cell if and only if the kind's
> authored elevation devotion is below its sovereignty floor** — a statement with
> no terrain in it at all.

Measured across the pre-existing roster, on three seeds, the theorem holds
exactly. Every kind authored below its floor binds on elevation on **100.00 %**
of land; every kind above it binds elsewhere on a substantial share:

```
  kind        mass    floor  devotion   below?      s42       s7    s1234
  kobold      13.6   0.3078      0.95      no    43.72%   41.55%   51.45%
  goblin      18.1   0.3347      0.35      no   100.00%  100.00%   97.04%
  hobgoblin   74.8   0.4527      0.70      no    74.77%   77.32%   69.26%
  bugbear    132.0   0.4933      0.70      no    72.89%   78.12%   71.40%
  gnoll      136.1   0.4954      0.40     YES   100.00%  100.00%  100.00%
  human       70.0   0.4477      0.30     YES   100.00%  100.00%  100.00%
```

Goblin is the instructive row. Its devotion clears its floor by fifteen
thousandths and it still reads 97 % on one seed: the theorem is exact and the
margin is what makes the exactness visible.

The consequence for authoring is larger than the arithmetic suggests. A previous
campaign had measured three peoples, found all three bound on elevation, and the
observation had hardened into a belief that climate is simply *silent* for a
creature of human-ish mass. It is not. Hobgoblin weighs 74.8 kg — human's mass
class — and binds on elevation on three-quarters of the world, not all of it.
**Mass sets the floor; the authored devotion decides the bind.** The silence was
never a property of the model. It was an authoring convention nobody had noticed
they were following.

## Two modes, chosen deliberately

Knowing that, the dwarves were split across both sides of the line on purpose.

**Gully and hill sit below their floors** (devotion 0.30 against floors of
0.4385 and 0.4477). Elevation becomes the sole determinant on every cell, which
is the *maximum* differentiation available on the one axis that speaks. The
measurement confirms it to the digit: both are elevation-bound on 100.00 % of
land, on every seed.

**Desert sits above its floor** (0.70 against 0.4433 at 66 kg), so its
temperature and moisture curves bind. This is the campaign's demonstrator, and it
works with room to spare: desert-dwarf's binding axis is temperature or moisture
on **86.58 / 67.47 / 91.36 %** of land against a threshold of twenty percent
frozen before the run. Both below-floor dwarves read **0.00 %** on every climate
axis, on every seed. The prediction that an authored climate niche *can* be made
to bind was confirmed, wide, and in both directions.

That result also diagnoses an older puzzle. A people with a documented desert
stronghold had been selecting no arid cells at all, and the reason was never the
desert: its elevation devotion of 0.40 sits below its floor of 0.4954, so its
moisture curve is evaluated and then discarded by the minimum on every cell in
the world. The row now has a mechanism instead of a complaint. It was not
re-authored here — moving an existing people's capacity in the same change that
adds three new ones would destroy the attribution of both.

## Binding and differentiating are not the same property

The prediction the campaign expected to be boring was that the three dwarves
would occupy distinguishable regions of the world. Capacity fields were
correlated pairwise across every land cell, with a threshold of 0.95 frozen in
advance:

```
  pair                     s42       s7    s1234        verdict
  desert vs gully       0.5922   0.5353   0.6313   separated, 3/3
  gully  vs hill        0.6925   0.7551   0.6928   separated, 3/3
  desert vs hill        0.9629   0.8625   0.9796   NOT separated, 2/3
```

The third row is a refutation, and it inverts the design.

Desert-dwarf is the only one of the three whose climate niche actually binds. It
is also the **least** separated from hill-dwarf. Meanwhile gully and hill, which
differ on exactly one live quantity — an elevation optimum of 150 m against
900 m — separate to 0.69–0.76. **A live climate niche bought less spatial
distinctness than moving an elevation optimum did.**

Nothing was retuned to rescue it. The threshold was frozen before any dwarf
existed, the probe was built and validated before any dwarf existed, and the
refutation is now pinned as a witness in its own right, so that a later change
which does separate the pair announces itself rather than quietly inheriting the
finding.

This is the first result in the project to prise apart two things the model's
vocabulary had been treating as one. *Binding* is a question about which axis the
minimum selects in a cell. *Differentiating* is a question about how two kinds'
values sort across cells. A kind can win the first everywhere and still lose the
second, because an axis that binds everywhere is not thereby an axis that varies
informatively.

## Where the differentiation actually comes from

The obvious explanation was ready to hand. Capacity is a supply term multiplied
by a tolerance term; supply spans orders of magnitude while tolerance is bounded
in the unit interval; therefore supply drowns the niche and two kinds look alike
because they are reading the same landscape of plenty.

That explanation was measured, and it does not survive.

For a surface kind, capacity factors exactly into `saturated_supply ×
tolerance_minimum`, and the factorisation was proven rather than assumed — the
reconstruction is asserted bit-identical to the production value on every land
cell of every kind measured. Correlating the **supply factor alone**:

```
  pair                                    s42        s7     s1234
  desert vs gully   full capacity      0.5922    0.5353    0.6313
                    supply only        0.9997    0.9997    0.9996
  desert vs hill    full capacity      0.9629    0.8625    0.9796
                    supply only        1.0000    1.0000    1.0000
  gully  vs hill    full capacity      0.6925    0.7551    0.6928
                    supply only        0.9994    0.9994    0.9994

  all nine supply-only measurements lie in [0.99935, 0.99996]
```

A term that reads essentially 1.0 for *every* pair cannot be what makes one pair
read 0.96 and another 0.59. Over this family the supply term is very nearly
**kind-independent**: before tolerance is applied the three dwarves are all but
the same field. Every scrap of per-kind spatial differentiation therefore comes
from the tolerance layer, which moves these pairs by 0.24 to 0.46 in **both**
directions — it is what pulls desert and gully apart, and it is equally what
leaves desert and hill together.

The question had been framed as a two-branch reading on the desert-and-hill row
alone, and running all three pairs is what showed the framing could not carry it.
Recording that the frame failed is part of the result.

One scope limit is stated rather than buried, because it is exactly the kind of
overreach this campaign kept catching in itself. Pearson correlation is
scale-invariant. It measures how supply *sorts* cells, not how large supply is.
The older claim — that supply's magnitude swamps the niche's contribution to the
final number — is about magnitude, and this measurement neither confirms nor
discharges it. What has been ruled out is supply as the explanation for the
particular pattern of correlations above. The mechanism behind desert-and-hill's
residual similarity is left **unestablished**, deliberately, rather than narrated
into a story that fits.

## Long life finds its first occupants

A previous campaign gave the world a way to say that a creature is long-lived,
independently of how much it weighs — a dimensionless factor on the time laws,
leaving basal metabolism untouched, so that a paced creature lives longer without
being *colder*. It shipped that channel with nobody in it, and named this
campaign as the one that must fill it.

All three dwarves are paced at a factor of four, and pacing is authored as a
**family** trait rather than a habitat one. Longevity is a dwarf thing, not a
cave thing; making only the cave-dwellers long-lived would have asserted that
living underground is what makes a dwarf live for centuries, which is not a claim
anyone wanted to make and which the withdrawal of the cave kinds would then have
taken with it.

```
  kind             mass   allometric   paced(4.0)   maturity   generation
  gully-dwarf      62.0      66.95 y     267.79 y     53.56 y     117.83 y
  desert-dwarf     66.0      68.00 y     272.01 y     54.40 y     119.68 y
  hill-dwarf       70.0      69.01 y     276.04 y     55.21 y     121.46 y
```

The derived generation length reads 117.83 / 119.68 / 121.46 years against an
allometric baseline near thirty — a ratio of exactly 4.00, which is what a
dimensionless multiplier in the right position should produce and which nothing
in the world had previously demonstrated. Two channels **saturate** at one and
say so out loud: pace-of-life and reproductive tempo are normalised against a
ceiling this factor exceeds, so they are uninformative for a dwarf. Lifespan,
maturity and generation length stay linear, which is where the longevity is
legible.

The factor also has to *clear* something to be read at all. A settled people
moves onto the slow language-drift regime above a lifespan of 120 years, and a
70 kg endotherm reads 69 years under pure allometry. Four clears it on all three
with margin, so three tongues have moved onto the regime where a people's
daughters barely diverge — the cost, rather than the boon, that long life carries
in this model.

Reverting one dwarf to the mass-set schedule reddens both halves: the generation
length collapses to 30.36 years and the tongue falls back onto the fast regime.
That demonstration is what the earlier campaign could not run, and its absence
was the honest gap it recorded.

## Dwarves eat a food web, not the rock

Dwarves mine. The model has no extraction economy, and it does have a trophic
axis called *mineral* — soil and rock nutrients — whose only other holders are
the two creatures in the world that literally eat stone. The first authoring of
the family put dwarves on it.

The world objected immediately: a xorn's measured stronghold fell to zero cells,
because two peoples had arrived to compete for its diet. The tempting repair was
to relax the xorn's test — a sibling assertion had even predicted that this
campaign would move it. That would have inverted a correct measurement to protect
a false claim.

What shipped instead is that dwarves eat **fungus, the animals that eat fungus,
and the fermented products of both**, carried on the detritus axis by the same
conflation between a production channel and the prey web it supports that the
marine axis already makes. The comment says so plainly rather than implying the
model resolves a mushroom from its substrate. The xorn's test passes untouched: a
test reporting a false claim was repaired by removing the false claim.

One limitation is recorded beside the authored rows, because it bears directly on
everything above. The detritus supply field is **spatially constant** across all
land. Three dwarves therefore carry a supply term with no spatial information in
it whatever, which is one concrete reason the supply factor above reads
kind-independent, and which means the family's differentiation rests on the
elevation curve and nothing else. A decomposer axis derived from the biota above
it — the obvious repair — is deliberately not attempted here, because changing
what an axis *means* in the same breath as adding three peoples would make every
subsequent movement unattributable.

## A ceiling that was a fraction wearing the costume of a number

One preregistered bound broke, and it broke because it had never been the
quantity it appeared to be.

A calibration band on cross-species diversity permitted a claimed-diversity mean
in `[1.5, 3.0]`. Its own documentation justified the upper bound as "comfortably
below undifferentiated *oatmeal* sharing, where the strife statistic tends to
four, **the species count**". Four was the peopled roster at the time. So 3.0 was
never absolute — it was **seventy-five percent of oatmeal**, and oatmeal is the
size of the roster. The dependency was invisible because it had been compiled
into a literal.

Measured on the enlarged roster the mean is 3.4238. Against the stale literal
that is a breach. Against the quantity the bound actually meant it is a large
improvement: a diversity of 3.42 out of a possible eleven is thirty-one percent
of oatmeal, where the original band permitted seventy-five. **The new peoples are
partitioning space rather than piling onto it**, which is precisely what the
bound exists to check.

The ceiling is now derived as three-quarters of the peopled count, which
reproduces 3.0 exactly at a roster of four — the change is a no-op at the roster
the bound was written for, which is the strongest available form of this repair.
The floor stays absolute, because monoculture drives the statistic to one
whatever the roster size; that half never scaled. The competition temperature the
band interprets is untouched. And the honest cost is stated where the band lives:
a ceiling that scales with the roster is a weaker discriminator on a large roster
than a literal was on a small one.

## What the world did

Seed 42's committed world moved, and the size of the movement was deliberately
not predicted:

```
                               before        after
  settlements                     122          145
  peoples holding occupations       6            9
```

Occupations after, by people: kobold 150, hobgoblin 122, gnoll 68, goblin 33,
hill-dwarf 20, human 13, bugbear 9, gully-dwarf 3, desert-dwarf 2 — 420 in total.

**The dwarves hold very little ground, and that is reported rather than
explained.** Hill-dwarf's twenty is mid-roster; gully's three and desert's two are
the two smallest holdings on the map, and the climate-selected kind is the
smallest of all. A kind authored far above its sovereignty floor is sharply
excluded away from its optimum — that is the stated cost of the authoring style,
and a standing guard is what keeps its habitat non-empty. Whether that is what
produced these two numbers is not established. Counting occupations does not
decompose them, and this campaign has already mistaken one difference for a cause.

## Two thousand worlds

The census refreshed once, at the close, on the one host permitted to author it.

The thousand-world settlement census rewrote **1000 of 1000 rows**, which is what
a new settling people always does: it re-decides the settlement contest on every
seed. The companion study rewrote every row too, and the reason is not the one
anybody would predict from that sentence. Its rosters are synthetic solo and twin
worlds, so a new settling kind never competes there and the study is nearly
immune to a roster change by construction. Every line moved for a *textual*
reason — the metric registry gained a monophyly column for the new family, and a
new column shifts every row.

Underneath that, five cells genuinely moved, and all five are **repairs**. On the
solo roster the laboratory had been reading a species out of one component set
and resolving it against a freshly assembled canonical one: it built a lexicon
from the canonical goblinoid family of three while scoring it against a family of
one, and duly reported a monophyly break in a world that had none. The null
control is symmetric now where it was not.

Eleven calibration witnesses were re-pinned to their newly measured values, with
every directional claim they encode re-checked rather than assumed. Blind
attribution still beats chance decisively at 0.9122 against a floor of 0.75. Name
transparency fell from 0.798 to 0.743 — the safe direction, since only a rise
back toward 1.0 would mean phonological wear had stopped. Homophony rose across
every goblinoid daughter, and bugbear still leads them by better than threefold.
The capacity-weighted latitude clears its preregistered floor by 6.3×. One cause
is named for all of it — three new peoples, a re-decided contest — and no comment
claims anything about *where* the dwarves settle, because nothing here measured
that.

## The two who were left behind

Mountain-dwarf and duergar were authored, measured, and withdrawn before merge.

They differ from the other three, and from each other, only in **depth**, and
depth is the one thing the model cannot say. The campaign tried: a chamber's
elevation could be computed honestly as the surface elevation minus the depth to
the top of the band the void reaches, both quantities already committed, no new
model required. Measuring the cave-depth distribution *before* building it is
what stopped it. Two thirds of caves sit between 0 and 1800 m; the remaining
third sits at fourteen to twenty-one kilometres, which is correctly
uninhabitable. And among the habitable shallow caves the median depth is **0.0 m
on every seed**. The coordinate exists; the variance does not. Subtracting it
would have moved almost nothing.

What actually exposed the problem was a question about a result the campaign was
proud of. Duergar, under the corrected diet, rooted more toponyms than any other
people at seed 42 — marsh, spring, valley — and this was written up as an
emergent finding: a fungal food web wants damp ground, and the world's place
names said so without being told. One question dissolved it. **Duergar had been
authored at an elevation optimum of 300 m to mean *deep*.** Depth below the
surface and height above sea level are different quantities — a deep chamber
under a mountain sits high above the sea, a shallow cave in a marsh sits low —
and the authored curve had selected lowland marshes, exactly as written. The
toponymy was reporting the authoring.

That is the same fake the preceding campaign spent itself removing, in its
low-elevation costume rather than its low-insolation one, committed by the
campaign whose founding purpose was to avoid it. A kind whose defining trait the
model cannot express is the trap this programme's own ladder of probe-validity
exists to name, and shipping two of them would have been the programme declining
its own acceptance criterion in the campaign that discovered the gap.

## What this leaves

**The underworld should become a place.** A cave's biome is a function of its
kind and the band its void reaches — both already committed, no new draws — and a
kind that declares which biomes it can live in generalises the realm gate, the
one mechanism in this model measured to *select* rather than merely modulate.
Travel already gates on biome; habitat would gate settlement. The sea's four
depth zones are the shipped precedent, and there is a rare, clustered,
deliberately unoccupied biome already in the world to prove that such a thing can
exist without anybody living in it.

**The resource basis is a vector space whose referent is a food web.** A trophic
*level* cannot be expressed without a *link*, so a decomposer axis cannot be
derived from what grows above it, and a people that mines cannot be distinguished
from a people that grazes. Three existing open questions gained this campaign as
a witness and one new one was opened; none of them were fixable inside a roster
change without destroying its attribution.

**And the thing the numbers actually asked for.** The family's three kinds
differentiate on one axis, elevation, because it is the only axis carrying
spatial information they are authored to read differently. The supply term is
flat for them; the climate term binds for one of them and buys almost no
separation. A dwarf family living in the rock, in a world where the rock is a
place with conditions of its own, would have three or four live axes instead of
one. That is not this campaign's claim to make. It is the next one's, and it is
now measurable rather than assertable.
