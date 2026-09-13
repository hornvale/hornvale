# The Tidemark

The claim was one sentence: **a people can live in the sea, and the sea can
stop supporting them.**

Both halves landed. Six marine peoples are in the registry and place
settlements; a hydrothermal vent entering its failed state now ends the
occupation it was holding up. But the campaign's two most useful results are
its nulls, and they are the right place to start, because each is a
measurement that refused to let the campaign ship something plausible.

**The sea has no food chain**, and that was measured before a roster was
authored rather than after. Every marine kind in the world — six peoples and
four fauna — resolves to trophic height *exactly* 1.000, and the food web
omits all ten as keys. The subsistence roster the design called for was
therefore not written.

**The campaign's own preregistered counter for the expiring habitat reads
zero on both quantities it names, and the zero is a property of the
instrument rather than of the world.** The mechanism fires; the counter
cannot see it. That is a measurement-design finding, and the campaign reports
it as one.

## A third realm, enumerated by the compiler

`HabitatRealm` had two variants, and the whole discipline of adding a third
was to let the type system produce the site list rather than a grep. Adding
`Marine` and rebuilding, one reported site at a time, yielded **seven** sites
— three more than expected, because two test fixtures carry copies of the
seating match and a probe carries an alive-occupation match nobody had
enumerated.

The realm gate is duplicated across a domain boundary by construction:
`species::HabitatRealm` and `climate::facets::Realm` are the same axis
expressed twice, because a domain may not depend on a sibling. The campaign
therefore owes an agreement test at the composition root rather than merely a
new variant, and it is two-directional — forward over the three realms,
backward over the three media — because a one-directional round trip cannot
see a new realm that reuses an existing medium.

The substrate curve for the new realm reuses the subterranean optimum and
width (0.85, 0.3 — a seabed meets a body the way a cave floor does) and steps
devotion down again, 1.0 → 0.8 → 0.6, on the reasoning that the habitat is the
water column and the floor is less accountable for a marine body's rest than a
cave floor is for a cave-dweller's. The constants are not the guard; the
*relationship* is, pinned so that a future edit setting marine devotion equal
to subterranean's reddens instead of passing every gate silently.

**Nothing moved when the variant arrived, and that was measured two ways.**
`sea-elf` and `giant-crocodile` — the two shipped kinds a reader meeting
`Marine` would reasonably reclassify — were given explicit `Surface` rows, and
scored twice against one built world: once with the registry as it now stands
and once with every kind forced to `Surface`. Both read 40,799 vertices of
non-zero suitability, and every vertex was bit-identical between the arms. The
positive control in the same run is the drow, whose two arms separate 874
against 11,283 — so the test can tell "nothing moved" from "the realm vector
is not read at all".

That is also where the registry's meaning changed: **presence in the habitat
store no longer implies non-`Surface`.** Fifteen rows today — seven
subterranean, six marine, and two explicit `Surface` — and absence still
defaults to `Surface` for everyone else.

## The overlay reaches a world

The Vent and The Living Vent had built a marine overlay — a water column with
five pelagic bands, seeded hydrothermal sources, and a five-state succession
from absent through nascent, active and weakening to failed — that no build
path constructed and no command reached. Two campaigns of a subsystem, real,
tested, and off the user path.

The first question was which of the world's **two** vent representations
seating should read, and it was measured rather than argued.
`climate::Biome::HydrothermalVent` is ridge-derived and takes no draw;
worldgen's `WaterVent` is a seeded admission over seabed samples and is the
only one carrying a phase. They do not coincide:

| seed | vent biome | water vents | intersection | overlap of the smaller set |
|---|---:|---:|---:|---:|
| 42 | 684 | 623 | 174 | 27.9% |
| 7 | 802 | 585 | 175 | 29.9% |
| 3 | 587 | 509 | 150 | 29.5% |

They are different phenomena, so seating reads `WaterVent` and the biome keeps
its own coarser consumer. The overlay is now built inside the history bake —
one frame deeper than the design named — so that the standalone measurement
entry point and the settlement stage cannot disagree about whether the sea has
vents in it, which is precisely the divergence between an instrument and its
subject that this project keeps recording.

The habitat is read at genesis, and the reason is stronger than convention: the
ambient fields are derived at genesis, so reading succession at any other tick
would put two instants inside one overlay with no consumer able to say which
world it was looking at.

**What the pelagic ladder actually distinguishes is narrower than its
vocabulary**, and saying so cost the campaign a correction of its own spec.
The five strata carry temperature, chemistry, light, moisture and depth; of
those, only depth *varies by band* — the column is handed one temperature per
vertex, moisture is a constant, light varies and is read by nothing, and
chemistry is per-vertex except at the seabed band of a lit vent. So a kind
whose identity is a *pelagic zone* is expressible and a kind whose identity is
"warmer water at depth" is not. The first draft of that finding named which
inputs *reach* the scoring arm; the correction is that reaching and varying
are different questions, and conflating them is how a plumbing check reads as
an effect.

## Six peoples

Tritons, merfolk, abyssal elves, and three functional roles that had to earn
names: a reef mason, a kelp tender, a vent commensal. They place as predicted
at seed 42 — three, four, four, three and three settlements for the five
settled kinds, against a surface and subterranean total of 368 — and merfolk,
being `Gregarious` rather than `Settled`, place **exactly zero**. That zero is
the two-sided half of the prediction: a kind that forms no fixed place must not
form one, and a one-sided floor could not have caught the opposite defect.

Merfolk also ended a coincidence the model had been leaning on. The society
component has been gated on *minded and social* since decision 0068, and
`Settled` was only ever extensionally the same set — "no minded `Gregarious`
kind yet". There is one now, so the two sets differ by one, and every surface
that read "the settling peoples" as "the kinds carrying a society row" had to
be re-derived from what the code iterates rather than from the set the author
had in mind. That mistake was made twice inside the campaign before it was
pinned.

**Distinctness was measured over all fifteen pairs**, excluding depth and
excluding any axis the ladder does not read. The minimum is five, at the reef
mason against the triton — but the honest number beside it is that for that
worst pair only **two** of those differences reach placement at all (mass and
biome affinity), because the capacity path takes a fast exit that computes and
discards temperature, moisture and insolation for every kind that ships
prepared. The measurement is read as "the six are distinct kinds", never as
"the six are distinct places".

Two authoring constraints were found by violating them, and both are the same
lesson. In-group radius is *banded* by a downstream reader with deliberate
refusing gaps, and an authored 0.45 panicked a roster. Night vision is
*quantized* by the eye model, and a value 0.05 from an existing kind's rendered
the same eye. **Authoring a difference finer than the model can render is
authoring a distinction the engine cannot see.**

### The whole ocean, and why a shallow kind takes it

Nobody predicted this one, so it is recorded rather than tuned. Under a
strict argmax of per-species capacity, the reef mason holds **26,344 / 18,213 /
26,769** vertices at seeds 42, 7 and 1234 — very nearly the ocean. Its
settlement count is entirely normal, four, so the campaign's placement
measurement could not see it; it surfaced only because the abyssal elf's
domination check printed the whole distribution.

The cause is structural. **Depth confines downward only.** Every ocean column
has an epipelagic band at height zero, so a shallow kind is near-optimal at
*every* ocean vertex, and among shallow kinds the argmax then falls to a ~0.4%
difference in the sovereignty floor — because a biome affinity's default *is*
the kind's floor. That is a knife edge, and it is competitive exclusion
arriving in a new realm rather than a knob this campaign should turn after
seeing the number.

The abyssal elf itself does not dominate: 336 / 368 / 522 vertices against the
shelf-confined sea elf's 182 / 443 / 19, about 1.9x its marine sibling against
a 10x ceiling. The elf family's anchor is weaker than it looks, though — the
wood elf holds **zero** vertices under an argmax instrument at all three
seeds, outcompeted on land, so the family order the comparison rests on is
carried entirely by the sea elf.

### A guard that could not be written, until it could

Wiring the overlay into the bake left one property unassertable. The question
is whether the bake hoists the *vent-bearing* habitat rather than the ambient
one, and the campaign measured — rather than assumed — that no outcome test
could see it: two synthetic marine probes were run through the real bake, a
chemotroph whose habitable set the vent layer widened from 403 to 740 vertices
and a thermophile whose peak capacity it raised from 61.05 to 104.83, and
**neither moved a single settlement.** The bake marches epochs; it does not
take a capacity argmax. Any outcome assertion built on either would have passed
under the reversion — a guard that cannot fire. What shipped instead was a
source-text scan, parked with the weakness stated: a change keeping the literal
in a comment while calling the wrong constructor would pass it.

Six real peoples closed that gap exactly as predicted. All three of the vent
commensal's seed-42 settlements sit on vent-improved seabed — a 6.3x capacity
lift on 0.82% of the globe's vertices — and mutating the hoist back to the
ambient habitat takes three of three to **zero of three** and relocates every
site. The behavioural guard replaced the scan's weak half rather than
supplementing it. **A property can be unguardable because the roster is too
thin to exhibit it**, and the remedy is to record why and re-ask later, not to
write the assertion that would have passed anyway.

### A phototroph whose photosynthesis does nothing

The kelp tender weights photosynthate at 0.40. That weight moves capacity at
**0 of 29,679** submerged vertices, because the supply that feeds it is
terrestrial by construction. The producer exists — the overlay derives plankton
from depth-attenuated light — and the consumer exists, and nothing wires them
together: light is read by no scoring path, plankton by nothing but test
assertions.

The general form is worth more than the instance. An axis with no producer and
a producer with no consumer look alike from a distance and are not the same
defect: **a missing half needs vocabulary built; a missing join needs a wire
run.** This is a missing join, inside one campaign's own code, and it was left
standing and recorded rather than closed, because closing it would have moved
every marine world for a reason unrelated to the campaign's claim.

## What the sea turned out not to have

The design called for a subsistence roster — nine fauna under the six peoples,
a base for a predator guild. Before authoring any of it the campaign measured
what the roster would have to interact *with*, and the premise did not
survive contact.

Every marine kind resolves to trophic height exactly **1.000**: all six
peoples, and the reef shark, giant octopus, killer whale and giant squid that
were already there. The web omits all ten as keys — not present with an empty
prey list, absent entirely — because the model's predator test reads the
animal-prey axis and marine kinds spend no weight on it. By this model's own
classifier, **a killer whale is not a heterotroph.**

The control in the same run proves the harness sees edges when they exist: the
black dragon's prey list holds 38 entries, nine of the ten marine kinds among
them. Marine kinds can be eaten; they can never eat.

Competition, unlike predation, is real and already saturated. Pairwise niche
overlap among the ten is **1.0000 for eight of them** — four peoples and all
four fauna carry byte-identical niche vectors, so a 5,400 kg killer whale and a
merfolk settlement compete at exactly the coefficient the model would give two
merfolk. Only two kinds separate at all: the kelp tender at 0.8321 and the vent
commensal, the least-overlapping kind in the sea, at 0.3162.

So nine more kinds on the one marine axis would have added mass to an already
maximal cluster and called it a food chain. **No fauna were authored.** The
design that would make a marine roster mean something — diet as receptors and
co-location rather than as which substance-bucket a kind draws from — is
written down in full, registered as an idea, and handed forward as its own
campaign. The deliverable here is the probe and the finding.

One reading had to be corrected on the way, and it is the design's spine: the
black dragon's 38-entry prey list is not absurd. It is a correct statement of
*capability* misread as a statement of *realization*. A dragon can eat an
abyssal elf, and where they meet it would. The defect is one missing
co-location filter, not a broken derivation.

The same whole-registry readout turned up something the campaign was not
looking for: six *terrestrial* kinds sit pinned at height 1.000 as well,
because detritus-dominance pins a kind off the chain by convention. Carrion
retains identity — a species, a mass, a place — and detritus does not, and the
model has one axis for both. It is the marine flatness again, one axis standing
in for a structure, and it was invisible until the sea forced the whole roster
to be read at once.

## The habitat expires, and the counter that could not see it

The mechanism works. A vent's candidate ring — the fixed set of vertices a
source can ever light — is the index, because a failed vent is *nowhere* and
its present position cannot answer which ground it was under; a vertex two
vents light does not go dark while one still burns. When every hosting source
has failed during an occupation's own tenure, that occupation ends with
`Ended::Nature`, taking the same fork the bake already takes when the land goes
poor: somewhere to go and enough of them left, and they migrate and refound;
nowhere, and it is famine. No new cause variant, and no match on the cause
enumeration at all, so there is no wildcard to reclassify a breached delving.

Over the real seed-42 bake, of **1,259** occupations, four sit on ground a vent
hosts; two of those saw every hosting source fail during their tenure, and both
ended on that epoch. The overlay admits 623 vents across 2,149 hosted
vertices. So the reachability prediction is real in the other direction: the
bake seats a people on vent-hosted ground only 0.3% of the time, and when it
does and the source dies, the mechanism fires two out of two. Nothing was
retuned to reach that.

**The preregistered counter for the capacity half, however, reads zero on both
quantities the plan named — and both are falsifiers the mechanism cannot
produce.** Availability is a `{0,1}` mask over whether a water column exists at
all, and no vent adds or removes a pelagic band, so it is time-invariant by
construction. Capacity is a Michaelis-Menten saturation over a *summed* supply,
and the vent commensal weights marine forage beside chemosynthate while every
ocean vertex grades strictly above zero on forage, so an exact zero is
unreachable. The blindness is itself under guard: a test asserts that capacity
at every vent-moved vertex stays strictly positive across the sweep, so if the
counter ever *can* fire, the zero is due a re-reading.

On the axis a vent actually feeds, with an ablated control at zero to license
the attribution, the claim holds plainly: the vent-borne supply goes positive →
exactly nothing at **1,020** vertices, capacity moves at **1,104**, and at the
worst-hit vertex it falls to **14.9%** of that vertex's own peak.

**Writing the control first is what caught the plan's control being wrong.**
The plan specified holding vent phase constant. Implemented literally — instants
one whole cycle apart, so every vent state is bit-identical while the clock runs
1,100 days — it counted **1,435**, not zero, because freezing the phase freezes
the *succession* and leaves the *migration* running: a vent's position advances
with the cycle index. The real negative control is the ablated overlay, which
holds the whole vent layer out while the clock advances, and reads zero on all
three quantities. A control that is written after the live sweep is a control
that gets believed.

## Reach is not residence

A sea elf carries both a swim locomotion and a `Surface` residence, and until
now the second did all the work: the climate realm's access mode had **zero**
production readers anywhere in the tree, and diving gated only on whether there
was water present, so every body alike took the whole column and a swim row
bought nothing.

Reach is now asked of locomotion against access — nine arms, no wildcard, so a
future aerial realm cannot be silently admitted to every walker — and nothing
in that path reads the habitat realm, which is the separation's whole content.

**The gate is narrower than the first draft, and the narrowing was measured
rather than argued.** Refusing the water column outright to every non-swimmer
— which is what "reach is a capability" sounds like it means — reddened 12 of
1,234 vessel tests, and only four were about diving; the rest were
walk-determinism transcripts and committed fixtures, moved because a body that
used to enter the water no longer did. That is a larger claim than the design
makes, and the swim capability's own definition — *crosses deep water*, against
a walker's *walks and wades* — was already the narrower reading. A walker now
wades the sunlit band and goes no further; a sea elf takes all five.

The column also gained an observer. The Living Vent authored an
ordinary/diagnostic split in how a vent is reported — present consequences
versus an inferred phase, named as inferred — and nothing had ever called it. A
`water` command is that caller, and it is pinned through the real binary,
because a command hardcoding the diagnostic flag to false would leave every
library-level test in the workspace green.

## What moved, and what is owed

Placement changed, so worlds changed. Seed 42's committed fact count went
**23,431 → 25,281**, and the attribution was measured off the golden's own
predicate histogram rather than inferred from which commits landed:
settlements 396 → 435, occupations 1,148 → 1,259, ruins 752 → 824. That is a
settlement-placement shape, and the bulk of it is a *fixture selector* repair,
not the vent ending — whose own measurement is two occupations. The pin's
documentation says so, so that nobody later reads +1,849 facts as the price of
a vent mechanism.

That repair is worth naming because a board notice from another session found
it, not a review. The demo walk's subject selector filtered settlements on
*not marine biome* — and a subterranean settlement commits its vertex's
**surface** biome, so a hold under a forest passed the filter clean. It was the
same defect the campaign had just fixed one level up: "the first settlement in
ledger order" had become "the first non-marine-*biome* settlement in ledger
order", still a negation of the wrong axis. It now selects on the positive
property, using this campaign's own machinery — the settlement's people
resolves to the surface realm — and it is mutation-verified in both directions
on a seed chosen by sweeping for the failing shape rather than pinned by hand.

The deeper version of that story is the one to carry: **the fixtures had never
chosen a subject.** They took "the first settlement in ledger order" and
treated it as "the demo village", which it never meant. Adding any people
reorders the ledger; adding marine ones merely made it visible, by opening the
canonical demo walk in open water at the bathypelagic band. The remedy was to
make the choice explicit rather than to move the world — and regenerating with
the explicit target left the affected fixture byte-identical, which is the only
check that distinguishes *naming* a behaviour from *changing* it.

### A preregistered invariant fell, and neither gate could have caught it

Absorbing main brought four subterranean peoples from a campaign that had
landed in parallel. On the merged world, a preregistered survivorship claim from
an earlier campaign — that breached delvings are deeper *for their tenure* —
stopped clearing its own significance threshold.

The attribution was measured rather than assumed, and it is the interesting
part:

| roster | verdict |
|---|---|
| main alone, four subterranean peoples | green |
| this branch alone, six marine peoples | green |
| the merge, both | red |

Neither campaign's gate could have seen it: each was green on its own product.
The two did not even change the same idea — both merely widened the roster, and
the conditioned statistic is sensitive to roster width.

The root cause is not a refutation. With 32 breached workings across a
twelve-seed panel, four of five tenure strata carry fewer than ten, and two of
them carry one and three observations while holding a fifth of the pair mass.
**The claim is not evaluable on this panel** — which is a stronger and more
useful result than either supported or refuted, and `refuted` would assert a
negative the data cannot carry. The measured yield is 2.67 breaches per seed, so
ten per stratum needs about nineteen seeds against today's twelve.

What shipped is a power gate rather than a deletion or a re-tuning. The
assertion is armed only when every stratum clears ten breached workings, and
otherwise prints an explicit *unevaluated* report naming the shortfall per
stratum and the seed count that would close it. The significance threshold is
untouched; the gate **re-arms itself** the moment the panel grows past the
floor, with no edit to the file. Both halves of the test are gated together,
deliberately: the direction clause was still passing on the same one-and-three
strata, and a direction assertion on four observations is no better evidenced
than a significance one.

---

The campaign leaves four things owed, named rather than implied. The marine
tolerance substrate is frozen at genesis across every paleoclimate era, so a
marine kind's tolerances are scored against genesis conditions through a
multi-millennia bake — a ruled hoist, not an invariance, and a different
campaign's cost. The plankton-to-photosynthate join is unbuilt. The trophic
receptor model that would give the sea a food chain is designed and unstarted.
And the aerial realm is still the empty fourth sibling: the medium axis is
land, water, air, and two of the three now carry peoples.
