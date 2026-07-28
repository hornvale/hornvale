# The Vacancy

The roster held sixteen kinds. The world model declared far more states than
sixteen kinds could inhabit, and nothing in the codebase could tell the
difference between a state nobody had gotten to yet and a state that was
quietly broken.

That is the whole campaign in one sentence, and it is worth dwelling on why it
is not merely a tidiness complaint. An enum variant no creature carries, a
consumer branch no data reaches, a supply axis pinned at zero — each of these
compiles, passes every test, and produces no error. They are indistinguishable,
from inside the program, from features that work. The only way to tell is to
put something in them and look.

So the campaign's deliverable was never "thirteen creatures." It was an
instrument, and the creatures were what made the instrument say anything.

## Three ways a declared thing can fail to exist

The distinctions that organised the work fell out of asking what "declared but
not really there" actually covers, and finding it covers three different
diseases with three different cures.

A **stale claim** is a doc that has outlived its code. `MetabolicClass::Autotroph`
carried a comment explaining that a phototroph's basal rate is surface-limited,
so Kleiber's ¾ mass exponent does not apply, and that activating the class would
be its own modelling decision — "unused seam." The class had acquired two users
three campaigns earlier, and the allometry gives it an endotherm's constants. The
treant and the twig blight are, arithmetically, warm-blooded animals shaped like
plants. The cure is a doc correction and a test that pins the divergence so the
eventual fix arrives as a visible diff rather than a silent shift in numbers.

An **unreachable branch** is code no data can enter. `StatusBasis::Generosity` had
existed since the status system was written and no species had ever carried it;
the branch that gives a society-mind to a minded, non-settling creature had
existed since the decision that motivated it and had never had an instance. The
cure is a witness — which is what a roster expansion is *for*.

An **admissible void** is the dangerous one: a kind that loads, satisfies every
referential-integrity check, and has zero carrying capacity on every cell of
every world. Present in the registry, absent from the world, no error anywhere.
The cure is not a witness but a refusal, and the campaign's cheapest, longest-lived
artifact is the test that supplies it: no kind may be void.

## The instrument found four ghosts on its first run

The occupancy readout — thirty worlds, every kind, where each actually lives —
came back with twelve of sixteen kinds.

`ANIMAL_PREY` supply was hard-coded to `0.0`, a placeholder its own comment
admitted to. The three chromatic dragons and the owlbear are authored as pure
obligate predators. Zero supply, saturated, multiplied by any condition response
whatsoever, is zero. Those four kinds had never existed in any world the
generator had ever produced — and one campaign earlier, another had given those
dragons minds, perception, and a frozen Draconic tongue that nothing had ever
carried.

They were exactly the admissible-void case the campaign had defined and declared
must be refused rather than witnessed, sitting in the roster the whole time. The
fix is a prey supply field: a trophic-transfer fraction of forage, Lindeman's
ten percent, parallel to forage already being a fraction of primary production.
One constant, physically anchored, and non-circular by construction — it reads
production, never predator or prey populations.

## The sea was never closed

The campaign initially scoped aquatic life out, reasoning that the land mask
zeroes every resource-supply axis on submerged cells, so a marine kind would
starve everywhere. That reasoning was wrong, and the codebase said so in three
separate places.

The mask's own doc: *an aquatic kind arrives by authoring a marine supply axis
and a supply field defined on water, not by an exemption from a global rule.*
The carrying-capacity assembly: *nothing in this assembly special-cases water; an
aquatic kind authored onto a future marine supply axis would get a non-zero K at
sea from this same product, unchanged.* The axis basis: *the basis is open.*

An earlier campaign had put the mask on *supply* rather than on assembled
capacity precisely so that this day would need no exemption. The blocker was
self-imposed, produced by reading one sentence and stopping before the next.

Opening the ocean therefore cost one new axis and one supply field derived from
what climate already computes — the marine biome class, sea-surface temperature,
depth through the euphotic zone. Nine of the ten marine biomes now carry
occupancy. The tenth, the hadal trench, is absent from the generator entirely.

The design's proof is a crocodile. An amphibious kind weights both a terrestrial
axis and the marine one, and the sparse uptake vector and saturating sum handle
it with no special case anywhere — which is the outcome that would have
falsified the whole approach had it gone the other way.

## What "inhabited" turned out to mean

A late measurement changed the campaign's own claim about itself.

Chasing a report that the fifth people had moved seed 42's settlement count from
81 to 203, the count was measured three ways — at the campaign's base, with the
prey field reverted, and with it applied. Two hundred and three, all three times.
The number had been 203 before the campaign began; 81 was a stale reading from an
earlier campaign that nothing pinned.

The reason it could not have moved is structural. The deep-history bake supplanted
the demography placer as the settlement provider, and the bake reads plain
carrying capacity, not the per-axis niche product. Both supply fields this
campaign added feed the habitat model, the occupancy readout, and the laboratory's
coexistence readout — never genesis. And every fauna kind appears exactly once in
a generated world, as a registry declaration; only peoples appear substantively.

So the sea being inhabited means marine kinds have habitat, occupancy, and
competition exactly as the twelve already-viable terrestrial fauna do. It does not
mean a shark turns up in a world's ledger, because no fauna does. The gnoll — the
fifth people — is the one kind this campaign put into worlds, and it did so
emphatically: settlement-holding peoples went from four to five, with the gnoll
third-largest at twenty-seven settlements.

## The measurement that did not go the campaign's way

Six of seven preregistered criteria were met. The seventh is the interesting one.

The campaign asked that hot-arid, savanna, and boreal each gain a kind *centred*
there. Hot-arid did: the giant scorpion is the top-ranked desert occupant. Savanna
and boreal gained real presence and no dominance — and in both, the top slot
belongs to the treant, a sessile autotroph.

That is not bad luck. Carrying capacity is a supply term spanning orders of
magnitude multiplied by a condition product bounded in the unit interval, so an
authored niche can only modulate the primary-production signal, never select
against it. A photosynthate kind rides that signal wherever the world is green.
A predator authored for a particular climate cannot outrank it there.

The gnoll is the sharpest case. A people authored explicitly for hot-arid desert
has *zero* desert occupancy, and its largest share is temperate forest. Its niche
was deliberately left untuned: fitting the world to a preregistered criterion is
the one move that would have made the result worthless. The criterion is recorded
instead as a failing, ignored test that names its prerequisite — the same move an
earlier campaign made when its payoff turned out to need a mechanism that did not
yet exist.

A related discovery is quieter and probably more consequential: the axis species
author a light preference against is a pure function of latitude, with no canopy,
cloud, or terrain shadow. At a given latitude a rainforest floor and an open
desert receive identical light. Several shipped niches describe shade the model
cannot represent.

## What the roster now exercises

Twenty-nine kinds. `StatusBasis::Generosity` has its first witness in the
project's history. A herding predator exists in three climates and one ocean,
where every herd before was a pure grazer. A decomposer that cannot move.
An amphibious uptake vector. Nine marine biomes with occupants.

And four cells stay deliberately dark, each recorded with the reason rather than
left to look like an oversight: a nomadic minded people, whose blocker is that
settlement-free peoples are unaudited downstream; a second ametabolic kind, which
belongs to manufactured life rather than a biosphere; chemotrophy, which is a
fifth metabolic class and would make every reader of that enum a blast radius;
and an aquatic *people*, blocked not by condensation — which is not land-gated at
all — but by everything downstream of a settlement assuming land.

That last one has a companion finding. Putting sharks in the roster immediately
reddened the health battery: an agentified reef shark reads ninety-four percent
thirst-caused distress, because the walk layer gives every creature a freshwater
thirst drive it satisfies by travelling to drinkable water, and there is no
underwater locale. The guard is narrow — the wild never mints a predominantly
marine kind — but the finding is general. The drive vector assumes every creature
is a land animal, and a xorn burrowing through stone does not drink either.
