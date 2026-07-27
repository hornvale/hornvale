# The Waterline

This campaign set out to fix something real, was wrong about it twice, was
overtaken by a better fix while it worked, and shipped a guard it had not come
looking for. It is the shortest chronicle here and the one with the most
lessons in it, so it is worth telling straight.

## What it went looking for

A feasibility probe for the prey field — the deferred axis that would let
predators and dragons hold ground — went looking for one number and found
something else. Land creatures held carrying capacity below sea level. The
goblin appeared to dominate 930 cells, every one of them ocean. A campaign was
declared: the habitat model had no concept of **medium**, and the prey field
could not be built on a substrate where the best hunting was underwater.

## Where it was wrong

The probe classified ocean as *elevation below zero*. Sea level on this world
is **−2,936 metres**, and terrain had published the real predicate all along —
`is_ocean`, elevation below *sea level*. The two disagree on 8,162 cells.

Every land-and-ocean figure the campaign had produced was wrong. The world is
27% land, not 7%. The goblin's 930 cells were land, and always had been. Prey
production is 92% terrestrial, not 78% marine — which dissolved the argument
that the prey field needed this work first. And climate's habitability mask,
which the campaign had accused of not being a land test, is one: its first
clause is `elevation >= sea_level`.

What survived the correction was a single genuine defect. Of the five
resource-supply axes, `PHOTOSYNTHATE` and `PLANT_FORAGE` ride the habitability
mask, `ANIMAL_PREY` is a placeholder zero, and the remaining two — `MINERAL`,
read from lithology that terrain defines on the seafloor too, and `DETRITUS`, a
global constant — consult nothing. The kinds that eat them ate at sea.

## Where it was overtaken

While this work was in flight, [The Tumult](./the-tumult.md) fixed the same
defect and reasoned about it better.

It found the deeper cause: the leak had been masked by an accident. Before The
Tumult re-datumed elevation, an ocean cell sat about four kilometres from every
authored elevation optimum, so the Gaussian condition term zeroed the seabed
for everyone. Correcting the datum brought ocean cells to roughly eleven
hundred metres below sea level and exposed a gap that had been there all along.

And it chose the better place to stand. This campaign had gated **carrying
capacity** on a per-kind medium: a creature declares whether it lives above or
below the waterline, and the model excludes it elsewhere. The Tumult masked the
**supply fields** instead, and argued the difference exactly:

> Multiplying assembled K by a land mask would state "nothing lives in water"
> as a law of the model — a law that would have to be unstated the day an
> aquatic kind is authored. Masking the supply says the narrower, truer thing:
> *these* resources are land resources.

That is the right answer, and not only because it is simpler. A species'
`ResourceVector` is already a resource-hypervolume niche in the Hutchinsonian
sense; habitat is that niche's shadow, not a second axis beside it. A declared
medium is the older Grinnellian idea — habitat as a place a creature occupies —
bolted onto a model that had already moved past it. Worse, two mechanisms can
disagree: a kind with an aquatic diet and a terrestrial medium is expressible
and incoherent, where supply-masking makes that state impossible to write down.
Habitat comes out of what a creature eats, and nothing needs a per-species
exemption.

So the medium axis was removed — not because it lost a race, but because it was
the wrong shape for this model. The removal changed no world: every shipped
kind was terrestrial, for which the gate was redundant with the supply mask, or
was the xorn, for which it permitted everything.

## What it shipped

**A lock on the census.** The project has one canonical machine for census
goldens, because the machines are not byte-identical: on about a tenth of a
percent of values — discrete counts settled by a comparison upstream of the
quantize-at-emit boundary — two boxes differ by one. Quantization cannot
un-flip a count already decided.

Nothing enforced this. Every script comment restating *"this box is the single
canonical platform"* was written on that box, so read anywhere else it asserts
that wherever you are is canonical. Asked to regenerate the census, this
campaign was one command from doing it on the wrong machine.

The failure would have been silent. Not an error — a thousandth of the values
quietly wrong, and then drift-checked green forever after.

The guard now refuses on every path that can write those goldens, including the
one that mattered most and was found last: the `lab run` command printed in the
project's own documentation, which no shell guard could have caught. It is
declared once and read twice, and it fails closed.

**And two findings**, both larger than the campaign that produced them.

The first came from an attempt, made and withdrawn. The sovereignty floor —
which grants a creature a fraction of full habitat response *everywhere*, so
that a goblin is a third at home in the abyss — was replaced with a form that
decays: sovereignty as the fraction of the environmental gap a creature closes
for itself, which is the same algebra whether you read it as a dragon warming
its lair or as a wider tolerance band. It measured badly, and interestingly.
Coexistence diversity fell to 1.33 against a preregistered band of 1.5 to 3.0,
on every seed. **The diversity this project calibrated β to produce was
substantially an artifact of the floor**: with it, a badly-suited species held
comparable capacity everywhere and took a real share; without it, the cell goes
to a single winner. The constant β was frozen by a sweep conducted under the
old response shape, so it no longer means what it was chosen to mean. That is a
calibration study, and it is now its own campaign.

The second is smaller and sharper. Seed 42's ledger was byte-identical through
every version of this work, and the census still moved — one metric, on all
thousand rows. The census reads the coexistence *shadow*, not committed facts.
World-identity neutrality is not evidence of census neutrality, and checking
the first tells you nothing about the second.
