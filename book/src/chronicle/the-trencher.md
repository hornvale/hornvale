# The Trencher

The underworld had a food system that could not say what anything ate.

Supply was modelled at high resolution — seven chemical reactions, nine
lithology fields, per depth rung, per vertex. Diets were named at low
resolution: seven `ResourceAxis` values. Between them sat one number, produced
by averaging the seven reactions together, and the average had been chosen
empirically to dodge a clamp rather than designed to mean anything.

That mean was doing something worse than losing precision. It was averaging
across category boundaries. Five of its seven inputs were chemical foods; one
was a thermal gradient; one was a pile of surface detritus that already had an
axis of its own. A chamber rich in hydrogen and a chamber rich in methane
produced the same number, and so did a chamber that was merely warm.

## What the ceiling looked like

The predecessor campaign measured it. Across twelve seeds, at every depth rung,
the derived `ENERGY` reading never once reached the corpus band for `fed` —
the largest value anywhere was `0.424277`, against a scale whose upper bands
are `rich` at `0.75` and `teeming` at `1.0`. The underworld could not describe
itself as well-fed, and the reason was not that the rock was poor. Dividing
seven numbers by seven compresses them toward the middle, and the middle of
that scale is `lean`.

## The trichotomy

Before the supply could be fixed, the vocabulary describing eaters had to stop
lying. `TrophicMode` had been a single enum conflating three independent
questions: where an organism gets energy, where it gets electrons, and where it
gets carbon. A photosynthesising plant and a chemosynthesising bacterium differ
on the first; a plant and a cow differ on the third. One axis cannot hold that.

It is now three — energy source, electron donor, carbon source — across all
thirty-nine kinds, with a table of sanctioned triples. Three combinations are
in use. The rest are legal to write and refused by a test, which is the point:
a vocabulary that cannot express a wrong thing cannot express a surprising one
either.

## The sum, and what it cost to make it safe

The mean became per-metabolite sums. Four axes were appended to the basis —
hydrogen, reduced iron, reduced sulphur, methane — with chemosynthate retained
as their aggregate for generalists. Two reactions yielding the same molecule
add. The thermal gradient left the food vocabulary entirely and became a
bounded multiplier on the chemistry it accelerates. Detrital import went to the
detritus axis, where it always belonged.

The first cut of this overshot. Summing four metabolites produced per-rung
medians above `1.0` — past `teeming` at the median of every rung, which is the
same pathology as the mean with the sign reversed. A clamp downstream was
quietly flattening it, so the mechanism the campaign existed to build was being
destroyed by the campaign's own fix.

The repair separates two things the code had conflated in one function: how
much chemistry exists, and what the legacy ruler reports. The raw sum stays raw
— it is a resource magnitude, and every other supply in the scoring vocabulary
is an unbounded magnitude that saturates at the point of use. The *ruler* takes
a Type-II projection, the same transfer function the model already uses to turn
supply into suitability. Rescaling the corpus bands instead was considered and
is not expressible: the kernel's `EnvironmentVector` refuses any value outside
`[0,1]`, so the bands are a contract, not a convention.

Per-rung medians now read `0.501 / 0.568 / 0.666 / 0.677 / 0.677` — inside the
`fed` band, ordering preserved, nothing clamped.

## A gate for the things that are not physics

`thaumic` had been a reserved lithology field, hardcoded to zero at every site
since it was set aside, waiting for a world that admitted magic. It now has a
gate: a metaphysics pin, default-off, and a derivation behind it that reads
faults, hotspots and deep time — ley-lines, mana-wells, hallowed ground — from
terrain the world already owns. No new seeded draw, so no epoch.

An unpinned world is byte-identical, and that is not asserted on trust: a
control builds the same seed twice, charged and inert, and reports which fields
moved. `thaumic` moves at 12,512 of 40,962 vertices. Every other axis —
silica, grain, induration, carbonate, metamorphic grade, porosity, margin, soil
depth, basement, elevation, ocean, rock — moves at zero. The control was
written by a different hand than the derivation, and it was shown to go red
under three separate mutations before it was trusted.

## What this campaign did not do

It did not author the biota.

No organism in the world weights any of the four metabolite axes. The campaign
landed a supply vocabulary that nothing eats — a producer with no consumer,
which is precisely the defect it spent its first stage learning to name in
other people's code. It is a capability, not a behaviour, and every measurement
here that looks like a result is a baseline for whoever adds the first eater.

That was a deliberate cut. A parallel campaign is designing how organisms
*acquire* food — receptors keyed to the eater's capabilities rather than to the
substance-bucket it draws from — and authoring a roster in the vocabulary being
replaced would have meant authoring it twice. The two models turn out to be
complementary rather than competing: receptors discriminate eaters, supply axes
discriminate environments, and no receptor can say *this chamber has methane
but no hydrogen*, because that is a fact about rock.

## The honest caveat on the result

Four registered axes are not four usable metabolites.

Hydrogen spans `0.312–0.423` across the rungs and reduced iron `0.292–0.415`.
Methane sits at `0.025` and barely moves. It is not undiscriminating so much as
**dim**: methanogenesis is the only one of the seven reactions that is a bare
product of three unnormalised terms, where its siblings either saturate their
water gate or normalise their shape term to peak at one. Since supply is
summed, absolute magnitude is what an eater receives — so methane is roughly a
fourteenth the axis that hydrogen is, and a creature that named it instead of
hydrogen would take that cut for the privilege.

That asymmetry is older than this campaign. It was invisible while seven
reactions were averaged into one number, because an average has no per-input
scale to inspect. Disaggregation made it legible. The first finding a better
instrument produced was about the instrument it replaced, which is usually how
that goes.

**One clause above is wrong, and the correction is the interesting half.**
Methanogenesis is not *the only* reaction that is a bare product of raw terms;
it is the only one that is *entirely* raw. Sulphide oxidation is also a
three-term product and one of its three — metamorphic grade — is an unshaped
buffer fraction exactly like carbonate and porosity. The sharper sentence made
a cleaner story, and the cleaner story was false.

What survives is the arithmetic, which is what mattered: a product of `k` terms
each in `[0,1]` has mean `1/2^k`, so a three-term reaction sits at half a
two-term one whatever its chemistry says. Both depressed axes — sulphur and
methane — are fed by three-term reactions; both of the healthy ones are fed by
two-term reactions. The asymmetry was arity, not chemistry.

## The yield form, and the seed that was an anecdote

The repair is the geometric mean: a reaction's yield becomes the `k`-th root of
its `k` gating terms, so it reads *typical gate satisfaction* rather than
*joint probability*, and a reaction is not penalised for having been described
in more detail. It is a rule, not a fitted constant, which is the only reason
it is admissible at all.

Applied to all seven reactions it cost seed 42 forty-four percent of its facts.
Applied to the five that feed the four metabolite axes, it costs **nothing** —
five seeds produce byte-identical worlds.

Both sentences are about the same change, and the difference between them is a
reaction that is not a metabolite at all. Detrital import is alone on its own
axis, competes with nothing, and — unlike the four metabolites — is eaten by six
species whose appetites were calibrated against it. Correcting its arity is not
a fairness correction, because there is nothing for it to be fair *to*; it is a
recalibration of six niches wearing a fairness correction's clothes.

Isolating that took a per-arm bisection: six of the seven reactions moved
nothing, and the seventh moved everything. The forty-four percent was also
**one seed's anecdote** — across five seeds the same change costs about ten
percent, and seed 42 is a three-to-fourfold outlier. A number measured on one
world was reported as an effect size, which is a mistake this project has a
name for and made again anyway.

The world's response to that axis is not even monotone. Holding everything else
fixed, the shipped value sits on a local maximum: zero scores lower, doubling
scores lower, and a constant one scores lower, while the square root — which
lies numerically *between* the shipped value and one — scores lowest of all.
The history bake consumes the capacity field's **ordering**, which is
discontinuous, not its magnitude. Every argument made by reading the code was
correct about the inputs and useless about the result.

## A chemistry that nothing eats

The reason the metabolite half was free is worth stating plainly, because it is
the campaign's largest finding and it is an absence.

No species wants any of it. The count of appetites weighting hydrogen, reduced
iron, reduced sulphur or methane is **zero, on all four**. The axes are
computed, measured over a hundred thousand readings, and narrated to a player
who walks into a chamber — and eaten by nobody. That is why changing all four
produces an identical world, and why every calibration here is unfalsifiable by
the world in the ordinary way: no settlement moves, no creature is mis-sited,
no committed artifact drifts when a number is wrong.

It is a gap in the data, not in the machinery. The capacity calculation already
carries all four axes with a per-species weight; an author writes one appetite
vector and it works. That is now proven rather than asserted, by a test that
builds two synthetic creatures identical in mass, tolerance, realm and
temperament and differing *only* in which axis they eat, and measures that the
one weighting methane is fed better where methane is abundant — at thousands of
vertices, in the right direction, on three seeds, with zero exceptions.

So the honest description of this campaign is **preparatory**. It makes the
chemistry coherent, bounded and describable so that something can later be
authored to live on it. The freedom to change these numbers cheaply is
temporary: the first appetite that names methane makes every one of these
choices world-affecting.

## Naming a place by its chemistry

Two surfaces make the chemistry sayable.

The first is a band ladder per axis — absent, trace, thin, ample, abundant —
with cuts in raw units, chosen per axis because the four are not alike.
Hydrogen is absent from two vertices in five; methane is absent from none,
anywhere. A single shared ladder would call methane present everywhere, which
is true and useless.

The bands describe depth, which is what they exist for. Reading the share of
each axis in its top two bands from the shallowest rung to the deepest:
hydrogen rises from `0.158` to `0.384`, iron from `0.549` to `0.670`, methane
from `0.272` to `0.580`, and sulphur from **exactly zero** to `0.460`. A
sulphur-eater cannot live near the surface; at the bottom of the ladder nearly
half of all chambers would feed one. That zero is the thermal front reproduced
from a second instrument, which is the kind of agreement worth more than either
measurement alone.

The second surface is a sentence. A chamber already told a player which
reaction fed the creature living in it; it now says whether that reaction leads
comfortably or barely — *"drawn to the porous, water-logged carbonate, though a
sulphide-laced seam runs it close"* — on the twenty-eight percent of readings
where the runner-up comes within a fifth of the leader. A place with a
character is not the same as a place with a maximum, and until now the prose
said the same thing about both.
