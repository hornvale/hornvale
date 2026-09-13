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
