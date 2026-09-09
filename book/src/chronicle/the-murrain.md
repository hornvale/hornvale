# The Murrain

*The murrain is a pestilence named from its host. This campaign gives the
world disease without pretending that a named sufferer is required for a
pathogen to exist.*

## The population beneath the person

The Murrain began with a question about scale. A settlement is not its named
inhabitants, and a disease does not need a materialized kobold in the room to
decide whether it can persist. The authoritative population is therefore a
statistical substrate: reconstructed live population over committed occupation
trajectories, connected through each era's graph. It carries cohorts,
person-years, host populations, and metapopulations without minting every
person.

The Lot remains a projection of that substrate. It draws a salient life, a
composite case, or an aggregate readout for observation and play. A composite
may be an in-world physician's typical patient or historian's representative
farmer, but it is not a causal person. Only aggregate state and materialized
individuals can write consequences back. The projection is deliberately
weighted for salience and legibility rather than pretending that a random draw
will introduce the king.

That gives Hornvale two linked population truths rather than two competing
ones. The substrate answers whether a disease can persist and how a settlement
changes. The projection answers what a player, physician, or historian gets to
see.

## Two diseases

The campaign separates diffuse disease from epidemic disease. Endemic burden is
a draw-free read over the substrate, biome, water, and committed pathogen
arrivals. It names what background and infant deaths were from without adding a
second mortality term to the already calibrated Siler hazard. An outbreak is a
world event: it consumes the bake's sequential `history/bake/v4` stream,
crosses the era graph, removes susceptible population, and can end an
occupation by plague.

The pathogen catalogue has five authored species rows. Their host weights,
environmental niches, and arrival/endemic virulence are data; persistence is
code in the kernel-only epidemiology crate. Persistence is judged on the
connected metapopulation, not on one village. That distinction matters: the
largest present connected population in the nine-seed preregistered panel is
7,882, while no era/component reaches 10,000 in the Task 0 measurement. A
crowd disease therefore does not become a hidden smallpox generator merely
because one settlement has a large peak.

The epidemic still makes the world more than a static disease table. It emits
paired `struck-by` and `outbreak-deaths` facts, preserves the struck occupation
as place, and gives the event its own identity. Raids and later history see the
population that survived; the Lot can name an outbreak or an endemic pathogen
without becoming the population authority.

## What the instrument found

The canonical nine-seed readout passed H-P1, H-P2, H-P5, and H-P6. H-P3 was
falsified: the preregistered plague-ending band was too high on seven of eight
growing seeds. H-P4 was also falsified: outbreak-event counts were below the
preregistered lower bound on seven of eight. Neither result was repaired by
retuning a constant. The failures are measurements of the chosen world and
instrument, not reasons to erase the prediction.

The new laboratory surface records largest present metapopulation, endemicity,
plague endings, outbreak events, named disease deaths, and Lot slot fill. The
population-layer seam also made an important negative result visible: disease
attribution does not change Siler magnitude or life expectancy in this
campaign. A future campaign that wants disease to move `e₀` must re-derive the
background mortality model rather than multiplying a calibrated hazard.

The extra era derivation measured 0.944 CPU-seconds per world. Against the
canonical 1,186-second census row, the projection is 1,216.749 seconds: below
the 1,320-second alarm and 1,650-second refusal. The campaign kept the
performance question measurable instead of hiding it in an optimistic
estimate.

## What remains deliberately human

The household lattice is still absent, so within-community mixing is
homogeneous. Miasma remains an appearance—the bad air, wet ground, or crowded
house people believe in—not the pathogen's source. Illness in possession is a
future bridge between a standing body and the disease field. These are not
silences accidentally forgotten by the implementation; they are the next
questions named by this one.

The Murrain's lasting rule is simple: model the world at the scale at which a
phenomenon exists, then project it at the scale at which someone can encounter
it. A named life is a lens, not a prerequisite for a population.
