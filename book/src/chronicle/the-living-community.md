# The Living Community

*The present world is the last frame of a history it actually lived.*

The Sounding proved the macro-history bake was affordable; it did not build
the engine. This campaign — the first of the living-community program — does,
and in doing so changes what genesis *is*. Until now a world's settlements
were **condensed**: a closed-form carrying-capacity field said how many people
each cell could feed, an up-gradient flow gathered them into attractors, and
every village was placed fully formed at time zero, an equilibrium snapshot
with no past. That model answered *where* people live. It could not answer
*why this people and not that one*, and it left the map with no yesterday —
no ruin, no abandoned clearing, no ground that remembers.

The turn this campaign makes is to stop placing settlements and start
**growing** them. Genesis now seeds a handful of proto-communities on the
early world and runs a coarse forward history across two thousand years —
each community, each era, resolving against that era's carrying capacity:
grow under slack, throw a daughter settlement into vacant favourable ground,
retreat toward refugia when the climate turns against its cell, or fail and
end. The present world is simply the **last frame** of that run: a living
settlement is a community still alive at the year two thousand, a ruin is one
that died, and the grassy clearing with a doll in it is one that died a
lifetime ago. The one-shot placer is retired outright. The carrying-capacity
field survives, demoted from *placer* to *substrate* — the fitness landscape
the history plays out across, not the thing that hands out the answer.

## The overturn — commit the skeleton, derive the flesh

The design's load-bearing decision was not *what* to simulate but *where the
history lives*. The first instinct — materialise the present frame, replay the
deep past on demand when someone asks about it — is wrong, and wrong for a
precise reason: history is **displacement-coupled**. A people frozen out of
one region lands in another; answering "who left this ruin, and where did they
go?" by replay would re-run the entire global history for every query, because
no single site's past is locally replayable. So the model inverts. The whole
history **skeleton** — a compact, dated event log of foundings, migrations,
and endings, each carrying role-handles and a provenance pointer back to the
event that produced it — is committed to the ledger. The **flesh** — the
persona behind a handle, the objects a ruin leaves, the prose a site renders —
is a pure local function of a committed fact plus the lossless seed, derived
on demand and never stored.

This is the event-sourced-database identity taken completely seriously, and it
is the one Hornvale has always claimed: **the ledger *is* the history log; the
present is a materialised view over it.** "The communities still alive" is a
query, not a separate tier of state. The deep past becomes first-class and
queryable — the same index that answers any question of the fact graph answers
"what stood here before" — while the save stays Lorenz-safe, because the flesh
never reads a quantised checkpoint; it re-derives from the seed. History is a
sole provider: no draft placer survives alongside it to emit a contradictory
present, and every settlement, ruin, and territory traces to the skeleton
event that made it.

## What drives it, without a floor

The Sounding's cautionary lesson was that a finite, equilibrating world goes
inert — its communities settle at their carrying capacity and stop moving, and
its earlier benchmark had to *force* raids with a floor to see any turnover at
all. This campaign refuses a floor. Instead it runs the bake over a capacity
field that **varies per era**, driven by the paleoclimate the world already
computes: glacial cycles shift habitability across the millennia, peoples
retreat to refugia and re-expand, and the world stays perpetually perturbed by
a forcing that is realistic, textbook, and already deterministic from the seed.
No weather-shock machinery, no stochastic integration — just the same orbital
climate history replayed, so byte-identity holds. In place of a floor, a
falsification gate: a preregistered census that *aborts the build* if
displacement fails to fire at volume. The Sounding's floor, inverted into a
tripwire.

## What the world actually did — three measured findings

The campaign preregistered its gates and then measured, and the measurements
were more interesting than the predictions.

**The peoples-diversity payoff landed.** The four goblinoids — the near-
identical peoples that no spatial-resource axis could ever separate, competing
on one shared forage number — now hold **distinct territories**. Measured as
the mean pairwise overlap of their regions of influence, seed forty-two scores
**0.055** against a ceiling of 0.25, and every sampled seed lands under 0.06.
History separated them, not niche: identical peoples put in different places by
the accidents of where the ice drove whom. This is the reason the campaign
exists, confirmed on the real world. (The first cut of this metric was
*vacuous* — a Jaccard over settlement cells, which are one-settlement-each and
therefore structurally always disjoint, a test that could report success
without measuring anything. Caught and repaired to the region-of-influence
form before it could ship.)

**Displacement is migration, not war.** The bake was expected to turn peoples
over through a raid-flee-refound loop. On the real world it barely raids at
all: ample vacant land lets a community frozen out of its cell simply *migrate*
to empty ground rather than crowd into a fight. Seed forty-two fires scores of
climate migrations and almost no raids. This is not a failure — it is the
honest prehistory of a sparsely-peopled world, where most turnover was climate
migration and not conflict, and it is consistent with the program's own thesis
that organised war is a criticality phenomenon needing shocks and crowding.
Conflict-as-criticality is deferred, by evidence, to a later campaign; the
ruins this world leaves are abandonment ruins — communities that fled the ice —
not sacked ones.

**Stratigraphy accretes on the *worst* land, not the best.** The design
predicted the opposite: prime cells get re-occupied, ruin under ruin, so the
deepest layers should sit on the best ground. The data falsified it, robustly —
the correlation between layer depth and capacity is **negative** on every
sampled seed. The mechanism is clearer than the prediction was: prime land is
held *stably*, by one long occupation that never has to restack; it is the
**marginal, climate-contested** cell that flips occupied-and-abandoned across
glacial cycles, and each re-occupation stacks another layer. You read the
land's quality not by how deep its ruins pile but by how *little* they do. A
falsified prediction is a discovery, and this one was frozen into the gate as
measured (depth-capacity correlation negative) rather than flipped to save the
hypothesis.

## The impression — what the ground keeps

The campaign's emotional target was a single image: walk into a grassy
clearing and find a ragged doll, because a family lived here and left. When the
world was measured at its true present, though, it had **no fresh dolls** — the
youngest ruin on seed forty-two is two and a half centuries old, because the
climate pulses that empty communities are millennial, and a rag doll does not
survive two hundred years. The first residue model, which put every find on one
perishable clock, rendered the flagship site as *nothing remains* — a dead end
exactly where the payoff should be.

The resolution reframed the whole idea. The point was never the doll; it is
that **settlements leave impressions, organised by durability of material.**
Perishable things — cloth, wood, food, the doll — last decades; durable things
— potsherds, worked flint, bone, the turf-lines of foundations — last
millennia (Chaco, Mesa Verde, the tell of Jericho rebuilt at every level); a
few things are near-eternal. So an ancient abandonment ruin still yields a real
archaeological site: the perishable belongings are long gone, but the durable
domestic debris endures, and a reader piecing a hamlet back together from
potsherds and dwelling outlines is doing real archaeology. Perishable finds tie
to *recent, sudden* endings — the doll dropped in a flight nobody returned from
— which couples this durability slice to the conflict campaign still ahead.
This campaign ships the minimal slice; the full impression model — character
by species, geography, and cause — is captured as its own future direction.

The flagship the world actually offers is not a goblin's but a bugbear's: a
lineage at one clearing that returned to the same failing ground five times
across two centuries, dwindling seven souls to three as the ice advanced, each
generation fleeing and the next drifting back, until the last few walked away —
and in the grass a searcher finds their doll where a doorway once stood, and
beneath it the potsherds and turf-lines of the four hamlets before. It reads
like history because it is: a fold over an event log the world committed, and
every layer of it traceable to its cause.

## What this campaign is, and is not

It is the first campaign of a program of five. It delivers history-first
placement, standing ruins, separated territories, and a legible site surface;
it retires the placer and keeps the field. It does **not** deliver organised
conflict (deferred to the criticality campaign, by measurement), a real
transport topology for diaspora and raids (the connection graph, the next
campaign), the lazily-derivable timeline as a field, or personas fleshed into
named courts and guilds. Those are the program's remaining soundings. What this
one establishes is the spine they all stand on: a present that is a query over a
committed past, a world that earned its yesterday.
