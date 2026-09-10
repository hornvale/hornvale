# The Underworld Peoples

The Underworld had a ladder, chambers, and a way to seat a people. It did not
yet have the four peoples the ladder was meant to make legible. This campaign
added mountain dwarves, duergar, svirfneblin, and kuo-toa as real deterministic
participants in that existing substrate.

## Measure before naming

The admission probe came first. Across seeds 42, 7, and 1234 it found 874,
1681, and 1266 caves respectively; every candidate niche reached a reachable
rung, and wet and dry chambers both occurred. The result admitted all four
requested kinds without treating surface elevation as underground depth.

The implementation kept the projections separate. Species rows describe the
people, environment niches describe what a chamber offers, and delve seating
consumes those facts. Mountain dwarf and duergar share the dwarf family while
svirfneblin and kuo-toa remain distinct; kuo-toa also carries swimming and a
water-sensitive niche. No biome taxonomy or new random draw was needed.

## Four names enter the world

The four kinds now appear in the species, habitat, locomotion, biosphere,
environment, and language registries required by the existing contracts.
Their seating is limited to reachable Underworld rungs. The tests hold light
constant while changing water for kuo-toa, and compare the dwarf candidates on
the same measured chamber population rather than asserting an invented modal
rung.

The census refresh changed the committed world evidence, including the
Underworld readouts. Its most visible witness is breached delvings: seed 0
moved from 1 to 11. The movement is the expected consequence of adding people
who can actually use the deep substrate, not a display-only roster change.

## The cost of saying the same name once

World generation used to resolve each placed people's collective autonym once
for reservation and again for minting. The naming path now caches the resolved
autonym per `(kind, epoch)` while preserving registry order, collision handling,
and stream consumption. A focused test pins one resolution per people.

This was a deliberately small optimization: the cache is local to the naming
projection, not a new ECS cache or global store. The refreshed census completed
in 1913 seconds, about 3.7% faster than the preceding 1986-second run, while
the final 1434.633-second timed census body still crossed the repository's
yellow alarm. That alarm is recorded as an open profiling follow-up; no
threshold was relaxed and no golden was hidden.

The campaign leaves the larger vertical-relationship bet open. It proved that
the Underworld can host multiple registry-backed peoples and that aquatic
medium remains distinct from subterranean depth. It did not implement
surface–underground commerce, speciation, or a new depth-resolution model.
