# The Living Vent

The Living Vent is the temporal continuation of The Vent's Waterworld
pressure test. It asks whether hydrothermal consequences can change over
world time without turning the model into a fluid or ecology simulator.

The answer is a bounded yes. A stable seeded vent derives a pure snapshot from
exact `WorldTime` ticks through five states: absent, nascent, active,
weakening, and failed. The source identity and seabed remain present while
local chemistry, temperature, chemosynthetic bloom, nutrients, and reef/kelp
suitability respond independently. A fixed candidate ring permits bounded
migration; a current-following pass transports aggregate influence for a
bounded number of hops without rewriting substrate.

The model keeps marine nouns explicit. No species, metabolism, reproduction,
per-organism ecology, dense planet-by-time cache, event queue, fluid solver,
universal habitat wrapper, or authored vent history was added. Ordinary
observation reports present consequences; diagnostic observation names
inferred phase, provenance, local/transported contribution, and uncertainty.

Seed-42 evidence measured 623 phase/source rows, 83,997 refresh rows, 7,038
propagation candidates, and 83,997 stock rows. The focused Waterworld suite
passed 32/32, docs tests 75/75, and the local commit gate passed with a
58.028-second final run. The Sluice stage gate passed all four phases on the
real merge product in 1,483.5 seconds.

The bounded experiment leaves full current networks, reef fragmentation,
signal distortion, and marine consumers for later campaigns. Persistent
multi-cycle history was measured and not earned: present downstream stocks
already distinguish active, weakening, and failed states.
